** Interface tests, 6-8-2022, 17-01-2023
Suppose we have an interface for a repository which stores strings under integer keys:
> from typing import Protocol
>
> class Repository(Protocol):
>   def get(self, key: int) -> str:
>     pass
>   
>   def set(self, key: int, value: str) -> None:
>     pass
Moreover, suppose we have one implementation `DictRepository` of this interface, which uses a dictionary to store the values.

Let's write a test for `DictRepository.set`.
> def test_that_set_puts_values_in_the_dict() -> None:
>   my_dict: dict[int, str] = {}
>   repository = DictRepository(my_dict)
>   repository.set(4, 'hi')
>   assert my_dict = {4: 'hi'}
Let's add one for the `get` method too:
> def test_that_get_gets_the_values_from_the_dict() -> None:
>   my_dict: dict[int, str] = {4: 'mom'}
>   repository = DictRepository(my_dict)
>   assert repository.get(4) == 'mom'
Great! Now our dict-based repository is well-tested, and we can be sure our code works when put in production, and we won't be called at 3AM by angry customers asking why their dicts aren't getting filled.

Disaster strikes; the customer, once content with filling up simple dictionaries, now wishes to store their strings in a file. We comply, and set out to write another implementation of `Repository`, `FileRepository`. We start by writing some tests.

We could write some tests very similar to the ones for `DictRepository`, replacing the dict-based setups and asserts by file-based ones. But this has some drawbacks. Firstly, it smells like a violation of DRY, since these repositories function in essentially the same way (put some stuff in, get the same stuff back later), save for some minor details (files vs dicts). Secondly, suppose we have some change in the expected behaviour of the interface. For example, say we want it to raise `SomeException` when it cannot find a key. Then we'll have to write separate tests for each of its implementations asserting that they do so, and it's very easy to forget one or two.

*** A solution: interface tests
There must be a better way. Let's say we treat our test code as though it were actual code (which it is), and we take "program to an interface, not an implementation" seriously. That is, we try to program the tests of `DictRepository` and `FileRepository` to the interface `Repository`. Of course this is not entirely possible, we'll still have to do file or dict-specific setup, but let's see how far we get. 

Some consideration leads to the following test:
> def test_repository(repository: Repository) -> None:
>   repository.set(4, 'hi')
>   assert repository.get(4) == 'hi'
Assuming we have a test framework which can figure out the setup and teardown of the fixtures on its own, this test case is perfect! It can be used for both `FileRepository` and `DictRepository`, and even for future implementations of `Repository`, if ever we write one. Despite being a perfectly adequate testcase for both implementations, it's coupled to the implementation of neither.

For a more concrete benefit, assume a feature request comes in that `Repositories` should raise a `SomeException`, as above. Again we can write a test completely decoupled from any specific implementation:
> def test_repository(repository: Repository) -> None:
>   with assert_raises(SomeException):
>     repository.get(4)
This test is valid for all implementations, and will fail until we've implemented the feature for all of them, subverting the error-prone implementation hunt (especially with structural as opposed to nominal subtyping) we'd have to do if we were to write tests per implementation.

As another benefit, if ever we'd need to implement something like a `RedisRepository`, then we'll have a test suite ready to be used, at no extra cost!

Now that we're convinced that 
-low coupling between pieces of code is good, 
-test code is code as much as the code it tests is, 
-therefore low coupling between a test and its subject is good, 
let's consider how to implement this in actual test frameworks, as opposed to the hypothetical annotation-based framework we've been working with so far. We consider `unittest` and `pytest`. Each have their pros and cons.

** Actual implementations
*** unittest
In `unittest`, tests are organized in classes. I don't know why this is, because usually the main reason to make something a class is because there's state afoot, but I see no state here (maybe my knowledge of `unittest` is lacking). A more cynical interpretation is that the other main reason to make something a class is because you're forced to, which certainly explains why `JUnit` uses classes to organize tests, and since `unittest` is inspired by `JUnit`, maybe this design non-choice was copied without putting too much thought into it.

As a result, the only way I've found so far to implement tests with low coupling to their subjects is using the template pattern. The result causes more confusion than it's worth, and is added here just as a curiosity. For example, for the `Repository` interface as above,
> from abc import ABC, abstractmethod
> from unittest import TestCase
>
> class AbstractRepositoryTestCase(TestCase, ABC):
>   @abstractmethod
>   def _get_repository(self) -> Repository: 
>     ...
>
>   def test_repository(self) -> None:
>     repository = self._get_repository()
>     repository.set(4, 'hi')
>     self.assertEqual('hi', repository.get(4))
with the test case for `DictRepository` being
> class DictRepositoryTest(AbstractRepositoryTestCase):
>   def _get_repository(self) -> Repository:
>     return DictRepository({})

Actually the implementation as is, is not quite correct. Depending on how you run your tests, and where `AbstractRepositoryTestCase` is located, `unittest` might try to instantiate and run `AbstractRepositoryTestCase`, thinking it's a proper test case. This will cause `unittest` to error out. To remedy this, we can wrap the abstract test case in a class hiding it from `unittest`:
> class AbstractTestCaseHider:
>   class AbstractRepositoryTestCase(TestCase, ABC):
>     ...
>
> class DictRepositoryTest(AbstractTestCaseHider.AbstractRepositoryTestCase):
>   ...
Alternatively, we could avoid inheriting from `TestCase` in the base case, and have the clients inherit from both `AbstractRepositoryTestCase` and `unittest.TestCase`. But then you're starting down the road of multiple inheritance, which is in general a very bad idea.

The combination of the template pattern and the test case hider business makes this a profoundly confusing solution, not worthy of recommendation.

*** pytest
In `pytest`, the solution is more elegant, and less confusing (which doesn't say that much).
> # conftest.py
>
> @pytest.fixture
> def dict_repository():
>   yield DictRepository({})
>
> @pytest.fixture
> def file_repository():
>   filepath = pathlib.Path('my_file')
>   filepath.touch()
>   yield FileRepository(filepath)
>   filepath.unlink(missing_ok=True)
>
> @pytest.fixture(
>   params=[
>     dict_repository.__name__,
>     file_repository.__name__,
>   ]
> )
> def repository(request):
>   return request.getfixturevalue(request.param)
With this `conftest`, we can write tests for `Repository` as follows:
> def test_repository(repository):
>   repository.set(4, 'hi')
>   assert repository.get(4) == 'hi'
This test will be run for both implementations of `Repository` specified in `conftest`. The main drawback this approach has compared to the `unittest` approach is its lack of type safety. This is because of `pytest`'s preference for implicit over explicit. It injects dependencies by matching based on strings, we lose type safety for everything we inject into a test function. Despite this (quite serious) drawback, I still prefer the `pytest` solution over the `unittest` one. 

Ideally there would be a test framework which handles fixtures similarly to `pytest`, but injects them in an explicit rather than implicit way, similarly to `fastapi` does. I'm imagining something like
> def repository():
>   yield from dict_repository()
>   yield from file_repository()
> 
> def test_something(repository: some_test_framework.Dependencies[repository]):
>   ...
I'm not sure if that'd actually work out, but it's a digression anyway.
