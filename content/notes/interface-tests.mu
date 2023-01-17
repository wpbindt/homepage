** Interface tests, 6-8-2022
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

We could write some tests very similar to the ones for `DictRepository`, replacing the dict-based setups and asserts by file-based ones. But this has some drawbacks. Firstly, it smells like a violation of DRY, since these repositories function in essentially the same way (put some stuff in, get the same stuff back later), save for some minor details (files vs dicts). Secondly, suppose we have some change in the expected behaviour of the interface. For example, say we want it to raise `SomeCustomException` when it cannot find a key. Then we'll have to write separate tests for each of its implementations asserting that they do so, and it's very easy to forget one or two.

*** Interface tests
There must be a better way. Let's say we treat our test code as though it were actual code (which it is), and we take "program to an interface, not an implementation" seriously. That is, we try to program the tests of `DictRepository` and `FileRepository` to the interface `Repository`. Of course this is not entirely possible, we'll still have to do file or dict-specific setup, but let's see how far we get. 

Some consideration leads to the following test:
> def test_repository(repository: Repository) -> None:
>   repository.set(4, 'hi')
>   assert repository.get(4) == 'hi'


Suppose you have some interface `MyInterface`, and two implementations `Implementation1` and `Implementation2`. If these implementations have some shared behavior, then you might want to write an abstract test case like this
> from abc import ABC, abstractmethod
> from unittest import TestCase
>
> class AbstractTestCase(TestCase, ABC):
>     @abstractmethod
>     def _get_tested_object(self) -> MyInterface: 
>         ...
>
>     def test_something(self) -> None:
>         test_obj = self._get_tested_object()
>
>         # make some assertions about the object
>
> class TestCase1(AbstractTestCase):
>     def _get_tested_object(self) -> MyInterface:
>         return Implementation1()
With a similar concrete TestCase for the other implementation. However, when you do this, most test runners (nose and unittest at least) will try to instantiate the abstract test class, and fail accordingly. Instead, do this:
> class AbstractTestCaseHider:
>     class AbstractTestCase(TestCase, ABC):
>         ...
>
> class TestCase1(AbstractTestCaseHider.AbstractTestCase):
>     ...
Then the test runners will not find the abstract test case, and will just run the concrete ones.

