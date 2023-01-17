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

Disaster strikes; the customer, once content with simple dictionaries, now wishes to store their strings in a file. We comply and set out to write another implementation of `Repository`, `FileRepository`.

If we take "program to an interface, not an implementation" seriously, then ideally we would write the tests for these implementations in a way that is as independent from the specific implementation as possible. 
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

