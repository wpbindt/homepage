** 6-8-2022

Suppose you have some interface MyInterface, and two implementations Implementation1 and Implementation2. If these implementations have some shared behavior, then you might want to write an abstract test case like this
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

