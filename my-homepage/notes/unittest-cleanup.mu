** 6-7-2022

I was under the impression that Python's `unittest.TestCase.tearDown` (and its async counterpart) only runs if all tests either fail or pass (so no uncaught exceptions).
I was also under the impression that `unittest.TestCase.addCleanup` (and its async counterpart) allows you to run teardown code regardless of uncaught exceptions in your test methods.
The first impression is wrong. `tearDown` is run regardless of uncaught exceptions in the test methods. When an exception is raised in `TestCase.setUp`, it does not run.
