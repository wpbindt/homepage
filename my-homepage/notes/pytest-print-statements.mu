** 15-01-23

Normally, `pytest` does not show stdout unless a test fails, or completes in
some other way. It does this because it transforms the output at that level
somehow. This means that debugging a hanging test with judiciously placed print
statements becomes impossible. This is fixed using the built-in `capsys`
fixture, as follows
> from time import sleep
>
> def test_that_hangs(capsys):
>     with capsys.disabled():
>         print('I show up immediately')
>     print('I do not')
>     sleep(100000)
>

