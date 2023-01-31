** 15-01-23

I learned (probably relearned) an interesting solution to fizzbuzz. 
It was in a blog post by Naomi Liu, called Subverting the Technical Interview.
It starts off with a fun discussion of monoids and Church numerals (which seems
disconnected from the actual solution, but probably I'm missing something), and
then it gives a fizzbuzz solution without using modulus. The blog post is written
in Haskell, but translated to Python it goes as follows:
> from itertools import count, cycle
>
> fizzes = cycle(['', '', 'fizz'])
> buzzes = cycle(['', '', '', '', 'buzz'])
> fizzbuzzes = (x + y for x, y in zip(fizzes, buzzes))
> integers = map(str, count(1))
> solution = (fizzbuzz or integer for fizzbuzz, integer in zip(fizzbuzzes, integers))
