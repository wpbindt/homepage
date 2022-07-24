**24-7-2022

One way to write a code example (which is what RSpec-inspired frameworks call tests) in hspec is using 
>Test.Hspec.it :: (Example a) => String -> a -> SpecWith (Arg a)
For example,
>import Test.Hspec (hspec, it)
> 
>hspec $ it "is equal to itself" (1 == 1)
checks that 1 is equal to itself. Running this gives the output
>is equal to itself [✔]
>
>Finished in 0.0001 seconds
>1 example, 0 failures
The function
>Test.Hspec.describe :: String -> SpecWith a -> SpecWith a
can be used to group different code examples (akin to TestCases in xUnit frameworks). It also makes the output a bit nicer. For example,
>import Test.Hspec (hspec, it, describe)
> 
>hspec (describe "the number 1" (it "is equal to itself" (1 == 1)))
results in the output
>the number 1
>  is equal to itself [✔]
>
>Finished in 0.0001 seconds
>1 example, 0 failures
Now, when we deliberately write a failing test, for example
>import Test.Hspec (hspec, it, describe)
> 
>hspec (describe "the number 1" (it "is equal to 2" (1 == 2)))
then the output becomes
>the number 1
>  is equal to itself [✘]
>
>Failures:
>
>  <interactive>:4:33: 
>  1) the number 1 is equal to itself
>
>  To rerun use: --match "/the number 1/is equal to itself/"
>
>Randomized with seed 1528042340
>
>Finished in 0.0003 seconds
>1 example, 1 failure
There is no information about why the example fails. This can be fixed by using Expectations. An expectation can be made using (among others)
> shouldBe :: (Show a, Eq a) => a -> a -> Expectation
As the type constraint suggest, this allows hspec to print the expected and actual value upon failure. For example,
>import Test.Hspec (hspec, it, describe, shouldBe)
> 
>hspec (describe "the number 1" (it "is equal to 2" (1 `shouldBe` 2)))
results in
>the number 1
>  is equal to itself [✘]
>
>Failures:
>
>  <interactive>:8:60: 
>  1) the number 1 is equal to itself
>       expected: 2
>        but got: 1
>
>  To rerun use: --match "/the number 1/is equal to itself/"
>
>Randomized with seed 200382252
>
>Finished in 0.0003 seconds
>1 example, 1 failure
which is a bit nicer.
