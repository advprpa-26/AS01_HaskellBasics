{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use list literal" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Eta reduce" #-}

module Main (main) where
import Prelude hiding (Foldable)
import Test.Hspec ( hspec, describe, it, shouldBe, Spec )
import Test.QuickCheck ( Testable(property) )


-------------------------------------------------------------------------------
-- 1. Definitions
-------------------------------------------------------------------------------

-- The following is called a definition or binding.
-- It binds the value `True` to the name `a1`.
a1 = True

-- Names always start with a lowercase letter.
-- This is the same as a constant definition in another language.
-- This is not the same as a variable in other languages.
-- `a1` will always be equal to `True`. The value can't be changed to `False` afterwards.

-- Bindings can and should be associated with a type.
-- On the first line `a2` is declared to be of type `Char`.
-- On the second line `a2` is defined to be equal to the character 'c'.
a2 :: Char
a2 = 'c'

-------------------------------------------------------------------------------
-- 2. Basic types
-------------------------------------------------------------------------------

-- Haskell is a typed language. This means that every expression has a type.
-- Haskell's type system describes the rules when the parts (eg. a function and its argument) fit together and when they do not.
-- Haskell's type checker enforces these rules and only accepts programs when they are type correct.

-- Haskell has a few basic types:

t1 :: Bool -- Truth values
t1 = False -- The type Bool has two values: True and False

t2 :: Char -- Characters
t2 = 'c' -- In single quotes

t3 :: Int -- Machine word whole numbers (64 bits on my machine)
t3 = 12

t4 :: Integer -- Arbitrary large whole numbers
t4 = 100000000000000000000000

t5 :: Float -- Floating point numbers with single precision
t5 = 3.141

t6 :: Double -- Floating point numbers with Double precision
t6 = 3.14159

-------------------------------------------------------------------------------
-- 3. Data types
-------------------------------------------------------------------------------

-- A new data type can be defined using `data`:
-- `ABool` is the name of the type.
-- This type has two value constructors called `AFalse` and `ATrue`
-- The name of the type and the names of the values must all start with uppercase letters.
data ABool = AFalse | ATrue

c1 :: ABool
c1 = ATrue

-- Values of type ABool can not be printed in ghci (try it by typing `c1` in the repl),
-- neither can they be compared for equality (try `c1 == AFalse`) because those operations are just not defined.
-- Haskell's deriving mechanism allows for the automatic generation of this functionality:

data Color = Red | Green | Blue deriving (Show, Eq)

-- Values of type `Color` can be shown (`c2` ~> "Blue")
c2 :: Color
c2 = Blue


-- TODO: Write your own enumeration type for weekdays.
-- Make sure that the values can be printed and compared.

-- TODO: Define a binding named `bestDay` with your value for Sunday. Give the binding a type signature.

-------------------------------------------------------------------------------
-- 4. Basic syntax
-------------------------------------------------------------------------------

-- Function types are written with an arrow: `input -> output`.
-- `f1` is a function from `Int` to `Bool`.
f1 :: Int -> Bool
f1 i = i > 0 -- Returns whether `i` is larger than `0`.

-- The following applies the function `f1` to the argument `2` (and gives the result the name `r1`).
-- In many programming languages one would wrap the arguments in parentheses `f1(2);` - but not in Haskell.
r1 :: Bool
r1 = f1 2 -- There is only a space between the function `f1` and its argument `2`.

-- A function can take more than one argument (more on this below):
f2 :: Int -> Int -> Int
f2 a b = a + b

-- Here we apply the function f2 to two arguments:
-- In Java one would write f2(1,2);
r2 :: Int
r2 = f2 1 2

-- TODO: Define the function sumOfSquares:
-- It should take two arguments and return the sum of their squares.
sumOfSquares :: Int -> Int -> Int
sumOfSquares = error "TODO"

-- This is a test which is executed by the test framework.
sumOfSquaresSpec :: Spec
sumOfSquaresSpec =
    describe "sumOfSquares" $ do
        it "sumOfSquares 2 3 == 13" $
            sumOfSquares 2 3 `shouldBe` 13
        it "behaves like `f(x,y) = x*x + y*y`" $
            property $ \(x::Int, y:: Int) ->     -- checked against 100 randomly generated (x, y) pairs
                sumOfSquares x y == x*x + y*y    -- our test states what property must hold


-- Conditional execution can be written using:
-- 1. conditional expression:
f3 :: Int -> Bool -> Int
f3 a b = if b then a + 1 else a -- Every `if` requires an `else` clause


-- 2. Guarded equations:
f4 :: Int -> Bool -> Int
f4 a b | b         = a + 1 -- any `Bool` expression can be used after |
       | otherwise = a     -- otherwise is defined as `True`

-- 3. Pattern matching:
f5 :: Int -> Bool -> Int
f5 a True = a + 1
f5 a False = a


-- Pattern matching works top-down through the equations until the first matches:
f6 :: Color -> Bool
f6 Red = True
f6 _   = False -- `_` is the catch all pattern

-- This function will always return False, also if applied to `Red` (because the first equation matches always):
f7 :: Color -> Bool
f7 _   = False
f7 Red = True

-- And this is a partial function. Partial in that it is not defined for all values of its parameter type (also called domain).
-- This function crashes if applied to `Red`.
-- Partial functions are bad, because they can lead to bugs.
-- Try: `f8 Red`
f8 :: Color -> Char
f8 Green = 'g'
f8 Blue  = 'b'


-- TODO: Define the function nextColor:
-- Next of red is green, next of green is blue, next of blue is red again.
nextColor :: Color -> Color
nextColor = error "TODO"

-- This is a test which is executed by the test framework.
nextColorSpec :: Spec
nextColorSpec =
    describe "nextColor" $ do
        it "nextColor Red == Green" $
            nextColor Red `shouldBe` Green
        it "nextColor Green == Blue" $
            nextColor Green `shouldBe` Blue
        it "nextColor Blue == Red" $
            nextColor Blue `shouldBe` Red

-- Local definitions can be used for helper functions or intermediate results and avoid the pollution of the name space.
-- There are two ways to declare local definitions.
-- 1. let expressions:
f9 :: Color -> Int
f9 c = let a :: Color    -- a and f are local definitions and only visible within the `let` and `in` block:
           a = Red

           f :: Color -> Int
           f x | x == a = 0
           f _          = 12
       in f c

-- 2. where clauses:
f10 :: Color -> Int
f10 c = f c
  where
    a :: Color    -- a and f are local definitions and only visible within the `where` block and the function clause this where block is attached to:
    a = Red

    f :: Color -> Int
    f x | x == a = 0
    f _          = 12

-- TODO: Write the type of the given function `f11`:
--- f11 :: TODO
f11 a c = f10 a > c


-------------------------------------------------------------------------------
-- 5. Algebraic Data Types ADT
-------------------------------------------------------------------------------

-- Value constructors can have multiple components:
data Part = Storage Int
          | Display Int Int
          | Power Bool
          deriving (Show, Eq)

-- The constructor `Storage` takes an `Int` as argument and constructs a value of type `Part`.
s1 :: Part
s1 = Storage 1024

-- Pattern matching is used to distinguish the different constructors and give the components names.
-- Those names (lower case) can then be used on the right hand side:
-- Try:
-- price (Storage 1024)
-- price (Power True)
price :: Part -> Int
price (Storage mb)  = mb  -- Parentheses are required around the patterns
price (Display l w) = l * w
price (Power True)  = 100
price (Power False) = 50

-- TODO: Write the type of the given definition `s2`.
-- s2 :: TODO
s2 a b = price a + price (Power b)


-- It is easy to define a Pair of `Int` values:
-- The name of the type is `IPair` the value constructor is named `IP`.
-- The value constructor could have been named `IPair` as well (and that would be ok),
-- but it also could lead to confusion between type and value.
data IPair = IP Int Int deriving Eq

-- An example value
ip :: IPair
ip = IP 5 4


-- TODO: Define the function pairProduct:
-- It returns the product of its components.
pairProduct :: IPair -> Int
pairProduct = error "TODO"

pairProductSpec :: Spec
pairProductSpec =
    describe "pairProduct" $ do
        it "pairProduct (IP 4 5) = 20" $
            pairProduct (IP 4 5) `shouldBe` 20
        it "swapping twice returns the original" $
            property $ \(a::Int) b -> iswap (iswap (IP a b)) == IP a b -- nice property


-- A function which swaps the values on `IPair` values:
-- Try: `iswap (IP 1 2)`
iswap :: IPair -> IPair
iswap (IP i1 i2) = IP i2 i1

-- Instead of defining pairs for every desired combination of types, we can use type variables (same as generics in Java).
-- The following defines a type constructor named `Pair` which takes two types `a` and `b` as arguments.
-- The value constructor `P` takes two components of the given types:
data Pair a b = P a b deriving (Show, Eq)

-- This is a `Pair` with an `Int` and a `Bool`:
p1 :: Pair Int Bool
p1 = P 12 True

-- And here is one with a Part and a Color
p2 :: Pair Part Color
p2 = P (Power True) Red

-- Here is a function to extract the first component.
-- Try: `first p2`
-- This function is polymorphic, meaning it works for pairs with different component types.
-- Pro: Can you give a wrong implementation that terminates?
first :: Pair a b -> a
first (P x _) = x

-- Because pairs are very practical, there is builtin syntax in haskell:
p3 :: (Part, Color)
p3 = (Power True, Red)

-- Tuples can have arbitrary many components of arbitrary types.
-- The number and types of the components are fixed by the tuple's type:
p4 :: (Int, Bool, Color)
p4 = (1, True, Red)

-- For pairs (tuples with two components) there are projection functions (accessors)
-- for the two components `fst` and `snd`.
p5 :: Bool
p5 = fst (True, Red)

p6 :: Color
p6 = snd (True,Red)

-- TODO: What is the type of the given definition `p7`?
-- p7 :: TODO
p7 = snd (True, fst (Red, 'X'))


-- Without record syntax, accessor functions for the attributes must be written by hand:
data Person = Person String String
firstName (Person fn _) = fn
lastName (Person _ ln)  = ln

-- By using record syntax, the accessor functions are generated automatically.
data Lecturer = MkLecturer {fstName :: String, sndName :: String}

name :: String
name = fstName (MkLecturer "Peter" "Meier")

-- TODO: What is the type of the generated function `sndName`?
-- You can check your answer with `:t sndName` in the repl.


-------------------------------------------------------------------------------
-- 6. Lists
-------------------------------------------------------------------------------

-- Here is a data type for a linked list of elements of type `a`:
data List a -- `List` is called a type constructor and `a` is a type variable. This is similar to `interface List<A>` in Java.
   = Nil -- There are two ways to construct a list: An empty list, or ...
   | Node a (List a) -- a node with payload `a` and the rest of the list (of type `List a`).
   deriving (Show, Eq)

-- Notice that all elements in a list must be of the same type.
-- But the type variable `a` can be instantiated by the "user" of the `List` type constructor.
-- An example with 3 elements of type `Int`:
l1 :: List Int -- Here we instantiate the type variable `a` with `Int`
l1 = Node 1 (Node 2 (Node 3 Nil))

-- It is easy to add another element to the front:
l2 :: List Int
l2 = Node 0 l1

-- Here is a function to access the first element of such a list:
firstE :: List a -> a
firstE (Node x _) = x

-- And here is an example usage.
e1 :: Int
e1 = firstE l2

-- But this function does not work if the list is empty.
-- This is why partial functions are BAD!
e2 :: Int
e2 = firstE Nil -- Crashes!

-- TODO: Define the function isEmpty:
-- It returns whether the given list is empty.
isEmpty :: List a -> Bool
isEmpty = error "TODO"

isEmptySpec :: Spec
isEmptySpec =
    describe "isEmpty" $ do
        it "isEmpty Nil = True" $
            isEmpty Nil `shouldBe` True
        it "isEmpty (Node 'x' Nil) = False" $
            isEmpty (Node 'x' Nil) `shouldBe` False


-- Like tuples, lists are often used and have special syntax in haskell.
l3 :: [Int] -- A list with elements of type Int. Between the brackets is the element type.
l3 = [1,2,3] -- Literal syntax to create a list with three elements

-- Here is an example of an empty list. The empty list value `[]` is polymorphic - its type is [a].
-- Thus it can be used as an empty list of any element type.
-- For example as an empty list of elements of type Bool.
l4 :: [Bool]
l4 = []

-- The literal syntax for values is syntactic sugar for the following.
-- Instead of prefix `Node` we write infix `:`.
-- Compare this to 1 `Node` (2 `Node` (3 `Node` Nil))
l5 :: [Int]
l5 = 1 : (2 : (3 : []))

-- Because the value constructor `:` is right associative, the parentheses can be omitted:
l6 :: [Int]
l6 = 1 : 2 : 3 : []

-- There are many predefined functions for lists.
-- Note: head and tail are partial functions (not defined for empty lists). GHC warns about this.
-- TODO: Inspect the resulting values of l7, l8 and l9:
l7 :: Int
l7 = head [1,2,3]

l8 :: [Int]
l8 = tail [1,2,3]

l9 :: [Int]
l9 = [1,2,3] ++ [4,5,6]

-- Lists are often inspected using pattern matching.
-- As always, when writing a pattern, parentheses are required.
-- This function returns the first two elements of a list of at least length 2
-- or the empty list otherwise:
getFirstTwo :: [a] -> [a]
getFirstTwo (a1:a2:_) = [a1,a2] -- `a1:a2:[]` on the rhs would also work
getFirstTwo _         = []


-- TODO: Define the function firstAndThird:
-- It returns in a pair the first and third element of a list.
-- Is a total (in contrast to partial) implementation possible?
firstAndThird :: [a] -> (a,a)
firstAndThird = error "TODO"

firstAndThirdSpec :: Spec
firstAndThirdSpec =
    describe "firstAndThird" $ do
        it "firstAndThird [1,2,3,4] = (1,3)" $
            firstAndThird ([1,2,3,4] :: [Int]) `shouldBe` (1,3)


-- By the way, Strings are just lists with elements of type Char.
-- The prelude (standard library) defines:
-- type String = [Char]
-- Which makes the name `String` a type alias (just another name) for `[Char]`.
-- Strings also have literal syntax:
s :: String
s = "Look how easy it is to write a list of Chars :)" -- Same as ['L', 'o', ...] and thus same as 'L' : 'o' : ...


-------------------------------------------------------------------------------
-- 7. Currying and partial application
-------------------------------------------------------------------------------

-- The function type arrow associates to the right.
-- A function which takes "multiple parameters" like the following ...
add :: Int -> Int -> Int
add a b = a + b

-- ... is actually the same as this.
addx :: Int -> (Int -> Int)
addx = add

-- The type signature of addx says:
-- Give me one first `Int` and I give you back a function `(Int -> Int)`.
-- So the truth is, every function takes exactly one argument.

-- This also means that we can apply such a function to a single argument.
-- This is called partial application
-- Apply `add` to the first argument `1`, the result is a function `Int -> Int` which increments its argument.
inc :: Int -> Int
inc = add 1

-- Function application binds strongest and associates to the left.
-- Because the function arrow is right associative and function application is left associative
-- we can naturally write `f a b c` and it feels as comfortable as like f would take multiple arguments where in fact it takes one after the other:
res :: Int
res = add 1 2 -- actually means ((add 1) 2)

-- TODO: What is the type of `pa1`?
-- pa1 :: TODO
pa1 = f True 1
  where f :: Bool -> Int -> Int -> Bool
        f _ _ _ = True


-------------------------------------------------------------------------------
-- 8. Higher order functions
-------------------------------------------------------------------------------

-- Higher order functions are functions which take other functions as arguments.
-- Here an example of a function which:
-- 1. takes a pair with an `Int` as its first component and an arbitrary second component
-- 2. takes a function from `Int -> Int`
-- and applies the function f to the first component of the pair.
mapFst :: (Int,b) -> (Int -> Int) -> (Int, b)
mapFst (i,b) f = (f i, b)

-- This can be used as follows:
h :: (Int, Bool)
h = mapFst (5, True) inc -- Here we pass `inc` as an argument

-- The function could be typed more general:
mapFst' :: (a,b) -> (a -> a) -> (a, b)
mapFst' (i,b) f = (f i, b) -- the implementation stays the same

-- There are many higher order functions defined for lists:
-- map :: (a -> b) -> [a] -> [b] -- transforms every element of the list of a's and returns the list of b's.
-- filter :: (a -> Bool) -> [a] -> [a] -- Keeps only the elements which evaluate to `True` using the given predicate (test function).

-- Applies the function `inc` to every element in the list and collects the results:
mr :: [Int]
mr = map inc [1,2,3,4]

-- Keeps only the elements for which `even` evaluates to `True`:
fr :: [Int]
fr = filter even [1,2,3,4]

-- TODO: Define the function evenWhenSquared:
-- It takes a list of Ints, squares every Int, and keeps only those values which are even.
-- Define and use a local function `square :: Int -> Int`
evenWhenSquared :: [Int] -> [Int]
evenWhenSquared = error "TODO"

evenWhenSquaredSpec :: Spec
evenWhenSquaredSpec =
    describe "evenWhenSquared" $ do
        it "evenWhenSquared [1,2,3,4] = [4,16]" $
            evenWhenSquared ([1,2,3,4] :: [Int]) `shouldBe` [4,16]

-- TODO: What is the type of the given definition `ho1`?
-- ho1 :: TODO
ho1 = map fst

-------------------------------------------------------------------------------
-- 9. Lambda Expressions (anonymous functions)
-------------------------------------------------------------------------------

-- The following defines a function named `la1` which increments its argument.
-- It consists of two things: Functionality (increment the value) and the name `la1`.
la1 :: Int -> Int
la1 i = i + 1

-- In Haskell we can have functionality without a name (anonymous function).
-- This is called a lambda expression and written like this:
-- \i -> i + 1
-- The following binds the lambda function to the name `la2`:
la2 :: Int -> Int
la2 = \i -> i + 1

-- Sometimes we do not need the name, but are only interested in the functionality.
-- This is often the case when working with higher order functions:
la3 :: [Int]
la3 = map (\i -> i + 1) [1,2,3]

-- TODO: Define the function evenWhenSquared':
-- It takes a list of Ints, squares every Int, and keeps only those values which are even.
-- Use a lambda expression to square the values.
evenWhenSquared' :: [Int] -> [Int]
evenWhenSquared' = error "TODO"

evenWhenSquared'Spec :: Spec
evenWhenSquared'Spec =
    describe "evenWhenSquared'" $ do
        it "evenWhenSquared' [1,2,3,4] = [4,16]" $
            evenWhenSquared' ([1,2,3,4] :: [Int]) `shouldBe` [4,16]

-------------------------------------------------------------------------------
-- 10. Operators and sections
-------------------------------------------------------------------------------

-- Operators are non alphanumeric names which are written infix (between the arguments).
o1 :: Int
o1 = 1 + 2

-- One can define its own operators:
(!+!) :: Int -> Int -> Int
a !+! b = abs a + abs b
infixl 6 !+! -- One can even define its associativity and precedence.

-- This can now be used and written infix:
o2 :: Int
o2 = (-4) !+! 5


-- To place an operator in front of its arguments, it can be wrapped in parentheses:
o3 :: Int
o3 = (+) 1 2


-- And we have special syntax called "sections" to partially apply an operator and treat it as function:
o4 :: Int -> Int
o4 = (+ 1) -- same as (\i -> i + 1)

-- It also works for the first argument:
o5 :: Int -> Int
o5 = (1 -) -- same as (\i -> 1 - i)


-- Even functions and value constructors can be written infix if surrounded with backticks:
mul :: Int -> Int -> Int
mul = (*)

o6 :: Int
o6 = 3 `mul` 4

-- TODO: Define the function evenWhenSquared'':
-- It takes a list of Ints, squares every Int, and keeps only those values which are even.
-- Use a `^` operator section to square the values.
evenWhenSquared'' :: [Int] -> [Int]
evenWhenSquared'' = error "TODO"

evenWhenSquared''Spec :: Spec
evenWhenSquared''Spec =
    describe "evenWhenSquared''" $ do
        it "evenWhenSquared'' [1,2,3,4] = [4,16]" $
            evenWhenSquared'' ([1,2,3,4] :: [Int]) `shouldBe` [4,16]


-------------------------------------------------------------------------------
-- 11. Recursion
-------------------------------------------------------------------------------
-- We have no while or for loops in Haskell, we use recursion!

-- This function builds the product of all numbers smaller than n. (only works for positive numbers.)
-- prod 3
-- ~> 3 * prod 2
-- ~> 3 * 2 * prod 1
-- ~> 3 * 2 * 1
-- ~> 6
prod :: Int -> Int
prod 1 = 1              -- Base case, here recursion terminates
prod n = n * prod (n-1) -- Recursive case, here the function is called typically with a smaller part of the input.

-- Recursion needs some exercise.
-- Implementing existing list functions is a good approach:
size :: [a] -> Int
size []       = 0 -- Empty list has size 0
size (_:rest) = 1 + size rest -- We have 1 element plus the size of the rest

{-
Advice on recursion from Graham Hutton, Programming in Haskell:
"Defining recursive functions is like riding a bicycle:
it looks easy when someone else is doing it;
it may seem impossible when you first try to do it yourself,
but becomes simple and natural with practice."

Procedure:
Function drop removes the first n elements of a list.

Step 1: Define the type:
drop :: Int -> [Int] -> [Int]

Step 2: Enumerate the cases:
drop 0 []
drop 0 (x:xs)
drop n []
drop n (x:xs) = ...

Step 3: Define the simple cases:
drop 0 [] = []
drop 0 (x:xs) = (x:xs)
drop n [] = []

Step 4: Define the other cases:
drop n (x:xs) = drop (n-1) xs

Step 5: Generalize and simplify.
-}

-- TODO: Define the function sumOfSquares:
-- It takes a list and returns the sum of their squared elements.
sumOfSquaresRec :: [Int] -> Int
sumOfSquaresRec = error "TODO"

sumOfSquaresRecSpec :: Spec
sumOfSquaresRecSpec =
    describe "sumOfSquaresRec" $ do
        it "sumOfSquaresRec [2,3] == 13" $
            sumOfSquaresRec [2,3] `shouldBe` 13
        it "sumOfSquaresRec [2,3,4] == 29" $
            sumOfSquaresRec [2,3,4] `shouldBe` 29
        it "on a list with two elements, matches sumOfSquares" $
            property $ \(x::Int, y::Int) -> sumOfSquaresRec [x,y] == sumOfSquares x y


-- TODO: Define the function `convertList` which takes a `List a` and converts it to a Haskell `[a]`:
convertList :: List a -> [a]
convertList = error "TODO"

convertListSpec :: Spec
convertListSpec =
    describe "convertList" $ do
        it "convertList Nil == []" $
            convertList Nil `shouldBe` ([] :: [Int])
        it "convertList (Node 'a' (Node 'b' Nil)) == \"ab\"" $
            convertList (Node 'a' (Node 'b' Nil))  `shouldBe` "ab" -- Remember that String = [Char]


{-
                                ____
  (                            |   /
  )\ )                   (     |  /
 (()/(    (     (       ))\    | /
  ((_))   )\    )\ )   /((_)   |/
  _| |   ((_)  _(_/(  (_))    (
/ _` |  / _ \ | ' \)) / -_)   )\
\__,_|  \___/ |_||_|  \___|  ((_)
-}

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "4. Basic Syntax" $ do
    sumOfSquaresSpec
    nextColorSpec
  describe "5. Algebraic Data Types ADT" $ do
    pairProductSpec
  describe "6. Lists" $ do
    isEmptySpec
    firstAndThirdSpec
  describe "8. Higher Order Functions" $ do
    evenWhenSquaredSpec
  describe "9. Lambda Expressions (anonymous functions)" $ do
    evenWhenSquared'Spec
  describe "10. Operators and sections" $ do
    evenWhenSquared''Spec
  describe "11. Recursion" $ do
    sumOfSquaresRecSpec
    convertListSpec
