{-# LANGUAGE DeriveFunctor #-}
-- ========================================================================== --
{- |
= PUH Level 4 Battle

  Welcome to your fourth PUH Battle. You know the drill. Solve **all three tasks**
  to win and follow the instructions. If you think something is ambiguous, **ask
  for clarification in the main slack channel (#haskell)**. Follow the type
  signatures and don't modify them.

  Good luck, merry Christmas (or happy holidays, which ever you prefer :) ) and
  happy New year!

-}
-- ========================================================================== --
module LevelBattle where
--

{- * DATA TYPES, RECURSIVE DATA STRUCTURES -}

{- ** LB 4.1 -}

{- |
  Cool thing about Haskell is that it doesn't hide much from you aside from
  little syntax sugar and some primitives necessary for the performance. If you
  want to redefine '+' operator you can. This is not something only core GHC
  developers are privileged to.

  With that in mind, let's define our own natural numbers (zero included) data
  type.

  We could try to do something like this:

  @
  data N = Z | One | Two | Three | ...
  @

  But obviously that would take a long time to type out. Better solution would
  be to define our natural numbers recursively, so let's do that instead.

  We can start by defining our base case (which is 0 or 'Z' since you have to
  use custom value constructors) and then we can define each number as a
  successor ('S') of the previous one.

  Complete the definition of 'N' using only custom constructors (don't just
  wrap 'Int' with 'N' or something like that).

  HINT: We are basically defining a linked list.
-}

data N = Z | S N

{- * DERIVING & DEFINING TYPECLASS INSTANCES -}

{- ** LB 4.2 -}

{- |
  That previous task was really easy, so let's make things more interesting.

  Do the following:

  1. Derive the 'Eq' class for 'N'.
  2. Make 'N' an official number by defining the 'Num' instance for it.
  3. Make 'N' an instance of 'Enum'.
  4. Make 'N' an instance of 'Show' by first converting it to 'Int' and then
     showing that 'Int'.

  You are not allowed to implement 'Num' by converting 'N' to 'Int'. Be good and
  do it by unwrapping your recursive 'N' data structure.

  Also throw an 'error' in cases where 'N' can happen to be negative (since
  natural numbers can't be negative).

  PRO TIP: You can get more information about type class by typing e.g.
  @:i Enum@ into the GHCI. To get even more details use Hoogle.

  HINT: Remember that '+' and '-' are just functions which means you can
  use pattern matching just as easily.
-}

instance Eq N where
  n1 == n2 = equal n1 n2
    where equal Z Z           = True
          equal Z _           = False
          equal _ Z           = False
          equal (S n1) (S n2) = equal n1 n2

instance Num N where
  n1 + n2     = sum n1 n2
    where sum Z Z           = Z
          sum x Z           = x
          sum Z x           = x
          sum (S n1) (S n2) = S (S (sum n1 n2))
  n1 - n2     = minus n1 n2
    where minus Z Z           = Z
          minus x Z           = x
          minus Z x           = error "Negative number"
          minus (S n1) (S n2) = minus n1 n2
  n1 * n2     = product n1 n2
    where product Z _ = Z
          product _ Z = Z
          product n1 (S n2) = n1 + (n1 * n2) 
  abs n       = n
  signum n
    | n == Z       = 0
    | otherwise    = 1
  fromInteger n
    | n < 0     = error "Negative number" 
    | n == 0    = Z
    | otherwise = S (fromInteger (n-1)::N)

instance Enum N where
  toEnum = fromInteger . fromIntegral
  fromEnum = toInt

instance Show N where
  show = show . toInt

toInt :: N -> Int
toInt Z = 0
toInt (S n) = 1 + toInt n

{- * RECORDS, PARAMETERISED TYPES, MAYBE TYPE, FMAP -}

{- ** LB 4.3 -}

{- |
  Define the parameterised record 'Ingredient' with fields 'name :: String' and
  'amount :: Maybe a'.

  Once that's done implement the function 'convert' (by using 'fmap') which can
  convert one type of 'Ingredient' into another.

  FUN FACT: You can derive a functor by enabling 'DeriveFunctor' language
  extension (and make this task super trivial in the process, let the compiler
  do the work for you :) ).

  PRO TIP : Don't think of the 'fmap' as a function which takes in a function
  from 'a' to 'b' and 'f a' and returns 'f b'. Think of it as a function which
  converts / "lifts" a function 'a -> b' into 'f a -> f b'.
-}

data Ingredient a = Ingredient { name :: String, amount :: Maybe a } deriving (Show)

convert :: ( a -> b ) -> Ingredient a -> Ingredient b
convert f ingredient = Ingredient (name ingredient) (fmap f (amount ingredient))