-- ========================================================================== --
{- |
# PUH Level 2 Battle

  Welcome to your second PUH Battle!

  Unlike training, you're alone here. But don't fret - if you solved all your
  exercises and have a good understanding of topics covered, you should have
  no problems.

  This battle consists of 3 tasks. **You must successfully solve all of them in
  order to win the battle.** Every task has a single function in the form of
  `l2x`, e.g. for the first task you have to write a function `l21`.

  The output will be tested both automatically and manually, which means you must
  take care of the output formatting (spaces, punctuation etc.)

  **Do not change provided type signatures!** They're here to help you. Use them
  wisely.

  Finally, you are allowed (encouraged, even) to **create helper functions** as
  you see fit. However, every helper function **must** have type signatures
  defined.

  The soft deadline is Saturday, 3 November 2018 at 12pm (noon).
  The hard deadline is Saturday, 10 November 2018 at 12pm (noon).

  If you want two takes at this battle, submit the first one (this one) before the
  soft deadline.

  More on this can be read here:
  https://puh.takelab.fer.hr/PUH/readme/blob/master/markdown/course-organisation.md#submission-deadlines.



  You can use cabal or GHCi.

  To use cabal, run

  >> cabal repl

  which will run repl. To reload, type `:r`.

  If that's not working for you, run

  >> ghci

  and load the Level Battle:

  >> :l LevelBattle


  Although you are not allowed to ask for help in solving the exercises, if you
  have any trouble with infrastructure, running repl, or anything similar,
  **please ask for help on Slack** (but do it in public channels, e.g. `#haskell`).

  Good luck!
-}
-- ========================================================================== --

module LevelBattle where
--
import Data.List
import Data.Char
--

-- * L 2.1
--
-- | Write a recursive function which removes consecutive repeated elements from a list.
-- Example:
-- lb21 "aaabcccbdda" = "abcbda"


lb21 :: Eq a => [a] -> [a]
lb21 xs = lb21' (reverse xs) []

lb21' :: Eq a => [a] -> [a] -> [a]
lb21' [] rs = rs
lb21' (x:xs) [] = lb21' xs [x]
lb21' (x:xs) rc@(r:rs) = if x==r then lb21' xs rc else lb21' xs (x:rc)


-- ** L 2.2
--
-- | Implement a tail recursive function which returns the first index of
-- the given element a list. It should return -1 if the element is not in the list.
-- Example:
-- lb22 3 [1, 3, 5, 1, 3] = 1

lb22 :: Eq a => a -> [a] -> Int
lb22 s xs = lb22' s xs 0

lb22' :: Eq a => a -> [a] -> Int -> Int
lb22' s [] _ = -1
lb22' s (x:xs) i = if s==x then i else lb22' s xs (i+1)

-- ** L 2.3
--
-- | Implement a recursive function for integer multiplication in terms of addition.
-- This implementation should work correctly for positive numbers and 0 (don't worry
-- about negative numbers)

lb23 :: Int -> Int -> Int
lb23 a b = lb23' a b 0

lb23' :: Int -> Int -> Int -> Int
lb23' a 0 s = s
lb23' a b s = lb23' a (b-1) (s+a)


-- ** L 2.4
--
-- | Implement a function which computes all the levels of a sum triangle for a list.
-- The level of a triangle sum has one fewer element than the previous level and
-- each element is computed as a sum of the two consecutive elements in the previous
-- level.
-- Example:
-- lb24 [1.0, 2.0, 3.0, 4.0] = [[1.0, 2.0, 3.0, 4.0], [3.0, 5.0, 7.0], [8.0, 12.0], [20.0]]
-- In the case of an empty list, return an empty list.

lb24 :: [Double] -> [[Double]]
lb24 [] = []
lb24 xs = lb24' xs [xs]

lb24' :: [Double] -> [[Double]] -> [[Double]]
lb24' [x] s  = reverse s
lb24' xs s = lb24' t (t:s)
    where t = triangle xs []

triangle :: [Double] -> [Double] -> [Double]
triangle [x] s = reverse s
triangle (x:y:xs) s = triangle (y:xs) ((x+y):s)