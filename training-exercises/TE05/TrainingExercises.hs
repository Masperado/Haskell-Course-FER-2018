-- =============================================================================== --
{- |
  Welcome to your fifth Haskell training. Get ready to rumble.
  Where will you let recursion lead you?

  You should know the drill by now - solve the exercises in `TrainingExercises.hs`,
  run the tests with Cabal, push to `training-05`, create a Merge Request,
  and assign it to your TA.

  Keep in mind that **all exposed functions must have type signatures** (helper functions that are defined in local definitions don't)!

  As always, ask your TA if you need any help.
-}
-- =============================================================================== --
--
module TrainingExercises where
--
import Data.List
import Data.Char
--

{- * 5.1 Recursive functions with accumulators -}

{-
 - For the following functions, we'd like you to to use the `seq` function to
 - reduce the accumulators to WHNF where needed to avoid building big thunks.
 -}

-- ** TE 5.1.1
--
-- | Define a recursive function with accumulation which finds the
-- | first longest word in a sentence.
-- | Make sure you keep track of the length of the currently longest word
-- | too, so you don't call `length` repeatedly on the same word.

te511 :: String -> String
te511 x = te511' (words x) ""
te511' :: [String] -> String -> String
te511' [] a = a
te511' (x:xs) a = let b = longest in longest `seq` te511' xs longest
  where longest = if length a >= length x then a else x

-- ** TE 5.1.2
--
-- | Define a recursive function with accumulation which takes a list of
-- | polynomial coefficients and a variable, and calculates the polynomial using
-- | Horner's method (https://en.wikipedia.org/wiki/Horner%27s_method ).
-- | The coefficients are in descending order of variable exponents, so a list
-- | representing the polynomial x^3 - 2x + 3 would be  as  [1, 0, -2, 3].

te512 :: Num a => [a] -> a -> a
te512 xs v = te512' xs v 0
te512' :: Num a => [a] -> a -> a -> a
te512' [] v s = s
te512' (x:xs) v s = let r = s*v+x in r `seq` te512' xs v r


-- ** TE 5.1.3
--
-- | Define a function which computes the population standard deviation of a list of
-- | numbers. To achieve this you need to compute the mean and variance of the list:
-- | do this using recursive functions with accumulation.

te513 :: Floating a => [a] -> a
te513 [] = error "empty list"
te513 xs = (variance xs m 0 / l) ** (1/2)
  where m = mean xs l 0
        l = fromIntegral $ length xs

variance :: Floating a => [a] -> a -> a -> a
variance [] m s = s
variance (x:xs) m s = let t = s + (x - m)**2 in t `seq` variance xs m t

mean :: Floating a => [a] -> a -> a -> a
mean [] l m = m
mean (x:xs) l m = let s = m + x/l in s `seq` mean xs l s 

-- ** TE 5.1.4
--
-- | An aspiring rollercoaster designer wants to test out his if his newest
-- | creation is safe to ride, and needs your help!
-- | Define a function which takes a list of pairs which describe a section of
-- | the track. The first element will be a String which will be either "even",
-- | "drop", "turn left" or "turn right". The second element will be a number.
-- |
-- | If it's a "drop", the car accelerates as if it was in free-fall (accelerating
-- | 9.81 m/s^2) and the number indicates the height of the drop in meters.
-- | The car maintains its current speed coming into the drop.
-- |
-- | If it's "even", the car decelerates by 0.5 m/s every meter it passes. The
-- | number indicates the length of the even segment. If the car decelerates to
-- | 0 km/s, the track is deemed unsafe as the passengers will become stuck!
-- |
-- | If it's either of the two "turn"s, the number indicates the radius of the
-- | turn in meters. The car will derail if it turns too tightly: it can only
-- | withstand centripetal acceleration of up to and including 5G. And if there
-- | are 3 or more alternating turns directly in a row, the passengers will become
-- | nauseous, which can be unsafe.
-- |
-- | The car starts moving at 20 km/h. The function must return a list indicating
-- | whether the rollercoaster is safe or not. If it is safe, it returns an empty list.
-- | If the rollercoaster is not safe, it returns a list with one element: the
-- | index of the segment of the track where it becomes unsafe.
-- | (Later on, you will learn a much more elegant way of representing a result
-- | which might contain a value, or might contain nothing at all, but this will
-- | do for now.)

te514 :: [(String, Double)] -> [Int]
te514 [] = error "empty list"
te514 xs = te514' xs 5.56 0 [] [-1]
-- Danas na FER-u stvarno svako može proći Fiziku 1, pa čak i oni ljudi koji misle da će 
-- auto brzine 5.56 m/s zaustaviti se nakon što uspori sa 11.1112*0.5, tj. 5.5556 metara u sekundi
te514' :: [(String, Double)] -> Double -> Double -> [String] -> [Int] -> [Int]
te514' [] v c t [i]
  | v <= 0 = [i]
  | t == ["turn left", "turn right", "turn left"] = [i]
  | t == ["turn right", "turn left", "turn right"] = [i]
  | c > 5*9.81 = [i]
  | otherwise = []
te514' (x:xs) v c t [i]
  | v <= 0 = [i]
  | t == ["turn left", "turn right", "turn left"] = [i]
  | t == ["turn right", "turn left", "turn right"] = [i]
  | c > 5*9.81 = [i]
  | fst x == "even" = let nv = (v - 0.5 * snd x) in te514' xs nv 0 [] [i+1]
  | fst x == "drop" = let nv = ((v*v + 2 * 9.81 * snd x)**(1/2)) in te514' xs nv 0 [] [i+1]
  | fst x == "turn left" = let nc = (v*v/snd x) in te514' xs v nc ("turn left":t) [i + 1]
  | fst x == "turn right" = let nc = (v*v/snd x) in te514' xs v nc ("turn right":t) [i + 1]



-- ** TE 5.1.5 - EXTRA
--
-- | Define a recursive function with accumulation which computes the square root
-- | of a given number using Newton's method, with the given number of iterations.
-- | Use the halved original number as an initial guess for the method.
te515 :: (Ord a, Fractional a, Integral b) => a -> b -> a
te515 a b = te515' a b (a/2) 

te515' :: (Ord a, Fractional a, Integral b) => a -> b -> a -> a
te515' a 0 g = g
te515' a b g = let ng = (g - (g*g-a)/(2*g)) in te515' a (b-1) ng

