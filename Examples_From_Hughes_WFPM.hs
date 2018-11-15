{-# LANGUAGE Copatterns #-}
module Hughes where

import Prelude hiding ((!!),sqrt)

{-
Stream codata type and helpers
-}
codata Stream a where
  Head :: Stream a -> a
  Tail :: Stream a -> Stream a

instance Functor Stream where
  fmap f s =
    { Head # -> f (obs_Head s)
    ; Tail # -> fmap f (obs_Tail s) }

zipWithS :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWithS f s r =
  { Head # -> f (obs_Head s) (obs_Head r)
  ; Tail # -> zipWithS f (obs_Tail s) (obs_Tail r) }

iterateS :: (a -> a) -> a -> Stream a
iterateS f x =
  { Head # -> x
  ; Tail # -> iterateS f (f x) }

(!!) :: Stream a -> Int -> a
s !! 1 = obs_Head s
s !! n = (obs_Tail s) !! (n-1)

--------------------------------------------------------------------------------
--                 Newton-Raphson Square Roots (pg 9-10)                      --
--------------------------------------------------------------------------------

sqrt' :: Double -> Double -> Stream Double
sqrt' x y = iterateS (\a -> (a + (x/a)) / 2) y

sqrt :: Int -> Stream Double
sqrt x = sqrt' (fromIntegral x) 1

-- projects out the first element that is less than epsilon different than the
-- previous.
within :: (Num a, Ord a) => a -> Stream a -> a
within eps s =
  let a = obs_Head s
      b = obs_Head (obs_Tail s) in
    case abs (a - b) <= eps of
      True -> b
      False -> within eps (obs_Tail s)

-- projects out the first element who's ratio to the previous estimation is
-- epsilon from 1
relative :: (Fractional a, Ord a) => a -> Stream a -> a
relative eps s =
  let a = obs_Head s
      b = obs_Head (obs_Tail s) in
    case abs (a/b - 1) <= eps of
      True -> b
      False -> within eps (obs_Tail s)

sqrt_within :: Int -> Double -> Double
sqrt_within x eps = within eps (sqrt x)

sqrt_relative :: Int -> Double -> Double
sqrt_relative x eps = relative eps (sqrt x)

--------------------------------------------------------------------------------
--                     Numeric Differentiation (pg 11-14)                     --
--------------------------------------------------------------------------------
{- In this section, Hughes often refers to "sequences of approximations". To us,
   these are the codata type Stream.
-}

fun1,fun2 :: Fractional a => a -> a
fun1 x = 2*x + 1
fun2 x = x*x

easydiff :: Fractional a => (a -> a) -> a -> a -> a
easydiff f x h = ((f (x + h)) - (f x)) / h

differentiate' :: Fractional a => a -> (a -> a) -> a -> Stream a
differentiate' h0 f x = fmap (easydiff f x) (iterateS (\x -> x/2) h0)

differentiate :: Fractional a => (a -> a) -> a -> Stream a
differentiate f x = differentiate' 1 f x

--
--- We can use the helper functions ~elimerror~, ~order~, and ~improve~ to
--- converge faster
elimerror :: Floating a => Int -> Stream a -> Stream a
elimerror n s =
  { Head # ->
      let a = obs_Head s
          b = obs_Head (obs_Tail s) in
        ((b * (2**(fromIntegral n))) - a)/(2**(fromIntegral n) - 1)
  ; Tail # -> elimerror n (obs_Tail s) }

order :: Stream Double -> Int
order s =
  let a = obs_Head s
      b = obs_Head (obs_Tail s)
      c = obs_Head (obs_Tail (obs_Tail s)) in
    floor (logBase 2 ((a-c)/(b-c) - 1))

improve :: Stream Double -> Stream Double
improve s = elimerror (order s) s

-- Hughes calls this ~second~ which removes every other element
thin :: Stream a -> Stream a
thin s = { Head # -> obs_Head s
         ; Tail # -> obs_Tail (obs_Tail s) }
----
--

fastDiff :: (Double -> Double) -> Double -> Stream Double
fastDiff f x = thin $ differentiate f x

--------------------------------------------------------------------------------
--                       Numeric Integration (pg 14-15)                       --
--------------------------------------------------------------------------------
easyIntegrate :: Fractional a => (a -> a) -> a -> a -> a
easyIntegrate f a b = (f a + f b) * (b - a) / 2

integrate ::  Fractional a => (a -> a) -> a -> a -> Stream a
integrate f a b =
  { Head # -> easyIntegrate f a b
  ; Tail # ->
      let mid = (a+b) / 2 in
        zipWithS (+) (integrate f a mid) (integrate f mid b)
  }

fastIntegrate :: (Double -> Double) -> Double -> Double -> Stream Double
fastIntegrate f a b = thin . improve $ integrate f a b


--------------------------------------------------------------------------------
--                         Playing Games (pg 16-22)                           --
--------------------------------------------------------------------------------

codata Tree a where
  Node     :: Tree a -> a
  Children :: Tree a -> [Tree a]

instance Functor Tree where
  fmap f t =
    { Node #     -> f (obs_Node t)
    ; Children # -> map (fmap f) (obs_Children t) }

data Board

moves :: Board -> [Board]
moves = undefined

gametree :: Board -> Tree Board
gametree =
  { Node     [# b] -> b
  ; Children [# b] -> map gametree (moves b) }

-- remove subtrees of a certain depth
prune :: Int -> Tree s -> Tree s
prune = { Node     [# _ t] -> obs_Node t
        ; Children [# 0 _] -> []
        ; Children [# d t] -> map (prune (d-1)) (obs_Children t) }

score :: Board -> (Board,Int)
score = undefined

-- Returns a Maybe Board because there is no next move in a final gamestate
maximize :: Tree (Board,Int) -> Maybe Board
maximize = undefined

evaluate :: Board -> Maybe Board
evaluate = maximize . fmap score . prune 6 . gametree
