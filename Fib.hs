{-# LANGUAGE Copatterns, GADTs #-}

import System.Environment (getArgs)

codata Stream a where
  Head :: Stream a -> a
  Tail :: Stream a -> Stream a

nthStream :: Int -> Stream a -> a
nthStream = { # 0 -> obs_Head
            ; # n -> nthStream (n-1) . obs_Tail }

zipWithStream :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWithStream =
  { Head [# f as bs] -> f (obs_Head as) (obs_Head bs)
  ; Tail [# f as bs] -> zipWithStream f (obs_Tail as) (obs_Tail bs) }

fibStream :: Stream Integer
fibStream =
  { Head #        -> 1
  ; Head [Tail #] -> 1
  ; Tail [Tail #] -> zipWithStream (+) fibStream (obs_Tail fibStream) }

main :: IO ()
main =
  do (x:_) <- getArgs
     print . nthStream (read x) $ fibStream
