{-# LANGUAGE Copatterns, GADTs #-}

import System.Environment (getArgs)

codata Stream a where
  Head :: Stream a -> a
  Tail :: Stream a -> Stream a

nthStream :: Int -> Stream a -> a
nthStream = { # 0 -> obs_Head
            ; # n -> nthStream (n-1) . obs_Tail }

countUp :: Int -> Stream Int
countUp n = { Head # -> n
            ; Tail # -> countUp (n+1) }

natStream :: Stream Int
natStream = countUp 1

filterStream :: (a -> Bool) -> Stream a -> Stream a
filterStream p s =
  let h = obs_Head s in
    case p h of
      True  -> { Head # -> h
               ; Tail # -> filterStream p (obs_Tail s) }
      False -> filterStream p (obs_Tail s)

siftStream :: Stream Int -> Stream Int
siftStream =
  { Head [# s] -> obs_Head s
  ; Tail [# s] -> siftStream
                . filterStream (\x -> mod x (obs_Head s) > 0)
                . obs_Tail $ s }

primeStream :: Stream Int
primeStream = siftStream (obs_Tail natStream)

main :: IO ()
main =
  do (x:_) <- getArgs
     print . nthStream (read x) $ primeStream
