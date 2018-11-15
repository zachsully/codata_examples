{-# LANGUAGE Copatterns #-}
module Set where

codata Set where
  IsEmpty  :: Set -> Bool
  Contains :: Set -> Int -> Bool
  Insert   :: Set -> Int -> Set
  Union    :: Set -> Set -> Set

finiteSet :: [Int] -> Set
finiteSet =
  { IsEmpty  [# xs]   -> xs == []
  ; Contains [# xs] y -> elem y xs
  ; Insert   [# xs] y -> finiteSet (y:xs)
  ; Union    [# xs] s -> foldr (\x t -> obs_Insert t x) s xs }

emptySet :: Set
emptySet = finiteSet []

evenSetWith :: Set -> Set
evenSetWith =
  { IsEmpty  [# s]   -> False
  ; Contains [# s] y -> even y || obs_Contains s y
  ; Insert   [# s] y -> evenSetWith (obs_Insert s y)
  ; Union    [# s] t -> evenSetWith (obs_Union s t) }

evenSet :: Set
evenSet = evenSetWith emptySet
