module HaskellInDepth.CH4.PlayWithComonad where


import Control.Comonad ( Comonad(extend) )
data A = A
data R = R
data B = B

instance Semigroup R where
    R <> R = R

instance Monoid R where
    mempty = R


f :: R -> A
f = undefined

comonadExample :: ((R -> A) -> B) -> R -> B
comonadExample ff = extend ff f

monadExample :: (A -> (R -> B)) -> R -> B
monadExample ff = f >>= ff
