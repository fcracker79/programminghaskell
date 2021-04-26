module HaskellInDepth.CH2.Numbers where


import Data.Fixed (Fixed, HasResolution(..))
class (Num a) => MyDiv a where 
    mydiv :: a -> a -> a


instance MyDiv Integer where
    mydiv = div


instance MyDiv Double where
    mydiv = (/)


instance MyDiv Int where
    mydiv = div


data E4

instance HasResolution E4 where
    resolution _ = 10000


type Fixed4 = Fixed E4