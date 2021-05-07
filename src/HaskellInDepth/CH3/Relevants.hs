{-# LANGUAGE RecordWildCards #-}
module HaskellInDepth.CH3.Relevants where


import Fmt
import Data.Text

-- Great `time` package


-- `cassava` for CSV


-- `fmt` package


{-
(|+) :: (Buildable a, Fmt.Internal.Core.FromBuilder b) => a -> Builder -> b
(+|) :: Fmt.Internal.Core.FromBuilder b => Builder -> Builder -> b
(||+) :: (Show a, Fmt.Internal.Core.FromBuilder b) => a -> Builder -> b
(+||) :: Fmt.Internal.Core.FromBuilder b => Builder -> Builder -> b

`b` is always a `FromBuilder`.

Double piped functions take a `Show` as variable `a`.
Single piped functions taks a `Builder` as variable `a`.

-}
data Dino = Dino {a:: Int, b:: Int}


instance Buildable Dino where
    build (Dino a b) = "Dino (" +| a |+ ", " +| b |+ ")"


-- fmt is polymorphic
xFmt :: String 
xFmt = fmt $ "hello " +| Dino 1 2 |+ "world"
yFmt :: Text 
yFmt = fmt $ "hello " +| Dino 1 2 |+ "world"


-- RecordsWildcards can be used to build instances
xWildCards :: Dino
xWildCards = 
    let a = 1
        b = 2
    in Dino{..}
