module HaskellInDepth.CH2.Relevants where


import Fmt
data Dino = Dino1 | Dino2 deriving(Show)


instance Buildable Dino where
    build Dino1 = "D1"
    build Dino2 = "D2"


main :: IO()
main = do
    fmt $ "This will be 'Dino1': " +|| Dino1 ||+ ", as from Show\n"
    fmt $ "This will be 'D1': "+| Dino1 |+ ", as from Buildable\n"


