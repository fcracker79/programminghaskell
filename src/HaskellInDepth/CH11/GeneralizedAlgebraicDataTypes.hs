{-# LANGUAGE GADTs #-}
module HaskellInDepth.CH11.GeneralizedAlgebraicDataTypes() where


data DynValue a where
    S :: String -> DynValue String
    C :: Char -> DynValue Char
    B :: Bool -> DynValue Bool


data WrappedDynValue where
    Wrap :: DynValue a -> WrappedDynValue


getValue :: DynValue a -> a
getValue (B b) = b
getValue (C c) = c
getValue (S s) = s



instance Show a => Show (DynValue a) where
    show = show . getValue


x :: Show a => [DynValue a]
x = undefined


printValue :: DynValue a -> IO ()
printValue (B b) = print b
printValue (C c) = print c
printValue (S s) = print s

y :: IO ()
y = mapM_ printValue [S "a"]

y2 :: IO ()
y2 = mapM_ printValue [B True]


z :: [WrappedDynValue]
z = [Wrap (B True), Wrap (S "hello")]


printWDValue :: WrappedDynValue -> IO ()
printWDValue (Wrap dv) = printValue dv
