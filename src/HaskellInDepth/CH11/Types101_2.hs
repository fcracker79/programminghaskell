{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module HaskellInDepth.CH11.Types101_2 where

-- Can't make a derived instance of ‘Num (Temp unit)’:
-- ‘Num’ is not a stock derivable class (Eq, Show, etc.)
newtype Temp unit = Temp Double deriving (Num, Fractional)
data F
data C
data K


class UnitName u where
    -- The type variable ‘u0’ is ambiguous. So I have to use AllowAmbiguousTypes
    unitName :: String

instance UnitName C where
    unitName = "C"

instance UnitName F where
    unitName = "F"

-- Expecting one more argument to ‘Temp’
-- Expected a type, but ‘Temp’ has kind ‘* -> *’
-- So I have to use PolyKinds
instance UnitName Temp where
    unitName = "_unspecified unit_"

instance UnitName u => UnitName (Temp u) where
    -- Pattern syntax in expression context: unitName@u. So I have to use TypeApplications
    -- Not in scope: type variable ‘u’. SO I have to use ScopedTypeVariables
    unitName = unitName @u


dino :: (Show a, Show b) => a -> b -> String
dino a b = "a is " ++ show a ++ ", b is " ++ show b

dinoInt :: Show b => Int -> b -> String
dinoInt = dino @Int

dinoIntString :: Int -> String -> String
dinoIntString = dino @Int @String

data Proxy x = Proxy

class DinoProxy u where
    dinoProxy :: Proxy u -> String

instance DinoProxy String where
    dinoProxy _ = "stringa"

instance DinoProxy Int where
    dinoProxy _ = "intero"


class DinoTypeVar u where
    dinoTypeVar :: String

instance DinoTypeVar String where
    dinoTypeVar = "stringa"

instance DinoTypeVar Int where
    dinoTypeVar = "intero"

main :: IO ()
main = do
    putStrLn "Using proxy"
    putStrLn $ dinoProxy (Proxy :: Proxy Int)
    putStrLn "Using type applications"
    putStrLn $ dinoTypeVar @Int
