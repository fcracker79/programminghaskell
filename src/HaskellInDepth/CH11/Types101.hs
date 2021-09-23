{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HaskellInDepth.CH11.Types101 where


-- PHANTOM TYPES
import Data.Proxy ( Proxy(..) )
import GHC.Types (KindRep)
newtype Temp unit = Temp Double deriving (Num, Fractional)
data F
data C
data K

paperBurning :: Temp F
paperBurning = 451

absoluteZero :: Temp C
absoluteZero = -273.15


f2c :: Temp F -> Temp C
f2c (Temp f) = Temp ((f-32)*5/9)

c2f :: Temp C -> Temp F
c2f (Temp c) = Temp(c * 9 / 5 + 32)

c2k :: Temp C -> Temp K
c2k (Temp c) = Temp (c - 273.15)

f2k :: Temp F -> Temp K
f2k = c2k . f2c


-- PROXIES
-- I can use this function by passing a casted Proxy term: since `unitName` does not depend on the value,
-- I can safely pass a (Proxy :: Proxy <type>) as an argument to execute a specific function.
class UnitName u where
    unitName :: Proxy u -> String


instance UnitName C where
    unitName _ = "C"

instance UnitName F where
    unitName _ = "F"

instance UnitName K where
    unitName _ = "K"

instance UnitName unit => UnitName (Temp unit) where
    unitName _ = unitName (Proxy :: Proxy unit)

instance UnitName unit => Show (Temp unit) where
    show (Temp t) = show t ++ "°" ++ unitName (Proxy :: Proxy unit)

main :: IO ()
main = do
    -- Couldn't match type ‘C’ with ‘F’
    -- let x = paperBurning - absoluteZero
    let x = f2c paperBurning - absoluteZero
    print x
