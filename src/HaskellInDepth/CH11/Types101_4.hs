{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HaskellInDepth.CH11.Types101_4 where


-- Thanks to DataKinds, now TempUnits is a kind and F and C are types.
data TempUnits = F | C

-- This works because of KindSignatures
newtype Temp (u :: TempUnits) = Temp Double deriving (Num, Fractional)


instance Show (Temp F) where
    show (Temp d) = show d ++ " Farenheit"

instance Show F where
    show _ = "Farenheits"


-- Since F is now a type, I can use it in signatures
paperBurning :: Temp F
paperBurning = 451

absoluteZero :: Temp C
absoluteZero = -273.15

f2c :: Temp F -> Temp C
f2c (Temp f) = Temp ((f-32)*5/9)
