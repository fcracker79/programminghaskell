{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module TypeFamilyExample where


data V2

data V3


data P x a where
  P2 :: x -> x -> P x V2
  P3 :: x -> x -> x -> P x V3

type family Scale a b where
  Scale x V2 = (x, x)
  Scale x V3 = (x, x, x)

scale :: Num x => P x a -> Scale x a -> P x a
scale (P2 x y) (kx, ky) = P2 (x * kx) (y * ky)
scale (P3 x y z) (kx, ky, kz) = P3 (x * kx) (y * ky) (z * kz)


type family P' x a where
    P' x V2 = (x, x)
    P' x V3 = (x, x, x)

scale' :: Num x => P' x V2 -> Scale x V2 -> P' x V2
scale' (x, y) (kx, ky) = (x * kx, y * ky)

data S x a where
  S2 :: x -> x -> S x V2
  S3 :: x -> x -> x -> S x V3


scale'' :: Num x => P x a -> S x a -> P x a
scale'' (P2 x y) (S2 kx ky) = P2 (x * kx) (y * ky)
scale'' (P3 x y z) (S3 kx ky kz) = P3 (x * kx) (y * ky) (z * kz)
