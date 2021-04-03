{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilyDependencies #-}

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

pino :: P Integer V2
pino = P2 1 2

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


data Shape x where
  Circle :: x -> Shape x
  Square :: x -> Shape x
  Rectangle :: x -> x -> Shape x


area :: Shape Double -> Double 
area (Circle x) = pi * x * x
area (Square x) = x * x
area (Rectangle x y) = x * y


data Shape2 x = Circle2 x | Square2 x | Rectangle2 x x

area2 :: Shape2 Double -> Double 
area2 (Circle2 x) = pi * x * x
area2 (Square2 x) = x * x
area2 (Rectangle2 x y) = x * y


data P_NOGADT x a = P2_NOGADT x x | P3_NOGADT x x x
data S_NOGADT x = S2_NOGADT x x | S3_NOGADT x x x
scaleNoGADT :: Num x => P_NOGADT x a -> S_NOGADT x -> P_NOGADT x a
scaleNoGADT (P2_NOGADT x y) (S2_NOGADT kx ky) = P2_NOGADT (x * kx) (y * ky)
scaleNoGADT (P3_NOGADT x y z) (S3_NOGADT kx ky kz) = P3_NOGADT (x * kx) (y * ky) (z * kz)


data TYPE_CIRCLE
data TYPE_RECTANGLE
data TYPE_SQUARE


data Shape3 x a where
  Circle3 :: x -> Shape3 x TYPE_CIRCLE
  Rectangle3 :: x -> x -> Shape3 x TYPE_RECTANGLE
  Square3 :: x -> Shape3 x TYPE_SQUARE


type family Shape4 x a where
  Shape4 x TYPE_CIRCLE = x
  Shape4 x TYPE_RECTANGLE = (x, x)
  Shape4 x TYPE_SQUARE = x

circle3 :: Shape3 Integer TYPE_CIRCLE
circle3 = Circle3 12

circle4 :: Shape4 Integer TYPE_CIRCLE
circle4 = 12
