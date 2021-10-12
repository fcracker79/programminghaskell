{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
module HaskellInDepth.CH11.TypeFamilies where

-- TYPE SYNONYMS

-- Let's disable injectivity
-- type family Simplify t = tt | tt -> t
type family Simplify t

type instance Simplify Integer = Integer
type instance Simplify Int = Integer
-- In case we activate injectivity, here we get:
-- Type family equation right-hand sides overlap; this violates
-- the family's injectivity annotation:
type instance Simplify Double = Integer
type instance Simplify String = String
type instance Simplify Char = String
type instance Simplify Bool = String


-- Not injective. Cannot do that
-- x :: [Simplify t]
-- x = undefined 


class Simplifier t where
    simplify :: t -> Simplify t

instance Simplifier Integer where
    simplify = id

instance Simplifier Int where
    simplify = fromIntegral

instance Simplifier Double where
    simplify = round

instance Simplifier String where
    simplify = id

instance Simplifier Bool where
    simplify = show

instance Simplifier Char where
    simplify = (:"")


-- DATA FAMILIES

data family XList a
newtype instance XList () = XListUnit Integer
data instance XList Bool = XBits Integer Integer

class XListable a where
    xempty :: XList a
    xcons :: a -> XList a -> XList a
    xheadMay :: XList a -> Maybe a


-- Since it is injective, I can do this
x :: [XList a]
x = undefined


instance XListable () where
    xempty = XListUnit 0
    xcons () (XListUnit n) = XListUnit (n + 1)
    xheadMay (XListUnit 0) = Nothing
    xheadMay _ = Just ()


instance XListable Bool where
    xempty = XBits 0 0
    xcons b (XBits bits n) = XBits (bits * 2 + if b then 1 else 0) (n + 1)
    xheadMay (XBits bits n)
        | n <= 0 = Nothing
        | otherwise = Just $ odd bits

-- ASSOCIATED FAMILIES

class Graph g where
    type Vertex g
    data Edge g
    src, tgt :: Edge g -> Vertex g
    outEdges :: g -> Vertex g -> [Edge g]


neighbors :: Graph g => g -> Vertex g -> [Vertex g]
neighbors g v = map tgt (outEdges g v)


-- SOME EXPERIMENTS

type family MyList a

type instance MyList Bool = (Int, Int)
type instance MyList () = Int


class MyListable a where
    myempty :: MyList a


instance MyListable () where
    myempty = 0


instance MyListable Bool where
    myempty = (0, 0)


z :: Int
z = myempty @()

z2 :: (Int, Int)
z2 = myempty @Bool