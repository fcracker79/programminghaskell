{-# LANGUAGE ExistentialQuantification #-}
module HaskellInDepth.CH4.NiceFolds where


import Data.Monoid
import qualified Prelude as P
import Prelude hiding(sum, product, length)
import Data.Foldable hiding(sum, fold, product, length)
import Control.Applicative
import Control.Parallel.Strategies


-- Simplest case
data Fold i o = forall m . Monoid m => Fold (i -> m) (m -> o)


fold :: (Foldable f, Functor f) => Fold i o -> f i -> o
fold (Fold tally summarize) is = summarize $ reduce $ fmap tally is
    where reduce = foldl' (<>) mempty


sum :: Num n => Fold n n
sum = Fold Sum getSum

product :: Num n => Fold n n
product = Fold Product getProduct

-- More interesting example: Average

data Average a = Average { numerator :: !a, denominator :: !Int }

instance Num a => Semigroup (Average a) where
    (Average xL nL) <> (Average xR nR) = Average (xL + xR) (nL + nR)

instance Num a => Monoid (Average a) where
    mempty = Average 0 0


averageOLD :: Fractional a => Fold a a
averageOLD = Fold tally summarize
    where 
        tally x = Average x 1
        summarize (Average numerator denominator) = numerator / fromIntegral denominator


head :: Fold a (Maybe a)
head = Fold (First . Just) getFirst

tail :: Fold a (Maybe a)
tail = Fold (Last . Just) getLast

all :: (a -> Bool) -> Fold a Bool 
all predicate = Fold (All . predicate) getAll

any :: (a -> Bool) -> Fold a Bool 
any predicate = Fold (Any . predicate) getAny


length :: Num n => Fold i n
length = Fold (\_ -> Sum 1) getSum


-- Exponential moving average
data EMA a = EMA { samples :: Int, value :: !a }

instance Fractional a => Semigroup (EMA a) where
    (EMA nL xL) <> (EMA 1 xR) = EMA n x
        where
            n = nL + 1
            x = xL * 0.7 + xR
    (EMA nL xL) <> (EMA nR xR) = EMA n x
        where
            n = nL + 1
            x = xL * (0.7 ^ nR) + xR

instance Fractional a => Monoid (EMA a) where
    mempty = EMA 0 0


ema :: Fractional a => Fold a a
ema = Fold tally summarize
    where 
        tally x = EMA 1 x
        summarize (EMA _ x) = x * 0.3


-- Combining folds

-- Tuples are monoids, provided that their elements are as well.
combineOld :: Fold i a -> Fold i b -> Fold i (a, b)
combineOld (Fold tallyL summarizeL) (Fold tallyR summarizeR) = 
    Fold tally summarize
        where 
            tally x = (tallyL x, tallyR x)
            summarize (l, r) = (summarizeL l, summarizeR r)

-- Applicative Fold

instance Functor (Fold i) where
    fmap f (Fold i o) = Fold i (f . o)

-- instance Applicative (Fold i) where
--     pure o = Fold (const ()) (const o)
--     (<*>) (Fold tallyF summarizeF) (Fold tallyX summarizeX) = Fold tally summarize
--         where
--             -- Given an 'i', I get both monoids from the two folds... 
--             tally i = (tallyF i, tallyX i)
--             -- ...then the first summarize provides a FUNCTION, whereas the second one produces its argument
--             summarize (mF, mX) = summarizeF mF (summarizeX mX)

-- instance Applicative (Fold i) where
--     pure o = Fold (const ()) (const o)
--     (<*>) f i = apply (combine f i)
--         where apply (Fold i f) = Fold i ((\(ff, oo) -> ff oo) . f)

-- Pair

data Pair a b = P !a !b


instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
    (<>) (P a1 b1) (P a2 b2) = P (a1 <> a2) (b1 <> b2)

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
    mempty = P mempty mempty


-- Applicative using above defined Pair
instance Applicative (Fold i) where
    pure o = Fold (const ()) (const o)
    Fold tallyF summarizeF <*> Fold tallyX summarizeX = Fold tally summarize 
        where 
            tally i = P (tallyF i) (tallyX i)
            summarize (P oF oX) = summarizeF oF (summarizeX oX) 


combine :: Fold i a -> Fold i b -> Fold i (a, b)
combine = liftA2 (,)

-- Using applicative interface


sumAndProduct :: Num a => [a] -> (a, a)
sumAndProduct = fold ((,) <$> sum <*> product)

-- This is BAD as, due to the product, the array elements cannot be garbage collected while proceeding with the sum.
badSumAndProduct :: (Foldable t, Num b) => t b -> (b, b)
badSumAndProduct xs = (P.sum xs, P.product xs)


-- Numerical instance
instance Num b => Num (Fold a b) where
    fromInteger n = pure (fromInteger n)
    negate = fmap negate 
    abs = fmap abs
    signum = fmap signum
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

instance Fractional b => Fractional (Fold a b) where
    fromRational n = pure (fromRational n)
    recip = fmap recip
    (/) = liftA2 (/)

instance Floating b => Floating (Fold a b) where
    pi = pure pi
    exp = fmap exp
    sqrt = fmap sqrt
    log = fmap log
    sin = fmap sin
    cos = fmap cos
    tan = fmap tan
    asin = fmap asin
    acos = fmap acos
    atan = fmap atan
    sinh = fmap sinh
    cosh = fmap cosh
    tanh = fmap tanh
    acosh = fmap acosh
    asinh = fmap asinh
    atanh = fmap atanh

    (**) = liftA2 (**)
    logBase = liftA2 logBase

average :: Fold Double Double
average = sum / length

compositionOfFoldExample :: Double
compositionOfFoldExample = fold (cos average ^ 2 + sin average ^ 2) [1..10]

-- We use Num instance of Fold
wat :: Integer
wat = fold 99 [1..10]


standardDeviation :: Fold Double Double
standardDeviation = sqrt (sumOfSquares / length - (sum / length) ^ 2)
    where sumOfSquares = Fold (Sum . (^ 2)) getSum


-- Consume in parallel
foldlParallel :: Fold i o -> [[i]] -> o
foldlParallel (Fold tally summarize) iss = 
        summarize ( reduce (map inner iss `using` parList rseq ) )
    where 
        reduce = Data.Foldable.foldl' mappend mempty
        inner is = reduce (map tally is)
