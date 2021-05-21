{-# LANGUAGE ExistentialQuantification #-}
module HaskellInDepth.CH4.NiceFolds where


import Data.Monoid
import Prelude hiding(sum)
import Data.Foldable


-- Simplest case
data Fold i o = forall m . Monoid m => Fold (i -> m) (m -> o)


fold :: (Foldable f, Functor f) => Fold i o -> f i -> o
fold (Fold tally summarize) is = summarize $ reduce $ fmap tally is
    where reduce = foldl' (<>) mempty


sum :: Num n => Fold n n
sum = Fold Sum getSum


-- More interesting example: Average

data Average a = Average { numerator :: !a, denominator :: !Int }

instance Num a => Semigroup (Average a) where
    (Average xL nL) <> (Average xR nR) = Average (xL + xR) (nL + nR)

instance Num a => Monoid (Average a) where
    mempty = Average 0 0


average :: Fractional a => Fold a a
average = Fold tally summarize
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
combine :: Fold i a -> Fold i b -> Fold i (a, b)
combine (Fold tallyL summarizeL) (Fold tallyR summarizeR) = 
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

instance Applicative (Fold i) where
    pure o = Fold (const ()) (const o)
    (<*>) f i = apply (combine f i)
        where apply (Fold i f) = Fold i ((\(ff, oo) -> ff oo) . f)
