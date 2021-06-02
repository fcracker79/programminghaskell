{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module HaskellInDepth.CH3.PaolinoApplicativeMealy where


import Control.Arrow (Arrow (arr, first), (&&&), (>>>))
import Control.Category ( Category(..) )
import Control.Foldl (Fold (Fold), fold)
import qualified Control.Foldl as L
import Protolude
    ( snd,
      Functor (fmap),
      Ord,
      Applicative(pure, (<*>), liftA2),
      Maybe(Nothing),
      notImplemented, undefined, fst, const, Any, uncurry, Semigroup ((<>)), ($), flip )
import Control.Comonad ( Functor )
import Control.Applicative ((<$>))
import Control.Lens (Profunctor(lmap))


{-
-- encoding of a mealy machine, notice the difference with Fold, a moore machine

data Fold a b  where 
  Fold :: (x -> a -> x) -> x -> (x -> b) -> Fold a b 

with Fold we can extract 'b' without applying 'a', Pointed instance , moore observability
OTOH vertical composition is hard, in particular the Category instance is probably not sound
(to be proved)
Vertical composition will make them act as one while the output of one goes into the input of the othe one
We would like to compose 
> Fold b c -> Fold a b -> Fold a c 
to do that we introduce another machine Mealy (Scan in foldl package) that produces 'b' only when fed with an 'a', non observability of mealy 
Mealy will compose better giving a 'c' every 'a' 

-}
data Mealy a b where
  Mealy :: (x -> a -> (x, b)) -> x -> Mealy a b

deriving instance Functor (Mealy a)

-- Mealy still have lateral composition via applicative 
instance Applicative (Mealy a) where 
  pure :: b -> Mealy a b
  pure b = Mealy 
    (\x _ -> (x, b))
    do ()
  (<*>) :: Mealy a (b -> c) -> Mealy a b -> Mealy a c
  Mealy f x <*> Mealy g y = Mealy (\(x', y') a' -> let (x'', f'') = f x' a'
                                                       (x''', b) = g y' a'
                                                       newstate = (x'', x''')
                                                   in (newstate, f'' b)
                                ) (x, y)

-- going from moore to mealy is easy , 
-- 1 . we loose observability on the first state
-- 2 . we use the observing function to output 'b' from x at every 'a'
mealy :: Fold a b -> Mealy a b
mealy (Fold f x g) = Mealy (\x' a' -> (f x' a', g x')) x

-- hard: implement *mealy* function without matching the Fold constructor
-- moore machines are comonads su we do not need to desctruture them here 
mealy2 :: Fold a b -> Mealy a b
mealy2 m  = Mealy
  do notImplemented 
  do m

-- going from mealy to moore wants back the initial observation
-- we could have an isomorphism if *mealy* was 
-- > mealy :: Fold a b -> (Mealy a b, b)
-- then 
-- > uncurry moore . mealy == identity
moore :: Mealy a b -> b -> Fold a b
moore (Mealy f x) b = Fold f' x' g'
  where 
    f' (x'', _) a'' = f x'' a''
    x' = (x, b)
    g' = do snd

-- vertical composition for mealy, very similar to the applicative but with result threading
instance Category Mealy where
  (.) :: Mealy b c -> Mealy a b -> Mealy a c
  Mealy f x . Mealy g y = Mealy ff (y, x)
    where ff (x0', x0'') a' = let (x', b) = g x0' a'
                                  (x'', c) = f x0'' b
                              in ((x', x''), c)
  id = Mealy (,) ()

-- and now we can compose Fold vertically buy lifting them to Mealy
minOf :: Ord b => Fold a b -> Fold a (Maybe b)
minOf f = moore
  do mealy L.minimum . mealy f
  do Nothing 

-- finally the parallel composition which is a superset of the applicative one 
instance Arrow Mealy where
  arr :: (a -> b) -> Mealy a b 
  arr f = Mealy
    do \x a -> (x, f a)
    do ()

  first :: Mealy a b -> Mealy (a,c) (b,c)
  first (Mealy f x) = Mealy ff g
    where ff x (a, c) = let (x', b) = f x a in (x, (b, c))
          g = do x

-- and now we can use arrows to freely compose Folds after lifting them to Mealys
-- in this case we want to compute min and max of every step of a fold
-- we can write this with applicative or arrow
minAndMaxOf :: Ord b => Fold a b  -> Fold a (Maybe (b, b))
minAndMaxOf f  = ff <$> moore (mealy f >>> mealy L.minimum &&& mealy L.maximum ) (Nothing, Nothing)
  where 
    c = moore (mealy f >>> mealy L.minimum &&& mealy L.maximum ) (Nothing, Nothing)
    c1 :: Ord b => Mealy b (Maybe b, Maybe b)
    c1 = mealy L.minimum &&& mealy L.maximum
    ff :: Applicative m => (m a, m b) -> m (a, b)
    ff = uncurry (liftA2 (,))


minApplicative :: forall a b. Ord b => Fold a b  -> Fold a (Maybe (b, b))
minApplicative f = moore q3 Nothing
  where 
    q :: Ord b => Fold b (Maybe b, Maybe b)
    q = (,) <$> L.minimum <*> L.maximum
    q2 :: Fold b (Maybe (b, b))
    q2 = uncurry (liftA2 (,)) <$> q
    q3 :: Mealy a (Maybe (b, b))
    q3 = mealy f >>> mealy q2
    q4 :: Mealy a (Maybe (b, b))
    q4 = mealy q2 . mealy f 



-- profunctor + applicative = arrow
myArrow :: forall a b a' b'. Fold a b -> Fold a' b' -> Fold (a, a') (b, b') 
myArrow x y = (,) <$> lmap fst x <*> lmap snd y

-- final: make an example of something you cannot do with applicative but you can with arrow
-- (***) operates on b as well, whereas applicative can only operate on c