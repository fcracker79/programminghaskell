{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE InstanceSigs #-}

module HaskellInDepth.CH3.PaolinoApplicativeMealy where


import Control.Arrow (Arrow (arr, first), (&&&))
import Control.Category ( Category(..) )
import Control.Foldl (Fold (Fold))
import qualified Control.Foldl as L
import Protolude
    ( snd,
      Functor,
      Ord,
      Applicative(pure, (<*>)),
      Maybe(Nothing),
      notImplemented, undefined, fst, const )
import Control.Comonad ( Functor )


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
minAndMaxOf f  = notImplemented

-- final: make an example of something you cannot do with applicative but you can with arrow
