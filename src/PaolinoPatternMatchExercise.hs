{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module PaolinoPatternMatchExercise where


import Protolude ( head )

data ABC x = A x | B x | C  deriving (Foldable, Show)

-- implementare senza pattern matching
quiz :: ABC x -> Maybe x 
quiz = Protolude.head

main :: IO ()
main = do
    let a = A 1
    let c = C::(ABC Int)
    Prelude.print $ quiz a
    Prelude.print $ quiz c

data ABC2 x = A2 x | B2 x | C2 deriving(Functor, Foldable, Traversable, Show)
quiz2 :: y -> ABC2 x -> (Maybe x, ABC2 y)
quiz2 y abcx = (Protolude.head abcx, A2 y)
