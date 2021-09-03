{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
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
