-- {-# LANGUAGE NoRankNTypes #-}
module HaskellInDepth.CH11.ArbitraryRank where


-- Without RankNTypes we would get the following:
--   Illegal polymorphic type: forall a. Num a => a -> a
--     Perhaps you intended to use RankNTypes
newtype NumModifier = NumModifier {
    run :: forall a. Num a => a -> a
}

processInts :: NumModifier -> [Int] -> [Int]
processInts nm xs = map (run nm) xs


f :: Int -> Int
f = undefined

-- x :: NumModifier
-- Couldn't match type ‘a’ with ‘Int’
--   ‘a’ is a rigid type variable bound by
--     a type expected by the context:
--       forall a. Num a => a -> a
-- x = NumModifier f