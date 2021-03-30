module HaskellProgrammingFromFirstPrinciples.PlayWithSeq where


import qualified Data.Sequence as S
import Data.Sequence( Seq(..))
myseq :: S.Seq String
myseq = S.fromList $ fmap show [1..100]


main :: IO ()
main = do
    putStr "Initial sequence: "
    print myseq
    putStr "Adding an element to the head: "
    print $ "HEAD" :<| myseq
    putStr "Adding an element to the end: "
    print $ myseq :|> "END"
