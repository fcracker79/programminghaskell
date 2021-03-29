module HaskellProgrammingFromFirstPrinciples.PlayWithIO where


import Control.Concurrent ( newEmptyMVar, putMVar, takeMVar, MVar )
import GHC.IO (unsafePerformIO)
myData :: IO (MVar Int)
myData = newEmptyMVar
main :: IO ()
main = do
    mv <- myData
    --putMVar mv 0
    mv' <- myData
    zlaero <- takeMVar mv'
    print zero


unsafeMyData :: MVar Int
unsafeMyData = unsafePerformIO newEmptyMVar

unsafeMain :: IO ()
unsafeMain = do
    let mv = unsafeMyData
    putMVar mv 0
    let mv' = unsafeMyData
    zero <- takeMVar mv'
    print zero
