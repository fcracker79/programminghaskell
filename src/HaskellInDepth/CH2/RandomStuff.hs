module HaskellInDepth.CH2.RandomStuff where


import System.Random


main :: IO ()
main = do
    m1 <- getStdRandom uniform :: IO Int
    m2 <- getStdRandom uniform :: IO Int
    print $ "m1:" ++ show m1
    print $ "m2:" ++ show m2
    m3 <- getStdRandom (uniformR (0, 100)) :: IO Int
    m4 <- getStdRandom (uniformR (0, 100)) :: IO Int
    print $ "m3:" ++ show m3
    print $ "m4:" ++ show m4
    g <- getStdGen 
    let m5 :: Int
        (m5, g2) = uniform g
    print $ "m5:" ++ show m5
    let m6 :: Int
        (m6, g3) = uniform g
    print $ show m6 ++ "Same as m5, as we are using the same StdGen instance"

    let m5 :: Int
        (m5, g2) = uniform g
    print m5
    let m6 :: Int
        (m6, g3) = uniform g
    print m6
    