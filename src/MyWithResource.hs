module MyWithResource where

import Control.Exception (finally, catch, throwIO)

newtype  MyResource a = MyResource a deriving(Show)
newtype MyHandle a = MyHandle a deriving(Show)

withMyResouce :: Show a => MyResource a -> (MyHandle a -> IO r) -> IO r
withMyResouce (MyResource a) f = do
    print $ "Acquiring resouce " ++ show a
    let h = MyHandle a
    f h `finally` error h
    -- print $ "Releasing resouce " ++ show h ++ " after success" 
    -- return result
    where 
        error :: Show a => MyHandle a -> IO ()
        error h = do
            print $ "Releasing resouce " ++ show h


doStuff :: Show a => MyHandle a -> IO String
doStuff a = do
    print $ "Using handle " ++ show a
    return $ "Done with " ++ show a


failDoingStuff :: Show a => MyHandle a -> IO String
failDoingStuff a = do
    print $ "Using handle " ++ show a ++ " with failure"
    throwIO $ userError "My error" 
    return $ "Hopefully unreachable code using " ++ show a


main :: IO ()
main = do
    result <- withMyResouce (MyResource "This is my resouce") doStuff
    print $ "I received the result " ++ result

    result <- withMyResouce (MyResource "This is my resouce which will fail") failDoingStuff
    print $ "I received another result, which I should never have as I expect it to have a failure " ++ result
