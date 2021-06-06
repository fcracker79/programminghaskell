module HaskellInDepth.CH5.IORefExample where


import Data.IORef ( modifyIORef', newIORef, readIORef, IORef )
import Text.Read (readMaybe)

import Data.Foldable (traverse_)
import System.Environment (getArgs)
import System.Directory.Extra (listContents, doesDirectoryExist, doesFileExist)
import Control.Monad.Extra (whenM, ifM, zipWithM_)


sumNumbers :: IO Int
sumNumbers = do
        s <- newIORef 0
        go s
    where
        go :: IORef Int -> IO Int 
        go acc = readNumber >>= processNumber acc
        
        readNumber :: IO (Maybe Int)
        readNumber = do
            putStr "Put integer number (not a number to finish): "
            readMaybe <$> getLine

        processNumber :: IORef Int -> Maybe Int -> IO Int
        processNumber acc Nothing = readIORef acc
        processNumber acc (Just n) = modifyIORef' acc (+ n) >> go acc


fileCount :: FilePath -> IO Int
fileCount fpath = do
    counter <- newIORef 0
    whenM (doesDirectoryExist fpath) $ go counter fpath
    readIORef counter
    where
        go :: IORef Int -> FilePath -> IO ()
        go cnt fp = listContents fp >>= traverse_ (processEntry cnt)
        processEntry :: IORef Int -> FilePath -> IO ()
        processEntry cnt fp = ifM (doesDirectoryExist fp) (go cnt fp) (inc cnt)
        inc :: IORef Int -> IO ()
        inc cnt = modifyIORef' cnt (+ 1)


fileCountMirko :: FilePath -> IO Int
fileCountMirko fpath = do
    ifM (doesDirectoryExist fpath) (recurse fpath)
        (ifM (doesFileExist fpath) (return 1) (return 0))
    where recurse _fpath = do
                                subpaths <- listContents _fpath
                                let counts = fmap fileCountMirko subpaths
                                mcounts <- sequence counts
                                return $ sum mcounts

fileCountMirko2 :: FilePath -> IO Int
fileCountMirko2 fpath = do
    ifM (doesDirectoryExist fpath) (sum <$> (listContents fpath >>= traverse fileCountMirko2))
        (ifM (doesFileExist fpath) (return 1) (return 0))
