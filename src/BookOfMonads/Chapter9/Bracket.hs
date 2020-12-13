module BookOfMonads.Chapter9.Bracket where


import qualified Control.Exception as CE
import qualified System.IO.Error as SIE
import qualified Control.Monad.Except as CME
import qualified GHC.IO.Exception as GIE

acquire :: IO String
acquire = do
          print "acquire"
          return "myResource"

release :: String -> IO ()
release x = print $ "release of " ++ x

useWithFailure :: String -> IO ()
useWithFailure x = do
                   print $ "Attempting to use " ++ x
                   SIE.ioError(SIE.userError ("An error occurred while using " ++ x))


data MyError = MyError1 | MyError2 deriving(Show)
useWithThrowError :: String -> IO ()
useWithThrowError x = do
                      print $ "Attempting to use" ++ x
                      CME.throwError $ GIE.IOError Nothing GIE.IllegalOperation "loc" ("an error occurred while using" ++ x) Nothing Nothing

continuationWithPrint :: (String -> IO a) -> IO a
continuationWithPrint = CE.bracket acquire release

-- "acquire"
-- "Attempting to use myResource"
-- "release of myResource"
-- *** Exception: user error (An error occurred while using myResource)
continuationWithIOError :: IO ()
continuationWithIOError = continuationWithPrint useWithFailure

continuationWithSuccess :: IO Int
continuationWithSuccess = continuationWithPrint (return . length)


continuationWithThrowError :: IO ()
continuationWithThrowError = continuationWithPrint useWithThrowError
