module PlayWithContinuation where


import Control.Exception (bracket, Exception)
import Control.Exception.Base (throwIO)
import System.IO (withFile, IOMode (WriteMode), Handle, hPutStrLn)

newtype Resource = Resource String
newtype Release = Release String
newtype UsageResult = UsageResult String

data MyError = MyError deriving(Show)

instance Exception MyError


instance Show Resource where
    show (Resource r) = "Resource " ++ r 


instance Show Release where
    show (Release r) = "Release " ++ r


instance Show UsageResult where
    show (UsageResult r) = "UsageResult " ++ r


myAcquire :: IO Resource 
myAcquire = do
    print "Acquiring resource"
    return $ Resource "<resource>"


myRelease :: Resource -> IO Release
myRelease r = do
    print $ "Releasing " ++ show r
    return $ Release "<release>"


myUseResource :: Resource -> IO UsageResult
myUseResource r = do
    print $ "Using " ++ show r
    return $ UsageResult $ show r

myFailUsingResouce :: Resource -> IO UsageResult
myFailUsingResouce _ = throwIO MyError

playWithBracket :: Bool -> IO UsageResult
playWithBracket fail = bracket myAcquire myRelease $ if fail then myFailUsingResouce else myUseResource


handleFile :: Handle -> IO ()
handleFile h = do
    hPutStrLn h "This is just an example"

playWithFile :: [Char] -> IO ()
playWithFile fileName = do
    withFile ("/tmp/" ++ fileName) WriteMode handleFile


newtype MyCont r a = MyCont ((a -> r) -> r)
mycont :: MyCont r a -> (a -> r) -> r
mycont (MyCont f) = f


-- mf = ( ( a -> b ) -> r) -> r ) = ff -> r = (fab -> r) -> r
-- ma = ( a -> r ) -> r = fa -> r
-- mb = ( b -> r) -> r = fb -> r
-- ff = ( a -> b ) -> r)
-- fab = a -> b
-- fa = a -> r
instance Applicative (MyCont r) where
    (MyCont mf) <*> (MyCont ma) = MyCont (\fb -> mf (\fab -> ma (\a -> fb (fab a)) ))
--                                                   ^       ^  ^                ^
--                                                   |       |  |---- a -> r ----|
--                                                   |       |-------- r --------|
--                                                   |------ (a -> b) -> r ------|
instance Monad (MyCont r) where
    MyCont ma >>= f = MyCont $ \fb -> ma (\a -> mycont (f a) fb)
