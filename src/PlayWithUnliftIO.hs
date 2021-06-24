module PlayWithUnliftIO where


import Control.Monad.IO.Unlift ( MonadUnliftIO(withRunInIO) )
import Control.Monad.Trans.Maybe ( MaybeT )
import Control.Monad.Reader ( ReaderT )

f :: ReaderT String IO Int -> IO Int
f = undefined

g :: (ReaderT String IO Int -> IO Int) -> IO Char
g = undefined


h :: ReaderT String IO Char
h = withRunInIO g
