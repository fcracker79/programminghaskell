module HaskellInDepth.CH6.Examples where


import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer
import Control.Monad.Reader


f :: StateT String (MaybeT IO) String
f = do
    liftIO $ print "hello"
    return "world"

-- g :: StateT String (MaybeT (Either Integer)) String
-- g = do
--     liftIO $ print "hello"
--     return "world"


data Dino = Dino Integer


g :: Dino -> Integer 
g = undefined

newtype Dino2 = Dino2 Dino

x :: Dino2
x = Dino2 $ Dino 1

-- Nope
-- y = g x
