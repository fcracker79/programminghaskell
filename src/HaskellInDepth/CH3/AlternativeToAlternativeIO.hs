module HaskellInDepth.CH3.AlternativeToAlternativeIO where

import Control.Monad.Catch (MonadCatch, catchAll)
import Control.Monad.Cont (ContT (ContT), runContT)
import Data.Text (Text)
import Protolude (putText, void)

catch' :: MonadCatch m => m r -> ContT r m ()
catch' = void . ContT . catchAll

alternativeIO :: Bool -> Bool -> IO ()
alternativeIO fail1 fail2 = flip runContT pure $ do
  catch' $ if fail1 then ioError (userError "Failed 1") else print "OK1"
  catch' $ if fail2 then ioError (userError "Failed 2") else print "OK2"
  putText "all failed"
