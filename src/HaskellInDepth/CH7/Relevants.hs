{-# LANGUAGE TemplateHaskell #-}

module HaskellInDepth.CH7.Relevants where

import Control.Monad.Except ( when, MonadError(throwError, catchError), ExceptT, runExceptT )
import TextShow ( fromText, TextShow(showb, showt), Builder, toText, unlinesB )
import Data.Text ( Text, unpack, words )
import Control.Monad.State ( when, MonadState(get, put), gets, modify, State, evalState, StateT (StateT) )
import Control.Monad.Reader ( ReaderT (runReaderT), asks )
import Text.Read (readMaybe)
import Data.Foldable (traverse_)
import Control.Applicative
import Control.Exception
import GHC.IO.IOMode
import System.IO
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control
import Control.Monad.Logger
import Control.Monad.Trans
import Data.ByteString(pack)

-- newtype ExceptT e m a = ExceptT (m (Either e a))

{-
class Monad m => MonadError e m | m -> e where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a

instance [safe] Monad m => MonadError e (ExceptT e m)

-}


rpns :: [Text]
rpns = [
    "answer",
    "12 13 + 1",
    "2 +",
    "x y +",
    "1x +",
    "1 22 1 22 0 2 * * * * *",
    "10 1 2 + 2 2 1 2 * + * * * 1 x 2 + + +"]

type Stack = [Integer]
type EnvVars = [(Text, Integer)]

data EvalError =
      NotEnoughElements
    | ExtraElements
    | NotANumber Text
    | UnknownVar Text
    | MultipleErrors EvalError EvalError
    | NoError


instance Semigroup EvalError where
  e1 <> NoError = e1
  NoError <> e2 = e2
  e1 <> e2 = MultipleErrors e1 e2


instance Monoid EvalError where
  mempty = NoError


instance TextShow EvalError where
    showb NotEnoughElements = "Not enough elements in the expression"
    showb ExtraElements = "There are extra elements in the expression"
    showb (NotANumber t) = "Expression component '" <> fromText t <> "' is not a number"
    showb (UnknownVar t) = "Variable '" <> fromText t <> "' not found"
    showb (MultipleErrors e1 e2) = showb e1 <> showb e2
    showb NoError = undefined


type EvalM = ReaderT EnvVars (ExceptT EvalError (State Stack))

push :: Integer -> EvalM ()
push x = modify (x:)

pop :: EvalM Integer
pop = get >>= pop'
  where
    pop' :: Stack -> EvalM Integer
    pop' [] = throwError NotEnoughElements
    pop' (x:xs) = put xs >> pure x


oneElementOnStack :: EvalM ()
oneElementOnStack = do
  len <- gets length
  when (len /= 1) $ throwError ExtraElements


readVar :: Text -> EvalM Integer
readVar name = do
  var <- asks (lookup name)
  case var of
    Just n -> pure n
    Nothing -> throwError $ UnknownVar name

readNumber :: Text -> EvalM Integer
readNumber txt =
  case readMaybe (unpack txt)::(Maybe Integer) of
    Just n -> pure n
    _ -> throwError $ NotANumber txt


readSafe :: (Read a, Alternative m) => Text -> m a
readSafe str = maybe empty pure (readMaybe (unpack str))


evalRPNOnce ::Text -> EvalM Integer
evalRPNOnce str =
  clearStack >> traverse_ step (Data.Text.words str) >> oneElementOnStack >> pop
  where
    clearStack = put []
    step :: Text -> EvalM ()
    step "+" = processTops (+)
    step "*" = processTops (*)
    step "-" = processTops (-)
    step t = readSafe t >>= push
    processTops op = flip op <$> pop <*> pop >>= push


reportEvalResults :: Either EvalError [Builder] -> Text
reportEvalResults (Left e) = "Error: " <> showt e
reportEvalResults (Right b) = toText $ unlinesB b

evalRPNMany :: [Text] -> EnvVars -> Text
evalRPNMany txts env = reportEvalResults $ evalState (runExceptT (runReaderT (mapM evalOnce txts) env)) []
  where
    evalOnce txt = (fromText txt <>) <$> (buildOk <$> evalRPNOnce txt) `catchError` (pure . buildErr)
    buildOk res = " = " <> showb res
    buildErr err = " Error: " <> showb err

-- RUNTIME Errors

bracketAcquire :: String -> IO Handle
bracketAcquire = flip openFile ReadMode

bracketRelease :: Handle -> IO ()
bracketRelease = hClose

bracketUse :: Handle -> IO String
bracketUse = hGetContents

stupidReadFile :: String -> IO String
stupidReadFile filename = bracket
  (bracketAcquire filename)
  bracketRelease
  bracketUse


bracketAcquireUnliftIO :: MonadUnliftIO m => String -> m Handle
bracketAcquireUnliftIO = undefined

bracketReleaseUnliftIO :: MonadUnliftIO m => Handle -> m ()
bracketReleaseUnliftIO = undefined

bracketUseUnliftIO :: MonadUnliftIO m => Handle -> m String
bracketUseUnliftIO = undefined

bracketUnliftIO filename = bracket 
  (bracketAcquireUnliftIO filename) 
  bracketReleaseUnliftIO 
  bracketUseUnliftIO


liftbracket :: MonadUnliftIO m => forall a b c. m a -> (a -> m b) -> (a -> m c) -> m c
liftbracket acquire release use = withRunInIO (\runInIO -> bracket (runInIO acquire) (runInIO . release) (runInIO . use))

lifttry :: (Exception e, MonadUnliftIO m) => m a -> m (Either e a)
lifttry ma = withRunInIO (\runInIO -> try (runInIO ma))

lifttryJust :: (Exception e, MonadUnliftIO m) => (e -> Maybe b) -> m a -> m (Either b a)
lifttryJust f ma = withRunInIO (\runInIO -> tryJust f (runInIO ma))
