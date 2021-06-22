module HaskellInDepth.CH6.EvalRPN where


import Control.Monad.State
    ( guard, StateT, MonadState(put, get), evalStateT, gets, modify )
import Data.Foldable ( traverse_ )
import Control.Applicative ( Alternative(empty) )
import Text.Read(readMaybe)


type Stack = [Integer]
type EvalM = StateT Stack Maybe

push :: Integer -> EvalM ()
push x = modify (x:)


{-

3 possible alternatives at (1):
1. Using Alternative(guard)
2. Using lift by lifting the Nothing (`when (null xs) $ lift Nothing)
3. Using MonadFail, which is implemented by Maybe and thus by the monad transformer
   (`(x:xs) <- get`)
-}
pop :: EvalM Integer
pop = do
    (x:xs) <- get  -- (1)
    put xs
    pure x


isEmpty :: EvalM Bool
isEmpty = gets null

notEmpty :: EvalM Bool
notEmpty = not <$> isEmpty

oneElementOnStack :: EvalM ()
oneElementOnStack = do
    l <- gets length
    guard (l == 1)


readSafe :: (Read a, Alternative m) => String -> m a
readSafe str = maybe empty pure (readMaybe str)


evalRPN :: String -> Maybe Integer
evalRPN expr = evalStateT evalRPN' []
    where
        evalRPN' = traverse_ step (words expr) >> oneElementOnStack >> pop
        step "+" = processTops (+)
        step "*" = processTops (*)
        step "-" = processTops (-)
        step t = readSafe t >>= push
        processTops op = flip op <$> pop <*> pop >>= push