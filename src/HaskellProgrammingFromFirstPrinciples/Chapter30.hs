{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
module HaskellProgrammingFromFirstPrinciples.Chapter30 where
import Control.Exception
    ( SomeException,
      catch,
      ArithException(DivideByZero),
      AsyncException(StackOverflow) )
import System.Environment (getArgs)
import Data.Typeable ( Typeable, cast )

{-

type SomeException :: *
data SomeException = forall e. Exception e => SomeException e

class (Typeable e, Show e) => Exception e where
  toException :: e -> SomeException
  fromException :: SomeException -> Maybe e
  displayException :: e -> String



-}


data MyException = 
    forall e .
    (Show e, Typeable e) => MyException e

instance Show MyException where
    showsPrec p (MyException e) = showsPrec p e


data SomeError = Arith ArithException | Async AsyncException | SomethingElse deriving (Show)


multiError :: Int-> Either MyException Int
multiError n = case n of
    0 -> Left (MyException DivideByZero)
    1 -> Left (MyException StackOverflow)
    _ -> Right n

discriminateError :: MyException-> SomeError
discriminateError (MyException e) =
    case cast e of
        (Just arith) -> Arith arith
        Nothing -> case cast e of
            (Just async) -> Async async
            Nothing -> SomethingElse

runDisc :: Int -> SomeError
runDisc n = either discriminateError (const SomethingElse) (multiError n)

dino :: Typeable a => a -> [Char]
dino n = case cast n of
    Just s -> "ciao " ++ s
    Nothing -> "Not a string"


willIFail :: Integer -> IO ()
willIFail denom = print (div 5 denom) `catch` handler
    where handler :: SomeException -> IO ()
          handler = print

willIFailButDontCatch :: Integer -> IO ()
willIFailButDontCatch denom = print (div 5 denom) `catch` handler
    where handler :: AsyncException  -> IO ()
          handler = print

willIFailPrecise :: Integer -> IO ()
willIFailPrecise denom = print (div 5 denom) `catch` handler
    where handler :: ArithException -> IO ()
          handler = print
