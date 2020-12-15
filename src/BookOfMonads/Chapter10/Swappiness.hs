module BookOfMonads.Chapter10.Swappiness where

import qualified BookOfMonads.Chapter6.Writer as W
import qualified BookOfMonads.Chapter6.Reader as R
import BookOfMonads.Chapter10.TestCompose


swapWriter :: Monad m => (W.MyWriter w :.: m) a -> (m :.: W.MyWriter w) a
swapWriter (Compose (W.MyWriter (w, ma))) = Compose $ fmap (\a -> W.MyWriter (w, a)) ma


swapReader :: Monad m => (m :.: R.MyReader r) a -> (R.MyReader r :.: m) a
swapReader (Compose m) = Compose $ R.MyReader $ \r -> R.runReader <$> m <*> pure r
