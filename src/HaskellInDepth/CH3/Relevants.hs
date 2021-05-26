{-# LANGUAGE RecordWildCards #-}
-- Required for `deriving(Generic)`
{-# LANGUAGE DeriveGeneric #-}
-- Required for `deriving(FromNameRecord)`
{-# LANGUAGE DeriveAnyClass #-}
-- Required for `deriving(Functor)`
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- We need to explicitly specify if I want DeriveAnyClass or GeneralizedNewTypeDeriving
{-# LANGUAGE DerivingStrategies #-}

module HaskellInDepth.CH3.Relevants where


import qualified Text.Blaze.Html5 as H
import Fmt ( (+|), fmt, (|+), Buildable(..) )
import Data.Text ( Text )
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Foldable (maximumBy, minimumBy, toList)
import Colonnade ( ascii, headed, Colonnade )
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Blaze.Html5
    ( Html, string, text, body, h1, style, title )
import Text.Blaze.Colonnade ( encodeHtmlTable )
import Colonnade.Encode ( Colonnade, Headed )
import Fmt.Internal ( (+|), fmt, pretty, (|+) )
import Data.String (IsString)
import Data.Time ( Day, defaultTimeLocale, parseTimeM )
import Data.Csv (decodeByName, FromField(..), FromNamedRecord, Header)
import Data.ByteString.Char8 (unpack)
import Codec.Binary.UTF8.Generic (toString, UTF8Bytes (pack), fromString)
import GHC.Generics (Generic)
import Data.Vector (Vector)
import Control.Monad.Except
import Control.Lens.Combinators


-- Great `time` package


-- `cassava` for CSV


-- `fmt` package


{-
(|+) :: (Buildable a, Fmt.Internal.Core.FromBuilder b) => a -> Builder -> b
(+|) :: Fmt.Internal.Core.FromBuilder b => Builder -> Builder -> b
(||+) :: (Show a, Fmt.Internal.Core.FromBuilder b) => a -> Builder -> b
(+||) :: Fmt.Internal.Core.FromBuilder b => Builder -> Builder -> b

`b` is always a `FromBuilder`.

Double piped functions take a `Show` as variable `a`.
Single piped functions taks a `Builder` as variable `a`.

-}
data Dino = Dino {dino1:: Int, dino2:: Int} deriving(Show)


instance Buildable Dino where
    build (Dino a b) = "Dino (" +| a |+ ", " +| b |+ ")"


-- fmt is polymorphic
xFmt :: String 
xFmt = fmt $ "hello " +| Dino 1 2 |+ "world"
yFmt :: Text 
yFmt = fmt $ "hello " +| Dino 1 2 |+ "world"


-- RecordsWildcards can be used to build instances
xWildCards :: Dino
xWildCards = 
    let dino1 = 1
        dino2 = 2
    in Dino{..}

xWildCardsBEWARE :: Dino
xWildCardsBEWARE = 
    let dino1WRONG = 1
        dino2 = 2
    in Dino{..}



-- Sort by
getSortField :: Dino -> Int 
getSortField Dino{..} = dino1

compareFunction :: Dino -> Dino -> Ordering
compareFunction = comparing getSortField

myArrayToBeSorted :: [Dino]
myArrayToBeSorted = [Dino 10 20, Dino 50 10, Dino (-1) 100]
mySortedArray :: [Dino]
mySortedArray = sortBy compareFunction myArrayToBeSorted
maxDino :: Dino
maxDino = maximumBy compareFunction myArrayToBeSorted
minDino :: Dino
minDino = minimumBy compareFunction myArrayToBeSorted

-- A simple attempt to sort a foldable. Since `Foldable` has no such `fromList`, we have to get it as an input param.
mySortBy :: Foldable t => ([a] -> t a) -> (a -> a -> Ordering) -> t a -> t a
mySortBy fromList f = fromList . sortBy f . toList


-- Nice tabular data library
colStats :: Colonnade Headed Dino String
colStats = mconcat [ 
                headed "Dino 1" (show . dino1)
                , headed "Dino 2" (show . dino2)
            ]
textTable :: Foldable t => t Dino -> String
textTable = ascii colStats
outputTable :: IO ()
outputTable = putStr $ textTable $ fmap (Dino 1) [1..100]


-- Useful functions for monads

--when
mywhen :: Applicative f => Bool -> f () -> f ()
mywhen b r = if b then r else pure ()

-- unless
myunless :: Applicative f => Bool -> f () -> f ()
myunless b = mywhen (not b)


-- HTML generation
viaFmt :: Buildable a => a -> Html
viaFmt = text . pretty
colStatsHtml :: Colonnade Headed Dino Html
colStatsHtml = mconcat [ 
                headed "Dino 1" (viaFmt . dino1)
                , headed "Dino 2" (viaFmt . dino2)
            ]

myHtml :: String
myHtml = renderHtml $ H.docTypeHtml $ do
    H.head $ do
        title $ string "Test HTML"
        style tableStyle
    body $ do
        h1 "Dini"
        encodeHtmlTable mempty colStatsHtml $ fmap (Dino 1) [1..10]
    string "WHERE AM I?"
    where tableStyle = "table {border-collapse: collapse}" <> "td, th {border: 1px solid black; padding: 5px}"


-- MonadFail

mysqrt :: (MonadFail m, Floating a, Ord a) => a -> m a
mysqrt i = do
    if i >= 0 then return (sqrt i) else fail "Negative value"

mysqrtError :: (MonadError String m, Floating a, Ord a) => a -> m a
mysqrtError i = do
    if i >= 0 then return (sqrt i) else throwError "Negative value"

mysqrtMaybeError :: (Floating a, Ord a) => a -> Either String a
mysqrtMaybeError = mysqrtError

instance MonadFail (Either String) where
    fail = Left

{-
There are many deriving strategies:
1. stock: known as Standard. Used for the following classes:
   1.1 Eq
   1.2 Ord
   1.3 Show
   1.4 Read
   1.5 Enum
   1.6 Bounded
   1.7 Ix
2. newtype
   Just unwraps the type, implementsa using the inner stuff, then wraps the type again
3. anyclass
   Based on the default implementation
4. via
   Based on another type's implementation.

Source: https://kowainik.github.io/posts/deriving
-}
newtype LeftFail a = LeftFail (Either String a) 
    deriving stock (Show)
    deriving newtype (Functor, Applicative, Monad)
leftFail :: LeftFail a -> Either String a
leftFail (LeftFail x) = x

instance MonadFail LeftFail where
    fail = LeftFail . Left

newtype StringError = StringError { stringError :: String} deriving newtype (IsString)

instance MonadFail (Either StringError) where
    fail = Left . StringError

maybeSqrt :: (Floating a, Ord a) => a -> Maybe a
maybeSqrt = mysqrt

ioSqrt :: (Floating a, Ord a) => a -> IO a
ioSqrt = mysqrt

eitherSqrt :: (Floating a, Ord a) => a -> Either String a
eitherSqrt = mysqrt

leftFailSqrt :: (Floating a, Ord a) => a -> LeftFail a
leftFailSqrt = mysqrt

stringErrorSqrt :: (Floating a, Ord a) => a -> LeftFail a
stringErrorSqrt = mysqrt

-- CSV parsing (cassava)

-- I can parse arbitrary fields
instance FromField Day where
    parseField = parseTimeM True defaultTimeLocale "%Y-%m-%d" . toString

data QuoteData = QuoteData {
    day :: Day,
    volume :: Int,
    open :: Double,
    close :: Double,
    high :: Double,
    low :: Double
} deriving (Generic, FromNamedRecord, Show)

csvData :: Either String (Header, Vector QuoteData)
csvData = (decodeByName . fromString)
    "\
    \day,close,volume,open,high,low\n \
    \2019-05-01,210.520004,64827300,209.880005,215.309998,209.229996\n \
    \2019-05-02,209.149994,31996300,209.839996,212.649994,208.130005\n \
    \2019-05-03,211.75,20892400,210.889999,211.839996,210.229996 \
    \ "

column :: Colonnade Headed Dino Html
column = headed "Dino 1" (viaFmt . dino1)

lmapf :: () -> Dino
lmapf = const $ Dino 1 2

lcolumn :: Colonnade Headed () Html
lcolumn = lmap lmapf column

lcolumn2 :: Colonnade Headed [Dino] Html
lcolumn2 = lmap (!! 4) column