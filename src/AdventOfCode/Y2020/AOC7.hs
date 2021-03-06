{-# LANGUAGE TupleSections #-}
module AdventOfCode.Y2020.AOC7(
    parseBagRules,
    howManyBagsCanContain,
    BagType(..)
) where

import qualified Data.Map as Map
import qualified Misc.MyParser as P
import Control.Applicative((<|>), empty, many)
import AdventOfCode.Utils(split)
import Control.Monad.Trans.Reader(Reader, ask)
import qualified Data.Set as Set

data BagType = 
    LIGHT_RED | BRIGHT_WHITE | MUTED_YELLOW | DARK_ORANGE | 
    SHINY_GOLD | DARK_OLIVE | VIBRANT_PLUM | FADED_BLUE | DOTTED_BLACK 
    deriving(Show, Eq, Ord)


parseBagType :: P.MyParser BagType
parseBagType = foldl (<|>) empty [
    do
        P.symbol "light red"
        return LIGHT_RED,
    do
        P.symbol "bright white"
        return BRIGHT_WHITE,
    do
        P.symbol "muted yellow"
        return MUTED_YELLOW,
    do
        P.symbol "dark orange"
        return DARK_ORANGE,
    do
        P.symbol "shiny gold"
        return SHINY_GOLD,
    do
        P.symbol "dark olive"
        return DARK_OLIVE,
    do
        P.symbol "vibrant plum"
        return VIBRANT_PLUM,
    do
        P.symbol "faded blue"
        return FADED_BLUE,
    do
        P.symbol "dotted black"
        return DOTTED_BLACK
    ]

newtype BagRules = BagRules { containingMap :: Map.Map BagType [BagType] } deriving(Show)

type BagRule = (BagType, [BagType])

parseBagRule :: P.MyParser (BagType, [BagType])
parseBagRule = do
    bagType <- parseBagType
    P.symbol "bags contain"
    containedBagTypes <- parseNumBagTypes
    return (bagType, containedBagTypes)


parseNumBagTypes :: P.MyParser [BagType]
parseNumBagTypes = 
    do  
        many (P.char ' ')
        P.symbol "no other bags."
        return []
    <|>
    do
        P.space
        P.nat
        P.space
        bagType <- parseBagType
        P.symbol "bags" <|> P.symbol "bag"
        sep <- P.item
        case sep of
            ',' -> do
                    otherBagTypes <- parseNumBagTypes
                    return (bagType: otherBagTypes)
            _ -> return [bagType]


parseBagRulesArray :: P.MyParser [BagRule]
parseBagRulesArray = 
    do
        P.sat (== '\n') <|> return '\n'
        r <- parseBagRule
        rs <- parseBagRulesArray
        return (r: rs)
    <|> 
    do
        P.finished
        return []


-- TupleSections 
bagRuleToContainedMap :: BagRule -> Map.Map BagType [BagType]
bagRuleToContainedMap (bag, bags) = Map.fromList $ fmap (, [bag]) bags

parseContainedMap :: P.MyParser [Map.Map BagType [BagType]]
parseContainedMap = fmap (fmap bagRuleToContainedMap) parseBagRulesArray

mergeMaps :: Map.Map BagType [BagType] -> [Map.Map BagType [BagType]] -> Map.Map BagType [BagType]
mergeMaps = foldl $ Map.unionWith (++)

parseBagRules :: P.MyParser BagRules
parseBagRules = fmap BagRules $ mergeMaps Map.empty <$> parseContainedMap


containingMaps :: BagType -> Reader BagRules [BagType]
containingMaps b = do
    rules <- fmap containingMap ask
    let children = Map.lookup b rules
    case children of
        Nothing -> return []
        Just justChildren -> foldl (++) justChildren <$> mapM containingMaps justChildren


howManyBagsCanContain :: BagType -> Reader BagRules Int
howManyBagsCanContain b = fmap length $ Set.fromList <$> containingMaps b
