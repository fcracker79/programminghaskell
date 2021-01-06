module AdventOfCode.Y2020.AOC4 where


import qualified Misc.MyParser as P
import Control.Applicative(many, empty)
import qualified Data.Set as Set
import Data.Maybe (isJust)


type FieldName = String
type FieldValue = String


blankChars = Set.fromList "\n\t "

nonBlank :: P.MyParser Char
nonBlank = P.sat (\x -> not (x `Set.member` blankChars))

parseField :: P.MyParser (FieldName, FieldValue)
parseField = do
    fieldName <- P.token $ many (P.sat (/= ':'))
    P.char ':'
    fieldValue <- P.token $ many nonBlank
    P.trimming
    return (fieldName, fieldValue)


mandatoryFields = Set.fromList [
        "byr",
        "iyr",
        "eyr",
        "hgt",
        "hcl",
        "ecl",
        "pid"
        --, "cid"
    ]

allFields = mandatoryFields `Set.union` Set.singleton "cid"

parseDocument :: P.MyParser [(FieldName, FieldValue)]
parseDocument = do
    fields <- many parseField
    let fieldNames = Set.fromList $ fmap fst fields
    
    let noRepetitions = length fields == length fieldNames
    let allMandatoryFields = mandatoryFields `Set.isSubsetOf` fieldNames
    let noUnknownField = fieldNames `Set.isSubsetOf` allFields

    let _validDocument = allMandatoryFields && noRepetitions && noUnknownField
    if _validDocument then return fields else empty

isValidDocument :: String -> Bool
isValidDocument s = isJust $ P.parse parseDocument s

countValidDocuments :: [String] -> Int
countValidDocuments ss = length [x | x <- fmap isValidDocument ss, x]
