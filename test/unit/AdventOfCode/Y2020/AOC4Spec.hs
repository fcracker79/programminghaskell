module AdventOfCode.Y2020.AOC4Spec where



import Test.Hspec
import AdventOfCode.Y2020.AOC4


spec :: Spec
spec = do
    describe "Prelude.head" $ do
        it "I correctly find valid documents" $ do
            countValidDocuments [
                    "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\
                    \byr:1937 iyr:2017 cid:147 hgt:183cm",
                    
                    "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n\
                    \hcl:#cfa07d byr:1929",
                    
                    "hcl:#ae17e1 iyr:2013\n\
                    \eyr:2024\n\
                    \ecl:brn pid:760753108 byr:1931\n\
                    \hgt:179cm",
                    
                    "hcl:#cfa07d eyr:2025 pid:166559648\n\
                    \iyr:2011 ecl:brn hgt:59in"
                ] 
            `shouldBe` 2
