import Data.Char
import Data.List

contentsToWords :: [Char] -> [String]
contentsToWords cs = words $ map (\c -> if c `elem` ['"', ','] then ' ' else c) cs

alphabeticalValue :: [Char] -> Int
alphabeticalValue = foldr (\c n -> ord c - ord 'A' + 1 + n) 0

solution nameText = sum $ zipWith (*) [1..] nameValues
  where
    names = sort $ contentsToWords nameText
    nameValues = map alphabeticalValue names

main = do
    contents <- readFile "names.txt"
    print $ solution contents
