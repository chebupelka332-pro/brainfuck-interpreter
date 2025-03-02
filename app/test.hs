import Data.Char (ord)
import Text.Read (readMaybe)

stringToNumber :: String -> Maybe Int
stringToNumber s
    | all (`elem` "0123456789") s = readMaybe s
    | otherwise = Just (ord (head s))

main :: IO ()
main = do
    print $ stringToNumber "123"     -- Just 123
    print $ stringToNumber "a\n"     -- Just 97
    print $ stringToNumber "abcd\n"  -- Just 97
    print $ stringToNumber "42x"     -- Just 52
    print $ stringToNumber ""     -- Just 52
