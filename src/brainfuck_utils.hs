module BrainfuckUtils where

import Data.Char (ord, chr, toLower)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

-- Список допустимых команд Brainfuck
validCommands :: String
validCommands = "><.,+-[]"

-- Фильтрация кода (оставляем только допустимые команды)
filterBrainfuck :: String -> String
filterBrainfuck = filter (`elem` validCommands)

stringToNumber :: String -> Maybe Int
stringToNumber s
    | all (`elem` "0123456789") s = readMaybe s
    | otherwise = Just (ord (head s))

extract :: Maybe Int -> Int
extract = Data.Maybe.fromMaybe 0   -- Значение по умолчанию

-- Определение специальных символов по их ASCII-кодам
asciiSpecial :: Int -> String
asciiSpecial n = case n of
    0   -> "NULL"
    1   -> "SOH"
    2   -> "STX"
    3   -> "ETX"
    4   -> "EOT"
    5   -> "ENQ"
    6   -> "ACK"
    7   -> "BEL"
    8   -> "BS"
    9   -> "TAB"
    10  -> "LF"
    11  -> "VT"
    12  -> "FF"
    13  -> "CR"
    14  -> "SO"
    15  -> "SI"
    16  -> "DLE"
    17  -> "DC1"
    18  -> "DC2"
    19  -> "DC3"
    20  -> "DC4"
    21  -> "NAK"
    22  -> "SYN"
    23  -> "ETB"
    24  -> "CAN"
    25  -> "EM"
    26  -> "SUB"
    27  -> "ESC"
    28  -> "FS"
    29  -> "GS"
    30  -> "RS"
    31  -> "US"
    32  -> "SPACE"
    127 -> "DEL"
    _   -> [chr n]

-- Функция для вывода символа по коду
printSymbol :: Int -> String
printSymbol n
    | n >= 0 && n <= 127 = asciiSpecial n
    | otherwise = "no ASCII"

-- Проверка расширения файла
isValidExtension :: String -> Bool
isValidExtension fileName = any (`endsWith` map toLower fileName) [".bf", ".b"]
    where endsWith ext f = ext == reverse (take (length ext) (reverse f))