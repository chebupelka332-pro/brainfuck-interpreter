module Main where

import System.Environment (getArgs)
import System.IO (readFile, hSetEcho, stdin, BufferMode(NoBuffering), hFlush, stdout)
import Data.Char (toLower, ord, chr)
import Control.Monad (when, unless)
import Text.Read (readMaybe)

-- Список допустимых команд Brainfuck
validCommands :: String
validCommands = "><.,+-[]"

-- Проверка расширения файла
isValidExtension :: String -> Bool
isValidExtension fileName = any (`endsWith` map toLower fileName) [".bf", ".b"]
    where endsWith ext f = ext == reverse (take (length ext) (reverse f))

-- Фильтрация кода (оставляем только допустимые команды)
filterBrainfuck :: String -> String
filterBrainfuck = filter (`elem` validCommands)

stringToNumber :: String -> Maybe Int
stringToNumber s
    | all (`elem` "0123456789") s = readMaybe s
    | otherwise = Just (ord (head s))

extract :: Maybe Int -> Int
extract m = case m of
    Just n  -> n
    Nothing -> 0  -- Значение по умолчанию

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
    7   -> "BEL"   -- Звуковой сигнал
    8   -> "BS"    -- Backspace
    9   -> "TAB"   -- Горизонтальная табуляция
    10  -> "LF"    -- Line Feed (новая строка)
    11  -> "VT"
    12  -> "FF"
    13  -> "CR"    -- Carriage Return (возврат каретки)
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
    27  -> "ESC"   -- Escape
    28  -> "FS"
    29  -> "GS"
    30  -> "RS"
    31  -> "US"
    32  -> "SPACE" -- Пробел
    127 -> "DEL"   -- Удаление
    _   -> [chr n] -- Все остальные символы

-- Функция для вывода символа по коду
printSymbol :: Int -> String
printSymbol n
    | n >= 0 && n <= 127 = asciiSpecial n
    | otherwise = "no ASCII"

type Memory = [Int]

data BFState = BFState { code :: String, pointer :: Int, memory :: Memory, pc :: Int }

-- Начальное состояние
initState :: String -> BFState
initState code = BFState code 0 (repeat 0) 0

-- Выполнение программы Brainfuck
runBrainfuck :: BFState -> IO ()
runBrainfuck state@(BFState code ptr mem pc)
    | pc >= length code = return ()
    | otherwise = case code !! pc of
        '>' -> runBrainfuck state { pointer = ptr + 1, pc = pc + 1 }
        '<' -> runBrainfuck state { pointer = ptr - 1, pc = pc + 1 }
        '+' -> runBrainfuck state { memory = updateCell (+1), pc = pc + 1 }
        '-' -> runBrainfuck state { memory = updateCell (subtract 1), pc = pc + 1 }
        '.' -> do
                putStr "Out: "
                let value = mem !! ptr
                putStr $ show value
                putStr $ " (" ++ printSymbol value ++ ")\n"
                runBrainfuck state { pc = pc + 1 }
        ',' -> do
                putStr "In: "
                hFlush stdout
                line <- getLine
                let c = extract $ stringToNumber line
                runBrainfuck state { memory = updateCell ( const c ), pc = pc + 1 }
        '[' -> if mem !! ptr == 0
                then runBrainfuck state { pc = findMatchingBracket pc 1 }
                else runBrainfuck state { pc = pc + 1 }
        ']' -> if mem !! ptr /= 0
                then runBrainfuck state { pc = findMatchingBracket pc (-1) }
                else runBrainfuck state { pc = pc + 1 }
        _   -> runBrainfuck state { pc = pc + 1 }
    where
        updateCell f = take ptr mem ++ [f (mem !! ptr)] ++ drop (ptr + 1) mem

        findMatchingBracket :: Int -> Int -> Int
        findMatchingBracket pos dir = go (pos + dir) 0
            where
                go i depth
                    | i < 0 || i >= length code = error "Error: bracket mismatch"
                    | code !! i == '[' && dir == 1 = go (i + dir) (depth + 1)
                    | code !! i == ']' && dir == -1 = go (i + dir) (depth + 1)
                    | code !! i == ']' && dir == 1 && depth == 0 = i
                    | code !! i == '[' && dir == -1 && depth == 0 = i
                    | code !! i == ']' || code !! i == '[' = go (i + dir) (depth - 1)
                    | otherwise = go (i + dir) depth

main :: IO ()
main = do
    args <- getArgs
    when (null args) (error "Error: Specify the file name with the Brainfuck program")

    let fileName = head args
    unless (isValidExtension fileName) (error "Error: Invalid file extension. Acceptable .bf or .b")

    code <- readFile fileName
    let cleanCode = filterBrainfuck code

    putStrLn "Running Brainfuck program:" 
    runBrainfuck (initState cleanCode)