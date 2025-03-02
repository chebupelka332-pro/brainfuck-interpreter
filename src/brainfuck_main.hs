module Main where

import System.Environment (getArgs)
import System.IO (readFile, hSetEcho, stdin, BufferMode(NoBuffering), hFlush, stdout)
import Control.Monad (when, unless)
import BrainfuckUtils

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
