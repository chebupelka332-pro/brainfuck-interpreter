module Main where

import System.Environment (getArgs)
import System.IO (readFile, hSetEcho, stdin, BufferMode(NoBuffering), hFlush, stdout)
import Control.Monad (when, unless)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import BrainfuckUtils

type Memory = [Int]

data BFState = BFState { code :: String, pointer :: Int, memory :: Memory, pc :: Int, memSize :: Int, wrap :: Bool, cellMod :: Maybe Int }

initState :: String -> Int -> Bool -> Maybe Int -> BFState
initState code size wrap cellMod = BFState code 0 (replicate size 0) 0 size wrap cellMod

runBrainfuck :: BFState -> IO ()
runBrainfuck state@(BFState code ptr mem pc size wrap cellMod)
    | pc >= length code = return ()
    | otherwise = case code !! pc of
        '>' -> runBrainfuck state { pointer = movePointer (ptr + 1), pc = pc + 1 }
        '<' -> runBrainfuck state { pointer = movePointer (ptr - 1), pc = pc + 1 }
        '+' -> runBrainfuck state { memory = updateCell (+1), pc = pc + 1 }
        '-' -> runBrainfuck state { memory = updateCell (subtract 1), pc = pc + 1 }
        '.' -> do
                putStr "Out: "
                let value = mem !! ptr
                putStr $ show value ++ " (" ++ printSymbol value ++ ")\n"
                runBrainfuck state { pc = pc + 1 }
        ',' -> do
                putStr "In: "
                hFlush stdout
                line <- getLine
                let c = extract $ stringToNumber line
                runBrainfuck state { memory = updateCell (const c), pc = pc + 1 }
        '[' -> if mem !! ptr == 0
                then runBrainfuck state { pc = findMatchingBracket pc 1 }
                else runBrainfuck state { pc = pc + 1 }
        ']' -> if mem !! ptr /= 0
                then runBrainfuck state { pc = findMatchingBracket pc (-1) }
                else runBrainfuck state { pc = pc + 1 }
        _   -> runBrainfuck state { pc = pc + 1 }
    where
        movePointer p
            | wrap = (p + size) `mod` size
            | not wrap && (p >= size || p < 0) = error "Error: ptr is out of range" 
            | otherwise = max 0 (min (size - 1) p)

        updateCell f = take wrappedPtr mem ++ [applyMod (f (mem !! wrappedPtr))] ++ drop (wrappedPtr + 1) mem
            where
                wrappedPtr = if wrap then (ptr + size) `mod` size else ptr
                applyMod x = case cellMod of
                    Just modVal | modVal > 0 -> (x + modVal) `mod` modVal
                    Just modVal | modVal <= 0 -> x
                    _ -> x

        findMatchingBracket :: Int -> Int -> Int
        findMatchingBracket pos dir = go (pos + dir) 0
            where
                (open, close) = if dir == 1 then ('[', ']') else (']', '[')

                go i depth
                    | i < 0 || i >= length code = error "Error: bracket mismatch"
                    | code !! i == open = go (i + dir) (depth + 1)
                    | code !! i == close = if depth == 0 then i else go (i + dir) (depth - 1)
                    | otherwise = go (i + dir) depth

main :: IO ()
main = do
    args <- getArgs
    let (fileName, memSize, wrap, cellMod) = parseArgs args

    putStr "\n------------------\nDebug information:\n"
    putStr $ "CellMod: " ++ show cellMod ++ "\n"
    putStr $ "MemSize: " ++ show memSize ++ "\n"
    putStr $ "Wrap: " ++ show wrap ++ "\n"
    putStr "------------------\n\n"

    unless (isValidExtension fileName) (error "Error: Invalid file extension. Acceptable .bf or .b")

    code <- readFile fileName
    let cleanCode = filterBrainfuck code

    putStrLn "Running Brainfuck program:" 
    runBrainfuck (initState cleanCode memSize wrap cellMod)