module Main where

import System.Environment (getArgs)
import System.IO (readFile, hSetEcho, stdin, BufferMode(NoBuffering), hFlush, stdout)
import Control.Monad (when, unless)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import BrainfuckUtils

type Memory = [Int]

data BFState = BFState { code :: String, pointer :: Int, memory :: Memory, pc :: Int, memSize :: Int, wrap :: Bool }

initState :: String -> Int -> Bool -> BFState
initState code size wrap = BFState code 0 (replicate size 0) 0 size wrap

runBrainfuck :: BFState -> IO ()
runBrainfuck state@(BFState code ptr mem pc size wrap)
    | pc >= length code = return ()
    | otherwise = case code !! pc of
        '>' -> runBrainfuck state { pointer = movePointer (ptr + 1), pc = pc + 1 }
        '<' -> runBrainfuck state { pointer = movePointer (ptr - 1), pc = pc + 1 }
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

        updateCell f = take wrappedPtr mem ++ [f (mem !! wrappedPtr)] ++ drop (wrappedPtr + 1) mem
            where
                wrappedPtr = if wrap then (ptr + size) `mod` size else ptr

        findMatchingBracket :: Int -> Int -> Int
        findMatchingBracket pos dir = go (pos + dir) 0
            where
                (open, close) = if dir == 1 then ('[', ']') else (']', '[')

                go i depth
                    | i < 0 || i >= length code = error "Error: bracket mismatch"
                    | code !! i == open = go (i + dir) (depth + 1)
                    | code !! i == close = if depth == 0 then i else go (i + dir) (depth - 1)
                    | otherwise = go (i + dir) depth

parseArgs :: [String] -> (String, Int, Bool)
parseArgs args = (fileName, fromMaybe 30000 memSize, wrap)
    where
        fileName = fromMaybe (error "Error: Specify the file name with the Brainfuck program") (findFile args)
        memSize = parseSize args
        wrap = "-c" `elem` args

        findFile = safeHead . filter (not . isFlag)
        isFlag ('-':_) = True
        isFlag _ = False

        parseSize [] = Nothing
        parseSize ("-s":n:_) = readMaybe n
        parseSize (_:xs) = parseSize xs

        safeHead [] = Nothing
        safeHead (x:_) = Just x

main :: IO ()
main = do
    args <- getArgs
    let (fileName, memSize, wrap) = parseArgs args

    unless (isValidExtension fileName) (error "Error: Invalid file extension. Acceptable .bf or .b")

    code <- readFile fileName
    let cleanCode = filterBrainfuck code

    putStrLn "Running Brainfuck program:" 
    runBrainfuck (initState cleanCode memSize wrap)
