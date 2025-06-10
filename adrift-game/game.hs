-- Author: Alexandre Thurow Bender
----- Adrift -----
module Main where

import Data.Char
import System.IO
import System.Random
import Data.List
import Data.Maybe
import System.Console.ANSI
import Control.Concurrent
import System.Exit

-- Stage board data structure
type StageBoard = [[Char]]

-- Game board example 9x9
stageBoard :: StageBoard
stageBoard = [['-','-','-','-','-','X','-','-','-','-'],
              ['-','-','X','-','-','-','X','-','-','-'],
              ['-','-','-','-','-','-','-','-','-','-'],
              ['-','-','-','-','-','O','-','-','-','-'],
              ['-','-','-','G','-','-','-','X','-','-'],
              ['-','-','-','-','-','X','-','-','-','-'],
              ['-','-','X','-','-','-','-','-','-','-'],
              ['-','-','X','-','-','-','X','-','-','-'],
              ['-','-','X','-','-','-','X','-','-','-'],
              ['-','-','-','-','-','-','-','-','-','-']]

----- Access functions -----
gArr :: Int -> [t] -> t
gArr 0 (x : xs) = x
gArr n (x : xs) = gArr (n - 1) xs

uArr :: Int -> a -> [a] -> [a]
uArr 0 y (x : xs) = y : xs
uArr n y (x : xs) = x : uArr (n - 1) y xs

gPos :: Int -> Int -> [[a]] -> a
gPos x y matrix = gArr y (gArr x matrix)

gPosPair :: (Int, Int) -> [[a]] -> a
gPosPair (x, y) matrix = gArr y (gArr x matrix)

uPos :: Int -> Int ->  a -> [[a]] -> [[a]]
uPos x y n board = uArr x (uArr y n (gArr x board)) board

uPosPair :: (Int, Int) ->  a -> [[a]] -> [[a]]
uPosPair (x, y) n board = uArr x (uArr y n (gArr x board)) board

isValidPos :: Int -> Int -> Int -> Bool
isValidPos size x y = x < size && y < size && x > -1 && y > -1

isValidPair :: Int -> (Int, Int) -> Bool
isValidPair size (x, y) = x < size && y < size && x > -1 && y > -1

charToString :: Char -> String
charToString c = [c]

----- Game functions -----
displayStage :: StageBoard -> String
displayStage stageBoard = printLine ((length stageBoard) - 1) stageBoard

printLine :: Int -> StageBoard -> String
printLine 0 stageBoard = printBoardLine 0 stageBoard ++ "\n"
printLine n stageBoard = printLine (n - 1) stageBoard ++ (printBoardLine n stageBoard) ++ "\n"

printBoardLine :: Int -> StageBoard -> String
printBoardLine n stageBoard = addSpaces (gArr n stageBoard)

addSpaces :: [Char] -> [Char]
addSpaces [] = []
addSpaces [x] = [x]
addSpaces (x : xs) = x : ' ' : addSpaces xs

generateStage :: Int -> (StageBoard, Bool)
generateStage level
    | (length stages) > level = (gArr level stages, True)
    | otherwise = (gArr 0 stages, False)

-- We use Data.Maybe to handle cases in which the character is not present in the board
findChar :: [[Char]] -> Char -> Maybe (Int, Int)
findChar c t = listToMaybe [ (x, y) | (x, line) <- zip [0..] c, y <- elemIndices t line ]

getPlayerPosition :: StageBoard -> (Int, Int)
getPlayerPosition stageBoard = fromJust (findChar stageBoard 'O')

getGoalPosition :: StageBoard -> (Int, Int)
getGoalPosition stageBoard = fromJust (findChar stageBoard 'G')

moveUp :: (Int, Int) -> (Int, Int)
moveUp (x, y) = (x - 1, y)

moveDown :: (Int, Int) -> (Int, Int)
moveDown (x, y) = (x + 1, y)

moveRight :: (Int, Int) -> (Int, Int)
moveRight (x, y) = (x, y + 1)

moveLeft :: (Int, Int) -> (Int, Int)
moveLeft (x, y) = (x, y - 1)

movePlayer :: StageBoard -> Char -> StageBoard
movePlayer stageBoard 'w'
    | isValidPair (length stageBoard) (moveUp (getPlayerPosition stageBoard)) == False = uPosPair (getPlayerPosition stageBoard) '-' stageBoard
    | gPosPair (moveUp (getPlayerPosition stageBoard)) stageBoard == 'X' = stageBoard
    | gPosPair (moveUp (getPlayerPosition stageBoard)) stageBoard == 'G' = uPosPair (moveUp (getPlayerPosition stageBoard)) 'O' (uPosPair (getPlayerPosition stageBoard) '-' stageBoard)
    | otherwise = movePlayer (uPosPair (moveUp (getPlayerPosition stageBoard)) 'O' (uPosPair (getPlayerPosition stageBoard) '-' stageBoard)) 'w'

movePlayer stageBoard 's'
    | isValidPair (length stageBoard) (moveDown (getPlayerPosition stageBoard)) == False = uPosPair (getPlayerPosition stageBoard) '-' stageBoard
    | gPosPair (moveDown (getPlayerPosition stageBoard)) stageBoard == 'X' = stageBoard
    | gPosPair (moveDown (getPlayerPosition stageBoard)) stageBoard == 'G' = uPosPair (moveDown (getPlayerPosition stageBoard)) 'O' (uPosPair (getPlayerPosition stageBoard) '-' stageBoard)
    | otherwise = movePlayer (uPosPair (moveDown (getPlayerPosition stageBoard)) 'O' (uPosPair (getPlayerPosition stageBoard) '-' stageBoard)) 's'

movePlayer stageBoard 'd'
    | isValidPair (length stageBoard) (moveRight (getPlayerPosition stageBoard)) == False = uPosPair (getPlayerPosition stageBoard) '-' stageBoard
    | gPosPair (moveRight (getPlayerPosition stageBoard)) stageBoard == 'X' = stageBoard
    | gPosPair (moveRight (getPlayerPosition stageBoard)) stageBoard == 'G' = uPosPair (moveRight (getPlayerPosition stageBoard)) 'O' (uPosPair (getPlayerPosition stageBoard) '-' stageBoard)
    | otherwise = movePlayer (uPosPair (moveRight (getPlayerPosition stageBoard)) 'O' (uPosPair (getPlayerPosition stageBoard) '-' stageBoard)) 'd'

movePlayer stageBoard 'a'
    | isValidPair (length stageBoard) (moveLeft (getPlayerPosition stageBoard)) == False = uPosPair (getPlayerPosition stageBoard) '-' stageBoard
    | gPosPair (moveLeft (getPlayerPosition stageBoard)) stageBoard == 'X' = stageBoard
    | gPosPair (moveLeft (getPlayerPosition stageBoard)) stageBoard == 'G' = uPosPair (moveLeft (getPlayerPosition stageBoard)) 'O' (uPosPair (getPlayerPosition stageBoard) '-' stageBoard)
    | otherwise = movePlayer (uPosPair (moveLeft (getPlayerPosition stageBoard)) 'O' (uPosPair (getPlayerPosition stageBoard) '-' stageBoard)) 'a'

getInput :: IO Char
getInput = do
    direction <- hGetChar stdin
    if (direction == '\ESC')
    then do
        hSetEcho stdin False
        hideCursor
        clearScreen
        setCursorPosition 0 0
        exitSuccess
    else do
        if (direction /= 'w' && direction /= 's' && direction /= 'a' && direction /= 'd')
        then do
            getInput
        else do
            return direction

----- Main game loop -----

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    hideCursor
    clearScreen
    setCursorPosition 0 0
    putStr "        === Adrift ===\n    Press any key to continue\n"
    direction <- hGetChar stdin

    clearScreen
    setCursorPosition 0 0
    putStr "        Instructions:\n\n"
    putStr "    Move using the WASD keys.\nUse the obstacles to reach the objective.\n\n        O: Player\n        X: Obstacle\n        G: Goal\n        -: Empty space\n\n    (Press any key to continue)"
    direction <- hGetChar stdin

    stageLoop 0 3

stageLoop :: Int -> Int -> IO ()
stageLoop stage lives
    | snd (generateStage stage) = gameLoop (fst (generateStage stage)) stage lives
    | otherwise = do
        clearScreen
        setCursorPosition 0 0
        putStr "Congratulations!\n    You beat the game.\n        Play again?\n"
        direction <- hGetChar stdin
        main

gameLoop :: StageBoard -> Int -> Int -> IO ()
gameLoop stageBoard stageNumber lives = do
    clearScreen
    setCursorPosition 0 0
    putStr (displayStage stageBoard)

    direction <- getInput


    --movePlayer direction
    let newGB = movePlayer stageBoard direction

    if (findChar newGB 'O' == Nothing)
    then do
        clearScreen
        setCursorPosition 0 0
        putStr (displayStage newGB)
        
        if (lives - 1 < 0)
        then do
            putStr "No more lifes remaining.\n    Game over.\n        Try again?\n"
            direction <- hGetChar stdin
            stageLoop 0 3
        else do
            putStr ("You are adrift.\n    " ++ (show (lives - 1)) ++ " lives remaining.\n")
            Control.Concurrent.threadDelay 700000
            stageLoop stageNumber (lives - 1)

    else do
        if (findChar newGB 'G' == Nothing)
        then do
            clearScreen
            setCursorPosition 0 0
            putStr (displayStage newGB)
            putStr "Stage cleared!\n"
            Control.Concurrent.threadDelay 700000
            stageLoop (stageNumber + 1) lives

        else do
            gameLoop newGB stageNumber lives

stages :: [StageBoard]
stages = [[['-','-','-','-','-','-','-','-','-','-'],
           ['-','-','-','-','-','-','-','-','O','-'],
           ['-','-','-','X','-','-','-','-','-','-'],
           ['-','-','-','-','-','-','X','-','-','-'],
           ['-','-','-','-','G','-','-','-','-','-'],
           ['-','-','-','-','-','X','-','-','-','-'],
           ['-','-','X','-','-','-','-','-','-','-'],
           ['-','-','-','-','-','-','-','-','X','-'],
           ['-','-','-','-','-','-','-','-','-','-'],
           ['-','-','-','-','-','-','-','-','-','-']],

          [['X','X','X','X','X','X','X','X','X','X'],
           ['-','-','X','-','-','-','X','O','-','X'],
           ['-','X','-','-','-','-','-','-','-','X'],
           ['-','-','-','-','-','X','-','-','-','X'],
           ['-','-','-','G','-','-','-','X','-','X'],
           ['-','-','X','-','-','X','-','-','-','X'],
           ['-','-','X','-','-','-','-','-','-','X'],
           ['-','-','X','-','-','-','X','-','-','X'],
           ['-','-','-','-','-','-','-','-','-','X'],
           ['-','-','-','-','-','-','-','-','-','X']],

          [['-','-','-','X','-','-','-','-','X','-'],
           ['-','-','X','X','-','-','-','-','-','-'],
           ['-','-','X','-','-','-','-','-','-','-'],
           ['X','-','-','-','X','O','-','-','-','-'],
           ['-','-','-','G','X','-','-','X','-','-'],
           ['-','-','X','X','X','X','-','-','-','-'],
           ['-','-','X','-','-','-','-','-','-','-'],
           ['-','-','X','-','-','-','X','-','-','X'],
           ['-','-','-','-','-','-','-','-','-','X'],
           ['-','-','-','X','-','-','-','-','-','X']],

          [['-','-','-','-','-','X','-','-','X','-'],
           ['-','-','X','O','-','-','X','-','-','-'],
           ['X','-','-','-','-','-','-','-','-','-'],
           ['X','-','X','-','-','-','-','-','-','-'],
           ['-','-','-','-','-','-','-','X','-','-'],
           ['-','-','X','X','X','X','X','X','-','-'],
           ['-','-','X','-','-','-','-','-','-','X'],
           ['-','-','X','G','-','-','X','-','-','-'],
           ['X','-','-','-','-','-','X','-','-','-'],
           ['-','X','-','-','X','-','-','-','-','-']],

          [['-','-','-','-','-','X','-','-','X','-'],
           ['-','-','X','G','-','-','X','-','-','-'],
           ['X','-','-','-','-','-','-','-','-','-'],
           ['X','-','X','-','-','-','-','-','-','-'],
           ['-','-','-','-','-','-','-','X','-','-'],
           ['-','-','X','X','X','X','X','X','-','-'],
           ['-','-','X','-','-','-','-','-','-','X'],
           ['-','-','X','O','-','-','X','-','-','-'],
           ['X','-','-','-','-','-','X','-','-','-'],
           ['-','X','-','-','X','-','-','-','-','-']],

          [['-','X','-','-','-','-','-','-','-','-'],
           ['-','-','-','X','X','-','-','-','-','-'],
           ['X','-','-','X','-','-','-','-','-','X'],
           ['X','-','-','-','-','-','-','-','-','X'],
           ['-','-','-','-','-','-','X','-','-','-'],
           ['-','-','-','-','O','X','G','-','-','-'],
           ['-','-','-','-','-','-','-','X','-','-'],
           ['-','X','-','-','-','-','X','-','-','-'],
           ['-','-','X','-','-','-','-','-','X','-'],
           ['-','-','-','-','-','-','-','X','-','-']],

          [['-','-','-','X','X','-','X','X','-','-'],
           ['-','O','-','X','-','-','X','-','-','G'],
           ['-','-','-','X','-','-','-','-','X','X'],
           ['-','-','-','X','-','-','-','-','X','-'],
           ['-','-','X','X','-','-','-','-','-','-'],
           ['-','-','-','-','X','-','-','-','-','-'],
           ['X','-','-','-','-','-','-','-','-','X'],
           ['-','-','-','-','-','X','-','-','-','-'],
           ['-','X','-','-','-','X','-','-','-','-'],
           ['-','-','-','-','-','-','-','-','X','-']],

          [['-','-','-','-','-','-','-','X','X','-'],
           ['-','X','-','-','-','X','-','-','-','X'],
           ['-','-','-','X','X','-','-','-','X','-'],
           ['X','-','-','-','-','-','-','-','-','X'],
           ['-','-','X','-','X','-','-','-','X','-'],
           ['-','-','X','-','-','-','-','-','-','G'],
           ['X','-','-','-','-','-','-','-','X','-'],
           ['-','O','-','-','-','-','-','-','-','X'],
           ['-','-','-','X','-','-','-','-','X','-'],
           ['-','X','-','-','X','-','-','X','-','X']],

          [['-','X','-','-','X','X','-','X','X','-'],
           ['X','-','O','-','-','-','-','-','-','X'],
           ['-','-','-','-','-','-','-','-','X','X'],
           ['-','-','X','-','-','-','-','-','-','-'],
           ['-','-','-','-','-','-','X','-','-','-'],
           ['-','-','-','-','-','-','-','-','-','-'],
           ['-','-','-','-','-','-','-','-','-','-'],
           ['X','-','X','-','-','-','G','-','-','X'],
           ['X','-','-','X','-','-','-','-','-','X'],
           ['-','-','-','-','-','X','-','X','-','-']],

          [['-','-','X','G','-','-','-','-','X','-'],
           ['X','-','-','-','X','-','X','X','-','X'],
           ['-','-','-','-','X','-','-','-','-','X'],
           ['-','X','-','-','-','-','-','-','-','-'],
           ['-','-','-','X','X','-','X','-','-','X'],
           ['X','-','-','-','-','X','-','-','X','-'],
           ['-','X','X','X','-','-','-','-','X','-'],
           ['X','-','-','-','-','-','-','X','-','-'],
           ['-','-','-','-','X','X','-','-','O','-'],
           ['-','X','-','-','-','-','-','X','-','-']]]