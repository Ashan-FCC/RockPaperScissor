{-# LANGUAGE InstanceSigs, TypeApplications, ScopedTypeVariables  #-}
module Main where

import Control.Concurrent
import System.Random
import System.IO
import Control.Monad.State
import System.Console.ANSI
import Text.Read
import System.Exit

type Score     = Int
type Freq      = Int
type Row       = Int
type Col       = Int

data Result    = Win | Lose | Draw deriving Show
data Strategy  = Cheat | Dumb | Random deriving (Bounded, Enum, Eq, Read, Show)
data Move      = Rock | Paper | Scissor deriving (Bounded, Enum, Eq, Read, Show)

data Pattern = Pattern (Freq, Freq, Freq) deriving Show
data Game = Game
  { _gPlayerScore   :: Score
  , _gComputerScore :: Score
  , _gHistory       :: History
  , _gStrategy      :: Strategy
  } deriving Show

data History = History
  { _hMove    :: Move     -- previous move of the player
  , _hResult  :: Result   -- result of the player (won or lost)
  , _hPattern :: Pattern -- What is the frequency of players rock,paper, scissors used
  } deriving Show

instance Ord Move where
  compare :: Move -> Move -> Ordering
  compare Rock Scissor  = GT
  compare Rock Paper     = LT
  compare Paper Rock     = GT
  compare Paper Scissor = LT
  compare Scissor Paper = GT
  compare Scissor Rock  = LT
  compare _ _            = EQ


-- Impure

randEnum :: forall a. (Bounded a, Enum a) => IO a
randEnum = do
  let min = fromEnum $ minBound @a
      max = fromEnum $ maxBound @a
  r <- randomRIO $ (min, max)
  return . toEnum $ r

-- Logic
processMove :: Move -> IO Move
processMove playerMove = do
  computerMove <- randEnum @Move
  return computerMove

--Input / Output

getMove :: IO Move
getMove = do
  let tryAgain = do
        writeLine 6 2 "You entered an invalid move, please enter again"
        getMove
  hSetEcho stdin False
  _m <-  getChar
  hSetEcho stdin True
  case _m of
    '1' -> return Rock
    '2' -> return Paper
    '3' -> return Scissor
    'q' -> showCursor >> exitSuccess
    _   -> tryAgain

quietly :: IO a -> IO a
quietly ioa = do
  hSetEcho stdin False
  a <- ioa
  hSetEcho stdin True
  return a

writeLine :: Row -> Col -> String -> IO ()
writeLine r c s = setCursorPosition r c >> clearLine >> putStrLn s

play :: IO ()
play = do
  clearScreen
  writeLine 1 3 "Welcome to Rock Paper Scissors !"
  writeLine 4 2 "Rock = 1 | Paper = 2 | Scissors = 3 || Quit = q | New Game = n"
  writeLine 5 2 "Please enter your move!"
  playerMove <- getMove
  writeLine 6 2 $ "You chose " ++ show playerMove ++"!"
  writeLine 7 2 "Hmm, let me think ..."
  --threadDelay (10 ^ 6) -- 10^6 microseconds = 1 second
  computerMove <- processMove playerMove
  writeLine 8 2 $ "I choose " ++ show computerMove ++ "!"
  case compare playerMove computerMove of
    GT -> writeLine 9 2 "You win"
    LT -> writeLine 9 2 "I win"
    EQ -> writeLine 9 2 "Its a tie!"
  writeLine 10 2 "Enter q to quit or anything else to start a new game"
  c <- getChar
  if c == 'q' then showCursor >> exitSuccess else play

main = do
  hSetBuffering stdin NoBuffering -- function
  clearScreen
  hideCursor
  play

