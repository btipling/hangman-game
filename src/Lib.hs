{-# LANGUAGE TemplateHaskell #-}
module Lib
    ( runGame
    ) where


import Data.ByteString ( ByteString )
import System.Random ( randomRIO )
import qualified Data.List
import Data.FileEmbed ( embedFile )
import Data.Text.Encoding ( decodeUtf8 )
import Data.Text ( pack, unpack )
import System.Console.ANSI
    ( setSGR,
      Color(Red, Green, White),
      ColorIntensity(Vivid, Dull),
      ConsoleLayer(Foreground),
      SGR(Reset, SetColor) )
import Control.Monad (forM_)
import Data.Char

contentBytes :: Data.ByteString.ByteString
contentBytes = $(embedFile "./resources/words.txt")

printClueChar :: Char -> IO ()
printClueChar charToDisplay = do 
  setSGR [SetColor Foreground Dull White]
  putChar charToDisplay
  setSGR [Reset]

printIncorrectChar :: Char -> IO ()
printIncorrectChar charToDisplay = do 
  setSGR [SetColor Foreground Vivid Red]
  putChar charToDisplay
  setSGR [Reset]

printGameWord :: Char -> IO ()
printGameWord charToDisplay = do 
  setSGR [SetColor Foreground Vivid Green]
  putChar charToDisplay
  setSGR [Reset]

data GameState = GameState {inCorrectGuesses, correctGuesses, gameword :: [Char], state :: Either String String }

newtype Game a = Game a
instance Monad Game where
  (Game a) >>= k        = k a

instance Functor Game where
  fmap f (Game x) = Game (f x)

instance Applicative Game where
  pure = Game
  Game f <*> Game x = Game (f x)

unwrap :: Game a -> a
unwrap (Game a) = a

continueGame :: GameState -> String -> Game GameState
continueGame gs msg = Game (gs{state=Right msg})

gameOver :: GameState -> String -> Game GameState
gameOver gs msg = Game (gs{state=Left msg})

hasChar :: GameState -> Char -> Game GameState
hasChar gs guess
  | guess `elem` gameword gs = if guess `elem` correctGuesses gs  then
         Game gs
      else
         Game gs{correctGuesses=correctGuesses gs ++ [guess]}
  | guess `elem` inCorrectGuesses gs = Game gs
  | otherwise = Game gs{inCorrectGuesses=inCorrectGuesses gs ++ [guess]}

foundAll :: [Char] -> [Char] -> Bool
foundAll _currentGuesses _gameword
  | null _gameword = True 
  | head _gameword `elem` _currentGuesses = foundAll _currentGuesses $ tail _gameword
  | otherwise = False


progressGame :: GameState -> Char -> Game GameState
progressGame gs guess = do
  updatedGs <- hasChar gs guess 
  if length (inCorrectGuesses updatedGs) > maxGuesses then
    gameOver updatedGs "You lost!"
  else
    if foundAll (correctGuesses updatedGs) (gameword updatedGs) then
      gameOver updatedGs "You won!"
    else
      continueGame updatedGs "Keep going!"

maxGuesses :: Int
maxGuesses = 6

getNonSpaceChar :: IO Char
getNonSpaceChar = do
  guess <- getChar
  if isSpace guess then getNonSpaceChar else return guess

guessedChar :: Char -> [Char] -> Bool
guessedChar currentChar hasGuessed 
  | null hasGuessed = False
  | currentChar `elem` hasGuessed = True
  | otherwise = False 

getClue :: [Char] -> [Char] -> [Char]
getClue [] _ = []
getClue (x:xs) hasGuessed = (if guessedChar x hasGuessed then x else '_') : getClue xs hasGuessed

printClue :: (Char -> IO ()) -> [Char] -> IO ()
printClue charPrinter clue = do
  forM_ clue $ \x -> do
          charPrinter x 
          putChar ' '
  putChar '\n'

tick :: GameState -> IO ()
tick gs = do
    putStrLn "Clue:"
    printClue printClueChar (getClue (gameword gs) (correctGuesses gs))
    putStrLn "Type a guess:"
    guess <- getNonSpaceChar
    let updatedGs = unwrap (progressGame gs guess) 
    case state updatedGs of
      Left v -> do
        putStrLn $ "Game over: " ++ v
        putStrLn "The word was:"
        printClue printGameWord (gameword updatedGs)
      Right v -> do
        putStrLn $ "Try again: " ++ v
        putStrLn "Incorrect guesses:"
        printClue printIncorrectChar (inCorrectGuesses updatedGs)
        tick updatedGs


runGame :: IO ()
runGame = do 
    putStrLn "Hello!"
    let contentsText = decodeUtf8 contentBytes
    let contents = Data.Text.unpack contentsText
    let gameWords = Data.List.lines contents
    randI <- randomRIO (0, Data.List.length gameWords - 1)
    let pickedWord = gameWords !! randI
    let pickedWordTxt = Data.Text.pack pickedWord
    let _gameword = Data.Text.unpack pickedWordTxt
    putStrLn "Let's play hangman!"
    tick GameState{correctGuesses = [], inCorrectGuesses = [], gameword = _gameword, state = Right ""}
  
    

