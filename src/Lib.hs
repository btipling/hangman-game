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
      Color(Red),
      ColorIntensity(Vivid),
      ConsoleLayer(Foreground),
      SGR(Reset, SetColor) )
import Control.Monad (forM_)

contentBytes :: Data.ByteString.ByteString
contentBytes = $(embedFile "./resources/words.txt")

printPickedChar :: Char -> IO ()
printPickedChar charToDisplay = do 
  setSGR [SetColor Foreground Vivid Red]
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

addStuff :: GameState -> Game GameState
addStuff gs = Game (gs{gameword = gameword gs ++ "lolzerbolzer"})

continueGame :: GameState -> String -> Game GameState
continueGame gs msg = Game (gs{state=Right msg})

gameOver :: GameState -> String -> Game GameState
gameOver gs msg = Game (gs{state=Left msg})

progressGame :: GameState -> Game GameState
progressGame myStr = do
  newStr <- addStuff myStr
  gs <- addStuff newStr
  if length (inCorrectGuesses gs) > maxGuesses then
    gameOver gs "All done!"
  else
    continueGame gs "Keep going!"

maxGuesses :: Int
maxGuesses = 6

tick :: GameState -> IO ()
tick gs = do
    putStrLn "Type a guess:"
    guess <- getChar
    let updatedGs = unwrap (progressGame gs{inCorrectGuesses = inCorrectGuesses gs ++ [guess]}) 
    case state updatedGs of
      Left v -> putStrLn $ "Game over: " ++ v
      Right v -> do
        putStrLn $ "Tray again: " ++ v
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
    let charList = Data.Text.unpack pickedWordTxt
    forM_  charList $ \x -> if x == 'a' then printPickedChar x else putChar x
    putChar '\n'
    putStrLn "All done!"
    tick GameState{correctGuesses = [], inCorrectGuesses = [], gameword = "lolzer", state = Right "Game started"}
  
    

