{-# LANGUAGE TemplateHaskell #-}
module Lib
    ( someFunc
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

newtype Game a = Game {gameword :: a}
instance Monad Game where
  gs >>= k        = k (gameword gs)

instance Functor Game where
  fmap f (Game x) = Game (f x)

instance Applicative Game where
  pure = Game
  Game f <*> Game x = Game (f x)

unwrap :: Game a -> a
unwrap = gameword

addStuff :: [Char] -> Game [Char]
addStuff toAdd = Game {gameword = toAdd ++ "lolzerbolzer"}

anotherFunc :: [Char] -> Game [Char]
anotherFunc myStr = do
  newStr <- addStuff myStr
  addStuff newStr


someFunc :: IO ()
someFunc = do 
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
    let lol = unwrap (anotherFunc "lolzer")
    putStrLn lol
    putStrLn "reallydone!"
    putStrLn "Type a guess:"
    guess <- getChar
    putStrLn ("You typed: " ++ [guess])
    

