{-# LANGUAGE TemplateHaskell #-}
module Lib
    ( someFunc
    ) where


import Data.ByteString
import System.Random
import qualified Data.List
import Data.FileEmbed
import Data.Text.Encoding
import Data.Text
import System.Console.ANSI
import Control.Monad (forM_)

contentBytes :: Data.ByteString.ByteString
contentBytes = $(embedFile "./resources/words.txt")

printPickedChar :: Char -> IO ()
printPickedChar charToDisplay = do 
  setSGR [SetColor Foreground Vivid Red]
  putChar charToDisplay
  setSGR [Reset]

data Game a = Game {gameword :: a}
instance Monad Game where
  gs >>= k        = k (gameword gs)

instance Functor Game where
  fmap f (Game x) = Game (f x)

instance Applicative Game where
  pure = Game
  Game f <*> Game x = Game (f x)

unwrap :: (Game a) -> a
unwrap g = gameword g

addStuff :: [Char] -> Game [Char]
addStuff toAdd = Game {gameword = (toAdd ++ "lolzerbolzer")}

anotherFunc :: [Char] -> Game [Char]
anotherFunc myStr = do
  newStr <- addStuff myStr
  wut <- addStuff newStr
  return wut


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
    forM_  charList $ (\x -> if x == 'a' then printPickedChar x else putChar x) 
    putChar '\n'
    putStrLn "All done!"
    let lol = unwrap (anotherFunc "lolzer")
    putStrLn lol
    putStrLn "reallydone!"
    

