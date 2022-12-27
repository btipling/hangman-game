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

