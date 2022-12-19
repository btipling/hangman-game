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

contentBytes :: Data.ByteString.ByteString
contentBytes = $(embedFile "./resources/words.txt")

someFunc :: IO ()
someFunc = do 
    putStrLn "Hello!"
    let contentsText = decodeUtf8 contentBytes
    let contents = Data.Text.unpack contentsText
    let gameWords = Data.List.lines contents
    randI <- randomRIO (0, Data.List.length gameWords - 1)
    -- print a random word
    putStrLn (gameWords !! randI)
