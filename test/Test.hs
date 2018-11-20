{-# LANGUAGE DataKinds #-}

module Main where

import Test.Tasty
import Test.Tasty.Golden

import Data.ByteString.Lazy as BS
import Data.Binary.Get as G
import Data.Word

import Clash.Prelude

import Top
import Types

getWords :: Get [Word32]
getWords = do
    empty <- isEmpty
    if empty
    then pure mempty
    else do
        w <- getWord32host
        ws<- getWords
        pure (w:ws)

readWords :: FilePath -> IO [Word32]
readWords path = do
    input <- BS.readFile path
    let len = fromIntegral $ BS.length input
        len' = len - (len `mod` 4)
    pure $ runGet (isolate len' getWords) input

testDecode :: [Instruction] -> [InstrDescr]
testDecode ilist = simulate decodeRig ilist

main :: IO ()
main = do
    ws <- readWords "test/dummy.bin"
    print ws
    let ins = fmap (Instruction . fromIntegral) ws
    print $ testDecode ins
