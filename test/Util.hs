module Util (readWords) where

import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get as G
import Data.Word

import Clash.Prelude

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
