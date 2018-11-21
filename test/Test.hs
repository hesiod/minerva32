{-# LANGUAGE DataKinds #-}

module Main where

import Test.Tasty
import Test.Tasty.Golden

import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get as G
import Data.Word

import Clash.Prelude
import Clash.Explicit.Testbench
import qualified Prelude (zip)

import Top
import Types

import TestData
import Util

testDecode :: [FetchResults] -> [InstrDescr]
testDecode ilist = simulate decodeRig ilist

tb :: Signal System Bool
tb = done
    where
        vi = $(listToVecTH frs)
        vo = $(listToVecTH out)
        tinput = stimuliGenerator clk rst vi
        toutput = outputVerifier clk rst vo
        done = toutput (withClockReset clk rst $ decodeRig tinput)
        clk = tbSystemClockGen (not <$> done)
        rst = systemResetGen

topEntity :: Signal System Bool
topEntity = tb

main :: IO ()
main = do
    ws <- readWords "test/dummy.bin"
    print ws
    let ins = fmap (\(pc, i) -> FetchResults (Instruction (fromIntegral i :: BitVector 32)) pc) $ Prelude.zip [0,4..] ws
    print $ testDecode ins
