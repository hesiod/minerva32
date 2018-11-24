module Top (
    cpu, cpu'
) where

import Types
import Stage.InstrFetch
import Stage.InstrDecode
import Stage.Execute
import Stage.Writeback

import Clash.Prelude

{-# NOINLINE cpu #-}
cpu :: HiddenClockReset dom gated synchronous => DataFlow dom Bool Bool () RegisterFile
cpu = liftDF go
    where
        go _ iV iR = (regFile, wV, wR)
            where
                fdFlow = df $ fetchStage `seqDF` decodeStage
                --ewFlow = df $ executeStage `seqDF` writebackStage
                eFlow = df $ executeStage
                wFlow = df $ writebackStage
                
                (descr, fdV, fdR) = fdFlow (erDoJump <$> executeResults) iV eR
                (eres, eV, eR) = eFlow (bundle (descr, forwardResponse)) fdV wR
                (executeResults, forwardRequest, readResult) = unbundle eres
                (wres, wV, wR) = wFlow (bundle (executeResults, forwardRequest, readResult)) eV iR
                (regFile, forwardResponse) = unbundle wres

--Signal System () -> Signal System Bool -> Signal System Bool -> 

{-# ANN cpu' (defSyn "cpu") #-}
cpu' :: Clock System Source -> Reset System Synchronous -> (Signal System RegisterFile, Signal System Bool, Signal System Bool)
cpu' clk rst = (exposeClockReset $ df cpu) clk rst (pure ()) alwaysTrue alwaysTrue
    where
        alwaysTrue = pure True -- fromList [True]

{-
topEntity :: SystemClockReset => Signal System FetchResults -> Signal System InstrDescr
topEntity = decodeRig
-}