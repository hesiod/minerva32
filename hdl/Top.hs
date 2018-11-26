module Top (
    cpu, cpu', tb
) where

import Types
import Stage.InstrFetch
import Stage.InstrDecode
import Stage.Execute
import Stage.Writeback

import Clash.Prelude

import Clash.Explicit.Testbench

import Primitives.PLL

alwaysTrue :: Signal dom Bool
alwaysTrue = pure True

{-# NOINLINE cpu #-}
cpu :: HiddenClockReset dom gated synchronous => Signal dom RegisterFile
cpu = go initiallyInvalid alwaysTrue
    where
        initiallyInvalid = register False (pure True)
        go iV iR = regFile
            where
                fdFlow = df $ fetchStage `seqDF` decodeStage
                --ewFlow = df $ executeStage `seqDF` writebackStage
                eFlow = df executeStage
                wFlow = df writebackStage
                
                (descr, fdV, fdR) = fdFlow (erDoJump <$> executeResults) iV eR
                (eres, eV, eR) = eFlow (bundle (descr, forwardResponse)) fdV wR
                (executeResults, forwardRequest, readResult) = unbundle eres
                (wres, wV, wR) = wFlow (bundle (executeResults, forwardRequest, readResult)) eV iR
                (regFile, forwardResponse) = unbundle wres

{-# NOINLINE cpu' #-}
{-# ANN cpu' (Synthesize {
    t_name = "cpu",
    t_inputs = [PortName "ref_clk", PortName "ext_reset"],
    t_output = PortName "dummyBit"
    }) #-}
cpu' :: Clock RefClkDomain Source -> Reset RefClkDomain Asynchronous -> Signal PllDomain Bit
cpu' sclk srst = withClockReset clk rstSync $ (\rf -> (rf !! 0) ! 0) <$> cpu
    where
        (clk, locked) = pll sclk srst
        rstSync = resetSynchronizer clk (unsafeToAsyncReset locked)

{-# NOINLINE tb #-}
{- ANN tb (defSyn "tb") #-}
tb :: Clock System Source -> Reset System Asynchronous -> Signal System Bool
tb = exposeClockReset done
    where
        vo = replicate d1000 def
        toutput = outputVerifier clk rst vo
        done = toutput (withClockReset clk rst cpu)
        clk = tbSystemClockGen (not <$> done)
        rst = systemResetGen
