{-# LANGUAGE RecordWildCards, BinaryLiterals, DataKinds, ViewPatterns #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}

module Stage.InstrFetch (
    fetchStage
) where

import Clash.Prelude

import Data.Maybe

import Types

irom :: HiddenClockReset dom gated synchronous => Signal dom (Unsigned 8) -> Signal dom Instruction
irom = fmap Instruction . romFilePow2 "dummy.lit"

nextPc :: Signal dom MWord -> Signal dom (Maybe MWord) -> Signal dom MWord
nextPc current doJump = fromMaybe <$> (current + 4) <*> doJump

{-# NOINLINE fetchStage #-}
fetchStage :: HiddenClockReset dom gated synchronous => DataFlow dom Bool Bool (Maybe MWord) FetchResults
fetchStage = liftDF go
    where
        go doJump iV iR = (FetchResults <$> readResult <*> regPc, oV, iR)
            where
                resetVector = 0
                next = nextPc regNextPc doJump
                ce = (&&) <$> iV <*> iR
                oV = register False ce
                regNextPc = regEn resetVector ce next
                regPc = regEn resetVector ce regNextPc
                resizedPc = unpack.resize.(slice d31 d2) <$> regNextPc
                readResult = irom resizedPc
