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
irom = fmap Instruction . romFilePow2 "../rv32/test/dummy.lit"

nextPc :: Signal dom MWord -> Signal dom (Maybe MWord) -> Signal dom MWord
nextPc current doJump = fromMaybe <$> current <*> doJump

{-# NOINLINE fetchStage #-}
fetchStage :: HiddenClockReset dom gated synchronous => DataFlow dom Bool Bool (Maybe MWord) FetchResults
fetchStage = liftDF go
    where
        go doJump iV iR = (FetchResults <$> irom resizedPc <*> regPc, iV, iR)
            where
                resetVector = 0
                regPc = regEn resetVector ((&&) <$> iV <*> iR) (nextPc regPc doJump)
                resizedPc = unpack.resize <$> regPc
