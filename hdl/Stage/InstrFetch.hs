{-# LANGUAGE RecordWildCards, BinaryLiterals, DataKinds, ViewPatterns, ImplicitParams #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

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
fetchStage :: HiddenClockReset dom gated synchronous => DataFlow dom Bool Bool DoJump (FetchResults, Kill)
fetchStage = liftDF go
    where
        go doJump iV iR = (bundle (FetchResults <$> readResult <*> regPc, Kill . isJust <$> doJump), oV, iR)
            where
                resetVector = 0
                next = nextPc regNextPc doJump
                ce = (&&) <$> iV <*> iR
                oV = register False ce
                regNextPc = regEn resetVector ce next
                regPc = regEn resetVector ce regNextPc
                resizedPc = unpack.resize.slice d31 d2 <$> regNextPc
                readResult = irom resizedPc
