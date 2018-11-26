{-# LANGUAGE RecordWildCards, BinaryLiterals, DataKinds, ViewPatterns, ScopedTypeVariables, NoImplicitPrelude, TypeFamilies #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}

module Stage.Writeback (
    writebackStage
) where

import Clash.Prelude

import Types

writebackData :: Maybe WritebackSrc -> MWord -> MWord -> MWord -> Maybe MWord
writebackData wbsrc alures memres pc = case wbsrc of
    Just WbPc -> Just pc
    Just WbAluRes -> Just alures
    Just WbMem -> Just memres
    Nothing -> Nothing

nextRegs :: RegisterIndex -> Maybe MWord -> RegisterFile -> RegisterFile
nextRegs rd (Just wbval) rf | rd /= 0 = replace rd wbval rf
nextRegs _ _ rf = rf

forward :: ExecuteResults -> RegisterFile -> ForwardResponse
forward executeResults regFile = ForwardResponse (fwd rs1v rdv) (fwd rs2v rdv)
    where
        fwd :: RegisterIndex -> RegisterIndex -> MWord
        fwd reqidx destidx = if (writebackSrc.erInstrDescr $ executeResults) == Just WbAluRes && reqidx == destidx then erAluRes executeResults else regFile !! reqidx
        (rs1v, rs2v, rdv) = let ii = inter.erInstrDescr $ executeResults
                            in (rs1 ii, rs2 ii, rd ii)

{-# NOINLINE writebackStage #-}
writebackStage :: forall dom gated synchronous. HiddenClockReset dom gated synchronous => DataFlow dom Bool Bool (ExecuteResults, ForwardRequest, MWord) (RegisterFile, ForwardResponse)
writebackStage = liftDF go
    where 
        go (unbundle -> (executeResults, req, readResult)) iV iR = (bundle (regFile, forward <$> executeResults <*> regFile), iV, iR)
            where
                wbdata = writebackData
                    <$> (writebackSrc.erInstrDescr <$> executeResults)
                    <*> (erAluRes <$> executeResults)
                    <*> readResult
                    <*> (pc.erInstrDescr <$> executeResults)
                regFile = let rdv = rd.inter.erInstrDescr <$> executeResults
                          in regEn (replicate d16 0xDEADC0DE) iV (nextRegs <$> rdv <*> wbdata <*> regFile)
