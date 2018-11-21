{-# LANGUAGE RecordWildCards, BinaryLiterals, DataKinds, ViewPatterns #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}

module Stage.Writeback (
)where

import Clash.Prelude

import Types



-- aluMux :: AluSrc 