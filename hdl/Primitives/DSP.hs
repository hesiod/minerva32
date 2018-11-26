{-# LANGUAGE DataKinds, QuasiQuotes #-}

module Primitives.DSP (
    dspAddSub, dspAdd, dspSub
) where

import Clash.Prelude

import Clash.Annotations.Primitive
import Clash.Prelude
import Data.String.Interpolate      (i)
import Data.String.Interpolate.Util (unindent)

{-# ANN dspAddSub (InlinePrimitive Verilog $ unindent [i|
  [ { "BlackBox" :
      { "name" : "Primitives.DSP.dspAddSub"
      , "kind": "Declaration"
      , "template" : 
        "// begin InlinePrimitive dspAddSub:
        SB_MAC16 #(
            .A_REG(0),
            .B_REG(0),
            .C_REG(0),
            .D_REG(0),
            .TOPADDSUB_UPPERINPUT(1'b1),
            .TOPADDSUB_CARRYSELECT(2'b11),
            .BOTADDSUB_UPPERINPUT(1'b1)
        ) ~GENSYM[dsp][0] (
            .A(~ARG[1][31:16]),
            .B(~ARG[1][15:0]),
            .C(~ARG[2][31:16]),
            .D(~ARG[2][15:0]),
            .O(~RESULT),
            .CE(1'b1),
            .ADDSUBTOP(~ARG[0]),
            .ADDSUBBOT(~ARG[0])
        );
        // end InlinePrimitive dspAddSub"
      }
    }
  ]
  |]) #-}
{-# NOINLINE dspAddSub #-}
dspAddSub :: Signal dom Bit -> Signal dom (BitVector 32) -> Signal dom (BitVector 32) -> Signal dom (BitVector 32)
dspAddSub do_sub a b = doAddSub <$> do_sub <*> a <*> b
    where
        doAddSub 0 a b = a + b
        doAddSub 1 a b = a - b

dspAdd :: Signal dom (BitVector 32) -> Signal dom (BitVector 32) -> Signal dom (BitVector 32)
dspAdd = dspAddSub (pure 0)

dspSub :: Signal dom (BitVector 32) -> Signal dom (BitVector 32) -> Signal dom (BitVector 32)
dspSub = dspAddSub (pure 1)