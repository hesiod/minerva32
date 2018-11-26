{-# LANGUAGE DataKinds, QuasiQuotes #-}

module Primitives.PLL (
    RefClkDomain, PllDomain,
    pll
) where

import Clash.Prelude

import Clash.Annotations.Primitive
import Clash.Prelude
import Data.String.Interpolate      (i)
import Data.String.Interpolate.Util (unindent)

import Clash.Signal.Internal
import Unsafe.Coerce

type RefClkDomain = Dom "ref_clk" 83334
type PllDomain = Dom "pll_clk" 33334

-- .FEEDBACK_PATH(\"SIMPLE\"),
{-# ANN pll (InlinePrimitive Verilog $ unindent [i|
  [ { "BlackBox" :
      { "name" : "Primitives.PLL.pll"
      , "kind": "Declaration"
      , "template" : 
  "// begin InlinePrimitive pll:
  wire ~GENSYM[clock_out][1];
  wire ~GENSYM[locked][2];
  
  SB_PLL40_PAD #(
        .DIVR(4'b0000),
        .DIVF(7'b1001111),
        .DIVQ(3'b101),
        .FILTER_RANGE(3'b001)
    ) ~GENSYM[uut][0] (
        .LOCK(~SYM[2]),
        .RESETB(1'b1),
        .BYPASS(1'b0),
        .PACKAGEPIN(~ARG[0]),
        .PLLOUTCORE(~SYM[1])
    );
  assign ~RESULT = { ~SYM[2], ~SYM[1] };
  // end InlinePrimitive pll"
      }
    }
  ]
  |]) #-}
{-# NOINLINE pll #-}
pll :: Clock RefClkDomain Source -> Reset RefClkDomain Asynchronous -> (Clock PllDomain Source, Signal PllDomain Bool)
pll clk (Async rst) = (unsafeCoerce (clockGate clk rst), unsafeCoerce rst)