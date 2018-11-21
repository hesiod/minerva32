module Top (
    decodeRig, topEntity
) where

import Types
import Stage.InstrDecode

import Clash.Prelude

decodeRig :: Signal dom FetchResults -> Signal dom InstrDescr
decodeRig instr = decoded
    where
        (decoded,_,_) = (df decodeStage) instr alwaysTrue alwaysTrue
        alwaysTrue = fromList [True]

topEntity :: SystemClockReset => Signal System FetchResults -> Signal System InstrDescr
topEntity = decodeRig
