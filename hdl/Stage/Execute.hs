{-# LANGUAGE RecordWildCards, NamedFieldPuns, BinaryLiterals, DataKinds, ViewPatterns, ConstraintKinds #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}

module Stage.Execute (
    execute,
    executeStage
) where

import Clash.Prelude

import Types

type Operands = (MWord, MWord)

branchCompare :: BranchType -> Operands -> Bit
branchCompare b (op1, op2) = boolToBit $ go b
    where
        go B_EQ  = op1 == op2
        go B_NE  = op1 /= op2
        go B_LT  = (unpack op1 :: Signed 32)   <  (unpack op2 :: Signed 32)
        go B_GE  = (unpack op1 :: Signed 32)   >= (unpack op2 :: Signed 32)
        go B_LTU = (unpack op1 :: Unsigned 32) <  (unpack op2 :: Unsigned 32)
        go B_GEU = (unpack op1 :: Unsigned 32) >= (unpack op2 :: Unsigned 32)

runAlu :: AluOp -> Operands -> MWord
runAlu Add  (op1, op2) = op1 + op2
runAlu Sub  (op1, op2) = op1 - op2
runAlu Sll  (op1, op2) = op1 `shiftL` fromIntegral op2
runAlu Slt  (op1, op2) = (zeroBits :: BitVector 31) ++# pack ((unpack op1 :: Signed 32)   < (unpack op2 :: Signed 32))
runAlu Sltu (op1, op2) = (zeroBits :: BitVector 31) ++# pack ((unpack op1 :: Unsigned 32) < (unpack op2 :: Unsigned 32))
runAlu Xor  (op1, op2) = op1 `xor` op2
runAlu Or   (op1, op2) = op1 .|. op2
runAlu And  (op1, op2) = op1 .&. op2
runAlu Srl  (op1, op2) = op1 `rotateR` fromIntegral (slice d4 d0 op2)
runAlu Sra  (op1, op2) = op1 `shiftR`  fromIntegral (slice d4 d0 op2)

aluSelect :: InstrDescr -> Operands -> Operands
aluSelect InstrDescr{pc,inter,aluCtrl} (vrs1, vrs2) = (select (aluSrc1 aluCtrl) vrs1, select (aluSrc2 aluCtrl) vrs2)
    where
        select :: AluSrc -> MWord -> MWord
        select Zero _ = zeroBits
        select Rs vrs = vrs
        select Pc _ = pc
        select Imm12 _ = pack $ resize (unpack (slice d11 d0 . immediate $ inter) :: Signed 12)
        select OffImm12 _ = pack $ resize (unpack (slice d12 d0 . immediate $ inter) :: Signed 13)
        select Imm20 _ = (slice d31 d12 . immediate $ inter) ++# zeroBits
        select OffImm20 _ = pack $ resize (unpack (slice d20 d0 . immediate $ inter) :: Signed 21)

execute :: InstrDescr -> Operands -> ExecuteResults
execute descr@InstrDescr{..} vrs = ExecuteResults alu_res do_jump
    where
        ops = aluSelect descr vrs
        alu_res = runAlu (aluOp aluCtrl) ops
        do_jump = case jumpType of
            Nothing -> 0
            Just Jump -> 1
            Just (Branch branchtype) -> branchCompare branchtype ops

executeStage :: HiddenClockReset dom gated synchronous => DataFlow dom Bool Bool (InstrDescr, ForwardResponse) (ExecuteResults, ForwardRequest)
executeStage = liftDF go
    where 
        go (unbundle -> (descr, response)) iV iR = (bundle (re, req), iV, iR)
            where
                vrs = bundle (vrs1 <$> response, vrs2 <$> response)
                req = uncurry ForwardRequest <$> bundle (fmap (rs1.inter) descr, fmap (rs2.inter) descr)
                re = regEn def ((&&) <$> iV <*> iR) (execute <$> descr <*> vrs)

