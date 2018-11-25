{-# LANGUAGE RecordWildCards, BinaryLiterals, DataKinds, ViewPatterns, NamedFieldPuns #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}

module Stage.InstrDecode (
    interDecode, decode,
    decodeStage, decodeRig
)where

import Data.Maybe
import Clash.Prelude

import Types

decodeOpcode :: BitVector 7 -> Opcode
decodeOpcode 0b0000011 = LOAD
decodeOpcode 0b0100011 = STORE
decodeOpcode 0b0110111 = LUI
decodeOpcode 0b0010111 = AUIPC
decodeOpcode 0b1101111 = JAL
decodeOpcode 0b1100111 = JALR
decodeOpcode 0b1100011 = BRANCH
decodeOpcode 0b0010011 = IARITH
decodeOpcode 0b0110011 = ARITH
decodeOpcode 0b0001111 = FENCE
decodeOpcode 0b1110011 = SYSTEM
decodeOpcode _ = INVALID

decodeCsr :: BitVector 3 -> SystemOp
decodeCsr f3 = case f3 of
    0b000 -> ECallBreak
    0b001 -> CsrWrite
    0b010 -> CsrRead
    0b011 -> CsrClear
    0b101 -> CsrWriteImmediate
    0b110 -> CsrReadImmediate
    0b111 -> CsrClearImmediate

decodeWidth :: BitVector 3 -> Width
decodeWidth 0b000 = Byte
decodeWidth 0b001 = HalfWord
decodeWidth 0b010 = Word
decodeWidth _ = Word

decodeBranchType :: BitVector 3 -> BranchType
decodeBranchType 0b000 = B_EQ
decodeBranchType 0b001 = B_NE
decodeBranchType 0b100 = B_LT
decodeBranchType 0b101 = B_GE
decodeBranchType 0b110 = B_LTU
decodeBranchType 0b111 = B_GEU
decodeBranchType _ = B_EQ -- shouldnt occur

instrType :: Opcode -> InstrType
instrType LOAD = I
instrType STORE = S
instrType LUI = U
instrType AUIPC = U
instrType JAL = J
instrType JALR = I
instrType BRANCH = B
instrType ARITH = R
instrType IARITH = I
instrType INVALID = Z
instrType SYSTEM = I
instrType FENCE = Z

interDecode :: Instruction -> InterInstr
interDecode (Instruction instr) =
        let opcode = decodeOpcode $ slice d6 d0 instr
            itype = instrType opcode
        in idecode itype
    where
        idecode Z = def
        idecode I = InterInstr {
                opcode = decodeOpcode $ slice d6 d0 instr,
                funct3 = slice d14 d12 instr,
                funct7 = def,
                immediate = (zeroBits :: BitVector 20) ++# slice d31 d20 instr,
                rs1 = unpack $ slice d19 d15 instr,
                rs2 = def,
                rd = unpack $ slice d11 d7 instr
            }
        idecode R = InterInstr {
                opcode = decodeOpcode $ slice d6 d0 instr,
                funct3 = slice d14 d12 instr,
                funct7 = slice d31 d25 instr,
                immediate = def,
                rs1 = unpack $ slice d19 d15 instr,
                rs2 = unpack $ slice d24 d20 instr,
                rd = unpack $ slice d11 d7 instr
            }
        idecode S = InterInstr {
                opcode = decodeOpcode $ slice d6 d0 instr,
                funct3 = slice d14 d12 instr,
                funct7 = def,
                immediate = (zeroBits :: BitVector 20) ++# slice d31 d25 instr ++# slice d11 d7 instr,
                rs1 = unpack $ slice d19 d15 instr,
                rs2 = unpack $ slice d24 d20 instr,
                rd = unpack $ slice d11 d7 instr
            }
        idecode B = InterInstr {
                opcode = decodeOpcode $ slice d6 d0 instr,
                funct3 = slice d14 d12 instr,
                funct7 = def,
                immediate = (zeroBits :: BitVector 19) ++# pack (instr ! 31) ++# pack (instr ! 7) ++# slice d30 d25 instr ++# slice d11 d8 instr ++# (zeroBits :: BitVector 1),
                rs1 = unpack $ slice d19 d15 instr,
                rs2 = unpack $ slice d24 d20 instr,
                rd = unpack $ slice d11 d7 instr
            }
        idecode U = InterInstr {
                opcode = decodeOpcode $ slice d6 d0 instr,
                funct3 = def,
                funct7 = def,
                immediate = slice d31 d12 instr ++# (zeroBits :: BitVector 12),
                rs1 = def,
                rs2 = def,
                rd = unpack $ slice d11 d7 instr
            }
        idecode J = InterInstr {
                opcode = decodeOpcode $ slice d6 d0 instr,
                funct3 = def,
                funct7 = def,
                immediate = (zeroBits :: BitVector 11) ++# pack (instr ! 31) ++# slice d19 d12 instr ++# pack (instr ! 20) ++# slice d30 d21 instr ++# (zeroBits :: BitVector 1),
                rs1 = def,
                rs2 = def,
                rd = unpack $ slice d11 d7 instr
            }

decodeBasicArith :: Bool -> Bit -> BitVector 3 -> AluOp
decodeBasicArith False 0b1 0b000 = Sub
decodeBasicArith _ funct7_5 funct3 = case funct3 of
    0b000 -> Add
    0b001 -> Sll
    0b010 -> Slt
    0b011 -> Sltu
    0b100 -> Xor
    0b110 -> Or
    0b111 -> And
    0b101 -> if funct7_5 == high then Sra else Srl
    _ -> Add

decodeArith :: InterInstr -> AluOp
decodeArith InterInstr{..} = decodeBasicArith (opcode == IARITH) (funct7 ! 5) funct3

{-# NOINLINE writeback #-}
writeback :: Opcode -> Maybe WritebackSrc
writeback LOAD = Just WbMem
writeback LUI = Just WbAluRes
writeback AUIPC = Just WbAluRes
writeback JAL = Just WbPc
writeback JALR = Just WbPc
writeback IARITH = Just WbAluRes
writeback ARITH = Just WbAluRes
writeback _ = Nothing

{-# NOINLINE memrequest #-}
memrequest :: Opcode -> Maybe MemoryRequest
memrequest opc = case opc of
    LOAD -> Just MemRead
    STORE -> Just MemWrite
    _ -> Nothing

{-# NOINLINE jumptype #-}
jumptype :: BitVector 3 -> Opcode -> Maybe JumpType
jumptype f3 opc = case opc of
    JAL -> Just Jump
    JALR -> Just Jump
    BRANCH -> Just . Branch . decodeBranchType $ f3
    _ -> Nothing

{-# NOINLINE alu #-}
alu :: InterInstr -> Opcode -> AluCtrl
alu inter opc = case opc of
    LOAD -> AluCtrl Add Rs Imm12
    STORE -> AluCtrl Add Rs Imm12
    LUI -> AluCtrl Add Zero Imm20
    AUIPC -> AluCtrl Add Pc Imm20
    JAL -> AluCtrl Add Pc OffImm20
    JALR -> AluCtrl Add Rs Imm12
    BRANCH -> AluCtrl Add Pc OffImm12
    IARITH -> AluCtrl (decodeArith inter) Rs Imm12
    ARITH -> AluCtrl (decodeArith inter) Rs Rs
    INVALID -> def

{-# NOINLINE decode #-}
decode :: FetchResults -> InstrDescr
decode (FetchResults (interDecode -> inter@InterInstr{opcode, funct3}) pc) = InstrDescr {
        inter = inter,
        writebackSrc = writeback opcode,
        memoryRequest = memrequest opcode,
        jumpType = jumptype funct3 opcode,
        aluCtrl = alu inter opcode,
        pc = pc
    }

{-# NOINLINE decodeStage #-}
decodeStage :: HiddenClockReset dom gated synchronous => DataFlow dom Bool Bool (FetchResults, Kill) InstrDescr
decodeStage = liftDF go
    where
        go (unbundle -> (fetchResults, kill)) iV iR = (decoded, iV, iR)
            where
                ce = (&&) <$> ((&&) <$> iV <*> iR) <*> (shouldKill <$> kill)
                oV = register False ce
                decoded = regEn def ce (decode <$> fetchResults)

decodeRig :: HiddenClockReset dom gated synchronous => Signal dom (FetchResults, Kill) -> Signal dom InstrDescr
decodeRig inval = decoded
    where
        (decoded,_,_) = df decodeStage inval alwaysTrue alwaysTrue
        alwaysTrue = fromList [True]
