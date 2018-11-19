module Stage.InstrDecode where

import Clash.Prelude

import Types

decodeOpcode :: BitVector 7 -> Instr
decodeOpcode 0b1100000 = LOAD
decodeOpcode 0b1101000 = STORE
decodeOpcode 0b1101101 = LUI
decodeOpcode 0b1100101 = AUIPC
decodeOpcode 0b1111011 = JAL
decodeOpcode 0b1111001 = JALR
decodeOpcode 0b1111000 = BRANCH
decodeOpcode 0b1100100 = IARITH
decodeOpcode 0b1101100 = ARITH
decodeOpcode _ = INVALID

decodeWidth :: BitVector 3 -> Width
decodeWidth 0b000 = Byte
decodeWidth 0b001 = HalfWord
decodeWidth 0b010 = Word
decodeWidth _ = Word

decodeBranchType :: BitVector 3 -> BranchType
decodeBranchType 0b000 = Types.EQ
decodeBranchType 0b001 = NE
decodeBranchType 0b100 = Types.LT
decodeBranchType 0b101 = GE
decodeBranchType 0b110 = LTU
decodeBranchType 0b111 = GEU
decodeBranchType _ = Clash.Prelude.undefined

instrType :: Instr -> InstrType
instrType LOAD = I
instrType STORE = S
instrType LUI = U
instrType AUIPC = U
instrType JAL = J
instrType JALR = I
instrType BRANCH = B
instrType IARITH = I

interDecode :: InstrType -> BitVector 32 -> InterInstr
interDecode Z _ = def
interDecode I instr = InterInstr { 
        funct3 = slice d14 d12 instr,
        funct7 = def,
        immediate = (zeroBits :: BitVector 20) ++# slice d31 d20 instr,
        rs1 = unpack $ slice d19 d15 instr,
        rs2 = def,
        rd = unpack $ slice d11 d7 instr
    }
interDecode R instr = InterInstr { 
        funct3 = slice d14 d12 instr,
        funct7 = slice d31 d25 instr,
        immediate = def,
        rs1 = unpack $ slice d19 d15 instr,
        rs2 = unpack $ slice d24 d20 instr,
        rd = unpack $ slice d11 d7 instr
    }
interDecode S instr = InterInstr { 
    funct3 = slice d14 d12 instr,
    funct7 = def,
    immediate = (zeroBits :: BitVector 20) ++# slice d31 d25 instr ++# slice d11 d7 instr,
    rs1 = unpack $ slice d19 d15 instr,
    rs2 = unpack $ slice d24 d20 instr,
    rd = unpack $ slice d11 d7 instr
}
interDecode B instr = InterInstr { 
    funct3 = slice d14 d12 instr,
    funct7 = def,
    immediate = (zeroBits :: BitVector 19) ++# (pack $ instr ! 31) ++# (pack $ instr ! 7) ++# slice d30 d25 instr ++# slice d11 d8 instr ++# (zeroBits :: BitVector 1),
    rs1 = unpack $ slice d19 d15 instr,
    rs2 = unpack $ slice d24 d20 instr,
    rd = unpack $ slice d11 d7 instr
}
interDecode U instr = InterInstr { 
        funct3 = def,
        funct7 = def,
        immediate = slice d31 d12 instr ++# (zeroBits :: BitVector 12),
        rs1 = def,
        rs2 = def,
        rd = unpack $ slice d11 d7 instr
    }
interDecode J instr = InterInstr { 
        funct3 = def,
        funct7 = def,
        immediate = (zeroBits :: BitVector 11) ++# (pack $ instr ! 31) ++# slice d19 d12 instr ++# (pack $ instr ! 20) ++# slice d30 d21 instr ++# (zeroBits :: BitVector 1),
        rs1 = def,
        rs2 = def,
        rd = unpack $ slice d11 d7 instr
    }

decode :: BitVector 32 -> InstrDescr
decode instr = undefined
    where
        opcode = decodeOpcode $ slice d6 d0 instr
        itype = instrType opcode
        inter = interDecode itype instr