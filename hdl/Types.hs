{-# LANGUAGE DuplicateRecordFields #-}

module Types where

import Clash.Prelude
import Data.Default.Class

data InstrType = R | I | S | B | U | J | Z deriving (Eq, Show)

data Instr = LOAD | STORE | LUI | AUIPC | JAL | JALR | BRANCH | IARITH | ARITH | INVALID deriving (Eq, Show)

data Width = Byte | HalfWord | Word deriving (Eq, Show)

data AluOp = ADD | SUB | SLL | SLT | SLTU | XOR | OR | AND | SRL | SRA deriving (Eq, Show)
data AluSrc = Zero | Rs | Imm12 | Imm20 | OffImm12 | OffImm20 | Pc deriving (Eq, Show)

data JumpType = Jump | Branch deriving (Eq, Show)

data WritebackSrc = WbPc | WbAluRes | WbMem deriving (Eq, Show)

data BranchType = EQ | NE | LT | GE | LTU | GEU deriving (Eq, Show)

data InstrDescr = InstrDescr {
    aluOp :: Maybe AluOp,
    aluSrc1 :: Maybe AluSrc,
    aluSrc2 :: Maybe AluSrc,
    brType :: Maybe BranchType,
    writeback :: Maybe WritebackSrc,
    jumpType :: Maybe JumpType,
    memRequest :: Bool,
    memWrite :: Bool
    } deriving (Eq, Show)

instance Default InstrDescr where
    def = InstrDescr Nothing Nothing Nothing Nothing Nothing Nothing False False

decodeOpcode :: BitVector 5 -> Instr
decodeOpcode 0b00000 = LOAD
decodeOpcode 0b01000 = STORE
decodeOpcode 0b01101 = LUI
decodeOpcode 0b00101 = AUIPC
decodeOpcode 0b11011 = JAL
decodeOpcode 0b11001 = JALR
decodeOpcode 0b11000 = BRANCH
decodeOpcode 0b00100 = IARITH
decodeOpcode 0b01100 = ARITH
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
decodeBranchType _ = undefined

instrType :: Instr -> InstrType
instrType LOAD = I
instrType STORE = S
instrType LUI = U
instrType AUIPC = U
instrType JAL = J
instrType JALR = I
instrType BRANCH = B
instrType IARITH = I

