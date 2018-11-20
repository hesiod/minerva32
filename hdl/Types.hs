{-# LANGUAGE DataKinds, DeriveGeneric, DeriveAnyClass #-}

module Types where

import GHC.Generics (Generic)
import Control.DeepSeq

import Clash.Prelude
import Data.Default.Class

data InstrType = R | I | S | B | U | J | Z deriving (Eq, Show, Generic, NFData)

data Opcode = LOAD | STORE | LUI | AUIPC | JAL | JALR | BRANCH | IARITH | ARITH | INVALID deriving (Eq, Show, Generic, NFData)

data Width = Byte | HalfWord | Word deriving (Eq, Show, Generic, NFData)

--data AluOp = ADD | SUB | SLL | SLT | SLTU | XOR | OR | AND | SRL | SRA deriving (Eq, Show, Generic, NFData)
data AluOp = Add | Sub | Sll | Slt | Sltu | Xor | Or | And | Srl | Sra deriving (Eq, Show, Generic, NFData)
data AluSrc = Zero | Rs | Imm12 | Imm20 | OffImm12 | OffImm20 | Pc deriving (Eq, Show, Generic, NFData)

data JumpType = Jump | Branch deriving (Eq, Show, Generic, NFData)

data WritebackSrc = WbPc | WbAluRes | WbMem deriving (Eq, Show, Generic, NFData)

data BranchType = EQ | NE | LT | GE | LTU | GEU deriving (Eq, Show, Generic, NFData)

data MemoryRequest = MemWrite | MemRead deriving (Eq, Show, Generic, NFData)

type RegisterIndex = Index 32

newtype Instruction = Instruction (BitVector 32) deriving (Generic, NFData)

data InterInstr = InterInstr {
    opcode :: Opcode,
    funct3 :: BitVector 3,
    funct7 :: BitVector 7,
    immediate :: BitVector 32,
    rs1 :: RegisterIndex,
    rs2 :: RegisterIndex,
    rd :: RegisterIndex
    } deriving (Eq, Show, Generic, NFData)

instance Default InterInstr where
    def = InterInstr INVALID zeroBits zeroBits zeroBits minBound minBound minBound

data AluCtrl = AluCtrl {
    aluOp :: AluOp,
    aluSrc1 :: AluSrc,
    aluSrc2 :: AluSrc
    } deriving (Eq, Show, Generic, NFData)

instance Default AluCtrl where
    def = AluCtrl Add Zero Zero

data InstrDescr = InstrDescr {
    inter :: InterInstr,
    aluCtrl :: AluCtrl,
    branchType :: Maybe BranchType,
    jumpType :: Maybe JumpType,
    writebackSrc :: Maybe WritebackSrc,
    memoryRequest :: Maybe MemoryRequest
    } deriving (Eq, Show, Generic, NFData)

instance Default InstrDescr where
    def = InstrDescr def def Nothing Nothing Nothing Nothing
    