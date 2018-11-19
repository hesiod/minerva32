{-# LANGUAGE DataKinds #-}

module Types where

import Clash.Prelude
import Data.Default.Class

data InstrType = R | I | S | B | U | J | Z deriving (Eq, Show)

data Opcode = LOAD | STORE | LUI | AUIPC | JAL | JALR | BRANCH | IARITH | ARITH | INVALID deriving (Eq, Show)

data Width = Byte | HalfWord | Word deriving (Eq, Show)

--data AluOp = ADD | SUB | SLL | SLT | SLTU | XOR | OR | AND | SRL | SRA deriving (Eq, Show)
data AluOp = Add | Sub | Sll | Slt | Sltu | Xor | Or | And | Srl | Sra deriving (Eq, Show)
data AluSrc = Zero | Rs | Imm12 | Imm20 | OffImm12 | OffImm20 | Pc deriving (Eq, Show)

data JumpType = Jump | Branch deriving (Eq, Show)

data WritebackSrc = WbPc | WbAluRes | WbMem deriving (Eq, Show)

data BranchType = EQ | NE | LT | GE | LTU | GEU deriving (Eq, Show)

data MemoryRequest = MemWrite | MemRead deriving (Eq, Show)

type RegisterIndex = Index 32

newtype Instruction = Instruction (BitVector 32)

data InterInstr = InterInstr {
    opcode :: Opcode,
    funct3 :: BitVector 3,
    funct7 :: BitVector 7,
    immediate :: BitVector 32,
    rs1 :: RegisterIndex,
    rs2 :: RegisterIndex,
    rd :: RegisterIndex
    } deriving (Eq, Show)

instance Default InterInstr where
    def = InterInstr ARITH zeroBits zeroBits zeroBits minBound minBound minBound

data AluCtrl = AluCtrl {
    aluOp :: AluOp,
    aluSrc1 :: AluSrc,
    aluSrc2 :: AluSrc
    } deriving (Eq, Show)

instance Default AluCtrl where
    def = AluCtrl Add Zero Zero

data InstrDescr = InstrDescr {
    inter :: InterInstr,
    aluCtrl :: AluCtrl,
    branchType :: Maybe BranchType,
    jumpType :: Maybe JumpType,
    writebackSrc :: Maybe WritebackSrc,
    memoryRequest :: Maybe MemoryRequest
    } deriving (Eq, Show)

instance Default InstrDescr where
    def = InstrDescr def def Nothing Nothing Nothing Nothing
    