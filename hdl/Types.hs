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

type RegisterIndex = Index 32

data InterInstr = InterInstr {
    funct3 :: BitVector 3,
    funct7 :: BitVector 7,
    immediate :: BitVector 32,
    rs1 :: RegisterIndex,
    rs2 :: RegisterIndex,
    rd :: RegisterIndex
    } deriving (Eq, Show)

instance Default InterInstr where
    def = InterInstr zeroBits zeroBits zeroBits minBound minBound minBound

data InstrDescr = InstrDescr {
    inter :: InterInstr,
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
    def = InstrDescr def Nothing Nothing Nothing Nothing Nothing Nothing False False
    