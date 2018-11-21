{-# LANGUAGE DeriveLift, DataKinds, DeriveGeneric, DeriveAnyClass #-}

module Types where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Clash.Prelude
import Data.Default.Class (Default(..))

data InstrType = R | I | S | B | U | J | Z deriving (Eq, Show, Generic, NFData, Lift, ShowX)

data Opcode = LOAD | STORE | LUI | AUIPC | JAL | JALR | BRANCH | IARITH | ARITH | INVALID deriving (Eq, Show, Generic, NFData, Lift, ShowX)

data Width = Byte | HalfWord | Word deriving (Eq, Show, Generic, NFData, Lift, ShowX)

--data AluOp = ADD | SUB | SLL | SLT | SLTU | XOR | OR | AND | SRL | SRA deriving (Eq, Show, Generic, NFData, Lift, ShowX)
data AluOp = Add | Sub | Sll | Slt | Sltu | Xor | Or | And | Srl | Sra deriving (Eq, Show, Generic, NFData, Lift, ShowX)
data AluSrc = Zero | Rs | Imm12 | Imm20 | OffImm12 | OffImm20 | Pc deriving (Eq, Show, Generic, NFData, Lift, ShowX)

data JumpType = Jump | Branch BranchType deriving (Eq, Show, Generic, NFData, Lift, ShowX)

data WritebackSrc = WbPc | WbAluRes | WbMem deriving (Eq, Show, Generic, NFData, Lift, ShowX)

data BranchType = B_EQ | B_NE | B_LT | B_GE | B_LTU | B_GEU deriving (Eq, Show, Generic, NFData, Lift, ShowX)

data MemoryRequest = MemWrite | MemRead deriving (Eq, Show, Generic, NFData, Lift, ShowX)

type RegisterIndex = Index 32
type MWord = BitVector 32

newtype Instruction = Instruction (BitVector 32) deriving (Generic, NFData, Lift, ShowX)

data InterInstr = InterInstr {
    opcode :: Opcode,
    funct3 :: BitVector 3,
    funct7 :: BitVector 7,
    immediate :: BitVector 32,
    rs1 :: RegisterIndex,
    rs2 :: RegisterIndex,
    rd :: RegisterIndex
    } deriving (Eq, Show, Generic, NFData, Lift, ShowX)

instance Default InterInstr where
    def = InterInstr INVALID zeroBits zeroBits zeroBits minBound minBound minBound

data AluCtrl = AluCtrl {
    aluOp :: AluOp,
    aluSrc1 :: AluSrc,
    aluSrc2 :: AluSrc
    } deriving (Eq, Show, Generic, NFData, Lift, ShowX)

instance Default AluCtrl where
    def = AluCtrl Add Zero Zero

data InstrDescr = InstrDescr {
    inter :: InterInstr,
    aluCtrl :: AluCtrl,
    jumpType :: Maybe JumpType,
    writebackSrc :: Maybe WritebackSrc,
    memoryRequest :: Maybe MemoryRequest,
    pc :: MWord
    } deriving (Eq, Show, Generic, NFData, Lift, ShowX)

instance Default InstrDescr where
    def = InstrDescr def def Nothing Nothing Nothing def

newtype RegisterFile = RegisterFile (Vec 32 MWord)

data FetchResults = FetchResults {
    instruction :: Instruction,
    fetchedPc :: MWord
    } deriving (Generic, NFData, Lift, ShowX)

--data DecodeResults = DecodeResults {}

data ExecuteResults = ExecuteResults {
    aluRes :: MWord,
    doJump :: Bit
    } deriving (Eq, Show, Generic, NFData, Lift, ShowX)
instance Default ExecuteResults where
    def = ExecuteResults zeroBits 0

data ForwardRequest = ForwardRequest {
    req_rs1 :: RegisterIndex,
    req_rs2 :: RegisterIndex
    } deriving (Eq, Show, Generic, NFData, Lift, ShowX)
data ForwardResponse = ForwardResponse {
    vrs1 :: MWord,
    vrs2 :: MWord
    } deriving (Eq, Show, Generic, NFData, Lift, ShowX)
