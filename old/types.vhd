----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 06/20/2018 09:52:42 PM
-- Design Name: 
-- Module Name: types - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

package types is
    type ITYPE is ( R, I, S, B, U, J, Z );
    
    subtype INSTRUCTION is std_logic_vector(4 downto 0);
    constant LOAD: INSTRUCTION := "00000";
    constant STORE: INSTRUCTION := "01000";
    constant LUI: INSTRUCTION := "01101";
    constant AUIPC: INSTRUCTION := "00101";
    constant JAL: INSTRUCTION := "11011";
    constant JALR: INSTRUCTION := "11001";
    constant BRANCH: INSTRUCTION := "11000";
    constant IARITH: INSTRUCTION := "00100";
    constant ARITH: INSTRUCTION := "01100";
    
    subtype WIDTH is std_logic_vector(2 downto 0);
    constant BYTE: WIDTH := "000";
    constant HALF_WORD: WIDTH := "001";
    constant WORD: WIDTH := "010";
    
    type ALUOP is (
        ALU_NONE,
        ALU_ADD, ALU_SUB,
        ALU_SLL,
        ALU_SLT, ALU_SLTU,
        ALU_XOR, ALU_OR, ALU_AND,
        ALU_SRL, ALU_SRA
    );
    type ALUSRC is (
        ALU_ZERO, ALU_RS, ALU_IMM12, ALU_IMM20, ALU_OFF_IMM12, ALU_OFF_IMM20, ALU_PC
    );
    type JUMPTYPE is (
        J_NONE, J_JUMP, J_BRANCH
    );
    type WBSRC is (
        WB_NONE, WB_PC, WB_ALU_RES, WB_MEM
    );
    type BRANCHTYPE is ( 
        BR_NONE,
        BR_EQ, BR_NE, BR_LT, BR_GE, BR_LTU, BR_GEU
    );
    type alu_results is record
        res_add: std_logic_vector(31 downto 0);
        res_sub: std_logic_vector(31 downto 0);
        res_xor: std_logic_vector(31 downto 0);
        res_or: std_logic_vector(31 downto 0);
        res_and: std_logic_vector(31 downto 0);
        res_sll, res_srl, res_sra: std_logic_vector(31 downto 0);
        res_slt, res_sltu: std_logic; 
    end record alu_results;
    type instruction_data is record
        immediate: std_logic_vector(31 downto 0);
        funct3: std_logic_vector(2 downto 0);
        funct7: std_logic_vector(6 downto 0);
        rs1, rs2: std_logic_vector(4 downto 0);
        rd: std_logic_vector(4 downto 0);
        alu_op: ALUOP;
        alu_src_1, alu_src_2: ALUSRC;
        br_type: BRANCHTYPE;
        wb: std_logic;
        wb_src: WBSRC;
        
        jump_type: JUMPTYPE;
       
        mem_request, mem_write: std_logic;
    end record instruction_data;
    constant idescr_zero : instruction_data := (
        alu_op => ALU_NONE,
        alu_src_1 => ALU_ZERO, alu_src_2 => ALU_ZERO,
        br_type => BR_NONE,
        wb => '0',
        wb_src => WB_NONE,
        jump_type => J_NONE,
        mem_request => '0',
        mem_write => '0',
        others => (others => '0')
    );
end package types;
