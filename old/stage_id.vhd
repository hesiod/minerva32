----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 06/20/2018 09:13:25 PM
-- Design Name: 
-- Module Name: decode - Behavioral
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
use work.types.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity stage_id is
    Port (
        clk: in std_logic;
        reset: in std_logic;
        
        kill: in std_logic;
        busy: out std_logic;
        stalled: in std_logic;
        valid_in: in std_logic;
        valid_out: out std_logic;
        
        pc_in: in std_logic_vector(31 downto 0);
        pc_out: out std_logic_vector(31 downto 0);
        
        instruction: in std_logic_vector(31 downto 0);
        idescr_out: out instruction_data
    );
end stage_id;

architecture Behavioral of stage_id is
    signal reg_alu_op, imm_alu_op: ALUOP;
    signal int_br_type: BRANCHTYPE;
    signal pc_alu_op: ALUOP;
    signal itype: ITYPE;
    signal opcode: std_logic_vector(6 downto 0);
    
    signal immediate: std_logic_vector(31 downto 0);
    signal funct3: std_logic_vector(2 downto 0);
    signal funct7: std_logic_vector(6 downto 0);
    signal rs1, rs2: std_logic_vector(4 downto 0);
    signal rd: std_logic_vector(4 downto 0);
    
    signal ce: std_logic;
    
    signal imm_shift: ALUOP;
begin
    opcode <= instruction(6 downto 0);
    
    ce <= valid_in and not stalled;
    busy <= stalled;
    
    with funct7(5) & funct3 select reg_alu_op <=
        ALU_ADD  when "0000",
        ALU_SUB  when "1000",
        ALU_SLL  when "0001",
        ALU_SLT  when "0010",
        ALU_SLTU when "0011",
        ALU_XOR  when "0100",
        ALU_OR   when "0110",
        ALU_AND  when "0111",
        ALU_SRL  when "0101",
        ALU_SRA  when "1101",
        ALU_NONE when others;
        
    imm_shift <= ALU_SRA when funct7(5) = '1' else ALU_SRL;
    with funct3 select imm_alu_op <=
        ALU_ADD  when "000",
        ALU_SLL  when "001",
        ALU_SLT  when "010",
        ALU_SLTU when "011",
        ALU_XOR  when "100",
        imm_shift when "101",
        ALU_OR   when "110",
        ALU_AND  when "111";
        
    with funct3 select int_br_type <=
        BR_EQ when "000",
        BR_NE when "001",
        BR_LT when "100",
        BR_GE when "101",
        BR_LTU when "110",
        BR_GEU when "111",
        BR_NONE when others;
        
    with opcode(6 downto 2) select itype <=
        -- LOAD       
        I when LOAD,
        -- STORE
        S when STORE,
        -- LUI
        U when LUI,
        -- AUIPC
        U when AUIPC,
        -- JAL
        J when JAL,
        -- JALR
        I when JALR,
        -- BRANCH
        B when BRANCH,
        -- IARITH
        I when IARITH,
        -- ARITH
        R when ARITH,
        Z when others;
        
    decode_type: process(clk, reset, kill, valid_in)
    begin
        if rising_edge(clk) then
            if reset = '1' or kill = '1' or valid_in = '0' then
                valid_out <= '0';
                pc_out <= (others => '0');
                --busy <= '0';
                idescr_out.alu_op <= ALU_NONE;
                idescr_out.alu_src_1 <= ALU_ZERO;
                idescr_out.alu_src_2 <= ALU_ZERO;
                idescr_out.br_type <= BR_NONE;
                idescr_out.wb <= '0';
                idescr_out.wb_src <= WB_NONE;
                idescr_out.jump_type <= J_NONE;
                idescr_out.mem_request <= '0';
                idescr_out.mem_write <= '0';
                idescr_out.immediate <= (others => '0');
                idescr_out.rs1 <= (others => '0');
                idescr_out.rs2 <= (others => '0');
                idescr_out.funct3 <= (others => '0');
                idescr_out.funct7 <= (others => '0');
                idescr_out.rd <= (others => '0');
            else
                valid_out <= valid_in and not stalled;
                --busy <= stalled;
                pc_out <= pc_in;
                                
                if ce = '1' then
                    idescr_out.alu_op <= ALU_NONE;
                    idescr_out.alu_src_1 <= ALU_ZERO;
                    idescr_out.alu_src_2 <= ALU_ZERO;
                    idescr_out.br_type <= BR_NONE;
                    idescr_out.wb <= '0';
                    idescr_out.wb_src <= WB_NONE;
                    idescr_out.jump_type <= J_NONE;
                    idescr_out.mem_request <= '0';
                    idescr_out.mem_write <= '0';
                    idescr_out.immediate <= immediate;
                    idescr_out.rs1 <= rs1;
                    idescr_out.rs2 <= rs2;
                    idescr_out.funct3 <= funct3;
                    idescr_out.funct7 <= funct7;
                    idescr_out.rd <= rd;
                    case opcode(6 downto 2) is
                        when LOAD =>
                            --itype <= I;
                            idescr_out.wb <= '1';
                            idescr_out.wb_src <= WB_MEM;
                            idescr_out.alu_op <= ALU_ADD;
                            idescr_out.alu_src_1 <= ALU_RS;
                            idescr_out.alu_src_2 <= ALU_IMM12;
                            idescr_out.mem_request <= '1';
                        when STORE =>
                            --itype <= S;
                            idescr_out.alu_op <= ALU_ADD;
                            idescr_out.alu_src_1 <= ALU_RS;
                            idescr_out.alu_src_2 <= ALU_IMM12;
                            idescr_out.mem_request <= '1';
                            idescr_out.mem_write <= '1';
                        when LUI =>
                            --itype <= U;
                            idescr_out.alu_op <= ALU_ADD;
                            idescr_out.alu_src_1 <= ALU_ZERO;
                            idescr_out.alu_src_2 <= ALU_IMM20;
                            idescr_out.wb <= '1';
                            idescr_out.wb_src <= WB_ALU_RES;
                        when AUIPC =>
                            --itype <= U;
                            idescr_out.alu_op <= ALU_ADD;
                            idescr_out.alu_src_1 <= ALU_PC;
                            idescr_out.alu_src_2 <= ALU_IMM20;
                            idescr_out.wb <= '1';
                            idescr_out.wb_src <= WB_ALU_RES;
                        when JAL =>
                            --itype <= J;
                            idescr_out.alu_op <= ALU_ADD;
                            idescr_out.alu_src_1 <= ALU_PC;
                            idescr_out.alu_src_2 <= ALU_OFF_IMM20;
                            idescr_out.wb <= '1';
                            idescr_out.wb_src <= WB_PC;
                            idescr_out.jump_type <= J_JUMP;
                        when JALR =>
                            --itype <= I;
                            idescr_out.alu_op <= ALU_ADD;
                            idescr_out.alu_src_1 <= ALU_RS;
                            idescr_out.alu_src_2 <= ALU_IMM12;
                            idescr_out.wb <= '1';
                            idescr_out.wb_src <= WB_PC;
                            idescr_out.jump_type <= J_JUMP;
                        when BRANCH =>
                            --itype <= B;
                            idescr_out.alu_op <= ALU_ADD;
                            idescr_out.alu_src_1 <= ALU_PC;
                            idescr_out.alu_src_2 <= ALU_OFF_IMM12;
                            idescr_out.br_type <= int_br_type;
                            idescr_out.jump_type <= J_BRANCH;
                        when IARITH =>
                            --itype <= I;
                            idescr_out.wb <= '1';
                            idescr_out.alu_op <= imm_alu_op;
                            idescr_out.alu_src_1 <= ALU_RS;
                            idescr_out.alu_src_2 <= ALU_IMM12;
                            idescr_out.wb <= '1';
                            idescr_out.wb_src <= WB_ALU_RES;
                        when ARITH =>
                            --itype <= R;
                            idescr_out.wb <= '1';
                            idescr_out.alu_op <= reg_alu_op;
                            idescr_out.alu_src_1 <= ALU_RS;
                            idescr_out.alu_src_2 <= ALU_RS;
                            idescr_out.wb <= '1';
                            idescr_out.wb_src <= WB_ALU_RES;
                        when others =>
                            --itype <= Z;
                            valid_out <= '0';
                    end case;
                end if; 
            end if;
        end if;
    end process;
    
    decode: process(itype, instruction)
    begin
        case itype is
            when Z =>
                immediate <= (others => '0');
                rs1 <= (others => '0');
                rs2 <= (others => '0');
                funct3 <= (others => '0');
                funct7 <= (others => '0');
                rd <= (others => '0');
            when I =>
                immediate(11 downto 0) <= instruction(31 downto 20);
                immediate(31 downto 12) <= (others => '0');
                rs1 <= instruction(19 downto 15);
                rs2 <= (others => '0');
                funct3 <= instruction(14 downto 12);
                funct7 <= (others => '0');
                rd <= instruction(11 downto 7);
            when R =>
                immediate <= (others => '0');
                rs1 <= instruction(19 downto 15);
                rs2 <= instruction(24 downto 20);
                funct3 <= instruction(14 downto 12);
                funct7 <= instruction(31 downto 25);
                rd <= instruction(11 downto 7);
            when S =>
                immediate(11 downto 5) <= instruction(31 downto 25);
                immediate(4 downto 0) <= instruction(11 downto 7);
                immediate(31 downto 12) <= (others => '0');
                rs1 <= instruction(19 downto 15);
                rs2 <= instruction(24 downto 20);
                funct3 <= instruction(14 downto 12);
                funct7 <= (others => '0');
                rd <= instruction(11 downto 7);
            when B =>
                immediate(12) <= instruction(31);
                immediate(10 downto 5) <= instruction(30 downto 25);
                immediate(4 downto 1) <= instruction(11 downto 8);
                immediate(11) <= instruction(7);
                immediate(31 downto 13) <= (others => '0');
                immediate(0) <= '0';
                rs1 <= instruction(19 downto 15);
                rs2 <= instruction(24 downto 20);
                funct3 <= instruction(14 downto 12);
                funct7 <= (others => '0');
                rd <= instruction(11 downto 7);
            when U =>
                immediate(31 downto 12) <= instruction(31 downto 12);
                immediate(11 downto 0) <= (others => '0');
                rs1 <= (others => '0');
                rs2 <= (others => '0');
                funct3 <= (others => '0');
                funct7 <= (others => '0');
                rd <= instruction(11 downto 7);
            when J =>
                immediate(20) <= instruction(31);
                immediate(10 downto 1) <= instruction(30 downto 21);
                immediate(11) <= instruction(20);
                immediate(19 downto 12) <= instruction(19 downto 12);
                immediate(0) <= '0';
                immediate(31 downto 21) <= (others => '0');
                rs1 <= (others => '0');
                rs2 <= (others => '0');
                funct3 <= (others => '0');
                funct7 <= (others => '0');
                rd <= instruction(11 downto 7);
        end case;
    end process;

end Behavioral;
