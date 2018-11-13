----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 07/10/2018 07:34:04 PM
-- Design Name: 
-- Module Name: rv32 - Behavioral
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
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity stage_ex is
    Port (        
        clk, reset: in std_logic;
        
        busy: out std_logic;
        stalled: in std_logic;
        valid_in: in std_logic;
        valid_out: out std_logic;
        
        pc_in: in std_logic_vector(31 downto 0);
        pc_out: out std_logic_vector(31 downto 0);
        
        idescr_in: in instruction_data;
        idescr_out: out instruction_data;
        
        op1, op2: in std_logic_vector(31 downto 0);
        cop1, cop2: in std_logic_vector(31 downto 0);
        
        --alu_res: out std_logic_vector(31 downto 0);
        results: out alu_results;
        do_jump: out std_logic
    );
end stage_ex;


architecture Behavioral of stage_ex is
    signal int_alu_res: std_logic_vector(31 downto 0);
    signal int_do_jump: std_logic;
    signal cmp_result: std_logic;
    
    signal int_results: alu_results;
    
    signal ce, was_valid: std_logic;
    
    signal slt, ult: std_logic;
    signal shamt: integer range 0 to 31;
begin

    ce <= valid_in and not stalled;
    
    
    compute_arith: process(idescr_in, cop1, cop2) 
    begin
        case idescr_in.br_type is
            when BR_EQ =>
                if cop1 = cop2 then
                    cmp_result <= '1';
                else
                    cmp_result <= '0';
                end if;
            when BR_NE =>
                if cop1 = cop2 then
                    cmp_result <= '0';
                else
                    cmp_result <= '1';
                end if;
            when BR_LT =>
                if signed(cop1)   <  signed(cop2) then
                    cmp_result <= '1';
                else
                    cmp_result <= '0';
                end if;
            when BR_GE =>
                if signed(cop1)   >= signed(cop2) then
                    cmp_result <= '1';
                else
                    cmp_result <= '0';
                end if;
            when BR_LTU =>
                if unsigned(cop1) <  unsigned(cop2) then
                    cmp_result <= '1';
                else
                    cmp_result <= '0';
                end if;
            when BR_GEU =>
                if unsigned(cop1) >= unsigned(cop2) then
                    cmp_result <= '1';
                else
                    cmp_result <= '0';
                end if;
            when BR_NONE =>
                cmp_result <= '0';
        end case;
    end process;

    
--    with idescr_in.alu_op select int_alu_res <=
--        -- ADD
--        std_logic_vector(signed(op1) + signed(op2)) when ALU_ADD,
--        -- SUB
--        std_logic_vector(signed(op1) - signed(op2)) when ALU_SUB,
--        -- XOR
--        op1 xor op2 when ALU_XOR,
--        -- OR
--        op1 or op2 when ALU_OR,
--        -- AND
--        op1 and op2 when ALU_AND,
--        -- SLT
--        (0 => slt, 31 downto 1 => '0') when ALU_SLT,
--        -- SLTU
--        (0 => ult, 31 downto 1 => '0') when ALU_SLTU,
--        -- SLL
--        std_logic_vector(shift_left(unsigned(op1), shamt)) when ALU_SLL,
--        -- SRL
--        std_logic_vector(shift_right(unsigned(op1), shamt)) when ALU_SRL,
--        -- SRA
--        std_logic_vector(shift_right(signed(op1), shamt)) when ALU_SRA,
--        -- NONE
--        (others => '0') when ALU_NONE;
        
    int_results.res_add <= std_logic_vector(signed(op1) + signed(op2));
    int_results.res_sub <= std_logic_vector(signed(op1) - signed(op2));
    int_results.res_xor <= op1 xor op2;
    int_results.res_or <= op1 or op2;
    int_results.res_and <= op1 and op2;
    int_results.res_sll <= std_logic_vector(shift_left(unsigned(op1), shamt));
    int_results.res_srl <= std_logic_vector(shift_right(unsigned(op1), shamt));
    int_results.res_sra <= std_logic_vector(shift_right(signed(op1), shamt));
    int_results.res_slt <= slt;
    int_results.res_sltu <= ult;
    
    with idescr_in.jump_type select int_do_jump <=
        '0' when J_NONE,
        '1' when J_JUMP,
        cmp_result when J_BRANCH;
        
    shamt <= to_integer(unsigned(op2(4 downto 0)));
    slt <= '1' when signed(op1) < signed(op2) else '0';
    ult <= '1' when unsigned(op1) < unsigned(op2) else '0';

    busy <= stalled;
    
    advance: process(clk, reset)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                valid_out <= '0';
                was_valid <= '0';
                --alu_res <= (others => '0');
                do_jump <= '0';
                pc_out <= (others => '0');
                idescr_out <= idescr_zero;
            else
                valid_out <= valid_in or was_valid;
                
                if ce = '0' then
                    if valid_in = '1' then
                        was_valid <= '1';
                    end if;
                    do_jump <= '0';
                else
                    if was_valid = '1' then
                        was_valid <= '0';
                    end if;
                    
                    idescr_out <= idescr_in;
                    pc_out <= pc_in;
                    
                    --alu_res <= int_alu_res;
                    do_jump <= int_do_jump;
                    
                    results <= int_results;
                end if;
            end if;
        end if;
    end process;

end Behavioral;
