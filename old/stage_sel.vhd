----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 07/23/2018 03:51:07 PM
-- Design Name: 
-- Module Name: stage_sel - Behavioral
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
entity stage_sel is
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
        
        results: in alu_results;
        alu_res: out std_logic_vector(31 downto 0)
    );
end stage_sel;

architecture Behavioral of stage_sel is
    signal int_alu_res: std_logic_vector(31 downto 0);
    signal cmp_result: std_logic;
    
    signal ce, was_valid: std_logic;
    
    signal slt, ult: std_logic;
    signal shamt: integer range 0 to 31;
begin

    ce <= valid_in and not stalled;
   
    with idescr_in.alu_op select int_alu_res <=
        -- ADD
        results.res_add when ALU_ADD,
        -- SUB
        results.res_sub when ALU_SUB,
        -- XOR
        results.res_xor when ALU_XOR,
        -- OR
        results.res_or when ALU_OR,
        -- AND
        results.res_and when ALU_AND,
        -- SLT
        (0 => results.res_slt, 31 downto 1 => '0') when ALU_SLT,
        -- SLTU
        (0 => results.res_sltu, 31 downto 1 => '0') when ALU_SLTU,
        -- SLL
        results.res_sll when ALU_SLL,
        -- SRL
        results.res_srl when ALU_SRL,
        -- SRA
        results.res_sra when ALU_SRA,
        -- NONE
        (others => '0') when ALU_NONE;
        
    busy <= stalled;
    
    advance: process(clk, reset)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                valid_out <= '0';
                was_valid <= '0';
                alu_res <= (others => '0');
                pc_out <= (others => '0');
                idescr_out <= idescr_zero;
            else
                valid_out <= valid_in or was_valid;
                
                if ce = '0' then
                    if valid_in = '1' then
                        was_valid <= '1';
                    end if;
                else
                    if was_valid = '1' then
                        was_valid <= '0';
                    end if;
                    
                    idescr_out <= idescr_in;
                    pc_out <= pc_in;
                    
                    alu_res <= int_alu_res;
                end if;
            end if;
        end if;
    end process;

end Behavioral;
