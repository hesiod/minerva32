----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 07/19/2018 07:22:14 PM
-- Design Name: 
-- Module Name: stage_of - Behavioral
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

entity stage_of is
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
        
        req_rs1, req_rs2: out std_logic_vector(4 downto 0);
        vrs1, vrs2: in std_logic_vector(31 downto 0);
        
        op1, op2: out std_logic_vector(31 downto 0);
        cop1, cop2: out std_logic_vector(31 downto 0)
    );
end stage_of;


architecture Behavioral of stage_of is
    signal int_do_jump: std_logic;
    signal int_op1, int_op2: std_logic_vector(31 downto 0);
    
    signal ce, was_valid: std_logic;
begin

    ce <= valid_in and not stalled;
    
    with idescr_in.alu_src_1 select int_op1 <=
        vrs1 when ALU_RS,
        std_logic_vector(resize(signed(idescr_in.immediate(11 downto 0)), 32)) when ALU_IMM12,
        std_logic_vector(resize(signed(idescr_in.immediate(12 downto 0)), 32)) when ALU_OFF_IMM12,
        idescr_in.immediate(31 downto 12) & (11 downto 0 => '0') when ALU_IMM20,
        std_logic_vector(resize(signed(idescr_in.immediate(20 downto 0)), 32)) when ALU_OFF_IMM20,
        pc_in when ALU_PC,
        (others => '0') when ALU_ZERO;

    with idescr_in.alu_src_2 select int_op2 <=
        vrs2 when ALU_RS,
        std_logic_vector(resize(signed(idescr_in.immediate(11 downto 0)), 32)) when ALU_IMM12,
        std_logic_vector(resize(signed(idescr_in.immediate(12 downto 0)), 32)) when ALU_OFF_IMM12,
        idescr_in.immediate(31 downto 12) & (11 downto 0 => '0') when ALU_IMM20,
        std_logic_vector(resize(signed(idescr_in.immediate(20 downto 0)), 32)) when ALU_OFF_IMM20,
        pc_in when ALU_PC,
        (others => '0') when ALU_ZERO;
    
    
    req_rs1 <= idescr_in.rs1;
    req_rs2 <= idescr_in.rs2;
    
    busy <= stalled;
    
    advance: process(clk, reset)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                valid_out <= '0';
                was_valid <= '0';
                pc_out <= (others => '0');
                idescr_out <= idescr_zero;
                op1 <= (others => '0');
                op2 <= (others => '0');
                cop1 <= (others => '0');
                cop2 <= (others => '0');
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
                    
                    op1 <= int_op1;
                    op2 <= int_op2;
                    cop1 <= vrs1;
                    cop2 <= vrs2;
                end if;
            end if;
        end if;
    end process;

end Behavioral;
