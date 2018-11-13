----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 07/11/2018 01:33:26 AM
-- Design Name: 
-- Module Name: pc_feeder - Behavioral
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

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity pc_feeder is
    generic (
        RESET_VECTOR: std_logic_vector(31 downto 0) := (others => '0')
    );
    port (
        clk, reset: in std_logic;
        
        stalled: in std_logic;
        
        do_jump: in std_logic;
        
        target_pc: in std_logic_vector(31 downto 0);
        pc: out std_logic_vector(31 downto 0)
    );
end pc_feeder;

architecture Behavioral of pc_feeder is
    signal current, predicted: std_logic_vector(31 downto 0); 
begin
    predicted <= std_logic_vector(unsigned(current) + to_unsigned(4, 32));
    pc <= current;
       
    advance: process(clk, reset)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                current <= RESET_VECTOR; --std_logic_vector(unsigned(RESET_VECTOR) - to_unsigned(4, 32));
            else
                if do_jump = '1' then
                    current <= target_pc;
                elsif stalled = '0' then
                    current <= predicted;
                end if;
            end if;
        end if;
    end process;

end Behavioral;
