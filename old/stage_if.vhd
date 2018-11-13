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

entity stage_if is
    Port (
        clk: in std_logic;
        reset: in std_logic;
        
        kill: in std_logic;
        busy: out std_logic;
        stalled: in std_logic;
        valid_out: out std_logic;
        
        pc_in: in std_logic_vector(31 downto 0);
        pc_out: out std_logic_vector(31 downto 0);
        
        instruction: out std_logic_vector(31 downto 0);
        
        iaddr: out std_logic_vector(31 downto 0);
        irequest: out std_logic;
        ivalid: in std_logic;
        iwait: in std_logic;
        idata: in std_logic_vector(31 downto 0)
    );
end stage_if;

architecture Behavioral of stage_if is
    signal ce: std_logic;
    signal discard, start, valid, was_valid: std_logic;
begin
    ce <= not stalled;
    
    --busy <= stalled or iwait;
    
    iaddr <= pc_in;
    
    valid_out <= valid;
    
    --irequest <= not (iwait or was_valid or stalled) and run;
    
    advance: process(clk, ce, reset)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                valid <= '0';
                was_valid <= '0';
                start <= '1';
                discard <= '0';
                irequest <= '0';
                busy <= '0';
                pc_out <= (others => '0');
                instruction <= (others => '0');
            elsif kill = '1' then
                valid <= '0';
                was_valid <= '0';
                start <= '1';
                discard <= '1';
                irequest <= '0';
                busy <= '1';
                pc_out <= (others => '0');
                instruction <= (others => '0');
            else
                if ivalid = '1' then
                    if discard = '1' then
                        discard <= '0';
                    else
                        instruction <= idata;
                        pc_out <= pc_in;
                    end if;
                end if;
                
                if ce = '1' then
                    if was_valid = '1' then
                        was_valid <= '0';
                        valid <= '1';
                        busy <= '0';
                    else
                        busy <= discard or not ivalid;
                        valid <= ivalid;
                    end if;
                    if valid = '1' then
                        irequest <= '1';
                    elsif start = '1' and discard = '0' then
                        irequest <= '1';
                        start <= '0';
                    else
                        irequest <= '0';
                    end if;
--                    if (was_valid = '1' or ivalid = '1') then
--                        pc_out <= pc_in;
--                    end if; 
                    
                else
                    valid <= '0';
                    if ivalid = '1' and discard = '0' then
                        was_valid <= '1';
                    end if;
                end if;
            end if;
        end if;
    end process;

end Behavioral;
