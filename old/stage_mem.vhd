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

entity stage_mem is
    Port (        
        clk, reset: in std_logic;
        
        busy: out std_logic;
        stalled: in std_logic;
        valid_in: in std_logic;
        valid_out: out std_logic;
        
        pc_in: in std_logic_vector(31 downto 0);
        pc_out: out std_logic_vector(31 downto 0);
        
        idescr_in: in instruction_data;
        idescr_out: out instruction_data := idescr_zero;
        
        alu_res: in std_logic_vector(31 downto 0);
        alu_res_fwd: out std_logic_vector(31 downto 0);
        mem_res: out std_logic_vector(31 downto 0);
        
        daddr: out std_logic_vector(31 downto 0);
        dwrite: out std_logic;
        dwwidth: out std_logic_vector(2 downto 0);
        drequest: out std_logic;
        drvalid: in std_logic;
        dwready: in std_logic;
        drwait: in std_logic;
        dwdata: out std_logic_vector(31 downto 0);
        drdata: in std_logic_vector(31 downto 0);
        
        do_req_mrs2: out std_logic;
        req_mrs2: out std_logic_vector(4 downto 0) := "00000";
        vmrs2: in std_logic_vector(31 downto 0)
    );
end stage_mem;


architecture Behavioral of stage_mem is
    signal ce, was_valid: std_logic;
    signal pending: std_logic;
    
    type mem_state is (MNone, MRead, MWrite, MWaitRead, MWaitWrite);
    signal mstate, next_mstate: mem_state;
begin
    
    ce <= not stalled;
    
    busy <= stalled or pending;
    
--    pending <= idescr_in.mem_request and
--               not ((not idescr_in.mem_write and drvalid)
--                    or (idescr_in.mem_write and dwready))));
--    busy <= stalled or pending;
    
    delta: process(mstate, idescr_in, drvalid, dwready, drwait)
    begin
        case mstate is
            when MNone =>
                if idescr_in.mem_request = '1' then
                    if idescr_in.mem_write = '1' then
                        next_mstate <= MWrite;
                    else
                        next_mstate <= MRead;
                    end if;
                else
                    next_mstate <= MNone;
                end if;
            when MRead =>
                if drwait = '1' then
                    next_mstate <= MWaitRead;
                else
                    next_mstate <= MRead;
                end if;
            when MWrite =>
                next_mstate <= MWaitWrite;
            when MWaitRead =>
                if drvalid = '1' then
                    next_mstate <= MNone;
                else
                    next_mstate <= MWaitRead;
                end if;
            when MWaitWrite =>
                if dwready = '1' then
                    next_mstate <= MNone;
                else
                    next_mstate <= MWaitWrite;
                end if;
        end case;
    end process;
    
    lambda: process(mstate, idescr_in, alu_res, vmrs2)
    begin
        case mstate is
            when MNone =>
                do_req_mrs2 <= '0';
                req_mrs2 <= "00000";
                daddr <= (others => '0');
                dwdata <= (others => '0'); 
                dwwidth <= (others => '0');
                drequest <= '0';
                dwrite <= '0';
            when MRead =>
                do_req_mrs2 <= '0';
                req_mrs2 <= "00000";
                daddr <= alu_res;
                dwdata <= (others => '0');
                dwwidth <= (others => '0');
                drequest <= '1';
                dwrite <= '0';
            when MWrite =>
                do_req_mrs2 <= '1';
                req_mrs2 <= idescr_in.rs2;
                daddr <= alu_res;
                dwdata <= vmrs2; 
                dwwidth <= idescr_in.funct3;
                drequest <= '1';
                dwrite <= '1';
            when MWaitRead =>
                do_req_mrs2 <= '0';
                req_mrs2 <= "00000";
                daddr <= (others => '0');
                dwdata <= (others => '0'); 
                dwwidth <= (others => '0');
                drequest <= '0';
                dwrite <= '0';
            when MWaitWrite =>
                do_req_mrs2 <= '0';
                req_mrs2 <= "00000";
                daddr <= (others => '0');
                dwdata <= (others => '0'); 
                dwwidth <= (others => '0');
                drequest <= '0';
                dwrite <= '0';
        end case;
    end process;
    
    advance: process(clk, reset)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                mstate <= MNone;
                valid_out <= '0';
                was_valid <= '0';
                alu_res_fwd <= X"DEADC0DE";
                mem_res <= X"DEADC0DE";
                pc_out <= (others => '0');
                idescr_out <= idescr_zero;
                pending <= '0';
            else
                valid_out <= valid_in and not stalled and not pending;
                
                
                if valid_in = '1' or mstate /= MNone then
                    mstate <= next_mstate;
                end if;
                if (valid_in = '1' and next_mstate /= MNone) or (mstate /= MNone and next_mstate /= MNone) then
                    pending <= '1';
                else
                    pending <= '0';
                end if;
                
                if ce = '0' then
                    if (valid_in = '1' and next_mstate = MNone) then
                        was_valid <= '1';
                    end if;
                else
                    if was_valid = '1' then
                        was_valid <= '0';
                    end if;
                    if was_valid = '1' or (valid_in = '1' and next_mstate = MNone) then
                        -- Passthrough
                        valid_out <= '1';
                                    
                        idescr_out <= idescr_in;
                        pc_out <= pc_in;    
                        
                        alu_res_fwd <= alu_res;
                        mem_res <= (others => '0');
                    elsif (mstate = MWaitRead and next_mstate = MNone) then
                        -- Finishing Read
                        valid_out <= '1';
                
                        idescr_out <= idescr_in;
                        pc_out <= pc_in;    
                        
                        alu_res_fwd <= alu_res;
                        mem_res <= drdata;
                    elsif (mstate = MWaitWrite and next_mstate = MNone) then
                        -- Finishing Write
                        valid_out <= '1';
                                    
                        idescr_out <= idescr_in;
                        pc_out <= pc_in;    
                        
                        alu_res_fwd <= alu_res;
                        mem_res <= (others => '0');
                    else
                        valid_out <= '0';
                                                        
                        idescr_out <= idescr_in;
                        pc_out <= pc_in;
                        
                        alu_res_fwd <= (others => '0');
                        mem_res <= (others => '0');
                    end if;
                end if;
            end if;
        end if;
    end process;

end Behavioral;
