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

entity stage_wb is
    Port (        
        clk, reset: in std_logic;
        
        busy: out std_logic;
        valid_in: in std_logic;
        
        pc_in: in std_logic_vector(31 downto 0);

        idescr_in: in instruction_data;
        
        alu_res: in std_logic_vector(31 downto 0);
        mem_res: in std_logic_vector(31 downto 0);
        
        do_req_rs: in std_logic;
        req_rs1, req_rs2: in std_logic_vector(4 downto 0);
        vrs1, vrs2: out std_logic_vector(31 downto 0);
                                
        do_req_mrs2: in std_logic;
        req_mrs2: in std_logic_vector(4 downto 0);
        vmrs2: out std_logic_vector(31 downto 0)
    );
end stage_wb;


architecture Behavioral of stage_wb is
    
    component regfile is
        Port (
            clk: in std_logic;
            reset: in std_logic;
            rwen: in std_logic;
            rs1: in std_logic_vector (4 downto 0);
            rs2: in std_logic_vector (4 downto 0);
            rd: in std_logic_vector (4 downto 0);
            rrs1: out std_logic_vector (31 downto 0);
            rrs2: out std_logic_vector (31 downto 0);
            wrd: in std_logic_vector (31 downto 0);
            req_mrs2: in std_logic_vector(4 downto 0);
            vmrs2: out std_logic_vector(31 downto 0)
        );
    end component;
    component wb_mux is
        Port (
            clk: in STD_LOGIC;
            
            wb_src: in WBSRC;
            wrwidth: in std_logic_vector (2 downto 0);
            
            pc: in std_logic_vector(31 downto 0);
            vres: in std_logic_vector(31 downto 0);
            mres: in std_logic_vector(31 downto 0);
            
            wb_data: out std_logic_vector(31 downto 0)
        );
    end component;
    
    signal wb_data: std_logic_vector(31 downto 0);
    
    signal ce: std_logic;
    signal hazard_wd, hazard_rs, hazard_mrs, data_hazard: std_logic;
    
    signal sel_rs1, sel_rs2, sel_mrs2: integer range 1 to 31;
    signal sel_rd: integer range 0 to 31;
    
    signal wb_data_mem: std_logic_vector(31 downto 0);
    type regfilet is array (31 downto 1) of std_logic_vector(31 downto 0);
    signal reg: regfilet;
begin
--    regfile_0: regfile port map (
--        clk => clk, reset => reset,
--        rs1 => req_rs1, rs2 => req_rs2,
--        rd => idescr_in.rd,
--        rrs1 => vrs1, rrs2 => vrs2,
--        req_mrs2 => req_mrs2, vmrs2 => vmrs2,
--        wrd => wb_data,
--        rwen => idescr_in.wb
--    );
--    wb_mux_0: wb_mux port map (
--        clk => clk,
--        wb_src => idescr_in.wb_src, wrwidth => idescr_in.funct3,
--        pc => pc_in, vres => alu_res, mres => mem_res,
--        wb_data => wb_data
--    );

     with idescr_in.funct3 select wb_data_mem <=
         -- LB
         std_logic_vector(resize(signed(mem_res(7 downto 0)), 32)) when "000",
         -- LH
         std_logic_vector(resize(signed(mem_res(15 downto 0)), 32)) when "001",
         -- LW
         mem_res when "010",
         -- LBU
         std_logic_vector(resize(unsigned(mem_res(7 downto 0)), 32)) when "100",
         -- LHU
         std_logic_vector(resize(unsigned(mem_res(15 downto 0)), 32)) when "101",
         -- 1011, 111
         mem_res when others;
 
     with idescr_in.wb_src select wb_data <=
        (others => '0') when WB_NONE,
        alu_res when WB_ALU_RES,
        std_logic_vector(unsigned(pc_in) + to_unsigned(4, 32)) when WB_PC,
        wb_data_mem when WB_MEM;
        
    hazard_rs <= '1' when idescr_in.rd = req_rs1 or idescr_in.rd = req_rs2 else '0';
    hazard_mrs <= '1' when idescr_in.rd = req_mrs2 else '0';
    hazard_wd <= '0' when idescr_in.rd = "00000" else '1';
    data_hazard <= hazard_wd and idescr_in.wb and ((do_req_rs and hazard_rs) or (do_req_mrs2 and hazard_mrs));
    
    ce <= valid_in;
    
    sel_rs1 <= to_integer(unsigned(req_rs1));
    sel_rs2 <= to_integer(unsigned(req_rs2));
    sel_mrs2 <= to_integer(unsigned(req_mrs2));
    sel_rd <= to_integer(unsigned(idescr_in.rd));
    
    with sel_rs1 select vrs1 <=
        (31 downto 0 => '0') when 0,
        reg(sel_rs1) when others;
    with sel_rs2 select vrs2 <=
        (31 downto 0 => '0') when 0,
        reg(sel_rs2) when others;
    with sel_mrs2 select vmrs2 <=
        (31 downto 0 => '0') when 0,
        reg(sel_mrs2) when others;
    
    advance: process(clk, reset)
    begin        
        if rising_edge(clk) then
            if reset = '1' then
                reg <= (others => (others => '0'));
                busy <= '0';
            else
                busy <= data_hazard;
                
                if ce = '1' and idescr_in.wb = '1' and sel_rd /= 0 then
                    reg(sel_rd) <= wb_data;
                end if;
            end if;
        end if;
    end process;

end Behavioral;
