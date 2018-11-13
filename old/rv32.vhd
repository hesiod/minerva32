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

entity rv32 is
	generic (
		-- Users to add parameters here
        RESET_VECTOR: std_logic_vector(31 downto 0) := x"C0000000";
		-- User parameters ends
		-- Do not modify the parameters beyond this line

		-- Parameters of Axi Master Bus Interface M00_AXI
		C_M00_AXI_START_DATA_VALUE	: std_logic_vector	:= x"AA000000";
		C_M00_AXI_TARGET_SLAVE_BASE_ADDR	: std_logic_vector	:= x"40000000";
		C_M00_AXI_ADDR_WIDTH	: integer	:= 32;
		C_M00_AXI_DATA_WIDTH	: integer	:= 32;
		C_M00_AXI_TRANSACTIONS_NUM	: integer	:= 4
	);
    Port (
        cpu_reset: in std_logic;
                
        -- Ports of Axi Master Bus Interface M00_AXI
        m00_axi_aclk    : in std_logic;
        m00_axi_aresetn    : in std_logic;
        -- WRITE ADDR
        m00_axi_awaddr    : out std_logic_vector(C_M00_AXI_ADDR_WIDTH-1 downto 0);
        m00_axi_awprot    : out std_logic_vector(2 downto 0);
        m00_axi_awvalid    : out std_logic;
        m00_axi_awready    : in std_logic;
        -- WRITE DATA
        m00_axi_wdata    : out std_logic_vector(C_M00_AXI_DATA_WIDTH-1 downto 0);
        m00_axi_wstrb    : out std_logic_vector(C_M00_AXI_DATA_WIDTH/8-1 downto 0);
        m00_axi_wvalid    : out std_logic;
        m00_axi_wready    : in std_logic;
        -- WRITE RESPONSE
        m00_axi_bresp    : in std_logic_vector(1 downto 0);
        m00_axi_bvalid    : in std_logic;
        m00_axi_bready    : out std_logic;
        -- READ ADDR
        m00_axi_araddr    : out std_logic_vector(C_M00_AXI_ADDR_WIDTH-1 downto 0);
        m00_axi_arprot    : out std_logic_vector(2 downto 0);
        m00_axi_arvalid    : out std_logic;
        m00_axi_arready    : in std_logic;
        -- READ DATA
        m00_axi_rdata    : in std_logic_vector(C_M00_AXI_DATA_WIDTH-1 downto 0);
        m00_axi_rresp    : in std_logic_vector(1 downto 0);
        m00_axi_rvalid    : in std_logic;
        m00_axi_rready    : out std_logic
    );
end rv32;


architecture Behavioral of rv32 is
    component cache is
        Port (
            iaddr: in std_logic_vector(31 downto 0);
            irequest: in std_logic;
            ivalid: out std_logic;
            iwait: out std_logic;
            idata: out std_logic_vector(31 downto 0);
            
            daddr: in std_logic_vector(31 downto 0);
            dwrite: in std_logic;
            dwwidth: in std_logic_vector(2 downto 0);
            drequest: in std_logic;
            drvalid: out std_logic;
            dwready: out std_logic;
            drwait: out std_logic;
            dwdata: in std_logic_vector(31 downto 0);
            drdata: out std_logic_vector(31 downto 0);
            
            -- Ports of Axi Master Bus Interface M00_AXI
            m00_axi_aclk    : in std_logic;
            m00_axi_aresetn    : in std_logic;
            -- WRITE ADDR
            m00_axi_awaddr    : out std_logic_vector(31 downto 0);
            m00_axi_awprot    : out std_logic_vector(2 downto 0);
            m00_axi_awvalid    : out std_logic;
            m00_axi_awready    : in std_logic;
            -- WRITE DATA
            m00_axi_wdata    : out std_logic_vector(31 downto 0);
            m00_axi_wstrb    : out std_logic_vector(3 downto 0);
            m00_axi_wvalid    : out std_logic;
            m00_axi_wready    : in std_logic;
            -- WRITE RESPONSE
            m00_axi_bresp    : in std_logic_vector(1 downto 0);
            m00_axi_bvalid    : in std_logic;
            m00_axi_bready    : out std_logic;
            -- READ ADDR
            m00_axi_araddr    : out std_logic_vector(31 downto 0);
            m00_axi_arprot    : out std_logic_vector(2 downto 0);
            m00_axi_arvalid    : out std_logic;
            m00_axi_arready    : in std_logic;
            -- READ DATA
            m00_axi_rdata    : in std_logic_vector(31 downto 0);
            m00_axi_rresp    : in std_logic_vector(1 downto 0);
            m00_axi_rvalid    : in std_logic;
            m00_axi_rready    : out std_logic
        );
    end component;
    
    component pc_feeder is
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
    end component;
    
    component stage_if is
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
    end component;
    
    component stage_id is
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
    end component;
    
    component stage_of is
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
    end component;
    
    component stage_ex is
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
    end component;
    
    component stage_sel is
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
    end component;
    
    component stage_mem is
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
            req_mrs2: out std_logic_vector(4 downto 0);
            vmrs2: in std_logic_vector(31 downto 0)
        );
    end component;
    
    component stage_wb is
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
    end component;
        
    signal do_jump: std_logic;
    signal feeder_pc: std_logic_vector(31 downto 0);
    signal if_pc, id_pc, of_pc, ex_pc, sel_pc, mem_pc, wb_pc: std_logic_vector(31 downto 0);
    signal if_valid, id_valid, of_valid, ex_valid, sel_valid, mem_valid, wb_valid: std_logic;
    signal if_stalled, id_stalled, of_stalled, ex_stalled, sel_stalled, mem_stalled, wb_stalled: std_logic;
    signal id_idescr, ex_idescr, of_idescr, sel_idescr, mem_idescr, wb_idescr: instruction_data;
    
    signal iaddr: std_logic_vector(31 downto 0);
    signal irequest: std_logic;
    signal ivalid: std_logic;
    signal iwait: std_logic;
    signal idata: std_logic_vector(31 downto 0);
    
    signal daddr: std_logic_vector(31 downto 0);
    signal dwrite: std_logic;
    signal dwwidth: std_logic_vector(2 downto 0);
    signal drequest: std_logic;
    signal drvalid: std_logic;
    signal dwready: std_logic;
    signal drwait: std_logic;
    signal dwdata: std_logic_vector(31 downto 0);
    signal drdata: std_logic_vector(31 downto 0);
        
    signal if_instruction: std_logic_vector(31 downto 0);
    
    signal vres, vres_fwd, mres: std_logic_vector(31 downto 0);
    signal req_rs1, req_rs2: std_logic_vector(4 downto 0);
    signal vrs1, vrs2: std_logic_vector(31 downto 0);
    signal op1, op2, cop1, cop2: std_logic_vector(31 downto 0);
    signal results: alu_results;
    
    signal do_req_mrs2: std_logic;
    signal req_mrs2: std_logic_vector(4 downto 0);
    signal vmrs2: std_logic_vector(31 downto 0);
    
    --signal m00_axi_aresetn, m00_axi_aclk: std_logic;
    signal reset: std_logic;
begin
    reset <= not m00_axi_aresetn;
    --m00_axi_aclk <= clk;
    --m00_axi_aresetn <= not reset;

    cache_0: cache port map (
        m00_axi_aclk => m00_axi_aclk,
        m00_axi_aresetn => m00_axi_aresetn,
        iaddr => iaddr,
        irequest => irequest,
        ivalid => ivalid,
        iwait => iwait,
        idata => idata,
        daddr => daddr,
        dwrite => dwrite,
        dwwidth => dwwidth,
        drequest => drequest,
        drvalid => drvalid,
        dwready => dwready,
        drwait => drwait,
        dwdata => dwdata,
        drdata => drdata,
        m00_axi_awaddr  => m00_axi_awaddr ,
        m00_axi_awprot  => m00_axi_awprot ,
        m00_axi_awvalid => m00_axi_awvalid,
        m00_axi_awready => m00_axi_awready,
        m00_axi_wdata   => m00_axi_wdata  ,
        m00_axi_wstrb   => m00_axi_wstrb  ,
        m00_axi_wvalid  => m00_axi_wvalid ,
        m00_axi_wready  => m00_axi_wready ,
        m00_axi_bresp   => m00_axi_bresp  ,
        m00_axi_bvalid  => m00_axi_bvalid ,
        m00_axi_bready  => m00_axi_bready ,
        m00_axi_araddr => m00_axi_araddr,
        m00_axi_arprot => m00_axi_arprot,
        m00_axi_arready => m00_axi_arready,
        m00_axi_arvalid => m00_axi_arvalid,
        m00_axi_rdata => m00_axi_rdata,
        m00_axi_rresp => m00_axi_rresp,
        m00_axi_rvalid => m00_axi_rvalid,
        m00_axi_rready => m00_axi_rready
    );
    pc_feeder_0: pc_feeder generic map (
        RESET_VECTOR => RESET_VECTOR
    ) port map (
        clk => m00_axi_aclk, reset => cpu_reset,
        stalled => if_stalled,
        do_jump => do_jump,
        target_pc => results.res_add,
        pc => feeder_pc
    );
    stage_if_0: stage_if port map (
        clk => m00_axi_aclk, reset => reset,
        kill => do_jump,
        instruction => if_instruction,
        pc_in => feeder_pc,
        pc_out => if_pc,
        valid_out => if_valid,
        busy => if_stalled,
        stalled => id_stalled,
        iaddr => iaddr,
        ivalid => ivalid,
        iwait => iwait,
        irequest => irequest,
        idata => idata
    );
    stage_id_0: stage_id port map (
        clk => m00_axi_aclk, reset => reset,
        kill => do_jump,
        instruction => if_instruction,
        idescr_out => id_idescr,
        pc_in => if_pc,
        pc_out => id_pc,
        valid_in => if_valid,
        valid_out => id_valid,
        busy => id_stalled,
        stalled => ex_stalled
    );
    stage_of_0: stage_of port map (
        clk => m00_axi_aclk, reset => reset,
        idescr_in => id_idescr,
        idescr_out => of_idescr,
        pc_in => id_pc,
        pc_out => of_pc,
        valid_in => id_valid,
        valid_out => of_valid,
        busy => of_stalled,
        stalled => ex_stalled, 
        op1 => op1, op2 => op2,
        cop1 => cop1, cop2 => cop2,
        req_rs1 => req_rs1, req_rs2 => req_rs2,
        vrs1 => vrs1, vrs2 => vrs2
    );
    stage_ex_0: stage_ex port map (
        clk => m00_axi_aclk, reset => reset,
        idescr_in => of_idescr,
        idescr_out => ex_idescr,
        pc_in => of_pc,
        pc_out => ex_pc,
        valid_in => of_valid,
        valid_out => ex_valid,
        busy => ex_stalled,
        stalled => mem_stalled, 
        results => results,
        do_jump => do_jump,
        op1 => op1, op2 => op2,
        cop1 => cop1, cop2 => cop2
    );
    stage_sel_0: stage_sel port map (
        clk => m00_axi_aclk, reset => reset,
        idescr_in => ex_idescr,
        idescr_out => sel_idescr,
        pc_in => ex_pc,
        pc_out => sel_pc,
        valid_in => ex_valid,
        valid_out => sel_valid,
        busy => sel_stalled,
        stalled => mem_stalled, 
        alu_res => vres,
        results => results
    );
    stage_mem_0: stage_mem port map (
        clk => m00_axi_aclk, reset => reset,
        idescr_in => ex_idescr,
        idescr_out => mem_idescr,
        pc_in => ex_pc,
        pc_out => mem_pc,
        valid_in => ex_valid,
        valid_out => mem_valid,
        busy => mem_stalled,
        stalled => wb_stalled, 
        alu_res => vres,
        alu_res_fwd => vres_fwd,
        mem_res => mres,
        daddr => daddr,
        dwrite => dwrite,
        dwwidth => dwwidth,
        drequest => drequest,
        drvalid => drvalid,
        dwready => dwready,
        drwait => drwait,
        dwdata => dwdata,
        drdata => drdata,
        do_req_mrs2 => do_req_mrs2,
        req_mrs2 => req_mrs2,
        vmrs2 => vmrs2
    );
    stage_wb_0: stage_wb port map (
        clk => m00_axi_aclk, reset => reset,
        idescr_in => mem_idescr,
        pc_in => mem_pc,
        valid_in => mem_valid,
        busy => wb_stalled, 
        alu_res => vres_fwd,
        mem_res => mres,
        do_req_rs => id_valid,
        req_rs1 => req_rs1, req_rs2 => req_rs2,
        vrs1 => vrs1, vrs2 => vrs2,
        do_req_mrs2 => do_req_mrs2,
        req_mrs2 => req_mrs2,
        vmrs2 => vmrs2
    );
        
--    pc_mux_0: pc_mux port map (
--        clk => m00_axi_aclk,
--        pc => if_pc,
--        vres => vres,
--        pc_src => id_idescr.pc_src,
--        set_pc => id_pc 
--    );
        
--    advance: process(m00_axi_aclk, m00_axi_aresetn)
--    begin
--        if rising_edge(m00_axi_aclk) then
--            if m00_axi_aresetn = '0' then
--                kill_if <= '0';
--                kill_id <= '0';
--            else
--            end if;
--        end if;
--    end process;
end Behavioral;
