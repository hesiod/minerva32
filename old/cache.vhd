----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 07/04/2018 02:01:49 PM
-- Design Name: 
-- Module Name: cache - Behavioral
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

entity cache is
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
        drwait: out std_logic;
--        dwwait: out std_logic;
        drvalid: out std_logic;
        dwready: out std_logic;
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
end cache;

architecture Behavioral of cache is
    --signal reading, writing: std_logic;
    type read_state is (RNone, RData, RInstruction);
    signal readst, last_read: read_state;
    type bus_state is (BReset, BHold, BNone, BAddrValid, BWaitDataValid);
    signal busst: bus_state;
    
    type write_state is (WReset, WHold, WNone, WWaitReady, WWaitAWReady, WWaitWReady, WFinished);
    signal writest, next_writest: write_state;
    
    signal next_busst: bus_state;
    signal next_readst: read_state;
    
    signal width: WIDTH;
    
    signal store_r_addr, store_r_data: std_logic;
    signal store_w_addr, store_w_data: std_logic;
begin
    m00_axi_awprot <= "100";
    
    writest_delta: process(writest, drequest, dwrite, m00_axi_wready, m00_axi_awready, m00_axi_bvalid)
    begin
        case writest is
            when WReset =>
                next_writest <= WNone;
            when WHold =>
                if drequest = '1' and dwrite = '1' then
                    next_writest <= WWaitReady;
                else
                    next_writest <= WNone;
                end if;
            when WNone =>
                if drequest = '1' and dwrite = '1' then
                    next_writest <= WWaitReady;
                else
                    next_writest <= WNone;
                end if;
            when WWaitReady =>
                if m00_axi_awready = '1' and m00_axi_wready = '1' then
                    next_writest <= WFinished;
                elsif m00_axi_awready = '1' then
                    next_writest <= WWaitWReady;
                elsif m00_axi_wready = '1' then
                    next_writest <= WWaitAWReady;
                else
                    next_writest <= WWaitReady;
                end if;
            when WWaitWReady =>
                if m00_axi_wready = '1' then
                    next_writest <= WFinished;
                else
                    next_writest <= WWaitWReady;
                end if;
            when WWaitAWReady =>
                if m00_axi_awready = '1' then
                    next_writest <= WFinished;
                else
                    next_writest <= WWaitAWReady;
                end if;
            when WFinished =>
                if m00_axi_bvalid = '1' then
                    next_writest <= WHold;
                else
                    next_writest <= WFinished;
                end if;
        end case;
        case writest is
            when WReset =>
                m00_axi_awvalid <= '0';
                m00_axi_wvalid <= '0';
                m00_axi_bready <= '0';
                
                dwready <= '0';
                store_w_addr <= '0';
                store_w_data <= '0';
            when WHold =>
                m00_axi_awvalid <= '0';
                m00_axi_wvalid <= '0';
                m00_axi_bready <= '0';
                
                dwready <= '1';
                store_w_addr <= '1';
                store_w_data <= '1';
            when WNone =>
                m00_axi_awvalid <= '0';
                m00_axi_wvalid <= '0';
                m00_axi_bready <= '0';
                
                dwready <= '0';
                store_w_addr <= '1';
                store_w_data <= '1';
            when WWaitReady =>
                m00_axi_awvalid <= '1';
                m00_axi_wvalid <= '1';
                m00_axi_bready <= '0';
                
                dwready <= '0';
                store_w_addr <= '0';
                store_w_data <= '0';
            when WWaitAWReady =>
                m00_axi_awvalid <= '1';
                m00_axi_wvalid <= '0';
                m00_axi_bready <= '0';
                
                dwready <= '0';
                store_w_addr <= '0';
                store_w_data <= '1';
            when WWaitWReady =>
                m00_axi_awvalid <= '0';
                m00_axi_wvalid <= '1';
                m00_axi_bready <= '0';
                
                dwready <= '0';
                store_w_addr <= '1';
                store_w_data <= '0';
            when WFinished =>
                m00_axi_awvalid <= '0';
                m00_axi_wvalid <= '0';
                m00_axi_bready <= '1';
                
                dwready <= '0';
                store_w_addr <= '1';
                store_w_data <= '1';
        end case;
    end process;
    
    decode_wwidth: process(width)
    begin
        case width is
            when BYTE =>
                m00_axi_wstrb <= "0001";
            when HALF_WORD =>
                m00_axi_wstrb <= "0011";
            when WORD =>
                m00_axi_wstrb <= "1111";
            when others =>
                m00_axi_wstrb <= "1111";
        end case; 
    end process;
    
    readst_delta: process(irequest, drequest, dwrite)
    begin
        if drequest = '1' and dwrite = '0' then
            next_readst <= RData;
        elsif irequest = '1' then
            next_readst <= RInstruction;
        else
            next_readst <= RNone;
        end if;
    end process;
    
    read_lambda: process(readst, busst, dwrite, last_read)
    begin
        case readst is
            when RInstruction =>
                m00_axi_arprot <= "101";
            when RData =>
                m00_axi_arprot <= "001";
            when others =>
                m00_axi_arprot <= "000";
        end case;
        case busst is
            when BReset =>
                m00_axi_arvalid <= '0';
                m00_axi_rready <= '0';
                store_r_addr <= '0';
                store_r_data <= '0';
                ivalid <= '0';
                drvalid <= '0';
            when BHold =>
                m00_axi_arvalid <= '0';
                m00_axi_rready <= '0';
                store_r_addr <= '1';
                store_r_data <= '0';
                if last_read = RInstruction then
                    ivalid <= '1';
                    drvalid <= '0';
                elsif last_read = RData and dwrite = '0' then
                    ivalid <= '0';
                    drvalid <= '1';
                else
                    ivalid <= '0';
                    drvalid <= '0';
                end if;
            when BNone =>
                m00_axi_arvalid <= '0';
                m00_axi_rready <= '0';
                store_r_addr <= '1';
                store_r_data <= '0';
                ivalid <= '0';
                drvalid <= '0';
            when BAddrValid =>
                m00_axi_arvalid <= '1';
                m00_axi_rready <= '0';
                store_r_addr <= '0';
                store_r_data <= '1';
                ivalid <= '0';
                drvalid <= '0';
            when BWaitDataValid =>
                m00_axi_arvalid <= '0';
                m00_axi_rready <= '1';
                store_r_addr <= '0';
                store_r_data <= '1';
                ivalid <= '0';
                drvalid <= '0';
            when others =>
                m00_axi_arvalid <= '0';
                m00_axi_rready <= '0';
                store_r_addr <= '0';
                store_r_data <= '0';
                ivalid <= '0';
                drvalid <= '0';
        end case;
    end process;
    
    bus_delta: process(m00_axi_arready, readst, m00_axi_arready, m00_axi_rvalid, busst)
    begin
        case busst is
            when BReset =>
                next_busst <= BNone;
            when BHold =>
                if readst /= RNone then
                    next_busst <= BAddrValid;
                else
                    next_busst <= BNone;
                end if;
            when BNone =>
                if readst /= RNone then
                    next_busst <= BAddrValid;
                else
                    next_busst <= BNone;
                end if;
            when BAddrValid =>
                if m00_axi_arready = '1' then
                    next_busst <= BWaitDataValid;
                else
                    next_busst <= BAddrValid;
                end if;
            when BWaitDataValid =>
                if m00_axi_rvalid = '1' then
                    next_busst <= BHold;
                else
                    next_busst <= BWaitDataValid;
                end if;
        end case;
    end process;

    do_read_write: process(m00_axi_aresetn, m00_axi_aclk, store_r_addr, store_r_data, readst)
    begin
        if rising_edge(m00_axi_aclk) then
            if m00_axi_aresetn = '0' then
                readst <= RNone;
                last_read <= RNone;
                busst <= BReset;
                writest <= WReset;
                
                m00_axi_araddr <= x"00000000";
                idata <= x"00000000";
                drdata <= x"00000000";
                iwait <= '0';
                drwait <= '0';
            else
                writest <= next_writest;
                
                if store_w_addr = '1' then
                    m00_axi_awaddr <= daddr;
                end if;
                if store_w_data = '1' then
                    m00_axi_wdata <= dwdata;
                    width <= dwwidth;
                end if;
            
                if readst = RNone or next_busst = BHold then
                    readst <= next_readst;
                    last_read <= readst;
                end if;
                busst <= next_busst;
                
                if readst = RInstruction and next_busst /= BHold then
                    iwait <= '1';
                elsif readst = RNone and next_readst = RInstruction then
                    iwait <= '1';
                else
                    iwait <= '0';
                end if;
                if readst = RData then
                    drwait <= '1';
                elsif readst = RNone and next_readst = RData then
                    drwait <= '1';
                else
                    drwait <= '0';
                end if;
                if store_r_addr = '1' then
                    if readst = RInstruction then
                        m00_axi_araddr <= iaddr;
                    elsif readst = RData then
                        m00_axi_araddr <= daddr;
                    else
                        m00_axi_araddr <= x"DEADBEEF";
                    end if;
                end if;
                if store_r_data = '1' then
                    if readst = RInstruction then
                        idata <= m00_axi_rdata;
                        drdata <= (others => '0');
                    elsif readst = RData then
                        idata <= (others => '0');
                        drdata <= m00_axi_rdata;
                    else
                        idata <= x"DEADBEEF";
                        drdata <= x"DEADBEEF";
                    end if;
                end if;
            end if;
        end if;
    end process;

end Behavioral;
