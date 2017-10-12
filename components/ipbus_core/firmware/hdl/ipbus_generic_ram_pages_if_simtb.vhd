library IEEE;
use IEEE.STD_LOGIC_1164.ALL;


use work.ipbus.ALL;
use work.ipbus_trans_decl.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity ipbus_generic_ram_pages_if_simtb is
--  Port ( );
end ipbus_generic_ram_pages_if_simtb;

architecture Behavioral of ipbus_generic_ram_pages_if_simtb is

  signal ipb_clk: std_logic := '0';

  signal rst_ipb: std_logic := '1';
  signal rst_pcie: std_logic := '1';
  signal nuke: std_logic := '0';

  signal rx_addr : std_logic_vector(10 downto 0);
  signal rx_data : std_logic_vector(31 downto 0);
  signal rx_we : std_logic := '0';

  signal tx_addr : std_logic_vector(11 downto 0);
  signal tx_data : std_logic_vector(31 downto 0);
  signal tx_we : std_logic;
   
  signal trans_in : ipbus_trans_in;
  signal trans_out : ipbus_trans_out;

begin

  ipb_clk <= not ipb_clk after 16 ns;

  STIMULUS: process
  begin
    rst_ipb <= '1';
    rst_pcie <= '1';
    wait for 128 ns;
    wait until ipb_clk = '0';
    rst_ipb <= '0';
    rst_pcie <= '0';
    wait for 256 ns;
    --- IPBUS INPUT PACKET: HEADERS ---
    rx_we <= '1';
    rx_addr <= "000" & X"00";
    rx_data <= X"00000003"; -- packet length; WAS header length, payload length
    wait for 32 ns;
    rx_addr <= "000" & X"01";
    rx_data <= X"200001F0"; -- IPbus packet header
    -- IPBUS INPUT PACKET: READ TRANSACTION --
    wait for 32 ns;
    rx_addr <= "000" & X"02";
    rx_data <= X"2001010F";
    wait for 32 ns;
    rx_addr <= "000" & X"03";
    rx_data <= X"00000001";
    -- END OF INPUT PACKET --
    wait for 32 ns;
    rx_we <= '0';
    --wait for 32 ns;
    --ram_rx_payload_send <= '1';
    --wait for 32 ns;
    --ram_rx_payload_send <= '0';
    wait;
  end process;
  
--  TX_BUF_STIMULUS : process
--  begin
--    ram_tx_addr <= '0' & X"00";
--    wait for 1440ns;
----    if rising_edge(ram_tx_req_send) then
--      ram_tx_addr <= '0' & X"00";
--      wait for 32 ns;
--      ram_tx_addr <= '0' & X"01";
--      wait for 32 ns;
--      ram_tx_addr <= '0' & X"02";
--      wait for 32 ns;
--      ram_tx_addr <= '0' & X"03";
--      wait for 32 ns;
--      ram_tx_addr <= '0' & X"04";
--      wait for 32 ns;
--      ram_tx_addr <= '0' & X"05";
--      wait;
----    end if;
--  end process;

  ipbus_if : entity work.ipbus_generic_ram_pages_if
    port map (
      pcie_clk => ipb_clk,
      rst_pcieclk => rst_pcie,
      ipb_clk => ipb_clk,
      rst_ipb => rst_ipb,

      rx_addr => rx_addr,
      rx_data => rx_data,
      rx_we => rx_we,

      tx_addr => tx_addr,
      tx_data => tx_data,
      tx_we => tx_we,
    
      trans_out => trans_out,
      trans_in => trans_in
    );


    trans: entity work.dummytransactor
    port map(
      ipb_clk => ipb_clk,
      rst_ipb => rst_ipb,

      rdata => trans_in.rdata,
      pkt_rdy => trans_in.pkt_rdy,
      pkt_done => trans_out.pkt_done,
      raddr => trans_out.raddr,
      we => trans_out.we,
      waddr => trans_out.waddr,
      wdata => trans_out.wdata
    );

end Behavioral;
