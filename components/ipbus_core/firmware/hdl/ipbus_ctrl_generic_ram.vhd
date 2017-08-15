
-- Tom Williams, July 2017


library ieee;
use ieee.std_logic_1164.all;
use work.ipbus.all;
use work.ipbus_trans_decl.all;


entity ipbus_ctrl_generic_ram is
  generic (
  	-- Number of address bits to select RX or TX buffer in UDP I/F
  	-- Number of RX and TX buffers is 2**BUFWIDTH
  	BUFWIDTH: natural := 4;
  	-- Number of address bits within each buffer in UDP I/F
  	-- Size of each buffer is 2**ADDRWIDTH
  	ADDRWIDTH: natural := 9;

  	N_OOB: natural := 0
  );
  port (
  	ram_clk: in std_logic;
  	rst_ramclk: in std_logic;
  	ipb_clk: in std_logic;
  	rst_ipb: in std_logic;

    ram_rx_addr : in std_logic_vector(ADDRWIDTH - 1 downto 0);
    ram_rx_data : in std_logic_vector(31 downto 0);
    ram_rx_reset : in std_logic;
    ram_rx_payload_send : in std_logic;
    ram_rx_payload_we : in std_logic;
    ram_tx_addr : in std_logic_vector(ADDRWIDTH - 1 downto 0);
    ram_tx_busy : in std_logic;
    ram_tx_data : out std_logic_vector(31 downto 0);
    ram_tx_req_send : out std_logic;

    ipb_out: out ipb_wbus; -- IPbus bus signals
    ipb_in: in ipb_rbus;
    ipb_req: out std_logic;
    ipb_grant: in std_logic := '1';

    pkt: out std_logic;
    pkt_oob: out std_logic;
    oob_in: in ipbus_trans_in_array(N_OOB - 1 downto 0) := (others => ('0', X"00000000"));
    oob_out: out ipbus_trans_out_array(N_OOB - 1 downto 0)
  );

end ipbus_ctrl_generic_ram;


architecture rtl of ipbus_ctrl_generic_ram is

  signal trans_in, trans_in_pcie: ipbus_trans_in;
  signal trans_out, trans_out_pcie: ipbus_trans_out;
  signal buf_in_a: ipbus_trans_in_array(N_OOB downto 0);
  signal buf_out_a: ipbus_trans_out_array(N_OOB downto 0);
  signal pkts: std_logic_vector(N_OOB downto 0);

begin

  generic_ram_if: entity work.ipbus_generic_ram_if
    generic map(
      BUFWIDTH => BUFWIDTH,
      ADDRWIDTH => ADDRWIDTH
    )
    port map(
      pcie_clk => ram_clk,
      rst_pcieclk => rst_ramclk,
      ipb_clk => ipb_clk,
      rst_ipb => rst_ipb,

      ram_rx_addr => ram_rx_addr,
      ram_rx_data => ram_rx_data,
      ram_rx_reset => ram_rx_reset,
      ram_rx_payload_send => ram_rx_payload_send,
      ram_rx_payload_we => ram_rx_payload_we,
      ram_tx_addr => ram_tx_addr,
      ram_tx_busy => ram_tx_busy,

      pkt_done => trans_out_pcie.pkt_done,
      raddr => trans_out_pcie.raddr,
      waddr => trans_out_pcie.waddr,
      wdata => trans_out_pcie.wdata,
      we => trans_out_pcie.we,

      ram_tx_data => ram_tx_data,
      ram_tx_req_send => ram_tx_req_send,

      pkt_ready => trans_in_pcie.pkt_rdy,
      rdata => trans_in_pcie.rdata
    );


  arb_gen: if N_OOB > 0 generate

    buf_in_a <= oob_in & trans_in_pcie;
    trans_out_pcie <= buf_out_a(0);
    oob_out <= buf_out_a(N_OOB downto 1);

    arb: entity work.ipbus_trans_arb
      generic map(NSRC => N_OOB + 1)
      port map(
      	clk => ipb_clk,
      	rst => rst_ipb,
      	buf_in => buf_in_a,
      	buf_out => buf_out_a,
      	trans_out => trans_in,
      	trans_in => trans_out,
      	pkt => pkts
      );

    pkt <= pkts(0);
    pkt_oob <= '1' when pkts(N_OOB downto 1) /= (N_OOB - 1 downto 0 => '0') else '0';

  end generate;


  n_arb_gen: if N_OOB = 0 generate 

    trans_in <= trans_in_pcie;
    trans_out_pcie <= trans_out;
    pkt <= trans_out_pcie.pkt_done;
    pkt_oob <= '0';

  end generate;


  trans: entity work.ipbus_trans
    port map(
      clk => ipb_clk,
      rst => rst_ipb,
      ipb_out => ipb_out,
      ipb_in => ipb_in,
      ipb_req => ipb_req,
      ipb_grant => ipb_grant,
      trans_in => trans_in,
      trans_out => trans_out,
      cfg_vector_in => (Others => '0'),
      cfg_vector_out => open
    );

end rtl;