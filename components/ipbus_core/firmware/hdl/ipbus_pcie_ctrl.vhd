

-- Tom Williams, July 2017


library ieee;
use ieee.std_logic_1164.all;
use work.ipbus.all
use work.ipbus_trans_decl.all;


entity ipbus_pcie_ctrl is
  generic (
  	-- Number of address bits to select RX or TX buffer in UDP I/F
  	-- Number of RX and TX buffers is 2**BUFWIDTH
  	BUFWIDTH: natural := 4;
  	-- Number of address bits within each buffer in UDP I/F
  	-- Size of each buffer is 2**ADDRWIDTH
  	ADDRWIDTH: natural := 11;

  	N_OOB: natural := 0
  );
  port (
  	pcie_clk: in std_logic;
  	rst_pcieclk: in std_logic;
  	ipb_clk: in std_logic;
  	rst_ipb: in std_logic;

    pcie_rx_data: in std_logic_vector(31 downto 0); -- AXI4 style PCIe signals
    pcie_rx_valid: in std_logic;
    pcie_rx_last: in std_logic;
    pcie_rx_error: in std_logic;

    pcie_tx_data: out std_logic_vector(31 downto 0);
    pcie_tx_valid: out std_logic;
    pcie_tx_last: out std_logic;
    pcie_tx_error: out std_logic;
    pcie_tx_ready: in std_logic;

    ipb_out: out ipb_wbus; -- IPbus bus signals
    ipb_in: in ipb_rbus;
    ipb_req: out std_logic;
    ipb_grant: in std_logic := '1';

    pkt: out std_logic;
    pkt_oob: out std_logic;
    oob_in: in ipbus_trans_in_array(N_OOB - 1 downto 0) := (others => ('0', X"00000000"));
    oob_out: out ipbus_trans_out_array(N_OOB - 1 downto 0)
  );

end ipbus_pcie_ctrl;


architecture rtl of ipbus_pcie_ctrl is

  signal trans_in, trans_in_pcie: ipbus_trans_in;
  signal trans_out, trans_out_pcie: ipbus_trans_out;
  signal buf_in_a: ipbus_trans_in_array(N_OOB downto 0);
  signal buf_out_a: ipbus_trans_out_array(N_OOB downto 0);
  signal pkts: std_logic_vector(N_OOB downto 0);

begin

  pcie_if: entity work.pcie_if
    generic map(
      BUFWIDTH => BUFWIDTH,
      ADDRWIDTH => ADDRWIDTH
    )
    port map(
      pcie_clk => pcie_clk,
      rst_pcieclk => rst_pcieclk,
      ipb_clk => ipb_clk,
      rst_ipb => rst_ipb,

      pcie_rx_data => pcie_rx_data,
      pcie_rx_error => pcie_rx_error,
      pcie_rx_last => pcie_rx_last,
      pcie_rx_valid => pcie_rx_valid,
      pcie_tx_ready => pcie_tx_ready,

      pkt_done => trans_out_pcie.pkt_done,
      raddr => trans_out_pcie.raddr,
      waddr => trans_out_pcie.waddr,
      wdata => trans_out_pcie.wdata,
      we => trans_out_pcie.we,

      pcie_tx_data => pcie_tx_data,
      pcie_tx_error => pcie_tx_error,
      pcie_tx_last => pcie_tx_last,
      pcie_tx_valid => pcie_tx_valid,

      pkt_ready => trans_in_pcie.pkt_ready,
      rdata => trans_in_pcie.rdata

      );


  arb_gen: if N_OOB > 0 generate

    buf_in_a <= oob_in & trans_in_pcie;
    trans_out_pcie <= bug_out_a(0);
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
    pkt <= trans_out_udp.pkt_done;
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
      trans_out => trans_out
    )

end rtl;