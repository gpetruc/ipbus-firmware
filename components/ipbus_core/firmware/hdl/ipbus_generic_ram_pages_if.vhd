-- ...
-- ...
-- Tom Williams, July 2017

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.ipbus_trans_decl.all;


entity ipbus_generic_ram_pages_if is
  generic (
  -- Number of address bits to select RX or TX buffer
  -- Number of RX and TX buffers is 2 ** INTERNALWIDTH
  BUFWIDTH: natural := 2;

  -- Number of address bits within each buffer
  -- Size of each buffer is 2**ADDRWIDTH
  ADDRWIDTH: natural := 9
  );
  port (
  	pcie_clk: in std_logic;
  	rst_pcieclk: in std_logic;
  	ipb_clk: in std_logic;
  	rst_ipb: in std_logic;

    rx_addr : in std_logic_vector(BUFWIDTH + ADDRWIDTH - 1 downto 0);
    rx_data : in std_logic_vector(31 downto 0);
    rx_we   : in std_logic;

    tx_addr : out std_logic_vector(BUFWIDTH + ADDRWIDTH downto 0);
    tx_data : out std_logic_vector(31 downto 0);
    tx_we   : out std_logic; 

    trans_out : in ipbus_trans_out;
    trans_in  : out ipbus_trans_in
  );

end ipbus_generic_ram_pages_if;



architecture rtl of ipbus_generic_ram_pages_if is

  -- meta data for publication
  SIGNAL rx_page_idx : unsigned(BUFWIDTH - 1 downto 0) := (Others => '0');
  SIGNAL tx_page_idx : unsigned(BUFWIDTH - 1 downto 0) := (Others => '0');
  SIGNAL tx_page_count : unsigned(31 downto 0) := (Others => '0');

  -- init
  SIGNAL init_phase : std_logic := '0';
  SIGNAL init_clk_count : unsigned(2 downto 0) := "000";

  -- rx handler
  SIGNAL rx_pkt_size : std_logic_vector(31 downto 0);
  SIGNAL rx_data_i : std_logic_vector(31 downto 0);
  SIGNAL rx_addr_i : std_logic_vector(ADDRWIDTH - 1 downto 0);
  SIGNAL rx_send_i, rx_send_i_d, rx_send_i_d2 : std_logic := '0';

  type tx_state_type is (TX_RESET, TX_TRANSFER_STATUS, TX_IDLE, TX_TRANSFER_PACKET, TX_UPDATE_COUNT);
  SIGNAL tx_state : tx_state_type := TX_RESET;
  SIGNAL tx_next_state : tx_state_type;
  SIGNAL tx_state_clock_count : unsigned(ADDRWIDTH - 1 downto 0) := (Others => '0');
  SIGNAL tx_addr_i : std_logic_vector(BUFWIDTH + ADDRWIDTH downto 0);
  SIGNAL tx_addr_i_stat : std_logic_vector(BUFWIDTH + ADDRWIDTH downto 0);
  SIGNAL tx_addr_i_ctrl : std_logic_vector(BUFWIDTH + ADDRWIDTH downto 0);
  SIGNAL tx_data_i_stat : std_logic_vector(31 downto 0);
  SIGNAL tx_data_i_ctrl : std_logic_vector(31 downto 0);
  SIGNAL tx_page_size : unsigned(ADDRWIDTH - 1 downto 0) := (Others => '0');
  SIGNAL tx_we_i_ctrl : std_logic;
  SIGNAL tx_busy_i : std_logic;

  SIGNAL ram_tx_req_send : std_logic;

  constant page_addr_zero : std_logic_vector(ADDRWIDTH - 1 downto 0) := (Others => '0');

begin

  reset_block : process (pcie_clk)
  begin
    if rising_edge(pcie_clk) then
      if rst_pcieclk = '1' then
        --rx_page_idx <= (Others => '0');
        --tx_page_idx <= (Others => '0');
        --tx_page_count <= (Others => '0');
        --rx_send_i <= '0';
        --rx_pkt_size <= (Others => '0');
        init_clk_count <= (Others => '1');
      elsif init_clk_count /= 3 then
        init_clk_count <= init_clk_count + 1;
        init_phase <= '1';
      else
        init_phase <= '0';
      end if;
    end if;
  end process reset_block;

  --process (pcie_clk)
  --begin
  --  if rising_edge(pcie_clk) then
  --    if init_clk_count /= 3 then
  --      init_clk_count <= init_clk_count + 1;
  --      init_phase <= '1';
  --    else
  --     init_phase <= '0';
  --    end if;
  --  end if;
  --end process;


  rx_addr_i <= rx_addr(ADDRWIDTH - 1 downto 0); -- converted to  - 2**BUFWIDTH;

  rx_data_i <= x"0001" & std_logic_vector((unsigned(rx_data(15 downto 0)) - 1)) when rx_addr = (std_logic_vector(rx_page_idx) & page_addr_zero) else rx_data;

  rx_pkt_size_extractor : process (pcie_clk)
  begin
    if rising_edge(pcie_clk) then
      if rst_pcieclk = '1' then
        rx_pkt_size <= (Others => '1');
      elsif rx_addr = (std_logic_vector(rx_page_idx) & page_addr_zero) then
        rx_pkt_size <= rx_data;
      else
      end if;
    end if;
  end process rx_pkt_size_extractor;

  rx_pkt_end_detector : process (pcie_clk)
  begin
    if rising_edge(pcie_clk) then
      if rst_pcieclk = '1' then
        rx_send_i <= '0';
        rx_page_idx <= (Others => '0');
      elsif rx_addr = (std_logic_vector(rx_page_idx) & std_logic_vector(resize(unsigned(rx_pkt_size),ADDRWIDTH))) then
        rx_send_i <= '1';
        rx_page_idx <= rx_page_idx + 1;
      else
        rx_send_i <= '0';
      end if;
    end if;
  end process;

  process (pcie_clk)
  begin
    if rising_edge(pcie_clk) then
      rx_send_i_d <= rx_send_i;
    end if;
  end process;

  process (pcie_clk)
  begin
    if rising_edge(pcie_clk) then
      rx_send_i_d2 <= rx_send_i_d;
    end if;
  end process;


  --------------------------
  --   TX STATE MACHINE   --
  --------------------------

  process (pcie_clk)
  begin
    if rising_edge(pcie_clk) then
      if rst_pcieclk = '1' then
        tx_state <= TX_RESET;
      else
        tx_state <= tx_next_state;
      end if;
    end if;
  end process;

  process (pcie_clk)
  begin
    if rising_edge(pcie_clk) then
      if rst_pcieclk = '1' or (tx_next_state /= tx_state) then
        tx_state_clock_count <= (Others => '0');
      else
        tx_state_clock_count <= tx_state_clock_count + 1;
      end if;
    end if;
  end process;


  -- Combinatorial logic that determines next state
  process (tx_state, tx_state_clock_count, ram_tx_req_send) -- FIXME: replace all with actual list
  begin
    case tx_state is
      when TX_RESET =>
        tx_next_state <= TX_TRANSFER_STATUS;

      when TX_TRANSFER_STATUS =>
        if tx_state_clock_count(2 downto 0) /= "011" then
          tx_next_state <= TX_TRANSFER_STATUS;
        else
          tx_next_state <= TX_IDLE;
        end if;

      when TX_IDLE =>
        if ram_tx_req_send = '0' then
          tx_next_state <= TX_IDLE;
        else
          tx_next_state <= TX_TRANSFER_PACKET;
        end if;

      when TX_TRANSFER_PACKET =>
        if (tx_state_clock_count /= tx_page_size + 1) then
          tx_next_state <= TX_TRANSFER_PACKET;
        else
          tx_next_state <= TX_TRANSFER_STATUS;
        end if;

      when others =>
        tx_next_state <= TX_RESET;

    end case;
  end process;


  tx_addr <= tx_addr_i;
  tx_addr_i <= tx_addr_i_stat when tx_state = TX_TRANSFER_STATUS else tx_addr_i_ctrl;
  tx_addr_i_stat <= std_logic_vector(resize(unsigned(tx_state_clock_count), BUFWIDTH + ADDRWIDTH + 1));
  process (pcie_clk)
  begin
    if rising_edge(pcie_clk) then
      tx_addr_i_ctrl <= std_logic_vector(resize(tx_page_idx & tx_state_clock_count, BUFWIDTH + ADDRWIDTH + 1) + 4);
    end if ;
  end process ;

  tx_data <= tx_data_i_stat when tx_state = TX_TRANSFER_STATUS else tx_data_i_ctrl;
  process (tx_state_clock_count)
  begin
    if tx_state_clock_count(1 downto 0) = "00" then
      tx_data_i_stat <= std_logic_vector(to_unsigned(2**BUFWIDTH, 32));
    elsif tx_state_clock_count(1 downto 0) = "01" then
      tx_data_i_stat <= std_logic_vector(to_unsigned(2**ADDRWIDTH, 32));
    elsif tx_state_clock_count(1 downto 0) =  "10" then
      tx_data_i_stat <= std_logic_vector(resize(rx_page_idx, 32));
    else
      tx_data_i_stat <= std_logic_vector(tx_page_count);
    end if;
  end process;

  tx_extract_page_size : process (pcie_clk)
  begin
    if rising_edge(pcie_clk) then
      if ((rst_pcieclk = '1') or (tx_state = TX_IDLE)) then
        tx_page_size(ADDRWIDTH - 1 downto 1) <= (Others => '1');
        tx_page_size(0) <= '0';
      elsif (tx_state = TX_TRANSFER_PACKET and tx_state_clock_count = 1) then
        tx_page_size <= resize(unsigned(tx_data_i_ctrl(15 downto 0)) + 1, ADDRWIDTH);
      end if;
    end if;
  end process;

  tx_we <= '1' when (tx_state = TX_TRANSFER_STATUS) else tx_we_i_ctrl;
  process (pcie_clk)
  begin
    if rising_edge(pcie_clk) then
      if (tx_state = TX_TRANSFER_PACKET) then
        tx_we_i_ctrl <= '1';
      else
        tx_we_i_ctrl <= '0';
      end if;
    end if ;
  end process;

  tx_busy_i <= '1' when (tx_state = TX_TRANSFER_PACKET) else '0';

  process (pcie_clk)
  begin
    if rising_edge(pcie_clk) then
      if (tx_state = TX_TRANSFER_PACKET and tx_next_state = TX_TRANSFER_STATUS) then
        tx_page_count <= tx_page_count + 1;
      end if;
    end if;
  end process;



  -----------------------------------------------
  --  GENERIC IPBUS PACKET-ORIENTED INTERFACE  --
  -----------------------------------------------

  ipbus_ram_pkt_if : entity work.ipbus_generic_ram_if
    generic map (
      BUFWIDTH => BUFWIDTH,
      ADDRWIDTH => ADDRWIDTH
    )
    port map (
      pcie_clk => pcie_clk,
      rst_pcieclk => rst_pcieclk,
      ipb_clk => ipb_clk,
      rst_ipb => rst_ipb,

      ram_rx_addr => rx_addr_i,
      ram_rx_data => rx_data_i,
      ram_rx_reset => rst_pcieclk,
      ram_rx_payload_send => rx_send_i_d2,
      ram_rx_payload_we => rx_we,
      ram_tx_addr => std_logic_vector(tx_state_clock_count),
      ram_tx_busy => tx_busy_i,

      pkt_done => trans_out.pkt_done,
      raddr => trans_out.raddr,
      waddr => trans_out.waddr,
      wdata => trans_out.wdata,
      we => trans_out.we,

      ram_tx_data => tx_data_i_ctrl,
      ram_tx_req_send => ram_tx_req_send,

      pkt_ready => trans_in.pkt_rdy,
      rdata => trans_in.rdata
    );


end rtl;