-- ...
-- ...
-- Tom Williams, July 2017

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity ipbus_generic_ram_if is
  generic (
-- Number of address bits to select RX or TX buffer
-- Number of RX and TX buffers is 2**BUFWIDTH
    BUFWIDTH: natural := 2;

---- Number of address bits to select internal buffer
---- Number of internal buffers is 2**INTERNALWIDTH
--    INTERNALWIDTH: natural := 1;

-- Number of address bits within each buffer
-- Size of each buffer is 2**ADDRWIDTH
-- Two less than in udp_if_flat, since data words are 32-bit here rather than 8-bit
    ADDRWIDTH: natural := 9
  );
  port (
    pcie_clk: in std_logic;
    rst_pcieclk: in std_logic;
    ipb_clk: in std_logic;
    rst_ipb: in std_logic;

--    enable: in std_logic;
    --pcie_rx_data: in std_logic_vector(31 downto 0);
    --pcie_rx_error: in std_logic;
    --pcie_rx_last: in std_logic;
    --pcie_rx_valid: in std_logic;
    --pcie_tx_ready: in std_logic;
    ram_rx_addr : in std_logic_vector(ADDRWIDTH - 1 downto 0);
    ram_rx_data : in std_logic_vector(31 downto 0);
    ram_rx_reset : in std_logic;
    ram_rx_payload_send : in std_logic;
    ram_rx_payload_we : in std_logic;
    ram_tx_addr : in std_logic_vector(ADDRWIDTH - 1 downto 0);
    ram_tx_busy : in std_logic;

    pkt_done: in std_logic;
    raddr: in std_logic_vector(11 downto 0); -- ?? FIXME ?? use diff width, since diff data width?
    waddr: in std_logic_vector(11 downto 0); -- ?? FIXME ?? use diff width, since diff data width?
    wdata: in std_logic_vector(31 downto 0);
    we: in std_logic;

    --pcie_tx_data: out std_logic_vector(31 downto 0);
    --pcie_tx_error: out std_logic;
    --pcie_tx_last: out std_logic;
    --pcie_tx_valid: out std_logic;
    ram_tx_data : out std_logic_vector(31 downto 0);
    ram_tx_req_send : out std_logic;

    pkt_ready: out std_logic;
    rdata: out std_logic_vector(31 downto 0)
  );

end ipbus_generic_ram_if;



architecture rtl of ipbus_generic_ram_if is

   SIGNAL req_resend: std_logic;

   SIGNAL rst_pcieclk_reg: std_logic;
   SIGNAL rx_wea: std_logic;

   SIGNAL we_125: std_logic;

   SIGNAL rx_read_buffer, rx_read_buffer_125: std_logic_vector(BUFWIDTH - 1 downto 0);
   SIGNAL rx_write_buffer, tx_read_buffer: std_logic_vector(BUFWIDTH - 1 downto 0);
   SIGNAL tx_write_buffer, tx_write_buffer_125, resend_buf: std_logic_vector(BUFWIDTH - 1 downto 0);

   SIGNAL rx_full_addra, rx_full_addrb, tx_full_addra, tx_full_addrb: std_logic_vector(BUFWIDTH + ADDRWIDTH - 1 downto 0);
   SIGNAL pkt_rcvd, rx_ram_busy, rx_req_send_125, udpram_sent: std_logic;
   SIGNAL rx_ram_sent, tx_ram_written: std_logic;

   signal clean_buf: std_logic_vector(2**BUFWIDTH - 1 downto 0);

begin

  rx_full_addra <= rx_write_buffer & ram_rx_addr;
  rx_full_addrb <= rx_read_buffer & raddr(ADDRWIDTH - 1 downto 0);

  tx_full_addra <= tx_write_buffer & waddr(ADDRWIDTH - 1 downto 0);
  tx_full_addrb <= tx_read_buffer & ram_tx_addr;



-- register reset
  rst_pcieclk_block: process(pcie_clk)
  begin
    if rising_edge(pcie_clk) then
      rst_pcieclk_reg <= rst_pcieclk
-- pragma translate_off
      after 4 ns
-- pragma translate_on
      ;
    end if;
  end process rst_pcieclk_block;



  ipbus_rx_ram: entity work.pcie_DualPortRAM
    generic map (
      BUFWIDTH => BUFWIDTH,
      ADDRWIDTH => ADDRWIDTH
    )
    port map (
      clkA => pcie_clk,        -- from input port
      clkB => ipb_clk,         -- from input port
      wea => rx_wea,           -- from rx_ram_selector
      addra => rx_full_addra,  -- TODO: from rx_ram_selector + udp_build_payload
      addrb => rx_full_addrb,  -- from clock domain crossing + input port
      dia => ram_rx_data,     -- TODO: from 'payload' instance of 'udp_build_payload'
      dob => rdata             -- to output port
    );

  rx_ram_selector: entity work.udp_buffer_selector
    generic map (
      BUFWIDTH => BUFWIDTH
    )
    port map (
      mac_clk => pcie_clk,               -- from input port
      rst_macclk_reg => rst_pcieclk_reg, -- from local register based on rst_macclk
      written => pkt_rcvd,               -- from rx_transactor
      we => rx_wea,                      -- from rx_transactor
      sent => rx_ram_sent,               -- from clock domain crossing
      req_resend => '0',
      resend_buf => (Others => '0'),
      busy => rx_ram_busy,               -- to rx_transactor
      write_buf => rx_write_buffer,      -- to ipbus_rx_ram
      req_send => rx_req_send_125,       -- TODO (partial): to clock domain crossing and udp_status_buffer
      send_buf => rx_read_buffer_125,    -- to clock domain crossing (and thence ipbus_rx_ram)
      clean_buf => open
    );

  rx_transactor: entity work.udp_rxtransactor_if
    port map (
      mac_clk => pcie_clk,                        -- from input port
      rx_reset => ram_rx_reset,                       -- TODO: from udp_do_rx_reset
      payload_send => ram_rx_payload_send,               -- TODO: from udp_build_payload
      payload_we => ram_rx_payload_we,                   -- TODO: from udp_build_payload
      rx_ram_busy => rx_ram_busy,                 -- from rx_ram_selector
      pkt_rcvd => pkt_rcvd,                       -- TODO (partial): to udp_buffer_selector and udp_status_buffer
      rx_wea => rx_wea,                           -- to ipbus_rx_ram and rx_ram_selector
      rxpayload_dropped => open  -- TODO: to udp_status_buffer and contributes to output port -- FIXME: Change to output port??
    );


  ipbus_tx_ram: entity work.pcie_DualPortRAM
    generic map (
      BUFWIDTH => BUFWIDTH,
      ADDRWIDTH => ADDRWIDTH
    )
    port map (
      clkA => ipb_clk,         -- from input port
      clkB => pcie_clk,        -- from input port
      wea => we,               -- from input port
      addra => tx_full_addra,  -- from clock domain crossing + input port
      addrb => tx_full_addrb,  -- TODO: from tx_ram_selector + 'tx_main' instance of 'udp_tx_mux'
      dia => wdata,            -- from input port
      dob => ram_tx_data            -- TODO: to tx_byte_sum and udp_tx_mux
    );

  tx_ram_selector: entity work.udp_buffer_selector
    generic map (
      BUFWIDTH => BUFWIDTH
    )
    port map (
      mac_clk => pcie_clk,                -- from input port
      rst_macclk_reg => rst_pcieclk_reg,  -- from local register based on rst_macclk
      written => tx_ram_written,          -- from clock domain crossing
      we => we_125,                       -- from clock domain crossing
      sent => udpram_sent,                -- from tx_transactor
      req_resend => req_resend,           -- from tx_transactor
      resend_buf => resend_buf,           -- from tx_transactor
      busy => open,
      write_buf => tx_write_buffer_125,   -- to clock domain crossing (thence tx_full_addra = addra of tx_ram)
      req_send => ram_tx_req_send,            -- TODO: to udp_tx_mux and udp_status_buffer
      send_buf => tx_read_buffer,         -- to tx_transactor and tx_full_addrb = addrb of tx_ram
      clean_buf => clean_buf              -- to tx_transactor 
    );
   -- clean_buf : buf has had full packet written into it

  tx_transactor: entity work.udp_txtransactor_if
    generic map (
      BUFWIDTH => BUFWIDTH
    )
    port map (
      mac_clk => pcie_clk,                 -- from input port
      rst_macclk_reg => rst_pcieclk_reg,   -- from rx_ram_selector
      pkt_resend => '0',            -- TODO: from udp_build_resend
      resend_pkt_id => (Others => '0'),      -- TODO: from udp_build_resend
      ipbus_out_hdr => (Others => '0'),      -- TODO: from udp_tx_mux 
      ipbus_out_valid => '0',  -- TODO: from udp_tx_mux
      tx_read_buffer => tx_read_buffer,    -- from tx_ram_selector
      udpram_busy => ram_tx_busy,          -- TODO: from 'tx_main' instance of 'udp_tx_mux'
      clean_buf => clean_buf,              -- from tx_ram_selector
      req_not_found => open,    -- TODO: to udp_status_buffer
      req_resend => req_resend,            -- to tx_ram_selector
      resend_buf => resend_buf,            -- to tx_ram_selector
      udpram_sent => udpram_sent           -- to tx_ram_selector
    );


  clock_crossing_if: entity work.udp_clock_crossing_if
    generic map (
      BUFWIDTH => BUFWIDTH
    )
    port map (
      mac_clk => pcie_clk,                         -- from input port
      rst_macclk_reg => rst_pcieclk_reg,           -- Check with Sankey: from local register based on rst_macclk
      rx_read_buffer_125 => rx_read_buffer_125,    -- from rx_ram_selector
      rx_req_send_125 => rx_req_send_125,          -- from rx_ram_selector
      tx_write_buffer_125 => tx_write_buffer_125,  -- from tx_ram_selector
      enable_125 => open,
      rarp_125 => open,
      rst_ipb_125 => open,                  -- TODO: to udp_status_buffer
      rx_ram_sent => rx_ram_sent,                  -- TODO (partial): to rx_ram_selector and udp_status_buffer
      tx_ram_written => tx_ram_written,            -- TODO (partial): to tx_ram_selector and udp_status_buffer
      we_125 => we_125,                            -- to tx_ram_selector
      ipb_clk => ipb_clk,                          -- from input port
      rst_ipb => rst_ipb,                          -- from input port
      enable => '0',
      pkt_done => pkt_done,                        -- from input port
      RARP => '0',
      we => we,                                    -- from input port
      pkt_rdy => pkt_ready,                        -- to output port
      rx_read_buffer => rx_read_buffer,            -- to (part of) rx_full_addrb signal
      tx_write_buffer => tx_write_buffer           -- to (part of) tx_full_addra signal
    );


end rtl;