-- Dummy transactor...
-- copies rx packet to tx...
--
-- Dave Sankey, September 2012
--
-- updated to 2016 I/F Mar 2016

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity dummytransactor is
 port (
   ipb_clk: in std_logic;
   rst_ipb: in std_logic;
--
   rdata: in std_logic_vector(31 downto 0);
   pkt_rdy: in std_logic;
   pkt_done: out std_logic;
   raddr: out std_logic_vector(11 downto 0);
--
   we: out std_logic;
   waddr: out std_logic_vector(11 downto 0);
   wdata: out std_logic_vector(31 downto 0)
 );
end dummytransactor;

architecture dummy of dummytransactor is

begin

 wdata <= rdata;

mem_copy: process (ipb_clk)
 variable active, we_i: std_logic;
 variable counter, next_counter, endaddr: unsigned(10 downto 0);
 begin
   if rising_edge(ipb_clk) then
     if rst_ipb = '1' then
       active := '0';
     end if;
     if pkt_rdy = '1' then
       active := '1';
     end if;
     if active = '0' then
       counter := (Others => '0');
       next_counter := (Others => '0');
       endaddr := (Others => '0');
       we_i := '0';
     else
       we_i := '1';
       counter := next_counter;
       if counter = to_unsigned(0, 11) then
         endaddr := unsigned(rdata(26 downto 16)) +
         unsigned(rdata(10 downto 0));
       elsif counter = endaddr then
         active := '0';
       end if;
       next_counter := counter + 1;
     end if;
     we <= we_i
-- pragma translate_off
     after 5 ns
-- pragma translate_on
     ;
     pkt_done <= not we_i
-- pragma translate_off
     after 5 ns
-- pragma translate_on
     ;
     raddr <= "0" & std_logic_vector(next_counter)
-- pragma translate_off
     after 5 ns
-- pragma translate_on
     ;
     waddr <= "0" & std_logic_vector(counter)
-- pragma translate_off
     after 5 ns
-- pragma translate_on
     ;
   end if;
 end process;

end dummy;
