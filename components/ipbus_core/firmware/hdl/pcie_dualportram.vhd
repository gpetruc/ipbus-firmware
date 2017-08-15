


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity pcie_DualPortRAM is
  generic (
    BUFWIDTH: natural := 0;
    ADDRWIDTH: natural := 0
  );
  port (
  	ClkA : in std_logic;
  	ClkB : in std_logic;
  	wea : in std_logic;
  	addra : in std_logic_vector(BUFWIDTH + ADDRWIDTH - 1 downto 0);
  	addrb : in std_logic_vector(BUFWIDTH + ADDRWIDTH - 1 downto 0);
  	dia : in std_logic_vector(31 downto 0);
  	dob : out std_logic_vector(31 downto 0)
  );
end entity pcie_DualPortRAM;


architecture rtl of pcie_DualPortRAM is
  type ram_type is array (2**(BUFWIDTH + ADDRWIDTH) - 1 downto 0) of std_logic_vector (31 downto 0);
  signal ram : ram_type;
  --attribute block_ram : boolean;
  --attribute block_ram of RAM : signal is TRUE;

begin

  write: process (ClkA)
  begin
    if (rising_edge(ClkA)) then
      if (wea = '1') then
        ram(to_integer(unsigned(addra))) <= dia
-- pragma translate_off
        after 4 ns
-- pragma translate_on
        ;
      end if;
    end if;
  end process write;

  read: process (ClkB)
  begin
    if (rising_edge(ClkB)) then
      dob <= ram(to_integer(unsigned(addrb)))
-- pragma translate_off
      after 4 ns
-- pragma translate_on
      ;
    end if;
  end process read;

end architecture rtl;