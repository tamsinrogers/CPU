library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cpubench is
end cpubench;

architecture test of cpubench is
  constant num_cycles : integer := 50;

  signal iport: std_logic_vector( 7 downto 0) := "00000000";
  signal oport: std_logic_vector(15 downto 0) := "0000000000000000";

  signal PC: std_logic_vector( 7 downto 0) := "00000000";
  signal IR: std_logic_vector(15 downto 0) := "0000000000000000";
  signal RA: std_logic_vector(15 downto 0) := "0000000000000000";
  signal RB: std_logic_vector(15 downto 0) := "0000000000000000";
  signal RC: std_logic_vector(15 downto 0) := "0000000000000000";
  signal RD: std_logic_vector(15 downto 0) := "0000000000000000";
  signal RE: std_logic_vector(15 downto 0) := "0000000000000000";
  
  signal clock: std_logic := '1';
  signal reset: std_logic;

  
  -- component statement for the ALU
  component cpu
    port (
      clock   : in  std_logic;                      -- main clock
      reset : in  std_logic;                       -- reset button

      PCview : out std_logic_vector( 7 downto 0);
      IRview : out std_logic_vector(15 downto 0);
      RAview : out std_logic_vector(15 downto 0);
      RBview : out std_logic_vector(15 downto 0);
      RCview : out std_logic_vector(15 downto 0);
      RDview : out std_logic_vector(15 downto 0);
      REview : out std_logic_vector(15 downto 0);

      iport : in  std_logic_vector(7 downto 0);    -- input port
      oport : out std_logic_vector(15 downto 0));  -- output port
  end component;
  
begin



  -- start off with a short reset
  reset <= '0', '1' after 1 ns;

  -- create a clock
  process
  begin
    for i in 1 to num_cycles loop
      clock <= not clock;
      wait for 1 ns;
      
      clock <= not clock;
      wait for 1 ns;
    end loop;
    wait;
  end process;

  iport <= "00000000";

  cpuinstance: cpu
    port map( clock, reset, PC, IR, RA, RB, RC, RD, RE, iport, oport );

  
end test;