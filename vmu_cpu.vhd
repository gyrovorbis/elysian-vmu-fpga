library IEEE;
use IEEE.STD_LOGIC_1164.all;

use work.memBus.all;
use work.VmuMemoryMappedRegister;

entity VmuCpu is 
	port (
		clk			: in std_logic;
		reset		: in std_logic;
		memBusIn	: inout VmuMemoryBusIn;
		memBusOut	: out 	VmuMemoryBusOut
	);

end entity;

architecture behav of VmuCpu is 
	signal pc 		: integer := 0;
	signal intReq	: integer := 0;
	signal intMask	: integer := 0;
	signal curInstr	: std_logic_vector(23 downto 0);
	signal extReg 	: VmuMemMappedRegRange(7 downto 0);
begin

	extRegMap: entity VmuMemoryMappedRegister 
		generic map(x"0d") 
		port map(memBusIn, memBusOut, extReg);

	process(clk) 

	begin
		if(rising_edge(clk)) then

		end if;

	end process;


end architecture;