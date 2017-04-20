library IEEE;
use IEEE.STD_LOGIC_1164.all;

use work.memBus.all;
use work.VmuCpu;
use work.VmuRam;
use work.VmuFlash;
use work.VmuRom;
use work.VmuWram;
use work.VmuLcdc;
use work.VmuIoPorts;

entity VmuDevice is 
		

end entity;

architecture rtl of VmuDevice is 
	signal memBusIn 	: VmuMemoryBusIn;
	signal memBusOut 	: VmuMemoryBusOut;
	signal clock		: std_logic := '0';
	signal cpuReset		: std_logic := '0';
begin

	clock <= not clock after 100ms;

	cpu: entity VmuCpu port map (
		clk			=> clock,
		reset		=> cpuReset,
		memBusIn 	=> memBusIn,
		memBusOut 	=> memBusOut
	);

	ram: entity VmuRam port map (
		memBusIn 	=> memBusIn,
		memBusOut 	=> memBusOut
	);

	rom: entity VmuRom port map (
		memBusIn 	=> memBusIn,
		memBusOut 	=> memBusOut
	);

	flash: entity VmuFlash port map (
		memBusIn 	=> memBusIn,
		memBusOut 	=> memBusOut
	);

	wram: entity VmuWram port map (
		memBusIn 	=> memBusIn,
		memBusOut 	=> memBusOut
	);

	lcdc: entity VmuLcdc port map (
		memBusIn 	=> memBusIn,
		memBusOut 	=> memBusOut
	);

	ioPorts: entity VmuIoPorts port map (
		memBusIn 	=> memBusIn,
		memBusOut 	=> memBusOut
	);
	

end architecture;