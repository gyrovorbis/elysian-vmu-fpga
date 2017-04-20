library IEEE;
use IEEE.STD_LOGIC_1164.all;

package memBus is 

	type VmuMemMappedRegRange is array(natural range <>) of std_logic_vector;

	type VmuMemoryBusIn is record
		writeEn 	: std_logic;
		address 	: std_logic_vector(7 downto 0);
		dataIn 		: std_logic_vector(7 downto 0);
	end record;

	type VmuMemoryBusOut is record
		dataOut 	: std_logic_vector(7 downto 0);
	end record;

end package;


library IEEE;
use IEEE.STD_LOGIC_1164.all;
use ieee.numeric_std.all;
use work.memBus.all;

entity VmuMemoryMappedRegister is
	generic (
		BASE_ADDR	: std_logic_vector(7 downto 0);
		COUNT		: integer := 1
	);
	port (
		memBusIn	: in VmuMemoryBusIn;
		memBusOut	: out VmuMemoryBusOut;
		regSignal	: inout VmuMemMappedRegRange(COUNT-1 downto 0)(7 downto 0)
	);
end entity;

architecture rtl of VmuMemoryMappedRegister is 
begin
	process(memBusIn)
		variable offset : integer := to_integer(signed(memBusIn.address)) - to_integer(signed(BASE_ADDR));
	begin
		if((offset >= 0) and (offset < COUNT)) then
			if(memBusIn.writeEn = '1') then
				regSignal(offset) <= memBusIn.dataIn;
				memBusOut.dataOut <= memBusIn.dataIn;
			else 
				memBusOut.dataOut <= regSignal(offset);
			end if;

		else 
			memBusOut.dataOut <= "ZZZZZZZZ";

		end if;
		
		
	end process;
end architecture;