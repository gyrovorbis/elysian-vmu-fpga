
use work.memBus.all;

entity VmuLcdc is 
	port (
		memBusIn	: in VmuMemoryBusIn;
		memBusOut	: out VmuMemoryBusOut
	);

end entity;

architecture rtl of VmuLcdc is
	--This is where logic will go for LCD controller
	--which will read from XRAM and display to some physical device
	--such as VGA, once that's implemented.
begin

end architecture;