
use work.memBus.all;

entity VmuWram is 
	port (
		memBusIn	: in VmuMemoryBusIn;
		memBusOut	: out VmuMemoryBusOut
	);

end entity;

architecture rtl of VmuWram is 
	--TODO! This is only used for a certain mode when plugged into the controller.
begin

end architecture;