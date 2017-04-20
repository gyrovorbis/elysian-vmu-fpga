
use work.memBus.all;

entity VmuWram is 
	port (
		memBusIn	: in VmuMemoryBusIn;
		memBusOut	: out VmuMemoryBusOut
	);

end entity;

architecture rtl of VmuWram is 
begin

end architecture;