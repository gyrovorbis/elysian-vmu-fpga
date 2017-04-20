
use work.memBus.all;

entity VmuIoPorts is 
	port (
		memBusIn	: in VmuMemoryBusIn;
		memBusOut	: out VmuMemoryBusOut
	);

end entity;

architecture rtl of VmuIoPorts is 
begin

end architecture;