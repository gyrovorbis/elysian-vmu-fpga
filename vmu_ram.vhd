
use work.memBus.all;

entity VmuRam is 
	port (
		memBusIn	: in VmuMemoryBusIn;
		memBusOut	: out VmuMemoryBusOut
	);

end entity;

architecture rtl of VmuRam is 
begin

end architecture;