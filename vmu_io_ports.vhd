
use work.memBus.all;

entity VmuIoPorts is 
	port (
		memBusIn	: in VmuMemoryBusIn;
		memBusOut	: out VmuMemoryBusOut
	);

end entity;

architecture rtl of VmuIoPorts is 
	--TODO: These are used for serial communication between devices 
	--for file transfer and multiplayer gaming. 
begin

end architecture;