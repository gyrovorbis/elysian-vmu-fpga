library IEEE;
use IEEE.STD_LOGIC_1164.all;

package sfr is 

	--PSW - Processor Status Word (0x101)
	constant SFR_PSW_CY_BIT 	: natural := 7;       --Carry Flag 
	constant SFR_PSW_AC_BIT 	: natural := 6;       --Auxiliary Carry Flag
	constant SFR_PSW_IRBK1_BIT 	: natural := 4;       --Indirect Address Register Bank
	constant SFR_PSW_IRBK0_BIT  : natural := 3;
	constant SFR_PSW_OV_BIT     : natural := 2;       --Overflow Flag 
	constant SFR_PSW_RAMBK0_BIT : natural := 1;       --RAM Bank 

end package;