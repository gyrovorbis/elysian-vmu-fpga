library IEEE;
use IEEE.STD_LOGIC_1164.all;

package instrSet is 

	type VmuInstrArgType is (    
		INSTR_ARG_TYPE_NONE,
    	INSTR_ARG_TYPE_R8,
    	INSTR_ARG_TYPE_R16,
    	INSTR_ARG_TYPE_I8,
    	INSTR_ARG_TYPE_D9,
    	INSTR_ARG_TYPE_N2, 
    	INSTR_ARG_TYPE_A12,
    	INSTR_ARG_TYPE_A16,
    	INSTR_ARG_TYPE_B3,
    	INSTR_ARG_TYPE_COUNT
	);	

	type VmuInstrArgTypes is array(natural range <>) of VmuInstrArgType;

	type VmuInstructionAttr is record
		opcode		: std_logic_vector(7 downto 0);
		operands	: VmuInstrArgTypes(2 downto 0);
		opBits		: integer;
		bytes		: integer;
		cycles		: integer;
	end record;


end package;
