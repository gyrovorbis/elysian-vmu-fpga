library IEEE;
use IEEE.STD_LOGIC_1164.all;

package instrSet is 

constant OPCODE_NOP 			: integer := 16#00#;
constant OPCODE_BR 				: integer := 16#01#;
constant OPCODE_LD 				: integer := 16#02#;
constant OPCODE_LD_COUNT 		: integer := 2;
constant OPCODE_LD_IND 			: integer := 16#04#;
constant OPCODE_LD_IND_COUNT 	: integer := 4;
constant OPCODE_CALL 			: integer := 16#08#;
constant OPCODE_CALL_COUNT 		: integer := 8;
constant OPCODE_CALLR 			: integer := 16#10#;
constant OPCODE_BRF 			: integer := 16#11;
constant OPCODE_ST 				: integer := 16#12#;
constant OPCODE_ST_COUNT 		: integer := 2;
constant OPCODE_ST_IND 			: integer := 16#14#;
constant OPCODE_ST_IND_COUNT 	: integer := 4;
constant OPCODE_CALLF 			: integer := 16#20#;
constant OPCODE_JMPF 			: integer := 16#21#;
constant OPCODE_MOV 			: integer := 16#22#;
constant OPCODE_MOV_COUNT 		: integer := 2;
constant OPCODE_MOV_IND 		: integer := 16#24#;
constant OPCODE_MOV_IND_COUNT 	: integer := 4;
constant OPCODE_JMP 			: integer := 16#28#;
constant OPCODE_JMP_COUNT 		: integer := 8;
constant OPCODE_MUL 			: integer := 16#30#;
constant OPCODE_BEI 			: integer := 16#31#;
constant OPCODE_BE 				: integer := 16#32#;
constant OPCODE_BE_COUNT 		: integer := 2;
constant OPCODE_BE_IND 			: integer := 16#34#;
constant OPCODE_BE_IND_COUNT 	: integer := 4;
constant OPCODE_DIV 			: integer := 16#40#;
constant OPCODE_BNEI 			: integer := 16#41#;
constant OPCODE_BNE 			: integer := 16#42#;
constant OPCODE_BNE_COUNT 		: integer := 2;
constant OPCODE_BNE_IND 		: integer := 16#44#;
constant OPCODE_BNE_IND_COUNT 	: integer := 4;
constant OPCODE_BPC 			: integer := 16#48#;
constant OPCODE_BPC_COUNT 		: integer := 8;
constant OPCODE_LDF 			: integer := 16#50#;
constant OPCODE_LDF_COUNT 		: integer := 1;
constant OPCODE_STF 			: integer := 16#51#;
constant OPCODE_STF_COUNT 		: integer := 2;
constant OPCODE_DBNZ 			: integer := 16#52#;
constant OPCODE_DBNZ_COUNT 		: integer := 2;
constant OPCODE_DBNZ_IND 		: integer := 16#54#;
constant OPCODE_DBNZ_IND_COUNT 	: integer := 4;
constant OPCODE_PUSH 			: integer := 16#60#;
constant OPCODE_PUSH_COUNT 		: integer := 2;
constant OPCODE_INC 			: integer := 16#62#;
constant OPCODE_INC_COUNT  		: integer := 2;
constant OPCODE_INC_IND 		: integer := 16#64#;
constant OPCODE_INC_IND_COUNT 	: integer := 4;
constant OPCODE_BP 				: integer := 16#68#;
constant OPCODE_BP_COUNT 		: integer := 8;
constant OPCODE_POP 			: integer := 16#70#;
constant OPCODE_POP_COUNT 		: integer := 2;
constant OPCODE_DEC 			: integer := 16#72#;
constant OPCODE_DEC_COUNT 		: integer := 2;
constant OPCODE_DEC_IND 		: integer := 16#74#;
constant OPCODE_DEC_IND_COUNT 	: integer := 4;
constant OPCODE_BZ 				: integer := 16#80#;
constant OPCODE_ADDI 			: integer := 16#81#;
constant OPCODE_ADD  			: integer := 16#82#;
constant OPCODE_ADD_COUNT 		: integer := 2;
constant OPCODE_ADD_IND 		: integer := 16#84#;
constant OPCODE_ADD_IND_COUNT 	: integer := 4;
constant OPCODE_BN 				: integer := 16#88#;
constant OPCODE_BN_COUNT 		: integer := 8;
constant OPCODE_BNZ 			: integer := 16#90#;
constant OPCODE_ADDCI 			: integer := 16#91#;
constant OPCODE_ADDC 			: integer := 16#92#;
constant OPCODE_ADDC_COUNT 		: integer := 2;
constant OPCODE_ADDC_IND 		: integer := 16#94#;
constant OPCODE_ADDC_IND_COUNT 	: integer := 4;
constant OPCODE_RET 			: integer := 16#a0#;
constant OPCODE_SUBI 			: integer := 16#a1#;
constant OPCODE_SUB 			: integer := 16#a2#;
constant OPCODE_SUB_COUNT 		: integer := 2;
constant OPCODE_SUB_IND 		: integer := 16#a4#;
constant OPCODE_SUB_IND_COUNT 	: integer := 4;
constant OPCODE_NOT1 			: integer := 16#a8#;
constant OPCODE_NOT1_COUNT 		: integer := 8;
constant OPCODE_RETI 			: integer := 16#b0#;
constant OPCODE_SUBCI 			: integer := 16#b1#;
constant OPCODE_SUBC 			: integer := 16#b2#;
constant OPCODE_SUBC_COUNT 		: integer := 2;
constant OPCODE_SUBC_IND 		: integer := 16#b4#;
constant OPCODE_SUBC_IND_COUNT 	: integer := 4;
constant OPCODE_ROR 			: integer := 16#c0#;
constant OPCODE_LDC 			: integer := 16#c1#;
constant OPCODE_XCH 			: integer := 16#c2#;
constant OPCODE_XCH_COUNT 		: integer := 2;
constant OPCODE_XCH_IND 		: integer := 16#c4#;
constant OPCODE_XCH_IND_COUNT 	: integer := 4;
constant OPCODE_CLR1 			: integer := 16#c8#;
constant OPCODE_CLR1_COUNT 		: integer := 8;
constant OPCODE_RORC 			: integer := 16#d0#;
constant OPCODE_ORI 			: integer := 16#d1#;
constant OPCODE_OR  			: integer := 16#d2#;
constant OPCODE_OR_COUNT 		: integer := 2;
constant OPCODE_OR_IND 			: integer := 16#d4#;
constant OPCODE_OR_IND_COUNT 	: integer := 4;
constant OPCODE_ROL  			: integer := 16#e0#;
constant OPCODE_ANDI 			: integer := 16#e1#;
constant OPCODE_AND  			: integer := 16#e2#;
constant OPCODE_AND_COUNT 		: integer := 2;
constant OPCODE_AND_IND 		: integer := 16#e4#;
constant OPCODE_AND_IND_COUNT 	: integer := 4;
constant OPCODE_SET1 			: integer := 16#e8#;
constant OPCODE_SET1_COUNT 		: integer := 8;
constant OPCODE_ROLC 			: integer := 16#f0#;
constant OPCODE_XORI 			: integer := 16#f1#;
constant OPCODE_XOR  			: integer := 16#f2#;
constant OPCODE_XOR_COUNT 		: integer := 2;
constant OPCODE_XOR_IND 		: integer := 16#f4#;
constant OPCODE_XOR_IND_COUNT 	: integer := 4;

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
