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
	constant OPCODE_BRF 			: integer := 16#11#;
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

	type VmuInstrAttr is record
		opcode		: natural;
		operands	: VmuInstrArgTypes(2 downto 0);
		opBits		: integer;
		bytes		: integer;
		cycles		: integer;
	end record;

	type VmuInstrMap is array (0 to 255) of VmuInstrAttr;


	constant instrMap : VmuInstrMap := (
		OPCODE_NOP => (
			OPCODE_NOP,
			( others => INSTR_ARG_TYPE_NONE ),
			8,
			1,
			1			
		),
		OPCODE_BR => (
			OPCODE_BR,
			( 0 => INSTR_ARG_TYPE_R8, others => INSTR_ARG_TYPE_NONE ),
			8,
			2,
			2
		), 
		OPCODE_LD to OPCODE_LD+OPCODE_LD_COUNT-1=> (
			OPCODE_LD,
			( 0 => INSTR_ARG_TYPE_D9, others => INSTR_ARG_TYPE_NONE ),
			7,
			2,
			1
		),
		OPCODE_LD_IND to OPCODE_LD_IND+OPCODE_LD_IND_COUNT-1 => (
			OPCODE_LD_IND,
			( 0 => INSTR_ARG_TYPE_N2, others => INSTR_ARG_TYPE_NONE ),
			6,
			1,
			1
		), 
		OPCODE_CALL to OPCODE_CALL + 16#7# | OPCODE_CALL + 16#10# to OPCODE_CALL + 16#17# => (
			OPCODE_CALL,
			( 0 => INSTR_ARG_TYPE_A12, others => INSTR_ARG_TYPE_NONE ),
			5,
			2,
			2
		),
		OPCODE_CALLR => (
			OPCODE_CALLR,
			( 0 => INSTR_ARG_TYPE_R16, others => INSTR_ARG_TYPE_NONE ),
			8,
			3,
			4
		),
		OPCODE_BRF => (
			OPCODE_BRF,
			( 0 => INSTR_ARG_TYPE_R16, others => INSTR_ARG_TYPE_NONE ),
			8,
			3,
			4
		),
		OPCODE_ST to OPCODE_ST + OPCODE_ST_COUNT-1 => (
			OPCODE_ST,
			( 0 => INSTR_ARG_TYPE_D9, others => INSTR_ARG_TYPE_NONE ),
			7,
			2,
			1
		),
		OPCODE_ST_IND to OPCODE_ST_IND + OPCODE_ST_IND_COUNT-1 => (
			OPCODE_ST_IND,
			( 0 => INSTR_ARG_TYPE_N2, others => INSTR_ARG_TYPE_NONE ),
			6,
			1,
			1
		),
		OPCODE_CALLF => (
			OPCODE_CALLF,
			( 0 => INSTR_ARG_TYPE_A16, others => INSTR_ARG_TYPE_NONE ),
			8,
			3,
			2
		),
		OPCODE_JMPF => (
			OPCODE_JMPF,
			( 0 => INSTR_ARG_TYPE_A16, others => INSTR_ARG_TYPE_NONE ),
			8,
			3,
			2
		),
		OPCODE_MOV to OPCODE_MOV+OPCODE_MOV_COUNT-1 => (
			OPCODE_MOV,
			(INSTR_ARG_TYPE_D9, INSTR_ARG_TYPE_I8, INSTR_ARG_TYPE_NONE),
			7,
			3,
			2
		),
		OPCODE_MOV_IND to OPCODE_MOV_IND+OPCODE_MOV_IND_COUNT-1 => (
			OPCODE_MOV_IND,
			(INSTR_ARG_TYPE_N2, INSTR_ARG_TYPE_I8, INSTR_ARG_TYPE_NONE),
			6,
			2,
			1
		),
		OPCODE_JMP to OPCODE_JMP + 16#7# | OPCODE_JMP + 16#10# to OPCODE_JMP + 16#17# => (
			OPCODE_JMP,
			(INSTR_ARG_TYPE_A12, others => INSTR_ARG_TYPE_NONE),
			5,
			2,
			2
		),
		OPCODE_MUL => (
			OPCODE_MUL,
			(others => INSTR_ARG_TYPE_NONE),
			8,
			1,
			7
		),
		OPCODE_BEI => (
			OPCODE_BEI,
			(INSTR_ARG_TYPE_I8, INSTR_ARG_TYPE_R8, INSTR_ARG_TYPE_NONE),
			8, 
			3,
			2
		),
		OPCODE_BE to OPCODE_BE + OPCODE_BE_COUNT-1 => (
			OPCODE_BE,
			(INSTR_ARG_TYPE_D9, INSTR_ARG_TYPE_R8, INSTR_ARG_TYPE_NONE),
			7,
			3,
			2
		),
		OPCODE_BE_IND to OPCODE_BE_IND + OPCODE_BE_IND_COUNT-1 => (
			OPCODE_BE_IND,
			(INSTR_ARG_TYPE_N2, INSTR_ARG_TYPE_I8, INSTR_ARG_TYPE_R8),
			6,
			3,
			2
		),
		OPCODE_DIV => (
			OPCODE_DIV,
			(others => INSTR_ARG_TYPE_NONE),
			8,
			1,
			7
		),
		OPCODE_BNEI => (
			OPCODE_BNEI,
			(INSTR_ARG_TYPE_I8, INSTR_ARG_TYPE_R8, INSTR_ARG_TYPE_NONE),
			8,
			3,
			2
		),
		OPCODE_BNE to OPCODE_BNE + OPCODE_BNE_COUNT-1 => (
			OPCODE_BNE,
			(INSTR_ARG_TYPE_D9, INSTR_ARG_TYPE_R8, INSTR_ARG_TYPE_NONE),
			7,
			3,
			2
		),
		OPCODE_BNE_IND to OPCODE_BNE_IND + OPCODE_BNE_IND_COUNT-1 => (
			OPCODE_BNE_IND,
			(INSTR_ARG_TYPE_N2, INSTR_ARG_TYPE_I8, INSTR_ARG_TYPE_R8),
			6,
			3,
			2
		),
    	OPCODE_BPC to OPCODE_BPC + 16#7# |  OPCODE_BPC + 16#10# to OPCODE_BPC + 16#17# => (
        	OPCODE_BPC,
	        (INSTR_ARG_TYPE_B3, INSTR_ARG_TYPE_D9, INSTR_ARG_TYPE_R8),
			5,
	        3,
	        2
    	),
    	OPCODE_LDF => (
        	OPCODE_LDF,
        	(others => INSTR_ARG_TYPE_NONE),
			8,
        	1,
        	2
    	),
    	OPCODE_STF => (
	    	OPCODE_STF,
	        (others => INSTR_ARG_TYPE_NONE),
	        8,
			1,
	        2
    	),
    	OPCODE_DBNZ to OPCODE_DBNZ+OPCODE_DBNZ_COUNT-1 => (
	        OPCODE_DBNZ,
	        (INSTR_ARG_TYPE_D9, INSTR_ARG_TYPE_R8, INSTR_ARG_TYPE_NONE),
	        7,
			3,
	        2
	    ),
    	OPCODE_DBNZ_IND to OPCODE_DBNZ_IND+OPCODE_DBNZ_IND_COUNT-1 => (
      		OPCODE_DBNZ_IND,
	        (INSTR_ARG_TYPE_N2, INSTR_ARG_TYPE_R8, INSTR_ARG_TYPE_NONE),
	        6,
			2,
	        2
	    ),
    	OPCODE_PUSH to OPCODE_PUSH+OPCODE_PUSH_COUNT-1 => (
	        OPCODE_PUSH,
	        ( 0 => INSTR_ARG_TYPE_D9, others => INSTR_ARG_TYPE_NONE),
	        7,
			2,
	        2
	    ),
    	OPCODE_INC to OPCODE_INC+OPCODE_INC_COUNT-1 => (
	        OPCODE_INC,
	        ( 0 => INSTR_ARG_TYPE_D9, others => INSTR_ARG_TYPE_NONE ),
	        7,
			2,
	        1
	    ),
    	OPCODE_INC_IND to OPCODE_INC_IND+OPCODE_INC_IND_COUNT-1 => (
	        OPCODE_INC_IND,
	        (0 => INSTR_ARG_TYPE_N2, others => INSTR_ARG_TYPE_NONE),
	        6,
			1,
	        1
	    ),
    	OPCODE_BP to OPCODE_BP + 16#7# | OPCODE_BP + 16#10# to OPCODE_BP + 16#17# => (
	       	OPCODE_BP,
	        (INSTR_ARG_TYPE_B3, INSTR_ARG_TYPE_D9, INSTR_ARG_TYPE_R8),
			5,        
			3,
	        2
	    ),
    	OPCODE_POP to OPCODE_POP+OPCODE_POP_COUNT-1 => (
			OPCODE_POP,
	        (0 => INSTR_ARG_TYPE_D9, others => INSTR_ARG_TYPE_NONE),
	        7,
			2,
	        2
	    ),
    	OPCODE_DEC to OPCODE_DEC+OPCODE_DEC_COUNT-1 => (
			OPCODE_DEC,
	        (0 => INSTR_ARG_TYPE_D9, others => INSTR_ARG_TYPE_NONE),
	        7,
			2,
	        1
	    ),
    	OPCODE_DEC_IND to OPCODE_DEC_IND+OPCODE_DEC_IND_COUNT-1 => (
			OPCODE_DEC_IND,
	        (0 => INSTR_ARG_TYPE_N2, others => INSTR_ARG_TYPE_NONE),
	        6,
			1,
	        1
	    ),
    	OPCODE_BZ => (
	       	OPCODE_BZ,
	        (0 => INSTR_ARG_TYPE_R8, others => INSTR_ARG_TYPE_NONE),
	        8,
			2,
	        2
	    ),
    	OPCODE_ADDI => (
			OPCODE_ADDI,
	        (0 => INSTR_ARG_TYPE_I8, others => INSTR_ARG_TYPE_NONE),
	        8,
			2,
	        1
	    ),
	    OPCODE_ADD to OPCODE_ADD + OPCODE_ADD_COUNT-1 => (
	        OPCODE_ADD,
	       	(0 => INSTR_ARG_TYPE_D9, others => INSTR_ARG_TYPE_NONE),
			7,
	        2,
	        1
	    ),
    	OPCODE_ADD_IND to OPCODE_ADD_IND+OPCODE_ADD_IND_COUNT-1 => (
			OPCODE_ADD_IND,
	        (0 => INSTR_ARG_TYPE_N2, others => INSTR_ARG_TYPE_NONE),
	        6,
			1,
	        1
	    ),
    	OPCODE_BN to OPCODE_BN + 16#7# | OPCODE_BN + 16#10# to OPCODE_BN + 16#17# => (
			OPCODE_BN,
	        (INSTR_ARG_TYPE_B3, INSTR_ARG_TYPE_D9, INSTR_ARG_TYPE_R8),
	        5,
			3,
	        2
	    ),
    	OPCODE_BNZ => (
	       	OPCODE_BNZ,
	        (0 => INSTR_ARG_TYPE_R8, others => INSTR_ARG_TYPE_NONE),
	        8,
			2,
	        2
	    ),
	    OPCODE_ADDCI => (
	        OPCODE_ADDCI,
	        (0 => INSTR_ARG_TYPE_I8, others => INSTR_ARG_TYPE_NONE),
	        8,
	   		2,
	        1
	    ),
	    OPCODE_ADDC to OPCODE_ADDC+OPCODE_ADDC_COUNT-1 => (
	        OPCODE_ADDC,
	        (0 => INSTR_ARG_TYPE_D9, others => INSTR_ARG_TYPE_NONE),
	        7,
			2,
	        1
	    ),
	    OPCODE_ADDC_IND to OPCODE_ADDC_IND+OPCODE_ADDC_IND_COUNT-1 => (
	        OPCODE_ADDC_IND,
	        (0 => INSTR_ARG_TYPE_N2, others => INSTR_ARG_TYPE_NONE),
	        6,
			1,
	        1
	    ),
	    OPCODE_RET => (
	        OPCODE_RET,
	        (others => INSTR_ARG_TYPE_NONE),
	        8,
			1,
	        2
	    ),
    	OPCODE_SUBI => (
	        OPCODE_SUBI,
	        (0 => INSTR_ARG_TYPE_I8, others => INSTR_ARG_TYPE_NONE),
	        8,
			2,
	        1
	    ),
	    OPCODE_SUB to OPCODE_SUB+OPCODE_SUB_COUNT-1 => (
			OPCODE_SUB,
	        (0 => INSTR_ARG_TYPE_D9, others => INSTR_ARG_TYPE_NONE),
	        7,
			2,
	        1
	    ),
	    OPCODE_SUB_IND to OPCODE_SUB_IND+OPCODE_SUB_IND_COUNT-1 => (
			OPCODE_SUB_IND,
	        (0 => INSTR_ARG_TYPE_N2, others => INSTR_ARG_TYPE_NONE),
	        6,
			1,
	        1
	    ),
    	OPCODE_NOT1 to OPCODE_NOT1 + 16#7# | OPCODE_NOT1 + 16#10# to OPCODE_NOT1 + 16#17# => (
	 		OPCODE_NOT1,
	        (INSTR_ARG_TYPE_B3, INSTR_ARG_TYPE_D9, INSTR_ARG_TYPE_NONE),
	        5,
			2,
	        1
	    ),
    	OPCODE_RETI => (
			OPCODE_RETI,
	        (others => INSTR_ARG_TYPE_NONE),
	        8,
			1,
	        2
	    ),
    	OPCODE_SUBCI => (
	        OPCODE_SUBCI,
	        (0 => INSTR_ARG_TYPE_I8, others => INSTR_ARG_TYPE_NONE),
	        8,
			2,
	        1
	    ),
    	OPCODE_SUBC to OPCODE_SUBC+OPCODE_SUBC_COUNT-1 => (
			OPCODE_SUBC,
	        (0 => INSTR_ARG_TYPE_D9, others => INSTR_ARG_TYPE_NONE),
	        7,
			2,
	        1
	    ),
    	OPCODE_SUBC_IND to OPCODE_SUBC_IND+OPCODE_SUBC_IND_COUNT-1 => (
			OPCODE_SUBC_IND,
			(0 => INSTR_ARG_TYPE_N2, others => INSTR_ARG_TYPE_NONE),
			6,
	        1,
	        1
	    ),
	    OPCODE_ROR => (
	        OPCODE_ROR,
	        (others => INSTR_ARG_TYPE_NONE),
	        8,
			1,
	        1
	    ),
	    OPCODE_LDC => (
			OPCODE_LDC,
	        (others => INSTR_ARG_TYPE_NONE),
	        8,
			1,
	        2
	    ),
    	OPCODE_XCH to OPCODE_XCH+OPCODE_XCH_COUNT-1 => (
			OPCODE_XCH,
	        (0 => INSTR_ARG_TYPE_D9, others => INSTR_ARG_TYPE_NONE),
	        7,
			2,
	        1
	    ),
	    OPCODE_XCH_IND to OPCODE_XCH_IND+OPCODE_XCH_IND_COUNT-1 => (
			OPCODE_XCH_IND,
	        (0 => INSTR_ARG_TYPE_N2, others => INSTR_ARG_TYPE_NONE),
	        6,
	   		1,
	        1
	    ),
	    OPCODE_CLR1 to OPCODE_CLR1 + 16#7# | OPCODE_CLR1 + 16#10# to OPCODE_CLR1 + 16#17# => (
	        OPCODE_CLR1,
	        (INSTR_ARG_TYPE_B3, INSTR_ARG_TYPE_D9, INSTR_ARG_TYPE_NONE),
	        5,
	        2,
	        1
	    ),
	    OPCODE_RORC => (
	        OPCODE_RORC,
	        (others => INSTR_ARG_TYPE_NONE),
	        8,
			1,
	        1
	    ),
	    OPCODE_ORI => (
			OPCODE_ORI,
	        (0 => INSTR_ARG_TYPE_I8, others => INSTR_ARG_TYPE_NONE),
			8,
	        2,
	        1
	    ),
	    OPCODE_OR to OPCODE_OR+OPCODE_OR_COUNT-1 => (
			OPCODE_OR,
	        (0 => INSTR_ARG_TYPE_D9, others => INSTR_ARG_TYPE_NONE),
			7,
	        2,
	        1
	    ),
	    OPCODE_OR_IND to OPCODE_OR_IND+OPCODE_OR_IND_COUNT-1 => (
			OPCODE_OR_IND,
	        (0 => INSTR_ARG_TYPE_N2, others => INSTR_ARG_TYPE_NONE),
			6,
	        1,
	        1
	    ),
	    OPCODE_ROL => (
			OPCODE_ROL,
	        (others => INSTR_ARG_TYPE_NONE),
	     	8,
		   	1,
	        1
	    ),
	    OPCODE_ANDI => (
	        OPCODE_ANDI,
	        (0 => INSTR_ARG_TYPE_I8, others => INSTR_ARG_TYPE_NONE),
			8,
	        2,
	        1
		),
	    OPCODE_AND to OPCODE_AND+OPCODE_AND_COUNT-1 => (
			OPCODE_AND,
	        (0 => INSTR_ARG_TYPE_D9, others => INSTR_ARG_TYPE_NONE),
			7,
	        2,
	        1
	    ),
	    OPCODE_AND_IND to OPCODE_AND_IND+OPCODE_AND_IND_COUNT-1 => (
			OPCODE_AND_IND,
	        (0 => INSTR_ARG_TYPE_N2, others => INSTR_ARG_TYPE_NONE),
			6,
	        1,
	        1
		),
	    OPCODE_SET1 to OPCODE_SET1 + 16#7# | OPCODE_SET1 + 16#10# to OPCODE_SET1 + 16#17# => (
	        OPCODE_SET1,
	        (INSTR_ARG_TYPE_B3, INSTR_ARG_TYPE_D9, INSTR_ARG_TYPE_NONE),
			5,
	        2,
	        1
	    ),
	    OPCODE_ROLC => (
			OPCODE_ROLC,
	        (others => INSTR_ARG_TYPE_NONE),
			8,
	        1,
	        1
	    ),
	    OPCODE_XORI => (
			OPCODE_XORI,
	        (0 => INSTR_ARG_TYPE_I8, others => INSTR_ARG_TYPE_NONE),
			8,
	        2,
	        1
	    ),
    	OPCODE_XOR to OPCODE_XOR+OPCODE_XOR_COUNT-1 => (
			OPCODE_XOR,
	        (0 => INSTR_ARG_TYPE_D9, others => INSTR_ARG_TYPE_NONE),
			7,
	        2,
	        1
	    ),
	    OPCODE_XOR_IND to OPCODE_XOR_IND+OPCODE_XOR_IND_COUNT-1 => (
			OPCODE_XOR_IND,
	        (0 => INSTR_ARG_TYPE_N2, others => INSTR_ARG_TYPE_NONE),
			6,
	        1,
	        1
	    )
	);

end package;
