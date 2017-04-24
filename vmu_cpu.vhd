library IEEE;
use IEEE.STD_LOGIC_1164.all;

package cpu is 

	type VmuCpuInstrOperands is record
		immediate 	: natural;
		direct 		: natural;
		indAddress 	: natural;
		indReg		: std_logic_vector(1 downto 0);
		bitPos 		: natural;
		absolute 	: natural;
		rel8S 		: integer;
		rel16U		: natural;
	end record;
 
	type VmuMem is array(natural range <>) of std_logic_vector(7 downto 0);

	constant VmuSfrSize : integer := 128;
	--type VmuSfrs is array(VmuSfrSize-1 downto 0) of std_logic_vector(7 downto 0);
	subtype VmuSfrs is VmuMem(VmuSfrSize-1 downto 0);

	constant VmuInstrMemSize : integer := 65536;
	subtype VmuInstrMem is VmuMem(VmuInstrMemSize-1 downto 0);

	constant VmuRomSize : integer := 65536;
	subtype VmuRom is VmuMem(VmuRomSize-1 downto 0);

	constant VmuRamBankSize : integer := 256;
	constant VmuRamBankCount : integer := 3;
	subtype VmuRamBank is VmuMem(VmuRamBankSize-1 downto 0);
	type VmuRam is array(VmuRamBankCount-1 downto 0) of VmuRamBank;

	constant VmuFlashBankSize : integer := 65536; 
	constant VmuFlashBankCount : integer := 2;
	subtype VmuFlash is VmuMem(VmuFlashBankSize*VmuFlashBankCount-1 downto 0);

	constant VmuXRamBankSize : integer := 128;
	constant VmuXRamBankCount : integer := 3;
	subtype VmuXRamBank is VmuMem(VmuXramBankSize-1 downto 0);
	type VmuXRam is array(VmuXramBankCount-1 downto 0) of VmuXramBank;

	constant VmuMemSegmentSize : integer := 128;

	constant VmuMemSegmentGp1 : integer := 0;
	constant VmuMemSegmentGp2 : integer := 1;
	constant VmuMemSegmentSfr : integer := 2;
	constant VmuMemSegmentXram : integer := 3;
	constant VmuMemSegmentCount : integer := 4;
	subtype VmuMemMap is VmuMem(VmuMemSegmentCount*VmuMemSegmentSize-1 downto 0);

end package;


library IEEE;
use IEEE.STD_LOGIC_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

use work.memBus.all;
use work.cpu.all;
use work.instrSet.all;
use work.VmuMemoryMappedRegister;
use work.memMap.all;
use work.sfr.all;

entity VmuCpu is 
	port (
		clk			: in std_logic;
		reset		: in std_logic;
		memBusIn	: inout VmuMemoryBusIn;
		memBusOut	: out VmuMemoryBusOut
	);

end entity;

architecture behav of VmuCpu is 
	signal sfr			: VmuSfrs;
	signal rom			: VmuRom;
	signal ram			: VmuRam;
	signal flash		: VmuFlash;
	signal xRam			: VmuXRam;
	signal instrMem 	: VmuInstrMem;
	signal memMap		: VmuMemMap;
	signal pc 			: natural;
	signal nextPc		: natural;
	signal instr		: std_logic_vector(23 downto 0);
	signal opcode		: natural;
	signal operands 	: VmuCpuInstrOperands;
	signal intReq		: integer;
	signal intMask		: integer;
	signal pswReg		: std_logic_vector(7 downto 0);
	signal extReg 		: std_logic_vector(7 downto 0);

	function intToMemVec(num : integer) return std_logic_vector is
	begin
		return std_logic_vector(to_signed(num, 8));
	end function;

	function uintToMemVec(num : integer) return std_logic_vector is
	begin
		return std_logic_vector(to_unsigned(num, 8));
	end function;

	function memToSfr(addr : integer) return integer is
	begin
		return addr - ADDR_SFR_OFFSET;
	end function;

begin

	--Configure data memory map and address space.
	memMap(VmuMemSegmentSize-1 downto 0) 
		<= ram(to_integer(unsigned(std_logic_vector'(""&sfr(memToSfr(ADDR_SFR_PSW))(SFR_PSW_RAMBK0_BIT)))))(VmuMemSegmentSize-1 downto 0);
	memMap((VmuMemSegmentSfr*VmuMemSegmentSize)-1 downto VmuMemSegmentGp2*VmuMemSegmentSize) 
		<= ram(to_integer(unsigned(std_logic_vector'(""&sfr(memToSfr(ADDR_SFR_PSW))(SFR_PSW_RAMBK0_BIT)))))(VmuRamBankSize-1 downto VmuMemSegmentSize);
	memMap((VmuMemSegmentXram*VmuMemSegmentSize)-1 downto VmuMemSegmentSfr*VmuMemSegmentSize) 
		<= sfr;
	memMap((VmuMemSegmentXram+1)*VmuMemSegmentSize-1 downto VmuMemSegmentXram*VmuMemSegmentSize) 
		<= xRam(to_integer(unsigned(sfr(memToSfr(ADDR_SFR_XBNK))(2 downto 0))));

	--Configure instruction memory map/source and address space (ROM when EXT is 0, Flash when 1).
	instrMem(VmuInstrMemSize-1 downto 0) <= VmuInstrMem(flash(VmuInstrMemSize-1 downto 0)) when extReg(0) = '1' else
											VmuInstrMem(rom(VmuInstrMemSize-1 downto 0));

	--Fetch instruction (3 bytes is largest instruction size, extra data ignored for smaller instructions).
	instr(23 downto 16) <= instrMem(pc);
	instr(15 downto 8) 	<= instrMem(pc+1) when pc+1 < VmuInstrMemSize else
							"00000000";
	instr(7 downto 0) <= instrMem(pc+2) when pc+2 < VmuInstrMemSize else
							"00000000";

	--Extract opcode from the first byte of the instruction
	opcode <= instrMap(to_integer(unsigned(instr(23 downto 16)))).opcode;

	--Decode instruction and fetch operands.
	process(instr) is
		variable attr 		: VmuInstrAttr;
		variable ops 		: VmuCpuInstrOperands;
		variable shInstr 	: std_logic_vector(23 downto 0);
	begin
		attr 	:= instrMap(opcode);
		shInstr := instr;

		if(attr.operands(0) = INSTR_ARG_TYPE_B3 and 
		   attr.operands(1) = INSTR_ARG_TYPE_D9 and
		   attr.operands(2) = INSTR_ARG_TYPE_R8) then
			ops.rel8S 	:= to_integer(signed(shInstr(7 downto 0)));
			ops.direct 	:= to_integer(unsigned(shInstr(20) & instr(15 downto 8)));
			ops.bitPos 	:= to_integer(unsigned(shInstr(18 downto 16)));

		elsif(attr.operands(0) = INSTR_ARG_TYPE_B3 and 
			  attr.operands(1) = INSTR_ARG_TYPE_D9 and
			  attr.operands(2) = INSTR_ARG_TYPE_NONE) then

			ops.direct 	:= to_integer(unsigned(shInstr(12) & shInstr(7 downto 0)));
			ops.bitPos 	:= to_integer(unsigned(shInstr(10 downto 8)));
		else

			for i in 0 to 2 loop 
				case attr.operands(i) is 
			    	when INSTR_ARG_TYPE_R8 => 
                		ops.rel8S := to_integer(signed(shInstr(7 downto 0)));
                		shInstr := "00000000" & shInstr(23 downto 8);
			    	when INSTR_ARG_TYPE_R16 => 
						ops.rel16U := to_integer(unsigned(std_logic_vector'(shInstr(7 downto 0) & shInstr(23 downto 15))));
              			shInstr := "0000000000000000" & shInstr(23 downto 16);
			    	when INSTR_ARG_TYPE_I8 => 
						ops.immediate := to_integer(unsigned(shInstr(7 downto 0)));
					    shInstr := "00000000" & shInstr(23 downto 8);
			    	when INSTR_ARG_TYPE_D9 => 
						ops.direct := to_integer(unsigned(shInstr(8 downto 0)));
					    shInstr := "000000000" & shInstr(23 downto 9);
			    	when INSTR_ARG_TYPE_N2 =>
						ops.indReg := shInstr(1 downto 0);
						shInstr := "00" & shInstr(23 downto 2);
			    	when INSTR_ARG_TYPE_A12 => 
						ops.absolute := to_integer(unsigned(shInstr(11 downto 0)));
						shInstr := "000000000000" & shInstr(23 downto 12);
			    	when INSTR_ARG_TYPE_A16 => 
						ops.absolute := to_integer(unsigned(shInstr(15 downto 0)));
						shInstr := "0000000000000000" & shInstr(23 downto 16);
			    	when INSTR_ARG_TYPE_B3 =>
						ops.bitPos := to_integer(unsigned(shInstr(2 downto 0)));
						shInstr := "000" & shInstr(23 downto 3);
					when others => null;
				end case;
			end loop;
		end if;

		operands <= ops;
	end process;

	--Fetch register-indirect operand.
	operands.indAddress <= to_integer(unsigned(std_logic_vector'(operands.indReg(1) & 
		memMap(to_integer(unsigned(std_logic_vector'(operands.indReg & 
			sfr(memToSfr(ADDR_SFR_PSW))(SFR_PSW_IRBK1_BIT downto SFR_PSW_IRBK0_BIT))))))));

	--Execute instruction with operands and opcode.
	process(operands, opcode) 
		variable spInt 						: integer;
		variable pendingPc					: natural;
		variable pcVector, vec16 			: std_logic_vector(15 downto 0);
		variable vec24 						: std_logic_vector(23 downto 0);
		variable vec1, vec2, vec3			: std_logic_vector(7 downto 0);
		variable op1, op2, op3, op4, cin 	: integer;
	begin
		--Initialize intermediate local variables.
		spInt 		:= to_integer(unsigned(memMap(ADDR_SFR_SP)));
		pendingPc	:= pc + instrMap(opcode).bytes;
		pcVector 	:= std_logic_vector(to_unsigned(pendingPc, 16));
		if(memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) = '1') then
			cin := 1;
		else 
			cin := 0;
		end if;

		--Execute Instruction
		case(opcode) is
			when OPCODE_NOP => null;
			when OPCODE_BR => 
				pendingPc := pendingPc + operands.rel8S;
			when OPCODE_LD => 
				memMap(ADDR_SFR_ACC) <= memMap(operands.direct);
			when OPCODE_LD_IND => 
				memMap(ADDR_SFR_ACC) <= memMap(operands.indAddress);
			when OPCODE_CALL => 
				ram(0)(spInt+1) <= pcVector(7 downto 0);
				ram(0)(spInt+2) <= pcVector(15 downto 8); 
				memMap(ADDR_SFR_SP) <= uintToMemVec(spInt+2);
				pcVector(11 downto 0) := std_logic_vector(to_unsigned(operands.absolute, 12)); 
				pendingPc := to_integer(unsigned(pcVector));
			when OPCODE_CALLR =>
				ram(0)(spInt+1) <= pcVector(7 downto 0);
				ram(0)(spInt+2) <= pcVector(15 downto 8); 
				memMap(ADDR_SFR_SP) <= uintToMemVec(spInt+2);
				pendingPc := pendingPc + (operands.rel16U mod 65536) - 1;
			when OPCODE_BRF => 
        		pendingPc := pendingPc + (operands.rel16U mod 65536) - 1;
			when OPCODE_ST => 
				memMap(operands.direct) <= memMap(ADDR_SFR_ACC);
			when OPCODE_ST_IND => 
				memMap(operands.indAddress) <= memMap(ADDR_SFR_ACC);
			when OPCODE_CALLF => 
				ram(0)(spInt+1) <= pcVector(7 downto 0);
				ram(0)(spInt+2) <= pcVector(15 downto 8); 
				memMap(ADDR_SFR_SP) <= uintToMemVec(spInt+2);
				pendingPc := operands.absolute;
			when OPCODE_JMPF => 
				pendingPc := operands.absolute;
			when OPCODE_MOV => 
				memMap(operands.direct) <= uintToMemVec(operands.immediate);
			when OPCODE_MOV_IND => 
				memMap(operands.indAddress) <= uintToMemVec(operands.immediate);
			when OPCODE_JMP => 
				pcVector(11 downto 0) := std_logic_vector(to_unsigned(operands.absolute, 12));
        		pendingPc := to_integer(unsigned(pcVector));
			when OPCODE_MUL => 
				--Convert from vector, multiply, convert back,
				vec24 := "00000000" & memMap(ADDR_SFR_ACC) & memMap(ADDR_SFR_C);
				op1 := to_integer(unsigned(memMap(ADDR_SFR_B)));
				op1 := op1 * to_integer(unsigned(vec24));
				vec24 := std_logic_vector(to_unsigned(op1, 24));

				--Write 24-bit result back as 3 separate bytes.
				memMap(ADDR_SFR_C) 		<= vec24(7 downto 0);
				memMap(ADDR_SFR_ACC) 	<= vec24(15 downto 8);
				memMap(ADDR_SFR_B) 		<= vec24(23 downto 16);

				--Update PSW
            	memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '0';
	            if(op1 > 65535) then 
	                memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '1';
	            else
	                memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '0';
	            end if;
			when OPCODE_BEI => 
				if(to_integer(unsigned(memMap(ADDR_SFR_ACC))) < operands.immediate) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '1';
				else
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '0';
					if(to_integer(unsigned(memMap(ADDR_SFR_ACC))) = operands.immediate) then
						pendingPc := pendingPc + operands.rel8S;
					end if;
				end if;
			when OPCODE_BE => 
				if(memMap(ADDR_SFR_ACC) < memMap(operands.direct)) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '1';
				else
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '0';
					if(memMap(ADDR_SFR_ACC) = memMap(operands.direct)) then
						pendingPc := pendingPc + operands.rel8S;
					end if;
				end if;
			when OPCODE_BE_IND =>
				if(memMap(ADDR_SFR_ACC) < memMap(operands.indAddress)) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '1';
				else
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '0';
					if(memMap(ADDR_SFR_ACC) = memMap(operands.indAddress)) then
						pendingPc := pendingPc + operands.rel8S;
					end if;
				end if;
			when OPCODE_DIV => 
				--Fetch operands, convert to int, divide, convert back to vector.
				op1 := to_integer(unsigned(memMap(ADDR_SFR_B)));

				if(op1 /= 0) then --protect against divide-by-zero
					vec16 		:= memMap(ADDR_SFR_ACC) & memMap(ADDR_SFR_C);
					op2			:= to_integer(unsigned(vec16));
					op3			:= op2 / op1;
					op4			:= op2 mod op1;
				else 
					op3 	:= to_integer(unsigned(std_logic_vector'("11111111" & memMap(ADDR_SFR_C))));
					op4		:= 0;
				end if;

				vec16 := std_logic_vector(to_unsigned(op3, 16));

				--Write result back as 16-bit quotient and 8-bit remainder.
				memMap(ADDR_SFR_B) 		<= uintToMemVec(op4);
				memMap(ADDR_SFR_ACC) 	<= vec16(15 downto 8);
				memMap(ADDR_SFR_C) 		<= vec16(7 downto 0);

				--Update PSW flags. Overflow active when remainder is present.
				memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '0';
				if(op4 /= 0) then 
					memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '1';
				else 
					memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '0';
				end if;
			when OPCODE_BNEI => 
				if(to_integer(unsigned(memMap(ADDR_SFR_ACC))) < operands.immediate) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '1';
				else
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '0';
				end if;

				if(to_integer(unsigned(memMap(ADDR_SFR_ACC))) /= operands.immediate) then
					pendingPc := pc + operands.rel8S;
				end if;
			when OPCODE_BNE => 
				if(to_integer(unsigned(memMap(ADDR_SFR_ACC))) < memMap(operands.direct)) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '1';
				else
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '0';
				end if;

				if(to_integer(unsigned(memMap(ADDR_SFR_ACC))) /= memMap(operands.direct)) then
					pendingPc := pendingPc + operands.rel8S;
				end if;
			when OPCODE_BNE_IND => 
				if(to_integer(unsigned(memMap(ADDR_SFR_ACC))) < memMap(operands.indAddress)) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '1';
				else
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '0';
				end if;

				if(to_integer(unsigned(memMap(ADDR_SFR_ACC))) /= memMap(operands.indAddress)) then
					pendingPc := pendingPc + operands.rel8S;
				end if;
			when OPCODE_BPC =>
				if(memMap(operands.direct)(operands.bitPos) = '1') then 
					memMap(operands.direct)(operands.bitPos) <= '0';
					pendingPc := pendingPc + operands.rel8S;
				end if; 
			when OPCODE_LDF => null;	--not implementing flash read/programming yet!
			when OPCODE_STF => null;
			when OPCODE_DBNZ => 
				op1 := to_integer(unsigned(memMap(operands.direct)))-1;
				memMap(operands.direct) <= uintToMemVec(op1);
				if(op1 /= 0) then
					pendingPc := pendingPc + operands.rel8S;
				end if;
			when OPCODE_DBNZ_IND => 
				op1 := to_integer(unsigned(memMap(operands.indAddress)))-1;
				memMap(operands.direct) <= uintToMemVec(op1);
				if(op1 /= 0) then
					pendingPc := pendingPc + operands.rel8S;
				end if;
 			when OPCODE_PUSH => 
				spInt := spInt + 1;
				ram(0)(spInt) <= memMap(operands.direct);
				memMap(ADDR_SFR_SP) <= uintToMemVec(spInt);
			when OPCODE_INC => 
				op1 := to_integer(unsigned(memMap(operands.direct))) + 1;
				memMap(operands.direct) <= uintToMemVec(op1);
			when OPCODE_INC_IND => 
				op1 := to_integer(unsigned(memMap(operands.indAddress))) + 1;
				memMap(operands.indAddress) <= uintToMemVec(op1);
			when OPCODE_BP => 
				if(memMap(operands.direct)(operands.bitPos) = '1') then
					pendingPc := pendingPc + operands.rel8S;
				end if;
			when OPCODE_POP => 
				memMap(operands.direct) <= ram(0)(spInt);
				memMap(ADDR_SFR_SP) <= uintToMemVec(spInt-1);
			when OPCODE_DEC => 
				op1 := to_integer(unsigned(memMap(operands.direct))) - 1;
				memMap(operands.direct) <= uintToMemVec(op1);
			when OPCODE_DEC_IND => 
				op1 := to_integer(unsigned(memMap(operands.indAddress))) - 1;
				memMap(operands.indAddress) <= uintToMemVec(op1);
			when OPCODE_BZ => 
				if(to_integer(unsigned(memMap(ADDR_SFR_ACC))) = 0) then 
					pendingPc := pendingPc + operands.rel8S;
				end if;
			when OPCODE_ADDI => 
				op1 := to_integer(unsigned(memMap(ADDR_SFR_ACC)));
				op2 := operands.immediate;
				op3 := op1 + op2;	
				memMap(ADDR_SFR_ACC) <= uintToMemVec(op3);
				
				--Carry
				if(op3 > 255) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '1';
				else 
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '0';
				end if;

				--Overflow
				vec1 := uintToMemVec(op1);
				vec2 := uintToMemVec(op2);
				
				vec3 := (not vec1 xor vec2) and (vec2 xor uintToMemVec(op3));
				if(vec3(7) = '1') then
					memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '1';
				else
					memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '0';
				end if;

				--Aux carry
				op1 := op1 mod 16;
				op2 := op2 mod 16;
				op3 := op1 + op2;
				if(op3 > 15) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_AC_BIT) <= '1';
				else 
					memMap(ADDR_SFR_PSW)(SFR_PSW_AC_BIT) <= '0';
				end if;
			when OPCODE_ADD => 
				op1 := to_integer(unsigned(memMap(ADDR_SFR_ACC)));
				op2 := to_integer(unsigned(memMap(operands.direct)));
				op3 := op1 + op2;	
				memMap(ADDR_SFR_ACC) <= uintToMemVec(op3);
				
				--Carry
				if(op3 > 255) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '1';
				else 
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '0';
				end if;

				--Overflow
				vec1 := uintToMemVec(op1);
				vec2 := uintToMemVec(op2);
				
				vec3 := (not vec1 xor vec2) and (vec2 xor uintToMemVec(op3));
				if(vec3(7) = '1') then
					memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '1';
				else
					memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '0';
				end if;

				--Aux carry
				op1 := op1 mod 16;
				op2 := op2 mod 16;
				op3 := op1 + op2;
				if(op3 > 15) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_AC_BIT) <= '1';
				else 
					memMap(ADDR_SFR_PSW)(SFR_PSW_AC_BIT) <= '0';
				end if;
			when OPCODE_ADD_IND => 
				op1 := to_integer(unsigned(memMap(ADDR_SFR_ACC)));
				op2 := to_integer(unsigned(memMap(operands.indAddress)));
				op3 := op1 + op2;	
				memMap(ADDR_SFR_ACC) <= uintToMemVec(op3);
				
				--Carry
				if(op3 > 255) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '1';
				else 
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '0';
				end if;

				--Overflow
				vec1 := uintToMemVec(op1);
				vec2 := uintToMemVec(op2);
				
				vec3 := (not vec1 xor vec2) and (vec2 xor uintToMemVec(op3));
				if(vec3(7) = '1') then
					memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '1';
				else
					memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '0';
				end if;

				--Aux carry
				op1 := op1 mod 16;
				op2 := op2 mod 16;
				op3 := op1 + op2;
				if(op3 > 15) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_AC_BIT) <= '1';
				else 
					memMap(ADDR_SFR_PSW)(SFR_PSW_AC_BIT) <= '0';
				end if;
			when OPCODE_BN => 
				if(memMap(operands.direct)(operands.bitPos) = '0') then
					pendingPc := pendingPc + operands.rel8S;
				end if;
			when OPCODE_BNZ => 
				if(to_integer(unsigned(memMap(ADDR_SFR_ACC))) /= 0) then
					pendingPc := pendingPc + operands.rel8S;	
				end if;
			when OPCODE_ADDCI => 
				op1 := to_integer(unsigned(memMap(ADDR_SFR_ACC)));
				op2 := operands.immediate + cin;
				op3 := op1 + op2;	
				memMap(ADDR_SFR_ACC) <= uintToMemVec(op3);
				
				--Carry
				if(op3 > 255) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '1';
				else 
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '0';
				end if;

				--Overflow
				vec1 := uintToMemVec(op1);
				vec2 := uintToMemVec(op2);
				
				vec3 := (not vec1 xor vec2) and (vec2 xor uintToMemVec(op3));
				if(vec3(7) = '1') then
					memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '1';
				else
					memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '0';
				end if;

				--Aux carry
				op1 := op1 mod 16;
				op2 := op2 mod 16;
				op3 := op1 + op2;
				if(op3 > 15) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_AC_BIT) <= '1';
				else 
					memMap(ADDR_SFR_PSW)(SFR_PSW_AC_BIT) <= '0';
				end if;
			when OPCODE_ADDC => 
				op1 := to_integer(unsigned(memMap(ADDR_SFR_ACC)));
				op2 := to_integer(unsigned(memMap(operands.direct))) + cin;
				op3 := op1 + op2;	
				memMap(ADDR_SFR_ACC) <= uintToMemVec(op3);
				
				--Carry
				if(op3 > 255) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '1';
				else 
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '0';
				end if;

				--Overflow
				vec1 := uintToMemVec(op1);
				vec2 := uintToMemVec(op2);
				
				vec3 := (not vec1 xor vec2) and (vec2 xor uintToMemVec(op3));
				if(vec3(7) = '1') then
					memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '1';
				else
					memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '0';
				end if;

				--Aux carry
				op1 := op1 mod 16;
				op2 := op2 mod 16;
				op3 := op1 + op2;
				if(op3 > 15) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_AC_BIT) <= '1';
				else 
					memMap(ADDR_SFR_PSW)(SFR_PSW_AC_BIT) <= '0';
				end if;
			when OPCODE_ADDC_IND => 
				op1 := to_integer(unsigned(memMap(ADDR_SFR_ACC)));
				op2 := to_integer(unsigned(memMap(operands.indAddress)))+1;
				op3 := op1 + op2;	
				memMap(ADDR_SFR_ACC) <= uintToMemVec(op3);
				
				--Carry
				if(op3 > 255) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '1';
				else 
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '0';
				end if;

				--Overflow
				vec1 := uintToMemVec(op1);
				vec2 := uintToMemVec(op2);
				
				vec3 := (not vec1 xor vec2) and (vec2 xor uintToMemVec(op3));
				if(vec3(7) = '1') then
					memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '1';
				else
					memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '0';
				end if;

				--Aux carry
				op1 := op1 mod 16;
				op2 := op2 mod 16;
				op3 := op1 + op2;
				if(op3 > 15) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_AC_BIT) <= '1';
				else 
					memMap(ADDR_SFR_PSW)(SFR_PSW_AC_BIT) <= '0';
				end if;
			when OPCODE_RET => 
				pcVector := ram(0)(spInt) & ram(0)(spInt-1);
				memMap(ADDR_SFR_SP) <= uintToMemVec(spInt-2);
				pendingPc := to_integer(unsigned(pcVector));
			when OPCODE_SUBI => 
				op1 := to_integer(unsigned(memMap(ADDR_SFR_ACC)));
				op2 := operands.immediate;
				op3 := op1 - op2;

				memMap(ADDR_SFR_ACC) <= uintToMemVec(op3);

				--Carry
				if(op3 < 0) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '1';
				else 
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '0';
				end if;

				--Overflow
				if(op2 < 1) then
					if(255 + op2 >= op1) then
						memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '1';
					else 
						memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '0';
					end if;

				else
					if(op2 <= op1) then
						memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '1';
					else
						memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '0';
					end if;

				end if;

				--Aux carry
				op1 := op1 mod 16;
				op2 := op2 mod 16;
				op3 := op1 - op2;
				if(op3 < 0) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_AC_BIT) <= '1';
				else 
					memMap(ADDR_SFR_PSW)(SFR_PSW_AC_BIT) <= '0';
				end if;
			when OPCODE_SUB => 
				op1 := to_integer(unsigned(memMap(ADDR_SFR_ACC)));
				op2 := to_integer(unsigned(memMap(operands.direct)));
				op3 := op1 - op2;

				memMap(ADDR_SFR_ACC) <= uintToMemVec(op3);

				--Carry
				if(op3 < 0) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '1';
				else 
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '0';
				end if;

				--Overflow
				if(op2 < 1) then
					if(255 + op2 >= op1) then
						memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '1';
					else 
						memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '0';
					end if;

				else
					if(op2 <= op1) then
						memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '1';
					else
						memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '0';
					end if;

				end if;

				--Aux carry
				op1 := op1 mod 16;
				op2 := op2 mod 16;
				op3 := op1 - op2;
				if(op3 < 0) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_AC_BIT) <= '1';
				else 
					memMap(ADDR_SFR_PSW)(SFR_PSW_AC_BIT) <= '0';
				end if;
			when OPCODE_SUB_IND => 
				op1 := to_integer(unsigned(memMap(ADDR_SFR_ACC)));
				op2 := to_integer(unsigned(memMap(operands.indAddress)));
				op3 := op1 - op2;

				memMap(ADDR_SFR_ACC) <= uintToMemVec(op3);

				--Carry
				if(op3 < 0) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '1';
				else 
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '0';
				end if;

				--Overflow
				if(op2 < 1) then
					if(255 + op2 >= op1) then
						memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '1';
					else 
						memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '0';
					end if;

				else
					if(op2 <= op1) then
						memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '1';
					else
						memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '0';
					end if;

				end if;

				--Aux carry
				op1 := op1 mod 16;
				op2 := op2 mod 16;
				op3 := op1 - op2;
				if(op3 < 0) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_AC_BIT) <= '1';
				else 
					memMap(ADDR_SFR_PSW)(SFR_PSW_AC_BIT) <= '0';
				end if;
			when OPCODE_NOT1 => 
				memMap(operands.direct)(operands.bitPos) <= not memMap(operands.direct)(operands.bitPos); 
   			when OPCODE_RETI => 
				pcVector := ram(0)(spInt) & ram(0)(spInt-1);
				memMap(ADDR_SFR_SP) <= uintToMemVec(spInt-2);
				pendingPc := to_integer(unsigned(pcVector));
				intMask <= intMask - 1;
			when OPCODE_SUBCI =>
				op1 := to_integer(unsigned(memMap(ADDR_SFR_ACC)));
				op2 := operands.immediate;
				op3 := op1 - op2 - cin;

				memMap(ADDR_SFR_ACC) <= uintToMemVec(op3);

				--Carry
				if(op3 < 0) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '1';
				else 
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '0';
				end if;

				--Overflow
				if(op2 < 1) then
					if(255 + op2 >= op1) then
						memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '1';
					else 
						memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '0';
					end if;

				else
					if(op2 <= op1) then
						memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '1';
					else
						memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '0';
					end if;

				end if;

				--Aux carry
				op1 := op1 mod 16;
				op2 := op2 mod 16;
				op3 := op1 - op2;
				if(op3 < 0) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_AC_BIT) <= '1';
				else 
					memMap(ADDR_SFR_PSW)(SFR_PSW_AC_BIT) <= '0';
				end if;
			when OPCODE_SUBC => 
				op1 := to_integer(unsigned(memMap(ADDR_SFR_ACC)));
				op2 := to_integer(unsigned(memMap(operands.direct)));
				op3 := op1 - op2 - cin;

				memMap(ADDR_SFR_ACC) <= uintToMemVec(op3);

				--Carry
				if(op3 < 0) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '1';
				else 
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '0';
				end if;

				--Overflow
				if(op2 < 1) then
					if(255 + op2 >= op1) then
						memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '1';
					else 
						memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '0';
					end if;

				else
					if(op2 <= op1) then
						memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '1';
					else
						memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '0';
					end if;

				end if;

				--Aux carry
				op1 := op1 mod 16;
				op2 := op2 mod 16;
				op3 := op1 - op2;
				if(op3 < 0) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_AC_BIT) <= '1';
				else 
					memMap(ADDR_SFR_PSW)(SFR_PSW_AC_BIT) <= '0';
				end if;
			when OPCODE_SUBC_IND => 
				op1 := to_integer(unsigned(memMap(ADDR_SFR_ACC)));
				op2 := to_integer(unsigned(memMap(operands.indAddress)));
				op3 := op1 - op2 - cin;

				memMap(ADDR_SFR_ACC) <= uintToMemVec(op3);

				--Carry
				if(op3 < 0) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '1';
				else 
					memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= '0';
				end if;

				--Overflow
				if(op2 < 1) then
					if(255 + op2 >= op1) then
						memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '1';
					else 
						memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '0';
					end if;

				else
					if(op2 <= op1) then
						memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '1';
					else
						memMap(ADDR_SFR_PSW)(SFR_PSW_OV_BIT) <= '0';
					end if;

				end if;

				--Aux carry
				op1 := op1 mod 16;
				op2 := op2 mod 16;
				op3 := op1 - op2;
				if(op3 < 0) then
					memMap(ADDR_SFR_PSW)(SFR_PSW_AC_BIT) <= '1';
				else 
					memMap(ADDR_SFR_PSW)(SFR_PSW_AC_BIT) <= '0';
				end if;
			when OPCODE_ROR => 
				memMap(ADDR_SFR_ACC)(7 downto 0) <= memMap(ADDR_SFR_ACC)(0) & memMap(ADDR_SFR_ACC)(7 downto 1);
			when OPCODE_LDC => 
				op1 := to_integer(unsigned(memMap(ADDR_SFR_ACC)));
				vec16 := memMap(ADDR_SFR_TRH) & memMap(ADDR_SFR_TRL);
				op2 := to_integer(unsigned(vec16));
				op3 := op1 + op2;
				memMap(ADDR_SFR_ACC) <= instrMem(op3);
			when OPCODE_XCH => 
				memMap(ADDR_SFR_ACC) <= memMap(operands.direct);
				memMap(operands.direct) <= memMap(ADDR_SFR_ACC);
			when OPCODE_XCH_IND => 
				memMap(ADDR_SFR_ACC) <= memMap(operands.direct);
				memMap(operands.indAddress) <= memMap(ADDR_SFR_ACC);
			when OPCODE_CLR1 => 
				memMap(operands.direct)(operands.bitPos) <= '0';
			when OPCODE_RORC => 
				memMap(ADDR_SFR_ACC) <= memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) & memMap(ADDR_SFR_ACC)(7 downto 1);
				memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= memMap(ADDR_SFR_ACC)(0);
			when OPCODE_ORI => 	
				memMap(ADDR_SFR_ACC) <= memMap(ADDR_SFR_ACC) or uintToMemVec(operands.immediate);
			when OPCODE_OR => 
				memMap(ADDR_SFR_ACC) <= memMap(ADDR_SFR_ACC) or memMap(operands.direct);
			when OPCODE_OR_IND =>
				memMap(ADDR_SFR_ACC) <= memMap(ADDR_SFR_ACC) or memMap(operands.indAddress);
			when OPCODE_ROL =>
				memMap(ADDR_SFR_ACC) <= memMap(ADDR_SFR_ACC)(6 downto 0) & memMap(ADDR_SFR_ACC)(7);
			when OPCODE_ANDI => 
				memMap(ADDR_SFR_ACC) <= memMap(ADDR_SFR_ACC) and uintToMemVec(operands.immediate);
			when OPCODE_AND => 
				memMap(ADDR_SFR_ACC) <= memMap(ADDR_SFR_ACC) and memMap(operands.direct);
			when OPCODE_AND_IND => 
				memMap(ADDR_SFR_ACC) <= memMap(ADDR_SFR_ACC) and memMap(operands.indAddress);
			when OPCODE_SET1 => 
				memMap(operands.direct)(operands.bitPos) <= '1';
			when OPCODE_ROLC => 
				memMap(ADDR_SFR_ACC) <= memMap(ADDR_SFR_ACC)(6 downto 0) & memMap(ADDR_SFR_ACC)(7);
				memMap(ADDR_SFR_PSW)(SFR_PSW_CY_BIT) <= memMap(ADDR_SFR_ACC)(7);
			when OPCODE_XORI => 
				memMap(ADDR_SFR_ACC) <= memMap(ADDR_SFR_ACC) xor uintToMemVec(operands.immediate);
			when OPCODE_XOR => 
				memMap(ADDR_SFR_ACC) <= memMap(ADDR_SFR_ACC) xor memMap(operands.direct);
			when OPCODE_XOR_IND => 
				memMap(ADDR_SFR_ACC) <= memMap(ADDR_SFR_ACC) xor memMap(operands.indAddress);
			when others => null;
		end case;

		--Commit new PC value for next clock cycle.
		nextPc <= pendingPc;

		--Handle firmware calls

	end process;


	--Trigger next processor cycle by setting the new PC value.
	--Handle reset/CPU and register initialization.
	process(clk, reset) is
	begin
		if(clk = '1' and clk'event) then
		
			--Initialize CPU and registers to starting state
			if(reset = '1') then 
				--Reset PC
				pc <= 0;

				--Initialize SFRs
				--Fetch instructions from Flash
				sfr(memToSfr(ADDR_SFR_EXT))(1) 	<= '1'; 
				--Use RAM bank #1.
				sfr(memToSfr(ADDR_SFR_PSW))(SFR_PSW_RAMBK0_BIT) <= '1'; 
				--Disable low battery signal
				sfr(memToSfr(ADDR_SFR_P7))(1) 	<= '1'; 
				--Initialize Stack Pointer
				sfr(memToSfr(ADDR_SFR_SP)) 		<= uintToMemVec(ADDR_STACK_BEGIN-1); 
				--Clear out all controller input pins
				sfr(memToSfr(ADDR_SFR_P3))		<= "11111111"; 
			else
				pc <= nextPc;
			end if;		


		end if;
	end process;

end architecture;