library IEEE;
use IEEE.STD_LOGIC_1164.all;

use work.memBus.all;
use work.VmuMemoryMappedRegister;

type VmuCpuInstrOperands is record
	immediate 	: unsigned;
	direct 		: std_logic_vector(8 downto 0);
	indOffset 	: std_logic_vector(2 downto 0);
	indValue 	: std_logic_vector(7 downto 0);
	offset 		: std_logic(2 downto 0);
	absolute 	: std_logic(12 downto 0);
	relative 	: signed;
end record;

function VmuCpuInstrDecodeOperands(instr: std_logic_vector(23 downto 0)) return VmuCpuInstrOperands is 

begin 

end function;

entity VmuCpu is 
	port (
		clk			: in std_logic;
		reset		: in std_logic;
		memBusIn	: inout VmuMemoryBusIn;
		memBusOut	: out 	VmuMemoryBusOut;
		flashBusIn	: inout VmuFlashBusIn;
		flashBusOut	: out	VmuFlashBusOut;
	);

end entity;

architecture behav of VmuCpu is 
	signal pc 		: std_vector(8 downto 0) := "00000000";
	signal intReq	: integer := 0;
	signal intMask	: integer := 0;
	signal extReg 	: VmuMemMappedRegRange(7 downto 0);
begin

	extRegMap: entity VmuMemoryMappedRegister 
		generic map(x"0d") 
		port map(memBusIn, memBusOut, extReg);

	process(clk) 
		variable instr		: std_logic_vector(23 downto 0);
		variable opcode		: integer;
		variable operands	: VmuCpuInstrOperands;
	begin
	
		if(rising_edge(clk)) then
			
			--instruction fetch
			if(extReg = '1') then 
				--fetch from Flash
				flashBusOut.address <= pc;
				--wait for data?
				instr(7 downto 0) := flashBusIn.dataIn;
				opcode := to_integer(unsigned(instr(7 downto 0)))
				pc := pc + 1;

				if(instrSet.instrMap(opcode).bytes >= 2) then
					flashBusOut.address <= pc;
					instr(15 downto 8) := flashBusIn.dataIn;
					pc := pc + 1;
	
					if(instrSet.instrMap(opcode.bytes >= 3) then
						flashBusOut.address <= pc;
						instr(23 downto 16) := flashBusIn.dataIn;
						pc := pc + 1;

					end	if;

				end if;
				
			else 
				--fetch from ROM (not implemented)
			end if;

			--Decode Instruction
			operands := VmuCpuInstrDecodeOperands(instr);

			--Execute Instruction
			case(instrSet.instrMap(opcode).opcode) is
				when OPCODE_NOP => null;
				when others => null;

			end case;

			--Handle firmware calls

		end if;

	end process;


end architecture;