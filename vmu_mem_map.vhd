library IEEE;
use IEEE.STD_LOGIC_1164.all;

package memMap is 
	constant ADDR_SFR_OFFSET	: natural := 16#100#; -- Starting address of SFR registers in memory space

	constant ADDR_SFR_ACC 		: natural := 16#100#; -- Accumulator Register
	constant ADDR_SFR_PSW 		: natural := 16#101#; -- Program Status Word Register
	constant ADDR_SFR_B 		: natural := 16#102#; -- B Reg (General Purpose)
	constant ADDR_SFR_C 		: natural := 16#103#; -- C Reg (General Purpose)
	constant ADDR_SFR_TRL		: natural := 16#104#; -- Table Reference Low Byte
	constant ADDR_SFR_TRH		: natural := 16#105#; -- Table Reference Hight Byte
	constant ADDR_SFR_SP		: natural := 16#106#; -- Stack Pointer
	constant ADDR_SFR_PCON		: natural := 16#107#; -- Power Control Register
	constant ADDR_SFR_IE 		: natural := 16#108#; -- Interrupt Enable
	constant ADDR_SFR_IP 		: natural := 16#109#; -- Interrupt Ranking Priority Control
	constant ADDR_SFR_EXT 		: natural := 16#10d#; -- External Control Register (IMem <= 0: ROM, 1: Flash)
	constant ADDR_SFR_OCR 		: natural := 16#10e#; -- Oscillation Control Register
	constant ADDR_SFR_MCR		: natural := 16#120#; -- Mode Control Register
	constant ADDR_SFR_STAD		: natural := 16#102#; -- Start Address Register
	constant ADDR_SFR_XBNK		: natural := 16#125#; -- XRAM Bank #
	constant ADDR_SFR_VCCR 		: natural := 16#127#; -- LCD Contrast Control Register
	constant ADDR_SFR_P1		: natural := 16#144#; -- Port 1 Latch
	constant ADDR_SFR_P1_DDR 	: natural := 16#145#; -- Port 1 Data Direction Register
	constant ADDR_SFR_P1_FCR	: natural := 16#146#; -- Port 1 Function Control Register
	constant ADDR_SFR_P3		: natural := 16#14c#; -- Port 3 Latch
	constant ADDR_SFR_P3_DDR	: natural := 16#14d#; -- Port 3 Data Direction Register
	constant ADDR_SFR_P3_INT	: natural := 16#14e#; -- Port 3 Interrupt Control Register
	constant ADDR_SFR_P7		: natural := 16#15c#; -- Port 7 Latch
	constant ADDR_SFR_I01CR		: natural := 16#15d#; -- External Interupt 0, 1 Control
	constant ADDR_SFR_I23CR		: natural := 16#15e#; -- External Interupt 2, 3 Control
	constant ADDR_SFR_ISL		: natural := 16#15f#; -- Input Signal Selection Register
	constant ADDR_SFR_VSEL		: natural := 16#163#; -- VMS Control Register
	constant ADDR_SFR_VRMAD1	: natural := 16#164#; -- Work RAM Address 1
	constant ADDR_SFR_VRMAD2	: natural := 16#165#; -- Work RAM Address 2
	constant ADDR_SFR_VTRBF		: natural := 16#166#; -- Send/Receive Buffer
	constant ADDR_SFR_VLREG		: natural := 16#167#; -- Length Registration
	constant ADDR_SFR_BTCR		: natural := 16#17f#; -- Base Timer Control Register

	constant ADDR_STACK_BEGIN	: natural := 16#80#;  -- Always in RAM Bank 0!
	constant ADDR_STACK_END		: natural := 16#FF#;  -- Always in RAM Bank 0!



end package;