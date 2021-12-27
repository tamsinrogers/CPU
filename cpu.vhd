-- Tamsin Rogers
-- 11/2/20
-- CS232 Project 7
-- cpu.vhd

-- Quartus II VHDL Template
-- Four-State Moore State Machine

-- A Moore machine's outputs are dependent only on the current state.
-- The output is written only when the state changes.  (State
-- transitions are synchronous.)

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cpu is

	port 
	(
		clock   : in  std_logic;                       -- main clock
		reset : in  std_logic;                       -- reset button
		input   : in  std_logic; 
		
		PCview : out std_logic_vector(7 downto 0);  -- debugging outputs
		
		IRview : out std_logic_vector(15 downto 0);
		RAview : out std_logic_vector(15 downto 0);
		RBview : out std_logic_vector(15 downto 0);
		RCview : out std_logic_vector(15 downto 0);
		RDview : out std_logic_vector(15 downto 0);
		REview : out std_logic_vector(15 downto 0);
		
		iport : in  std_logic_vector(7 downto 0);    -- input port
		oport : out std_logic_vector(15 downto 0); 	-- output port
		
		IRlight : out std_logic_vector(9 downto 0);
		PClight : out std_logic_vector(7 downto 0);
		
		digit0:	out std_logic_vector(6 downto 0);		-- first digit in display output values
      digit1:	out std_logic_vector(6 downto 0)		-- second digit in display output values
		
	 );

end entity;

architecture rtl of cpu is

	-- ROM
	component programROM
	PORT
	(
		address		: IN STD_LOGIC_VECTOR (7 DOWNTO 0);
		clock		: IN STD_LOGIC  := '1';
		q		: OUT STD_LOGIC_VECTOR (15 DOWNTO 0)
	);
	end component;
	
	-- RAM
	component DataRAM
	PORT
	(
		address		: IN STD_LOGIC_VECTOR (7 DOWNTO 0);
		clock		: IN STD_LOGIC  := '1';
		data		: IN STD_LOGIC_VECTOR (15 DOWNTO 0);
		wren		: IN STD_LOGIC ;
		q		: OUT STD_LOGIC_VECTOR (15 DOWNTO 0)
	);
	end component;
	
	-- ALU
	component ALU
	PORT 
  	(
		srcA : in  unsigned(15 downto 0);         				-- input A
		srcB : in  unsigned(15 downto 0);         				-- input B
		op   : in  std_logic_vector(2 downto 0);  				-- operation
		cr   : out std_logic_vector(3 downto 0);  				-- condition outputs
		dest : out unsigned(15 downto 0)	       				-- output value
	);
	end component;
	
	-- HEXDISPLAY
	component hexDisplay
	port 
	(
		a: in UNSIGNED(3 downto 0);
		result: out UNSIGNED(6 downto 0)
	);
	
	end component;
	
	-- internal signals
	signal	ra		:	std_logic_vector(15 downto 0); 			-- register A
	signal	rb		:	std_logic_vector(15 downto 0); 			-- register B
	signal	rc		:	std_logic_vector(15 downto 0); 			-- register C
	signal	rd		:	std_logic_vector(15 downto 0); 			-- register D
	signal	re		:	std_logic_vector(15 downto 0); 			-- register E
	signal	sp		:	std_logic_vector(15 downto 0);			-- stack pointer
	signal	ir		:	std_logic_vector(15 downto 0);			-- instruction register
	signal	pc		:	std_logic_vector(7 downto 0);				-- program counter
	signal	srca	:	unsigned(15 downto 0); 						-- ALU source A (ALU input bus #1)
	signal	srcb	:	unsigned(15 downto 0); 						-- ALU source B (ALU input bus #2)
	signal	dest	:	unsigned(15 downto 0); 						-- ALU output (ALU output bus)
	signal	mar		:	STD_LOGIC_VECTOR(7 DOWNTO 0);			-- memory address register
	signal	mbr		:	STD_LOGIC_VECTOR(15 DOWNTO 0);		-- memory buffer register
	signal	outreg	:	std_logic_vector(15 downto 0);		-- output port register
	signal	rom_data_out :	STD_LOGIC_VECTOR(15 DOWNTO 0);	-- ROM data-out wire
	signal	ram_data_out :	STD_LOGIC_VECTOR(15 DOWNTO 0);	-- RAM data-out wire
	signal	ram_write_enable :	STD_LOGIC;						-- RAM write-enable wire
	signal 	opcode	:	std_logic_vector(2 downto 0); 		-- ALU opcode
	signal	cr	:	std_logic_vector(3 downto 0); 				-- ALU condition register
	signal	cond	:	std_logic_vector(3 downto 0);
	
	signal slowclock : std_logic;								-- the slowclock signal
  	signal clockcounter: unsigned (24 downto 0);				-- the counter to be used in the slowclock


	-- Build an enumerated type for the state machine
	type state_type is (startup, fetch, execute_setup, execute_alu, execute_memwait, 
						execute_write, execute_returnpause1, execute_returnpause2, halt);

	-- Register to hold the current state
	signal state   : state_type;
	signal counter	: unsigned(2 downto 0);

begin

	-- assign signals to their matching outputs
	PCView <= PC;
	IRView <= IR;
	RAView <= RA;
	RBView <= RB;
	RCView <= RC;
	RDView <= RD;
	REView <= RE;
	oport <= OUTREG;

	-- port map statements
	programROM1 : programROM					
		port map
		(
			clock => clock,
			address => PC,
			q => rom_data_out
		);
	
	dataRAM1 : DataRAM					
		port map
		(
			address => mar,
			data => mbr,
			q => ram_data_out,
			wren => ram_write_enable,
			clock => clock
		);
	
	ALU1 : alu					
		port map
		(
			srcA => srca,
			srcB => srcb,
			dest => dest,
			op => opcode,
			cr => cond
		);
	
	display1: hexDisplay
		port map(a => unsigned(outreg(3 downto 0)), std_logic_vector(result) => digit0);	-- first digit of outreg result shows up on the first hex display
	
	display2: hexDisplay
		port map(a => unsigned(outreg(7 downto 4)), std_logic_vector(result) => digit1);	-- second digit of outreg result shows up on the second hex display
		
		
	
		-- slow down the clock
	  process(clock, reset) 
		begin
		  if reset = '0' then
			clockcounter <= "0000000000000000000000000";
		  
		  elsif (rising_edge(clock)) then
				clockcounter <= clockcounter + 12;						
		  end if;
	  end process;
	
	  slowclock <= clockcounter(24);

	-- Logic to advance to the next state
	process (slowclock, reset)
	begin
	
		-- RESET case
		if reset = '0' then
			pc <= "00000000";
			ir <= "0000000000000000";
			outreg <= "0000000000000000";
			mar <= "00000000";
			mbr <= "0000000000000000";
			ra <= "0000000000000000";
			rb <= "0000000000000000";
			rc <= "0000000000000000";
			rd <= "0000000000000000";
			re <= "0000000000000000";
			sp <= "0000000000000000";
			cr <= "0000";
			state <= startup;
			counter <= "000";
			
		elsif (rising_edge(slowclock)) then
		
			case state is
			
				-- START state
				when startup =>
					counter <= counter+1;										-- increment the counter
					if counter = 7 then
						state <= fetch;											-- move to fetch
					end if;
				
				-- FETCH state
				when fetch =>
					IR <= rom_data_out;											-- copy the ROM data wire contents to the IR
					PC <= std_logic_vector(unsigned(PC)+1);				-- increment the PC
					state <= execute_setup;										-- move to execute-setup
				
				-- EXECUTE-SETUP state	
				when execute_setup =>
					-- load instruction
					if IR(15 downto 12)="0000" or IR(15 downto 12)="0001" then
						if IR(11) = '1' then										-- if IR(11) is set
							-- move the correct RAM address into the MAR
							mar <= std_logic_vector(unsigned(IR(7 downto 0)) + unsigned(re(7 downto 0)));	
						else															-- if IR(11) is not set
							mar <= IR(7 downto 0);
						end if;
						
						-- put the data to write into the RAM into the MBR
						if IR(10 downto 8) = "000" then
							mbr <= ra;
						elsif IR(10 downto 8) = "001" then
							mbr <= rb;
						elsif IR(10 downto 8) = "010" then
							mbr <= rc;
						elsif IR(10 downto 8) = "011" then
							mbr <= rd;
						elsif IR(10 downto 8) = "100" then
							mbr <= re;
						elsif IR(10 downto 8) = "101" then
							mbr <= sp;
						end if;
						
						state <= execute_alu;
						
					-- unconditional branch
					elsif IR(15 downto 12)="0010" then
						PC <= IR(7 downto 0);
						state <= execute_alu;
						
					-- conditional branch
					elsif IR(15 downto 12)="0011" then
						
						-- move the 8 low bits of the IR to the PC if the condition is true
						if IR(9 downto 8) = "00" then
							if cond(0) = '1' then					
								PC <= IR(7 downto 0); 		
							end if;
						elsif IR(9 downto 8) = "01" then
							if cond(0) = '1' then					
								PC <= IR(7 downto 0); 		
							end if;
						elsif IR(9 downto 8) = "10" then
							if cond(0) = '1' then					
								PC <= IR(7 downto 0); 		
							end if;
						elsif IR(9 downto 8) = "11" then
							if cond(0) = '1' then									
								PC <= IR(7 downto 0); 		
							end if;
						end if;
						
						state <= execute_alu;
					
					-- call instruction
					elsif IR(15 downto 12)="0011" then
						PC <= IR(7 downto 0);
						MAR <= SP(7 downto 0);
						MBR <= "0000" & CR & PC;
						sp <= std_logic_vector(unsigned(sp)+1);			-- increment the stack pointer
						state <= execute_alu;
					
					-- return instruction
					elsif IR(15 downto 12)="0011" then
						mar <= std_logic_vector(unsigned(sp(7 downto 0))-1);
						sp <= std_logic_vector(unsigned(sp)-1); 			-- decrement the stack pointer
						state <= execute_alu;
					
					-- push instruction
					elsif IR(15 downto 12)="0100" then
						mar <= sp(7 downto 0);
						sp <= std_logic_vector(unsigned(sp)+1);			-- increment the stack pointer
						
						-- put the value specified in the source bits into the MBR
						if IR(11 downto 9) = "000" then
							mbr <= ra;
						elsif IR(11 downto 9) = "001" then
							mbr <= rb;
						elsif IR(11 downto 9) = "010" then
							mbr <= rc;
						elsif IR(11 downto 9) = "011" then
							mbr <= rd;
						elsif IR(11 downto 9) = "100" then
							mbr <= re;
						elsif IR(11 downto 9) = "101" then
							mbr <= sp;
						end if;
						
						state <= execute_alu;
					
					-- pop instruction
					elsif IR(15 downto 12)="0101" then
						mar <= std_logic_vector(unsigned(sp(7 downto 0))-1);
						sp <= std_logic_vector(unsigned(sp)-1);
						state <= execute_alu;
					
					-- binary ALU operations
					elsif IR(15 downto 12)="1000" or IR(15 downto 12)="1001" or IR(15 downto 12)="1010" or IR(15 downto 12)="1011" or IR(15 downto 12)="1100" then
					
						-- srcA
						if IR(11 downto 9) = "000" then
							srcA <= unsigned(ra);
						elsif IR(11 downto 9) = "001" then
							srcA <= unsigned(rb);
						elsif IR(11 downto 9) = "010" then
							srcA <= unsigned(rc);
						elsif IR(11 downto 9) = "011" then
							srcA <= unsigned(rd);
						elsif IR(11 downto 9) = "100" then
							srcA <= unsigned(re);
						elsif IR(11 downto 9) = "101" then
							srcA <= unsigned(sp);
						elsif IR(11 downto 9) = "110" then
							srcA <= "0000000000000000";
						elsif IR(11 downto 9) = "000" then
							srcA <= "1111111111111111";
						end if;
						
						-- srcB
						if IR(8 downto 6) = "000" then
							srcB <= unsigned(ra);
						elsif IR(8 downto 6) = "001" then
							srcB <= unsigned(rb);
						elsif IR(8 downto 6) = "010" then
							srcB <= unsigned(rc);
						elsif IR(8 downto 6) = "011" then
							srcB <= unsigned(rd);
						elsif IR(8 downto 6) = "100" then
							srcB <= unsigned(re);
						elsif IR(8 downto 6) = "101" then
							srcB <= unsigned(sp);
						elsif IR(8 downto 6) = "110" then
							srcB <= "0000000000000000";
						elsif IR(8 downto 6) = "000" then
							srcB <= "1111111111111111";
						end if;
						
						opcode <= IR(14 downto 12);
						
						state <= execute_alu;
					
					-- unary ALU operations
					elsif IR(15 downto 12)="1101" or IR(15 downto 12)="1110" then
					
						-- srcA
						if IR(10 downto 8) = "000" then
							srcA <= unsigned(ra);
						elsif IR(10 downto 8) = "001" then
							srcA <= unsigned(rb);
						elsif IR(10 downto 8) = "010" then
							srcA <= unsigned(rc);
						elsif IR(10 downto 8) = "011" then
							srcA <= unsigned(rd);
						elsif IR(10 downto 8) = "100" then
							srcA <= unsigned(re);
						elsif IR(10 downto 8) = "101" then
							srcA <= unsigned(sp);
						elsif IR(10 downto 8) = "110" then
							srcA <= "0000000000000000";
						elsif IR(10 downto 8) = "000" then
							srcA <= "1111111111111111";
						end if;
						
						srcB(0) <= IR(11);
						
						state <= execute_alu;
					
					-- move operation
					elsif IR(15 downto 12)="1111" then
						if IR(11) = '1' then
							srcA <= "00000000" & unsigned(IR(10 downto 3));	-- signed extended value from the immediate value bits of the IR 
						else
						
							-- srcA
							if IR(10 downto 8) = "000" then
								srcA <= unsigned(ra);
							elsif IR(10 downto 8) = "001" then
								srcA <= unsigned(rb);
							elsif IR(10 downto 8) = "010" then
								srcA <= unsigned(rc);
							elsif IR(10 downto 8) = "011" then
								srcA <= unsigned(rd);
							elsif IR(10 downto 8) = "100" then
								srcA <= unsigned(re);
							elsif IR(10 downto 8) = "101" then
								srcA <= unsigned(sp);
							elsif IR(10 downto 8) = "110" then
								srcA <= "0000000000000000";
							elsif IR(10 downto 8) = "000" then
								srcA <= "1111111111111111";
							end if;
						end if;
						
						opcode <= "111";
						
						state <= execute_alu;
					
					-- exit instruction
					elsif IR(15 downto 10)="001111" then	
						state <= halt;
					
					else
						state <= execute_alu;
						
					end if;
				
				-- EXECUTE-ALU state
				when execute_alu =>
					-- if the operation is not reading from memory (store, push, call)
					if (IR(15 downto 12) = "0001" or IR(15 downto 12) = "0011" or IR(15 downto 12) = "0100") then
						ram_write_enable <= '1';	-- set RAM write enable signal to high
						state <= execute_memwait;
						
					-- if the instruction is reading from memory (load, pop, return)
					elsif (IR(15 downto 12) = "0000" or IR(15 downto 12) = "0101" or IR(15 downto 12) = "0011") then
						state <= execute_memwait;
						
					else
						state <= execute_write;
						
					end if;
				
				-- EXECUTE-MEMWAIT state
				when execute_memwait =>
					state <= execute_write;
				
				-- EXECUTE-WRITE state
				when execute_write =>
					ram_write_enable <= '0';
				
					-- load instruction 
					if IR(15 downto 12)="0001" then
						-- write the contents of the RAM data wire to the specified destination reigster
						if IR(10 downto 8) = "000" then
							ra <= ram_data_out;
						elsif IR(10 downto 8) = "001" then
							rb <= ram_data_out;
						elsif IR(10 downto 8) = "010" then
							rc <= ram_data_out;
						elsif IR(10 downto 8) = "011" then
							rd <= ram_data_out;
						elsif IR(10 downto 8) = "100" then
							re <= ram_data_out;
						elsif IR(10 downto 8) = "101" then
							sp <= ram_data_out;
						end if;
						state <= fetch;
					
					-- return instruction
					elsif IR(15 downto 12)="0011" then
						-- write the proper parts of the RAM data wire to the PC & CR
						PC <= ram_data_out(7 downto 0);	-- 8 bits
						CR <= ram_data_out(11 downto 8);	-- 4 bits
						state <= execute_returnpause1;
					
					-- pop instruction
					elsif IR(15 downto 12) = "0101" then
						-- write the value of the RAM data wire to the destination specified in the instruction
						if IR(11 downto 9) = "000" then
							ra <= ram_data_out;
						elsif IR(11 downto 9) = "001" then
							rb <= ram_data_out;
						elsif IR(11 downto 9) = "010" then
							rc <= ram_data_out;
						elsif IR(11 downto 9) = "011" then
							rd <= ram_data_out;
						elsif IR(11 downto 9) = "100" then
							re <= ram_data_out;
						elsif IR(11 downto 9) = "101" then
							sp <= ram_data_out;
						elsif IR(11 downto 9) = "110" then
							pc <= ram_data_out(7 downto 0);
						elsif IR(11 downto 9) = "111" then
							cr <= ram_data_out(3 downto 0);
						end if;
						
						state <= fetch;
						
					-- write to output
					elsif IR(15 downto 12)="0110" then
						-- set the output port register to the specified value
						if IR(11 downto 9) = "000" then
							outreg <= ra;
						elsif IR(11 downto 9) = "001" then
							outreg <= rb;
						elsif IR(11 downto 9) = "010" then
							outreg <= rc;
						elsif IR(11 downto 9) = "011" then
							outreg <= rd;
						elsif IR(11 downto 9) = "100" then
							outreg <= re;
						elsif IR(11 downto 9) = "101" then
							outreg <= sp;
						elsif IR(11 downto 9) = "110" then
							outreg <= "00000000" & pc;
						elsif IR(11 downto 9) = "111" then
							outreg <= ir;	
						end if;
						
						state <= fetch;
					
					-- load from input
					elsif IR(15 downto 12)="0111" then
						-- write the input port value to the specified register
						if IR(11 downto 9) = "000" then
							ra <= "00000000" & iport;
						elsif IR(11 downto 9) = "001" then
							rb <= "00000000" & iport;
						elsif IR(11 downto 9) = "010" then
							rc <= "00000000" & iport;
						elsif IR(11 downto 9) = "011" then
							rd <= "00000000" & iport;
						elsif IR(11 downto 9) = "100" then
							re <= "00000000" & iport;
						elsif IR(11 downto 9) = "101" then
							sp <= "00000000" & iport;
						end if;
						
						state <= fetch;
					
					-- binary & unary ALU operations
					elsif IR(15 downto 12)="1000" or IR(15 downto 12)="1001" or IR(15 downto 12)="1010" or IR(15 downto 12)="1011" or IR(15 downto 12)="1100" or IR(15 downto 12)="1101" or IR(15 downto 12)="1110" then
						-- write the destination value to the proper register
						if IR(2 downto 0) = "000" then
							ra <= std_logic_vector(dest);
						elsif IR(2 downto 0) = "001" then
							rb <= std_logic_vector(dest);
						elsif IR(2 downto 0) = "010" then
							rc <= std_logic_vector(dest);
						elsif IR(2 downto 0) = "011" then
							rd <= std_logic_vector(dest);
						elsif IR(2 downto 0) = "100" then
							re <= std_logic_vector(dest);
						elsif IR(2 downto 0) = "101" then
							sp <= std_logic_vector(dest);
						end if;
						
						cr <= cond;
						state <= fetch;
						
					-- move operation
					elsif IR(15 downto 12)="1111" then
						-- write the destination value to the proper register
						if IR(2 downto 0) = "000" then
							ra <= std_logic_vector(dest);
						elsif IR(2 downto 0) = "001" then
							rb <= std_logic_vector(dest);
						elsif IR(2 downto 0) = "010" then
							rc <= std_logic_vector(dest);
						elsif IR(2 downto 0) = "011" then
							rd <= std_logic_vector(dest);
						elsif IR(2 downto 0) = "100" then
							re <= std_logic_vector(dest);
						elsif IR(2 downto 0) = "101" then
							sp <= std_logic_vector(dest);
						end if;
						
						cr <= cond;
						state <= fetch;
						
					end if;
					state <= fetch;
				
				-- EXECUTE-RETURNPAUSE1 state
				when execute_returnpause1 =>
					state <= execute_returnpause2;
				
				-- EXECUTE-RETURNPAUSE2 state
				when execute_returnpause2 =>
					state <= fetch;
				
				-- HALT state
				when halt =>
					null;
					
			end case;
		end if;
	end process;
	
	-- light control
	IRlight(0) <= '0' when IR(15) = '1' else '1';
	IRlight(1) <= '0' when IR(14) = '1' else '1';
	IRlight(2) <= '0' when IR(13) = '1' else '1';
	IRlight(3) <= '0' when IR(12) = '1' else '1';
	IRlight(4) <= '0' when IR(11) = '1' else '1';
	IRlight(5) <= '0' when IR(10) = '1' else '1';
	IRlight(6) <= '0' when IR(9) = '1' else '1';
	IRlight(7) <= '0' when IR(8) = '1' else '1';
	IRlight(8) <= '0' when IR(7) = '1' else '1';
	IRlight(9) <= '0' when IR(6) = '1' else '1';
	
	PClight(0) <= '0' when PC(0) = '1' else '1';
	PClight(1) <= '0' when PC(1) = '1' else '1';
	PClight(2) <= '0' when PC(2) = '1' else '1';
	PClight(3) <= '0' when PC(3) = '1' else '1';
	PClight(4) <= '0' when PC(4) = '1' else '1';
	PClight(5) <= '0' when PC(5) = '1' else '1';
	PClight(6) <= '0' when PC(6) = '1' else '1';
	PClight(7) <= '0' when PC(7) = '1' else '1';

end rtl;