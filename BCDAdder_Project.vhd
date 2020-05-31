LIBRARY ieee;
USE ieee.std_logic_1164.ALL	;
ENTITY andGate IS
	PORT(a,b:IN std_logic;c:OUT std_logic);
END ;

ARCHITECTURE andArchitecture OF andGate IS
BEGIN
	c<=a AND b AFTER 7 ns;
END;  



LIBRARY ieee;
USE ieee.std_logic_1164.ALL	; 

ENTITY nandGate IS
	PORT(a,b:IN std_logic;c:OUT std_logic);
END ;

ARCHITECTURE nandArchitecure OF nandGate IS
BEGIN
	c<=a NAND b AFTER 5 ns;
END; 



LIBRARY ieee;
USE ieee.std_logic_1164.ALL	;
ENTITY orGate IS
	PORT(a,b:IN std_logic;c:OUT std_logic);
END ;

ARCHITECTURE orArchitecture OF orGate IS
BEGIN
	c<=a OR b AFTER 7 ns;
END; 



LIBRARY ieee;
USE ieee.std_logic_1164.ALL	;
ENTITY norGate IS
	PORT(a,b:IN std_logic;c:OUT std_logic);
END ;

ARCHITECTURE norArchitecture OF norGate IS
BEGIN
	c<=a NOR b AFTER 5 ns;
END; 	 


LIBRARY ieee;
USE ieee.std_logic_1164.ALL	;
ENTITY xnorGate IS
	PORT(a,b:IN std_logic;c:OUT std_logic);
END ;

ARCHITECTURE xnorArchitecture OF xnorGate IS
BEGIN
	c<=a XNOR b AFTER 9 ns;
END; 


LIBRARY ieee;
USE ieee.std_logic_1164.ALL	;
ENTITY xorGate IS
	PORT(a,b:IN std_logic;c:OUT std_logic);
END ;

ARCHITECTURE xorArchitecture OF xorGate IS
BEGIN
	c<=a XOR b AFTER 11 ns;
END; 


LIBRARY ieee;
USE ieee.std_logic_1164.ALL	;

ENTITY xor3Gate IS
	PORT(a,b,c:IN std_logic;f:OUT std_logic);
END ;	

ARCHITECTURE xor3Architecture OF xor3Gate IS
BEGIN
	f<=a XOR b XOR c AFTER 11 ns;  
	
END; 


LIBRARY ieee;
USE ieee.std_logic_1164.ALL	;

ENTITY or3Gate IS
	PORT(a,b,c:IN std_logic;f:OUT std_logic);
END ; 

ARCHITECTURE or3Architecture OF or3Gate IS
BEGIN
	f<=a OR b OR c AFTER 7 ns;
END;


LIBRARY ieee;
USE ieee.std_logic_1164.ALL	;

ENTITY and3Gate IS
	PORT(a,b,c:IN std_logic;f:OUT std_logic);
END ; 

ARCHITECTURE and3Architecure OF and3Gate IS
BEGIN
	f<=a AND b AND c AFTER 7 ns;
END;

LIBRARY ieee;
USE ieee.std_logic_1164.ALL	;

ENTITY or4Gate IS
	PORT(a,b,c,d:IN std_logic;f:OUT std_logic);
END ;

 ARCHITECTURE or4Architecure OF or4Gate IS
BEGIN
	f<=a OR b OR c OR d AFTER 7 ns;
END;


LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;   

ENTITY and4Gate IS
	PORT(x, y, z, w : IN STD_LOGIC;
			m : OUT STD_LOGIC);
END ENTITY and4Gate;	  

ARCHITECTURE and4Architecture OF and4Gate IS
BEGIN
	m <= x AND y AND z AND w AFTER 7 NS;
END ARCHITECTURE and4Architecture;	


LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;  

ENTITY and5Gate IS
	PORT(x, y, z, w, m : IN STD_LOGIC;
			n : OUT STD_LOGIC);
END ENTITY and5Gate;	  

ARCHITECTURE and5Architecture OF and5Gate IS
BEGIN
	n <= x AND y AND z AND w AND m AFTER 6 NS;
END ARCHITECTURE and5Architecture;


LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;   

ENTITY or5Gate IS
	PORT(x, y, z, w, m : IN STD_LOGIC;
			n : OUT STD_LOGIC);
END ENTITY or5Gate;	  

ARCHITECTURE or5Architecture OF or5Gate IS
BEGIN
	n <= x OR y OR z OR w OR m AFTER 6 NS;
END ARCHITECTURE or5Architecture;


LIBRARY ieee;
USE ieee.std_logic_1164.ALL	;
ENTITY InverterGate IS
	PORT(a:IN std_logic;b:OUT std_logic);
END ;

ARCHITECTURE InverterArchitecure OF InverterGate IS
BEGIN
	b<= NOT a AFTER 4 ns;
END; 



LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
 
ENTITY fulladd IS
  PORT ( x, y, cin: IN STD_LOGIC;
	     sum, cout: OUT STD_LOGIC);
END ENTITY fulladd;		



ARCHITECTURE FA OF fulladd IS	 
signal z,c0,c1,c2,c,w :std_logic;
BEGIN
	 g0:	 ENTITY WORK.xorGate(xorArchitecture)PORT MAP(x, y, w);  
     g1: ENTITY WORK.xorGate(xorArchitecture)PORT MAP(w, cin, sum);  
	-- g1: ENTITY WORK.xor3Gate(xor3Architecture)PORT MAP(x,y, cin,sum);  
	 g2: ENTITY WORK.andGate(andArchitecture)PORT MAP(y, cin, c0); 
     g3: ENTITY WORK.andGate(andArchitecture)PORT MAP(x, cin, c1); 	
     g4: ENTITY WORK.andGate(andArchitecture)PORT MAP(x, y, c2); 	   
	 --g5: ENTITY WORK.or3Gate(or3Architecture)PORT MAP(c0, c1, c2,cout);	 	
	 g5: ENTITY WORK.orGate(orArchitecture)PORT MAP(c0, c1, z);	 	
	 g6: ENTITY WORK.orGate(orArchitecture)PORT MAP(z, c2, cout);	 	 
	
END ;									

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
 
ENTITY testbenchFA IS
END ENTITY testbenchFA;


ARCHITECTURE simple OF testbenchFA IS
  SIGNAL in1, in2: STD_LOGIC;
  SIGNAL sum: STD_LOGIC;
  SIGNAL carry_in, carry_out: STD_LOGIC;
   BEGIN  
  g1: ENTITY work.fulladd(FA)
        PORT MAP ( x=>in1, y=>in2, cin=>carry_in, 
                   sum=>sum, cout=>carry_out);

	   in1 <= '1';
        
       in2 <= '0';
	   
       carry_in <= '1';

  END;   
  
   library	ieee;
use ieee.std_logic_1164.all;


ENTITY rippleadder IS
	PORT(a,b: IN std_logic_vector(3 DOWNTO 0);cin:IN std_logic; 
	sum: OUT std_logic_vector(3 DOWNTO 0);cout:OUT std_logic);
END; 

ARCHITECTURE rippleadder_gate OF rippleadder IS
SIGNAL carry:std_logic_vector(2 DOWNTO 0);	
BEGIN

    g1:    ENTITY work.fulladd(FA) 
		PORT MAP(a(0),b(0),cin,sum(0),carry(0));   
		
    g2:    ENTITY work.fulladd(FA) 
		PORT MAP(a(1),b(1),carry(0),sum(1),carry(1));	  
		
    g3:    ENTITY work.fulladd(FA) 
		PORT MAP(a(2),b(2),carry(1),sum(2),carry(2));	   
		
    g4:    ENTITY work.fulladd(FA) 
          PORT MAP(a(3),b(3),carry(2),sum(3),cout);
        
      END ARCHITECTURE rippleadder_gate;
   
	  
  library	ieee;
use ieee.std_logic_1164.all; 
  
  
  ENTITY testbenchRipple IS
END ENTITY testbenchRipple;


ARCHITECTURE simple OF testbenchRipple IS 
SIGNAL in1, in2: STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL output: STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL carry_in, carry_out: STD_LOGIC;	
   begin 
  g1: ENTITY work.rippleadder(rippleadder_gate)
        PORT MAP ( a=>in1,b=>in2, cin=>carry_in, sum=>output, cout=>carry_out);	
		in1 <= X"2";
      
       in2 <= X"5";
     
carry_in <= '0';
 end;
 
 

   

   
   library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
 
entity Partial_Full_Adder is
Port ( A : in STD_LOGIC;
B : in STD_LOGIC;
Cin : in STD_LOGIC;
S : out STD_LOGIC;
P : out STD_LOGIC;
G : out STD_LOGIC);
end Partial_Full_Adder;
 
architecture structural of Partial_Full_Adder is
signal w: STD_LOGIC;
begin
 
   --S <= A xor B xor Cin;
   --P <= A xor B;
    --G <= A and B;	
   g0: ENTITY work.andGate(andArchitecture)
        PORT MAP ( A,B,G);	
   g1: ENTITY work.xorGate(xorArchitecture)
	    PORT MAP ( A,B,P);				   
   g2: ENTITY work.xorGate(xorArchitecture)
          PORT MAP ( P,Cin,S);	   
end ;


library IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
 
ENTITY Carry_Look_Ahead IS
PORT ( A : IN STD_LOGIC_VECTOR (3 DOWNTO 0);
B : IN STD_LOGIC_VECTOR (3 DOWNTO 0);
Cin : IN STD_LOGIC;
S : OUT STD_LOGIC_VECTOR (3 DOWNTO 0);
Cout : OUT STD_LOGIC);
END Carry_Look_Ahead;
 
ARCHITECTURE CLA_arch OF Carry_Look_Ahead IS
 
SIGNAL c1,c2,c3,P0cin,P1G0,P1P0cin,P2G1,P2P1G0,P2P1P0Cin,P3G2,P3P2G1,P3P2P1G0,P3P2P1P0G0: STD_LOGIC;
SIGNAL P,G: STD_LOGIC_VECTOR(3 DOWNTO 0);
BEGIN
 
PFA1: ENTITY work.Partial_Full_Adder PORT MAP( A(0), B(0), Cin, S(0), P(0), G(0));
PFA2: ENTITY work.Partial_Full_Adder PORT MAP( A(1), B(1), c1, S(1), P(1), G(1));
PFA3: ENTITY work.Partial_Full_Adder PORT MAP( A(2), B(2), c2, S(2), P(2), G(2));
PFA4: ENTITY work.Partial_Full_Adder PORT MAP( A(3), B(3), c3, S(3), P(3), G(3));

	
	
	c1g0:ENTITY work.andGate(andArchitecture) PORT MAP ( P(0),Cin,P0cin);	
	c1g1:ENTITY work.orGate(orArchitecture) PORT MAP ( G(0),P0cin,c1);	 
	   
	c2g0:ENTITY work.andGate(andArchitecture) PORT MAP (P(1),G(0),P1G0);
	c2g1:ENTITY work.and3Gate(and3Architecure) PORT MAP ( P(1),P(0),Cin,P1P0cin); 
	c2g3:ENTITY work.or3Gate(or3Architecture) PORT MAP ( G(1), P1G0,P1P0cin,c2);		
		
	c3g0:ENTITY work.andGate(andArchitecture) PORT MAP (P(2),G(1),P2G1);
	c3g1:ENTITY work.and3Gate(and3Architecure) PORT MAP (P(2),P(1),G(0),P2P1G0);	
	c3g2:ENTITY work.and4Gate(and4Architecture) PORT MAP (P(2),P(1),P(0),Cin,P2P1P0Cin);	
	c3g3:ENTITY work.or4Gate(or4Architecure) PORT MAP ( G(2),P2G1,P2P1G0,P2P1P0Cin,c3);	
	
	c4g0:ENTITY work.andGate(andArchitecture) PORT MAP (P(3),G(2),P3G2);	
	c4g1:ENTITY work.and3Gate(and3Architecure) PORT MAP (P(3),P(2),G(1),P3P2G1);	
	c4g2:ENTITY work.and4Gate(and4Architecture) PORT MAP (P(3),P(2),P(1),G(0),P3P2P1G0);		
	c4g3:ENTITY work.and5Gate(and5Architecture) PORT MAP (P(3),P(2),P(1),P(0),Cin,P3P2P1P0G0);	
    c4g4:ENTITY work.or5Gate(or5Architecture) PORT MAP ( G(3),P3G2,P3P2G1,P3P2P1G0,P3P2P1P0G0,Cout);	
		
END ;									  


 
  LIBRARY	ieee;
USE ieee.std_logic_1164.ALL; 
  

 ENTITY BCDadder IS
	PORT(a,b: IN std_logic_vector(3 DOWNTO 0);cin:IN std_logic; 
	sum: OUT std_logic_vector(3 DOWNTO 0);cout:OUT std_logic);
  END;  
  
  
  
   ARCHITECTURE BCDadderR_gate OF BCDadder IS
   SIGNAL sum1,in2:std_logic_vector(3 DOWNTO 0);
   SIGNAL cout1,outputC,outputAnd1,outputAnd2,outputOr: std_logic;
   
   BEGIN
   g0: ENTITY work.rippleadder(rippleadder_gate)
        PORT MAP ( a,b, cin, sum1, cout1);	
   g1 :ENTITY work.andGate(andArchitecture)
	   	PORT MAP (sum1(3),sum1(2),outputAnd1);
   g2 :ENTITY work.andGate(andArchitecture)
	   	PORT MAP (sum1(3),sum1(1),outputAnd2);
   g3 :ENTITY work.orGate(orArchitecture)
	   	PORT MAP (outputAnd1,outputAnd2,outputOr);   
   g4 :ENTITY work.orGate(orArchitecture)
	   PORT MAP (outputOr,cout1,outputC);  		    
   in2<=  ('0',outputC,outputC,'0'); 
   g5: ENTITY work.rippleadder(rippleadder_gate)
        PORT MAP ( sum1,in2,'0', sum, cout);	      
   END;		   
   
   ARCHITECTURE BCDadderL_gate OF BCDadder IS
   SIGNAL sum1,in2:std_logic_vector(3 DOWNTO 0);
   SIGNAL cout1,outputC,outputAnd1,outputAnd2,outputOr: std_logic;
   
   BEGIN
   g0: ENTITY work.Carry_Look_Ahead (CLA_arch)
        PORT MAP ( a,b, cin, sum1, cout1);	
   g1 :ENTITY work.andGate(andArchitecture)
	   	PORT MAP (sum1(3),sum1(2),outputAnd1);
   g2 :ENTITY work.andGate(andArchitecture)
	   	PORT MAP (sum1(3),sum1(1),outputAnd2);
   g3 :ENTITY work.orGate(orArchitecture)
	   	PORT MAP (outputAnd1,outputAnd2,outputOr);   
   g4 :ENTITY work.orGate(orArchitecture)
	   PORT MAP (outputOr,cout1,outputC);  		    
   in2<=  ('0',outputC,outputC,'0'); 
   g5: ENTITY work.Carry_Look_Ahead (CLA_arch)
        PORT MAP ( sum1,in2,'0', sum, cout);	      
   END;		   
   
   
	  LIBRARY	ieee;
USE ieee.std_logic_1164.ALL; 
   
   
   
   ENTITY testbenchBCD IS
END ENTITY testbenchBCD;


ARCHITECTURE simple OF testbenchBCD IS 
SIGNAL in1, in2: STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL output: STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL carry_in, carry_out: STD_LOGIC;	
   BEGIN 
  g1: ENTITY work.BCDadder(BCDadderR_gate)
	  PORT MAP ( a=>in1,b=>in2, cin=>carry_in, sum=>output, cout=>carry_out);	
	  
	  
		in1 <= X"6";
      
       in2 <= X"6";
     
    carry_in <= '0';
  END; 			 
  
  
   ARCHITECTURE simple1 OF testbenchBCD IS 
SIGNAL in1, in2: STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL output: STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL carry_in, carry_out: STD_LOGIC;	
   BEGIN 
  g1: ENTITY work.BCDadder(BCDadderL_gate)
	  PORT MAP ( a=>in1,b=>in2, cin=>carry_in, sum=>output, cout=>carry_out);	
	  
	  
		in1 <= X"6";
      
       in2 <= X"6";
     
    carry_in <= '0';
    END; 
	
	
	
	
LIBRARY IEEE;
 USE IEEE.STD_LOGIC_1164.ALL;
 USE IEEE.STD_LOGIC_ARITH.ALL;
 
 ENTITY test_generator IS
 PORT ( clock: IN STD_LOGIC;
 TestIn1: OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
 TestIn2: OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
 ExpectedResult: OUT STD_LOGIC_VECTOR(4 DOWNTO 0));
 END ENTITY test_generator;	   
 
 ARCHITECTURE tb OF test_generator IS	  
 	signal sum:	 Integer;
 BEGIN
 PROCESS
 BEGIN

 FOR I IN 0 TO 15 LOOP
 FOR J IN 0 TO 15 LOOP
 -- Set the inputs to the adder
 TestIn1 <= CONV_STD_LOGIC_VECTOR(i,4);
 TestIn2 <= CONV_STD_LOGIC_VECTOR(j,4);
 -- Calculate what the output of the adder should be  
		
 sum<=i+j;
 IF	(sum > 9) THEN 
  ExpectedResult <= CONV_STD_LOGIC_VECTOR(sum+6,5);   
 ELSE 	  
	 ExpectedResult <= CONV_STD_LOGIC_VECTOR(sum,5);  
 	   END IF; 
 -- Wait until adder output has settled
 WAIT UNTIL rising_edge(clock);
 END LOOP;
 END LOOP;
 WAIT;
 END PROCESS;
 END ARCHITECTURE tb;
	
	
 LIBRARY IEEE;
 USE IEEE.STD_LOGIC_1164.ALL;
 USE IEEE.STD_LOGIC_ARITH.ALL;	
 
 ENTITY result_analyzer IS
 PORT ( clock: IN STD_LOGIC;
 TestIn1: IN STD_LOGIC_VECTOR(3 DOWNTO 0);
 TestIn2: IN STD_LOGIC_VECTOR(3 DOWNTO 0);
 ExpectedResult: IN STD_LOGIC_VECTOR(4 DOWNTO 0);
 ActualAdd: IN STD_LOGIC_VECTOR(3 DOWNTO 0);
 ActualCarry: IN STD_LOGIC);
 END ENTITY result_analyzer;	
 
 
 ARCHITECTURE tb OF result_analyzer IS
 BEGIN
 PROCESS(clock)
 BEGIN
 IF rising_edge(clock) THEN
 -- Check whether adder output matches expectation
 ASSERT ExpectedResult(3 DOWNTO 0) = ActualAdd
 and ExpectedResult(4) = ActualCarry
 REPORT "Adder output is incorrect"
 SEVERITY WARNING;
 END IF;
 END PROCESS;
 END ARCHITECTURE tb;
	
	

	
  LIBRARY IEEE;
 USE IEEE.STD_LOGIC_1164.ALL;	
 
 ENTITY addtest IS
 END ENTITY addtest;	 
 
 ARCHITECTURE tb OF addtest IS
 SIGNAL clock: std_logic:='0';
 --Declarations of test inputs and outputs
 SIGNAL TestIn1: STD_LOGIC_VECTOR(3 DOWNTO 0);
 SIGNAL TestIn2: STD_LOGIC_VECTOR(3 DOWNTO 0);
 SIGNAL AdderOut: STD_LOGIC_VECTOR(3 DOWNTO 0);
 SIGNAL AdderCarry: STD_LOGIC;
 SIGNAL ExpectedResult: STD_LOGIC_VECTOR(4 DOWNTO 0);
 
 BEGIN
 clock <= NOT clock AFTER 10 NS;				  
 
 -- Place one instance of test generation unit
 TG: ENTITY work.test_generator(tb)
 PORT MAP ( clock=>clock, TestIn1=>TestIn1, TestIn2=>TestIn2,
 ExpectedResult=>ExpectedResult);	
 
 -- Place one instance of the Unit Under Test
 UUT: ENTITY work.BCDadder(BCDadderL_gate)
 PORT MAP ( a=>TestIn1, b=>TestIn2, cin=>'0',
 sum=>AdderOut, cout=>AdderCarry );		 
 
 -- Place one instance of the result analyzer
 RA: ENTITY work.result_analyzer(tb)
 PORT MAP (clock=>clock, TestIn1=>TestIn1, TestIn2=>TestIn2,
 ExpectedResult=>ExpectedResult, ActualAdd=>AdderOut,
 ActualCarry=>AdderCarry);
 END ARCHITECTURE tb;
	
	

