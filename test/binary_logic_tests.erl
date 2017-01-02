-module(binary_logic_tests).
-include_lib("eunit/include/eunit.hrl").

-define(PRINT, true).

%%=============================================================================
%% Helper
%%=============================================================================
tester(Expected_Result,Actual,Print_Bool) ->

	case Print_Bool of
		true -> io:format("Expected:~p~nActual:~p~n",[Expected_Result,Actual]);
		1    -> io:format("Expected:~p~nActual:~p~n",[Expected_Result,Actual]);
		_    -> ok
	end,

	?assertEqual(Expected_Result,Actual).

%%=============================================================================
%% inverter(Bit) 
%%=============================================================================
inverter_1_test() ->

	Expected 	= <<1:1>>,
	Bit 		= <<0:1>>,
	tester(Expected,binary_logic:inverter(Bit),?PRINT).

%%-----------------------------------------------------------------------------
inverter_2_test() ->

	Expected 	= <<0:1>>,
	Bit 		= <<1:1>>,
	tester(Expected,binary_logic:inverter(Bit),?PRINT).

%%=============================================================================
%% and_gate(Bit1,Bit0)
%%=============================================================================
and_gate_1_test() ->

	Expected 	= <<0:1>>,
	Bit1 		= <<0:1>>,
	Bit0 		= <<0:1>>,
	tester(Expected,binary_logic:and_gate(Bit1,Bit0),?PRINT).

%%-----------------------------------------------------------------------------
and_gate_2_test() ->

	Expected 	= <<0:1>>,
	Bit1 		= <<0:1>>,
	Bit0 		= <<1:1>>,
	tester(Expected,binary_logic:and_gate(Bit1,Bit0),?PRINT).

%%-----------------------------------------------------------------------------
and_gate_3_test() ->

	Expected 	= <<0:1>>,
	Bit1 		= <<1:1>>,
	Bit0 		= <<0:1>>,
	tester(Expected,binary_logic:and_gate(Bit1,Bit0),?PRINT).

%%-----------------------------------------------------------------------------
and_gate_4_test() ->

	Expected 	= <<1:1>>,
	Bit1 		= <<1:1>>,
	Bit0 		= <<1:1>>,
	tester(Expected,binary_logic:and_gate(Bit1,Bit0),?PRINT).

%%=============================================================================
%% or_gate(Bit1,Bit0)
%%=============================================================================
or_gate_1_test() ->

	Expected 	= <<0:1>>,
	Bit1 		= <<0:1>>,
	Bit0 		= <<0:1>>,
	tester(Expected,binary_logic:or_gate(Bit1,Bit0),?PRINT).

%%-----------------------------------------------------------------------------
or_gate_2_test() ->

	Expected 	= <<1:1>>,
	Bit1 		= <<0:1>>,
	Bit0 		= <<1:1>>,
	tester(Expected,binary_logic:or_gate(Bit1,Bit0),?PRINT).

%%-----------------------------------------------------------------------------
or_gate_3_test() ->

	Expected 	= <<1:1>>,
	Bit1 		= <<1:1>>,
	Bit0 		= <<0:1>>,
	tester(Expected,binary_logic:or_gate(Bit1,Bit0),?PRINT).

%%-----------------------------------------------------------------------------
or_gate_4_test() ->

	Expected 	= <<1:1>>,
	Bit1 		= <<1:1>>,
	Bit0 		= <<1:1>>,
	tester(Expected,binary_logic:or_gate(Bit1,Bit0),?PRINT).

%%=============================================================================
%% xor_gate(Bit1,Bit0)
%%=============================================================================
xor_gate_1_test() ->

	Expected 	= <<0:1>>,
	Bit1 		= <<0:1>>,
	Bit0 		= <<0:1>>,
	tester(Expected,binary_logic:xor_gate(Bit1,Bit0),?PRINT).

%%-----------------------------------------------------------------------------
xor_gate_2_test() ->

	Expected 	= <<1:1>>,
	Bit1 		= <<0:1>>,
	Bit0 		= <<1:1>>,
	tester(Expected,binary_logic:xor_gate(Bit1,Bit0),?PRINT).

%%-----------------------------------------------------------------------------
xor_gate_3_test() ->

	Expected 	= <<1:1>>,
	Bit1 		= <<1:1>>,
	Bit0 		= <<0:1>>,
	tester(Expected,binary_logic:xor_gate(Bit1,Bit0),?PRINT).

%%-----------------------------------------------------------------------------
xor_gate_4_test() ->

	Expected 	= <<0:1>>,
	Bit1 		= <<1:1>>,
	Bit0 		= <<1:1>>,
	tester(Expected,binary_logic:xor_gate(Bit1,Bit0),?PRINT).

%%=============================================================================
%% nand_gate(Bit1,Bit0)
%%=============================================================================
nand_gate_1_test() ->

	Expected 	= <<1:1>>,
	Bit1 		= <<0:1>>,
	Bit0 		= <<0:1>>,
	tester(Expected,binary_logic:nand_gate(Bit1,Bit0),?PRINT).

%%-----------------------------------------------------------------------------
nand_gate_2_test() ->

	Expected 	= <<1:1>>,
	Bit1 		= <<0:1>>,
	Bit0 		= <<1:1>>,
	tester(Expected,binary_logic:nand_gate(Bit1,Bit0),?PRINT).

%%-----------------------------------------------------------------------------
nand_gate_3_test() ->

	Expected 	= <<1:1>>,
	Bit1 		= <<1:1>>,
	Bit0 		= <<0:1>>,
	tester(Expected,binary_logic:nand_gate(Bit1,Bit0),?PRINT).

%%-----------------------------------------------------------------------------
nand_gate_4_test() ->

	Expected 	= <<0:1>>,
	Bit1 		= <<1:1>>,
	Bit0 		= <<1:1>>,
	tester(Expected,binary_logic:nand_gate(Bit1,Bit0),?PRINT).

%%=============================================================================
%% bit_addition_with_carry(Bit1,Bit0,Carry_In)
%%=============================================================================
bit_addition_with_carry_1_test() ->

	Expected 	= {<<0:1>>,<<0:1>>},
	Bit1 		= <<0:1>>,
	Bit0		= <<0:1>>, 
	Carry_In 	= <<0:1>>,
	tester(Expected,binary_logic:bit_addition_with_carry(Bit1,Bit0,Carry_In),?PRINT).

%%-----------------------------------------------------------------------------
bit_addition_with_carry_2_test() ->

	Expected 	= {<<0:1>>,<<1:1>>},
	Bit1 		= <<0:1>>,
	Bit0		= <<1:1>>, 
	Carry_In 	= <<0:1>>,
	tester(Expected,binary_logic:bit_addition_with_carry(Bit1,Bit0,Carry_In),?PRINT).

%%-----------------------------------------------------------------------------
bit_addition_with_carry_3_test() ->

	Expected 	= {<<0:1>>,<<1:1>>},
	Bit1 		= <<1:1>>,
	Bit0		= <<0:1>>, 
	Carry_In 	= <<0:1>>,
	tester(Expected,binary_logic:bit_addition_with_carry(Bit1,Bit0,Carry_In),?PRINT).

%%-----------------------------------------------------------------------------
bit_addition_with_carry_4_test() ->

	Expected 	= {<<1:1>>,<<0:1>>},
	Bit1 		= <<1:1>>,
	Bit0		= <<1:1>>, 
	Carry_In 	= <<0:1>>,
	tester(Expected,binary_logic:bit_addition_with_carry(Bit1,Bit0,Carry_In),?PRINT).

%%-----------------------------------------------------------------------------
bit_addition_with_carry_5_test() ->

	Expected 	= {<<0:1>>,<<1:1>>},
	Bit1 		= <<0:1>>,
	Bit0		= <<0:1>>, 
	Carry_In 	= <<1:1>>,
	tester(Expected,binary_logic:bit_addition_with_carry(Bit1,Bit0,Carry_In),?PRINT).

%%-----------------------------------------------------------------------------
bit_addition_with_carry_6_test() ->

	Expected 	= {<<1:1>>,<<0:1>>},
	Bit1 		= <<0:1>>,
	Bit0		= <<1:1>>, 
	Carry_In 	= <<1:1>>,
	tester(Expected,binary_logic:bit_addition_with_carry(Bit1,Bit0,Carry_In),?PRINT).

%%-----------------------------------------------------------------------------
bit_addition_with_carry_7_test() ->

	Expected 	= {<<1:1>>,<<0:1>>},
	Bit1 		= <<1:1>>,
	Bit0		= <<0:1>>, 
	Carry_In 	= <<1:1>>,
	tester(Expected,binary_logic:bit_addition_with_carry(Bit1,Bit0,Carry_In),?PRINT).

%%-----------------------------------------------------------------------------
bit_addition_with_carry_8_test() ->

	Expected 	= {<<1:1>>,<<1:1>>},
	Bit1 		= <<1:1>>,
	Bit0		= <<1:1>>, 
	Carry_In 	= <<1:1>>,
	tester(Expected,binary_logic:bit_addition_with_carry(Bit1,Bit0,Carry_In),?PRINT).

%%-----------------------------------------------------------------------------
generic_addition_with_flags_2_test() ->

	Expected 	= {
					<<0:1>>,
					<<1:1,1:1,0:1,0:1, 0:1,1:1,0:1,1:1, 0:1,0:1,0:1,0:1, 0:1,0:1,0:1,0:1>>,
					#{
						v => 1,
						z => 0,
						n => 1,
						c => 0
					}
				  },
	Bits1 		= <<0:1,1:1,0:1,1:1, 1:1,0:1,1:1,0:1, 0:1,0:1,0:1,0:1, 0:1,0:1,0:1,0:1>>,
	Bits0 		= <<0:1,1:1,1:1,0:1, 1:1,0:1,1:1,1:1, 0:1,0:1,0:1,0:1, 0:1,0:1,0:1,0:1>>, 
	Carry_In 	= <<0:1>>,
	tester(Expected,binary_logic:generic_addition_with_flags(Bits1,Bits0,Carry_In),?PRINT).

%%=============================================================================
%% each_bit(Fun,Bits)
%%=============================================================================
each_bit_1_test() ->

	Expected 	= <<
						0:1,
						1:1,
						0:1,
						1:1,
						
						0:1,
						1:1,
						0:1,
						1:1
					>>,
	Fun 		= fun binary_logic:inverter/1,
	Bits 		= <<
						1:1,
						0:1,
						1:1,
						0:1,

						1:1,
						0:1,
						1:1,
						0:1
					>>,
	tester(Expected,binary_logic:each_bit(Fun,Bits),?PRINT).

%%=============================================================================
%% duplicate_bit(Times,Bit)
%%=============================================================================
duplicate_bit(Times,Bit) ->

	Expected 	= <<255:8>>,
	Times 		= 8,
	Bit 		= <<1:1>>,
	tester(Expected,duplicate_bit(Times,Bit),?PRINT).

%%=============================================================================
%% expand_to_size_with_expansion_bit(Size_To_Add,Bits,Expansion_Bit)
%%=============================================================================
expand_to_size_with_expansion_bit_1_test() ->

	Expected 		= <<1:8>>,
	Size_To_Add 	= 7,
	Bits 			= <<1:1>>, 
	Expansion_Bit 	= <<0:1>>,
	tester(Expected,binary_logic:expand_to_size_with_expansion_bit(Size_To_Add,Bits,Expansion_Bit),?PRINT).

%%=============================================================================
%% expand_to_size_with_zeros(Bits,Size)
%%=============================================================================
expand_to_size_with_zeros_1_test(Bits,Size) ->

	Expected 	= <<1:16>>,
	Bits 		= <<1:8>>,
	Size 		= 16,
	tester(Expected,binary_logic:expand_to_size_with_zeros(Bits,Size),?PRINT).

%%=============================================================================
%% sign_extend(Bits,Size)
%%=============================================================================
sign_extend_1_test() ->

	Expected 	= <<16#FFFF:16>>,
	Bits 		= <<16#FF:8>>,
	Size 		= 16,
	tester(Expected,binary_logic:sign_extend(Bits,Size),?PRINT).

%%=============================================================================
%% twos_complement(Bits)
%%=============================================================================
twos_complement_1_test() ->

	Expected 	= <<16#FF:8>>,
	Bits 		= <<1:8>>,
	tester(Expected,binary_logic:twos_complement(Bits),?PRINT).

%%=============================================================================
%% twos_complement_inv(Bits)
%%=============================================================================
twos_complement_inv_1_test() ->

	Expected 	= <<1:8>>,
	Bits 		= <<16#FF:8>>,
	tester(Expected,binary_logic:twos_complement_inv(Bits),?PRINT).

%%=============================================================================
%% generic_subtraction_with_flags(Minuend,Subtrahend)
%%=============================================================================
generic_subtraction_with_flags_1_test() ->

	Expected 	= {
					<< 1:1 >>,
					<< 1:1, 0:1, 1:1, 0:1, 0:1, 0:1, 0:1, 0:1  >>,
					#{c => 1,n => 1,v => 0,z => 0}
				  },
	Minuend	    = << 1:1, 1:1, 0:1, 0:1, 1:1, 0:1, 0:1, 0:1>>, 
	Subtrahend 	= << 0:1, 0:1, 1:1, 0:1, 1:1, 0:1, 0:1, 0:1 >>,
	tester(Expected,binary_logic:generic_subtraction_with_flags(Minuend,Subtrahend),?PRINT).

%%=============================================================================
%% and_binary(Binary1,Binary2)
%%=============================================================================
and_binary_1_test() ->

	Binary1  = <<2#11111111>>,
	Binary2  = <<2#11111111>>,
	Expected = <<2#11111111>>,
	tester(Expected,binary_logic: and_binary(Binary1,Binary2),?PRINT).

%%-----------------------------------------------------------------------------
and_binary_2_test() ->

	Binary1  = <<2#10101010>>,
	Binary2  = <<2#01010101>>,
	Expected = <<2#00000000>>,
	tester(Expected,binary_logic: and_binary(Binary1,Binary2),?PRINT).

%%=============================================================================
%% asl_binary_with_flags(Binary)
%%=============================================================================
asl_binary_with_flags_1_test() ->

	Binary 		= <<2#11000000>>,
	Expected 	= {
					<<1:1>>,
					<<2#10000000>>,
					#{
						v => 0,
						z => 0,
						n => 1,
						c => 1
					}
				  },
	tester(Expected,binary_logic:asl_binary_with_flags(Binary),?PRINT).

%%---------------------------------------------------------------------------
asl_binary_with_flags_2_test() ->

	Binary 		= <<2#11111111>>,
	Expected 	= {
					<<1:1>>,
					<<2#11111110>>,
					#{
						v => 0,
						z => 0,
						n => 1,
						c => 1
					}
				  },
	tester(Expected,binary_logic:asl_binary_with_flags(Binary),?PRINT).

%%---------------------------------------------------------------------------
asl_binary_with_flags_3_test() ->

	Binary 		= <<2#10111111>>,
	Expected 	= {
					<<1:1>>,
					<<2#01111110>>,
					#{
						v => 1,
						z => 0,
						n => 0,
						c => 1
					}
				  },
	tester(Expected,binary_logic:asl_binary_with_flags(Binary),?PRINT).

%%---------------------------------------------------------------------------
asl_binary_with_flags_4_test() ->

	Binary 		= <<2#10000000>>,
	Expected 	= {
					<<1:1>>,
					<<2#00000000>>,
					#{
						v => 1,
						z => 1,
						n => 0,
						c => 1
					}
				  },
	tester(Expected,binary_logic:asl_binary_with_flags(Binary),?PRINT).

%%=============================================================================
%% asr_binary_with_flags(Binary)
%%=============================================================================
asr_binary_with_flags_1_test() ->

	Binary 		= <<2#10000001>>,
	Expected 	= {
					<<1:1>>,
					<<2#11000000>>,
					#{
						z => 0,
						n => 1,
						c => 1
					}
				  },
	tester(Expected,binary_logic:asr_binary_with_flags(Binary),?PRINT).

%%---------------------------------------------------------------------------
asr_binary_with_flags_2_test() ->

	Binary 		= <<2#10000000>>,
	Expected 	= {
					<<0:1>>,
					<<2#11000000>>,
					#{
						z => 0,
						n => 1,
						c => 0
					}
				  },
	tester(Expected,binary_logic:asr_binary_with_flags(Binary),?PRINT).

%%---------------------------------------------------------------------------
asr_binary_with_flags_3_test() ->

	Binary 		= <<2#00000001>>,
	Expected 	= {
					<<1:1>>,
					<<2#00000000>>,
					#{
						z => 1,
						n => 0,
						c => 1
					}
				  },
	tester(Expected,binary_logic:asr_binary_with_flags(Binary),?PRINT).

%%---------------------------------------------------------------------------
asr_binary_with_flags_4_test() ->

	Binary 		= <<2#00000000>>,
	Expected 	= {
					<<0:1>>,
					<<2#00000000>>,
					#{
						z => 1,
						n => 0,
						c => 0
					}
				  },
	tester(Expected,binary_logic:asr_binary_with_flags(Binary),?PRINT).

%%=============================================================================
%% get_bit(Byte,Num)
%%=============================================================================
get_bit_1_test() ->

	Byte 		= <<2#10000000>>,
	Num 		= 7,
	Expected    = <<1:1>>,
	tester(Expected,binary_logic:get_bit(Byte,Num),?PRINT).

%%---------------------------------------------------------------------------
get_bit_2_test() ->

	Byte 		= <<2#01000000>>,
	Num 		= 6,
	Expected    = <<1:1>>,
	tester(Expected,binary_logic:get_bit(Byte,Num),?PRINT).

%%---------------------------------------------------------------------------
get_bit_3_test() ->

	Byte 		= <<2#00100000>>,
	Num 		= 5,
	Expected    = <<1:1>>,
	tester(Expected,binary_logic:get_bit(Byte,Num),?PRINT).

%%---------------------------------------------------------------------------
get_bit_4_test() ->

	Byte 		= <<2#00010000>>,
	Num 		= 4,
	Expected    = <<1:1>>,
	tester(Expected,binary_logic:get_bit(Byte,Num),?PRINT).

%%---------------------------------------------------------------------------
get_bit_5_test() ->

	Byte 		= <<2#00001000>>,
	Num 		= 3,
	Expected    = <<1:1>>,
	tester(Expected,binary_logic:get_bit(Byte,Num),?PRINT).

%%---------------------------------------------------------------------------
get_bit_6_test() ->

	Byte 		= <<2#00000100>>,
	Num 		= 2,
	Expected    = <<1:1>>,
	tester(Expected,binary_logic:get_bit(Byte,Num),?PRINT).

%%---------------------------------------------------------------------------
get_bit_7_test() ->

	Byte 		= <<2#00000010>>,
	Num 		= 1,
	Expected    = <<1:1>>,
	tester(Expected,binary_logic:get_bit(Byte,Num),?PRINT).

%%---------------------------------------------------------------------------
get_bit_8_test() ->

	Byte 		= <<2#00000001>>,
	Num 		= 0,
	Expected    = <<1:1>>,
	tester(Expected,binary_logic:get_bit(Byte,Num),?PRINT).

%%=============================================================================
%% set_bit(Byte,Num,Bit_Value)
%%=============================================================================
set_bit_1_test() ->

	Byte 		= <<2#00000000>>,
	Num 		= 7,
	Bit_Value   = <<1:1>>,
	Expected    = <<2#10000000>>,
	tester(Expected,binary_logic:set_bit(Byte,Num,Bit_Value),?PRINT).

%%---------------------------------------------------------------------------
set_bit_2_test() ->

	Byte 		= <<2#00000000>>,
	Num 		= 6,
	Bit_Value   = <<1:1>>,
	Expected    = <<2#01000000>>,
	tester(Expected,binary_logic:set_bit(Byte,Num,Bit_Value),?PRINT).

%%---------------------------------------------------------------------------
set_bit_3_test() ->

	Byte 		= <<2#00000000>>,
	Num 		= 5,
	Bit_Value   = <<1:1>>,
	Expected    = <<2#00100000>>,
	tester(Expected,binary_logic:set_bit(Byte,Num,Bit_Value),?PRINT).

%%---------------------------------------------------------------------------
set_bit_4_test() ->

	Byte 		= <<2#00000000>>,
	Num 		= 4,
	Bit_Value   = <<1:1>>,
	Expected    = <<2#00010000>>,
	tester(Expected,binary_logic:set_bit(Byte,Num,Bit_Value),?PRINT).

%%---------------------------------------------------------------------------
set_bit_5_test() ->

	Byte 		= <<2#00000000>>,
	Num 		= 3,
	Bit_Value   = <<1:1>>,
	Expected    = <<2#00001000>>,
	tester(Expected,binary_logic:set_bit(Byte,Num,Bit_Value),?PRINT).

%%---------------------------------------------------------------------------
set_bit_6_test() ->

	Byte 		= <<2#00000000>>,
	Num 		= 2,
	Bit_Value   = <<1:1>>,
	Expected    = <<2#00000100>>,
	tester(Expected,binary_logic:set_bit(Byte,Num,Bit_Value),?PRINT).

%%---------------------------------------------------------------------------
set_bit_7_test() ->

	Byte 		= <<2#00000000>>,
	Num 		= 1,
	Bit_Value   = <<1:1>>,
	Expected    = <<2#00000010>>,
	tester(Expected,binary_logic:set_bit(Byte,Num,Bit_Value),?PRINT).

%%---------------------------------------------------------------------------
set_bit_8_test() ->

	Byte 		= <<2#00000000>>,
	Num 		= 0,
	Bit_Value   = <<1:1>>,
	Expected    = <<2#00000001>>,
	tester(Expected,binary_logic:set_bit(Byte,Num,Bit_Value),?PRINT).


%%=============================================================================
%% complement_binary(Binary) 
%%=============================================================================
complement_binary_1_test() ->

	Binary 		= <<0>>,
	Expected    = <<16#FF>>,
	tester(Expected,binary_logic:complement_binary(Binary),?PRINT).

%%---------------------------------------------------------------------------
complement_binary_2_test() ->

	Binary 		= <<2#1111:4>>,
	Expected    = <<2#0000:4>>,
	tester(Expected,binary_logic:complement_binary(Binary),?PRINT). 