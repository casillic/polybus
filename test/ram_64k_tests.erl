-module(ram_64k_tests).
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
%% initalized_memory(Size)
%%=============================================================================
initalized_memory_1_test() ->

	Expected 	= <<0,0,0,0,0,0,0,0,0,0>>,
	Size 		= 10,
	tester(Expected,ram_64k:initalized_memory(Size),?PRINT).

%%=============================================================================
%% memory_immediate(Op_Code,Immediate)
%%=============================================================================
memory_immediate_1_test() ->

	Expected 	= <<16#89:8,255:8>>,
	Op_Code 	= <<16#89:8>>, 
	Immediate 	= <<255:8>>,
	tester(Expected,ram_64k:memory_immediate(Op_Code,Immediate),?PRINT).

%%=============================================================================
%% memory_direct(Op_Code,Direct)
%%=============================================================================
memory_direct_1_test() ->

	Expected    = <<16#1089:16,5:8>>,
	Op_Code 	= <<16#1089:16>>, 
	Direct 		= <<5:8>>,
	tester(Expected,ram_64k:memory_direct(Op_Code,Direct),?PRINT).

%%=============================================================================
%% memory_extended(Op_Code,Value,Location)
%%=============================================================================
memory_extended_1_test() ->

	Expected 	= <<16#89:8,10:16,0,0,0,0,0,0,0,5:8>>,
	Op_Code 	= <<16#89:8>>,
	Value 		= <<5:8>>,
	Location 	= <<10:16>>,
	tester(Expected,ram_64k:memory_extended(Op_Code,Value,Location),?PRINT).

%%-----------------------------------------------------------------------------
memory_extended_2_test() ->

	Expected 	= <<16#1089:16,10:16,0,0,0,0,0,0,5:8>>,
	Op_Code 	= <<16#1089:16>>,
	Value 		= <<5:8>>,
	Location 	= <<10:16>>,
	tester(Expected,ram_64k:memory_extended(Op_Code,Value,Location),?PRINT).

%%=============================================================================
%% set_word_pos(Data,Word,Position)
%%=============================================================================
set_word_pos_1_test() ->

	Data 		= <<0,0,0,0,0,0>>,
	Word 		= <<2#11111111:8,2#11111111:8>>,
	Position    = <<2:16>>,
	Expected 	= <<0,0,2#11111111:8,2#11111111:8,0,0>>,
	tester(Expected,ram_64k:set_word_pos(Data,Word,Position),?PRINT).