-module(polybus_tests).
-include_lib("eunit/include/eunit.hrl").

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
%%	set_byte_pos(Data,Byte,Pos)
%%=============================================================================
set_byte_pos_1_test() ->

	Expected 	= <<1,0,0,0>>,
	Data 		= <<0,0,0,0>>,
	Byte 		= 1,
	Pos 		= 0,
	tester(Expected,polybus:set_byte_pos(Data,Byte,Pos),true).

%%-----------------------------------------------------------------------------
set_byte_pos_2_test() ->

	Expected 	= <<0,0,0,1>>,
	Data 		= <<0,0,0,0>>,
	Byte 		= 1,
	Pos 		= 3,
	tester(Expected,polybus:set_byte_pos(Data,Byte,Pos),true).

%%-----------------------------------------------------------------------------
set_byte_pos_3_test() ->

	Expected 	= <<0,1,0,0>>,
	Data 		= <<0,0,0,0>>,
	Byte 		= 1,
	Pos 		= 1,
	tester(Expected,polybus:set_byte_pos(Data,Byte,Pos),true).

%%-----------------------------------------------------------------------------
set_byte_pos_4_test() ->

	Expected 	= <<0,0,1,0>>,
	Data 		= <<0,0,0,0>>,
	Byte 		= 1,
	Pos 		= 2,
	tester(Expected,polybus:set_byte_pos(Data,Byte,Pos),true).