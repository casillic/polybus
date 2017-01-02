-module(logic_tests).
-include_lib("eunit/include/eunit.hrl").

-define(PRINT, true).

% %%=============================================================================
% %% Helper
% %%=============================================================================
% tester(Expected_Result,Actual,Print_Bool) ->

% 	case Print_Bool of
% 		true -> io:format("Expected:~p~nActual:~p~n",[Expected_Result,Actual]);
% 		1    -> io:format("Expected:~p~nActual:~p~n",[Expected_Result,Actual]);
% 		_    -> ok
% 	end,

% 	?assertEqual(Expected_Result,Actual).

% %%=============================================================================
% %% decimal_to_8_bit(Decimal)
% %%=============================================================================
% decimal_to_8_bit_1_test() ->

% 	Expected 	= [0,0,0,0,0,0,0,1],
% 	Decimal 	= 1,
% 	tester(Expected,logic:decimal_to_8_bit(Decimal),?PRINT).

% %%-----------------------------------------------------------------------------
% decimal_to_8_bit_2_test() ->

% 	Expected 	= [0,0,0,0,1,0,1,1],
% 	Decimal 	= 11,
% 	tester(Expected,logic:decimal_to_8_bit(Decimal),?PRINT).

% %%=============================================================================
% %% decimal_to_binary(Decimal)
% %%=============================================================================
% decimal_to_binary_1_test() ->

% 	Expected    = [1,0,1,1],
% 	Decimal 	= 11,
% 	tester(Expected,logic:decimal_to_binary(Decimal),?PRINT).

% %%=============================================================================
% %% unsigned_to_bits_8(Number)
% %%=============================================================================
% unsigned_to_bits_8_1_test() ->

% 	Expected 	= [0,0,0,0, 0,0,0,0],
% 	Number 		= 0,
% 	tester(Expected,logic:unsigned_to_bits_8(Number),?PRINT).

% %%-----------------------------------------------------------------------------
% unsigned_to_bits_8_2_test() ->

% 	Expected 	= [1,0,1,0, 1,0,1,0],
% 	Number 		= 16#AA,
% 	tester(Expected,logic:unsigned_to_bits_8(Number),?PRINT).

% %%-----------------------------------------------------------------------------
% unsigned_to_bits_8_3_test() ->

% 	Expected 	= [0,1,0,1, 0,1,0,1],
% 	Number 		= 16#55,
% 	tester(Expected,logic:unsigned_to_bits_8(Number),?PRINT).

% %%-----------------------------------------------------------------------------
% unsigned_to_bits_8_4_test() ->

% 	Expected 	= [1,1,1,1, 1,1,1,1],
% 	Number 		= 16#FF,
% 	tester(Expected,logic:unsigned_to_bits_8(Number),?PRINT).

% %%=============================================================================
% %% bits_8_to_unsigned(Bit_List)
% %%=============================================================================
% bits_8_to_unsigned_1_test() ->

% 	Expected 	= 255,
% 	Bit_List 	= [1,1,1,1, 1,1,1,1],
% 	tester(Expected,logic:bits_8_to_unsigned(Bit_List),?PRINT).

% %%-----------------------------------------------------------------------------
% bits_8_to_unsigned_2_test() ->

% 	Expected 	= 16#55,
% 	Bit_List 	= [0,1,0,1, 0,1,0,1],
% 	tester(Expected,logic:bits_8_to_unsigned(Bit_List),?PRINT).