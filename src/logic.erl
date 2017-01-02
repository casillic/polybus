-module('logic').

-compile(export_all).

% %%----Unit_Tested-------------------------------------------------------------------------
% decimal_to_8_bit(Decimal) ->

% 	expand_to_length(decimal_to_binary(Decimal),8).

% %%----Unit_Tested-------------------------------------------------------------------------
% decimal_to_binary(Decimal) ->

% 	decimal_to_binary(Decimal,[]).

% decimal_to_binary(0,Remainders) ->

% 	Remainders;

% decimal_to_binary(Quotient,Remainders) -> 

% 	New_Quotient 	= Quotient div 2,
% 	New_Remainder 	= Quotient rem 2,

% 	decimal_to_binary(New_Quotient,[New_Remainder|Remainders]).

%%----Unit_Tested-------------------------------------------------------------------------
unsigned_to_bits_8(Number) ->

	[ X || <<X:1>> <= <<Number:8>>].

%%-----------------------------------------------------------------------------
unsigned_to_bits_16(Number) ->

	[ X || <<X:1>> <= <<Number:16>>].

%%-----------------------------------------------------------------------------
unsigned_to_bits_24(Number) ->

	[ X || <<X:1>> <= <<Number:24>>].

%%-----------------------------------------------------------------------------
unsigned_to_bits_32(Number) ->

	[ X || <<X:1>> <= <<Number:32>>].

%%-----------------------------------------------------------------------------
unsigned_to_bits_64(Number) ->

	[ X || <<X:1>> <= <<Number:64>>].

%%----Unit_Tested-------------------------------------------------------------------------
bits_8_to_unsigned(Bit_List) ->

	<<Number:8>> = << <<X/bits>> || X <- [<<X:1>> || X <- Bit_List] >>,
	Number.

%%-----------------------------------------------------------------------------
bits_16_to_unsigned(Bit_List) ->

	<<Number:16>> = << <<X/bits>> || X <- [<<X:1>> || X <- Bit_List] >>,
	Number.

%%-----------------------------------------------------------------------------
bits_24_to_unsigned(Bit_List) ->

	<<Number:24>> = << <<X/bits>> || X <- [<<X:1>> || X <- Bit_List] >>,
	Number.

%%-----------------------------------------------------------------------------
bits_32_to_unsigned(Bit_List) ->

	<<Number:32>> = << <<X/bits>> || X <- [<<X:1>> || X <- Bit_List] >>,
	Number.

%%-----------------------------------------------------------------------------
bits_64_to_unsigned(Bit_List) ->

	<<Number:64>> = << <<X/bits>> || X <- [<<X:1>> || X <- Bit_List] >>,
	Number.
