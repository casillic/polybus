%%%----------------------------------------------------------------------------
%% binary_logic
%%%----------------------------------------------------------------------------
-module(binary_logic).
-compile(export_all).

-type bit_type() :: <<_:1>>.

%%%=======================================================================================
%% @doc 
% Inverts a Bit<br/>
% Returns: 1 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec inverter(Bit::bit_type()) -> bit_type().
%%%=======================================================================================
inverter(Bit) ->

	case Bit of

		<<0:1>> -> <<1:1>>;
		<<1:1>> -> <<0:1>>
	end.

%%%=======================================================================================
%% @doc 
% ANDs two Bits<br/>
% Returns: 1 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec and_gate(Bit1::bit_type(), Bit2::bit_type()) -> bit_type().
%%%=======================================================================================
and_gate(Bit1, Bit2) ->

	case [Bit1, Bit2] of

		[<<0:1>>, <<0:1>>] -> <<0:1>>;
		[<<0:1>>, <<1:1>>] -> <<0:1>>;
		[<<1:1>>, <<0:1>>] -> <<0:1>>;
		[<<1:1>>, <<1:1>>] -> <<1:1>>
	end.

%%%=======================================================================================
%% @doc 
% ORs two Bits<br/>
% Returns: 1 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec or_gate(Bit1::bit_type(), Bit2::bit_type()) -> bit_type().
%%%=======================================================================================
or_gate(Bit1, Bit2) ->

	case [Bit1,Bit2] of

		[<<0:1>>,<<0:1>>] -> <<0:1>>;
		[<<0:1>>,<<1:1>>] -> <<1:1>>;
		[<<1:1>>,<<0:1>>] -> <<1:1>>;
		[<<1:1>>,<<1:1>>] -> <<1:1>>
	end.

%%%=======================================================================================
%% @doc 
% XORs two Bits<br/>
% Returns: 1 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec xor_gate(Bit1::bit_type(), Bit2::bit_type()) -> bit_type().
%%%=======================================================================================
xor_gate(Bit1, Bit2) ->

	case [Bit1, Bit2] of

		[<<0:1>>,<<0:1>>] -> <<0:1>>;
		[<<0:1>>,<<1:1>>] -> <<1:1>>;
		[<<1:1>>,<<0:1>>] -> <<1:1>>;
		[<<1:1>>,<<1:1>>] -> <<0:1>>
	end.

%%%=======================================================================================
%% @doc 
% NANDs two Bits<br/>
% Returns: 1 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec nand_gate(Bit1::bit_type(), Bit2::bit_type()) -> bit_type().
%%%=======================================================================================
nand_gate(Bit1,Bit2) ->

	case [Bit1,Bit2] of

		[<<0:1>>,<<0:1>>] -> <<1:1>>;
		[<<0:1>>,<<1:1>>] -> <<1:1>>;
		[<<1:1>>,<<0:1>>] -> <<1:1>>;
		[<<1:1>>,<<1:1>>] -> <<0:1>>
	end.

%%%=======================================================================================
%% @doc 
% Adds two Bits<br/>
% Returns: tuple {carry,result}<br/>
% ---- Unit-Tested ----<br/>
-spec bit_addition_with_carry(Bit1::bit_type(), Bit2::bit_type()) -> {bit_type(), bit_type()}.
%%%=======================================================================================
bit_addition_with_carry(Bit1,Bit0) ->

	bit_addition_with_carry(Bit1, Bit0, <<0:1>>).

%%%=======================================================================================
%% @doc 
% Adds two Bits with Carry In<br/>
% Returns: tuple {carry,result}<br/>
% ---- Unit-Tested ----<br/>
-spec bit_addition_with_carry(
								Bit1 		::bit_type(),
								Bit2 		::bit_type(),
								Carry_In 	::bit_type()
							  ) -> {bit_type(), bit_type()}.
%%%=======================================================================================
bit_addition_with_carry(Bit1, Bit0, Carry_In) ->

	{Carry,Result} = case [Bit1,Bit0,Carry_In] of

							[<<0:1>>,<<0:1>>,<<0:1>>] -> {<<0:1>>,<<0:1>>};
							[<<0:1>>,<<1:1>>,<<0:1>>] -> {<<0:1>>,<<1:1>>};
							[<<1:1>>,<<0:1>>,<<0:1>>] -> {<<0:1>>,<<1:1>>};
							[<<1:1>>,<<1:1>>,<<0:1>>] -> {<<1:1>>,<<0:1>>};

							[<<0:1>>,<<0:1>>,<<1:1>>] -> {<<0:1>>,<<1:1>>};
							[<<0:1>>,<<1:1>>,<<1:1>>] -> {<<1:1>>,<<0:1>>};						
							[<<1:1>>,<<0:1>>,<<1:1>>] -> {<<1:1>>,<<0:1>>};
							[<<1:1>>,<<1:1>>,<<1:1>>] -> {<<1:1>>,<<1:1>>}
						end,
	{Carry,Result}.

%%-----------------------------------------------------------------------------
generic_addition_with_flags(Bits1, Bits0) ->
	
	generic_addition_with_flags(Bits1, Bits0, <<0:1>>).

%%-----------------------------------------------------------------------------
generic_addition_with_flags(Bits1, Bits0, Carry_In) ->

	generic_addition_with_flags(Bits1,Bits0,Carry_In,<<>>,[]).

generic_addition_with_flags(<<>>, <<>>, Carry_In, Result, Carry_Array) ->

	[Cs,Cp|_] 			= Carry_Array,

	<<Overflow_Flag:1>> = xor_gate(Cs,Cp),

	Result_Size 		= bit_size(Result),

	Zero_Flag_Status 	= Result =:= <<0:Result_Size>>,

	Zero_Flag 			= case Zero_Flag_Status of 
								true -> 1;
								_ 	 -> 0
							end,

	<<Carry_Flag:1>> 	= Carry_In,

	<<
		Sign_Flag:1,
		_/bits
	>> = Result,

	Flags 				= #{
								v => Overflow_Flag,
								z => Zero_Flag,
								n => Sign_Flag,
								c => Carry_Flag
						   },

	{Carry_In,Result,Flags};

generic_addition_with_flags(Bits1,Bits0,Carry_In,Results,Carry_Array) ->
	
	Result_Size 				= bit_size(Results),

	Bits1_Size 					= bit_size(Bits1) - 1,
	Bits0_Size 					= bit_size(Bits0) - 1 ,

	<<
		New_Bits1:Bits1_Size,
		Bit1:1
	>> 	= Bits1,

	<<
		New_Bits0:Bits0_Size,
		Bit0:1
	>> 	= Bits0,

	{New_Carry_In, New_Bit} 	= bit_addition_with_carry(<<Bit1:1>>,<<Bit0:1>>,Carry_In),

	generic_addition_with_flags(
									<<New_Bits1:Bits1_Size>>,
									<<New_Bits0:Bits0_Size>>,
									New_Carry_In,
									<<New_Bit/bits,Results/bits>>,
									[New_Carry_In|Carry_Array]
								).

%%-----------------------------------------------------------------------------
byte_addition_with_flags(Bits1,Bits0) ->

	byte_addition_with_flags(Bits1,Bits0,<<0:1>>).

%%-----------------------------------------------------------------------------
byte_addition_with_flags(Bits1,Bits0,Carry_In) ->

	byte_addition_with_flags(Bits1,Bits0,Carry_In,<<>>,[]).

byte_addition_with_flags(<<>>,<<>>,Carry_In,Result,Carry_Array) ->

	[Cs,Cp,_,_,Half_Carry,_,_,_] = Carry_Array,

	<<Overflow_Flag:1>> 	= xor_gate(Cs,Cp),

	Result_Size 			= bit_size(Result),

	Zero_Flag_Status 		= Result =:= <<0:Result_Size>>,

	Zero_Flag 				= case Zero_Flag_Status of 
									true -> 1;
									_ 	 -> 0
								end,

	<<Carry_Flag:1>> 		= Carry_In,
	<<Half_Carry_Flag:1>> 	= Half_Carry,
	
	<<
		Sign_Flag:1,
		_/bits
	>> = Result,

	Flags 					= #{
									v => Overflow_Flag,
									z => Zero_Flag,
									n => Sign_Flag,
									h => Half_Carry_Flag,
									c => Carry_Flag
							   },

	{Carry_In,Result,Flags};

byte_addition_with_flags(Bits1,Bits0,Carry_In,Results,Carry_Array) ->
	
	Result_Size 				= bit_size(Results),

	Bits1_Size 					= bit_size(Bits1) - 1,
	Bits0_Size 					= bit_size(Bits0) - 1 ,

	<<
		New_Bits1:Bits1_Size,
		Bit1:1
	>> 	= Bits1,

	<<
		New_Bits0:Bits0_Size,
		Bit0:1
	>> 	= Bits0,

	{New_Carry_In, New_Bit} 	= bit_addition_with_carry(<<Bit1:1>>,<<Bit0:1>>,Carry_In),

	byte_addition_with_flags(
								<<New_Bits1:Bits1_Size>>,
								<<New_Bits0:Bits0_Size>>,
								New_Carry_In,
								<<New_Bit/bits,Results/bits>>,
								[New_Carry_In|Carry_Array]
							).

%%%=======================================================================================
%% @doc 
% Performs the function Fun on each bit in Bits<br/>
% ---- Unit-Tested ----<br/>
-spec each_bit(Fun::fun(), Bits::binary()) -> binary().
%%%=======================================================================================
each_bit(Fun,Bits) ->

	each_bit(Fun,Bits,<<>>).

each_bit(_Fun,<<>>,ACC) ->

	ACC;

each_bit(Fun,Bits,ACC) ->

	<<First:1,New_Bits/bits>> 	= Bits,

	New_Bit 					= Fun(<<First:1>>),

	each_bit(Fun,New_Bits,<<ACC/bits,New_Bit/bits>>).

%%%=======================================================================================
%% @doc 
% Duplicates a Bit<br/>
% ---- Unit-Tested ----<br/>
-spec duplicate_bit(Times::non_neg_integer(), Bit::<<_:1>>) -> binary().
%%%=======================================================================================
duplicate_bit(Times, Bit) ->
	
	duplicate_bit(Times, Bit, <<>>).

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
duplicate_bit(0, _Bit, ACC) ->

	ACC;

duplicate_bit(Times, Bit, ACC) ->

	duplicate_bit((Times - 1), Bit, <<ACC/bits, Bit/bits>>).

%%%=======================================================================================
%% @doc 
% Zero extends a group of bits<br/>
% ---- Unit-Tested ----<br/>
-spec expand_to_size_with_zeros(Bits::<<_:_*1>>, Size::non_neg_integer()) -> <<_:_*1>>.
%%%=======================================================================================
expand_to_size_with_zeros(Bits, Size) ->

	Bit_Size 		= bit_size(Bits),

	Size_To_Add 	= case Bit_Size < Size of

						true -> Size - Bit_Size;
						_ 	 -> 0
						end,

	expand_to_size_with_expansion_bit(Size_To_Add, Bits, <<0:1>>). 

%%----Unit_Tested-------------------------------------------------------------------------
expand_to_size_with_expansion_bit(0, Bits, _Expansion_Bit) ->

	Bits;

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
expand_to_size_with_expansion_bit(Size_To_Add, Bits, Expansion_Bit) ->
	
	expand_to_size_with_expansion_bit((Size_To_Add - 1) , << (Expansion_Bit)/bits, (Bits)/bits>>, Expansion_Bit).

%%%=======================================================================================
%% @doc 
% Sign extends a group of bits to the requested size<br/>
% ---- Unit-Tested ----<br/>
-spec sign_extend(Bits::binary(),Size::non_neg_integer()) -> binary().
%%%=======================================================================================
sign_extend(Bits,Size) when Size >= 0->

	Bit_Size 			= bit_size(Bits),

	Size_To_Add 		= case Bit_Size < Size of

							true -> Size - Bit_Size;
							_ 	 -> 0
							end,

	<<First:1,_/bits>> 	= Bits,

	Expansion_Bit 		= <<First:1>>,

	expand_to_size_with_expansion_bit(Size_To_Add,Bits,Expansion_Bit).

%%%=======================================================================================
%% @doc 
% Convert the bits to their twos complement<br/>
% ---- Unit-Tested ----<br/>
-spec twos_complement(Bits::binary()) -> <<_:1,_:_*1>>.
%%%=======================================================================================
twos_complement(Bits) ->

	Bit_Size 				= bit_size(Bits),

	Inverted_Bits 			= each_bit(fun inverter/1, Bits),

	One_Bits 				= expand_to_size_with_zeros(<<1:1>>, Bit_Size),

	{_Carry, Result,_Flags} = generic_addition_with_flags(Inverted_Bits, One_Bits),

	Result.

%%%=======================================================================================
%% @doc 
% Performs the inverse twos complement on the bits<br/>
% ---- Unit-Tested ----<br/>
-spec twos_complement_inv(Bits::<<_:_*1>>) -> <<_:_*1>>.
%%%=======================================================================================
twos_complement_inv(Bits) ->

	Bit_Size 				= bit_size(Bits),

	Neg_One 				= duplicate_bit(Bit_Size, <<1:1>>),

	{_Carry, Result,_Flags} = generic_addition_with_flags(Bits, Neg_One),

	each_bit(fun inverter/1, Result).

%%----Unit_Tested-------------------------------------------------------------------------
generic_subtraction_with_flags(Minuend,Subtrahend) ->

	Twos_Complement_Subtrahend = twos_complement(Subtrahend),

	generic_addition_with_flags(Minuend,Twos_Complement_Subtrahend).

%%%=======================================================================================
%% @doc 
% Performs Bitwise AND on two binaries<br/>
% The binaries must be the same length<br/>
% ---- Unit-Tested ----<br/>
-spec and_binary(Binary1::<<_:_*1>>, Binary2::<<_:_*1>>) -> <<_:_*1>>.
%%%=======================================================================================
and_binary(Binary1, Binary2) 
	when 
			bit_size(Binary1) =:= bit_size(Binary2)
	->

	and_binary(Binary1, Binary2, <<>>).

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
and_binary(<<>>, _, Result) ->

	Result;

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
and_binary(Binary1, Binary2, Result) ->

	<<Bit1:1, New_Binary1/bits>> 	= Binary1,
	<<Bit2:1, New_Binary2/bits>> 	= Binary2,

	AND_Result 						= and_gate(<<Bit1:1>>, <<Bit2:1>>),

	and_binary(New_Binary1, New_Binary2, <<Result/bits, AND_Result/bits>>).

%%%=======================================================================================
%% @doc 
% Performs Arithmetic Shift Lift ASL<br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec asl_binary_with_flags(Binary::binary()) -> { <<_:1>>, binary(), map()}.
%%%=======================================================================================
asl_binary_with_flags(Binary) ->

	<<Last_Bit:1, Second_Last:1, _/bits>> 	= Binary,
	<<_:1,Rest/bits>> 						= Binary,

	Result 									= <<Rest/bits, 0:1>>,

	<<Overflow_Flag:1>> 					= xor_gate(<<Last_Bit:1>>,<<Second_Last:1>>),

	Result_Size 							= bit_size(Result),

	Zero_Flag_Status 						= Result =:= <<0:Result_Size>>,

	Zero_Flag 								= case Zero_Flag_Status of 
													true -> 1;
													_ 	 -> 0
												end,

	Carry_Flag 								= Last_Bit,
	
	Sign_Flag								= Second_Last,

	Flags 									= #{
													v => Overflow_Flag,
													z => Zero_Flag,
													n => Sign_Flag,
													c => Carry_Flag
											   },

	{<<Carry_Flag:1>>, Result, Flags}.

%%%=======================================================================================
%% @doc 
% Performs Arithmetic Shift Right ASL<br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec asr_binary_with_flags(Binary::<<_:1,_:_*1>>) -> {<<_:1>>,<<_:1,_:_*1>>,#{'c':=0 | 1, 'n':=0 | 1, 'z':=0 | 1}}.
%%%=======================================================================================
asr_binary_with_flags(Binary) ->

	Binary_Size 									= bit_size(Binary),
	Adjusted_Binary_Size 							= Binary_Size - 1,

	<<Beginning:Adjusted_Binary_Size, Last_Bit:1>> 	= Binary,

	<<First_Bit:1, _/bits>> 						= Binary,

	Result 											= <<First_Bit:1,Beginning:Adjusted_Binary_Size>>,

	Result_Size 									= bit_size(Result),

	Zero_Flag_Status 								= Result =:= <<0:Result_Size>>,

	Zero_Flag 										= case Zero_Flag_Status of 
														true -> 1;
														_ 	 -> 0
													end,

	Carry_Flag 										= Last_Bit,
	
	Sign_Flag										= First_Bit,

	Flags 											= #{
															z => Zero_Flag,
															n => Sign_Flag,
															c => Carry_Flag
													   },

	{<<Carry_Flag:1>>,Result,Flags}.

%%----Unit_Tested-------------------------------------------------------------------------
get_bit(Byte,Num) ->

	<<B7:1, B6:1, B5:1, B4:1, B3:1, B2:1, B1:1, B0:1>> = Byte,

	case Num of

		7 -> <<B7:1>>;
		6 -> <<B6:1>>;
		5 -> <<B5:1>>;
		4 -> <<B4:1>>;
		3 -> <<B3:1>>;
		2 -> <<B2:1>>;
		1 -> <<B1:1>>;
		0 -> <<B0:1>>
	end.

%%----Unit_Tested-------------------------------------------------------------------------
set_bit(Byte,Num,Bit_Value) ->

	<<B7:1, B6:1, B5:1, B4:1, B3:1, B2:1, B1:1, B0:1>> = Byte,

	case Num of

		7 -> <<Bit_Value/bits, B6:1, B5:1, B4:1, B3:1, B2:1, B1:1, B0:1>>;
		6 -> <<B7:1, Bit_Value/bits, B5:1, B4:1, B3:1, B2:1, B1:1, B0:1>>;
		5 -> <<B7:1, B6:1, Bit_Value/bits, B4:1, B3:1, B2:1, B1:1, B0:1>>;
		4 -> <<B7:1, B6:1, B5:1, Bit_Value/bits, B3:1, B2:1, B1:1, B0:1>>;
		3 -> <<B7:1, B6:1, B5:1, B4:1, Bit_Value/bits, B2:1, B1:1, B0:1>>;
		2 -> <<B7:1, B6:1, B5:1, B4:1, B3:1, Bit_Value/bits, B1:1, B0:1>>;
		1 -> <<B7:1, B6:1, B5:1, B4:1, B3:1, B2:1, Bit_Value/bits, B0:1>>;
		0 -> <<B7:1, B6:1, B5:1, B4:1, B3:1, B2:1, B1:1, Bit_Value/bits>>
	end.

%%%=======================================================================================
%% @doc 
% Complement a Binary<br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec complement_binary(Binary::binary()) -> binary().
%%%=======================================================================================
complement_binary(Binary) ->

	each_bit(
				fun inverter/1,
				Binary
		    ).