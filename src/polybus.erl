%%%----------------------------------------------------------------------------
%% Polybus
%%%----------------------------------------------------------------------------

-module('polybus').

-compile(export_all).

%%%----------------------------------------------------------------------------
set_byte_pos(<<>>,Byte,_Pos) ->

	<<Byte:8>>;

set_byte_pos(Data,Byte,0) ->

	Length_Of_Data = byte_size(Data),

	case Length_Of_Data of 

		1 -> 	<<Byte:8>>;
		_ -> 	<<_:8,Rest/binary>> = Data,
				<<Byte:8,Rest/binary>>
		end;

set_byte_pos(Data,Byte,Pos) when Pos =:= (byte_size(Data) - 1) ->

	Length_Of_Data 	= byte_size(Data),

	Byte_Data 		= <<Byte:8>>,

	case Length_Of_Data of

		1 -> 	Byte_Data;

		_ -> 	First_Length = Length_Of_Data - 1,

				Rest = binary:part(Data,{0,First_Length}),

				<<Rest/binary,Byte_Data/binary>>
	    end;

set_byte_pos(Data,Byte,Pos) ->

	Length_Of_Data 	= byte_size(Data),

	First_Part 		= binary:part(Data,{0,Pos}),

	Second_Part 	= binary:part(Data,{Pos+1,Length_Of_Data-(Pos+1)}),

	<<
		First_Part/binary,
		Byte:8,
		Second_Part/binary
	>>.

%%%----------------------------------------------------------------------------
get_byte_pos(Data,Pos) ->

	binary:part(Data,Pos,1).