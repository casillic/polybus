%%%----------------------------------------------------------------------------
%% RAM 64k
%%%----------------------------------------------------------------------------

-module('ram_64k').
-behavior(gen_server).

-compile(export_all).

-export(
		[
			clear_memory/1,
			get_memory/1,
			get_pos/2,
			set_memory/2,
			set_pos_value/3,

			shut_down/1,
			start_link/1,

			set_byte_pos/3,
			set_word_pos/3
		]
		).

-define(MEMORY_SIZE, 	64000).
-define(INITIAL_VALUE, 	0).

%%---------------------------------------------------------------------------%%
%% Start Server
%%---------------------------------------------------------------------------%%
start_link(Name) ->
	gen_server:start_link({local,Name},?MODULE,[],[]).

%%---------------------------------------------------------------------------%%
%% Initialize State
%%---------------------------------------------------------------------------%%
init(_Args) ->

	Initial_State = initalized_memory(),

 {ok, Initial_State}.

%%---------------------------------------------------------------------------%%
initalized_memory() ->
	
	list_to_binary(lists:duplicate(?MEMORY_SIZE,?INITIAL_VALUE)).

%%----Unit_Tested-----------------------------------------------------------------------%%
initalized_memory(Size) ->
	
	list_to_binary(lists:duplicate(Size,?INITIAL_VALUE)).

%%---------------------------------------------------------------------------%%
%% Shutdown Server
%%---------------------------------------------------------------------------%%
shut_down(Name) ->
 	gen_server:call(whereis(Name),terminate).

handle_call(terminate,_From,State) ->
	{stop,normal,ok,State};

%%---------------------------------------------------------------------------%%
%% Handle Calls
%%---------------------------------------------------------------------------%%
handle_call(clear_memory,_From,_State) ->

	New_State = initalized_memory(),

	{reply,New_State,New_State};

handle_call(get_memory,_From,State) ->

	{reply,State,State};

handle_call({set_memory,Memory},_From,_State) ->

 	New_State = Memory,

 	{reply,New_State,New_State};

handle_call({set_pos_value,Position,Value},_From,State) ->

	New_State = set_byte_pos(State,Value,Position),

 	{reply,New_State,New_State};

handle_call({get_pos,Position},_From,State) ->

	Result 	= get_byte_pos(State,Position),

 	{reply,Result,State}.

%%---------------------------------------------------------------------------%%
%% Handle Casts
%%---------------------------------------------------------------------------%%
handle_cast(_, State) -> {noreply,State}.

%%---------------------------------------------------------------------------%%
%% Handle Others
%%---------------------------------------------------------------------------%%
handle_info(_Info, State) -> {noreply,State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%---------------------------------------------------------------------------%%
%% Reset Memory
%%---------------------------------------------------------------------------%%
clear_memory(Name) ->
 	gen_server:call(whereis(Name),clear_memory).

%%---------------------------------------------------------------------------%%
%% Get Memory
%%---------------------------------------------------------------------------%%
get_memory(Name) ->
 	gen_server:call(whereis(Name),get_memory).

get_pos(Name,Position) ->
	gen_server:call(whereis(Name),{get_pos,Position}).

%%---------------------------------------------------------------------------%%
%% Set Memory
%%---------------------------------------------------------------------------%%
set_memory(Name,Memory) ->
 	gen_server:call(whereis(Name),{set_memory,Memory}).

set_pos_value(Name,Position,Value) ->
	gen_server:call(whereis(Name),{set_pos_value,Position,Value}).

%%---------------------------------------------------------------------------%%
%% Terminate Call Back
%%---------------------------------------------------------------------------%%
terminate(Reason,_) ->
	io:format("RAM 64K Server Terminating! Reason:~p~n",[Reason]),
	ok.

%%%----------------------------------------------------------------------------
set_byte_pos(<<>>,Byte,_Pos) ->

	Byte;

set_byte_pos(Data,Byte,<<0:16>>) ->

	Length_Of_Data = byte_size(Data),

	case Length_Of_Data of 

		1 -> 	<<Byte:8>>;
		_ -> 	<<_:8,Rest/binary>> = Data,
				<<Byte/binary,Rest/binary>>
		end;

set_byte_pos(Data,Byte,<<Pos:16>>) when Pos =:= (byte_size(Data) - 1) ->

	Length_Of_Data 	= byte_size(Data),

	case Length_Of_Data of

		1 -> 	Byte;

		_ -> 	First_Length 	= Length_Of_Data - 1,

				Rest 			= binary:part(Data,{0,First_Length}),

				<<Rest/binary,Byte/binary>>
	    end;

set_byte_pos(Data,Byte,Position) ->

	<<Pos:16>> 		= Position,
	
	Length_Of_Data 	= byte_size(Data),

	First_Part 		= binary:part(Data,{0,Pos}),

	Second_Part 	= binary:part(Data,{Pos+1,Length_Of_Data-(Pos+1)}),

	<<
		First_Part/binary,
		Byte/bits,
		Second_Part/binary
	>>.

%%%----Unit_Tested------------------------------------------------------------------------
set_word_pos(Data,Word,Position) ->

	<<High_Byte:8,Low_Byte:8>> 	= Word,

	<<High_Address_Value:16>> 	= Position,

	Low_Address_Value 			= High_Address_Value + 1, 

	High_Adjusted_Data 			= set_byte_pos(Data,<<High_Byte:8>>,Position),

	set_byte_pos(High_Adjusted_Data,<<Low_Byte:8>>,<<Low_Address_Value:16>>).

%%%----------------------------------------------------------------------------
get_byte_pos(Data,Pos) ->

	<<Position:16>> = Pos,

	binary:part(Data,Position,1).

%%%----Unit_Tested------------------------------------------------------------------------
memory_immediate(Op_Code,Immediate) ->

	<<Op_Code/bits,Immediate/bits>>.

%%%----Unit_Tested------------------------------------------------------------------------
memory_direct(Op_Code,Direct) ->

	<<Op_Code/bits,Direct/bits>>.

%%%-----Unit_Tested-----------------------------------------------------------------------
memory_extended(Op_Code,Value,Location)  
	when  
		  bit_size(Op_Code)  =:= 8,
		  bit_size(Location) =:= 16
	 ->

	<<Loc_Number:16>> 	= Location,

	Fill_Size 			= Loc_Number - 3,

	Fill_Memory 		= initalized_memory(Fill_Size),
	
	<<Op_Code/bits,Location/bits,Fill_Memory/bits,Value/bits>>;

memory_extended(Op_Code,Value,Location)  
	when  
			bit_size(Op_Code)  =:= 16,
			bit_size(Location) =:= 16
	 ->

	<<Loc_Number:16>> 	= Location,

	Fill_Size 			= Loc_Number - 4,

	Fill_Memory 		= initalized_memory(Fill_Size),
	
	<<Op_Code/bits,Location/bits,Fill_Memory/bits,Value/bits>>.
