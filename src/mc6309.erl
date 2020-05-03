%%%----------------------------------------------------------------------------
%% MC6309
%%%----------------------------------------------------------------------------

%% @doc MC6309 CPU Emulator
%% @end

-module('mc6309').

-include("reg_size.hrl").
-include("inter_reg_bits.hrl").

-compile([export_all]).

-define(SET_RESET(VALUE), 		fun(CPU) -> cpu_set_reset(VALUE,CPU) 	end).
-define(SET_NMI(VALUE), 		fun(CPU) -> cpu_set_nmi(VALUE,CPU) 		end).
-define(SET_SWI(VALUE), 		fun(CPU) -> cpu_set_swi(VALUE,CPU) 		end).
-define(SET_IRQ(VALUE), 		fun(CPU) -> cpu_set_irq(VALUE,CPU) 		end).
-define(SET_FIRQ(VALUE), 		fun(CPU) -> cpu_set_firq(VALUE,CPU) 	end).
-define(SET_SWI2(VALUE), 		fun(CPU) -> cpu_set_swi2(VALUE,CPU)  	end).
-define(SET_SWI3(VALUE), 		fun(CPU) -> cpu_set_swi3(VALUE,CPU)  	end).
-define(SET_RESERVED(VALUE), 	fun(CPU) -> cpu_set_reserved(VALUE,CPU) end).
-define(SET_HALT(VALUE), 	    fun(CPU) -> cpu_set_halt(VALUE,CPU)  	end).
-define(SET_Q(VALUE), 	        fun(CPU) -> cpu_set_q(VALUE,CPU)  		end).
-define(SET_D(VALUE), 	        fun(CPU) -> cpu_set_d(VALUE,CPU)  		end).
-define(SET_W(VALUE), 	        fun(CPU) -> cpu_set_w(VALUE,CPU)  		end).
-define(SET_A(VALUE), 	        fun(CPU) -> cpu_set_a(VALUE,CPU)  		end).
-define(SET_B(VALUE), 	        fun(CPU) -> cpu_set_b(VALUE,CPU)  		end).
-define(SET_E(VALUE), 	        fun(CPU) -> cpu_set_e(VALUE,CPU)  		end).
-define(SET_F(VALUE), 	        fun(CPU) -> cpu_set_f(VALUE,CPU)  		end).
-define(SET_X(VALUE), 	        fun(CPU) -> cpu_set_x(VALUE,CPU)  		end).
-define(SET_Y(VALUE), 	        fun(CPU) -> cpu_set_y(VALUE,CPU)  		end).
-define(SET_U(VALUE), 	        fun(CPU) -> cpu_set_u(VALUE,CPU)  		end).
-define(SET_S(VALUE), 	        fun(CPU) -> cpu_set_s(VALUE,CPU)  		end).
-define(SET_PC(VALUE), 	        fun(CPU) -> cpu_set_pc(VALUE,CPU)  		end).
-define(SET_V(VALUE), 	        fun(CPU) -> cpu_set_v(VALUE,CPU)        end).
-define(SET_ZERO(VALUE), 	    fun(CPU) -> cpu_set_zero(VALUE,CPU)     end).
-define(SET_DP(VALUE), 	        fun(CPU) -> cpu_set_dp(VALUE,CPU)       end).
-define(SET_CC(VALUE), 	        fun(CPU) -> cpu_set_cc(VALUE,CPU)       end).
-define(SET_CC_E(VALUE), 	    fun(CPU) -> cpu_set_cc_e(VALUE,CPU)     end).
-define(SET_CC_F(VALUE), 	    fun(CPU) -> cpu_set_cc_f(VALUE,CPU)     end).
-define(SET_CC_H(VALUE), 	    fun(CPU) -> cpu_set_cc_h(VALUE,CPU)     end).
-define(SET_CC_I(VALUE), 	    fun(CPU) -> cpu_set_cc_i(VALUE,CPU)     end).
-define(SET_CC_N(VALUE), 	    fun(CPU) -> cpu_set_cc_n(VALUE,CPU)     end).
-define(SET_CC_Z(VALUE), 	    fun(CPU) -> cpu_set_cc_z(VALUE,CPU)     end).
-define(SET_CC_V(VALUE), 	    fun(CPU) -> cpu_set_cc_v(VALUE,CPU)     end).
-define(SET_CC_C(VALUE), 	    fun(CPU) -> cpu_set_cc_c(VALUE,CPU)     end).
-define(SET_MD(VALUE), 	        fun(CPU) -> cpu_set_md(VALUE,CPU)       end).
-define(SET_MD_DIV0(VALUE), 	fun(CPU) -> cpu_set_md_div0(VALUE,CPU)  end).
-define(SET_MD_IL(VALUE), 	    fun(CPU) -> cpu_set_md_il(VALUE,CPU)    end).
-define(SET_MD_FM(VALUE), 	    fun(CPU) -> cpu_set_md_fm(VALUE,CPU)    end).
-define(SET_MD_NM(VALUE), 	    fun(CPU) -> cpu_set_md_nm(VALUE,CPU)    end).

-define(UPDATE_FLAGS(MAP), 		fun(CPU) -> update_condition_codes_from_map(MAP,CPU) end).

-type cpu_type() 			:: <<_:184>>.
-type position_type() 		:: <<_:16>>.
-type address_type() 		:: <<_:16>>.
-type q_reg_type() 			:: <<_:32>>.
-type d_reg_type() 			:: <<_:16>>.
-type a_reg_type() 			:: <<_:8>>.
-type b_reg_type() 			:: <<_:8>>.
-type w_reg_type() 			:: <<_:16>>.
-type e_reg_type() 			:: <<_:8>>.
-type f_reg_type() 			:: <<_:8>>.
-type x_reg_type() 			:: <<_:16>>.
-type y_reg_type() 			:: <<_:16>>.
-type u_reg_type() 			:: <<_:16>>.
-type s_reg_type() 			:: <<_:16>>.
-type pc_reg_type() 		:: <<_:16>>.
-type v_reg_type() 			:: <<_:16>>.
-type zero_reg_type() 		:: <<_:16>>.
-type dp_reg_type() 		:: <<_:8>>.
-type cc_reg_type() 		:: <<_:8>>.
-type cc_e_reg_type() 		:: <<_:1>>.
-type cc_f_reg_type() 		:: <<_:1>>.
-type cc_h_reg_type() 		:: <<_:1>>.
-type cc_i_reg_type() 		:: <<_:1>>.
-type cc_n_reg_type() 		:: <<_:1>>.
-type cc_z_reg_type() 		:: <<_:1>>.
-type cc_v_reg_type() 		:: <<_:1>>.
-type cc_c_reg_type() 		:: <<_:1>>.
-type md_reg_type() 		:: <<_:8>>.
-type md_div0_reg_type() 	:: <<_:1>>.
-type md_il_reg_type() 		:: <<_:1>>.
-type md_fm_reg_type() 		:: <<_:1>>.
-type md_nm_reg_type() 		:: <<_:1>>.
-type reset_reg_type() 		:: <<_:1>>.
-type nmi_reg_type() 		:: <<_:1>>.
-type swi_reg_type() 		:: <<_:1>>.
-type irq_reg_type() 		:: <<_:1>>.
-type firq_reg_type() 		:: <<_:1>>.
-type swi2_reg_type() 		:: <<_:1>>.
-type swi3_reg_type() 		:: <<_:1>>.
-type reserved_reg_type() 	:: <<_:1>>.
-type halt_reg_type() 		:: <<_:1>>.

%%%----------------------------------------------------------------------------
get_byte_pos(Data, Pos) ->

	<<Position:16>> = Pos,

	binary:part(Data, Position, 1).

%%-----------------------------------------------------------------------------
set_byte_pos(Data, Byte, Position) ->

	ram_64k:set_byte_pos(
							Data,
							Byte,
							Position
						  ).

%%=============================================================================
-spec cpu_clear() -> cpu_type().
%%----Unit_Tested--------------------------------------------------------------
cpu_clear() ->
						<<  %=========================================================================|
							% |-----------------------Q----------------------|
							% |-----------D-----------|----------W-----------|  
							% |-----A-----|----B------|----E-----|----F------|                      
						    	0:?SIZE_A,  0:?SIZE_B,  0:?SIZE_E, 0:?SIZE_F,
							% |-----------X-----------|
							        0:?SIZE_X,
							% |-----------Y-----------|
							        0:?SIZE_Y,
							% |-----------U-----------|
							        0:?SIZE_U,
							% |-----------S-----------|
							        0:?SIZE_S,
							% |-----------PC----------|
							        0:?SIZE_PC,
							% |-----------V-----------|
							        0:?SIZE_V,
							% |---------ZERO----------|
							        0:?SIZE_ZERO, 
							% |----DP-----|                      
						    	0:?SIZE_DP,
							% |----CC-----|
							% |--E-----------|--F-----------|--H-----------|--I-----------|--N-----------|--Z-----------|--V-----------|--C-----------|                      
						    	0:?SIZE_CC_E,  0:?SIZE_CC_F,  0:?SIZE_CC_H,  0:?SIZE_CC_I,  0:?SIZE_CC_N,  0:?SIZE_CC_Z,  0:?SIZE_CC_V,  0:?SIZE_CC_C, 
							% |----MD-----|
							% |-div0------------|--il-----------|-------|-------|--------|--------|--FM-----------|--NM------------|                      
						    	0:?SIZE_MD_DIV0,  0:?SIZE_MD_IL,    0:1,    0:1,    0:1,     0:1,   0:?SIZE_MD_FM,   0:?SIZE_MD_NM,
						    % |-reset---------|--nmi--------|--swi--------|--irq--------|--firq--------|--swi2--------|--swi3---------|--reserved---------| 
							    1:?SIZE_RESET,  1:?SIZE_NMI,  0:?SIZE_SWI,  1:?SIZE_IRQ,  1:?SIZE_FIRQ,  0:?SIZE_SWI2,   0:?SIZE_SWI3,   0:?SIZE_RESERVED,
							% |-halt----------|-------|-------|-------|--------|--------|--------|------------|
							    0:?SIZE_HALT,    0:1,    0:1,    0:1,    0:1,     0:1,     0:1,     0:1
						>>. %=========================================================================|
						
%%-----------------------------------------------------------------------------
dont_care() -> 
				<<  %=========================================================================|
					% |-----------------------Q----------------------|
					% |-----------D-----------|----------W-----------|  
					% |-----A-----|----B------|----E-----|----F------|                      
				    	_:8,    	_:8,    	_:8,   	   _:8,
					% |-----------X-----------|
					        	_:16,
					% |-----------Y-----------|
					        	_:16,
					% |-----------U-----------|
					            _:16,
					% |-----------S-----------|
					            _:16,
					% |-----------PC----------|
					            _:16,
					% |-----------V-----------|
					            _:16,
					% |---------ZERO----------|
					            _:16, 
					% |----DP-----|                      
				    	_:8,
					% |----CC-----|
					% |--E--|--F--|--H--|--I--|--N--|--Z--|--V--|--C--|                      
				    	_:1,  _:1,  _:1,  _:1,  _:1,  _:1,  _:1,  _:1, 
					% |----MD-----|
					% |-div0--|--il---|-------|-------|--------|--------|--FM----|--NM--------|                      
				    	_:1,    _:1,    _:1,    _:1,    _:1,     _:1,     _:1,     _:1,
				    % |-reset-|--nmi--|--swi--|--irq--|--firq--|--swi2--|--swi3--|--reserved--| 
					    _:1,    _:1,    _:1,    _:1,    _:1,     _:1,     _:1,     _:1,
					% |-halt--|-------|-------|-------|--------|--------|--------|------------|
					    _:1,    _:1,    _:1,    _:1,    _:1,     _:1,     _:1,     _:1
				>>
				= cpu_clear().

%%-----------------------------------------------------------------------------
pretty_print(CPU) ->

	<<  %=========================================================================|
		% |-----------------------Q----------------------|
		% |-----------D-----------|----------W-----------|  
		% |-----A-----|----B------|----E-----|----F------|                      
	    	  A:8,     	B:8,     	E:8,    	F:8,
		% |-----------X-----------|
		        	X:16,
		% |-----------Y-----------|
		        	Y:16,
		% |-----------U-----------|
		            U:16,
		% |-----------S-----------|
		            S:16,
		% |-----------PC----------|
		            PC:16,
		% |-----------V-----------|
		            V:16,
		% |---------ZERO----------|
		            ZERO:16, 
		% |----DP-----|                      
	    	DP:8,
		% |----CC-----|
		% |----E----|--F-----|--H-----|--I-----|--N-----|--Z-----|--V-----|--C-----|                      
	      	CC_E:1,	  CC_F:1,  CC_H:1,  CC_I:1,  CC_N:1,  CC_Z:1,  CC_V:1,  CC_C:1, 
		% |----MD-----|
		% |-div0------|--il-------|---------|-------|--------|--------|--FM--------|--NM--------|                      
	    	MD_DIV0:1,    MD_IL:1,    _:1,    _:1,    _:1,     _:1,      MD_FM:1,     MD_NM:1,
	    % |-reset-----|--nmi------|--swi----|--irq--|--firq--|--swi2--|--sw13------|--reserved--| 
		    Reset:1,      NMI:1,      SWI:1,  IRQ:1,  FIRQ:1, SWI2:1,    SWI3:1, 	 Reserved:1,
		% |-halt------|-----------|---------|-------|--------|--------|------------|------------|
		    Halt:1,       _:1,        _:1,    _:1,    _:1,     _:1,      _:1,         _:1
	>> = CPU,

	io:format(
	  "|========================================================================|~n" ++
	  "|-----------------------Q----------------------|~n" ++
	  "|-----------D-----------|----------W-----------|~n" ++ 
	  "|-----A-----|----B------|----E-----|----F------|~n" ++                     
	  "     ~.16#    	 ~.16#        ~.16#       ~.16#~n" ++
	  "|-----------X-----------|~n" ++
	  "            ~.16#~n" ++
	  "|-----------Y-----------|~n" ++
	  "            ~.16#~n" ++
	  "|-----------U-----------|~n" ++
	  "            ~.16#~n" ++
	  "|-----------S-----------|~n" ++
	  "            ~.16#~n" ++
	  "|-----------PC----------|~n" ++
	  "            ~.16#~n" ++
	  "|-----------V-----------|~n" ++
	  "            ~.16#~n" ++
	  "|---------ZERO----------|~n" ++
	  "            ~.16#~n" ++
	  "|----DP-----|~n" ++                      
	  "     ~.16#~n" ++
	  "|----------------------CC-----------------------|~n" ++
	  "|--E--|--F--|--H--|--I--|--N--|--Z--|--V--|--C--|~n" ++  
	  "   ~p     ~p     ~p     ~p     ~p     ~p     ~p     ~p~n" ++
	  "|---------------MD--------------|~n" ++
	  "|-div0--|--il---|--FM---|--NM---|~n" ++
	  "   ~p        ~p       ~p       ~p~n" ++
	  "|-reset-|--nmi--|--swi--|--irq--|--firq--|--swi2--|--swi3--|--reserved--|~n" ++
	  "   ~p        ~p       ~p       ~p       ~p         ~p       ~p         ~p~n" ++
	  "|-halt--|~n" ++
	  "    ~p                                                             ~n" ++
	  "|========================================================================|~n",
	
	
			   [
			     A,B,E,F,X,Y,U,S,PC,V,ZERO,DP,CC_E,CC_F,CC_H,CC_I,CC_N,CC_Z,CC_V,CC_C, 
				 MD_DIV0,MD_IL,MD_FM,MD_NM,Reset,NMI,SWI,IRQ,FIRQ,SWI2,SWI3,Reserved,Halt
			   ]
			 ).

%%%=======================================================================================
%% @doc 
% Gets the Q Register bits from CPU_Data<br/>
% Returns: 32 bit binary<br/>
% ---- Unit-Tested ----<br/>
%% @end
-spec cpu_get_q(CPU_Data::cpu_type()) -> q_reg_type().
%%%=======================================================================================
cpu_get_q(CPU_Data) ->

	<<Q:?SIZE_Q, _/binary>> = CPU_Data,
 	<<Q:?SIZE_Q>>.

%%%=======================================================================================
%% @doc 
% Gets the D Register bits from CPU_Data<br/>
% Returns: 16 bit binary<br/>
% ---- Unit-Tested ----<br/>
%% @end
-spec cpu_get_d(CPU_Data::cpu_type()) -> d_reg_type().
%%%=======================================================================================
cpu_get_d(CPU_Data) ->

	<<D:?SIZE_D, _/binary>> = CPU_Data,
	<<D:?SIZE_D>>.

%%%=======================================================================================
%% @doc 
% Gets the A Register bits from CPU_Data<br/>
% Returns: 8 bit binary<br/>
% ---- Unit-Tested ----<br/>
%% @end
-spec cpu_get_a(CPU_Data::cpu_type()) -> a_reg_type().
%%%=======================================================================================
cpu_get_a(CPU_Data) ->

	<<A:?SIZE_A, _/binary>> = CPU_Data,
	<<A:?SIZE_A>>.

%%%=======================================================================================
%% @doc 
% Gets the B Register bits from CPU_Data<br/>
% Returns: 8 bit binary<br/>
% ---- Unit-Tested ----<br/>
%% @end
-spec cpu_get_b(CPU_Data::cpu_type()) -> b_reg_type().
%%%=======================================================================================
cpu_get_b(CPU_Data) ->

	<<_A:?SIZE_A, B:?SIZE_B, _/binary>> = CPU_Data,
	<<B:?SIZE_B>>.

%%%=======================================================================================
%% @doc 
% Gets the W Register bits from CPU_Data<br/>
% Returns: 16 bit binary<br/>
% ---- Unit-Tested ----<br/>
%% @end
-spec cpu_get_w(CPU_Data::cpu_type()) -> w_reg_type().
%%%=======================================================================================
cpu_get_w(CPU_Data) ->

	<<_D:?SIZE_D, W:?SIZE_W,_/binary>> = CPU_Data,
	<<W:?SIZE_W>>.

%%%=======================================================================================
%% @doc 
% Gets the E Register bits from CPU_Data<br/>
% Returns: 8 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_e(CPU_Data::cpu_type()) -> e_reg_type().
%%%=======================================================================================
cpu_get_e(CPU_Data) ->

	<<_D:?SIZE_D, E:?SIZE_E,_/binary>> = CPU_Data,
	<<E:?SIZE_E>>.

%%%=======================================================================================
%% @doc 
% Gets the F Register bits from CPU_Data<br/>
% Returns: 8 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_f(CPU_Data::cpu_type()) -> f_reg_type().
%%%=======================================================================================
cpu_get_f(CPU_Data) ->

	<<_D:?SIZE_D, _E:?SIZE_E, F:?SIZE_F,_/binary>> = CPU_Data,
	<<F:?SIZE_F>>.

%%%=======================================================================================
%% @doc 
% Gets the X Register bits from CPU_Data<br/>
% Returns: 16 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_x(CPU_Data::cpu_type()) -> x_reg_type().
%%%=======================================================================================
cpu_get_x(CPU_Data) ->

	<<_Q:?SIZE_Q, X:?SIZE_X, _/binary>> = CPU_Data,
	<<X:?SIZE_X>>.

%%%=======================================================================================
%% @doc 
% Gets the Y Register bits from CPU_Data<br/>
% Returns: 16 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_y(CPU_Data::cpu_type()) -> y_reg_type().
%%%=======================================================================================
cpu_get_y(CPU_Data) ->

	<<_Q:?SIZE_Q, _X:?SIZE_X, Y:?SIZE_Y, _/binary>> = CPU_Data,
	<<Y:?SIZE_Y>>.

%%%=======================================================================================
%% @doc 
% Gets the U Register bits from CPU_Data<br/>
% Returns: 16 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_u(CPU_Data::cpu_type()) -> u_reg_type().
%%%=======================================================================================
cpu_get_u(CPU_Data) ->

	<<_Q:?SIZE_Q, _X:?SIZE_X, _Y:?SIZE_Y, U:?SIZE_U,_/binary>> = CPU_Data,
	<<U:?SIZE_U>>.

%%%=======================================================================================
%% @doc 
% Gets the S Register bits from CPU_Data<br/>
% Returns: 16 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_s(CPU_Data::cpu_type()) -> s_reg_type().
%%%=======================================================================================
cpu_get_s(CPU_Data) ->

	<<_Q:?SIZE_Q, _X:?SIZE_X, _Y:?SIZE_Y, _U:?SIZE_U, S:?SIZE_S, _/binary>> = CPU_Data,
	<<S:?SIZE_S>>.

%%%=======================================================================================
%% @doc 
% Gets the PC Register bits from CPU_Data<br/>
% Returns: 16 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_pc(CPU_Data::cpu_type()) -> pc_reg_type().
%%%=======================================================================================
cpu_get_pc(CPU_Data) ->

	<<_Q:?SIZE_Q, _X:?SIZE_X, _Y:?SIZE_Y, _U:?SIZE_U, _S:?SIZE_S, PC:?SIZE_PC, _/binary>> = CPU_Data,
	<<PC:?SIZE_PC>>.

%%%=======================================================================================
%% @doc 
% Gets the V Register bits from CPU_Data<br/>
% Returns: 16 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_v(CPU_Data::cpu_type()) -> v_reg_type().
%%%=======================================================================================
cpu_get_v(CPU_Data) ->

	<<_Q:?SIZE_Q, _X:?SIZE_X, _Y:?SIZE_Y, _U:?SIZE_U, _S:?SIZE_S, _PC:?SIZE_PC, V:?SIZE_V, _/binary>> = CPU_Data,
	<<V:?SIZE_V>>.

%%%=======================================================================================
%% @doc 
% Gets the Zero Register bits from CPU_Data<br/>
% Returns: 16 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_zero(CPU_Data::cpu_type()) -> zero_reg_type().
%%%=======================================================================================
cpu_get_zero(CPU_Data) ->

	<<_Q:?SIZE_Q, _X:?SIZE_X, _Y:?SIZE_Y, _U:?SIZE_U, _S:?SIZE_S, _PC:?SIZE_PC, _V:?SIZE_V, Zero:?SIZE_ZERO, _/binary>> = CPU_Data,
	<<Zero:?SIZE_ZERO>>.

%%%=======================================================================================
%% @doc 
% Gets the DP Register bits from CPU_Data<br/>
% Returns: 8 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_dp(CPU_Data::cpu_type()) -> dp_reg_type().
%%%=======================================================================================
cpu_get_dp(CPU_Data) ->

	<<_Q:?SIZE_Q, _X:?SIZE_X, _Y:?SIZE_Y, _U:?SIZE_U, _S:?SIZE_S, _PC:?SIZE_PC, _V:?SIZE_V, _Zero:?SIZE_ZERO, DP:?SIZE_DP, _/binary>> = CPU_Data,
	<<DP:?SIZE_DP>>.

%%%=======================================================================================
%% @doc 
% Gets the CC Register bits from CPU_Data<br/>
% Returns: 8 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_cc(CPU_Data::cpu_type()) -> cc_reg_type().
%%%=======================================================================================
cpu_get_cc(CPU_Data) ->

	<<_Q:?SIZE_Q, _X:?SIZE_X, _Y:?SIZE_Y, _U:?SIZE_U, _S:?SIZE_S, _PC:?SIZE_PC, _V:?SIZE_V, _Zero:?SIZE_ZERO, _DP:?SIZE_DP, CC:?SIZE_CC, _/binary>> = CPU_Data,
	<<CC:?SIZE_CC>>.

%%%=======================================================================================
%% @doc 
% Gets the CC Register E bit from CPU_Data<br/>
% Returns: 1 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_cc_e(CPU_Data::cpu_type()) -> cc_e_reg_type().
%%%=======================================================================================
cpu_get_cc_e(CPU_Data) ->

	<<_Q:?SIZE_Q, _X:?SIZE_X, _Y:?SIZE_Y, _U:?SIZE_U, _S:?SIZE_S, _PC:?SIZE_PC, _V:?SIZE_V, _Zero:?SIZE_ZERO, _DP:?SIZE_DP, CC:?SIZE_CC, _/binary>> = CPU_Data,
	<<CC_E:?SIZE_CC_E, _:7>> = <<CC>>,
	<<CC_E:?SIZE_CC_E>>.

%%%=======================================================================================
%% @doc 
% Gets the CC Register F bit from CPU_Data<br/>
% Returns: 1 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_cc_f(CPU_Data::cpu_type()) -> cc_f_reg_type().
%%%=======================================================================================
cpu_get_cc_f(CPU_Data) ->

	<<_Q:?SIZE_Q, _X:?SIZE_X, _Y:?SIZE_Y, _U:?SIZE_U, _S:?SIZE_S, _PC:?SIZE_PC, _V:?SIZE_V, _Zero:?SIZE_ZERO, _DP:?SIZE_DP, CC:?SIZE_CC, _/binary>> = CPU_Data,
	<<_CC_E:?SIZE_CC_E, CC_F:?SIZE_CC_F, _:6>> = <<CC>>,
	<<CC_F:?SIZE_CC_F>>.

%%%=======================================================================================
%% @doc 
% Gets the CC Register H bit from CPU_Data<br/>
% Returns: 1 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_cc_h(CPU_Data::cpu_type()) -> cc_h_reg_type().
%%%=======================================================================================
cpu_get_cc_h(CPU_Data) ->

	<<_Q:?SIZE_Q, _X:?SIZE_X, _Y:?SIZE_Y, _U:?SIZE_U, _S:?SIZE_S, _PC:?SIZE_PC, _V:?SIZE_V, _Zero:?SIZE_ZERO, _DP:?SIZE_DP, CC:?SIZE_CC, _/binary>> = CPU_Data,
	<<_CC_E:?SIZE_CC_E, _CC_F:?SIZE_CC_F, CC_H:?SIZE_CC_H, _:5>> = <<CC>>,
	<<CC_H:?SIZE_CC_H>>.

%%%=======================================================================================
%% @doc 
% Gets the CC Register I bit from CPU_Data<br/>
% Returns: 1 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_cc_i(CPU_Data::cpu_type()) -> cc_i_reg_type().
%%%=======================================================================================
cpu_get_cc_i(CPU_Data) ->

	<<_Q:?SIZE_Q, _X:?SIZE_X, _Y:?SIZE_Y, _U:?SIZE_U, _S:?SIZE_S, _PC:?SIZE_PC, _V:?SIZE_V, _Zero:?SIZE_ZERO, _DP:?SIZE_DP, CC:?SIZE_CC, _/binary>> = CPU_Data,
	<<_CC_E:?SIZE_CC_E, _CC_F:?SIZE_CC_F, _CC_H:?SIZE_CC_H, CC_I:?SIZE_CC_I, _:4>> = <<CC>>,
	<<CC_I:?SIZE_CC_I>>.

%%%=======================================================================================
%% @doc 
% Gets the CC Register N bit from CPU_Data<br/>
% Returns: 1 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_cc_n(CPU_Data::cpu_type()) -> cc_n_reg_type().
%%%=======================================================================================
cpu_get_cc_n(CPU_Data) ->

	<<_Q:?SIZE_Q, _X:?SIZE_X, _Y:?SIZE_Y, _U:?SIZE_U, _S:?SIZE_S, _PC:?SIZE_PC, _V:?SIZE_V, _Zero:?SIZE_ZERO, _DP:?SIZE_DP, CC:?SIZE_CC, _/binary>> = CPU_Data,
	<<_CC_E:?SIZE_CC_E, _CC_F:?SIZE_CC_F, _CC_H:?SIZE_CC_H, _CC_I:?SIZE_CC_I, CC_N:?SIZE_CC_N, _:3>> = <<CC>>,
	<<CC_N:?SIZE_CC_N>>.

%%%=======================================================================================
%% @doc 
% Gets the CC Register Z bit from CPU_Data<br/>
% Returns: 1 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_cc_z(CPU_Data::cpu_type()) -> cc_z_reg_type().
%%%=======================================================================================
cpu_get_cc_z(CPU_Data) ->

	<<_Q:?SIZE_Q, _X:?SIZE_X, _Y:?SIZE_Y, _U:?SIZE_U, _S:?SIZE_S, _PC:?SIZE_PC, _V:?SIZE_V, _Zero:?SIZE_ZERO, _DP:?SIZE_DP, CC:?SIZE_CC, _/binary>> = CPU_Data,
	<<_CC_E:?SIZE_CC_E, _CC_F:?SIZE_CC_F, _CC_H:?SIZE_CC_H, _CC_I:?SIZE_CC_I, _CC_N:?SIZE_CC_N, CC_Z:?SIZE_CC_Z, _:2>> = <<CC>>,
	<<CC_Z:?SIZE_CC_Z>>.

%%%=======================================================================================
%% @doc 
% Gets the CC Register V bit from CPU_Data<br/>
% Returns: 1 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_cc_v(CPU_Data::cpu_type()) -> cc_v_reg_type().
%%%=======================================================================================
cpu_get_cc_v(CPU_Data) ->

	<<_Q:?SIZE_Q, _X:?SIZE_X, _Y:?SIZE_Y, _U:?SIZE_U, _S:?SIZE_S, _PC:?SIZE_PC, _V:?SIZE_V, _Zero:?SIZE_ZERO, _DP:?SIZE_DP, CC:?SIZE_CC, _/binary>> = CPU_Data,
	<<_CC_E:?SIZE_CC_E, _CC_F:?SIZE_CC_F, _CC_H:?SIZE_CC_H, _CC_I:?SIZE_CC_I, _CC_N:?SIZE_CC_N, _CC_Z:?SIZE_CC_Z, CC_V:?SIZE_CC_V, _:1>> = <<CC>>,
	<<CC_V:?SIZE_CC_V>>.

%%%=======================================================================================
%% @doc 
% Gets the CC Register C bit from CPU_Data<br/>
% Returns: 1 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_cc_c(CPU_Data::cpu_type()) -> cc_c_reg_type().
%%%=======================================================================================
cpu_get_cc_c(CPU_Data) ->

	<<_Q:?SIZE_Q, _X:?SIZE_X, _Y:?SIZE_Y, _U:?SIZE_U, _S:?SIZE_S, _PC:?SIZE_PC, _V:?SIZE_V, _Zero:?SIZE_ZERO, _DP:?SIZE_DP, CC:?SIZE_CC, _/binary>> = CPU_Data,
	<<_CC_E:?SIZE_CC_E, _CC_F:?SIZE_CC_F, _CC_H:?SIZE_CC_H, _CC_I:?SIZE_CC_I, _CC_N:?SIZE_CC_N, _CC_Z:?SIZE_CC_Z, _CC_V:?SIZE_CC_V, CC_C:?SIZE_CC_C>> = <<CC>>,
	<<CC_C:?SIZE_CC_C>>.

%%%=======================================================================================
%% @doc 
% Gets the MD Register bits from CPU_Data<br/>
% Returns: 8 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_md(CPU_Data::cpu_type()) -> md_reg_type().
%%%=======================================================================================
cpu_get_md(CPU_Data) ->

	<<_Q:?SIZE_Q, _X:?SIZE_X, _Y:?SIZE_Y, _U:?SIZE_U, _S:?SIZE_S, _PC:?SIZE_PC, _V:?SIZE_V, _Zero:?SIZE_ZERO, _DP:?SIZE_DP, _CC:?SIZE_CC, MD:?SIZE_MD, _/binary>> = CPU_Data,
	<<MD:?SIZE_MD>>.

%%%=======================================================================================
%% @doc 
% Gets the MD Register DIV0 bit from CPU_Data<br/>
% Returns: 1 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_md_div0(CPU_Data::cpu_type()) -> md_div0_reg_type().
%%%=======================================================================================
cpu_get_md_div0(CPU_Data) ->

	<<_Q:?SIZE_Q, _X:?SIZE_X, _Y:?SIZE_Y, _U:?SIZE_U, _S:?SIZE_S, _PC:?SIZE_PC, _V:?SIZE_V, _Zero:?SIZE_ZERO, _DP:?SIZE_DP, _CC:?SIZE_CC, MD:?SIZE_MD, _/binary>> = CPU_Data,
	<<MD_Div0:?SIZE_MD_DIV0, _:7>> = <<MD>>,
	<<MD_Div0:?SIZE_MD_DIV0>>.

%%%=======================================================================================
%% @doc 
% Gets the MD Register IL bit from CPU_Data<br/>
% Returns: 1 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_md_il(CPU_Data::cpu_type()) -> md_il_reg_type().
%%%=======================================================================================
cpu_get_md_il(CPU_Data) ->

	<<_Q:?SIZE_Q, _X:?SIZE_X, _Y:?SIZE_Y, _U:?SIZE_U, _S:?SIZE_S, _PC:?SIZE_PC, _V:?SIZE_V, _Zero:?SIZE_ZERO, _DP:?SIZE_DP, _CC:?SIZE_CC, MD:?SIZE_MD, _/binary>> = CPU_Data,
	<<_MD_Div0:?SIZE_MD_DIV0, MD_IL:?SIZE_MD_IL, _:6>> = <<MD>>,
	<<MD_IL:?SIZE_MD_IL>>.

%%%=======================================================================================
%% @doc 
% Gets the MD Register FM bit from CPU_Data<br/>
% Returns: 1 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_md_fm(CPU_Data::cpu_type()) -> md_fm_reg_type().
%%%=======================================================================================
cpu_get_md_fm(CPU_Data) ->

	<<_Q:?SIZE_Q, _X:?SIZE_X, _Y:?SIZE_Y, _U:?SIZE_U, _S:?SIZE_S, _PC:?SIZE_PC, _V:?SIZE_V, _Zero:?SIZE_ZERO, _DP:?SIZE_DP, _CC:?SIZE_CC, MD:?SIZE_MD, _/binary>> = CPU_Data,
	<<_MD_Div0:?SIZE_MD_DIV0, _MD_IL:?SIZE_MD_IL, _:4, MD_FM:?SIZE_MD_FM, _:1>> = <<MD>>,
	<<MD_FM:?SIZE_MD_FM>>.

%%%=======================================================================================
%% @doc 
% Gets the MD Register NM bit from CPU_Data<br/>
% Returns: 1 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_md_nm(CPU_Data::cpu_type()) -> md_nm_reg_type().
%%%=======================================================================================
cpu_get_md_nm(CPU_Data) ->

	<<_Q:?SIZE_Q, _X:?SIZE_X, _Y:?SIZE_Y, _U:?SIZE_U, _S:?SIZE_S, _PC:?SIZE_PC, _V:?SIZE_V, _Zero:?SIZE_ZERO, _DP:?SIZE_DP, _CC:?SIZE_CC, MD:?SIZE_MD, _/binary>> = CPU_Data,
	<<_MD_Div0:?SIZE_MD_DIV0, _MD_IL:?SIZE_MD_IL, _:4, _MD_FM:?SIZE_MD_FM, MD_NM:?SIZE_MD_NM>> = <<MD>>,
	<<MD_NM:?SIZE_MD_NM>>.

%%%=======================================================================================
%% @doc 
% Gets the RESET bit from CPU_Data<br/>
% Returns: 1 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_reset(CPU_Data::cpu_type()) -> reset_reg_type().
%%%=======================================================================================
cpu_get_reset(CPU_Data) ->

	<<
		_Q:?SIZE_Q, _X:?SIZE_X, _Y:?SIZE_Y, _U:?SIZE_U, _S:?SIZE_S, _PC:?SIZE_PC, _V:?SIZE_V, _Zero:?SIZE_ZERO, _DP:?SIZE_DP, _CC:?SIZE_CC, _MD:?SIZE_MD,
		Reset:?SIZE_RESET, _/bits
	>> = CPU_Data,
	<<Reset:?SIZE_RESET>>.

%%%=======================================================================================
%% @doc 
% Gets the NMI bit from CPU_Data<br/>
% Returns: 1 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_nmi(CPU_Data::cpu_type()) -> nmi_reg_type().
%%%=======================================================================================
cpu_get_nmi(CPU_Data) ->

	<<
		_Q:?SIZE_Q, _X:?SIZE_X, _Y:?SIZE_Y, _U:?SIZE_U, _S:?SIZE_S, _PC:?SIZE_PC, _V:?SIZE_V, _Zero:?SIZE_ZERO, _DP:?SIZE_DP, _CC:?SIZE_CC, _MD:?SIZE_MD,
		_Reset:?SIZE_RESET, NMI:?SIZE_NMI, _/bits
	>> = CPU_Data,
	<<NMI:?SIZE_NMI>>.

%%%=======================================================================================
%% @doc 
% Gets the SWI bit from CPU_Data<br/>
% Returns: 1 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_swi(CPU_Data::cpu_type()) -> swi_reg_type().
%%%=======================================================================================
cpu_get_swi(CPU_Data) ->

	<<
		_Q:?SIZE_Q, _X:?SIZE_X, _Y:?SIZE_Y, _U:?SIZE_U, _S:?SIZE_S, _PC:?SIZE_PC, _V:?SIZE_V, _Zero:?SIZE_ZERO, _DP:?SIZE_DP, _CC:?SIZE_CC, _MD:?SIZE_MD,
		_Reset:?SIZE_RESET, _NMI:?SIZE_NMI, SWI:?SIZE_SWI, _/bits
	>> = CPU_Data,
	<<SWI:?SIZE_SWI>>.

%%%=======================================================================================
%% @doc 
% Gets the IRQ bit from CPU_Data<br/>
% Returns: 1 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_irq(CPU_Data::cpu_type()) -> irq_reg_type().
%%%=======================================================================================
cpu_get_irq(CPU_Data) ->

	<<
		_Q:?SIZE_Q, _X:?SIZE_X, _Y:?SIZE_Y, _U:?SIZE_U, _S:?SIZE_S, _PC:?SIZE_PC, _V:?SIZE_V, _Zero:?SIZE_ZERO, _DP:?SIZE_DP, _CC:?SIZE_CC, _MD:?SIZE_MD,
		_Reset:?SIZE_RESET, _NMI:?SIZE_NMI, _SWI:?SIZE_SWI, IRQ:?SIZE_IRQ, _/bits
	>> = CPU_Data,
	<<IRQ:?SIZE_IRQ>>.

%%%=======================================================================================
%% @doc 
% Gets the FIRQ bit from CPU_Data<br/>
% Returns: 1 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_firq(CPU_Data::cpu_type()) -> firq_reg_type().
%%%=======================================================================================
cpu_get_firq(CPU_Data) ->

	<<
		_Q:?SIZE_Q, _X:?SIZE_X, _Y:?SIZE_Y, _U:?SIZE_U, _S:?SIZE_S, _PC:?SIZE_PC, _V:?SIZE_V, _Zero:?SIZE_ZERO, _DP:?SIZE_DP, _CC:?SIZE_CC, _MD:?SIZE_MD,
		_Reset:?SIZE_RESET, _NMI:?SIZE_NMI, _SWI:?SIZE_SWI, _IRQ:?SIZE_IRQ, FIRQ:?SIZE_FIRQ, _/bits
	>> = CPU_Data,
	<<FIRQ:?SIZE_FIRQ>>.

%%%=======================================================================================
%% @doc 
% Gets the SWI2 bit from CPU_Data<br/>
% Returns: 1 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_swi2(CPU_Data::cpu_type()) -> swi2_reg_type().
%%%=======================================================================================
cpu_get_swi2(CPU_Data) ->

	<<
		_Q:?SIZE_Q, _X:?SIZE_X, _Y:?SIZE_Y, _U:?SIZE_U, _S:?SIZE_S, _PC:?SIZE_PC, _V:?SIZE_V, _Zero:?SIZE_ZERO, _DP:?SIZE_DP, _CC:?SIZE_CC, _MD:?SIZE_MD,
		_Reset:?SIZE_RESET, _NMI:?SIZE_NMI, _SWI:?SIZE_SWI, _IRQ:?SIZE_IRQ, _FIRQ:?SIZE_FIRQ, SWI2:?SIZE_SWI2, _/bits
	>> = CPU_Data,
	<<SWI2:?SIZE_SWI2>>.

%%%=======================================================================================
%% @doc 
% Gets the SWI3 bit from CPU_Data<br/>
% Returns: 1 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_swi3(CPU_Data::cpu_type()) -> swi3_reg_type().
%%%=======================================================================================
cpu_get_swi3(CPU_Data) ->

	<<
		_Q:?SIZE_Q, _X:?SIZE_X, _Y:?SIZE_Y, _U:?SIZE_U, _S:?SIZE_S, _PC:?SIZE_PC, _V:?SIZE_V, _Zero:?SIZE_ZERO, _DP:?SIZE_DP, _CC:?SIZE_CC, _MD:?SIZE_MD,
		_Reset:?SIZE_RESET, _NMI:?SIZE_NMI, _SWI:?SIZE_SWI, _IRQ:?SIZE_IRQ, _FIRQ:?SIZE_FIRQ, _SWI2:?SIZE_SWI2, SWI3:?SIZE_SWI3, _/bits
	>> = CPU_Data,
	<<SWI3:?SIZE_SWI3>>.

%%%=======================================================================================
%% @doc 
% Gets the RESERVED bit from CPU_Data<br/>
% Returns: 1 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_reserved(CPU_Data::cpu_type()) -> reserved_reg_type().
%%%=======================================================================================
cpu_get_reserved(CPU_Data) ->

	<<
		_Q:?SIZE_Q, _X:?SIZE_X, _Y:?SIZE_Y, _U:?SIZE_U, _S:?SIZE_S, _PC:?SIZE_PC, _V:?SIZE_V, _Zero:?SIZE_ZERO, _DP:?SIZE_DP, _CC:?SIZE_CC, _MD:?SIZE_MD,
		_Reset:?SIZE_RESET, _NMI:?SIZE_NMI, _SWI:?SIZE_SWI, _IRQ:?SIZE_IRQ, _FIRQ:?SIZE_FIRQ, _SWI2:?SIZE_SWI2, _SWI3:?SIZE_SWI3, Reserved:?SIZE_RESERVED, _/bits
	>> = CPU_Data,
	<<Reserved:?SIZE_RESERVED>>.

%%%=======================================================================================
%% @doc 
% Gets the HALT bit from CPU_Data<br/>
% Returns: 1 bit binary<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_get_halt(CPU_Data::cpu_type()) -> halt_reg_type().
%%%=======================================================================================
cpu_get_halt(CPU_Data) ->

	<<
		_Q:?SIZE_Q, _X:?SIZE_X, _Y:?SIZE_Y, _U:?SIZE_U, _S:?SIZE_S, _PC:?SIZE_PC, _V:?SIZE_V, _Zero:?SIZE_ZERO, _DP:?SIZE_DP, _CC:?SIZE_CC, _MD:?SIZE_MD,
		_Reset:?SIZE_RESET, _NMI:?SIZE_NMI, _SWI:?SIZE_SWI, _IRQ:?SIZE_IRQ, _FIRQ:?SIZE_FIRQ, _SWI2:?SIZE_SWI2, _SWI3:?SIZE_SWI3, _Reserved:?SIZE_RESERVED,
		Halt:?SIZE_HALT, _/bits
	>> = CPU_Data,
	<<Halt:?SIZE_HALT>>.

%%%=======================================================================================
%% @doc 
% Sets the Reset bit in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_reset(Reset::reset_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_reset(Reset, CPU_Data) when bit_size(Reset) =:= 1 ->

	<<
		Q:32,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,
		_Reset:1,NMI:1,SWI:1,IRQ:1,FIRQ:1,SWI2:1,SWI3:1,Reserved:1,
		Halt:1,Rest/bits
	>> = CPU_Data,

	<<
		Q:32,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,
		Reset/bits,NMI:1,SWI:1,IRQ:1,FIRQ:1,SWI2:1,SWI3:1,Reserved:1,
		Halt:1,Rest/bits
	>>.

%%%=======================================================================================
%% @doc 
% Sets the NMI bit in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_nmi(NMI::nmi_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_nmi(NMI,CPU_Data) when bit_size(NMI) =:= 1 ->

	<<
		Q:32,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,
		Reset:1,_NMI:1,SWI:1,IRQ:1,FIRQ:1,SWI2:1,SWI3:1,Reserved:1,
		Halt:1,Rest/bits
	>> = CPU_Data,

	<<
		Q:32,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,
		Reset:1,NMI/bits,SWI:1,IRQ:1,FIRQ:1,SWI2:1,SWI3:1,Reserved:1,
		Halt:1,Rest/bits
	>>.

%%%=======================================================================================
%% @doc 
% Sets the SWI bit in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_swi(SWI::swi_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_swi(SWI,CPU_Data) when bit_size(SWI) =:= 1 ->

	<<
		Q:32,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,
		Reset:1,NMI:1,_SWI:1,IRQ:1,FIRQ:1,SWI2:1,SWI3:1,Reserved:1,
		Halt:1,Rest/bits
	>> = CPU_Data,

	<<
		Q:32,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,
		Reset:1,NMI:1,SWI/bits,IRQ:1,FIRQ:1,SWI2:1,SWI3:1,Reserved:1,
		Halt:1,Rest/bits
	>>.

%%%=======================================================================================
%% @doc 
% Sets the IRQ bit in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_irq(IRQ::irq_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_irq(IRQ,CPU_Data) when bit_size(IRQ) =:= 1 ->

	<<
		Q:32,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,
		Reset:1,NMI:1,SWI:1,_IRQ:1,FIRQ:1,SWI2:1,SWI3:1,Reserved:1,
		Halt:1,Rest/bits
	>> = CPU_Data,

	<<
		Q:32,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,
		Reset:1,NMI:1,SWI:1,IRQ/bits,FIRQ:1,SWI2:1,SWI3:1,Reserved:1,
		Halt:1,Rest/bits
	>>.

%%%=======================================================================================
%% @doc 
% Sets the FIRQ bit in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_firq(FIRQ::firq_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_firq(FIRQ,CPU_Data) when bit_size(FIRQ) =:= 1 ->

	<<
		Q:32,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,
		Reset:1,NMI:1,SWI:1,IRQ:1,_FIRQ:1,SWI2:1,SWI3:1,Reserved:1,
		Halt:1,Rest/bits
	>> = CPU_Data,

	<<
		Q:32,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,
		Reset:1,NMI:1,SWI:1,IRQ:1,FIRQ/bits,SWI2:1,SWI3:1,Reserved:1,
		Halt:1,Rest/bits
	>>.

%%%=======================================================================================
%% @doc 
% Sets the SWI2 bit in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_swi2(SWI2::swi2_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_swi2(SWI2,CPU_Data) when bit_size(SWI2) =:= 1 ->

	<<
		Q:32,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,
		Reset:1,NMI:1,SWI:1,IRQ:1,FIRQ:1,_SWI2:1,SWI3:1,Reserved:1,
		Halt:1,Rest/bits
	>> = CPU_Data,

	<<
		Q:32,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,
		Reset:1,NMI:1,SWI:1,IRQ:1,FIRQ:1,SWI2/bits,SWI3:1,Reserved:1,
		Halt:1,Rest/bits
	>>.

%%%=======================================================================================
%% @doc 
% Sets the SWI3 bit in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_swi3(SWI3::swi2_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_swi3(SWI3,CPU_Data) when bit_size(SWI3) =:= 1 ->

	<<
		Q:32,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,
		Reset:1,NMI:1,SWI:1,IRQ:1,FIRQ:1,SWI2:1,_SWI3:1,Reserved:1,
		Halt:1,Rest/bits
	>> = CPU_Data,

	<<
		Q:32,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,
		Reset:1,NMI:1,SWI:1,IRQ:1,FIRQ:1,SWI2:1,SWI3/bits,Reserved:1,
		Halt:1,Rest/bits
	>>.

%%%=======================================================================================
%% @doc 
% Sets the RESERVED bit in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_reserved(Reserved::reserved_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_reserved(Reserved,CPU_Data) when bit_size(Reserved) =:= 1 ->

	<<
		Q:32,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,
		Reset:1,NMI:1,SWI:1,IRQ:1,FIRQ:1,SWI2:1,SWI3:1,_Reserved:1,
		Halt:1,Rest/bits
	>> = CPU_Data,

	<<
		Q:32,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,
		Reset:1,NMI:1,SWI:1,IRQ:1,FIRQ:1,SWI2:1,SWI3:1,Reserved/bits,
		Halt:1,Rest/bits
	>>.

%%%=======================================================================================
%% @doc 
% Sets the HALT bit in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_halt(Halt::halt_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_halt(Halt,CPU_Data) when bit_size(Halt) =:= 1 ->

	<<
		Q:32,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,
		Reset:1,NMI:1,SWI:1,IRQ:1,FIRQ:1,SWI2:1,SWI3:1,Reserved:1,
		_Halt:1,Rest/bits
	>> = CPU_Data,

	<<
		Q:32,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,
		Reset:1,NMI:1,SWI:1,IRQ:1,FIRQ:1,SWI2:1,SWI3:1,Reserved:1,
		Halt/bits,Rest/bits
	>>.

%%%=======================================================================================
%% @doc 
% Sets the Q Register in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_q(Q::q_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_q(Q,CPU_Data) when bit_size(Q) =:= 32 ->

	<<_Q:32,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>> = CPU_Data,

	<<Q/bits,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>>.

%%%=======================================================================================
%% @doc 
% Sets the D Register in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_d(D::d_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_d(D,CPU_Data) when bit_size(D) =:= 16 ->

	<<_D:16,W:16,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>> = CPU_Data,

	<<D/bits,W:16,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>>.

%%%=======================================================================================
%% @doc 
% Sets the W Register in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_w(W::w_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_w(W,CPU_Data) when bit_size(W) =:= 16 ->

	<<D:16,_W:16,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>> = CPU_Data,

	<<D:16,W/bits,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>>.

%%%=======================================================================================
%% @doc 
% Sets the A Register in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_a(A::a_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_a(A,CPU_Data) when bit_size(A) =:= 8 ->

	<<_A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>> = CPU_Data,

	<<A/bits,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>>.

%%%=======================================================================================
%% @doc 
% Sets the B Register in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_b(B::b_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_b(B,CPU_Data) when bit_size(B) =:= 8 ->

	<<A:8,_B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>> = CPU_Data,

	<<A:8,B/bits,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>>.

%%%=======================================================================================
%% @doc 
% Sets the E Register in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_e(E::e_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_e(E,CPU_Data) when bit_size(E) =:= 8 ->

	<<A:8,B:8,_E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>> = CPU_Data,

	<<A:8,B:8,E/bits,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>>.

%%%=======================================================================================
%% @doc 
% Sets the F Register in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_f(F::f_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_f(F,CPU_Data) when bit_size(F) =:= 8 ->

	<<A:8,B:8,E:8,_F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>> = CPU_Data,

	<<A:8,B:8,E:8,F/bits,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>>.

%%%=======================================================================================
%% @doc 
% Sets the X Register in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_x(X::x_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_x(X,CPU_Data) when bit_size(X) =:= 16 ->

	<<A:8,B:8,E:8,F:8,_X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>> = CPU_Data,

	<<A:8,B:8,E:8,F:8,X/bits,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>>.

%%%=======================================================================================
%% @doc 
% Sets the Y Register in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_y(Y::y_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_y(Y,CPU_Data) when bit_size(Y) =:= 16 ->

	<<A:8,B:8,E:8,F:8,X:16,_Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>> = CPU_Data,

	<<A:8,B:8,E:8,F:8,X:16,Y/bits,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>>.

%%%=======================================================================================
%% @doc 
% Sets the U Register in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_u(U::u_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_u(U,CPU_Data) when bit_size(U) =:= 16 ->

	<<A:8,B:8,E:8,F:8,X:16,Y:16,_U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>> = CPU_Data,

	<<A:8,B:8,E:8,F:8,X:16,Y:16,U/bits,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>>.

%%%=======================================================================================
%% @doc 
% Sets the S Register in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_s(S::s_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_s(S,CPU_Data) when bit_size(S) =:= 16 ->

	<<A:8,B:8,E:8,F:8,X:16,Y:16,U:16,_S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>> = CPU_Data,

	<<A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S/bits,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>>.

%%%=======================================================================================
%% @doc 
% Sets the PC Register in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_pc(PC::pc_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_pc(PC,CPU_Data) when bit_size(PC) =:= 16 ->

	<<A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,_PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>> = CPU_Data,

	<<A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC/bits,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>>.

%%%=======================================================================================
%% @doc 
% Sets the V Register in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_v(V::v_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_v(V,CPU_Data) when bit_size(V) =:= 16 ->

	<<A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,_V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>> = CPU_Data,

	<<A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V/bits,Zero:16,DP:8,CC:8,MD:8,Rest/binary>>.

%%%=======================================================================================
%% @doc 
% Sets the ZERO Register in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_zero(Zero::zero_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_zero(Zero,CPU_Data) when bit_size(Zero) =:= 16 ->

	<<A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,_Zero:16,DP:8,CC:8,MD:8,Rest/binary>> = CPU_Data,

	<<A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero/bits,DP:8,CC:8,MD:8,Rest/binary>>.

%%%=======================================================================================
%% @doc 
% Sets the DP Register in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_dp(DP::dp_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_dp(DP,CPU_Data) when bit_size(DP) =:= 8 ->

	<<A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,_DP:8,CC:8,MD:8,Rest/binary>> = CPU_Data,

	<<A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP/bits,CC:8,MD:8,Rest/binary>>.

%%%=======================================================================================
%% @doc 
% Sets the CC Register in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_cc(CC::cc_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_cc(CC,CPU_Data) when bit_size(CC) =:= 8 ->

	<<A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,_CC:8,MD:8,Rest/binary>> = CPU_Data,

	<<A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC/bits,MD:8,Rest/binary>>.

%%%=======================================================================================
%% @doc 
% Sets the CC Register E Bit in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_cc_e(CC_E::cc_e_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_cc_e(CC_E,CPU_Data) when bit_size(CC_E) =:= 1 ->

	<<A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>> = CPU_Data,

	<<_CC_E:1,CC_F:1,CC_H:1,CC_I:1,CC_N:1,CC_Z:1,CC_V:1,CC_C:1>> = <<CC>>,

	<<
		A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,
		CC_E/bits,CC_F:1,CC_H:1,CC_I:1,CC_N:1,CC_Z:1,CC_V:1,CC_C:1,
		MD:8,Rest/binary
	>>.

%%%=======================================================================================
%% @doc 
% Sets the CC Register F Bit in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_cc_f(CC_F::cc_f_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_cc_f(CC_F,CPU_Data) when bit_size(CC_F) =:= 1 ->

	<<A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>> = CPU_Data,

	<<CC_E:1,_CC_F:1,CC_H:1,CC_I:1,CC_N:1,CC_Z:1,CC_V:1,CC_C:1>> = <<CC>>,

	<<
		A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,
		CC_E:1,CC_F/bits,CC_H:1,CC_I:1,CC_N:1,CC_Z:1,CC_V:1,CC_C:1,
		MD:8,Rest/binary
	>>.

%%%=======================================================================================
%% @doc 
% Sets the CC Register H Bit in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_cc_h(CC_H::cc_h_reg_type(), CPU_Data::cpu_type()) -> cc_reg_type().
%%%=======================================================================================
cpu_set_cc_h(CC_H,CPU_Data) when bit_size(CC_H) =:= 1 ->

	<<A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>> = CPU_Data,

	<<CC_E:1,CC_F:1,_CC_H:1,CC_I:1,CC_N:1,CC_Z:1,CC_V:1,CC_C:1>> = <<CC>>,

	<<
		A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,
		CC_E:1,CC_F:1,CC_H/bits,CC_I:1,CC_N:1,CC_Z:1,CC_V:1,CC_C:1,
		MD:8,Rest/binary
	>>.

%%%=======================================================================================
%% @doc 
% Sets the CC Register I Bit in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_cc_i(CC_I::cc_i_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_cc_i(CC_I,CPU_Data) when bit_size(CC_I) =:= 1 ->

	<<A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>> = CPU_Data,

	<<CC_E:1,CC_F:1,CC_H:1,_CC_I:1,CC_N:1,CC_Z:1,CC_V:1,CC_C:1>> = <<CC>>,

	<<
		A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,
		CC_E:1,CC_F:1,CC_H:1,CC_I/bits,CC_N:1,CC_Z:1,CC_V:1,CC_C:1,
		MD:8,Rest/binary
	>>.

%%%=======================================================================================
%% @doc 
% Sets the CC Register N Bit in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_cc_n(CC_N::cc_n_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_cc_n(CC_N,CPU_Data) when bit_size(CC_N) =:= 1 ->

	<<A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>> = CPU_Data,

	<<CC_E:1,CC_F:1,CC_H:1,CC_I:1,_CC_N:1,CC_Z:1,CC_V:1,CC_C:1>> = <<CC>>,

	<<
		A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,
		CC_E:1,CC_F:1,CC_H:1,CC_I:1,CC_N/bits,CC_Z:1,CC_V:1,CC_C:1,
		MD:8,Rest/binary
	>>.

%%%=======================================================================================
%% @doc 
% Sets the CC Register Z Bit in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_cc_z(CC_Z::cc_z_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_cc_z(CC_Z,CPU_Data) when bit_size(CC_Z) =:= 1 ->

	<<A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>> = CPU_Data,

	<<CC_E:1,CC_F:1,CC_H:1,CC_I:1,CC_N:1,_CC_Z:1,CC_V:1,CC_C:1>> = <<CC>>,

	<<
		A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,
		CC_E:1,CC_F:1,CC_H:1,CC_I:1,CC_N:1,CC_Z/bits,CC_V:1,CC_C:1,
		MD:8,Rest/binary
	>>.

%%%=======================================================================================
%% @doc 
% Sets the CC Register V Bit in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_cc_v(CC_V::cc_v_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_cc_v(CC_V,CPU_Data) when bit_size(CC_V) =:= 1 ->

	<<A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>> = CPU_Data,

	<<CC_E:1,CC_F:1,CC_H:1,CC_I:1,CC_N:1,CC_Z:1,_CC_V:1,CC_C:1>> = <<CC>>,

	<<
		A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,
		CC_E:1,CC_F:1,CC_H:1,CC_I:1,CC_N:1,CC_Z:1,CC_V/bits,CC_C:1,
		MD:8,Rest/binary
	>>.

%%%=======================================================================================
%% @doc 
% Sets the CC Register C Bit in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_cc_c(CC_C::cc_c_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_cc_c(CC_C,CPU_Data) when bit_size(CC_C) =:= 1 ->

	<<A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>> = CPU_Data,

	<<CC_E:1,CC_F:1,CC_H:1,CC_I:1,CC_N:1,CC_Z:1,CC_V:1,_CC_C:1>> = <<CC>>,

	<<
		A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,
		CC_E:1,CC_F:1,CC_H:1,CC_I:1,CC_N:1,CC_Z:1,CC_V:1,CC_C/bits,
		MD:8,Rest/binary
	>>.

%%%=======================================================================================
%% @doc 
% Sets the MD Register in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_md(MD::md_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_md(MD,CPU_Data) when bit_size(MD) =:= 8 ->

	<<A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,_MD:8,Rest/binary>> = CPU_Data,

	<<A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD/bits,Rest/binary>>.


%%%=======================================================================================
%% @doc 
% Sets the MD Register DIV0 Bit in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_md_div0(MD_Div0::md_div0_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_md_div0(MD_Div0,CPU_Data) when bit_size(MD_Div0) =:= 1 ->

	<<A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>> = CPU_Data,

	<<_MD_Div0:1,MD_IL:1,_:4,MD_FM:1,MD_NM:1>> = <<MD>>,

	<<
		A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,
		MD_Div0/bits,MD_IL:1,0:4,MD_FM:1,MD_NM:1,
		Rest/binary
	>>.

%%%=======================================================================================
%% @doc 
% Sets the MD Register IL Bit in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_md_il(MD_IL::md_il_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_md_il(MD_IL,CPU_Data) when bit_size(MD_IL) =:= 1 ->

	<<A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>> = CPU_Data,

	<<MD_Div0:1,_MD_IL:1,_:4,MD_FM:1,MD_NM:1>> = <<MD>>,

	<<
		A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,
		MD_Div0:1,MD_IL/bits,0:4,MD_FM:1,MD_NM:1,
		Rest/binary
	>>.

%%%=======================================================================================
%% @doc 
% Sets the MD Register FM Bit in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_md_fm(MD_FM::md_fm_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_md_fm(MD_FM,CPU_Data) when bit_size(MD_FM) =:= 1 ->

	<<A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>> = CPU_Data,

	<<MD_Div0:1,MD_IL:1,_:4,_MD_FM:1,MD_NM:1>> = <<MD>>,

	<<
		A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,
		MD_Div0:1,MD_IL:1,0:4,MD_FM/bits,MD_NM:1,
		Rest/binary
	>>.

%%%=======================================================================================
%% @doc 
% Sets the MD Register NM Bit in CPU_Data<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_set_md_nm(MD_NM::md_nm_reg_type(), CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
cpu_set_md_nm(MD_NM,CPU_Data) when bit_size(MD_NM) =:= 1 ->

	<<A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,MD:8,Rest/binary>> = CPU_Data,

	<<MD_Div0:1,MD_IL:1,_:4,MD_FM:1,_MD_NM:1>> = <<MD>>,

	<<
		A:8,B:8,E:8,F:8,X:16,Y:16,U:16,S:16,PC:16,V:16,Zero:16,DP:8,CC:8,
		MD_Div0:1,MD_IL:1,0:4,MD_FM:1,MD_NM/bits,
		Rest/binary
	>>.

%%%=======================================================================================
%% @doc 
% Performs a list of actions on the CPU<br/>
% Returns: Updated CPU Data<br/>
% ---- Unit-Tested ----<br/>
-spec cpu_perform_actions(Fun_List::list(fun())) -> cpu_type().
%%%=======================================================================================
cpu_perform_actions(Fun_List) ->

	cpu_perform_actions(Fun_List, cpu_clear()).

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cpu_perform_actions([], CPU_Data) ->

	CPU_Data;

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cpu_perform_actions([Fun | Funs], CPU_Data) ->

	New_CPU_Data = Fun(CPU_Data),

	cpu_perform_actions(Funs, New_CPU_Data).

%%%=======================================================================================
%% Position_Section
%%%=======================================================================================
%%%=======================================================================================
%% @doc 
% Increase the POS Address by one<br/>
% ---- Unit-Tested ----<br/>
-spec pos_inc(Pos::position_type()) -> position_type().
%%%=======================================================================================
pos_inc(Pos) ->

	pos_inc_n(Pos, 1).

%%%=======================================================================================
%% @doc 
% Increase the POS Address by two<br/>
% ---- Unit-Tested ----<br/>
-spec pos_inc2(Pos::position_type()) -> position_type().
%%%=======================================================================================
pos_inc2(Pos) ->

	pos_inc_n(Pos, 2).

%%%=======================================================================================
%% @doc 
% Increase the POS Address by N <br/>
% ---- Unit-Tested ----<br/>
-spec pos_inc_n(Pos::position_type(), N::integer()) -> position_type().
%%%=======================================================================================
pos_inc_n(Pos, N) ->

	<<Pos_Num:?SIZE_ADDRESS>> 	= Pos,
	Pos_Inc 					= Pos_Num + N,

	<<Pos_Inc:?SIZE_ADDRESS>>.

%%%=======================================================================================
%% Address_Mode_Section
%%%=======================================================================================
%%%=======================================================================================
%% @doc 
% Get the immediate data (8-bit) at pos from the Data<br/>
% Returns: {immediate_data_8, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec immediate_1_address(Pos::position_type(), Data::binary(), CPU_Data::cpu_type()) -> {<<_:8>>, cpu_type(), position_type()}.
%%%=======================================================================================
immediate_1_address(Pos, Data, CPU_Data) ->

	{get_byte_pos(Data, Pos), CPU_Data, pos_inc(Pos)}.

%%%=======================================================================================
%% @doc 
% Get the immediate data (16-bit) at pos from the Data<br/>
% Returns: {immediate_data_16, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec immediate_2_address(Pos::position_type(), Data::binary(), CPU_Data::cpu_type()) -> {<<_:16>>, cpu_type(), position_type()}.
%%%=======================================================================================
immediate_2_address(Pos, Data, CPU_Data) ->

	Higher 			= get_byte_pos(Data, Pos),
	Lower  			= get_byte_pos(Data, pos_inc(Pos)),

	{<<Higher/bits, Lower/bits>>, CPU_Data, pos_inc2(Pos)}.

%%%=======================================================================================
%% @doc 
% Get the value (8-bit) from a direct request<br/>
% Returns: {value, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec direct_1_address(Pos::position_type(), Data::binary(), CPU_Data::cpu_type()) -> {<<_:8>>, cpu_type(), position_type()}.
%%%=======================================================================================
direct_1_address(Pos, Data, CPU_Data) ->

	DP 				= cpu_get_dp(CPU_Data),
	Higher 			= DP,
	Lower  			= get_byte_pos(Data,Pos),
	Address  		= <<Higher/bits, Lower/bits>>,

	{get_byte_pos(Data, Address), CPU_Data, pos_inc(Pos)}.

%%%=======================================================================================
%% @doc 
% Get the address only from a direct request<br/>
% Returns: {address, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec direct_1_address_address_only(
										Pos 		::position_type(),
										Data 		::binary(),
										CPU_Data 	::cpu_type()
									) -> {address_type(), cpu_type(), position_type()}.
%%%=======================================================================================
direct_1_address_address_only(Pos, Data, CPU_Data) ->

	DP 				= cpu_get_dp(CPU_Data),
	Higher 			= DP,
	Lower  			= get_byte_pos(Data,Pos),
	Address  		= <<Higher/bits,Lower/bits>>,

	{Address, CPU_Data, pos_inc(Pos)}.

% %%%=======================================================================================
% direct_1_address_update_value(Pos, Data, CPU_Data, Update_Fun) ->

% 	{Value, CPU_Data, New_Pos} = direct_1_address(Pos, Data, CPU_Data),

% 	New_Value 	= Update_Fun(Value),

% 	{New_Value, New_CPU_Data, New_Pos, New_Data}.

%%%=======================================================================================
%% @doc 
% Get the value (16-bit) from a direct request<br/>
% Returns: {value, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec direct_2_address(
						Pos 		::position_type(), 
						Data 		::binary(), 
						CPU_Data 	::cpu_type()
					  ) -> {<<_:16>>, cpu_type(), position_type()}.
%%%=======================================================================================
direct_2_address(Pos, Data, CPU_Data) ->

	DP 				= cpu_get_dp(CPU_Data),
	Higher 			= DP,
	Lower  			= get_byte_pos(Data,Pos),

	Address 		= <<Higher/bits, Lower/bits>>,

	Result_Higher 	= get_byte_pos(Data, Address),
	Result_Lower 	= get_byte_pos(Data, pos_inc(Address)),
	Result 			= <<Result_Higher/bits, Result_Lower/bits>>,

	{Result, CPU_Data, pos_inc(Pos)}.

%%%=======================================================================================
%% @doc 
% Get the address only from a direct request<br/>
% Returns: {address, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec direct_2_address_address_only(
										Pos 		::position_type(),
										Data 		::binary(),
										CPU_Data 	::cpu_type()
									) -> {address_type(), cpu_type(), position_type()}.
%%%=======================================================================================
direct_2_address_address_only(Pos, Data, CPU_Data) ->

	DP 				= cpu_get_dp(CPU_Data),
	Higher 			= DP,
	Lower  			= get_byte_pos(Data,Pos),

	Address 		= <<Higher/bits,Lower/bits>>,

	{Address, CPU_Data, pos_inc(Pos)}.

%%----Unit_Tested-------------------------------------------------------------------------
indexed_1_address(Pos, Data, CPU_Data) ->

	indexed_address(Pos, Data, CPU_Data, 1).

%%-----------------------------------------------------------------------------
indexed_1_address_address_only(Pos, Data, CPU_Data) ->

	indexed_address_address_only(Pos, Data, CPU_Data, 1).

%%----Unit_Tested-------------------------------------------------------------------------
indexed_2_address(Pos,Data,CPU_Data) ->

	indexed_address(Pos,Data,CPU_Data,2).

%%-----------------------------------------------------------------------------
indexed_2_address_address_only(Pos,Data,CPU_Data) ->

	indexed_address_address_only(Pos,Data,CPU_Data,2).

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
indexed_address(Pos, Data, CPU_Data, Number_Of_Bytes) ->

	Post_Byte 												= get_byte_pos(Data, Pos),

	<<PB7:1,PB6:1,PB5:1,PB4:1,PB3:1,PB2:1,PB1:1,PB0:1>> 	= Post_Byte,

	case PB7 of

		0 -> 	%%-------------------------------------------------------------
				%% Constanst offset from R, 5 bit offset
				%%-------------------------------------------------------------

				constant_offset_from_r_5_bit_offset_direct(
															Pos,
															Data,
															CPU_Data,
															Number_Of_Bytes
													      );

		1 ->	case [PB3,PB2,PB1,PB0] of

					[0,0,0,0] -> 	case [PB6,PB5,PB4] of

										[0,0,1] -> 	% W No Offset (indirect)
													constant_offset_from_w_no_offset(
																						Pos,
																						Data,
																						CPU_Data,
																						Number_Of_Bytes
																					);
 
										[0,1,1] ->  % W 16 bit offset (indirect)
													constant_offset_from_w_16_bit_offset(
																							Pos,
																							Data,
																							CPU_Data,
																							Number_Of_Bytes
																						);

										[1,0,1] -> 	% W Post-Incrment by 2 (indirect)
													auto_increment_of_w_post_increment_by_2(
																								Pos,
																								Data,
																								CPU_Data,
																								Number_Of_Bytes
																							);
													
										[1,1,1] -> 	% W Pre-Decrement by 2 (indirect)
													auto_decrement_of_w_pre_decrement_by_2(
																								Pos,
																								Data,
																								CPU_Data,
																								Number_Of_Bytes
																							);

										[_,_,0] ->  %% Increment by 1
													auto_increment_of_r_post_increment_by_1(
																								Pos,
																								Data,
																								CPU_Data,
																								Number_Of_Bytes
																							)
										end;

					[0,0,0,1] ->	%% Increment by 2
									auto_increment_of_r_post_increment_by_2(
																				Pos,
																				Data,
																				CPU_Data,
																				Number_Of_Bytes
																			);

									

					[0,0,1,0] ->	%% Decrease by 1
									auto_decrement_of_r_pre_decrement_by_1(
																				Pos,
																				Data,
																				CPU_Data,
																				Number_Of_Bytes
																			);

									

					[0,0,1,1] ->	%% Decrease by 2
									auto_decrement_of_r_pre_decrement_by_2(
																				Pos,
																				Data,
																				CPU_Data,
																				Number_Of_Bytes
																			);

									

					[0,1,0,0] ->	%% Zero-Offset Indexed
									constant_offset_from_r_no_offset(
																		Pos,
																		Data,
																		CPU_Data,
																		Number_Of_Bytes
																	);

					[0,1,0,1] ->	%% B Register Offset
									accumulator_offset_from_r_b_accumulator(
																				Pos,
																				Data,
																				CPU_Data,
																				Number_Of_Bytes
																			);

					[0,1,1,0] ->	%% A Register Offset
									accumulator_offset_from_r_a_accumulator(
																				Pos,
																				Data,
																				CPU_Data,
																				Number_Of_Bytes
																			);

					[0,1,1,1] ->	%% E Register Offset
									accumulator_offset_from_r_e_accumulator(
																				Pos,
																				Data,
																				CPU_Data,
																				Number_Of_Bytes
																			);

					[1,0,0,0] -> 	% 8 Bit Offset
									constant_offset_from_r_8_bit_offset(
																			Pos,
																			Data,
																			CPU_Data,
																			Number_Of_Bytes
																		);

									

					[1,0,0,1] ->	% 16 Bit Offset
									constant_offset_from_r_16_bit_offset(
																			Pos,
																			Data,
																			CPU_Data,
																			Number_Of_Bytes
																		);

									

					[1,0,1,0] ->	%% F Register Offset
									accumulator_offset_from_r_f_accumulator(
																				Pos,
																				Data,
																				CPU_Data,
																				Number_Of_Bytes
																			);

					[1,0,1,1] ->	%% D Register Offset
									accumulator_offset_from_r_d_accumulator(
																				Pos,
																				Data,
																				CPU_Data,
																				Number_Of_Bytes
																			);

					[1,1,0,0] -> 	% PC 8 Bit Offset
									constant_offset_from_pc_8_bit_offset(
																			Pos,
																			Data,
																			CPU_Data,
																			Number_Of_Bytes
																		);

					[1,1,0,1] -> 	% PC 16 Bit Offset
									constant_offset_from_pc_16_bit_offset(
																			Pos,
																			Data,
																			CPU_Data,
																			Number_Of_Bytes
																		);

									

					[1,1,1,0] ->	%% W Register Offset
									accumulator_offset_from_r_w_accumulator(
																				Pos,
																				Data,
																				CPU_Data,
																				Number_Of_Bytes
																			);

					[1,1,1,1] -> 	case [PB6,PB5,PB4] of

										[0,0,0] -> 	% W No Offset (direct)
													constant_offset_from_w_no_offset(
																						Pos,
																						Data,
																						CPU_Data,
																						Number_Of_Bytes
																					);

										[0,1,0] -> 	% W 16 bit Offset (direct)
													constant_offset_from_w_16_bit_offset(
																							Pos,
																							Data,
																							CPU_Data,
																							Number_Of_Bytes
																						);

										[1,0,0] -> 	% W Post-Increment by 2 (direct)
													auto_increment_of_w_post_increment_by_2(
																								Pos,
																								Data,
																								CPU_Data,
																								Number_Of_Bytes
																							);

										[1,1,0] -> 	% W Pre-Decrement by 2 (direct)
													auto_decrement_of_w_pre_decrement_by_2(
																								Pos,
																								Data,
																								CPU_Data,
																								Number_Of_Bytes
																							);

										[0,0,1] -> 	% Extended Address (Indirect only)
													extended_indirect_16_bit_address(
																						Pos,
																						Data,
																						CPU_Data,
																						Number_Of_Bytes
																					)
													
													
										end

					end
	end.

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
indexed_address_address_only(Pos, Data, CPU_Data, Number_Of_Bytes) ->

	Post_Byte 												= get_byte_pos(Data,Pos),

	<<PB7:1,PB6:1,PB5:1,PB4:1,PB3:1,PB2:1,PB1:1,PB0:1>> 	= Post_Byte,

	case PB7 of

		0 -> 	%%-------------------------------------------------------------
				%% Constanst offset from R, 5 bit offset
				%%-------------------------------------------------------------

				constant_offset_from_r_5_bit_offset_direct_address_only(
																			Pos,
																			Data,
																			CPU_Data,
																			Number_Of_Bytes
																	      );

		1 ->	case [PB3,PB2,PB1,PB0] of

					[0,0,0,0] -> 	case [PB6,PB5,PB4] of

										[0,0,1] -> 	% W No Offset (indirect)
													constant_offset_from_w_no_offset_address_only(
																									Pos,
																									Data,
																									CPU_Data,
																									Number_Of_Bytes
																								);
 
										[0,1,1] ->  % W 16 bit offset (indirect)
													constant_offset_from_w_16_bit_offset_address_only(
																										Pos,
																										Data,
																										CPU_Data,
																										Number_Of_Bytes
																									);

										[1,0,1] -> 	% W Post-Incrment by 2 (indirect)
													auto_increment_of_w_post_increment_by_2_address_only(
																											Pos,
																											Data,
																											CPU_Data,
																											Number_Of_Bytes
																										);
													
										[1,1,1] -> 	% W Pre-Decrement by 2 (indirect)
													auto_decrement_of_w_pre_decrement_by_2_address_only(
																											Pos,
																											Data,
																											CPU_Data,
																											Number_Of_Bytes
																										);

										[_,_,0] ->  %% Increment by 1
													auto_increment_of_r_post_increment_by_1_address_only(
																											Pos,
																											Data,
																											CPU_Data,
																											Number_Of_Bytes
																										)
										end;

					[0,0,0,1] ->	%% Increment by 2
									auto_increment_of_r_post_increment_by_2_address_only(
																							Pos,
																							Data,
																							CPU_Data,
																							Number_Of_Bytes
																						);

									

					[0,0,1,0] ->	%% Decrease by 1
									auto_decrement_of_r_pre_decrement_by_1_address_only(
																							Pos,
																							Data,
																							CPU_Data,
																							Number_Of_Bytes
																						);

									

					[0,0,1,1] ->	%% Decrease by 2
									auto_decrement_of_r_pre_decrement_by_2_address_only(
																							Pos,
																							Data,
																							CPU_Data,
																							Number_Of_Bytes
																						);

									

					[0,1,0,0] ->	%% Zero-Offset Indexed
									constant_offset_from_r_no_offset_address_only(
																					Pos,
																					Data,
																					CPU_Data,
																					Number_Of_Bytes
																				);

					[0,1,0,1] ->	%% B Register Offset
									accumulator_offset_from_r_b_accumulator_address_only(
																							Pos,
																							Data,
																							CPU_Data,
																							Number_Of_Bytes
																						);

					[0,1,1,0] ->	%% A Register Offset
									accumulator_offset_from_r_a_accumulator_address_only(
																							Pos,
																							Data,
																							CPU_Data,
																							Number_Of_Bytes
																						);

					[0,1,1,1] ->	%% E Register Offset
									accumulator_offset_from_r_e_accumulator_address_only(
																							Pos,
																							Data,
																							CPU_Data,
																							Number_Of_Bytes
																						);

					[1,0,0,0] -> 	% 8 Bit Offset
									constant_offset_from_r_8_bit_offset_address_only(
																						Pos,
																						Data,
																						CPU_Data,
																						Number_Of_Bytes
																					);

									

					[1,0,0,1] ->	% 16 Bit Offset
									constant_offset_from_r_16_bit_offset_address_only(
																						Pos,
																						Data,
																						CPU_Data,
																						Number_Of_Bytes
																					);

									

					[1,0,1,0] ->	%% F Register Offset
									accumulator_offset_from_r_f_accumulator_address_only(
																							Pos,
																							Data,
																							CPU_Data,
																							Number_Of_Bytes
																						);

					[1,0,1,1] ->	%% D Register Offset
									accumulator_offset_from_r_d_accumulator_address_only(
																							Pos,
																							Data,
																							CPU_Data,
																							Number_Of_Bytes
																						);

					[1,1,0,0] -> 	% PC 8 Bit Offset
									constant_offset_from_pc_8_bit_offset_address_only(
																						Pos,
																						Data,
																						CPU_Data,
																						Number_Of_Bytes
																					);

					[1,1,0,1] -> 	% PC 16 Bit Offset
									constant_offset_from_pc_16_bit_offset_address_only(
																						Pos,
																						Data,
																						CPU_Data,
																						Number_Of_Bytes
																					);

									

					[1,1,1,0] ->	%% W Register Offset
									accumulator_offset_from_r_w_accumulator_address_only(
																							Pos,
																							Data,
																							CPU_Data,
																							Number_Of_Bytes
																						);

					[1,1,1,1] -> 	case [PB6,PB5,PB4] of

										[0,0,0] -> 	% W No Offset (direct)
													constant_offset_from_w_no_offset_address_only(
																									Pos,
																									Data,
																									CPU_Data,
																									Number_Of_Bytes
																								);

										[0,1,0] -> 	% W 16 bit Offset (direct)
													constant_offset_from_w_16_bit_offset_address_only(
																										Pos,
																										Data,
																										CPU_Data,
																										Number_Of_Bytes
																									);

										[1,0,0] -> 	% W Post-Increment by 2 (direct)
													auto_increment_of_w_post_increment_by_2_address_only(
																											Pos,
																											Data,
																											CPU_Data,
																											Number_Of_Bytes
																										);

										[1,1,0] -> 	% W Pre-Decrement by 2 (direct)
													auto_decrement_of_w_pre_decrement_by_2_address_only(
																											Pos,
																											Data,
																											CPU_Data,
																											Number_Of_Bytes
																										);

										[0,0,1] -> 	% Extended Address (Indirect only)
													extended_indirect_16_bit_address_address_only(
																									Pos,
																									Data,
																									CPU_Data,
																									Number_Of_Bytes
																								)
													
													
										end

					end
	end.

%%%=======================================================================================
%% @doc 
% Uses Register R which is encoded in the post-byte <br/>
% and a signed 5-bit offset which is also encoded in the post-byte<br/>
% The effective address is calculated by R + 5-bit offset <br/>
% This address mode supports direct addressing only <br/>
% Returns: {value, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec constant_offset_from_r_5_bit_offset_direct(
													Pos 				::position_type(),
													Data 				::binary(),
													CPU_Data 			::cpu_type(),
													Number_Of_Bytes 	::non_neg_integer()
												) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
constant_offset_from_r_5_bit_offset_direct(
											Pos,
											Data,
											CPU_Data,
											Number_Of_Bytes
									       ) ->

	Post_Byte 												= get_byte_pos(Data,Pos),

	<<_PB7:1,PB6:1,PB5:1,PB4:1,PB3:1,PB2:1,PB1:1,PB0:1>> 	= Post_Byte,

	<<RR:2>> 												= <<PB6:1,PB5:1>>,

	Register_Value 											= index_reg_value(RR,CPU_Data),
	<<Offset:5/signed>> 									= <<PB4:1,PB3:1,PB2:1,PB1:1,PB0:1>>,
	Address 												= Register_Value + Offset,

	case Number_Of_Bytes of

		1	-> 	{get_byte_pos(Data, <<Address:16>>), CPU_Data, pos_inc(Pos)};

		2 	-> 	FH 						= get_byte_pos(Data, <<Address:16>>),
				FL 						= get_byte_pos(Data, <<(Address + 1):16>>),
				Final 					= <<FH/bits, FL/bits>>,
				{Final, CPU_Data, pos_inc(Pos)}
		end.

%%%=======================================================================================
%% @doc 
% Uses Register R which is encoded in the post-byte <br/>
% and a signed 5-bit offset which is also encoded in the post-byte<br/>
% The effective address is calculated by R + 5-bit offset <br/>
% This address mode supports direct addressing only <br/>
% Returns: {address, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec constant_offset_from_r_5_bit_offset_direct_address_only(
																Pos 				::position_type(),
																Data 				::binary(),
																CPU_Data 			::cpu_type(),
																Number_Of_Bytes 	::non_neg_integer()
															) -> { address_type(), cpu_type(), position_type() }.
%%%=======================================================================================
constant_offset_from_r_5_bit_offset_direct_address_only(
															Pos,
															Data,
															CPU_Data,
															_Number_Of_Bytes
													    ) ->

	Post_Byte 												= get_byte_pos(Data,Pos),

	<<_PB7:1,PB6:1,PB5:1,PB4:1,PB3:1,PB2:1,PB1:1,PB0:1>> 	= Post_Byte,

	<<RR:2>> 												= <<PB6:1,PB5:1>>,

	Register_Value 											= index_reg_value(RR,CPU_Data),
	<<Offset:5/signed>> 									= <<PB4:1,PB3:1,PB2:1,PB1:1,PB0:1>>,
	Address 												= Register_Value + Offset,

	{<<Address:?SIZE_ADDRESS>>, CPU_Data, pos_inc(Pos)}.

%%%=======================================================================================
%% @doc 
% Uses Register R which is encoded in the post-byte <br/>
% Direct mode the effective address is the value in R<br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of R, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {value, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec constant_offset_from_r_no_offset(
										Pos 				::position_type(),
										Data 				::binary(),
										CPU_Data 			::cpu_type(),
										Number_Of_Bytes 	::non_neg_integer()
									  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
constant_offset_from_r_no_offset(
									Pos,
									Data,
									CPU_Data,
									Number_Of_Bytes
								) ->

	Post_Byte 													= get_byte_pos(Data, Pos),

	<<_PB7:1,PB6:1,PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	<<RR:2>> 													= <<PB6:1,PB5:1>>,

	Indirect_Field 												= PB4,

	Register_Value 												= index_reg_value(RR, CPU_Data),

	case Number_Of_Bytes of

		1 	-> 	case Indirect_Field of

					0 ->	{get_byte_pos(Data,<<Register_Value:16>>), CPU_Data, pos_inc(Pos)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Register_Value:16>>),
							IAL 							= get_byte_pos(Data,<<(Register_Value+1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

							{get_byte_pos(Data,Intermediate_Address), CPU_Data, pos_inc(Pos)}
					end;

		2 	-> 	case Indirect_Field of

					0 ->	FH 				= get_byte_pos(Data, <<Register_Value:16>>),
							FL 				= get_byte_pos(Data, <<(Register_Value + 1):16>>),
							Final 			= <<FH/bits, FL/bits>>,

							{Final, CPU_Data, pos_inc(Pos)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Register_Value:16>>),
							IAL 							= get_byte_pos(Data,<<(Register_Value+1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

							FH 								= get_byte_pos(Data,Intermediate_Address),
							FL 								= get_byte_pos(Data,pos_inc(Intermediate_Address)),
							Final 							= <<FH/bits,FL/bits>>,

							{Final, CPU_Data, pos_inc(Pos)}
					end
		end.

%%%=======================================================================================
%% @doc 
% Uses Register R which is encoded in the post-byte <br/>
% Direct mode the effective address is the value in R<br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of R, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {address, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec constant_offset_from_r_no_offset_address_only(
														Pos 				::position_type(),
														Data 				::binary(),
														CPU_Data 			::cpu_type(),
														Number_Of_Bytes 	::non_neg_integer()
													) -> { <<_:16>>, cpu_type(), position_type() }.
%%%=======================================================================================
constant_offset_from_r_no_offset_address_only(
												Pos,
												Data,
												CPU_Data,
												_Number_Of_Bytes
											 ) ->

	Post_Byte 													= get_byte_pos(Data,Pos),

	<<_PB7:1,PB6:1,PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	<<RR:2>> 													= <<PB6:1,PB5:1>>,

	Indirect_Field 												= PB4,

	Register_Value 												= index_reg_value(RR,CPU_Data),

	case Indirect_Field of

		0 ->	{<<Register_Value:16>>, CPU_Data, pos_inc(Pos)};

		1 -> 	IAH 							= get_byte_pos(Data,<<Register_Value:16>>),
				IAL 							= get_byte_pos(Data,<<(Register_Value+1):16>>),
				Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

				{Intermediate_Address, CPU_Data, pos_inc(Pos)}
		end.

%%%=======================================================================================
%% @doc 
% Uses Register R which is encoded in the post-byte <br/>
% and an 8-bit value encode after the post-byte <br/>
% Direct mode the effective address is the value in R + 8-bit value<br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of R + 8-bit value, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {value, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec constant_offset_from_r_8_bit_offset(
											Pos 				::position_type(),
											Data 				::binary(),
											CPU_Data 			::cpu_type(),
											Number_Of_Bytes 	::non_neg_integer()
										  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
constant_offset_from_r_8_bit_offset(
										Pos,
										Data,
										CPU_Data,
										Number_Of_Bytes
									) ->


	Post_Byte 													= get_byte_pos(Data,Pos),

	<<_PB7:1,PB6:1,PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	<<RR:2>> 													= <<PB6:1,PB5:1>>,

	Indirect_Field 												= PB4,

	Register_Value 												= index_reg_value(RR,CPU_Data),

	<<Signed_Next_Byte:8/signed>> 								= get_byte_pos(Data,pos_inc(Pos)),
	Sum 														= Register_Value + Signed_Next_Byte,

	case Number_Of_Bytes of

		1 	-> 	case Indirect_Field of

					0 ->	{get_byte_pos(Data,<<Sum:16>>), CPU_Data, pos_inc2(Pos)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
							IAL 							= get_byte_pos(Data,<<(Sum + 1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

							{get_byte_pos(Data,Intermediate_Address), CPU_Data, pos_inc2(Pos)}
					end;

		2 	-> 	case Indirect_Field of

					0 ->	FH 				= get_byte_pos(Data,<<Sum:16>>),
							FL 				= get_byte_pos(Data,<<(Sum+1):16>>),
							Final 			= <<FH/bits,FL/bits>>,

							{Final, CPU_Data, pos_inc2(Pos)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
							IAL 							= get_byte_pos(Data,<<(Sum+1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,
							FH 								= get_byte_pos(Data,Intermediate_Address),
							FL 								= get_byte_pos(Data,pos_inc(Intermediate_Address)),
							Final 							= <<FH/bits,FL/bits>>,

							{Final, CPU_Data, pos_inc2(Pos)}
					end
		end.

%%%=======================================================================================
%% @doc 
% Uses Register R which is encoded in the post-byte <br/>
% and an 8-bit value encode after the post-byte <br/>
% Direct mode the effective address is the value in R + 8-bit value<br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of R + 8-bit value, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {address, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec constant_offset_from_r_8_bit_offset_address_only(
														Pos 				::position_type(),
														Data 				::binary(),
														CPU_Data 			::cpu_type(),
														Number_Of_Bytes 	::non_neg_integer()
													  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
constant_offset_from_r_8_bit_offset_address_only(
													Pos,
													Data,
													CPU_Data,
													_Number_Of_Bytes
												) ->


	Post_Byte 													= get_byte_pos(Data,Pos),

	<<_PB7:1,PB6:1,PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	<<RR:2>> 													= <<PB6:1,PB5:1>>,

	Indirect_Field 												= PB4,

	Register_Value 												= index_reg_value(RR,CPU_Data),

	<<Signed_Next_Byte:8/signed>> 								= get_byte_pos(Data,pos_inc(Pos)),
	Sum 														= Register_Value + Signed_Next_Byte,

	case Indirect_Field of

		0 ->	{<<Sum:16>>, CPU_Data, pos_inc2(Pos)};

		1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
				IAL 							= get_byte_pos(Data,<<(Sum + 1):16>>),
				Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

				{Intermediate_Address, CPU_Data, pos_inc2(Pos)}
		end.

%%%=======================================================================================
%% @doc 
% Uses Register R which is encoded in the post-byte <br/>
% and an 16-bit value encode after the post-byte <br/>
% Direct mode the effective address is the value in R + 16-bit value<br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of R + 16-bit value, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {value, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec constant_offset_from_r_16_bit_offset(
											Pos 				::position_type(),
											Data 				::binary(),
											CPU_Data 			::cpu_type(),
											Number_Of_Bytes 	::non_neg_integer()
										  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
constant_offset_from_r_16_bit_offset(
										Pos,
										Data,
										CPU_Data,
										Number_Of_Bytes
									) ->


	Post_Byte 													= get_byte_pos(Data,Pos),

	<<_PB7:1,PB6:1,PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	<<RR:2>> 													= <<PB6:1,PB5:1>>,

	Indirect_Field 												= PB4,

	Register_Value 												= index_reg_value(RR,CPU_Data),

	Next_Byte_H 												= get_byte_pos(Data,pos_inc(Pos)),
	Next_Byte_L 												= get_byte_pos(Data,pos_inc2(Pos)),
	<<Signed_Next_Word:16/signed>> 								= <<Next_Byte_H/bits,Next_Byte_L/bits>>,
	Sum 														= Register_Value + Signed_Next_Word,

	case Number_Of_Bytes of

		1	->	case Indirect_Field of

					0 ->	{get_byte_pos(Data,<<Sum:16>>), CPU_Data, pos_inc_n(Pos,3)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
							IAL 							= get_byte_pos(Data,<<(Sum + 1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

							{get_byte_pos(Data,Intermediate_Address), CPU_Data, pos_inc_n(Pos,3)}
					end;

		2	-> 	case Indirect_Field of

					0 ->	FH 				= get_byte_pos(Data,<<Sum:16>>),
							FL 				= get_byte_pos(Data,<<(Sum+1):16>>),
							Final 			= <<FH/bits,FL/bits>>,

							{Final, CPU_Data, pos_inc_n(Pos,3)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
							IAL 							= get_byte_pos(Data,<<(Sum+1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,
							FH 								= get_byte_pos(Data,Intermediate_Address),
							FL 								= get_byte_pos(Data,pos_inc(Intermediate_Address)),
							Final 							= <<FH/bits,FL/bits>>,
							{Final, CPU_Data, pos_inc_n(Pos,3)}
					end
		end.

%%%=======================================================================================
%% @doc 
% Uses Register R which is encoded in the post-byte <br/>
% and an 16-bit value encode after the post-byte <br/>
% Direct mode the effective address is the value in R + 16-bit value<br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of R + 16-bit value, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {address, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec constant_offset_from_r_16_bit_offset_address_only(
															Pos 				::position_type(),
															Data 				::binary(),
															CPU_Data 			::cpu_type(),
															Number_Of_Bytes 	::non_neg_integer()
														  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
constant_offset_from_r_16_bit_offset_address_only(
													Pos,
													Data,
													CPU_Data,
													_Number_Of_Bytes
												) ->


	Post_Byte 													= get_byte_pos(Data,Pos),

	<<_PB7:1,PB6:1,PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	<<RR:2>> 													= <<PB6:1,PB5:1>>,

	Indirect_Field 												= PB4,

	Register_Value 												= index_reg_value(RR,CPU_Data),

	Next_Byte_H 												= get_byte_pos(Data,pos_inc(Pos)),
	Next_Byte_L 												= get_byte_pos(Data,pos_inc2(Pos)),
	<<Signed_Next_Word:16/signed>> 								= <<Next_Byte_H/bits,Next_Byte_L/bits>>,
	Sum 														= Register_Value + Signed_Next_Word,

	case Indirect_Field of

		0 ->	{<<Sum:16>>, CPU_Data, pos_inc_n(Pos,3)};

		1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
				IAL 							= get_byte_pos(Data,<<(Sum + 1):16>>),
				Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

				{Intermediate_Address, CPU_Data, pos_inc_n(Pos,3)}
		end.

%%%=======================================================================================
%% @doc 
% Uses Register W <br/>
% Direct mode the effective address is the value in W<br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of W, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {value, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec constant_offset_from_w_no_offset(
										Pos 				::position_type(),
										Data 				::binary(),
										CPU_Data 			::cpu_type(),
										Number_Of_Bytes 	::non_neg_integer()
									  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
constant_offset_from_w_no_offset(
									Pos,
									Data,
									CPU_Data,
									Number_Of_Bytes
								) ->

	Post_Byte 													= get_byte_pos(Data,Pos),

	<<_PB7:1,_PB6:1,_PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	Indirect_Field 												= PB4,

	<<Register_Value:16>>										= cpu_get_w(CPU_Data),

	case Number_Of_Bytes of

		1 	-> 	case Indirect_Field of

					0 ->	{get_byte_pos(Data,<<Register_Value:16>>), CPU_Data, pos_inc(Pos)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Register_Value:16>>),
							IAL 							= get_byte_pos(Data,<<(Register_Value+1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

							{get_byte_pos(Data,Intermediate_Address), CPU_Data, pos_inc(Pos)}
					end;

		2 	-> 	case Indirect_Field of

					0 ->	FH 				= get_byte_pos(Data,<<Register_Value:16>>),
							FL 				= get_byte_pos(Data,<<(Register_Value+1):16>>),
							Final 			= <<FH/bits,FL/bits>>,

							{Final, CPU_Data, pos_inc(Pos)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Register_Value:16>>),
							IAL 							= get_byte_pos(Data,<<(Register_Value+1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

							FH 								= get_byte_pos(Data,Intermediate_Address),
							FL 								= get_byte_pos(Data,pos_inc(Intermediate_Address)),
							Final 							= <<FH/bits,FL/bits>>,

							{Final, CPU_Data, pos_inc(Pos)}
					end
		end.

%%%=======================================================================================
%% @doc 
% Uses Register W <br/>
% Direct mode the effective address is the value in W<br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of W, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {address, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec constant_offset_from_w_no_offset_address_only(
														Pos 				::position_type(),
														Data 				::binary(),
														CPU_Data 			::cpu_type(),
														Number_Of_Bytes 	::non_neg_integer()
													  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
constant_offset_from_w_no_offset_address_only(
												Pos,
												Data,
												CPU_Data,
												_Number_Of_Bytes
											) ->

	Post_Byte 													= get_byte_pos(Data,Pos),

	<<_PB7:1,_PB6:1,_PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	Indirect_Field 												= PB4,

	<<Register_Value:16>>										= cpu_get_w(CPU_Data),

	case Indirect_Field of

		0 ->	{<<Register_Value:16>>, CPU_Data, pos_inc(Pos)};

		1 -> 	IAH 							= get_byte_pos(Data,<<Register_Value:16>>),
				IAL 							= get_byte_pos(Data,<<(Register_Value+1):16>>),
				Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

				{Intermediate_Address, CPU_Data, pos_inc(Pos)}
		end.

%%%=======================================================================================
%% @doc 
% Uses Register W<br/>
% and an 16-bit value encode after the post-byte <br/>
% Direct mode the effective address is the value in W + 16-bit value<br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of W + 16-bit value, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {value, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec constant_offset_from_w_16_bit_offset(
											Pos 				::position_type(),
											Data 				::binary(),
											CPU_Data 			::cpu_type(),
											Number_Of_Bytes 	::non_neg_integer()
										  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
constant_offset_from_w_16_bit_offset(
										Pos,
										Data,
										CPU_Data,
										Number_Of_Bytes
									) ->


	Post_Byte 													= get_byte_pos(Data,Pos),

	<<_PB7:1,_PB6:1,_PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	Indirect_Field 												= PB4,

	<<Register_Value:16>> 										= cpu_get_w(CPU_Data),

	Next_Byte_H 												= get_byte_pos(Data,pos_inc(Pos)),
	Next_Byte_L 												= get_byte_pos(Data,pos_inc2(Pos)),
	<<Signed_Next_Word:16/signed>> 								= <<Next_Byte_H/bits,Next_Byte_L/bits>>,
	Sum 														= Register_Value + Signed_Next_Word,

	case Number_Of_Bytes of

		1	->	case Indirect_Field of

					0 ->	{get_byte_pos(Data,<<Sum:16>>), CPU_Data, pos_inc_n(Pos,3)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
							IAL 							= get_byte_pos(Data,<<(Sum + 1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

							{get_byte_pos(Data,Intermediate_Address), CPU_Data, pos_inc_n(Pos,3)}
					end;

		2	-> 	case Indirect_Field of

					0 ->	FH 				= get_byte_pos(Data,<<Sum:16>>),
							FL 				= get_byte_pos(Data,<<(Sum+1):16>>),
							Final 			= <<FH/bits,FL/bits>>,

							{Final, CPU_Data, pos_inc_n(Pos,3)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
							IAL 							= get_byte_pos(Data,<<(Sum+1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,
							FH 								= get_byte_pos(Data,Intermediate_Address),
							FL 								= get_byte_pos(Data,pos_inc(Intermediate_Address)),
							Final 							= <<FH/bits,FL/bits>>,
							{Final, CPU_Data, pos_inc_n(Pos,3)}
					end
		end.

%%%=======================================================================================
%% @doc 
% Uses Register W<br/>
% and an 16-bit value encode after the post-byte <br/>
% Direct mode the effective address is the value in W + 16-bit value<br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of W + 16-bit value, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {address, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec constant_offset_from_w_16_bit_offset_address_only(
															Pos 				::position_type(),
															Data 				::binary(),
															CPU_Data 			::cpu_type(),
															Number_Of_Bytes 	::non_neg_integer()
														  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
constant_offset_from_w_16_bit_offset_address_only(
													Pos,
													Data,
													CPU_Data,
													_Number_Of_Bytes
												) ->


	Post_Byte 													= get_byte_pos(Data,Pos),

	<<_PB7:1,_PB6:1,_PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	Indirect_Field 												= PB4,

	<<Register_Value:16>> 										= cpu_get_w(CPU_Data),

	Next_Byte_H 												= get_byte_pos(Data,pos_inc(Pos)),
	Next_Byte_L 												= get_byte_pos(Data,pos_inc2(Pos)),
	<<Signed_Next_Word:16/signed>> 								= <<Next_Byte_H/bits,Next_Byte_L/bits>>,
	Sum 														= Register_Value + Signed_Next_Word,

	case Indirect_Field of

		0 ->	{<<Sum:16>>, CPU_Data, pos_inc_n(Pos,3)};

		1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
				IAL 							= get_byte_pos(Data,<<(Sum + 1):16>>),
				Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

				{Intermediate_Address, CPU_Data, pos_inc_n(Pos,3)}
		end.

%%%=======================================================================================
%% @doc 
% Uses Register R which is encoded in the post-byte <br/>
% and an 8-bit signed value from register A <br/>
% Direct mode the effective address is the value in R + 8-bit value<br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of R + 8-bit value, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {value, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec accumulator_offset_from_r_a_accumulator(
												Pos 				::position_type(),
												Data 				::binary(),
												CPU_Data 			::cpu_type(),
												Number_Of_Bytes 	::non_neg_integer()
											  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
accumulator_offset_from_r_a_accumulator(
											Pos,
											Data,
											CPU_Data,
											Number_Of_Bytes
										) ->


	Post_Byte 													= get_byte_pos(Data,Pos),

	<<_PB7:1,PB6:1,PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	<<RR:2>> 													= <<PB6:1,PB5:1>>,

	Indirect_Field 												= PB4,

	Register_Value 												= index_reg_value(RR,CPU_Data),

	<<Signed_Value_A:8/signed>> 								= cpu_get_a(CPU_Data),
	Sum 														= Register_Value + Signed_Value_A,

	case Number_Of_Bytes of


		1 	-> 	case Indirect_Field of

					0 ->	{get_byte_pos(Data,<<Sum:16>>), CPU_Data, pos_inc(Pos)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
							IAL 							= get_byte_pos(Data,<<(Sum+1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

							{get_byte_pos(Data,Intermediate_Address), CPU_Data, pos_inc(Pos)}
				end;

		2 	->	case Indirect_Field of

					0 ->	FH 				= get_byte_pos(Data,<<Sum:16>>),
							FL 				= get_byte_pos(Data,<<(Sum+1):16>>),
							Final 			= <<FH/bits,FL/bits>>,

							{Final, CPU_Data, pos_inc(Pos)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
							IAL 							= get_byte_pos(Data,<<(Sum+1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,
							FH 								= get_byte_pos(Data,Intermediate_Address),
							FL 								= get_byte_pos(Data,pos_inc(Intermediate_Address)),
							Final 							= <<FH/bits,FL/bits>>,

							{Final, CPU_Data, pos_inc(Pos)}
				end
		end.

%%%=======================================================================================
%% @doc 
% Uses Register R which is encoded in the post-byte <br/>
% and an 8-bit signed value from register A <br/>
% Direct mode the effective address is the value in R + 8-bit value<br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of R + 8-bit value, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {address, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec accumulator_offset_from_r_a_accumulator_address_only(
															Pos 				::position_type(),
															Data 				::binary(),
															CPU_Data 			::cpu_type(),
															Number_Of_Bytes 	::non_neg_integer()
														  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
accumulator_offset_from_r_a_accumulator_address_only(
														Pos,
														Data,
														CPU_Data,
														_Number_Of_Bytes
													) ->


	Post_Byte 													= get_byte_pos(Data,Pos),

	<<_PB7:1,PB6:1,PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	<<RR:2>> 													= <<PB6:1,PB5:1>>,

	Indirect_Field 												= PB4,

	Register_Value 												= index_reg_value(RR,CPU_Data),

	<<Signed_Value_A:8/signed>> 								= cpu_get_a(CPU_Data),
	Sum 														= Register_Value + Signed_Value_A,

	case Indirect_Field of

		0 ->	{<<Sum:16>>, CPU_Data, pos_inc(Pos)};

		1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
				IAL 							= get_byte_pos(Data,<<(Sum+1):16>>),
				Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

				{Intermediate_Address, CPU_Data, pos_inc(Pos)}
	end.

%%%=======================================================================================
%% @doc 
% Uses Register R which is encoded in the post-byte <br/>
% and an 8-bit signed value from register B <br/>
% Direct mode the effective address is the value in R + 8-bit value<br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of R + 8-bit value, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {value, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec accumulator_offset_from_r_b_accumulator(
												Pos 				::position_type(),
												Data 				::binary(),
												CPU_Data 			::cpu_type(),
												Number_Of_Bytes 	::non_neg_integer()
											  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
accumulator_offset_from_r_b_accumulator(
											Pos,
											Data,
											CPU_Data,
											Number_Of_Bytes
										) ->


	Post_Byte 													= get_byte_pos(Data,Pos),

	<<_PB7:1,PB6:1,PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	<<RR:2>> 													= <<PB6:1,PB5:1>>,

	Indirect_Field 												= PB4,

	Register_Value 												= index_reg_value(RR,CPU_Data),

	<<Signed_Value_B:8/signed>> 								= cpu_get_b(CPU_Data),
	Sum 														= Register_Value + Signed_Value_B,

	case Number_Of_Bytes of

		1 	->	case Indirect_Field of

					0 ->	{get_byte_pos(Data,<<Sum:16>>), CPU_Data, pos_inc(Pos)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
							IAL 							= get_byte_pos(Data,<<(Sum+1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

							{get_byte_pos(Data,Intermediate_Address), CPU_Data, pos_inc(Pos)}
					end;

		2 	->	case Indirect_Field of

					0 ->	FH 				= get_byte_pos(Data,<<Sum:16>>),
							FL 				= get_byte_pos(Data,<<(Sum+1):16>>),
							Final 			= <<FH/bits,FL/bits>>,

							{Final, CPU_Data, pos_inc(Pos)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
							IAL 							= get_byte_pos(Data,<<(Sum+1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,
							FH 								= get_byte_pos(Data,Intermediate_Address),
							FL 								= get_byte_pos(Data,pos_inc(Intermediate_Address)),
							Final							= <<FH/bits,FL/bits>>,

							{Final, CPU_Data, pos_inc(Pos)}
				end
		end.

%%%=======================================================================================
%% @doc 
% Uses Register R which is encoded in the post-byte <br/>
% and an 8-bit signed value from register B <br/>
% Direct mode the effective address is the value in R + 8-bit value<br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of R + 8-bit value, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {address, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec accumulator_offset_from_r_b_accumulator_address_only(
															Pos 				::position_type(),
															Data 				::binary(),
															CPU_Data 			::cpu_type(),
															Number_Of_Bytes 	::non_neg_integer()
														  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
accumulator_offset_from_r_b_accumulator_address_only(
														Pos,
														Data,
														CPU_Data,
														_Number_Of_Bytes
													) ->

	Post_Byte 													= get_byte_pos(Data,Pos),

	<<_PB7:1,PB6:1,PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	<<RR:2>> 													= <<PB6:1,PB5:1>>,

	Indirect_Field 												= PB4,

	Register_Value 												= index_reg_value(RR,CPU_Data),

	<<Signed_Value_B:8/signed>> 								= cpu_get_b(CPU_Data),
	Sum 														= Register_Value + Signed_Value_B,

	case Indirect_Field of

		0 ->	{<<Sum:16>>, CPU_Data, pos_inc(Pos)};

		1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
				IAL 							= get_byte_pos(Data,<<(Sum+1):16>>),
				Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

				{Intermediate_Address, CPU_Data, pos_inc(Pos)}
		end.

%%%=======================================================================================
%% @doc 
% Uses Register R which is encoded in the post-byte <br/>
% Direct mode the effective address is the value in R <br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of R, and then getting<br/>
% an another address from that memory location <br/>
% After this the value of R is increased by 1 automatically <br/>
% Returns: {value, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec auto_increment_of_r_post_increment_by_1(
												Pos 				::position_type(),
												Data 				::binary(),
												CPU_Data 			::cpu_type(),
												Number_Of_Bytes 	::non_neg_integer()
											  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
auto_increment_of_r_post_increment_by_1(
											Pos,
											Data,
											CPU_Data,
											Number_Of_Bytes
										) ->


	Post_Byte 													= get_byte_pos(Data,Pos),

	<<_PB7:1,PB6:1,PB5:1,_PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	<<RR:2>> 													= <<PB6:1,PB5:1>>,

	Register_Value 												= index_reg_value(RR,CPU_Data),

	New_CPU_Data_ 												= index_reg_set_value(RR,CPU_Data,Register_Value + 1),

	case Number_Of_Bytes of


		1 	->	{get_byte_pos(Data,<<Register_Value:16>>), New_CPU_Data_, pos_inc(Pos)};

		2	->	FH 		= get_byte_pos(Data,<<Register_Value:16>>),
				FL 		= get_byte_pos(Data,<<(Register_Value+1):16>>),

				Final 	= <<FH/bits,FL/bits>>,

				{Final, New_CPU_Data_, pos_inc(Pos)}

		end.

%%%=======================================================================================
%% @doc 
% Uses Register R which is encoded in the post-byte <br/>
% Direct mode the effective address is the value in R <br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of R, and then getting<br/>
% an another address from that memory location <br/>
% After this the value of R is increased by 1 automatically <br/>
% Returns: {address, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec auto_increment_of_r_post_increment_by_1_address_only(
															Pos 				::position_type(),
															Data 				::binary(),
															CPU_Data 			::cpu_type(),
															Number_Of_Bytes 	::non_neg_integer()
														  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
auto_increment_of_r_post_increment_by_1_address_only(
														Pos,
														Data,
														CPU_Data,
														_Number_Of_Bytes
													) ->

	Post_Byte 													= get_byte_pos(Data,Pos),

	<<_PB7:1,PB6:1,PB5:1,_PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	<<RR:2>> 													= <<PB6:1,PB5:1>>,

	Register_Value 												= index_reg_value(RR,CPU_Data),

	New_CPU_Data_ 												= index_reg_set_value(RR,CPU_Data,Register_Value + 1),

	{<<Register_Value:16>>, New_CPU_Data_, pos_inc(Pos)}.
	
%%%=======================================================================================
%% @doc 
% Uses Register R which is encoded in the post-byte <br/>
% Direct mode the effective address is the value in R <br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of R, and then getting<br/>
% an another address from that memory location <br/>
% After this the value of R is increased by 2 automatically <br/>
% Returns: {value, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec auto_increment_of_r_post_increment_by_2(
												Pos 				::position_type(),
												Data 				::binary(),
												CPU_Data 			::cpu_type(),
												Number_Of_Bytes 	::non_neg_integer()
											  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
auto_increment_of_r_post_increment_by_2(
											Pos,
											Data,
											CPU_Data,
											Number_Of_Bytes
										) ->


	Post_Byte 													= get_byte_pos(Data,Pos),

	<<_PB7:1,PB6:1,PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	<<RR:2>> 													= <<PB6:1,PB5:1>>,

	Indirect_Field 												= PB4,

	Register_Value 												= index_reg_value(RR,CPU_Data),
	New_CPU_Data_ 												= index_reg_set_value(RR,CPU_Data,Register_Value + 2),

	case Number_Of_Bytes of

		1 	-> case Indirect_Field of

					0 ->	{get_byte_pos(Data,<<Register_Value:16>>), New_CPU_Data_, pos_inc(Pos)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Register_Value:16>>),
							IAL 							= get_byte_pos(Data,<<(Register_Value+1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

							{get_byte_pos(Data,Intermediate_Address), New_CPU_Data_, pos_inc(Pos)}
				end;

		2 	-> case Indirect_Field of

					0 ->	FH 				= get_byte_pos(Data,<<Register_Value:16>>),
							FL 				= get_byte_pos(Data,<<(Register_Value+1):16>>),
							Final 			= <<FH/bits,FL/bits>>,

							{Final, New_CPU_Data_, pos_inc(Pos)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Register_Value:16>>),
							IAL 							= get_byte_pos(Data,<<(Register_Value+1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

							FH 								= get_byte_pos(Data,Intermediate_Address),
							FL 								= get_byte_pos(Data,pos_inc(Intermediate_Address)),
							Final 							= <<FH/bits,FL/bits>>,

							{Final, New_CPU_Data_, pos_inc(Pos)}
				end
		end.

%%%=======================================================================================
%% @doc 
% Uses Register R which is encoded in the post-byte <br/>
% Direct mode the effective address is the value in R <br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of R, and then getting<br/>
% an another address from that memory location <br/>
% After this the value of R is increased by 2 automatically <br/>
% Returns: {address, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec auto_increment_of_r_post_increment_by_2_address_only(
															Pos 				::position_type(),
															Data 				::binary(),
															CPU_Data 			::cpu_type(),
															Number_Of_Bytes 	::non_neg_integer()
														  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
auto_increment_of_r_post_increment_by_2_address_only(
														Pos,
														Data,
														CPU_Data,
														_Number_Of_Bytes
													) ->


	Post_Byte 													= get_byte_pos(Data,Pos),

	<<_PB7:1,PB6:1,PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	<<RR:2>> 													= <<PB6:1,PB5:1>>,

	Indirect_Field 												= PB4,

	Register_Value 												= index_reg_value(RR,CPU_Data),
	New_CPU_Data_ 												= index_reg_set_value(RR,CPU_Data,Register_Value + 2),

	case Indirect_Field of

		0 ->	{<<Register_Value:16>>, New_CPU_Data_, pos_inc(Pos)};

		1 -> 	IAH 							= get_byte_pos(Data,<<Register_Value:16>>),
				IAL 							= get_byte_pos(Data,<<(Register_Value+1):16>>),
				Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

				{Intermediate_Address, New_CPU_Data_, pos_inc(Pos)}
	end.

%%%=======================================================================================
%% @doc 
% Uses Register R which is encoded in the post-byte <br/>
% First the value of R is decreased by 1 automatically <br/>
% Direct mode the effective address is the value in R <br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of R, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {value, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec auto_decrement_of_r_pre_decrement_by_1(
												Pos 				::position_type(),
												Data 				::binary(),
												CPU_Data 			::cpu_type(),
												Number_Of_Bytes 	::non_neg_integer()
											  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
auto_decrement_of_r_pre_decrement_by_1(
											Pos,
											Data,
											CPU_Data,
											Number_Of_Bytes
										) ->


	Post_Byte 													= get_byte_pos(Data,Pos),

	<<_PB7:1,PB6:1,PB5:1,_PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	<<RR:2>> 													= <<PB6:1,PB5:1>>,

	Register_Value 												= index_reg_value(RR,CPU_Data) - 1,
	New_CPU_Data_ 												= index_reg_set_value(RR,CPU_Data,Register_Value),

	case Number_Of_Bytes of


		1	-> 	{get_byte_pos(Data,<<Register_Value:16>>), New_CPU_Data_, pos_inc(Pos)};
		2 	-> 	FH 		= get_byte_pos(Data,<<Register_Value:16>>),
				FL 		= get_byte_pos(Data,<<(Register_Value+1):16>>),

				Final 	= <<FH/bits,FL/bits>>,

				{Final, New_CPU_Data_, pos_inc(Pos)}
		end.

%%%=======================================================================================
%% @doc 
% Uses Register R which is encoded in the post-byte <br/>
% First the value of R is decreased by 1 automatically <br/>
% Direct mode the effective address is the value in R <br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of R, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {address, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec auto_decrement_of_r_pre_decrement_by_1_address_only(
															Pos 				::position_type(),
															Data 				::binary(),
															CPU_Data 			::cpu_type(),
															Number_Of_Bytes 	::non_neg_integer()
														  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
auto_decrement_of_r_pre_decrement_by_1_address_only(
														Pos,
														Data,
														CPU_Data,
														_Number_Of_Bytes
													) ->


	Post_Byte 													= get_byte_pos(Data,Pos),

	<<_PB7:1,PB6:1,PB5:1,_PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	<<RR:2>> 													= <<PB6:1,PB5:1>>,

	Register_Value 												= index_reg_value(RR,CPU_Data) - 1,
	New_CPU_Data_ 												= index_reg_set_value(RR,CPU_Data,Register_Value),

	{<<Register_Value:16>>, New_CPU_Data_, pos_inc(Pos)}.

%%%=======================================================================================
%% @doc 
% Uses Register R which is encoded in the post-byte <br/>
% First the value of R is decreased by 2 automatically <br/>
% Direct mode the effective address is the value in R <br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of R, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {value, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec auto_decrement_of_r_pre_decrement_by_2(
												Pos 				::position_type(),
												Data 				::binary(),
												CPU_Data 			::cpu_type(),
												Number_Of_Bytes 	::non_neg_integer()
											  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
auto_decrement_of_r_pre_decrement_by_2(
											Pos,
											Data,
											CPU_Data,
											Number_Of_Bytes
										) ->


	Post_Byte 													= get_byte_pos(Data,Pos),

	<<_PB7:1,PB6:1,PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	<<RR:2>> 													= <<PB6:1,PB5:1>>,

	Indirect_Field 												= PB4,

	Register_Value 												= index_reg_value(RR,CPU_Data) - 2,
	New_CPU_Data_ 												= index_reg_set_value(RR,CPU_Data,Register_Value),

	case Number_Of_Bytes of


		1	-> 	case Indirect_Field of

					0 ->	{get_byte_pos(Data,<<Register_Value:16>>), New_CPU_Data_, pos_inc(Pos)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Register_Value:16>>),
							IAL 							= get_byte_pos(Data,<<(Register_Value+1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

							{get_byte_pos(Data,Intermediate_Address), New_CPU_Data_, pos_inc(Pos)}
				end;

		2 	-> 	case Indirect_Field of

					0 ->	FH 				= get_byte_pos(Data,<<Register_Value:16>>),
							FL 				= get_byte_pos(Data,<<(Register_Value+1):16>>),
							Final  			= <<FH/bits,FL/bits>>,

							{Final, New_CPU_Data_, pos_inc(Pos)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Register_Value:16>>),
							IAL 							= get_byte_pos(Data,<<(Register_Value+1):16>>),
							Intermediate_Address 		 	= <<IAH/bits,IAL/bits>>,

							FH 								= get_byte_pos(Data,Intermediate_Address),
							FL 								= get_byte_pos(Data,pos_inc(Intermediate_Address)),
							Final							= <<FH/bits,FL/bits>>,
							{Final, New_CPU_Data_, pos_inc(Pos)}
				end
		end.

%%%=======================================================================================
%% @doc 
% Uses Register R which is encoded in the post-byte <br/>
% First the value of R is decreased by 2 automatically <br/>
% Direct mode the effective address is the value in R <br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of R, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {address, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec auto_decrement_of_r_pre_decrement_by_2_address_only(
															Pos 				::position_type(),
															Data 				::binary(),
															CPU_Data 			::cpu_type(),
															Number_Of_Bytes 	::non_neg_integer()
														  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
auto_decrement_of_r_pre_decrement_by_2_address_only(
														Pos,
														Data,
														CPU_Data,
														_Number_Of_Bytes
													) ->

	Post_Byte 													= get_byte_pos(Data,Pos),

	<<_PB7:1,PB6:1,PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	<<RR:2>> 													= <<PB6:1,PB5:1>>,

	Indirect_Field 												= PB4,

	Register_Value 												= index_reg_value(RR,CPU_Data) - 2,
	New_CPU_Data_ 												= index_reg_set_value(RR,CPU_Data,Register_Value),

	case Indirect_Field of

		0 ->	{<<Register_Value:16>>, New_CPU_Data_, pos_inc(Pos)};

		1 -> 	IAH 							= get_byte_pos(Data,<<Register_Value:16>>),
				IAL 							= get_byte_pos(Data,<<(Register_Value+1):16>>),
				Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

				{Intermediate_Address, New_CPU_Data_, pos_inc(Pos)}
	end.

%%%=======================================================================================
%% @doc 
% Uses Register W  <br/>
% Direct mode the effective address is the value in W <br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of W, and then getting<br/>
% an another address from that memory location <br/>
% After this the value of W is increased by 2 automatically <br/>
% Returns: {value, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec auto_increment_of_w_post_increment_by_2(
												Pos 				::position_type(),
												Data 				::binary(),
												CPU_Data 			::cpu_type(),
												Number_Of_Bytes 	::non_neg_integer()
											  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
auto_increment_of_w_post_increment_by_2(
											Pos,
											Data,
											CPU_Data,
											Number_Of_Bytes
										) ->


	Post_Byte 												= get_byte_pos(Data,Pos),

	<<_PB7:1,_PB6:1,_PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	Indirect_Field 											= PB4,

	<<Register_Value:16>> 									= cpu_get_w(CPU_Data),
	New_CPU_Data_ 											= cpu_set_w(<<(Register_Value + 2):16>>,CPU_Data),

	case Number_Of_Bytes of

		1 	-> case Indirect_Field of

					0 ->	{get_byte_pos(Data,<<Register_Value:16>>), New_CPU_Data_, pos_inc(Pos)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Register_Value:16>>),
							IAL 							= get_byte_pos(Data,<<(Register_Value+1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

							{get_byte_pos(Data,Intermediate_Address), New_CPU_Data_, pos_inc(Pos)}
				end;

		2 	-> case Indirect_Field of

					0 ->	FH 				= get_byte_pos(Data,<<Register_Value:16>>),
							FL 				= get_byte_pos(Data,<<(Register_Value+1):16>>),
							Final 			= <<FH/bits,FL/bits>>,

							{Final, New_CPU_Data_, pos_inc(Pos)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Register_Value:16>>),
							IAL 							= get_byte_pos(Data,<<(Register_Value+1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

							FH 								= get_byte_pos(Data,Intermediate_Address),
							FL 								= get_byte_pos(Data,pos_inc(Intermediate_Address)),
							Final 							= <<FH/bits,FL/bits>>,

							{Final, New_CPU_Data_, pos_inc(Pos)}
				end
		end.

%%%=======================================================================================
%% @doc 
% Uses Register W  <br/>
% Direct mode the effective address is the value in W <br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of W, and then getting<br/>
% an another address from that memory location <br/>
% After this the value of W is increased by 2 automatically <br/>
% Returns: {address, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec auto_increment_of_w_post_increment_by_2_address_only(
															Pos 				::position_type(),
															Data 				::binary(),
															CPU_Data 			::cpu_type(),
															Number_Of_Bytes 	::non_neg_integer()
														  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
auto_increment_of_w_post_increment_by_2_address_only(
														Pos,
														Data,
														CPU_Data,
														_Number_Of_Bytes
													) ->


	Post_Byte 												= get_byte_pos(Data,Pos),

	<<_PB7:1,_PB6:1,_PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	Indirect_Field 											= PB4,

	<<Register_Value:16>> 									= cpu_get_w(CPU_Data),
	New_CPU_Data_ 											= cpu_set_w(<<(Register_Value + 2):16>>,CPU_Data),

	case Indirect_Field of

		0 ->	{<<Register_Value:16>>, New_CPU_Data_, pos_inc(Pos)};

		1 -> 	IAH 							= get_byte_pos(Data,<<Register_Value:16>>),
				IAL 							= get_byte_pos(Data,<<(Register_Value+1):16>>),
				Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

				{Intermediate_Address, New_CPU_Data_, pos_inc(Pos)}
	end.

%%%=======================================================================================
%% @doc 
% Uses Register W <br/>
% First the value of W is decreased by 2 automatically <br/>
% Direct mode the effective address is the value in W <br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of W, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {value, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec auto_decrement_of_w_pre_decrement_by_2(
												Pos 				::position_type(),
												Data 				::binary(),
												CPU_Data 			::cpu_type(),
												Number_Of_Bytes 	::non_neg_integer()
											  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
auto_decrement_of_w_pre_decrement_by_2(
											Pos,
											Data,
											CPU_Data,
											Number_Of_Bytes
										) ->


	Post_Byte 												= get_byte_pos(Data,Pos),

	<<_PB7:1,_PB6:1,_PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	Indirect_Field 											= PB4,

	<<Register_Value_0:16>> 								= cpu_get_w(CPU_Data),
	Register_Value 											= Register_Value_0 - 2,

	New_CPU_Data_ 											= cpu_set_w(<<Register_Value:16>>,CPU_Data),

	case Number_Of_Bytes of

		1	-> 	case Indirect_Field of

					0 ->	{get_byte_pos(Data,<<Register_Value:16>>), New_CPU_Data_, pos_inc(Pos)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Register_Value:16>>),
							IAL 							= get_byte_pos(Data,<<(Register_Value+1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

							{get_byte_pos(Data,Intermediate_Address), New_CPU_Data_, pos_inc(Pos)}
				end;

		2 	-> 	case Indirect_Field of

					0 ->	FH 				= get_byte_pos(Data,<<Register_Value:16>>),
							FL 				= get_byte_pos(Data,<<(Register_Value+1):16>>),
							Final  			= <<FH/bits,FL/bits>>,

							{Final, New_CPU_Data_, pos_inc(Pos)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Register_Value:16>>),
							IAL 							= get_byte_pos(Data,<<(Register_Value+1):16>>),
							Intermediate_Address 		 	= <<IAH/bits,IAL/bits>>,

							FH 								= get_byte_pos(Data,Intermediate_Address),
							FL 								= get_byte_pos(Data,pos_inc(Intermediate_Address)),
							Final							= <<FH/bits,FL/bits>>,
							{Final, New_CPU_Data_, pos_inc(Pos)}
				end
		end.

%%%=======================================================================================
%% @doc 
% Uses Register W <br/>
% First the value of W is decreased by 2 automatically <br/>
% Direct mode the effective address is the value in W <br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of W, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {address, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec auto_decrement_of_w_pre_decrement_by_2_address_only(
															Pos 				::position_type(),
															Data 				::binary(),
															CPU_Data 			::cpu_type(),
															Number_Of_Bytes 	::non_neg_integer()
														  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
auto_decrement_of_w_pre_decrement_by_2_address_only(
														Pos,
														Data,
														CPU_Data,
														_Number_Of_Bytes
													) ->


	Post_Byte 												= get_byte_pos(Data,Pos),

	<<_PB7:1,_PB6:1,_PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	Indirect_Field 											= PB4,

	<<Register_Value_0:16>> 								= cpu_get_w(CPU_Data),
	Register_Value 											= Register_Value_0 - 2,

	New_CPU_Data_ 											= cpu_set_w(<<Register_Value:16>>,CPU_Data),

	case Indirect_Field of

		0 ->	{<<Register_Value:16>>, New_CPU_Data_, pos_inc(Pos)};

		1 -> 	IAH 							= get_byte_pos(Data,<<Register_Value:16>>),
				IAL 							= get_byte_pos(Data,<<(Register_Value+1):16>>),
				Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

				{Intermediate_Address, New_CPU_Data_, pos_inc(Pos)}
	end.

%%%=======================================================================================
%% @doc 
% Uses Register R which is encoded in the post-byte <br/>
% and an 16-bit signed value from register D <br/>
% Direct mode the effective address is the value in R + 16-bit value<br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of R + 16-bit value, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {value, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec accumulator_offset_from_r_d_accumulator(
												Pos 				::position_type(),
												Data 				::binary(),
												CPU_Data 			::cpu_type(),
												Number_Of_Bytes 	::non_neg_integer()
											  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
accumulator_offset_from_r_d_accumulator(
											Pos,
											Data,
											CPU_Data,
											Number_Of_Bytes
										) ->


	Post_Byte 													= get_byte_pos(Data,Pos),

	<<_PB7:1,PB6:1,PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	<<RR:2>> 													= <<PB6:1,PB5:1>>,

	Indirect_Field 												= PB4,

	Register_Value 												= index_reg_value(RR,CPU_Data),

	<<Signed_Value_D:16/signed>> 								= cpu_get_d(CPU_Data),
	Sum 														= Register_Value + Signed_Value_D,

	case Number_Of_Bytes of

		1 	->	case Indirect_Field of

						0 ->	{get_byte_pos(Data,<<Sum:16>>), CPU_Data, pos_inc(Pos)};

						1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
								IAL 							= get_byte_pos(Data,<<(Sum + 1):16>>),
								Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

								{get_byte_pos(Data,Intermediate_Address), CPU_Data, pos_inc(Pos)}
					end;

		2 	->	case Indirect_Field of

					0 ->	FH 				= get_byte_pos(Data,<<Sum:16>>),
							FL 				= get_byte_pos(Data,<<(Sum+1):16>>),
							Final 			= <<FH/bits,FL/bits>>,

							{Final, CPU_Data, pos_inc(Pos)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
							IAL 							= get_byte_pos(Data,<<(Sum+1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,
							FH 								= get_byte_pos(Data,Intermediate_Address),
							FL 								= get_byte_pos(Data,pos_inc(Intermediate_Address)),
							Final 							= <<FH/bits,FL/bits>>,
							{Final, CPU_Data, pos_inc(Pos)}
				end

	end.

%%%=======================================================================================
%% @doc 
% Uses Register R which is encoded in the post-byte <br/>
% and an 16-bit signed value from register D <br/>
% Direct mode the effective address is the value in R + 16-bit value<br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of R + 16-bit value, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {address, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec accumulator_offset_from_r_d_accumulator_address_only(
															Pos 				::position_type(),
															Data 				::binary(),
															CPU_Data 			::cpu_type(),
															_Number_Of_Bytes 	::non_neg_integer()
														  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
accumulator_offset_from_r_d_accumulator_address_only(
														Pos,
														Data,
														CPU_Data,
														_Number_Of_Bytes
													) ->


	Post_Byte 													= get_byte_pos(Data,Pos),

	<<_PB7:1,PB6:1,PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	<<RR:2>> 													= <<PB6:1,PB5:1>>,

	Indirect_Field 												= PB4,

	Register_Value 												= index_reg_value(RR,CPU_Data),

	<<Signed_Value_D:16/signed>> 								= cpu_get_d(CPU_Data),
	Sum 														= Register_Value + Signed_Value_D,

	case Indirect_Field of

		0 ->	{<<Sum:16>>, CPU_Data, pos_inc(Pos)};

		1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
				IAL 							= get_byte_pos(Data,<<(Sum + 1):16>>),
				Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

				{Intermediate_Address, CPU_Data, pos_inc(Pos)}
	end.

%%%=======================================================================================
%% @doc 
% Uses Register PC <br/>
% and an 8-bit signed value from after the post-byte <br/>
% Direct mode the effective address is the value in PC + 8-bit value<br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of PC + 8-bit value, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {value, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec constant_offset_from_pc_8_bit_offset(
												Pos 				::position_type(),
												Data 				::binary(),
												CPU_Data 			::cpu_type(),
												Number_Of_Bytes 	::non_neg_integer()
											  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
constant_offset_from_pc_8_bit_offset(
										Pos,
										Data,
										CPU_Data,
										Number_Of_Bytes
									) ->


	Post_Byte 												= get_byte_pos(Data,Pos),

	<<_PB7:1,_PB6:1,_PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	Indirect_Field 											= PB4,

	<<PC:16>> 												= cpu_get_pc(CPU_Data),
	<<Signed_Next_Byte:8/signed>> 							= get_byte_pos(Data,pos_inc(Pos)),
	Sum 													= PC + Signed_Next_Byte,

	case Number_Of_Bytes of 

		1 	-> 	case Indirect_Field of

					0 ->	{get_byte_pos(Data,<<Sum:16>>), CPU_Data, pos_inc2(Pos)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
							IAL 							= get_byte_pos(Data,<<(Sum + 1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

							{get_byte_pos(Data,Intermediate_Address), CPU_Data, pos_inc2(Pos)}
				end;

		2 	->	case Indirect_Field of

					0 ->	FH 				= get_byte_pos(Data,<<Sum:16>>),
							FL 				= get_byte_pos(Data,<<(Sum+1):16>>),
							Final 			= <<FH/bits,FL/bits>>,

							{Final, CPU_Data, pos_inc2(Pos)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
							IAL 							= get_byte_pos(Data,<<(Sum+1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,
							FH 								= get_byte_pos(Data,Intermediate_Address),
							FL 								= get_byte_pos(Data,pos_inc(Intermediate_Address)),
							Final 							= <<FH/bits,FL/bits>>,

							{Final, CPU_Data, pos_inc2(Pos)}
				end

	end.

%%%=======================================================================================
%% @doc 
% Uses Register PC <br/>
% and an 8-bit signed value from after the post-byte <br/>
% Direct mode the effective address is the value in PC + 8-bit value<br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of PC + 8-bit value, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {address, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec constant_offset_from_pc_8_bit_offset_address_only(
															Pos 				::position_type(),
															Data 				::binary(),
															CPU_Data 			::cpu_type(),
															Number_Of_Bytes 	::non_neg_integer()
														  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
constant_offset_from_pc_8_bit_offset_address_only(
													Pos,
													Data,
													CPU_Data,
													_Number_Of_Bytes
												) ->


	Post_Byte 												= get_byte_pos(Data,Pos),

	<<_PB7:1,_PB6:1,_PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	Indirect_Field 											= PB4,

	<<PC:16>> 												= cpu_get_pc(CPU_Data),
	<<Signed_Next_Byte:8/signed>> 							= get_byte_pos(Data,pos_inc(Pos)),
	Sum 													= PC + Signed_Next_Byte,

	case Indirect_Field of

		0 ->	{<<Sum:16>>, CPU_Data, pos_inc2(Pos)};

		1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
				IAL 							= get_byte_pos(Data,<<(Sum + 1):16>>),
				Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

				{Intermediate_Address, CPU_Data, pos_inc2(Pos)}
	end.

%%%=======================================================================================
%% @doc 
% Uses Register PC <br/>
% and an 16-bit signed value from after the post-byte <br/>
% Direct mode the effective address is the value in PC + 16-bit value<br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of PC + 16-bit value, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {value, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec constant_offset_from_pc_16_bit_offset(
												Pos 				::position_type(),
												Data 				::binary(),
												CPU_Data 			::cpu_type(),
												Number_Of_Bytes 	::non_neg_integer()
											  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
constant_offset_from_pc_16_bit_offset(
										Pos,
										Data,
										CPU_Data,
										Number_Of_Bytes
									 ) ->


	Post_Byte 												= get_byte_pos(Data,Pos),

	<<_PB7:1,_PB6:1,_PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	Indirect_Field 											= PB4,

	<<PC:16>> 												= cpu_get_pc(CPU_Data),
	Next_Byte_H 											= get_byte_pos(Data,pos_inc(Pos)),
	Next_Byte_L 											= get_byte_pos(Data,pos_inc2(Pos)),
	<<Signed_Next_Word:16/signed>> 							= <<Next_Byte_H/bits,Next_Byte_L/bits>>,
	Sum 													= PC + Signed_Next_Word,

	case Number_Of_Bytes of 

		1 	->	case Indirect_Field of

					0 ->	{get_byte_pos(Data,<<Sum:16>>), CPU_Data, pos_inc_n(Pos,3)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
							IAL 							= get_byte_pos(Data,<<(Sum + 1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

							{get_byte_pos(Data,Intermediate_Address), CPU_Data, pos_inc_n(Pos,3)}
				end;

		2 	->	case Indirect_Field of

					0 ->	FH 				= get_byte_pos(Data,<<Sum:16>>),
							FL 				= get_byte_pos(Data,<<(Sum+1):16>>),
							Final 			= <<FH/bits,FL/bits>>,

							{Final, CPU_Data, pos_inc_n(Pos,3)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
							IAL 							= get_byte_pos(Data,<<(Sum+1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,
							FH 								= get_byte_pos(Data,Intermediate_Address),
							FL 								= get_byte_pos(Data,pos_inc(Intermediate_Address)),
							Final 							= <<FH/bits,FL/bits>>,

							{Final, CPU_Data, pos_inc_n(Pos,3)}
				end

	end.

%%%=======================================================================================
%% @doc 
% Uses Register PC <br/>
% and an 16-bit signed value from after the post-byte <br/>
% Direct mode the effective address is the value in PC + 16-bit value<br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of PC + 16-bit value, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {address, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec constant_offset_from_pc_16_bit_offset_address_only(
															Pos 				::position_type(),
															Data 				::binary(),
															CPU_Data 			::cpu_type(),
															_Number_Of_Bytes 	::non_neg_integer()
														  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
constant_offset_from_pc_16_bit_offset_address_only(
													Pos,
													Data,
													CPU_Data,
													_Number_Of_Bytes
												  ) ->


	Post_Byte 												= get_byte_pos(Data,Pos),

	<<_PB7:1,_PB6:1,_PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	Indirect_Field 											= PB4,

	<<PC:16>> 												= cpu_get_pc(CPU_Data),
	Next_Byte_H 											= get_byte_pos(Data,pos_inc(Pos)),
	Next_Byte_L 											= get_byte_pos(Data,pos_inc2(Pos)),
	<<Signed_Next_Word:16/signed>> 							= <<Next_Byte_H/bits,Next_Byte_L/bits>>,
	Sum 													= PC + Signed_Next_Word,

	case Indirect_Field of

		0 ->	{<<Sum:16>>, CPU_Data, pos_inc_n(Pos,3)};

		1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
				IAL 							= get_byte_pos(Data,<<(Sum + 1):16>>),
				Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

				{Intermediate_Address, CPU_Data, pos_inc_n(Pos,3)}
	end.

%%%=======================================================================================
%% @doc 
% Used 16-bit address value from after the post-byte <br/>
% Indirect mode only in which the effective address is determined by<br/>
% first getting an address from the value of the 16-bits, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {value, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec extended_indirect_16_bit_address(
										Pos 				::position_type(),
										Data 				::binary(),
										CPU_Data 			::cpu_type(),
										Number_Of_Bytes 	::non_neg_integer()
									  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
extended_indirect_16_bit_address(
									Pos,
									Data,
									CPU_Data,
									Number_Of_Bytes
								) ->


	Next_Byte_H 											= get_byte_pos(Data,pos_inc(Pos)),
	Next_Byte_L 											= get_byte_pos(Data,pos_inc2(Pos)),
	Address 												= <<Next_Byte_H/bits,Next_Byte_L/bits>>,

	case Number_Of_Bytes of 

		1 	->	IAH 							= get_byte_pos(Data,Address),
				IAL 							= get_byte_pos(Data,pos_inc(Address)),
				Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

				{get_byte_pos(Data,Intermediate_Address), CPU_Data, pos_inc_n(Pos,3)};

		2	->	IAH 							= get_byte_pos(Data,Address),
				IAL 							= get_byte_pos(Data,pos_inc(Address)),
				Intermediate_Address		 	= <<IAH/bits,IAL/bits>>,
				FH 								= get_byte_pos(Data,Intermediate_Address),
				FL 								= get_byte_pos(Data,pos_inc(Intermediate_Address)),
				Final							= <<FH/bits,FL/bits>>,

				{Final, CPU_Data, pos_inc_n(Pos,3)}

	end.
	
%%%=======================================================================================
%% @doc 
% Used 16-bit address value from after the post-byte <br/>
% Indirect mode only in which the effective address is determined by<br/>
% first getting an address from the value of the 16-bits, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {address, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec extended_indirect_16_bit_address_address_only(
														Pos 				::position_type(),
														Data 				::binary(),
														CPU_Data 			::cpu_type(),
														Number_Of_Bytes 	::non_neg_integer()
													  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
extended_indirect_16_bit_address_address_only(
												Pos,
												Data,
												CPU_Data,
												_Number_Of_Bytes
											) ->

	Next_Byte_H 											= get_byte_pos(Data,pos_inc(Pos)),
	Next_Byte_L 											= get_byte_pos(Data,pos_inc2(Pos)),
	Address 												= <<Next_Byte_H/bits,Next_Byte_L/bits>>,

	IAH 							= get_byte_pos(Data,Address),
	IAL 							= get_byte_pos(Data,pos_inc(Address)),
	Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

	{Intermediate_Address, CPU_Data, pos_inc_n(Pos,3)}.

%%%=======================================================================================
%% @doc 
% Uses Register R which is encoded in the post-byte <br/>
% and an 8-bit signed value from register E <br/>
% Direct mode the effective address is the value in R + 8-bit value<br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of R + 8-bit value, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {value, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec accumulator_offset_from_r_e_accumulator(
												Pos 				::position_type(),
												Data 				::binary(),
												CPU_Data 			::cpu_type(),
												Number_Of_Bytes 	::non_neg_integer()
											  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
accumulator_offset_from_r_e_accumulator(
											Pos,
											Data,
											CPU_Data,
											Number_Of_Bytes
										) ->


	Post_Byte 													= get_byte_pos(Data,Pos),

	<<_PB7:1,PB6:1,PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	<<RR:2>> 													= <<PB6:1,PB5:1>>,

	Indirect_Field 												= PB4,

	Register_Value 												= index_reg_value(RR,CPU_Data),

	<<Signed_Value_E:8/signed>> 								= cpu_get_e(CPU_Data),
	Sum 														= Register_Value + Signed_Value_E,

	case Number_Of_Bytes of


		1 	-> 	case Indirect_Field of

					0 ->	{get_byte_pos(Data,<<Sum:16>>), CPU_Data, pos_inc(Pos)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
							IAL 							= get_byte_pos(Data,<<(Sum+1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

							{get_byte_pos(Data,Intermediate_Address), CPU_Data, pos_inc(Pos)}
				end;

		2 	->	case Indirect_Field of

					0 ->	FH 				= get_byte_pos(Data,<<Sum:16>>),
							FL 				= get_byte_pos(Data,<<(Sum+1):16>>),
							Final 			= <<FH/bits,FL/bits>>,

							{Final, CPU_Data, pos_inc(Pos)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
							IAL 							= get_byte_pos(Data,<<(Sum+1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,
							FH 								= get_byte_pos(Data,Intermediate_Address),
							FL 								= get_byte_pos(Data,pos_inc(Intermediate_Address)),
							Final 							= <<FH/bits,FL/bits>>,

							{Final, CPU_Data, pos_inc(Pos)}
				end
		end.

%%%=======================================================================================
%% @doc 
% Uses Register R which is encoded in the post-byte <br/>
% and an 8-bit signed value from register E <br/>
% Direct mode the effective address is the value in R + 8-bit value<br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of R + 8-bit value, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {address, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec accumulator_offset_from_r_e_accumulator_address_only(
															Pos 				::position_type(),
															Data 				::binary(),
															CPU_Data 			::cpu_type(),
															Number_Of_Bytes 	::non_neg_integer()
														  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
accumulator_offset_from_r_e_accumulator_address_only(
														Pos,
														Data,
														CPU_Data,
														_Number_Of_Bytes
													) ->


	Post_Byte 													= get_byte_pos(Data,Pos),

	<<_PB7:1,PB6:1,PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	<<RR:2>> 													= <<PB6:1,PB5:1>>,

	Indirect_Field 												= PB4,

	Register_Value 												= index_reg_value(RR,CPU_Data),

	<<Signed_Value_E:8/signed>> 								= cpu_get_e(CPU_Data),
	Sum 														= Register_Value + Signed_Value_E,

	case Indirect_Field of

		0 ->	{<<Sum:16>>, CPU_Data, pos_inc(Pos)};

		1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
				IAL 							= get_byte_pos(Data,<<(Sum+1):16>>),
				Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

				{Intermediate_Address, CPU_Data, pos_inc(Pos)}
	end.

%%%=======================================================================================
%% @doc 
% Uses Register R which is encoded in the post-byte <br/>
% and an 8-bit signed value from register F <br/>
% Direct mode the effective address is the value in R + 8-bit value<br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of R + 8-bit value, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {value, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec accumulator_offset_from_r_f_accumulator(
												Pos 				::position_type(),
												Data 				::binary(),
												CPU_Data 			::cpu_type(),
												Number_Of_Bytes 	::non_neg_integer()
											  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
accumulator_offset_from_r_f_accumulator(
											Pos,
											Data,
											CPU_Data,
											Number_Of_Bytes
										) ->


	Post_Byte 													= get_byte_pos(Data,Pos),

	<<_PB7:1,PB6:1,PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	<<RR:2>> 													= <<PB6:1,PB5:1>>,

	Indirect_Field 												= PB4,

	Register_Value 												= index_reg_value(RR,CPU_Data),

	<<Signed_Value_F:8/signed>> 								= cpu_get_f(CPU_Data),
	Sum 														= Register_Value + Signed_Value_F,

	case Number_Of_Bytes of


		1 	-> 	case Indirect_Field of

					0 ->	{get_byte_pos(Data,<<Sum:16>>), CPU_Data, pos_inc(Pos)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
							IAL 							= get_byte_pos(Data,<<(Sum+1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

							{get_byte_pos(Data,Intermediate_Address), CPU_Data, pos_inc(Pos)}
				end;

		2 	->	case Indirect_Field of

					0 ->	FH 				= get_byte_pos(Data,<<Sum:16>>),
							FL 				= get_byte_pos(Data,<<(Sum+1):16>>),
							Final 			= <<FH/bits,FL/bits>>,

							{Final, CPU_Data, pos_inc(Pos)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
							IAL 							= get_byte_pos(Data,<<(Sum+1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,
							FH 								= get_byte_pos(Data,Intermediate_Address),
							FL 								= get_byte_pos(Data,pos_inc(Intermediate_Address)),
							Final 							= <<FH/bits,FL/bits>>,

							{Final, CPU_Data, pos_inc(Pos)}
				end
		end.

%%%=======================================================================================
%% @doc 
% Uses Register R which is encoded in the post-byte <br/>
% and an 8-bit signed value from register F <br/>
% Direct mode the effective address is the value in R + 8-bit value<br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of R + 8-bit value, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {address, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec accumulator_offset_from_r_f_accumulator_address_only(
															Pos 				::position_type(),
															Data 				::binary(),
															CPU_Data 			::cpu_type(),
															Number_Of_Bytes 	::non_neg_integer()
														  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
accumulator_offset_from_r_f_accumulator_address_only(
														Pos,
														Data,
														CPU_Data,
														_Number_Of_Bytes
													) ->


	Post_Byte 													= get_byte_pos(Data,Pos),

	<<_PB7:1,PB6:1,PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	<<RR:2>> 													= <<PB6:1,PB5:1>>,

	Indirect_Field 												= PB4,

	Register_Value 												= index_reg_value(RR,CPU_Data),

	<<Signed_Value_F:8/signed>> 								= cpu_get_f(CPU_Data),
	Sum 														= Register_Value + Signed_Value_F,

	case Indirect_Field of

		0 ->	{<<Sum:16>>, CPU_Data, pos_inc(Pos)};

		1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
				IAL 							= get_byte_pos(Data,<<(Sum+1):16>>),
				Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

				{Intermediate_Address, CPU_Data, pos_inc(Pos)}
	end.

%%%=======================================================================================
%% @doc 
% Uses Register R which is encoded in the post-byte <br/>
% and an 16-bit signed value from register W <br/>
% Direct mode the effective address is the value in R + 16-bit value<br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of R + 16-bit value, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {value, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec accumulator_offset_from_r_w_accumulator(
												Pos 				::position_type(),
												Data 				::binary(),
												CPU_Data 			::cpu_type(),
												Number_Of_Bytes 	::non_neg_integer()
											  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
accumulator_offset_from_r_w_accumulator(
											Pos,
											Data,
											CPU_Data,
											Number_Of_Bytes
										) ->


	Post_Byte 												= get_byte_pos(Data,Pos),

	<<_PB7:1,PB6:1,PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	<<RR:2>> 												= <<PB6:1,PB5:1>>,

	Indirect_Field 											= PB4,

	Register_Value 											= index_reg_value(RR,CPU_Data),

	<<Signed_Value_W:16/signed>> 							= cpu_get_w(CPU_Data),
	Sum 													= Register_Value + Signed_Value_W,

	case Number_Of_Bytes of

		1 	->	case Indirect_Field of

						0 ->	{get_byte_pos(Data,<<Sum:16>>), CPU_Data, pos_inc(Pos)};

						1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
								IAL 							= get_byte_pos(Data,<<(Sum + 1):16>>),
								Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

								{get_byte_pos(Data,Intermediate_Address), CPU_Data, pos_inc(Pos)}
					end;

		2 	->	case Indirect_Field of

					0 ->	FH 				= get_byte_pos(Data,<<Sum:16>>),
							FL 				= get_byte_pos(Data,<<(Sum+1):16>>),
							Final 			= <<FH/bits,FL/bits>>,

							{Final, CPU_Data, pos_inc(Pos)};

					1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
							IAL 							= get_byte_pos(Data,<<(Sum+1):16>>),
							Intermediate_Address 			= <<IAH/bits,IAL/bits>>,
							FH 								= get_byte_pos(Data,Intermediate_Address),
							FL 								= get_byte_pos(Data,pos_inc(Intermediate_Address)),
							Final 							= <<FH/bits,FL/bits>>,
							{Final, CPU_Data, pos_inc(Pos)}
				end

	end.

%%%=======================================================================================
%% @doc 
% Uses Register R which is encoded in the post-byte <br/>
% and an 16-bit signed value from register W <br/>
% Direct mode the effective address is the value in R + 16-bit value<br/>
% Indirect mode the effective address is determine by<br/>
% first getting an address from the value of R + 16-bit value, and then getting<br/>
% an another address from that memory location <br/>
% Returns: {address, CPU_Data, New_Position} <br/>
% ---- Unit-Tested ----<br/>
-spec accumulator_offset_from_r_w_accumulator_address_only(
															Pos 				::position_type(),
															Data 				::binary(),
															CPU_Data 			::cpu_type(),
															Number_Of_Bytes 	::non_neg_integer()
														  ) -> { binary(), cpu_type(), position_type() }.
%%%=======================================================================================
accumulator_offset_from_r_w_accumulator_address_only(
														Pos,
														Data,
														CPU_Data,
														_Number_Of_Bytes
													) ->


	Post_Byte 												= get_byte_pos(Data,Pos),

	<<_PB7:1,PB6:1,PB5:1,PB4:1,_PB3:1,_PB2:1,_PB1:1,_PB0:1>> 	= Post_Byte,

	<<RR:2>> 												= <<PB6:1,PB5:1>>,

	Indirect_Field 											= PB4,

	Register_Value 											= index_reg_value(RR,CPU_Data),

	<<Signed_Value_W:16/signed>> 							= cpu_get_w(CPU_Data),
	Sum 													= Register_Value + Signed_Value_W,

	case Indirect_Field of

		0 ->	{<<Sum:16>>, CPU_Data, pos_inc(Pos)};

		1 -> 	IAH 							= get_byte_pos(Data,<<Sum:16>>),
				IAL 							= get_byte_pos(Data,<<(Sum + 1):16>>),
				Intermediate_Address 			= <<IAH/bits,IAL/bits>>,

				{Intermediate_Address, CPU_Data, pos_inc(Pos)}
	end.

%%%=======================================================================================
%% @doc 
% Extracts the value of the encode Register RR from the provided CPU_Data<br/>
% Returns: Integer Value not binary value<br/>
% ---- Unit-Tested ----<br/>
-spec index_reg_value(RR::non_neg_integer(), CPU_Data::cpu_type()) -> integer().
%%%=======================================================================================
index_reg_value(RR, CPU_Data) ->

	case RR of

		2#00 -> <<X:16>> = cpu_get_x(CPU_Data), X;
		2#01 -> <<Y:16>> = cpu_get_y(CPU_Data), Y;
		2#10 -> <<U:16>> = cpu_get_u(CPU_Data), U;
		2#11 -> <<S:16>> = cpu_get_s(CPU_Data), S
	end.

%%%=======================================================================================
%% @doc 
% Set a value to a register determined by RR encoding<br/>
% Returns: updated CPU_Data<br/>
% ---- Unit-Tested ----<br/>
-spec index_reg_set_value(RR::non_neg_integer(), CPU_Data::cpu_type(), Value::integer()) -> cpu_type().
%%%=======================================================================================
index_reg_set_value(RR,CPU_Data,Value) ->

	case RR of

		2#00 -> cpu_set_x(<<Value:16>>, CPU_Data);
		2#01 -> cpu_set_y(<<Value:16>>, CPU_Data);
		2#10 -> cpu_set_u(<<Value:16>>, CPU_Data);
		2#11 -> cpu_set_s(<<Value:16>>, CPU_Data)
	end.

%%%=======================================================================================
%% @doc 
% Takes a CPU_Data and increases the PC value by 1<br/>
% Returns: updated CPU_Data<br/>
% ---- Unit-Tested ----<br/>
-spec pc_1_inc(CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
pc_1_inc(CPU_Data) ->

	<<Current_PC:16>> 	= cpu_get_pc(CPU_Data),

	New_Current_PC  	= Current_PC + 1,

	cpu_set_pc(<<New_Current_PC:16>>, CPU_Data).

%%%=======================================================================================
%% @doc 
% Takes a CPU_Data and increases the PC value by 2<br/>
% Returns: updated CPU_Data<br/>
% ---- Unit-Tested ----<br/>
-spec pc_2_inc(CPU_Data::cpu_type()) -> cpu_type().
%%%=======================================================================================
pc_2_inc(CPU_Data) ->

	<<Current_PC:16>> 	= cpu_get_pc(CPU_Data),

	New_Current_PC  	= Current_PC + 2,

	cpu_set_pc(<<New_Current_PC:16>>,CPU_Data).

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Indexed No Offset from R Direct Addressing<br/>
% Returns: Post_Byte size 1<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_indexed_no_offset_r_direct(R::atom()) -> binary().
%%%=======================================================================================
post_byte_indexed_no_offset_r_direct(R) -> 
	
	R_Bits = rr_from_r(R),
	<<1:1,R_Bits/bits,0:1,0:1,1:1,0:1,0:1>>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Indexed No Offset from R Indirect Addressing<br/>
% Returns: Post_Byte size 1<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_indexed_no_offset_r_indirect(R::atom()) -> binary().
%%%=======================================================================================
post_byte_indexed_no_offset_r_indirect(R) -> 
	
	R_Bits = rr_from_r(R),
	<<1:1,R_Bits/bits,1:1,0:1,1:1,0:1,0:1>>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Indexed 5-bit Offset from R Direct Addressing<br/>
% Returns: Post_Byte size 1<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_indexed_5_offset_r_direct(R::atom(),Offset::integer()) -> binary().
%%%=======================================================================================
post_byte_indexed_5_offset_r_direct(R,Offset) ->

	Offset_Bits = <<Offset:5/signed>>,
	R_Bits = rr_from_r(R),
	<<0:1,R_Bits/bits,Offset_Bits/bits>>.

%%-----------------------------------------------------------------------------
%% Note: indirect 5 bit offset not available.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Indexed 8-bit Offset from R Direct Addressing<br/>
% Returns: Post_Byte size 2<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_indexed_8_offset_r_direct(R::atom(),Offset::integer()) -> binary().
%%%=======================================================================================
post_byte_indexed_8_offset_r_direct(R,Offset) ->

	R_Bits = rr_from_r(R),
	<<1:1,R_Bits/bits,0:1,1:1,0:1,0:1,0:1,Offset:8/signed>>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Indexed 8-bit Offset from R Indirect Addressing<br/>
% Returns: Post_Byte size 2<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_indexed_8_offset_r_indirect(R::atom(),Offset::integer()) -> binary().
%%%=======================================================================================
post_byte_indexed_8_offset_r_indirect(R,Offset) ->

	R_Bits = rr_from_r(R),
	<<1:1,R_Bits/bits,1:1,1:1,0:1,0:1,0:1,Offset:8/signed>>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Indexed 16-bit Offset from R Direct Addressing<br/>
% Returns: Post_Byte size 3<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_indexed_16_offset_r_direct(R::atom(),Offset::integer()) -> binary().
%%%=======================================================================================
post_byte_indexed_16_offset_r_direct(R,Offset) ->

	R_Bits = rr_from_r(R),
	<<1:1,R_Bits/bits,0:1,1:1,0:1,0:1,1:1,Offset:16/signed>>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Indexed 16-bit Offset from R Indirect Addressing<br/>
% Returns: Post_Byte size 3<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_indexed_16_offset_r_indirect(R::atom(),Offset::integer()) -> binary().
%%%=======================================================================================
post_byte_indexed_16_offset_r_indirect(R,Offset) ->

	R_Bits = rr_from_r(R),
	<<1:1,R_Bits/bits,1:1,1:1,0:1,0:1,1:1,Offset:16/signed>>.

%%%=======================================================================================
%% @doc 
% Generates RR Register encoding from register name (atom)<br/>
% ---- Unit-Tested ----<br/>
-spec rr_from_r(R::atom()) -> <<_:2>>.
%%%=======================================================================================
rr_from_r(R) ->

	case R of

		x -> <<0:1,0:1>>;
		y -> <<0:1,1:1>>;
		u -> <<1:1,0:1>>;
		s -> <<1:1,1:1>>
	end.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Indexed No Offset from W Direct Addressing<br/>
% Returns: Post_Byte size 1<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_indexed_no_offset_w_direct() -> <<_:8>>.
%%%=======================================================================================
post_byte_indexed_no_offset_w_direct() -> 
	
	<< 1:1, 0:1, 0:1, 0:1, 1:1, 1:1, 1:1, 1:1 >>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Indexed No Offset from W Indirect Addressing<br/>
% Returns: Post_Byte size 1<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_indexed_no_offset_w_indirect() -> <<_:8>>.
%%%=======================================================================================
post_byte_indexed_no_offset_w_indirect() -> 
	
	<< 1:1, 0:1, 0:1, 1:1, 0:1, 0:1, 0:1, 0:1 >>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Indexed 16-bit Offset from W Direct Addressing<br/>
% Returns: Post_Byte size 3<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_indexed_16_offset_w_direct(Offset::integer()) -> binary().
%%%=======================================================================================
post_byte_indexed_16_offset_w_direct(Offset) ->

	<<1:1, 0:1, 1:1, 0:1, 1:1, 1:1, 1:1, 1:1, Offset:16/signed>>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Indexed 16-bit Offset from W Indirect Addressing<br/>
% Returns: Post_Byte size 3<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_indexed_16_offset_w_indirect(Offset::integer()) -> binary().
%%%=======================================================================================
post_byte_indexed_16_offset_w_indirect(Offset) ->

	<<1:1, 0:1, 1:1, 1:1, 0:1, 0:1, 0:1, 0:1, Offset:16/signed>>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Register A Offset from R Direct Addressing<br/>
% Returns: Post_Byte size 1<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_indexed_a_offset_r_direct(R::atom()) -> binary().
%%%=======================================================================================
post_byte_indexed_a_offset_r_direct(R) ->

	R_Bits = rr_from_r(R),
	<< 1:1, R_Bits/bits, 0:1, 0:1, 1:1, 1:1, 0:1>>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Register A Offset from R Indirect Addressing<br/>
% Returns: Post_Byte size 1<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_indexed_a_offset_r_indirect(R::atom()) -> binary().
%%%=======================================================================================
post_byte_indexed_a_offset_r_indirect(R) ->

	R_Bits = rr_from_r(R),
	<< 1:1, R_Bits/bits, 1:1, 0:1, 1:1, 1:1, 0:1>>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Register B Offset from R Direct Addressing<br/>
% Returns: Post_Byte size 1<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_indexed_b_offset_r_direct(R::atom()) -> binary().
%%%=======================================================================================
post_byte_indexed_b_offset_r_direct(R) ->

	R_Bits = rr_from_r(R),
	<< 1:1, R_Bits/bits, 0:1, 0:1, 1:1, 0:1, 1:1>>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Register B Offset from R Indirect Addressing<br/>
% Returns: Post_Byte size 1<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_indexed_b_offset_r_indirect(R::atom()) -> binary().
%%%=======================================================================================
post_byte_indexed_b_offset_r_indirect(R) ->

	R_Bits = rr_from_r(R),
	<< 1:1, R_Bits/bits, 1:1, 0:1, 1:1, 0:1, 1:1>>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Register D Offset from R Direct Addressing<br/>
% Returns: Post_Byte size 1<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_indexed_d_offset_r_direct(R::atom()) -> binary().
%%%=======================================================================================
post_byte_indexed_d_offset_r_direct(R) ->

	R_Bits = rr_from_r(R),
	<< 1:1, R_Bits/bits, 0:1, 1:1, 0:1, 1:1, 1:1>>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Register D Offset from R Indirect Addressing<br/>
% Returns: Post_Byte size 1<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_indexed_d_offset_r_indirect(R::atom()) -> binary().
%%%=======================================================================================
post_byte_indexed_d_offset_r_indirect(R) ->

	R_Bits = rr_from_r(R),
	<< 1:1, R_Bits/bits, 1:1, 1:1, 0:1, 1:1, 1:1>>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Register E Offset from R Direct Addressing<br/>
% Returns: Post_Byte size 1<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_indexed_e_offset_r_direct(R::atom()) -> binary().
%%%=======================================================================================
post_byte_indexed_e_offset_r_direct(R) ->

	R_Bits = rr_from_r(R),
	<< 1:1, R_Bits/bits, 0:1, 0:1, 1:1, 1:1, 1:1>>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Register E Offset from R Indirect Addressing<br/>
% Returns: Post_Byte size 1<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_indexed_e_offset_r_indirect(R::atom()) -> binary().
%%%=======================================================================================
post_byte_indexed_e_offset_r_indirect(R) ->

	R_Bits = rr_from_r(R),
	<< 1:1, R_Bits/bits, 1:1, 0:1, 1:1, 1:1, 1:1>>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Register F Offset from R Direct Addressing<br/>
% Returns: Post_Byte size 1<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_indexed_f_offset_r_direct(R::atom()) -> binary().
%%%=======================================================================================
post_byte_indexed_f_offset_r_direct(R) ->

	R_Bits = rr_from_r(R),
	<< 1:1, R_Bits/bits, 0:1, 1:1, 0:1, 1:1, 0:1>>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Register F Offset from R Indirect Addressing<br/>
% Returns: Post_Byte size 1<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_indexed_f_offset_r_indirect(R::atom()) -> binary().
%%%=======================================================================================
post_byte_indexed_f_offset_r_indirect(R) ->

	R_Bits = rr_from_r(R),
	<< 1:1, R_Bits/bits, 1:1, 1:1, 0:1, 1:1, 0:1>>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Register W Offset from R Direct Addressing<br/>
% Returns: Post_Byte size 1<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_indexed_w_offset_r_direct(R::atom()) -> binary().
%%%=======================================================================================
post_byte_indexed_w_offset_r_direct(R) ->

	R_Bits = rr_from_r(R),
	<< 1:1, R_Bits/bits, 0:1, 1:1, 1:1, 1:1, 0:1>>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Register W Offset from R Indirect Addressing<br/>
% Returns: Post_Byte size 1<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_indexed_w_offset_r_indirect(R::atom()) -> binary().
%%%=======================================================================================
post_byte_indexed_w_offset_r_indirect(R) ->

	R_Bits = rr_from_r(R),
	<< 1:1, R_Bits/bits, 1:1, 1:1, 1:1, 1:1, 0:1>>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Post Increment 1 of R Direct Addressing<br/>
% Returns: Post_Byte size 1<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_post_increment_1_r_direct(R::atom()) -> binary().
%%%=======================================================================================
post_byte_post_increment_1_r_direct(R) ->

	R_Bits = rr_from_r(R),
	<< 1:1, R_Bits/bits, 0:1, 0:1, 0:1, 0:1, 0:1>>.

% Note: post increment 1 r indirect not allowed.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Post Increment 2 of R Direct Addressing<br/>
% Returns: Post_Byte size 1<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_post_increment_2_r_direct(R::atom()) -> binary().
%%%=======================================================================================
post_byte_post_increment_2_r_direct(R) ->

	R_Bits = rr_from_r(R),
	<< 1:1, R_Bits/bits, 0:1, 0:1, 0:1, 0:1, 1:1>>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Post Increment 2 of R Indirect Addressing<br/>
% Returns: Post_Byte size 1<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_post_increment_2_r_indirect(R::atom()) -> binary().
%%%=======================================================================================
post_byte_post_increment_2_r_indirect(R) ->

	R_Bits = rr_from_r(R),
	<< 1:1, R_Bits/bits, 1:1, 0:1, 0:1, 0:1, 1:1>>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Pre Decrement 1 of R Direct Addressing<br/>
% Returns: Post_Byte size 1<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_pre_decrement_1_r_direct(R::atom()) -> binary().
%%%=======================================================================================
post_byte_pre_decrement_1_r_direct(R) ->

	R_Bits = rr_from_r(R),
	<< 1:1, R_Bits/bits, 0:1, 0:1, 0:1, 1:1, 0:1>>.

% Note: pre decrement 1 r indirect not allowed.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Pre Decrement 2 of R Direct Addressing<br/>
% Returns: Post_Byte size 1<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_pre_decrement_2_r_direct(R::atom()) -> binary().
%%%=======================================================================================
post_byte_pre_decrement_2_r_direct(R) ->

	R_Bits = rr_from_r(R),
	<< 1:1, R_Bits/bits, 0:1, 0:1, 0:1, 1:1, 1:1>>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Pre Decrement 2 of R Indirect Addressing<br/>
% Returns: Post_Byte size 1<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_pre_decrement_2_r_indirect(R::atom()) -> binary().
%%%=======================================================================================
post_byte_pre_decrement_2_r_indirect(R) ->

	R_Bits = rr_from_r(R),
	<< 1:1, R_Bits/bits, 1:1, 0:1, 0:1, 1:1, 1:1>>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Post Increment 2 of W Direct Addressing<br/>
% Returns: Post_Byte size 1<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_post_increment_2_w_direct() -> binary().
%%%=======================================================================================
post_byte_post_increment_2_w_direct() ->

	<< 1:1, 1:1, 0:1, 0:1, 1:1, 1:1, 1:1, 1:1 >>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Post Increment 2 of W Indirect Addressing<br/>
% Returns: Post_Byte size 1<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_post_increment_2_w_indirect() -> binary().
%%%=======================================================================================
post_byte_post_increment_2_w_indirect() ->

	<< 1:1, 1:1, 0:1, 1:1, 0:1, 0:1, 0:1, 0:1 >>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Pre Decrement 2 of W Direct Addressing<br/>
% Returns: Post_Byte size 1<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_pre_decrement_2_w_direct() -> binary().
%%%=======================================================================================
post_byte_pre_decrement_2_w_direct() ->

	<< 2#11101111>>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Pre Decrement 2 of W Indirect Addressing<br/>
% Returns: Post_Byte size 1<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_pre_decrement_2_w_indirect() -> binary().
%%%=======================================================================================
post_byte_pre_decrement_2_w_indirect() ->

	<< 2#11110000>>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Indexed 8-bit Offset from PC Direct Addressing<br/>
% Returns: Post_Byte size 2<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_8_offset_pc_direct(Offset::integer()) -> binary().
%%%=======================================================================================
post_byte_8_offset_pc_direct(Offset) ->

	<<1:1, 0:1, 0:1, 0:1, 1:1, 1:1, 0:1, 0:1, Offset:8/signed>>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Indexed 8-bit Offset from PC Indirect Addressing<br/>
% Returns: Post_Byte size 2<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_8_offset_pc_indirect(Offset::integer()) -> binary().
%%%=======================================================================================
post_byte_8_offset_pc_indirect(Offset) ->

	<<1:1, 0:1, 0:1, 1:1, 1:1, 1:1, 0:1, 0:1, Offset:8/signed>>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Indexed 16-bit Offset from PC Direct Addressing<br/>
% Returns: Post_Byte size 3<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_16_offset_pc_direct(Offset::integer()) -> binary().
%%%=======================================================================================
post_byte_16_offset_pc_direct(Offset) ->

	<<1:1, 0:1, 0:1, 0:1, 1:1, 1:1, 0:1, 1:1, Offset:16/signed>>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Indexed 16-bit Offset from PC Indirect Addressing<br/>
% Returns: Post_Byte size 3<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_16_offset_pc_indirect(Offset::integer()) -> binary().
%%%=======================================================================================
post_byte_16_offset_pc_indirect(Offset) ->

	<<1:1, 0:1, 0:1, 1:1, 1:1, 1:1, 0:1, 1:1, Offset:16/signed>>.

%%%=======================================================================================
%% @doc 
% Generates a Post-Byte for Extended 16-bit Offset Indirect Addressing<br/>
% Returns: Post_Byte size 3<br/>
% ---- Unit-Tested ----<br/>
-spec post_byte_extended_16_address_indirect(Address::binary()) -> binary().
%%%=======================================================================================
post_byte_extended_16_address_indirect(Address) ->

	<<2#10011111,Address/bits>>.

%%%=======================================================================================
%% @doc 
% Updates the conditions codes via the Map<br/>
% Returns the updated CPU representation<br/>
% ---- Unit-Tested ----<br/>
-spec update_condition_codes_from_map(Map::map(),CPU_Data::binary()) -> binary().
%%%=======================================================================================
update_condition_codes_from_map(Map,CPU_Data) 
   when
   		is_map(Map),
   		is_binary(CPU_Data)
   ->

	E_CPU 	= CPU_Data,

	F_CPU 	= check_cc_status_update(Map,E_CPU,e,fun cpu_set_cc_e/2),
	H_CPU 	= check_cc_status_update(Map,F_CPU,f,fun cpu_set_cc_f/2),
	I_CPU 	= check_cc_status_update(Map,H_CPU,h,fun cpu_set_cc_h/2),
	N_CPU 	= check_cc_status_update(Map,I_CPU,i,fun cpu_set_cc_i/2),
	Z_CPU 	= check_cc_status_update(Map,N_CPU,n,fun cpu_set_cc_n/2),
	V_CPU 	= check_cc_status_update(Map,Z_CPU,z,fun cpu_set_cc_z/2),
	C_CPU 	= check_cc_status_update(Map,V_CPU,v,fun cpu_set_cc_v/2),

	check_cc_status_update(Map,C_CPU,c,fun cpu_set_cc_c/2).

%%%=======================================================================================
%% @doc 
% Checks if Key (atom) is in the Map<br/>
% If it is extract its value from the Map and update the condition code for that key<br/>
% Returns the updated CPU representation<br/>
% ---- Unit-Tested ----<br/>
-spec check_cc_status_update(Map::map(),CPU_Data::binary(),Key::atom(),Fun::function()) -> binary().
%%%=======================================================================================
check_cc_status_update(Map, CPU_Data, Key, Fun) 
    when 	
    		is_map(Map),
    		is_binary(CPU_Data),
    		is_atom(Key),
    		is_function(Fun)
    ->

	Status 	= maps:is_key(Key,Map),

	case Status of
			true 	-> 	Value = <<(maps:get(Key,Map)):1>>,
						Fun(Value,CPU_Data);
			_ 		->	CPU_Data
		end.

%%%=======================================================================================
%% @doc 
% Decodes the inter register names from a post byte <br/>
% Returns: a tuple with the register names as atoms<br/>
% E.g. {d,a} <br/>
% ---- Unit-Tested ----<br/>
-spec decode_inter_register_post_byte(Post_Byte::binary()) -> {atom(),atom()}.
%%%=======================================================================================
decode_inter_register_post_byte(Post_Byte) ->

	<<Source_Bits:4,Destination_Bits:4>> 	= Post_Byte,

	Source_Register 						= decode_inter_register_bits(<<Source_Bits:4>>),

	Destination_Register 					= decode_inter_register_bits(<<Destination_Bits:4>>),

	{Source_Register, Destination_Register}.

%%%=======================================================================================
%% @doc 
% Decode an Inter Register Bit encoding to the corresponding register name (atom)<br/>
% ---- Unit-Tested ----<br/>
-spec decode_inter_register_bits(Bits::<<_:4>>) -> atom().
%%%=======================================================================================
decode_inter_register_bits(Bits) ->

	<<Bits_Value:4>> = Bits,

	case Bits_Value of

			?INTER_REG_BITS_D       -> d;
			?INTER_REG_BITS_X       -> x;
			?INTER_REG_BITS_Y       -> y;
			?INTER_REG_BITS_U       -> u;
			?INTER_REG_BITS_S       -> s;
			?INTER_REG_BITS_PC      -> pc;
			?INTER_REG_BITS_W       -> w;
			?INTER_REG_BITS_V       -> v;
			?INTER_REG_BITS_A       -> a;
			?INTER_REG_BITS_B       -> b;
			?INTER_REG_BITS_CC      -> cc;
			?INTER_REG_BITS_DP      -> dp;
			?INTER_REG_BITS_ZERO_1  -> zero;
			?INTER_REG_BITS_ZERO_2  -> zero;
			?INTER_REG_BITS_E       -> e;
			?INTER_REG_BITS_F       -> f
		end.

%%%=======================================================================================
%% @doc 
% Decodes the Inter Register actual source value <br/>
% Based on the destination size and source register <br/>
% ---- Unit-Tested ----<br/>
-spec decode_inter_register_actual_source(Source_Reg::atom(),Destination_Size::non_neg_integer(),CPU::binary()) -> binary().
%%%=======================================================================================
decode_inter_register_actual_source(Source_Reg,Destination_Size,CPU) ->

	case [Destination_Size, Source_Reg] of

		%%-----------------------------------------------------------------------------
		%% [Destination: 8 bits, Source: any 16-bit register] -> lower 8 bits of 16 bit source
		%%-----------------------------------------------------------------------------

		[8, d] 		-> 	get_lower_8_reg_16(cpu_get_d(CPU));

		[8, w] 		-> 	get_lower_8_reg_16(cpu_get_w(CPU));

		[8, x] 		-> 	get_lower_8_reg_16(cpu_get_x(CPU));

		[8, y] 		-> 	get_lower_8_reg_16(cpu_get_y(CPU));

		[8, u] 		-> 	get_lower_8_reg_16(cpu_get_u(CPU));

		[8, s] 		-> 	get_lower_8_reg_16(cpu_get_s(CPU));

		[8, pc] 	-> 	get_lower_8_reg_16(cpu_get_pc(CPU));

		[8, v] 	    -> 	get_lower_8_reg_16(cpu_get_v(CPU));

		[8, zero] 	-> 	get_lower_8_reg_16(cpu_get_zero(CPU));

		%%-----------------------------------------------------------------------------
		%% [Destination: 16 bits, Source: A or B ] -> Accumulator D ]
		%%-----------------------------------------------------------------------------

		[16, a] 	-> cpu_get_d(CPU);

		[16, b] 	-> cpu_get_d(CPU);


		%%-----------------------------------------------------------------------------
		%% [Destination: 16 bits, Source: E or F ] -> Accumulator W ]
		%%-----------------------------------------------------------------------------

		[16, e] 	-> cpu_get_w(CPU);

		[16, f] 	-> cpu_get_w(CPU);

		%%-----------------------------------------------------------------------------
		%% [Destination: 16 bits, Source: CC] -> Zero in upper 8 bits; CC in lower 8 bits ]
		%%-----------------------------------------------------------------------------

		[16, cc] 	-> 	<<CC:8>> = cpu_get_cc(CPU),
						<<0:8,CC:8>>;

		%%-----------------------------------------------------------------------------
		%% [Destination: 16 bits, Source: DP] -> DP in upper 8 bits; Zero in the lower 8 bits ]
		%%-----------------------------------------------------------------------------

		[16, dp] 	-> 	<<DP:8>> = cpu_get_dp(CPU),
						<<DP:8,0:8>>;
						
		%%-----------------------------------------------------------------------------
		%% Everything Else
		%%-----------------------------------------------------------------------------

		[_, REG] 	-> case REG of

							%% Note: does not include Q or MD

							d 	  -> cpu_get_d(CPU);
							w 	  -> cpu_get_w(CPU);
							a 	  -> cpu_get_a(CPU);
							b 	  -> cpu_get_b(CPU);
							e 	  -> cpu_get_e(CPU);
							f 	  -> cpu_get_f(CPU);

							x 	  -> cpu_get_x(CPU);
							y 	  -> cpu_get_y(CPU);
							u 	  -> cpu_get_u(CPU);
							s 	  -> cpu_get_s(CPU);
							pc    -> cpu_get_pc(CPU);
							v 	  -> cpu_get_v(CPU);
							zero  -> cpu_get_zero(CPU);
							dp 	  -> cpu_get_dp(CPU);
							cc 	  -> cpu_get_cc(CPU)
						end
		end.


%%%=======================================================================================
%% @doc 
% Gets the Register based on name (atom)<br/>
% ---- Unit-Tested ----<br/>
-spec get_register_from_name(Reg_Name::atom(),CPU::binary()) -> binary().
%%%=======================================================================================
get_register_from_name(Reg_Name,CPU) ->

	case Reg_Name of

		q 	  -> cpu_get_q(CPU);
		d 	  -> cpu_get_d(CPU);
		w 	  -> cpu_get_w(CPU);
		a 	  -> cpu_get_a(CPU);
		b 	  -> cpu_get_b(CPU);
		e 	  -> cpu_get_e(CPU);
		f 	  -> cpu_get_f(CPU);

		x 	  -> cpu_get_x(CPU);
		y 	  -> cpu_get_y(CPU);
		u 	  -> cpu_get_u(CPU);
		s 	  -> cpu_get_s(CPU);
		pc    -> cpu_get_pc(CPU);
		v 	  -> cpu_get_v(CPU);
		zero  -> cpu_get_zero(CPU);
		dp 	  -> cpu_get_dp(CPU);
		cc 	  -> cpu_get_cc(CPU);
		md 	  -> cpu_get_md(CPU)
	end.


%%%=======================================================================================
%% @doc 
% Set the Register based on name (atom) with value<br/>
% ---- Unit-Tested ----<br/>
-spec set_register_from_name(Reg_Name::atom(),Value::binary(),CPU::binary()) -> binary().
%%%=======================================================================================
set_register_from_name(Reg_Name,Value,CPU) ->

	case Reg_Name of

		q 	  -> cpu_set_q(Value,CPU);
		d 	  -> cpu_set_d(Value,CPU);
		w 	  -> cpu_set_w(Value,CPU);
		a 	  -> cpu_set_a(Value,CPU);
		b 	  -> cpu_set_b(Value,CPU);
		e 	  -> cpu_set_e(Value,CPU);
		f 	  -> cpu_set_f(Value,CPU);

		x 	  -> cpu_set_x(Value,CPU);
		y 	  -> cpu_set_y(Value,CPU);
		u 	  -> cpu_set_u(Value,CPU);
		s 	  -> cpu_set_s(Value,CPU);
		pc    -> cpu_set_pc(Value,CPU);
		v 	  -> cpu_set_v(Value,CPU);
		zero  -> cpu_set_zero(Value,CPU);
		dp 	  -> cpu_set_dp(Value,CPU);
		cc 	  -> cpu_set_cc(Value,CPU);
		md 	  -> cpu_set_md(Value,CPU)
	end.

%%%=======================================================================================
%% @doc 
% Extracts the lower 8 bytes of a 16 bit register<br/>
% ---- Unit-Tested ----<br/>
-spec get_lower_8_reg_16(Reg_16_Binary::binary()) -> binary().
%%%=======================================================================================
get_lower_8_reg_16(Reg_16_Binary) ->

	<<_High:8,Low:8>> = Reg_16_Binary,
	<<Low:8>>.

%%%=======================================================================================
%% @doc 
% Get the bit size of a register (atom)<br/>
% ---- Unit-Tested ----<br/>
-spec get_size_of_register(Reg::atom()) -> non_neg_integer().
%%%=======================================================================================
get_size_of_register(Reg) ->

	case Reg of

		q    -> ?SIZE_Q;
		d    -> ?SIZE_D;
		w    -> ?SIZE_W;
		a    -> ?SIZE_A;
		b    -> ?SIZE_B;
		e    -> ?SIZE_E;
		f    -> ?SIZE_F;

		x    -> ?SIZE_X;
		y    -> ?SIZE_Y;
		u    -> ?SIZE_U;
		s    -> ?SIZE_S;
		pc   -> ?SIZE_PC;
		zero -> ?SIZE_ZERO;
		v 	 -> ?SIZE_V;
		dp 	 -> ?SIZE_DP;
		cc   -> ?SIZE_CC;
		md   -> ?SIZE_MD
	end.

%%%=======================================================================================
%% @doc 
% Get the flag map of the value for n and z flags<br/>
% ---- Unit-Tested ----<br/>
-spec generate_n_z_flags_map(Value::binary()) -> map().
%%%=======================================================================================
generate_n_z_flags_map(Value) -> 

	<<N_Value:1>> = get_n_flag_of_value(Value),
	<<Z_Value:1>> = get_z_flag_of_value(Value),

	#{
		n => N_Value,
		z => Z_Value
	}.

%%%=======================================================================================
%% @doc 
% Get the Negative Flag from a value<br/>
% ---- Unit-Tested ----<br/>
-spec get_n_flag_of_value(Value::binary()) -> <<_:1>>.
%%%=======================================================================================
get_n_flag_of_value(Value) ->

	<<N_Bit:1,_/bits>> = Value,
	<<N_Bit:1>>.

%%%=======================================================================================
%% @doc 
% Get the Zero Flag from a value<br/>
% ---- Unit-Tested ----<br/>
-spec get_z_flag_of_value(Value::binary()) -> <<_:1>>.
%%%=======================================================================================
get_z_flag_of_value(Value) ->

	Bit_Size 					= bit_size(Value),

	<<Actual_Value:Bit_Size>> 	= Value,

	case Actual_Value of

		0 -> <<1:1>>;
		_ -> <<0:1>>
	end.

%%-----------------------------------------------------------------------------
execution(Data,CPU_Data) ->

	PC 				= cpu_get_pc(CPU_Data),

	<<Op_Code:8>> 	= get_byte_pos(Data,PC),

	PC2 = case Op_Code of

				16#10 -> get_byte_pos(Data,pos_inc(PC));
				_ 	  -> <<0:8>>
			end,

	<<Op_Code2:8>> 	= PC2,

	{
		New_Data,
		New_CPU_Data
	} =	case [Op_Code,Op_Code2] of

			[16#3A,_] 	    -> 	abx_inherent_3a(PC, Data, CPU_Data);

			[16#89,_] 	    -> 	adca_immediate_89(PC, Data, CPU_Data);
			[16#99,_] 	    -> 	adca_direct_99(PC, Data, CPU_Data);
			[16#A9,_] 	    -> 	adca_indexed_a9(PC, Data, CPU_Data);
			[16#B9,_] 	    ->  adca_extended_b9(PC, Data, CPU_Data);

			[16#C9,_] 		-> 	adcb_immediate_c9(PC, Data, CPU_Data);
			[16#D9,_] 		-> 	adcb_direct_d9(PC, Data, CPU_Data);
			[16#E9,_] 		-> 	adcb_indexed_e9(PC, Data, CPU_Data);
			[16#F9,_] 		-> 	adcb_extended_f9(PC, Data, CPU_Data);

			[16#10, 16#89]	-> 	adcd_immediate_1089(PC, Data, CPU_Data);
			[16#10, 16#99]	-> 	adcd_direct_1099(PC, Data, CPU_Data);
			[16#10, 16#A9] 	-> 	adcd_indexed_10a9(PC, Data, CPU_Data);
			[16#10, 16#B9]	-> 	adcd_extended_10b9(PC, Data, CPU_Data);

			[16#10, 16#31]  -> 	adcr_immediate_1031(PC,Data,CPU_Data);

			[16#8B,_] 		->  adda_immediate_8b(PC,Data,CPU_Data);
			[16#C8,_] 		->  addb_immediate_cb(PC,Data,CPU_Data);
			[16#11,16#8B] 	->  adde_immediate_118b(PC,Data,CPU_Data);
			[16#11,16#CB] 	->  addf_immediate_11cb(PC,Data,CPU_Data);

			[16#9B,_] 		->  adda_direct_9b(PC,Data,CPU_Data);
			[16#DB,_] 		->  addb_direct_db(PC,Data,CPU_Data);
			[16#11,16#9b] 	->  adde_direct_119b(PC,Data,CPU_Data);
			[16#11,16#db] 	->  addf_direct_11db(PC,Data,CPU_Data);

			[16#AB,_] 		->  adda_indexed_ab(PC,Data,CPU_Data);
			[16#EB,_] 		->  addb_indexed_eb(PC,Data,CPU_Data);
			[16#11,16#AB] 	->  adde_indexed_11ab(PC,Data,CPU_Data);
			[16#11,16#EB] 	->  addf_indexed_11eb(PC,Data,CPU_Data);

			[16#BB,_] 		->  adda_extended_bb(PC,Data,CPU_Data);
			[16#FB,_] 		->  addb_extended_fb(PC,Data,CPU_Data);
			[16#11,16#BB]   ->  adde_extended_11bb(PC,Data,CPU_Data);
			[16#11,16#FB]   ->  addf_extended_11fb(PC,Data,CPU_Data);

			[16#C3,_] 		->  addd_immediate_c3(PC,Data,CPU_Data);
			[16#10,16#8B]   ->  addw_immediate_108b(PC,Data,CPU_Data);

			[16#D3,_] 		->  addd_direct_d3(PC,Data,CPU_Data);
			[16#10,16#9B] 	->  addw_direct_109b(PC,Data,CPU_Data);

			[16#E3,_] 		->  addd_indexed_e3(PC,Data,CPU_Data);
			[16#10,16#AB] 	->  addw_indexed_10ab(PC,Data,CPU_Data);

			[16#F3,_] 		->  addd_extended_f3(PC,Data,CPU_Data);
			[16#10,16#BB] 	->  addw_extended_10bb(PC,Data,CPU_Data);

			[16#10,16#30] 	->  addr_immediate_1030(PC,Data,CPU_Data);

			[16#02,_] 	    ->  aim_direct_02(PC,Data,CPU_Data);
			[16#62,_] 	    ->  aim_indexed_62(PC,Data,CPU_Data);
			[16#72,_] 	    ->  aim_extended_72(PC,Data,CPU_Data);

			[16#84,_] 	    ->  anda_immediate_84(PC,Data,CPU_Data);
			[16#C4,_] 	    ->  andb_immediate_c4(PC,Data,CPU_Data);

			[16#94,_] 	    ->  anda_direct_94(PC,Data,CPU_Data);
			[16#D4,_] 	    ->  andb_direct_d4(PC,Data,CPU_Data);

			[16#A4,_] 	    ->  anda_indexed_a4(PC,Data,CPU_Data);
			[16#E4,_] 	    ->  andb_indexed_e4(PC,Data,CPU_Data);

			[16#b4,_] 	    ->  anda_extended_b4(PC,Data,CPU_Data);
			[16#f4,_] 	    ->  andb_extended_f4(PC,Data,CPU_Data);

			[16#1c,_] 	    ->  andcc_immediate_1c(PC,Data,CPU_Data);

			[16#10,16#84] 	->  andd_immediate_1084(PC,Data,CPU_Data);
			[16#10,16#94] 	->  andd_direct_1094(PC,Data,CPU_Data);
			[16#10,16#A4] 	->  andd_indexed_10a4(PC,Data,CPU_Data);
			[16#10,16#B4] 	->  andd_extended_10b4(PC,Data,CPU_Data);

			[16#10,16#34] 	->  andr_immediate_1034(PC,Data,CPU_Data);

			[16#48,_] 	    ->  asla_inherent_48(PC,Data,CPU_Data);
			[16#58,_] 	    ->  aslb_inherent_58(PC,Data,CPU_Data);

			[16#08,_] 	    ->  asl_direct_08(PC,Data,CPU_Data);
			[16#68,_] 	    ->  asl_indexed_68(PC,Data,CPU_Data);
			[16#78,_] 	    ->  asl_extended_78(PC,Data,CPU_Data);

			[16#10,16#48] 	->  asld_inherent_1048(PC,Data,CPU_Data);

			[16#47,_] 	    ->  asra_inherent_47(PC,Data,CPU_Data);
			[16#57,_] 	    ->  asrb_inherent_57(PC,Data,CPU_Data);

			[16#07,_] 	    ->  asr_direct_07(PC,Data,CPU_Data);
			[16#67,_] 	    ->  asr_indexed_67(PC,Data,CPU_Data);
			[16#77,_] 	    ->  asr_extended_77(PC,Data,CPU_Data);

			[16#10,16#47] 	->  asrd_inherent_1047(PC,Data,CPU_Data);

			[16#11,16#30] 	->  band_direct_1130(PC,Data,CPU_Data);

			[16#24,_] 	    ->  bcc_relative_24(PC,Data,CPU_Data);

			[16#25,_] 	    ->  bcs_relative_25(PC,Data,CPU_Data); %% also blo_relative_25

			[16#11,16#34] 	->  beor_direct_1134(PC,Data,CPU_Data);

			[16#27,_] 	    ->  beq_relative_27(PC,Data,CPU_Data);
			[16#2c,_] 	    ->  bge_relative_2c(PC,Data,CPU_Data);
			[16#2e,_] 	    ->  bgt_relative_2e(PC,Data,CPU_Data);

			[16#22,_] 	    ->  bhi_relative_22(PC,Data,CPU_Data);
			%%[16#24,_] 	    ->  bhs_relative_24(PC,Data,CPU_Data); same as bcc_relative_24

			[16#11,16#31] 	->  biand_direct_1131(PC,Data,CPU_Data);

			[16#11,16#35] 	->  bieor_direct_1135(PC,Data,CPU_Data);
			[16#11,16#33] 	->  bior_direct_1133(PC,Data,CPU_Data);

			[16#85,_] 	    ->  bita_immediate_85(PC,Data,CPU_Data);
			[16#C5,_] 	    ->  bitb_immediate_c5(PC,Data,CPU_Data);

			[16#95,_] 	    ->  bita_direct_95(PC,Data,CPU_Data);
			[16#d5,_] 	    ->  bitb_direct_d5(PC,Data,CPU_Data);

			[16#A5,_] 	    ->  bita_indexed_a5(PC,Data,CPU_Data);
			[16#E5,_] 	    ->  bitb_indexed_e5(PC,Data,CPU_Data);

			[16#B5,_] 	    ->  bita_extended_b5(PC,Data,CPU_Data);
			[16#F5,_] 	    ->  bitb_extended_f5(PC,Data,CPU_Data);

			[16#10,16#85] 	->  bitd_immediate_1085(PC,Data,CPU_Data);
			[16#10,16#95] 	->  bitd_direct_1095(PC,Data,CPU_Data);
			[16#10,16#A5] 	->  bitd_indexed_10a5(PC,Data,CPU_Data);
			[16#10,16#B5] 	->  bitd_extended_10b5(PC,Data,CPU_Data);

			[16#11,16#3c] 	->  bitmd_immediate_113c(PC,Data,CPU_Data);

			[16#2f,_] 	    ->  ble_relative_2f(PC,Data,CPU_Data);

			%%[16#25,_] 		-> 	blo_relative_25(PC, Data, CPU_Data); also bcs_relative_25

			[16#23,_] 	    ->  bls_relative_23(PC,Data,CPU_Data);

			[16#2d,_] 	    ->  blt_relative_2d(PC,Data,CPU_Data);

			[16#2B,_] 	    ->  bmi_relative_2b(PC,Data,CPU_Data);

			[16#26,_] 	    ->  bne_relative_26(PC,Data,CPU_Data);

			[16#11,16#32] 	->  bor_direct_1132(PC,Data,CPU_Data);

			[16#2A,_] 	    ->  bpl_relative_2a(PC,Data,CPU_Data);

			[16#20,_] 	    ->  bra_relative_20(PC,Data,CPU_Data);

			[16#21,_] 	    ->  brn_relative_21(PC,Data,CPU_Data);

			[16#8d,_] 	    ->  bsr_relative_8d(PC,Data,CPU_Data);

			[16#28,_] 	    ->  bvc_relative_28(PC,Data,CPU_Data);

			[16#29,_] 	    ->  bvs_relative_29(PC,Data,CPU_Data);

			[16#4f,_] 	    ->  clra_inherent_4f(PC,Data,CPU_Data);
			[16#5d,_] 	    ->  clrb_inherent_5f(PC,Data,CPU_Data);
			[16#10,16#4F] 	->  clrd_inherent_104f(PC,Data,CPU_Data);
			[16#11,16#4F] 	->  clre_inherent_114f(PC,Data,CPU_Data);
			[16#11,16#5F] 	->  clrf_inherent_115f(PC,Data,CPU_Data);
			[16#10,16#5F] 	->  clrw_inherent_105f(PC,Data,CPU_Data);

			[16#0F,_] 	    ->  clr_direct_0f(PC,Data,CPU_Data);
			[16#6F,_] 	    ->  clr_indexed_6f(PC,Data,CPU_Data);
			[16#7F,_] 	    ->  clr_extended_7f(PC,Data,CPU_Data);

			[16#81,_] 	    ->  cmpa_immediate_81(PC,Data,CPU_Data);
			[16#C1,_] 	    ->  cmpb_immediate_c1(PC,Data,CPU_Data);
			[16#11,16#81] 	->  cmpe_immediate_1181(PC,Data,CPU_Data);
			[16#11,16#C1] 	->  cmpf_immediate_11c1(PC,Data,CPU_Data);

			[16#91,_] 	    ->  cmpa_direct_91(PC,Data,CPU_Data);
			[16#D1,_] 	    ->  cmpb_direct_d1(PC,Data,CPU_Data);
			[16#11,16#91] 	->  cmpe_direct_1191(PC,Data,CPU_Data);
			[16#11,16#D1] 	->  cmpf_direct_11d1(PC,Data,CPU_Data);

			[16#A1,_] 	    ->  cmpa_indexed_a1(PC,Data,CPU_Data);
			[16#E1,_] 	    ->  cmpb_indexed_e1(PC,Data,CPU_Data);
			[16#11,16#A1] 	->  cmpe_indexed_11a1(PC,Data,CPU_Data);
			[16#11,16#E1] 	->  cmpf_indexed_11e1(PC,Data,CPU_Data);

			[16#B1,_] 	    ->  cmpa_extended_b1(PC,Data,CPU_Data);
			[16#F1,_] 	    ->  cmpb_extended_f1(PC,Data,CPU_Data);
			[16#11,16#B1] 	->  cmpe_extended_11b1(PC,Data,CPU_Data);
			[16#11,16#F1] 	->  cmpf_extended_11f1(PC,Data,CPU_Data);

			[16#00,16#83] 	->  cmpd_immediate_0083(PC,Data,CPU_Data);
			[16#01,16#8C] 	->  cmps_immediate_018C(PC,Data,CPU_Data);
			[16#01,16#83] 	->  cmpu_immediate_0183(PC,Data,CPU_Data);
			[16#00,16#81] 	->  cmpw_immediate_0081(PC,Data,CPU_Data);
			[16#8C,_] 	    ->  cmpx_immediate_8c(PC,Data,CPU_Data);
			[16#00,16#8C] 	->  cmpy_immediate_008c(PC,Data,CPU_Data);

			[16#00,16#93] 	->  cmpd_direct_0093(PC,Data,CPU_Data);
			[16#01,16#9C] 	->  cmps_direct_019C(PC,Data,CPU_Data);
			[16#01,16#93] 	->  cmpu_direct_0193(PC,Data,CPU_Data);
			[16#00,16#91] 	->  cmpw_direct_0091(PC,Data,CPU_Data);
			[16#9C,_] 	    ->  cmpx_direct_9c(PC,Data,CPU_Data);
			[16#00,16#9C] 	->  cmpy_direct_009c(PC,Data,CPU_Data);

			[16#00,16#A3] 	->  cmpd_indexed_00a3(PC,Data,CPU_Data);
			[16#01,16#AC] 	->  cmps_indexed_01ac(PC,Data,CPU_Data);
			[16#01,16#A3] 	->  cmpu_indexed_01a3(PC,Data,CPU_Data);
			[16#00,16#A1] 	->  cmpw_indexed_00a1(PC,Data,CPU_Data);
			[16#AC,_] 	    ->  cmpx_indexed_ac(PC,Data,CPU_Data);
			[16#00,16#AC] 	->  cmpy_indexed_00ac(PC,Data,CPU_Data);

			[16#00,16#B3] 	->  cmpd_extended_00b3(PC,Data,CPU_Data);
			[16#01,16#BC] 	->  cmps_extended_01bc(PC,Data,CPU_Data);
			[16#01,16#B3] 	->  cmpu_extended_01b3(PC,Data,CPU_Data);
			[16#00,16#B1] 	->  cmpw_extended_00b1(PC,Data,CPU_Data);
			[16#BC,_] 	    ->  cmpx_extended_bc(PC,Data,CPU_Data);
			[16#00,16#BC] 	->  cmpy_extended_00bc(PC,Data,CPU_Data);

			[16#10,16#37] 	->  cmpr_immediate_1037(PC,Data,CPU_Data);

			[16#43,_] 		-> 	coma_inherent_43(PC, Data, CPU_Data);
			[16#53,_] 		-> 	comb_inherent_53(PC, Data, CPU_Data);
			[16#10,16#43] 	-> 	comd_inherent_1043(PC, Data, CPU_Data);
			[16#11,16#43] 	->  come_inherent_1143(PC, Data, CPU_Data);
			[16#11,16#53] 	-> 	comf_inherent_1153(PC, Data, CPU_Data);
			[16#10,16#53] 	-> 	comw_inherent_1053(PC, Data, CPU_Data);

			[16#03,_] 		-> 	com_memory_direct_03(PC, Data, CPU_Data);
			[16#63,_] 		-> 	com_memory_indexed_63(PC, Data, CPU_Data);
			[16#73,_] 		-> 	com_memory_extended_73(PC, Data, CPU_Data);

			[ _, _ ]        ->	ok

			end,

	%%-------------------------------------------------------------------------
	%% Check Halt Status
	%%-------------------------------------------------------------------------

	Halt_Status 	= cpu_get_halt(New_CPU_Data),

	case Halt_Status of

		<<1:1>> 	-> ok;
		<<0:1>> 	-> execution(New_Data,CPU_Data)
	end.

%%%=======================================================================================
%% @doc 
% ABX Instruction<br/>
% Add Accumulator B to Index Register X<br/>
% X' = X + ACCB<br/>
% The ABX instruction performs an unsigned addition of the contents of Accumulator B<br/>
% with the contents of Index Register X. The 16-bit result is placed into Index Register<br/>
% X. None of the Condition Code flags are affected<br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec abx_inherent_3a(
						Pos 		::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					 )  -> {binary(), cpu_type()}.
%%%=======================================================================================
abx_inherent_3a(Pos, Data, CPU_Data) 
    when
    		is_binary(Pos),
    		is_binary(Data),
    		is_binary(CPU_Data)
    ->

	B 						= cpu_get_b(CPU_Data),
	X 						= cpu_get_x(CPU_Data),

	B_Expanded 				= binary_logic:expand_to_size_with_zeros(B,16),

	{_Carry,Result,_Flags} 	= binary_logic:generic_addition_with_flags(X,B_Expanded),

	X_CPU 					= cpu_set_x(Result,CPU_Data),

	PC_CPU 					= cpu_set_pc(pos_inc(Pos),X_CPU),

	{Data, PC_CPU}.

%%%=======================================================================================
%% @doc 
% ADC(A) <br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec adca_immediate_89(
							Pos 		::position_type(),
							Data 		::binary(),
							CPU_Data 	::cpu_type()
						) -> {binary(), cpu_type()}.
%%%=======================================================================================
adca_immediate_89(Pos, Data, CPU_Data) ->

	{Memory_Data, New_CPU_Data, New_Pos} 	= immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	adc_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_a/1,
						fun cpu_set_a/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADC(A) <br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec adca_direct_99(
						Pos 		::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					) -> {binary(), cpu_type()}.
%%%=======================================================================================
adca_direct_99(Pos, Data, CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= direct_1_address(pos_inc(Pos),Data,CPU_Data),

	adc_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_a/1,
						fun cpu_set_a/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADC(A) <br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec adca_indexed_a9(
						Pos 		::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					) -> {binary(), cpu_type()}.
%%%=======================================================================================
adca_indexed_a9(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos}  = indexed_1_address(pos_inc(Pos),Data,CPU_Data),

	adc_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_a/1,
						fun cpu_set_a/2,
						New_Pos
					).
	
%%%=======================================================================================
%% @doc 
% ADC(A) <br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec adca_extended_b9(
						Pos 		::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					  ) -> {binary(), cpu_type()}.
%%%=======================================================================================
adca_extended_b9(Pos,Data,CPU_Data) ->

	{Memory_Data, New_CPU_Data, New_Pos}  = indexed_1_address(pos_inc(Pos),Data,CPU_Data),

	adc_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_a/1,
						fun cpu_set_a/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADC(B) <br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec adcb_immediate_c9(
							Pos 		::position_type(),
							Data 		::binary(),
							CPU_Data 	::cpu_type()
						) -> {binary(), cpu_type()}.
%%%=======================================================================================
adcb_immediate_c9(Pos,Data,CPU_Data) ->

	{Memory_Data, New_CPU_Data, New_Pos} 	= immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	adc_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_b/1,
						fun cpu_set_b/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADC(B) <br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec adcb_direct_d9(
						Pos 		::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					) -> {binary(), cpu_type()}.
%%%=======================================================================================
adcb_direct_d9(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= direct_1_address(pos_inc(Pos),Data,CPU_Data),

	adc_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_b/1,
						fun cpu_set_b/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADC(B) <br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec adcb_indexed_e9(
						Pos 		::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					 ) -> {binary(), cpu_type()}.
%%%=======================================================================================
adcb_indexed_e9(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos}  = indexed_1_address(pos_inc(Pos),Data,CPU_Data),

	adc_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_b/1,
						fun cpu_set_b/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADC(B) <br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec adcb_extended_f9(
						Pos 		::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					  ) -> {binary(), cpu_type()}.
%%%=======================================================================================
adcb_extended_f9(Pos,Data,CPU_Data) ->

	{Memory_Data, New_CPU_Data, New_Pos}  = indexed_1_address(pos_inc(Pos),Data,CPU_Data),

	adc_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_b/1,
						fun cpu_set_b/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADC Instruction (8 Bit)<br/>
% Add Memory Byte plus Carry with Accumulator A or B<br/>
% r' = r + (M) + C<br/>
% These instructions add the contents of a byte in memory plus the contents of the Carry<br/>
% flag with either Accumulator A or B. The 8-bit result is placed back into the specified<br/>
% accumulator.<br/>
% <br/>
% H The Half-Carry flag is set if a carry into bit 4 occurred; cleared otherwise.<br/>
% N The Negative flag is set equal to the new value of bit 7 of the accumulator<br/>
% Z The Zero flag is set if the new accumulator value is zero; cleared otherwise.<br/>
% V The Overflow flag is set if an overflow occurred; cleared otherwise.<br/>
% C The Carry flag is set if a carry out of bit 7 occured; cleared otherwise.<br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec adc_generic_part(
							_Pos 			::position_type(),
							Data 			::binary(),
							New_CPU_Data 	::cpu_type(),
							Memory_Data 	::binary(),
							Get_Reg_Fun 	::function(),
							Set_Reg_Fun 	::function(),
							New_Pos 		::binary()
						)  -> {binary(), cpu_type()}.
%%%=======================================================================================
adc_generic_part(
					_Pos,
					Data,
					New_CPU_Data,
					Memory_Data,
					Get_Reg_Fun,
					Set_Reg_Fun,
					New_Pos
				) ->

	%%-------------------------------------------------------------------------
	%% Add the contents of a byte in memory plus the contents of the carry flag
	%% with either Accumulator A or B. The 8-bit result is placed back into the
	%% specified accumulator
	%%-------------------------------------------------------------------------

	Carry 							= cpu_get_cc_c(New_CPU_Data),

	Reg 							= Get_Reg_Fun(New_CPU_Data),

	{_Carry_Out, Result, Flags} 	= binary_logic:byte_addition_with_flags(Reg, Memory_Data ,Carry),

	Final_CPU 						= cpu_perform_actions(
														 	[
														 		fun(CPU) -> Set_Reg_Fun(Result,CPU) end,
														 		?UPDATE_FLAGS(Flags),
														 		?SET_PC(New_Pos)
														 	],
														 	New_CPU_Data
														 ),

	{Data, Final_CPU}.


%%%=======================================================================================
%% @doc 
% ADCD (6309 ONLY)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec adcd_immediate_1089(
							Pos 		::position_type(),
							Data 		::binary(),
							CPU_Data 	::cpu_type()
						 ) -> {binary(), cpu_type()}.
%%%=======================================================================================
adcd_immediate_1089(Pos,Data,CPU_Data) ->

	{Memory_Data, New_CPU_Data, New_Pos} = immediate_2_address(pos_inc2(Pos),Data,CPU_Data),

	adcd_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						New_Pos
					 ).

%%%=======================================================================================
%% @doc 
% ADCD (6309 ONLY)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec adcd_direct_1099(
						Pos 	 	::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					  ) -> {binary(), cpu_type()}.
%%%=======================================================================================
adcd_direct_1099(Pos, Data, CPU_Data) ->

	{Memory_Data, New_CPU_Data, New_Pos} = direct_2_address(pos_inc2(Pos),Data,CPU_Data),

	adcd_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						New_Pos
					 ).

%%%=======================================================================================
%% @doc 
% ADCD (6309 ONLY)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec adcd_indexed_10a9(
							Pos 		::position_type(),
							Data 		::binary(),
							CPU_Data 	::cpu_type()
						) -> {binary(), cpu_type()}.
%%%=======================================================================================
adcd_indexed_10a9(Pos, Data, CPU_Data) ->

	{Memory_Data, New_CPU_Data, New_Pos}  = indexed_2_address(pos_inc2(Pos),Data,CPU_Data),

	adcd_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						New_Pos
					 ).

%%%=======================================================================================
%% @doc 
% ADCD (6309 ONLY)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec adcd_extended_10b9(
							Pos 		::position_type(),
							Data 		::binary(),
							CPU_Data 	::cpu_type()
						) -> {binary(), cpu_type()}.
%%%=======================================================================================
adcd_extended_10b9(Pos, Data, CPU_Data) ->

	{Memory_Data, New_CPU_Data, New_Pos}  = indexed_2_address(pos_inc2(Pos),Data,CPU_Data),

	adcd_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						New_Pos
					 ).

%%----Unit_Tested-------------------------------------------------------------------------
adcd_generic_part(
					_Pos,
					Data,
					New_CPU_Data,
					Memory_Data,
					New_Pos
				 ) ->

	%%-------------------------------------------------------------------------
	%% Adds the contents of a double-byte value in memory plus the value of the
	%% carry flag with Accumulator D. The 16 bit result is placed back into 
	%% Accumulator D
	%%-------------------------------------------------------------------------

	Carry 							= cpu_get_cc_c(New_CPU_Data),

	D 								= cpu_get_d(New_CPU_Data),

	{_Carry_Out, Result, Flags} 	= binary_logic:generic_addition_with_flags(D, Memory_Data ,Carry),

	Final_CPU 						= cpu_perform_actions(
															[
																?SET_D(Result),
																?UPDATE_FLAGS(Flags),
																?SET_PC(New_Pos)
															],
															New_CPU_Data
														 ),

	{Data, Final_CPU}.

%%%=======================================================================================
%% @doc 
% ADCR (6309 ONLY)<br/>
% Add Source Register plus Carry to Destination Register<br/>
% r1' = r1 + r0 + c<br/>
% The ADCR instruction adds the contents of a source register plus the contents of the <br/>
% Carry flag with the contents of a destination register. The result is placed into the <br/>
% destination register<br/>
% <br/>
% H The Half-Carry flag is not affected by the ADCR instruction<br/>
% N The Negative flag is set equal to the value of the result's high-order bit<br/>
% Z The Zero flag is set if the new value of the destination register is zero; cleared otherwise<br/>
% V The Overflow flag is set if an overflow occurred; cleared otherwise.<br/>
% C The Carry flag is set if a carry out of the high-order bit occured; cleared otherwise.<br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec adcr_immediate_1031(
							Pos 		::position_type(),
							Data 		::binary(),
							CPU_Data 	::cpu_type()
						) -> {binary(), cpu_type()}.
%%%=======================================================================================
adcr_immediate_1031(Pos,Data,CPU_Data) ->

	{Memory_Data, New_CPU_Data, New_Pos} 	= immediate_1_address(pos_inc2(Pos),Data,CPU_Data),

	Carry 									= cpu_get_cc_c(New_CPU_Data),

	{Source_Reg, Destination_Reg} 			= decode_inter_register_post_byte(Memory_Data),

	Destination_Size 						= get_size_of_register(Destination_Reg),

	Source_Value 							= decode_inter_register_actual_source(Source_Reg,Destination_Size,New_CPU_Data),

	Destination_Value 						= get_register_from_name(Destination_Reg,New_CPU_Data),

	{_Carry, Result, Flags} 				= binary_logic:generic_addition_with_flags(Source_Value,Destination_Value,Carry),

	New_CPU_Data_2 							= set_register_from_name(Destination_Reg,Result,New_CPU_Data),

	Final_CPU 								= cpu_perform_actions(
																	[
																		?UPDATE_FLAGS(Flags),
																		?SET_PC(New_Pos)
																	],
																	New_CPU_Data_2
																 ),

	{Data, Final_CPU}.

%%%=======================================================================================
%% @doc 
% ADD Generic Part (8 and 16 Bit)<br/>
% ----------------------------------------------------------------------------------------<br/>
% ADD Instruction (8 Bit)<br/>
% r' = r + (M) <br/>
% <br/>
% These instructions add the content of a byte in memory with one of the 8-bit <br/>
% accumulators (A,B,E,F). The 8-bit result is placed back into the specified accumulator<br/>
% <br/>
% H The Half-Carry flag is set if a carry into bit 4 occurred; cleared otherwise.<br/>
% N The Negative flag is set equal to the new value of bit 7 of the accumulator<br/>
% Z The Zero flag is set if the new accumulator value is zero; cleared otherwise.<br/>
% V The Overflow flag is set if an overflow occurred; cleared otherwise.<br/>
% C The Carry flag is set if a carry out of bit 7 occured; cleared otherwise.<br/>
% <br/>
% The 8-bit ADD instructions are used for single-byte addition, and for addition of the <br/>
% least-significant byte in multi-byte additions. Since the 6x09 also provides a 16-bit ADD<br/>
% instructions, it is not necessary to use the 8-bit ADD and ADC instructions for performing<br/>
% 16-bit addition.<br/>
% <br/>
% ----------------------------------------------------------------------------------------<br/>
% ADD (16 Bit)<br/>
% Add Memory Word to 16-Bit Accumulator<br/>
% r' = r + (M:M+1) <br/>
% These instructions add the content of a double-byte value in memory with one of the 16-bit <br/>
% accumulators (D,W). The 16-bit result is placed back into the specified accumulator.<br/>
% <br/>
% H The Half-Carry flag is not affected by these instructions.<br/>
% N The Negative flag is set equal to the new value of bit 15 of the accumulator.<br/>
% Z The Zero flag is set if the new accumulator value is zero; cleared otherwise.<br/>
% V The Overflow flag is set if an overflow occured; cleared otherwise.<br/>
% C The Carry flag is set if a carry out of bit 15 occured; cleared otherwise.<br/>
% <br/>
% The 16-bit ADD instructions are used for double-byte addition, and for addition of the<br/>
% least-significant word of multi-byte additions. See the description of the ADCD<br/>
% instruction for an example of how 32-bit addition can be performed on a 6309 processor.<br/>
% <br/>
% ---- Unit-Tested ----<br/>
%%%=======================================================================================
-spec add_generic_part(
							_Pos 			::position_type(),
							Data 			::binary(),
							New_CPU_Data 	::cpu_type(),
							Memory_Data 	::binary(),
							Get_Reg_Fun 	::function(),
							Set_Reg_Fun 	::function(),
							New_Pos 		::position_type()
						)  -> {binary(), cpu_type()}.
%%%=======================================================================================
add_generic_part(
					_Pos,
					Data,
					New_CPU_Data,
					Memory_Data,
					Get_Reg_Fun,
					Set_Reg_Fun,
					New_Pos
				) ->

	Reg 							= Get_Reg_Fun(New_CPU_Data),

	Reg_Size 						= bit_size(Reg),

	{_Carry_Out, Result, Flags} 	=	case Reg_Size of

										 8 -> binary_logic:byte_addition_with_flags(Reg, Memory_Data);
										 _ -> binary_logic:generic_addition_with_flags(Reg,Memory_Data)

										end,

	Final_CPU 						= cpu_perform_actions(
														 	[
														 		fun(CPU) -> Set_Reg_Fun(Result,CPU) end,
														 		?UPDATE_FLAGS(Flags),
														 		?SET_PC(New_Pos)
														 	],
														 	New_CPU_Data
														 ),

	{Data, Final_CPU}.

%%%=======================================================================================
%% @doc 
% ADD (8-bit)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec adda_immediate_8b(
							Pos 		::position_type(),
							Data 		::binary(),
							CPU_Data 	::cpu_type()
						) -> {binary(), cpu_type()}.
%%%=======================================================================================
adda_immediate_8b(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	add_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_a/1,
						fun cpu_set_a/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADD (8-bit)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec addb_immediate_cb(
							Pos 		::position_type(),
							Data 		::binary(),
							CPU_Data 	::cpu_type()
						) -> {binary(), cpu_type()}.
%%%=======================================================================================
addb_immediate_cb(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	add_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_b/1,
						fun cpu_set_b/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADD (8-bit)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec adde_immediate_118b(
							Pos 		::position_type(),
							Data 		::binary(),
							CPU_Data 	::cpu_type()
						) -> {binary(), cpu_type()}.
%%%=======================================================================================
adde_immediate_118b(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = immediate_1_address(pos_inc2(Pos),Data,CPU_Data),

	add_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_e/1,
						fun cpu_set_e/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADD (8-bit)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec addf_immediate_11cb(
							Pos 		::position_type(),
							Data 		::binary(),
							CPU_Data 	::cpu_type()
						) -> {binary(), cpu_type()}.
%%%=======================================================================================
addf_immediate_11cb(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = immediate_1_address(pos_inc2(Pos),Data,CPU_Data),

	add_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_f/1,
						fun cpu_set_f/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADD (8-bit)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec adda_direct_9b(
						Pos 		::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					) -> {binary(), cpu_type()}.
%%%=======================================================================================
adda_direct_9b(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = direct_1_address(pos_inc(Pos),Data,CPU_Data),

	add_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_a/1,
						fun cpu_set_a/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADD (8-bit)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec addb_direct_db(
						Pos 		::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					) -> {binary(), cpu_type()}.
%%%=======================================================================================
addb_direct_db(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = direct_1_address(pos_inc(Pos),Data,CPU_Data),

	add_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_b/1,
						fun cpu_set_b/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADD (8-bit)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec adde_direct_119b(
						Pos 		::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					) -> {binary(), cpu_type()}.
%%%=======================================================================================
adde_direct_119b(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = direct_1_address(pos_inc2(Pos),Data,CPU_Data),

	add_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_e/1,
						fun cpu_set_e/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADD (8-bit)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec addf_direct_11db(
						Pos 	 	::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					 ) -> {binary(), cpu_type()}.
%%%=======================================================================================
addf_direct_11db(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = direct_1_address(pos_inc2(Pos),Data,CPU_Data),

	add_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_f/1,
						fun cpu_set_f/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADD (8-bit)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec adda_indexed_ab(
						Pos 		::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					) -> {binary(), cpu_type()}.
%%%=======================================================================================
adda_indexed_ab(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos}  = indexed_1_address(pos_inc(Pos),Data,CPU_Data),

	add_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_a/1,
						fun cpu_set_a/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADD (8-bit)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec addb_indexed_eb(
						Pos 		::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					) -> {binary(), cpu_type()}.
%%%=======================================================================================
addb_indexed_eb(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos}  = indexed_1_address(pos_inc(Pos),Data,CPU_Data),

	add_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_b/1,
						fun cpu_set_b/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADD (8-bit)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec adde_indexed_11ab(
							Pos 		::position_type(),
							Data 		::binary(),
							CPU_Data 	::cpu_type()
						) -> {binary(), cpu_type()}.
%%%=======================================================================================
adde_indexed_11ab(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos}  = indexed_1_address(pos_inc2(Pos),Data,CPU_Data),

	add_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_e/1,
						fun cpu_set_e/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADD (8-bit)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec addf_indexed_11eb(
							Pos 		::position_type(),
							Data 		::binary(),
							CPU_Data 	::cpu_type()
						) -> {binary(), cpu_type()}.
%%%=======================================================================================
addf_indexed_11eb(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos}  = indexed_1_address(pos_inc2(Pos),Data,CPU_Data),

	add_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_f/1,
						fun cpu_set_f/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADD (8-bit)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec adda_extended_bb(
						Pos 		::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					) -> {binary(), cpu_type()}.
%%%=======================================================================================
adda_extended_bb(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos}  = indexed_1_address(pos_inc(Pos),Data,CPU_Data),

	add_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_a/1,
						fun cpu_set_a/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADD (8-bit)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec addb_extended_fb(
						Pos 		::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					  ) -> {binary(), cpu_type()}.
%%%=======================================================================================
addb_extended_fb(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos}  = indexed_1_address(pos_inc(Pos),Data,CPU_Data),

	add_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_b/1,
						fun cpu_set_b/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADD (8-bit)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec adde_extended_11bb(
							Pos 		::position_type(),
							Data 		::binary(),
							CPU_Data 	::cpu_type()
						) -> {binary(), cpu_type()}.
%%%=======================================================================================
adde_extended_11bb(Pos,Data,CPU_Data) -> 
	
	{Memory_Data, New_CPU_Data, New_Pos}  = indexed_1_address(pos_inc2(Pos),Data,CPU_Data),

	add_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_e/1,
						fun cpu_set_e/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADD (8-bit)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec addf_extended_11fb(
							Pos 		::position_type(),
							Data 		::binary(),
							CPU_Data 	::cpu_type()
						) -> {binary(), cpu_type()}.
%%%=======================================================================================
addf_extended_11fb(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos}  = indexed_1_address(pos_inc2(Pos),Data,CPU_Data),

	add_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_f/1,
						fun cpu_set_f/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADD (16-bit)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec addd_immediate_c3(
							Pos 		::position_type(),
							Data 		::binary(),
							CPU_Data 	::cpu_type()
						) -> {binary(), cpu_type()}.
%%%=======================================================================================
addd_immediate_c3(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = immediate_2_address(pos_inc(Pos),Data,CPU_Data),

	add_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_d/1,
						fun cpu_set_d/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADD (16-bit)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec addw_immediate_108b(
							Pos 		::position_type(),
							Data 		::binary(),
							CPU_Data 	::cpu_type()
						) -> {binary(), cpu_type()}.
%%%=======================================================================================
addw_immediate_108b(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = immediate_2_address(pos_inc2(Pos),Data,CPU_Data),

	add_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_w/1,
						fun cpu_set_w/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADD (16-bit)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec addd_direct_d3(
						Pos 		::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					) -> {binary(), cpu_type()}.
%%%=======================================================================================
addd_direct_d3(Pos,Data,CPU_Data) -> 
	
	{Memory_Data, New_CPU_Data, New_Pos} = direct_2_address(pos_inc(Pos),Data,CPU_Data),

	add_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_d/1,
						fun cpu_set_d/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADD (16-bit)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec addw_direct_109b(
						Pos 		::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					  ) -> {binary(), cpu_type()}.
%%%=======================================================================================
addw_direct_109b(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = direct_2_address(pos_inc2(Pos),Data,CPU_Data),

	add_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_w/1,
						fun cpu_set_w/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADD (16-bit)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec addd_indexed_e3(
						Pos 		::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					) -> {binary(), cpu_type()}.
%%%=======================================================================================
addd_indexed_e3(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos}  = indexed_2_address(pos_inc(Pos),Data,CPU_Data),

	add_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_d/1,
						fun cpu_set_d/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADD (16-bit)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec addw_indexed_10ab(
							Pos 		::position_type(),
							Data 		::binary(),
							CPU_Data 	::cpu_type()
						) -> {binary(), cpu_type()}.
%%%=======================================================================================
addw_indexed_10ab(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos}  = indexed_2_address(pos_inc2(Pos),Data,CPU_Data),

	add_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_w/1,
						fun cpu_set_w/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADD (16-bit)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec addd_extended_f3(
						Pos 		::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					 ) -> {binary(), cpu_type()}.
%%%=======================================================================================
addd_extended_f3(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos}  = indexed_2_address(pos_inc(Pos),Data,CPU_Data),

	add_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_d/1,
						fun cpu_set_d/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADD (16-bit)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec addw_extended_10bb(
							Pos 		::position_type(),
							Data 		::binary(),
							CPU_Data 	::cpu_type()
						) -> {binary(), cpu_type()}.
%%%=======================================================================================
addw_extended_10bb(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos}  = indexed_2_address(pos_inc2(Pos),Data,CPU_Data),

	add_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_w/1,
						fun cpu_set_w/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ADDR (6309 ONLY)<br/>
% Add Source Register to Destination Register<br/>
% r1' = r1 + r0 <br/>
% The ADDR instruction adds the contents of a source register plus the contents of the <br/>
% destination register. The result is placed into the destination register<br/>
% <br/>
% H The Half-Carry flag is not affected by the ADDR instruction<br/>
% N The Negative flag is set equal to the value of the result's high-order bit<br/>
% Z The Zero flag is set if the new value of the destination register is zero; cleared otherwise<br/>
% V The Overflow flag is set if an overflow occurred; cleared otherwise.<br/>
% C The Carry flag is set if a carry out of the high-order bit occured; cleared otherwise.<br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec addr_immediate_1030(
							Pos 		::position_type(),
							Data 		::binary(),
							CPU_Data 	::cpu_type()
						) -> {binary(), cpu_type()}.
%%%=======================================================================================
addr_immediate_1030(Pos,Data,CPU_Data) ->

	{Memory_Data, New_CPU_Data, New_Pos} 	= immediate_1_address(pos_inc2(Pos),Data,CPU_Data),

	{Source_Reg, Destination_Reg} 			= decode_inter_register_post_byte(Memory_Data),

	Destination_Size 						= get_size_of_register(Destination_Reg),

	Source_Value 							= decode_inter_register_actual_source(Source_Reg,Destination_Size,New_CPU_Data),

	Destination_Value 						= get_register_from_name(Destination_Reg,New_CPU_Data),

	{_Carry, Result, Flags} 				= binary_logic:generic_addition_with_flags(Source_Value,Destination_Value),

	New_CPU_Data_2 							= set_register_from_name(Destination_Reg,Result,New_CPU_Data),

	Final_CPU 								= cpu_perform_actions(
																	[
																		?UPDATE_FLAGS(Flags),
																		?SET_PC(New_Pos)
																	],
																	New_CPU_Data_2
																 ),

	{Data, Final_CPU}.

%%%=======================================================================================
%% @doc 
% AIM (6309 Only)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec aim_direct_02(
						Pos 		::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					) -> {binary(), cpu_type()}.
%%%=======================================================================================
aim_direct_02(Pos,Data,CPU_Data) -> 
	
	{Memory_Data, New_CPU_Data, New_Pos} 	= direct_1_address(pos_inc(Pos),Data,CPU_Data),

	{Designated_Memory_Location,_,_} 		= direct_1_address_address_only(pos_inc(Pos),Data,CPU_Data),

	Immediate_Byte 							= get_byte_pos(Data, New_Pos),

	aim_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						Immediate_Byte,
						Designated_Memory_Location,
						pos_inc(New_Pos) 				% to allow for the immediate byte
					).

%%%=======================================================================================
%% @doc 
% AIM (6309 Only)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec aim_indexed_62(
						Pos 		::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					) -> {binary(), cpu_type()}.
%%%=======================================================================================
aim_indexed_62(Pos, Data, CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= indexed_1_address(pos_inc(Pos),Data,CPU_Data),

	{Designated_Memory_Location,_,_} 		= indexed_1_address_address_only(pos_inc(Pos),Data,CPU_Data),

	Immediate_Byte 							= get_byte_pos(Data, New_Pos),

	aim_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						Immediate_Byte,
						Designated_Memory_Location,
						pos_inc(New_Pos) 				% to allow for the immediate byte
					).

%%%=======================================================================================
%% @doc 
% AIM (6309 Only)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec aim_extended_72(
						Pos 		::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					 ) -> {binary(), cpu_type()}.
%%%=======================================================================================
aim_extended_72(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= indexed_1_address(pos_inc(Pos),Data,CPU_Data),

	{Designated_Memory_Location,_,_} 		= indexed_1_address_address_only(pos_inc(Pos),Data,CPU_Data),

	Immediate_Byte 							= get_byte_pos(Data, New_Pos),

	aim_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						Immediate_Byte,
						Designated_Memory_Location,
						pos_inc(New_Pos) 				% to allow for the immediate byte
					).

%%%=======================================================================================
%% @doc 
% AIM (6309 Only)<br/>
% Logical AND of Immediate Value with Memory Byte <br/>
% M' = (M) AND IMM <br/>
% The AIM instruction logically ANDs the contents of a byte in memory with an 8-bit <br/>
% immediate value, The resulting value is placed back into the designated memory location. <br/>
% <br/>
%  N The Negative flag is set equal to the new value of bit 7 of the memory byte. <br/>
%  Z The Zero flag is set if the new value of the memory byte is zero; cleared otherwise. <br/>
%  V The Overflow flag is cleared by this instruction. <br/>
%  C The Carry flag is not affected by this instruction. <br/>
% <br/>
% AIM  is one of the more useful additions to the 6309 instruction set. It takes three separate <br/>
% instructions to perform the same operation on a 6809. <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec aim_generic_part(
						_Pos 						::position_type(),
						Data 						::binary(),
						New_CPU_Data 				::cpu_type(),
						Memory_Data 				::binary(),
						Immediate_Byte 				::binary(),
						Designated_Memory_Location 	::binary(),
						New_Pos 					::position_type()
					 ) -> { binary(), cpu_type() }.
%%%=======================================================================================
aim_generic_part(
					_Pos,
					Data,
					New_CPU_Data,
					Memory_Data,
					Immediate_Byte,
					Designated_Memory_Location,
					New_Pos
				) ->

	AND_Result 		= binary_logic:and_binary(Memory_Data, Immediate_Byte),

	New_Data 		= ram_64k:set_byte_pos(
											Data,
											AND_Result,
											Designated_Memory_Location
										  ),

	Flags 			= generate_n_z_flags_map(AND_Result),

	Final_CPU 		= cpu_perform_actions(	
										 	[
										 		?UPDATE_FLAGS(Flags),
										 		?SET_CC_V(<<0:?SIZE_CC_V>>), %% always clear V
										 		?SET_PC(New_Pos)
										 	],
										 	New_CPU_Data
										 ),

	{New_Data, Final_CPU}.

%%%=======================================================================================
%% @doc 
% AND (8-BIT)<br/>
% Logically AND Memory Byte with Accumulator A or B <br/>
% R' = R AND (M) <br/>
% These instructions logically AND the contents of a byte in memory with either <br/>
% Accumulator A or B. The 8-bit result is then placed in the specified accumulator. <br/>
% <br/>
% N The Negative flag is set equal to the new value of bit 7 of the accumulator.<br/>
% Z The Zero flag is set if the new value of the accumulator is zero; cleared otherwise. <br/>
% V The Overflow flas is cleared by this instruction. <br/>
% C The Carry flag is not affected by this instruction. <br/>
% <br/>
% The AND instructions are commonly used for clearing bits and for testing bits.<br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec and_generic_part(
						_Pos 			::position_type(),
						Data 			::binary(),
						New_CPU_Data 	::cpu_type(),
						Memory_Data 	::binary(),
						Get_Reg_Fun 	::function(),
						Set_Reg_Fun 	::function(),
						New_Pos 		::position_type()
					  ) -> {binary(), cpu_type()}.
%%%=======================================================================================
and_generic_part(
					_Pos,
					Data,
					New_CPU_Data,
					Memory_Data,
					Get_Reg_Fun,
					Set_Reg_Fun,
					New_Pos
				) ->

	Reg 			= Get_Reg_Fun(New_CPU_Data),

	Result 		    = binary_logic:and_binary(Reg, Memory_Data),

	Flags 			= generate_n_z_flags_map(Result),

	Final_CPU 		= cpu_perform_actions(
										 	[
										 		fun(CPU) -> Set_Reg_Fun(Result,CPU) end,
										 		?UPDATE_FLAGS(Flags),
										 		?SET_CC_V(<<0:?SIZE_CC_V>>), %% always clear V
										 		?SET_PC(New_Pos)
										 	],
										 	New_CPU_Data
										 ),

	{Data, Final_CPU}.

%%%=======================================================================================
%% @doc 
% AND (8-BIT)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec anda_immediate_84(
							Pos 		::position_type(),
							Data 		::binary(),
							CPU_Data 	::cpu_type()
						) -> {binary(), cpu_type()}.
%%%=======================================================================================
anda_immediate_84(Pos,Data,CPU_Data) ->

	{Memory_Data, New_CPU_Data, New_Pos} = immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	and_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_a/1,
						fun cpu_set_a/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% AND (8-BIT)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec andb_immediate_c4(
							Pos 		::position_type(),
							Data 		::binary(),
							CPU_Data 	::cpu_type()
						) -> {binary(), cpu_type()}.
%%%=======================================================================================
andb_immediate_c4(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	and_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_b/1,
						fun cpu_set_b/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% AND (8-BIT)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec anda_direct_94(
						Pos 		::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					) -> {binary(), cpu_type()}.
%%%=======================================================================================
anda_direct_94(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = direct_1_address(pos_inc(Pos),Data,CPU_Data),

	and_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_a/1,
						fun cpu_set_a/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% AND (8-BIT)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec andb_direct_d4(
						Pos 		::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					) -> {binary(), cpu_type()}.
%%%=======================================================================================
andb_direct_d4(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = direct_1_address(pos_inc(Pos),Data,CPU_Data),

	and_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_b/1,
						fun cpu_set_b/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% AND (8-BIT)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec anda_indexed_a4(
						Pos 		::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					) -> {binary(), cpu_type()}.
%%%=======================================================================================
anda_indexed_a4(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = indexed_1_address(pos_inc(Pos),Data,CPU_Data),

	and_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_a/1,
						fun cpu_set_a/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% AND (8-BIT)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec andb_indexed_e4(
						Pos 		::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					) -> {binary(), cpu_type()}.
%%%=======================================================================================
andb_indexed_e4(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = indexed_1_address(pos_inc(Pos),Data,CPU_Data),

	and_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_b/1,
						fun cpu_set_b/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% AND (8-BIT)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec anda_extended_b4(
						Pos 		::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					 ) -> {binary(), cpu_type()}.
%%%=======================================================================================
anda_extended_b4(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = indexed_1_address(pos_inc(Pos),Data,CPU_Data),

	and_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_a/1,
						fun cpu_set_a/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% AND (8-BIT)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec andb_extended_f4(
						Pos 		::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					  ) -> {binary(), cpu_type()}.
%%%=======================================================================================
andb_extended_f4(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = indexed_1_address(pos_inc(Pos),Data,CPU_Data),

	and_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_b/1,
						fun cpu_set_b/2,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ANDCC<br/>
% Logically AND Immediate Value with the CC Register <br/>
% CC' = CC AND IMM <br/>
% <br/>
% This instruction logically ANDs the contents of the Condition Codes register with the <br/>
% immediate byte specified in the instruction. The result is placed back in the Condition <br/>
% Codes register. <br/>
% <br/>
% The ANDCC instruction provides a method to clear specific flags in the Condition Codes <br/>
% register. All flags that correspond to "0" bits in the immediate operand are cleared, while <br/>
% those corresponding with "1"s are left unchanged.<br/>
% <br/>
% The bit numbers for each flag are shown below:<br/>
% <br/>
% |7|6|5|4|3|2|1|0| <br/>
% |E|F|H|I|N|Z|V|C| <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec andcc_immediate_1c(
							Pos 		::position_type(),
							Data 		::binary(),
							CPU_Data 	::cpu_type()
						) -> {binary(), cpu_type()}.
%%%=======================================================================================
andcc_immediate_1c(Pos,Data,CPU_Data) ->

	{Memory_Data, New_CPU_Data, New_Pos} 	= immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	CC_Reg 									= cpu_get_cc(CPU_Data),

	Result 									= binary_logic:and_binary(Memory_Data,CC_Reg),

	Final_CPU 								= cpu_perform_actions(
																 	[
																 		
																 		?SET_CC(Result),
																 		?SET_PC(New_Pos)
																 	],
																 	New_CPU_Data
																 ),

	{Data, Final_CPU}.

%%%=======================================================================================
%% @doc 
% ANDD (6309)<br/>
% Logically AND Memory Word with Accumulator D <br/>
% ACCD' = ACCD AND (M:M+1) <br/>
% The ANDD instruction logically ANDs the contents of a double-byte value in memory <br/>
% with the contents of Accumulator D. The 16-bit result is placed back into Accumulator D. <br/>
% N The Negative flag is set equal to the new value of bit 15 of Accumulator D. <br/>
% Z The Zero flag is set if the new value of the Accumulator D is zero; cleared otherwise. <br/>
% V The Overflow flag is cleared by this instruction. <br/>
% C The Carry flag is not affected by this instruction. <br/>
% <br/>
% One use for the ANDD instruction is to truncate bits of an address value. <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec andd_generic_part(
							_Pos 			::position_type(),
							Data 			::binary(),
							New_CPU_Data 	::cpu_type(),
							Memory_Data 	::binary(),
							New_Pos 		::position_type()
						) -> {binary(), cpu_type()}.
%%%=======================================================================================
andd_generic_part(
					_Pos,
					Data,
					New_CPU_Data,
					Memory_Data,
					New_Pos
				) ->

	Reg 			= cpu_get_d(New_CPU_Data),

	Result 		    = binary_logic:and_binary(Reg, Memory_Data),

	Flags 			= generate_n_z_flags_map(Result),

	Final_CPU 		= cpu_perform_actions(
										 	[
										 		?SET_D(Result),
										 		?UPDATE_FLAGS(Flags),
										 		?SET_CC_V(<<0:?SIZE_CC_V>>), %% always clear V
										 		?SET_PC(New_Pos)
										 	],
										 	New_CPU_Data
										 ),

	{Data, Final_CPU}.

%%%=======================================================================================
%% @doc 
% ANDD (6309)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec andd_immediate_1084(
							Pos 		::position_type(),
							Data 		::binary(),
							CPU_Data 	::cpu_type()
						 ) -> {binary(), cpu_type()}.
%%%=======================================================================================
andd_immediate_1084(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = immediate_2_address(pos_inc2(Pos),Data,CPU_Data),

	andd_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ANDD (6309)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec andd_direct_1094(
						Pos 		::position_type(),
						Data 		::binary(),
						CPU_Data 	::cpu_type()
					 ) -> {binary(), cpu_type()}.
%%%=======================================================================================
andd_direct_1094(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = direct_2_address(pos_inc2(Pos),Data,CPU_Data),

	andd_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						New_Pos
					).

%%%=======================================================================================
%% @doc 
% ANDD (6309)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec andd_indexed_10a4(
							Pos 		::position_type(),
							Data 		::binary(),
							CPU_Data 	::cpu_type()
						) -> {binary(), binary()}.
%%%=======================================================================================
andd_indexed_10a4(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = indexed_2_address(pos_inc2(Pos),Data,CPU_Data),

	andd_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						New_Pos
					 ).

%%%=======================================================================================
%% @doc 
% ANDD (6309)<br/>
% See Generic Part for details <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec andd_extended_10b4(
							Pos 		::position_type(),
							Data 		::binary(),
							CPU_Data 	::cpu_type()
						) -> {binary(), cpu_type()}.
%%%=======================================================================================
andd_extended_10b4(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = indexed_2_address(pos_inc2(Pos),Data,CPU_Data),

	andd_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						New_Pos
					 ).

%%%=======================================================================================
%% @doc 
% ANDR (6309)<br/>
% Logically AND Source Register with Destination Register <br/>
% R1' = R1 AND R0 <br/>
% The ANDR instruction logically ANDs the contents of a source register with the contents <br/>
% of a destination register. The result is placed into the destination register. <br/>
% <br/>
% N The Negative flag is set equal to the value of the result's high-order bit. <br/>
% Z The Zero flag is set if the new value of the destination register is zero; cleared otherwise. <br/>
% V The Overflow flag is cleared by this instruction. <br/>
% C The Carry flag is not affected by this instruction. <br/>
% <br/>
% ---- Unit-Tested ----<br/>
-spec andr_immediate_1034(
							Pos 		::position_type(),
							Data 		::binary(),
							CPU_Data 	::cpu_type()
						) -> {binary(), cpu_type()}.
%%%=======================================================================================
andr_immediate_1034(Pos,Data,CPU_Data) ->

	{Memory_Data, New_CPU_Data, New_Pos} 	= immediate_1_address(pos_inc2(Pos),Data,CPU_Data),

	{Source_Reg, Destination_Reg} 			= decode_inter_register_post_byte(Memory_Data),

	Destination_Size 						= get_size_of_register(Destination_Reg),

	Source_Value 							= decode_inter_register_actual_source(Source_Reg,Destination_Size,New_CPU_Data),

	Destination_Value 						= get_register_from_name(Destination_Reg,New_CPU_Data),

	Result 									= binary_logic:and_binary(Source_Value,Destination_Value),

	New_CPU_Data_2 							= set_register_from_name(Destination_Reg,Result,New_CPU_Data),

	Flags 									= generate_n_z_flags_map(Result),

	Final_CPU 								= cpu_perform_actions(
																	[
																		?UPDATE_FLAGS(Flags),
																		?SET_CC_V(<<0:?SIZE_CC_V>>),
																		?SET_PC(New_Pos)
																	],
																	New_CPU_Data_2
																 ),

	{Data, Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
asla_inherent_48(Pos,Data,CPU_Data) -> 

	Reg 					= cpu_get_a(CPU_Data),

	{_Carry,Result,Flags} 	= binary_logic:asl_binary_with_flags(Reg),

	Final_CPU 				= cpu_perform_actions(
													[
														?UPDATE_FLAGS(Flags),
														?SET_A(Result),
														?SET_PC(pos_inc(Pos))
													],
													CPU_Data
												 ),

	{Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
aslb_inherent_58(Pos,Data,CPU_Data) -> 

	Reg 					= cpu_get_b(CPU_Data),

	{_Carry,Result,Flags} 	= binary_logic:asl_binary_with_flags(Reg),

	Final_CPU 				= cpu_perform_actions(
													[
														?UPDATE_FLAGS(Flags),
														?SET_B(Result),
														?SET_PC(pos_inc(Pos))
													],
													CPU_Data
												 ),

	{Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
asl_generic_part(
					_Pos,
					Data,
					New_CPU_Data,
					Memory_Data,
					Designated_Memory_Location,
					New_Pos
				) ->

	{_Carry,Result,Flags} 	= binary_logic:asl_binary_with_flags(Memory_Data),

	New_Data 				= ram_64k:set_byte_pos(
													Data,
													Result,
													Designated_Memory_Location
												  ),

	Final_CPU 				= cpu_perform_actions(
													[
														?UPDATE_FLAGS(Flags),
														?SET_PC(New_Pos)
													],
													New_CPU_Data
												 ),

	{New_Data,Final_CPU}.

%%----Unit_Tested-------------------------------------------------------------------------
asl_direct_08(Pos,Data,CPU_Data) ->

	{Memory_Data, New_CPU_Data, New_Pos} 	= direct_1_address(pos_inc(Pos),Data,CPU_Data),

	{Designated_Memory_Location,_,_} 		= direct_1_address_address_only(pos_inc(Pos),Data,CPU_Data),

	asl_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						Designated_Memory_Location,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
asl_indexed_68(Pos,Data,CPU_Data) ->

	{Memory_Data, New_CPU_Data, New_Pos} 	= indexed_1_address(pos_inc(Pos),Data,CPU_Data),

	{Designated_Memory_Location,_,_} 		= indexed_1_address_address_only(pos_inc(Pos),Data,CPU_Data),

	asl_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						Designated_Memory_Location,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
asl_extended_78(Pos,Data,CPU_Data) ->

	{Memory_Data, New_CPU_Data, New_Pos} 	= indexed_1_address(pos_inc(Pos),Data,CPU_Data),

	{Designated_Memory_Location,_,_} 		= indexed_1_address_address_only(pos_inc(Pos),Data,CPU_Data),

	asl_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						Designated_Memory_Location,
						New_Pos
					).

%%----Unit_Tested------------------------------------------------------------------------
asld_inherent_1048(Pos,Data,CPU_Data) -> 

	Reg 					= cpu_get_d(CPU_Data),

	{_Carry,Result,Flags} 	= binary_logic:asl_binary_with_flags(Reg),

	Final_CPU 				= cpu_perform_actions(
													[
														?UPDATE_FLAGS(Flags),
														?SET_D(Result),
														?SET_PC(pos_inc2(Pos))
													],
													CPU_Data
												 ),

	{Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
asra_inherent_47(Pos,Data,CPU_Data) -> 

	Reg 					= cpu_get_a(CPU_Data),

	{_Carry,Result,Flags} 	= binary_logic:asr_binary_with_flags(Reg),

	Final_CPU 				= cpu_perform_actions(
													[
														?UPDATE_FLAGS(Flags),
														?SET_A(Result),
														?SET_PC(pos_inc(Pos))
													],
													CPU_Data
												 ),

	{Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
asrb_inherent_57(Pos,Data,CPU_Data) -> 

	Reg 					= cpu_get_b(CPU_Data),

	{_Carry,Result,Flags} 	= binary_logic:asr_binary_with_flags(Reg),

	Final_CPU 				= cpu_perform_actions(
													[
														?UPDATE_FLAGS(Flags),
														?SET_B(Result),
														?SET_PC(pos_inc(Pos))
													],
													CPU_Data
												 ),

	{Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
asr_generic_part(
					_Pos,
					Data,
					New_CPU_Data,
					Memory_Data,
					Designated_Memory_Location,
					New_Pos
				) ->

	{_Carry,Result,Flags} 	= binary_logic:asr_binary_with_flags(Memory_Data),

	New_Data 				= ram_64k:set_byte_pos(
													Data,
													Result,
													Designated_Memory_Location
												  ),

	Final_CPU 				= cpu_perform_actions(
													[
														?UPDATE_FLAGS(Flags),
														?SET_PC(New_Pos)
													],
													New_CPU_Data
												 ),

	{New_Data,Final_CPU}.

%%----Unit_Tested-------------------------------------------------------------------------
asr_direct_07(Pos,Data,CPU_Data) ->

	{Memory_Data, New_CPU_Data, New_Pos} 	= direct_1_address(pos_inc(Pos),Data,CPU_Data),

	{Designated_Memory_Location,_,_} 		= direct_1_address_address_only(pos_inc(Pos),Data,CPU_Data),

	asr_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						Designated_Memory_Location,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
asr_indexed_67(Pos,Data,CPU_Data) ->

	{Memory_Data, New_CPU_Data, New_Pos} 	= indexed_1_address(pos_inc(Pos),Data,CPU_Data),

	{Designated_Memory_Location,_,_} 		= indexed_1_address_address_only(pos_inc(Pos),Data,CPU_Data),

	asr_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						Designated_Memory_Location,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
asr_extended_77(Pos,Data,CPU_Data) ->

	{Memory_Data, New_CPU_Data, New_Pos} 	= indexed_1_address(pos_inc(Pos),Data,CPU_Data),

	{Designated_Memory_Location,_,_} 		= indexed_1_address_address_only(pos_inc(Pos),Data,CPU_Data),

	asr_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						Designated_Memory_Location,
						New_Pos
					).

%%----Unit_Tested------------------------------------------------------------------------
asrd_inherent_1047(Pos,Data,CPU_Data) -> 

	Reg 					= cpu_get_d(CPU_Data),

	{_Carry,Result,Flags} 	= binary_logic:asr_binary_with_flags(Reg),

	Final_CPU 				= cpu_perform_actions(
													[
														?UPDATE_FLAGS(Flags),
														?SET_D(Result),
														?SET_PC(pos_inc2(Pos))
													],
													CPU_Data
												 ),

	{Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
band_direct_1130(Pos,Data,CPU_Data) -> 

	Post_Byte 											= get_byte_pos(Data,pos_inc2(Pos)),

	{Memory_Data, New_CPU_Data, New_Pos} 				= direct_1_address(pos_inc_n(Pos,3),Data,CPU_Data),

	<<RR:2,Source_Bit_Num:3,Destination_Bit_Num:3>> 	= Post_Byte,

	Reg_Name 											= band_reg_name_decode(RR),

	Reg_Value 											= get_register_from_name(Reg_Name,New_CPU_Data),
	
	Source_Bit 											= binary_logic:get_bit(Memory_Data,Source_Bit_Num),

	Destination_Bit 									= binary_logic:get_bit(Reg_Value,Destination_Bit_Num),

	Result_Bit 											= binary_logic:and_gate(Source_Bit,Destination_Bit),

	New_Reg_Value 										= binary_logic:set_bit(Reg_Value,Destination_Bit_Num,Result_Bit),

	Final_CPU 											= cpu_perform_actions(
																				[
																					fun(CPU) -> set_register_from_name(Reg_Name,New_Reg_Value,CPU) end,
																					?SET_PC(New_Pos)
																				],
																				CPU_Data
																			 ),

	{Data,Final_CPU}.

%%----------------------------------------------------------------------------
-spec band_reg_name_decode(RR::non_neg_integer()) -> atom().
%%----Unit_Tested------------------------------------------------------------------------
band_reg_name_decode(RR) ->

	case RR of

		2#00 -> cc;
		2#01 -> a;
		2#10 -> b
	end.

%%----Unit_Tested------------------------------------------------------------------------
bcc_relative_24(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	<<CC_C_Value:1>> 						= cpu_get_cc_c(CPU_Data),

	Final_CPU 								= case CC_C_Value of 

													0 -> 	<<Relative_Jump:8/signed>> 	= Memory_Data,
															<<New_Pos_Value:16>> 		= New_Pos,
															cpu_perform_actions(
																					[
																						?SET_PC(<<(New_Pos_Value + Relative_Jump):?SIZE_ADDRESS>>)
																					],
																					New_CPU_Data
																				 );

													1 -> 	cpu_perform_actions(
																					[
																						?SET_PC(New_Pos)
																					],
																					New_CPU_Data
																				 )

												end,

	{Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
bcs_relative_25(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	<<CC_C_Value:1>> 						= cpu_get_cc_c(CPU_Data),

	Final_CPU 								= case CC_C_Value of 

													0 -> 	cpu_perform_actions(
																					[
																						?SET_PC(New_Pos)
																					],
																					New_CPU_Data
																				 );

													1 -> 	<<Relative_Jump:8/signed>> 	= Memory_Data,
															<<New_Pos_Value:16>> 		= New_Pos,
															cpu_perform_actions(
																					[
																						?SET_PC(<<(New_Pos_Value + Relative_Jump):?SIZE_ADDRESS>>)
																					],
																					New_CPU_Data
																				 )
												end,

	{Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
beor_direct_1134(Pos,Data,CPU_Data) -> 

	Post_Byte 											= get_byte_pos(Data,pos_inc2(Pos)),

	{Memory_Data, New_CPU_Data, New_Pos} 				= direct_1_address(pos_inc_n(Pos,3),Data,CPU_Data),

	<<RR:2,Source_Bit_Num:3,Destination_Bit_Num:3>> 	= Post_Byte,

	Reg_Name 											= band_reg_name_decode(RR),

	Reg_Value 											= get_register_from_name(Reg_Name,New_CPU_Data),
	
	Source_Bit 											= binary_logic:get_bit(Memory_Data,Source_Bit_Num),

	Destination_Bit 									= binary_logic:get_bit(Reg_Value,Destination_Bit_Num),

	Result_Bit 											= binary_logic:xor_gate(Source_Bit,Destination_Bit),

	New_Reg_Value 										= binary_logic:set_bit(Reg_Value,Destination_Bit_Num,Result_Bit),

	Final_CPU 											= cpu_perform_actions(
																				[
																					fun(CPU) -> set_register_from_name(Reg_Name,New_Reg_Value,CPU) end,
																					?SET_PC(New_Pos)
																				],
																				CPU_Data
																			 ),

	{Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
beq_relative_27(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	<<CC_Z_Value:1>> 						= cpu_get_cc_z(CPU_Data),

	Final_CPU 								= case CC_Z_Value of 

													0 -> 	cpu_perform_actions(
																					[
																						?SET_PC(New_Pos)
																					],
																					New_CPU_Data
																				 );

													1 -> 	<<Relative_Jump:8/signed>> 	= Memory_Data,
															<<New_Pos_Value:16>> 		= New_Pos,
															cpu_perform_actions(
																					[
																						?SET_PC(<<(New_Pos_Value + Relative_Jump):?SIZE_ADDRESS>>)
																					],
																					New_CPU_Data
																				 )
												end,

	{Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
bge_relative_2c(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	<<CC_N_Value:1>> 						= cpu_get_cc_n(CPU_Data),

	<<CC_V_Value:1>> 						= cpu_get_cc_v(CPU_Data),

	Greater_Status 							= CC_N_Value =:= CC_V_Value,

	Final_CPU 								= case Greater_Status of 

													false   -> 	cpu_perform_actions(
																					[
																						?SET_PC(New_Pos)
																					],
																					New_CPU_Data
																				 );

													true    -> 	<<Relative_Jump:8/signed>> 	= Memory_Data,
																<<New_Pos_Value:16>> 		= New_Pos,
																cpu_perform_actions(
																						[
																							?SET_PC(<<(New_Pos_Value + Relative_Jump):?SIZE_ADDRESS>>)
																						],
																						New_CPU_Data
																					 )
												end,

	{Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
bgt_relative_2e(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	<<CC_Z_Value:1>> 						= cpu_get_cc_z(CPU_Data),

	<<CC_N_Value:1>> 						= cpu_get_cc_n(CPU_Data),

	<<CC_V_Value:1>> 						= cpu_get_cc_v(CPU_Data),

	Status 									= (CC_N_Value =:= CC_V_Value) andalso (CC_Z_Value =:= 0),

	Final_CPU 								= case Status of 

													false   -> 	cpu_perform_actions(
																					[
																						?SET_PC(New_Pos)
																					],
																					New_CPU_Data
																				 );

													true    -> 	<<Relative_Jump:8/signed>> 	= Memory_Data,
																<<New_Pos_Value:16>> 		= New_Pos,
																cpu_perform_actions(
																						[
																							?SET_PC(<<(New_Pos_Value + Relative_Jump):?SIZE_ADDRESS>>)
																						],
																						New_CPU_Data
																					 )
												end,

	{Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
bhi_relative_22(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	<<CC_Z_Value:1>> 						= cpu_get_cc_z(CPU_Data),

	<<CC_C_Value:1>> 						= cpu_get_cc_c(CPU_Data),

	Greater_Status 							= (CC_Z_Value =:= 0) andalso (CC_C_Value =:= 0),

	Final_CPU 								= case Greater_Status of 

													false   -> 	cpu_perform_actions(
																					[
																						?SET_PC(New_Pos)
																					],
																					New_CPU_Data
																				 );

													true    -> 	<<Relative_Jump:8/signed>> 	= Memory_Data,
																<<New_Pos_Value:16>> 		= New_Pos,
																cpu_perform_actions(
																						[
																							?SET_PC(<<(New_Pos_Value + Relative_Jump):?SIZE_ADDRESS>>)
																						],
																						New_CPU_Data
																					 )
												end,

	{Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
bhs_relative_24(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	<<CC_C_Value:1>> 						= cpu_get_cc_c(CPU_Data),

	Greater_Status 							= CC_C_Value =:= 0,

	Final_CPU 								= case Greater_Status of 

													false   -> 	cpu_perform_actions(
																					[
																						?SET_PC(New_Pos)
																					],
																					New_CPU_Data
																				 );

													true    -> 	<<Relative_Jump:8/signed>> 	= Memory_Data,
																<<New_Pos_Value:16>> 		= New_Pos,
																cpu_perform_actions(
																						[
																							?SET_PC(<<(New_Pos_Value + Relative_Jump):?SIZE_ADDRESS>>)
																						],
																						New_CPU_Data
																					 )
												end,

	{Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
biand_direct_1131(Pos,Data,CPU_Data) -> 

	Post_Byte 											= get_byte_pos(Data,pos_inc2(Pos)),

	{Memory_Data, New_CPU_Data, New_Pos} 				= direct_1_address(pos_inc_n(Pos,3),Data,CPU_Data),

	<<RR:2,Source_Bit_Num:3,Destination_Bit_Num:3>> 	= Post_Byte,

	Reg_Name 											= band_reg_name_decode(RR),

	Reg_Value 											= get_register_from_name(Reg_Name,New_CPU_Data),
	
	Source_Bit 											= binary_logic:get_bit(Memory_Data,Source_Bit_Num),

	Destination_Bit 									= binary_logic:get_bit(Reg_Value,Destination_Bit_Num),

	Result_Bit 											= binary_logic:and_gate(binary_logic:inverter(Source_Bit),Destination_Bit),

	New_Reg_Value 										= binary_logic:set_bit(Reg_Value,Destination_Bit_Num,Result_Bit),

	Final_CPU 											= cpu_perform_actions(
																				[
																					fun(CPU) -> set_register_from_name(Reg_Name,New_Reg_Value,CPU) end,
																					?SET_PC(New_Pos)
																				],
																				CPU_Data
																			 ),

	{Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
bieor_direct_1135(Pos,Data,CPU_Data) -> 

	Post_Byte 											= get_byte_pos(Data,pos_inc2(Pos)),

	{Memory_Data, New_CPU_Data, New_Pos} 				= direct_1_address(pos_inc_n(Pos,3),Data,CPU_Data),

	<<RR:2,Source_Bit_Num:3,Destination_Bit_Num:3>> 	= Post_Byte,

	Reg_Name 											= band_reg_name_decode(RR),

	Reg_Value 											= get_register_from_name(Reg_Name,New_CPU_Data),
	
	Source_Bit 											= binary_logic:get_bit(Memory_Data,Source_Bit_Num),

	Destination_Bit 									= binary_logic:get_bit(Reg_Value,Destination_Bit_Num),

	Result_Bit 											= binary_logic:xor_gate(binary_logic:inverter(Source_Bit),Destination_Bit),

	New_Reg_Value 										= binary_logic:set_bit(Reg_Value,Destination_Bit_Num,Result_Bit),

	Final_CPU 											= cpu_perform_actions(
																				[
																					fun(CPU) -> set_register_from_name(Reg_Name,New_Reg_Value,CPU) end,
																					?SET_PC(New_Pos)
																				],
																				CPU_Data
																			 ),

	{Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
bior_direct_1133(Pos,Data,CPU_Data) -> 

	Post_Byte 											= get_byte_pos(Data,pos_inc2(Pos)),

	{Memory_Data, New_CPU_Data, New_Pos} 				= direct_1_address(pos_inc_n(Pos,3),Data,CPU_Data),

	<<RR:2,Source_Bit_Num:3,Destination_Bit_Num:3>> 	= Post_Byte,

	Reg_Name 											= band_reg_name_decode(RR),

	Reg_Value 											= get_register_from_name(Reg_Name,New_CPU_Data),
	
	Source_Bit 											= binary_logic:get_bit(Memory_Data,Source_Bit_Num),

	Destination_Bit 									= binary_logic:get_bit(Reg_Value,Destination_Bit_Num),

	Result_Bit 											= binary_logic:or_gate(binary_logic:inverter(Source_Bit),Destination_Bit),

	New_Reg_Value 										= binary_logic:set_bit(Reg_Value,Destination_Bit_Num,Result_Bit),

	Final_CPU 											= cpu_perform_actions(
																				[
																					fun(CPU) -> set_register_from_name(Reg_Name,New_Reg_Value,CPU) end,
																					?SET_PC(New_Pos)
																				],
																				CPU_Data
																			 ),

	{Data,Final_CPU}.

%%----Unit_Tested-------------------------------------------------------------------------
bit_generic_part(
					_Pos,
					Data,
					New_CPU_Data,
					Memory_Data,
					Get_Reg_Fun,
					New_Pos
				) ->

	Reg 							= Get_Reg_Fun(New_CPU_Data),

	Result 							= binary_logic:and_binary(Reg, Memory_Data),

	Flags 							= generate_n_z_flags_map(Result),


	Final_CPU 						= cpu_perform_actions(
														 	[
														 		?UPDATE_FLAGS(Flags),
														 		?SET_CC_V(<<0:?SIZE_CC_V>>),
														 		?SET_PC(New_Pos)
														 	],
														 	New_CPU_Data
														 ),

	{Data, Final_CPU}.

%%----Unit_Tested-------------------------------------------------------------------------
bita_immediate_85(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	bit_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_a/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
bitb_immediate_c5(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	bit_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_b/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
bita_direct_95(Pos,Data,CPU_Data) ->

	{Memory_Data, New_CPU_Data, New_Pos} 	= direct_1_address(pos_inc(Pos),Data,CPU_Data),

	bit_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_a/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
bitb_direct_d5(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= direct_1_address(pos_inc(Pos),Data,CPU_Data),

	bit_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_b/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
bita_indexed_a5(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= indexed_1_address(pos_inc(Pos),Data,CPU_Data),

	bit_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_a/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
bitb_indexed_e5(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= indexed_1_address(pos_inc(Pos),Data,CPU_Data),

	bit_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_b/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
bita_extended_b5(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= indexed_1_address(pos_inc(Pos),Data,CPU_Data),

	bit_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_a/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
bitb_extended_f5(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= indexed_1_address(pos_inc(Pos),Data,CPU_Data),

	bit_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_b/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
bitd_immediate_1085(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= immediate_2_address(pos_inc2(Pos),Data,CPU_Data),

	bit_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_d/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
bitd_direct_1095(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= direct_2_address(pos_inc2(Pos),Data,CPU_Data),

	bit_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_d/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
bitd_indexed_10a5(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= indexed_2_address(pos_inc2(Pos),Data,CPU_Data),

	bit_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_d/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
bitd_extended_10b5(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= indexed_2_address(pos_inc2(Pos),Data,CPU_Data),

	bit_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_d/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
bitmd_immediate_113c(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= immediate_1_address(pos_inc2(Pos),Data,CPU_Data),

	Reg_MD 									= cpu_get_md(New_CPU_Data),

	<<Div0:1,IL:1,Rest/bits>> 				= Reg_MD,

	<<Bit7:1,Bit6:1,_/bits>> 				= Memory_Data,

	Result 									= binary_logic:and_binary(<<Div0:1,IL:1>>,<<Bit7:1,Bit6:1>>),

	Zero_Status 							= Result =:= <<0:2>>,

	CC_Z 								    = case Zero_Status of
													true -> <<1:1>>;
													_ 	 -> <<0:1>>
												end,

	New_Div0 								= case Bit7 of

													0 -> <<Div0:1>>;
													1 -> <<0:1>>
												end,

	New_IL 									= case Bit6 of

													0 -> <<IL:1>>;
													1 -> <<0:1>>
												end,

	Final_CPU 								= cpu_perform_actions(
																 	[
																 		?SET_MD(<<New_Div0/bits,New_IL/bits,Rest/bits>>),
																 		?SET_CC_Z(CC_Z),
																 		?SET_PC(New_Pos)
																 	],
																 	New_CPU_Data
																 ),

	{Data, Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
ble_relative_2f(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	<<CC_Z_Value:1>> 						= cpu_get_cc_z(CPU_Data),

	<<CC_N_Value:1>> 						= cpu_get_cc_n(CPU_Data),

	<<CC_V_Value:1>> 						= cpu_get_cc_v(CPU_Data),

	Status 									= (CC_N_Value =/= CC_V_Value) orelse (CC_Z_Value =:= 1),

	Final_CPU 								= case Status of 

													false   -> 	cpu_perform_actions(
																					[
																						?SET_PC(New_Pos)
																					],
																					New_CPU_Data
																				 );

													true    -> 	<<Relative_Jump:8/signed>> 	= Memory_Data,
																<<New_Pos_Value:16>> 		= New_Pos,
																cpu_perform_actions(
																						[
																							?SET_PC(<<(New_Pos_Value + Relative_Jump):?SIZE_ADDRESS>>)
																						],
																						New_CPU_Data
																					 )
												end,

	{Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
blo_relative_25(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	<<CC_C_Value:1>> 						= cpu_get_cc_c(CPU_Data),

	Final_CPU 								= case CC_C_Value of 

													0 -> 	cpu_perform_actions(
																					[
																						?SET_PC(New_Pos)
																					],
																					New_CPU_Data
																				 );

													1 -> 	<<Relative_Jump:8/signed>> 	= Memory_Data,
															<<New_Pos_Value:16>> 		= New_Pos,
															cpu_perform_actions(
																					[
																						?SET_PC(<<(New_Pos_Value + Relative_Jump):?SIZE_ADDRESS>>)
																					],
																					New_CPU_Data
																				 )
												end,

	{Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
bls_relative_23(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	<<CC_Z_Value:1>> 						= cpu_get_cc_z(CPU_Data),

	<<CC_C_Value:1>> 						= cpu_get_cc_c(CPU_Data),

	Status 									= (CC_Z_Value =/= 0) orelse (CC_C_Value =/= 0),

	Final_CPU 								= case Status of 

													false   -> 	cpu_perform_actions(
																					[
																						?SET_PC(New_Pos)
																					],
																					New_CPU_Data
																				 );

													true    -> 	<<Relative_Jump:8/signed>> 	= Memory_Data,
																<<New_Pos_Value:16>> 		= New_Pos,
																cpu_perform_actions(
																						[
																							?SET_PC(<<(New_Pos_Value + Relative_Jump):?SIZE_ADDRESS>>)
																						],
																						New_CPU_Data
																					 )
												end,

	{Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
blt_relative_2d(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	<<CC_N_Value:1>> 						= cpu_get_cc_n(CPU_Data),

	<<CC_V_Value:1>> 						= cpu_get_cc_v(CPU_Data),

	Status 									= (CC_N_Value =/= CC_V_Value),

	Final_CPU 								= case Status of 

													false   -> 	cpu_perform_actions(
																					[
																						?SET_PC(New_Pos)
																					],
																					New_CPU_Data
																				 );

													true    -> 	<<Relative_Jump:8/signed>> 	= Memory_Data,
																<<New_Pos_Value:16>> 		= New_Pos,
																cpu_perform_actions(
																						[
																							?SET_PC(<<(New_Pos_Value + Relative_Jump):?SIZE_ADDRESS>>)
																						],
																						New_CPU_Data
																					 )
												end,

	{Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
bmi_relative_2b(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	<<CC_N_Value:1>> 						= cpu_get_cc_n(CPU_Data),

	Status 									= (CC_N_Value =/= 0),

	Final_CPU 								= case Status of 

													false   -> 	cpu_perform_actions(
																					[
																						?SET_PC(New_Pos)
																					],
																					New_CPU_Data
																				 );

													true    -> 	<<Relative_Jump:8/signed>> 	= Memory_Data,
																<<New_Pos_Value:16>> 		= New_Pos,
																cpu_perform_actions(
																						[
																							?SET_PC(<<(New_Pos_Value + Relative_Jump):?SIZE_ADDRESS>>)
																						],
																						New_CPU_Data
																					 )
												end,

	{Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
bne_relative_26(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	<<CC_Z_Value:1>> 						= cpu_get_cc_z(CPU_Data),

	Status 									= (CC_Z_Value =:= 0),

	Final_CPU 								= case Status of 

													false   -> 	cpu_perform_actions(
																					[
																						?SET_PC(New_Pos)
																					],
																					New_CPU_Data
																				 );

													true    -> 	<<Relative_Jump:8/signed>> 	= Memory_Data,
																<<New_Pos_Value:16>> 		= New_Pos,
																cpu_perform_actions(
																						[
																							?SET_PC(<<(New_Pos_Value + Relative_Jump):?SIZE_ADDRESS>>)
																						],
																						New_CPU_Data
																					 )
												end,

	{Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
bor_direct_1132(Pos,Data,CPU_Data) -> 

	Post_Byte 											= get_byte_pos(Data,pos_inc2(Pos)),

	{Memory_Data, New_CPU_Data, New_Pos} 				= direct_1_address(pos_inc_n(Pos,3),Data,CPU_Data),

	<<RR:2,Source_Bit_Num:3,Destination_Bit_Num:3>> 	= Post_Byte,

	Reg_Name 											= band_reg_name_decode(RR),

	Reg_Value 											= get_register_from_name(Reg_Name,New_CPU_Data),
	
	Source_Bit 											= binary_logic:get_bit(Memory_Data,Source_Bit_Num),

	Destination_Bit 									= binary_logic:get_bit(Reg_Value,Destination_Bit_Num),

	Result_Bit 											= binary_logic:or_gate(Source_Bit,Destination_Bit),

	New_Reg_Value 										= binary_logic:set_bit(Reg_Value,Destination_Bit_Num,Result_Bit),

	Final_CPU 											= cpu_perform_actions(
																				[
																					fun(CPU) -> set_register_from_name(Reg_Name,New_Reg_Value,CPU) end,
																					?SET_PC(New_Pos)
																				],
																				CPU_Data
																			 ),

	{Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
bpl_relative_2a(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	<<CC_N_Value:1>> 						= cpu_get_cc_n(CPU_Data),

	Status 									= (CC_N_Value =:= 0),

	Final_CPU 								= case Status of 

													false   -> 	cpu_perform_actions(
																					[
																						?SET_PC(New_Pos)
																					],
																					New_CPU_Data
																				 );

													true    -> 	<<Relative_Jump:8/signed>> 	= Memory_Data,
																<<New_Pos_Value:16>> 		= New_Pos,
																cpu_perform_actions(
																						[
																							?SET_PC(<<(New_Pos_Value + Relative_Jump):?SIZE_ADDRESS>>)
																						],
																						New_CPU_Data
																					 )
												end,

	{Data,Final_CPU}.

%%----------------------------------------------------------------------------
bra_relative_20(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	<<Relative_Jump:8/signed>> 				= Memory_Data,

	<<New_Pos_Value:16>> 					= New_Pos,

	Final_CPU 								= cpu_perform_actions(
																	[
																		?SET_PC(<<(New_Pos_Value + Relative_Jump):?SIZE_ADDRESS>>)
																	],
																	New_CPU_Data
																 ),

	{Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
brn_relative_21(Pos,Data,CPU_Data) -> 

	{_Memory_Data, New_CPU_Data, New_Pos} 	= immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	Final_CPU 								= cpu_perform_actions(
																	[
																		?SET_PC(New_Pos)
																	],
																	New_CPU_Data
																 ),

	{Data,Final_CPU}.

%%----------------------------------------------------------------------------
bsr_relative_8d(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	Reg_S 									= cpu_get_s(New_CPU_Data),

	<<Reg_S_Value:?SIZE_S>> 				= Reg_S,	

	New_Reg_S_Value 						= Reg_S_Value - 2,

	New_Data 								= ram_64k:set_word_pos(Data,New_Pos,<<New_Reg_S_Value:?SIZE_ADDRESS>>),

	<<Relative_Jump:8/signed>> 				= Memory_Data,
	<<New_Pos_Value:?SIZE_ADDRESS>> 		= New_Pos,
															
	Final_CPU 								= cpu_perform_actions(
																	[
																		?SET_S(<<New_Reg_S_Value:?SIZE_S>>),
																		?SET_PC(<<(New_Pos_Value + Relative_Jump):?SIZE_ADDRESS>>)
																	],
																	New_CPU_Data
																 ),

	{New_Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
bvc_relative_28(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	<<CC_V_Value:1>> 						= cpu_get_cc_v(CPU_Data),

	Status 									= (CC_V_Value =:= 0),

	Final_CPU 								= case Status of 

													false   -> 	cpu_perform_actions(
																					[
																						?SET_PC(New_Pos)
																					],
																					New_CPU_Data
																				 );

													true    -> 	<<Relative_Jump:8/signed>> 	= Memory_Data,
																<<New_Pos_Value:16>> 		= New_Pos,
																cpu_perform_actions(
																						[
																							?SET_PC(<<(New_Pos_Value + Relative_Jump):?SIZE_ADDRESS>>)
																						],
																						New_CPU_Data
																					 )
												end,

	{Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
bvs_relative_29(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} 	= immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	<<CC_V_Value:1>> 						= cpu_get_cc_v(CPU_Data),

	Status 									= (CC_V_Value =/= 0),

	Final_CPU 								= case Status of 

													false   -> 	cpu_perform_actions(
																					[
																						?SET_PC(New_Pos)
																					],
																					New_CPU_Data
																				 );

													true    -> 	<<Relative_Jump:8/signed>> 	= Memory_Data,
																<<New_Pos_Value:16>> 		= New_Pos,
																cpu_perform_actions(
																						[
																							?SET_PC(<<(New_Pos_Value + Relative_Jump):?SIZE_ADDRESS>>)
																						],
																						New_CPU_Data
																					 )
												end,

	{Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
clra_inherent_4f(Pos,Data,CPU_Data) -> 

	Final_CPU 	= cpu_perform_actions(
										[
											?SET_A(<<0:?SIZE_A>>),
											?SET_CC_N(<<0:?SIZE_CC_N>>),
											?SET_CC_Z(<<1:?SIZE_CC_Z>>),
											?SET_CC_V(<<0:?SIZE_CC_V>>),
											?SET_CC_C(<<0:?SIZE_CC_C>>),
											?SET_PC(pos_inc(Pos))
										],
										CPU_Data
									 ),

	{Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
clrb_inherent_5f(Pos,Data,CPU_Data) -> 

	Final_CPU 	= cpu_perform_actions(
										[
											?SET_B(<<0:?SIZE_B>>),
											?SET_CC_N(<<0:?SIZE_CC_N>>),
											?SET_CC_Z(<<1:?SIZE_CC_Z>>),
											?SET_CC_V(<<0:?SIZE_CC_V>>),
											?SET_CC_C(<<0:?SIZE_CC_C>>),
											?SET_PC(pos_inc(Pos))
										],
										CPU_Data
									 ),

	{Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
clrd_inherent_104f(Pos,Data,CPU_Data) -> 

	Final_CPU 	= cpu_perform_actions(
										[
											?SET_D(<<0:?SIZE_D>>),
											?SET_CC_N(<<0:?SIZE_CC_N>>),
											?SET_CC_Z(<<1:?SIZE_CC_Z>>),
											?SET_CC_V(<<0:?SIZE_CC_V>>),
											?SET_CC_C(<<0:?SIZE_CC_C>>),
											?SET_PC(pos_inc2(Pos))
										],
										CPU_Data
									 ),

	{Data,Final_CPU}.

%%----------------------------------------------------------------------------
clre_inherent_114f(Pos,Data,CPU_Data) -> 

	Final_CPU 	= cpu_perform_actions(
										[
											?SET_E(<<0:?SIZE_E>>),
											?SET_CC_N(<<0:?SIZE_CC_N>>),
											?SET_CC_Z(<<1:?SIZE_CC_Z>>),
											?SET_CC_V(<<0:?SIZE_CC_V>>),
											?SET_CC_C(<<0:?SIZE_CC_C>>),
											?SET_PC(pos_inc(Pos))
										],
										CPU_Data
									 ),

	{Data,Final_CPU}.

%%----------------------------------------------------------------------------
clrf_inherent_115f(Pos,Data,CPU_Data) -> 

	Final_CPU 	= cpu_perform_actions(
										[
											?SET_F(<<0:?SIZE_F>>),
											?SET_CC_N(<<0:?SIZE_CC_N>>),
											?SET_CC_Z(<<1:?SIZE_CC_Z>>),
											?SET_CC_V(<<0:?SIZE_CC_V>>),
											?SET_CC_C(<<0:?SIZE_CC_C>>),
											?SET_PC(pos_inc(Pos))
										],
										CPU_Data
									 ),

	{Data,Final_CPU}.

%%----Unit_Tested------------------------------------------------------------------------
clrw_inherent_105f(Pos,Data,CPU_Data) -> 

	Final_CPU 	= cpu_perform_actions(
										[
											?SET_W(<<0:?SIZE_W>>),
											?SET_CC_N(<<0:?SIZE_CC_N>>),
											?SET_CC_Z(<<1:?SIZE_CC_Z>>),
											?SET_CC_V(<<0:?SIZE_CC_V>>),
											?SET_CC_C(<<0:?SIZE_CC_C>>),
											?SET_PC(pos_inc2(Pos))
										],
										CPU_Data
									 ),

	{Data, Final_CPU}.

%%----Unit_Tested-------------------------------------------------------------------------
clr_generic_part(
					_Pos,
					Data,
					New_CPU_Data,
					Designated_Memory_Location,
					New_Pos
				) ->

	New_Data 		= ram_64k:set_byte_pos(
											Data,
											<<0:8>>,
											Designated_Memory_Location
										  ),

	Final_CPU 		= cpu_perform_actions(	
										 	[
										 		?SET_CC_N(<<0:?SIZE_CC_N>>),
												?SET_CC_Z(<<1:?SIZE_CC_Z>>),
												?SET_CC_V(<<0:?SIZE_CC_V>>),
												?SET_CC_C(<<0:?SIZE_CC_C>>),
										 		?SET_PC(New_Pos)
										 	],
										 	New_CPU_Data
										 ),

	{New_Data, Final_CPU}.

%%----Unit_Tested-------------------------------------------------------------------------
clr_direct_0f(Pos,Data,CPU_Data) -> 

	{Designated_Memory_Location, New_CPU_Data, New_Pos} = direct_1_address_address_only(pos_inc(Pos),Data,CPU_Data),

	clr_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Designated_Memory_Location,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
clr_indexed_6f(Pos,Data,CPU_Data) -> 

	{Designated_Memory_Location, New_CPU_Data, New_Pos} = indexed_1_address_address_only(pos_inc(Pos),Data,CPU_Data),

	clr_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Designated_Memory_Location,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
clr_extended_7f(Pos,Data,CPU_Data) -> 

	{Designated_Memory_Location, New_CPU_Data, New_Pos} = indexed_1_address_address_only(pos_inc(Pos),Data,CPU_Data),

	clr_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Designated_Memory_Location,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmp_generic_part(
					_Pos,
					Data,
					New_CPU_Data,
					Memory_Data,
					Get_Reg_Fun,
					New_Pos
				) ->

	Reg 							= Get_Reg_Fun(New_CPU_Data),

	{_Carry,_Result,Flags} 			= binary_logic:generic_subtraction_with_flags(Reg, Memory_Data),

	Final_CPU 						= cpu_perform_actions(
														 	[
														 		?UPDATE_FLAGS(Flags),
														 		?SET_PC(New_Pos)
														 	],
														 	New_CPU_Data
														 ),

	{Data, Final_CPU}.

%%----Unit_Tested-------------------------------------------------------------------------
cmpa_immediate_81(Pos,Data,CPU_Data) ->

	{Memory_Data, New_CPU_Data, New_Pos} = immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_a/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpb_immediate_c1(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = immediate_1_address(pos_inc(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_b/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpe_immediate_1181(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = immediate_1_address(pos_inc2(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_e/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpf_immediate_11c1(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = immediate_1_address(pos_inc2(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_f/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpa_direct_91(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = direct_1_address(pos_inc(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_a/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpb_direct_d1(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = direct_1_address(pos_inc(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_b/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpe_direct_1191(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = direct_1_address(pos_inc2(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_e/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpf_direct_11d1(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = direct_1_address(pos_inc2(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_f/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpa_indexed_a1(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = indexed_1_address(pos_inc(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_a/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpb_indexed_e1(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = indexed_1_address(pos_inc(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_b/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpe_indexed_11a1(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = indexed_1_address(pos_inc2(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_e/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpf_indexed_11e1(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = indexed_1_address(pos_inc2(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_f/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpa_extended_b1(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = indexed_1_address(pos_inc(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_a/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpb_extended_f1(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = indexed_1_address(pos_inc(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_b/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpe_extended_11b1(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = indexed_1_address(pos_inc2(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_e/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpf_extended_11f1(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = indexed_1_address(pos_inc2(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_f/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpd_immediate_0083(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = immediate_2_address(pos_inc2(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_d/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmps_immediate_018C(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = immediate_2_address(pos_inc2(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_s/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpu_immediate_0183(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = immediate_2_address(pos_inc2(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_u/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpw_immediate_0081(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = immediate_2_address(pos_inc2(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_w/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpx_immediate_8c(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = immediate_2_address(pos_inc(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_x/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpy_immediate_008c(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = immediate_2_address(pos_inc2(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_y/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpd_direct_0093(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = direct_2_address(pos_inc2(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_d/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmps_direct_019C(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = direct_2_address(pos_inc2(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_s/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpu_direct_0193(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = direct_2_address(pos_inc2(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_u/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpw_direct_0091(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = direct_2_address(pos_inc2(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_w/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpx_direct_9c(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = direct_2_address(pos_inc(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_x/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpy_direct_009c(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = direct_2_address(pos_inc2(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_y/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpd_indexed_00a3(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = indexed_2_address(pos_inc2(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_d/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmps_indexed_01ac(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = indexed_2_address(pos_inc2(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_s/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpu_indexed_01a3(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = indexed_2_address(pos_inc2(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_u/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpw_indexed_00a1(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = indexed_2_address(pos_inc2(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_w/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpx_indexed_ac(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = indexed_2_address(pos_inc(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_x/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpy_indexed_00ac(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = indexed_2_address(pos_inc2(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_y/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpd_extended_00b3(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = indexed_2_address(pos_inc2(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_d/1,
						New_Pos
					).

%%-----------------------------------------------------------------------------
cmps_extended_01bc(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = indexed_2_address(pos_inc2(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_s/1,
						New_Pos
					).

%%-----------------------------------------------------------------------------
cmpu_extended_01b3(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = indexed_2_address(pos_inc2(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_u/1,
						New_Pos
					).

%%-----------------------------------------------------------------------------
cmpw_extended_00b1(Pos,Data,CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = indexed_2_address(pos_inc2(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_w/1,
						New_Pos
					).

%%-----------------------------------------------------------------------------
cmpx_extended_bc(Pos, Data, CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = indexed_2_address(pos_inc(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_x/1,
						New_Pos
					).

%%-----------------------------------------------------------------------------
cmpy_extended_00bc(Pos, Data, CPU_Data) -> 

	{Memory_Data, New_CPU_Data, New_Pos} = indexed_2_address(pos_inc2(Pos),Data,CPU_Data),

	cmp_generic_part(
						Pos,
						Data,
						New_CPU_Data,
						Memory_Data,
						fun cpu_get_y/1,
						New_Pos
					).

%%----Unit_Tested-------------------------------------------------------------------------
cmpr_immediate_1037(Pos, Data, CPU_Data) ->

	{Memory_Data, New_CPU_Data, New_Pos} 	= immediate_1_address(pos_inc2(Pos),Data,CPU_Data),

	{Source_Reg, Destination_Reg} 			= decode_inter_register_post_byte(Memory_Data),

	Destination_Size 						= get_size_of_register(Destination_Reg),

	Source_Value 							= decode_inter_register_actual_source(Source_Reg,Destination_Size,New_CPU_Data),

	Destination_Value 						= get_register_from_name(Destination_Reg,New_CPU_Data),

	{_Carry,_Result,Flags} 					= binary_logic:generic_subtraction_with_flags(Destination_Value,Source_Value),

	Final_CPU 								= cpu_perform_actions(
																	[
																		?UPDATE_FLAGS(Flags),
																		?SET_PC(New_Pos)
																	],
																	New_CPU_Data
																 ),

	{Data, Final_CPU}.

%%----Unit_Tested-------------------------------------------------------------------------
com_generic_part(
					_Pos,
					Data,
					New_CPU_Data,
					Get_Reg_Fun,
					Set_Reg_Fun,
					New_Pos
				) ->

	Reg 							= Get_Reg_Fun(New_CPU_Data),

	Result 						 	= binary_logic:complement_binary(Reg),

	Flags 							= generate_n_z_flags_map(Result),

	Final_CPU 						= cpu_perform_actions(
														 	[
														 		fun(CPU) -> Set_Reg_Fun(Result,CPU) end,
														 		?UPDATE_FLAGS(Flags),
														 		?SET_CC_C(<<1:?SIZE_CC_C>>), % always set
														 		?SET_CC_V(<<0:?SIZE_CC_V>>), % always cleared
														 		?SET_PC(New_Pos)
														 	],
														 	New_CPU_Data
														 ),

	{Data, Final_CPU}.

%%----Unit_Tested--------------------------------------------------------------
coma_inherent_43(Pos, Data, CPU_Data) -> 

	com_generic_part(
						Pos,
						Data,
						CPU_Data,
						fun cpu_get_a/1,
						fun cpu_set_a/2,
						pos_inc(Pos)
					).

%%----Unit_Tested--------------------------------------------------------------
comb_inherent_53(Pos, Data, CPU_Data) -> 

	com_generic_part(
						Pos,
						Data,
						CPU_Data,
						fun cpu_get_b/1,
						fun cpu_set_b/2,
						pos_inc(Pos)
					).

%%----Unit_Tested--------------------------------------------------------------
comd_inherent_1043(Pos, Data, CPU_Data) ->
	
	com_generic_part(
						Pos,
						Data,
						CPU_Data,
						fun cpu_get_d/1,
						fun cpu_set_d/2,
						pos_inc2(Pos)
					).

%%----Unit_Tested--------------------------------------------------------------
come_inherent_1143(Pos, Data, CPU_Data) -> 
	
	com_generic_part(
						Pos,
						Data,
						CPU_Data,
						fun cpu_get_e/1,
						fun cpu_set_e/2,
						pos_inc2(Pos)
					).

%%----Unit_Tested--------------------------------------------------------------
comf_inherent_1153(Pos, Data, CPU_Data) -> 

	com_generic_part(
						Pos,
						Data,
						CPU_Data,
						fun cpu_get_f/1,
						fun cpu_set_f/2,
						pos_inc2(Pos)
					).

%%-----------------------------------------------------------------------------
comw_inherent_1053(Pos, Data, CPU_Data) -> 

	com_generic_part(
						Pos,
						Data,
						CPU_Data,
						fun cpu_get_w/1,
						fun cpu_set_w/2,
						pos_inc2(Pos)
					).

%%-----------------------------------------------------------------------------
com_memory_direct_03(PC, Data, CPU_Data) -> ok.

	% Pos 	= pos_inc(PC),
	% direct_1_address(Pos, Data, CPU_Data),

	% ram_64k:set_byte_pos(
	% 						Data,
	% 						Result,
	% 						Designated_Memory_Location
	% 					  )

%%-----------------------------------------------------------------------------
com_memory_indexed_63(PC, Data, CPU_Data) -> ok.
%%-----------------------------------------------------------------------------
com_memory_extended_73(PC, Data, CPU_Data) -> ok.

