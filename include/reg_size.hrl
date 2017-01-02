%%=========================================================================================
%%  Copyright (C) C.Casilli 2016
%%=========================================================================================
%
%   Project:        POLYBUS
%   Subproject:     reg_size
%   Component:      reg_size
%   Author:         Chris Casilli
%   Creation Date:  15 August 2016
%   File Name:      reg_size.hrl
%
%%=========================================================================================
%  reg_size
%%=========================================================================================

-define(SIZE_Q, 				32).
-define(SIZE_D, 				16).
-define(SIZE_W, 				16).
-define(SIZE_A, 				8).
-define(SIZE_B, 				8).
-define(SIZE_E,					8).
-define(SIZE_F,					8).

-define(SIZE_X, 				16).
-define(SIZE_Y, 				16).
-define(SIZE_U, 				16).
-define(SIZE_S, 				16).
-define(SIZE_PC, 				16).
-define(SIZE_V, 				16).
-define(SIZE_ZERO, 				16).

-define(SIZE_DP, 				8).

-define(SIZE_CC, 				8).
-define(SIZE_CC_E, 				1).
-define(SIZE_CC_F, 				1).
-define(SIZE_CC_H, 				1).
-define(SIZE_CC_I,				1).
-define(SIZE_CC_N, 				1).
-define(SIZE_CC_Z, 				1).
-define(SIZE_CC_V, 				1).
-define(SIZE_CC_C, 				1).

-define(SIZE_MD, 				8).
-define(SIZE_MD_DIV0, 			1).
-define(SIZE_MD_IL,             1).
-define(SIZE_MD_FM, 			1).
-define(SIZE_MD_NM, 			1).

-define(SIZE_RESET, 			1).
-define(SIZE_NMI, 				1).
-define(SIZE_SWI, 				1).
-define(SIZE_IRQ, 				1).
-define(SIZE_FIRQ, 				1).
-define(SIZE_SWI2, 				1).
-define(SIZE_SWI3, 				1).
-define(SIZE_RESERVED, 			1).
-define(SIZE_HALT, 				1).

-define(SIZE_ADDRESS, 			16).