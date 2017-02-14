%%%-------------------------------------------------------------------
%%% @author henglinli@gmail.com <chronos@localhost>
%%% @copyright (C) 2017, system_user
%%% @doc
%%%
%%% @end
%%% Created :  1 Feb 2017 by henglinli@gmail.com <chronos@localhost>
%%%-------------------------------------------------------------------
-define(VERSION, 16#5).
%%
-define(HELLO_NO_AUTHENTICATION, 16#0).
-define(HELLO_NOT_ACCEPTED, 16#ff).
%%
-define(RESERVED, 16#0).
%%
-define(CMD_CONNECT, 16#1).
-define(CMD_BIND, 16#2).
-define(CMD_UDP, 16#3).
%%
-define(ATYP_IPV4, 16#1).
-define(ATYP_ADDR, 16#3).
-define(ATYP_IPV6, 16#4).
%%
-define(REP_SUCCESS, 16#0).
-define(REP_FAILURE, 16#1).
-define(REP_DISALLOWED, 16#2).
-define(REP_NETWORK, 16#3).
-define(REP_HOST, 16#4).
-define(REP_REFUSED, 16#5).
-define(REP_TTL_TIMEOUT, 16#6).
-define(REP_UNKNOWN_COMMAND, 16#7).
-define(REP_UNKNOWN_ATYP, 16#8).
-define(REP_UNDEFINED, 16#9).

