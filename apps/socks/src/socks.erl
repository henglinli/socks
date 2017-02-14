%%%-------------------------------------------------------------------
%%% @author lihenglin <lee@Alpha>
%%% @copyright (C) 2017, lihenglin
%%% @doc
%%%
%%% @end
%%% Created : 21 Jan 2017 by lihenglin <lee@Alpha>
%%%-------------------------------------------------------------------
-module(socks).
%%
-author('henglinli@gmail.com').
%% API
-export([parse_version/1, parse_skip/2, parse_methods/2, noauth/0,
         unaccept/0]).
-export([parse_command/1, parse_ipv4/1,
         parse_ipv6/1, parse_addr/1]).
-export([reply/3, reply_ipv4/1]).
%%
-include("socks.hrl").
%%%===================================================================
%%% API
%%%===================================================================
-spec parse_version(Packet :: binary()) -> error |
                           {N :: 1..255, Rest :: binary()}.
parse_version(<<?VERSION:8/big-unsigned-integer,
                N:8/big-unsigned-integer,
                Methods/binary>>) ->
    case N == byte_size(Methods) of
        true ->
            {N, Methods};
        false ->
            error
    end;
%%
parse_version(_Packet) ->
    error.
%%
-spec parse_skip(Packet :: binary(), N :: integer()) ->
                  binary().
parse_skip(Packet, N) ->
    NBits = 8*N,
    <<_:NBits/binary, Rest/binary>> = Packet,
    Rest.
%%
-spec parse_methods(Methods :: binary(), N :: 1..255) ->
                           {sets:set(), binary()}.
parse_methods(Methods, N) ->
    parse_methods(Methods, N, sets:new()).
%%
-spec parse_methods(Methods :: binary(), N :: 1..255,
                    MethodSet :: sets:set()) ->
                           {sets:set(), binary()}.
parse_methods(<<First:8/big-unsigned-integer, Rest/binary>>,
              1, MethodSet) ->
    {sets:add_element(First, MethodSet), Rest};
parse_methods(<<First:8/big-unsigned-integer, Rest/binary>>,
             N, MethodSet) ->
    parse_methods(Rest, N-1,
                  sets:add_element(First, MethodSet)).
%%
-spec noauth() -> binary().
noauth() ->
    <<?VERSION:8/big-unsigned-integer,
      ?HELLO_NO_AUTHENTICATION:8/big-unsigned-integer>>.
-spec unaccept() -> binary().
unaccept() ->
    <<?VERSION:8/big-unsigned-integer,
      ?HELLO_NOT_ACCEPTED:8/big-unsigned-integer>>.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-spec parse_command(Packet :: binary()) ->
                           error |
                           {connect | bind | udp,
                            integer(), binary()}.
parse_command(<<?VERSION:8/big-unsigned-integer,
                Command:8/big-unsigned-integer,
                ?RESERVED:8/big-unsigned-integer,
                ATYP:8/big-unsigned-integer,
                Rest/binary>>) ->
case Command of
    ?CMD_CONNECT ->
        {connect, ATYP, Rest};
    ?CMD_BIND ->
        {bind, ATYP, Rest};
    ?CMD_UDP ->
        {udp, ATYP, Rest};
    _Others ->
        error
end;
%%
parse_command(_Else) ->
    error.
%% ipv4
-define(IPV4_PORT_LEN, 6).
-spec parse_ipv4(binary()) ->
                        {inet:ipv4_address(), inet:port_number()}.
parse_ipv4(<<D:8/big-unsigned-integer,
             C:8/big-unsigned-integer,
             B:8/big-unsigned-integer,
             A:8/big-unsigned-integer,
             Port:16/big-unsigned-integer>>) ->
    {{A, B, C, D}, Port};
%%
parse_ipv4(_Address) ->
    error.
%% address
-spec parse_addr(binary()) -> {string(), inet:port_number()}.
%%
parse_addr(<<Length:8/big-unsigned-integer, Rest/binary>>) ->
    case Length == (byte_size(Rest) - 2) of
        true ->
            <<Address:Length/binary,
              Port:16/big-unsigned-integer>> = Rest,
            {binary:bin_to_list(Address), Port};
        false ->
            error
    end;
%%
parse_addr(_Packet) ->
    error.
%% ipv6
-spec parse_ipv6(binary()) ->
                        {inet:ipv6_address(), inet:port_number()}.
parse_ipv6(<<H:16/big-unsigned-integer,
             G:16/big-unsigned-integer,
             F:16/big-unsigned-integer,
             E:16/big-unsigned-integer,
             D:16/big-unsigned-integer,
             C:16/big-unsigned-integer,
             B:16/big-unsigned-integer,
             A:16/big-unsigned-integer,
             Port:16/big-unsigned-integer>>) ->
    {{A, B, C, D, E, F, G, H}, Port};
%%
parse_ipv6(_Packet) ->
    error.
%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec reply(Rep :: 0..255,
            inet:ipv4_address() | inet:ipv6_address() | binary(),
            inet:port_number()) -> binary().
%%
reply(Rep, {A, B, C, D}, Port)->
    <<?VERSION:8/big-unsigned-integer,
      Rep:8/big-unsigned-integer,
      ?RESERVED:8/big-unsigned-integer,
      ?ATYP_IPV4:8/big-unsigned-integer,
      D:8/big-unsigned-integer,
      C:8/big-unsigned-integer,
      B:8/big-unsigned-integer,
      A:8/big-unsigned-integer,
      Port:16/big-unsigned-integer>>;
%%
reply(Rep, {A, B, C, D, E, F, G, H}, Port) ->
    <<?VERSION:8/big-unsigned-integer,
      Rep:8/big-unsigned-integer,
      ?RESERVED:8/big-unsigned-integer,
      ?ATYP_IPV6:8/big-unsigned-integer,
      H:16/big-unsigned-integer,
      G:16/big-unsigned-integer,
      F:16/big-unsigned-integer,
      E:16/big-unsigned-integer,
      D:16/big-unsigned-integer,
      C:16/big-unsigned-integer,
      B:16/big-unsigned-integer,
      A:16/big-unsigned-integer,
      Port:16/big-unsigned-integer>>;
%%
reply(Rep, Address, Port) ->
    <<?VERSION:8/big-unsigned-integer,
      Rep:8/big-unsigned-integer,
      ?RESERVED:8/big-unsigned-integer,
      ?ATYP_ADDR:8/big-unsigned-integer,
      Address/binary,
      Port:16/big-unsigned-integer>>.
%%
reply_ipv4(Rep) ->
    reply(Rep, {0,0,0,0}, 0).

