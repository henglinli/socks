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
-export([parse_methods/1, parse_skip/2]).
-export([new_noauth/0, new_auth/0, new_unaccept/0]).
-export([parse_auth/1]).
-export([parse_command/1, parse_ipv4/1,
         parse_ipv6/1, parse_addr/1]).
-export([new_ipv4/1, new_ipv6/1, new_addr/1]).
-export([new_success/0, new_failure/0]).
-export([have_auth/1, have_noauth/1]).
%%
-include("socks.hrl").
%%%===================================================================
%%% API
%%%===================================================================
-spec parse_methods(Packet :: binary()) -> error |
                                           ordsets:ordset(0..255).
parse_methods(<<?VERSION:8/big-unsigned-integer,
                N:8/big-unsigned-integer,
                Rest/binary>>) ->
case N == byte_size(Rest) of
    true ->
        parse_methods(Rest, N, ordsets:new());
    _Others ->
        error
end;
%%
parse_methods(_Packet) ->
    error.
%%
-spec parse_methods(Methods :: binary(), N :: 1..255,
                    MethodSet :: ordsets:set(0..255)) ->
                           ordsets:ordset(0..255).
%%
parse_methods(<<First:8/big-unsigned-integer>>, 1, Set) ->
    ordsets:add_element(First, Set);
%%
parse_methods(<<First:8/big-unsigned-integer,
                Rest/binary>>, N, Set) ->
    NewSet = ordsets:add_element(First, Set),
    parse_methods(Rest, N-1, NewSet).
%%
-spec parse_skip(Packet :: binary(), N :: integer()) ->
                        binary().
parse_skip(Packet, N) ->
    NBits = 8*N,
    <<_:NBits/binary, Rest/binary>> = Packet,
    Rest.

%%
-spec new_noauth() -> binary().
new_noauth() ->
    <<?VERSION:8/big-unsigned-integer,
      ?METHOD_NONE:8/big-unsigned-integer>>.
%%
-spec new_auth() -> binary().
new_auth() ->
    <<?VERSION:8/big-unsigned-integer,
      ?METHOD_PASSWORD:8/big-unsigned-integer>>.
%%
-spec new_unaccept() -> binary().
new_unaccept() ->
    <<?VERSION:8/big-unsigned-integer,
      ?METHOD_UNACCEPT:8/big-unsigned-integer>>.
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
-spec parse_ipv4(binary()) -> error |
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
-spec parse_addr(binary()) -> error | {string(), inet:port_number()}.
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
-spec parse_ipv6(binary()) -> error |
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
%%
-spec parse_auth(binary()) -> error | {string(), string()}.
parse_auth(<<?VERSION:8/big-unsigned-integer,
             Ulen:8/big-unsigned-integer,
             Rest/binary>>) ->
    case byte_size(Rest) > Ulen of
        false ->
            error;
        true ->
            Ubits = Ulen*8,
            <<User:Ubits/binary,
              Plen:8/big-unsigned-integer,
              Password/binary>> = Rest,
            case byte_size(Password) == Plen of
                false ->
                    error;
                true ->
                    {binary:bin_to_list(User),
                     binary:bin_to_list(Password)}
            end
    end;
%%
parse_auth(_Packet) ->
    error.
%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec new(Rep :: 0..255,
          inet:ipv4_address() | inet:ipv6_address() | binary(),
          inet:port_number()) -> binary().
%%
new(Rep, {A, B, C, D}, Port)->
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
new(Rep, {A, B, C, D, E, F, G, H}, Port) ->
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
new(Rep, Address, Port) ->
    <<?VERSION:8/big-unsigned-integer,
      Rep:8/big-unsigned-integer,
      ?RESERVED:8/big-unsigned-integer,
      ?ATYP_ADDR:8/big-unsigned-integer,
      Address/binary,
      Port:16/big-unsigned-integer>>.
%%
new_ipv4(Rep) ->
    new(Rep, {0,0,0,0}, 0).
%%
new_ipv6(Rep) ->
    new(Rep, {0,0,0,0,0,0,0,0}, 0).
%%
new_addr(Rep)->
    new(Rep, <<"localhost">>, 0).
%%
new_success() ->
    <<?AUTH_VER:8/big-unsigned-integer,
      0:8/big-unsigned-integer>>.
%%
new_failure()->
    <<?AUTH_VER:8/big-unsigned-integer,
      1:8/big-unsigned-integer>>.
%%
have_auth(MethodSet) ->
    ordsets:is_element(?METHOD_UNACCEPT, MethodSet).
%%
have_noauth(MethodSet) ->
    ordsets:is_element(?METHOD_NONE, MethodSet).

