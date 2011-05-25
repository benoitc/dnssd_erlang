%% -------------------------------------------------------------------
%%
%% Copyright (c) 2011 Andrew Tunnell-Jones. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
%% @doc
%% This module is the main interface to the application.
%%
%% If successful, the enumerate, browse, resolve and register functions all
%% return {ok, Ref}. Results will be sent to the process that started the
%% operation via a message of the form {dnssd, Ref, Result}. To stop the
%% operation pass Ref to stop/1. The operation will also be stopped if the
%% calling process exists.
%%
%% @end
-module(dnssd).
-export([start/0, stop/0, stop/1]).
-export([results/1]).
-export([enumerate/1]).
-export([browse/1, browse/2]).
-export([resolve/3, resolve_sync/3, resolve_sync/4]).
-export([register/2, register/3, register/4, register/6]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(APP_NAME, ?MODULE).
-define(IS_LIST_OR_BIN(Term), (is_list(Term) orelse is_binary(Term))).

%%--------------------------------------------------------------------
%% @doc Start the DNSSD application
%% @spec start() -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
start() -> application:start(?APP_NAME).

%%--------------------------------------------------------------------
%% @doc Stop the DNSSD application
%% @spec stop() -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
stop() -> application:stop(?APP_NAME).

%%--------------------------------------------------------------------
%% @doc Stop a DNSSD operation
%% @spec stop(Ref) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
stop(Ref) when is_reference(Ref) -> dnssd_server:stop(Ref).

%%--------------------------------------------------------------------
%% @doc Retrieve the current results of an operation
%% @spec results(Ref) -> {ok, Results} | {error, Error}
%% @end
%%--------------------------------------------------------------------
results(Ref) when is_reference(Ref) ->
    dnssd_server:results(Ref).

%%--------------------------------------------------------------------
%% @doc Enumerate browse or registration domains
%% @spec enumerate(Type) -> {ok, Ref} | {error, Error}
%% @end
%%--------------------------------------------------------------------
enumerate(Type) when Type =:= reg orelse Type =:= browse ->
    dnssd_server:enumerate(Type).

%%--------------------------------------------------------------------
%% @doc Browse for services.
%% @spec browse(Type) -> {ok, Ref} | {error, Error}
%% @equiv browse(Type, <<>>)
%% @end
%%--------------------------------------------------------------------
browse(Type) -> browse(Type, <<>>).

%%--------------------------------------------------------------------
%% @doc Browse for services. If domain is empty, browse all domains.
%% @spec browse(Type, Domain) -> {ok, Ref} | {error, Error}
%% @end
%%--------------------------------------------------------------------
browse(Type, Domain) when is_binary(Type), is_binary(Domain) ->
    case ensure_safe_type(Type) of
	Error when is_tuple(Error) -> Error;
	SafeType ->
	    dnssd_server:browse(SafeType, Domain)
    end;
browse(Type, Domain) when ?IS_LIST_OR_BIN(Type), ?IS_LIST_OR_BIN(Domain) ->
    browse(iolist_to_binary(Type), iolist_to_binary(Domain)).

%%--------------------------------------------------------------------
%% @doc Resolve a service instance.
%% @spec resolve(Name, Type, Domain) ->
%%           {ok, Ref} | {error, Error}
%% @end
%%--------------------------------------------------------------------
resolve(Name, Type, Domain)
  when is_binary(Name), is_binary(Type), is_binary(Domain) ->
    case ensure_safe_type(Type) of
	Error when is_tuple(Error) -> Error;
	SafeType ->
	    dnssd_server:resolve(Name, SafeType, Domain)
    end;
resolve(Name, Type, Domain)
  when ?IS_LIST_OR_BIN(Name), ?IS_LIST_OR_BIN(Type), ?IS_LIST_OR_BIN(Domain) ->
    resolve(iolist_to_binary(Name),
	    iolist_to_binary(Type),
	    iolist_to_binary(Domain)).

%%--------------------------------------------------------------------
%% @doc Returns the first result from resolving a service instance and
%%      then cancels the operation. Times out after 5 seconds.
%% @spec resolve_sync(Name, Type, Domain) ->
%%       {ok, {Hostname, Port, Txt}} | {error, Error}
%% @equiv resolve_sync(Name, Type, Domain, 5000)
%%--------------------------------------------------------------------
resolve_sync(Name, Type, Domain)
  when ?IS_LIST_OR_BIN(Name), ?IS_LIST_OR_BIN(Type), ?IS_LIST_OR_BIN(Domain) ->
    resolve_sync(Name, Type, Domain, 5000).

%%--------------------------------------------------------------------
%% @doc Returns the first result from resolving a service instance and
%%      then cancels the operation. Times out after Timeout milliseconds.
%% @spec resolve_sync(Name, Type, Domain, Timeout) ->
%%      {ok, {Hostname, Port, Txt}} | {error, Error}
%%--------------------------------------------------------------------
resolve_sync(Name, Type, Domain, Timeout)
  when ?IS_LIST_OR_BIN(Name), ?IS_LIST_OR_BIN(Type), ?IS_LIST_OR_BIN(Domain),
       is_integer(Timeout) andalso Timeout > 0 ->
    case resolve(Name, Type, Domain) of
	{ok, Ref} ->
	    receive
		{dnssd, Ref, {resolve, Result}} ->
		    ok = stop(Ref),
		    ok = flush(Ref),
		    {ok, Result};
		{dnssd, Ref, crash} ->
		    {error, crash}
	    after
		Timeout ->
		    ok = stop(Ref),
		    ok = flush(Ref),
		    {error, timeout}
	    end;
	{error, Error} ->
	    {error, Error}
    end.

%%--------------------------------------------------------------------
%% @doc Register a service.
%% @spec register(Type, Port) -> {ok, Ref} | {error, Error}
%% @equiv register(Type, Port, <<>>)
%% @end
%%--------------------------------------------------------------------
register(Type, Port) -> register(<<>>, Type, Port).

%%--------------------------------------------------------------------
%% @doc Register a service.
%%      If C is an integer, A is Name, B is Type and C is Port.
%%      If B is an integer, A is Type, B is Port and C is Txt.
%% @spec register(A, B, C) -> {ok, Ref} | {error, Error}
%% @equiv register(Name, Type, Port, Txt)
%% @end
%%--------------------------------------------------------------------
register(Name, Type, Port) when is_integer(Port) ->
    register(Name, Type, Port, <<>>);
register(Type, Port, Txt) when is_integer(Port) ->
    register(<<>>, Type, Port, Txt).

%%--------------------------------------------------------------------
%% @doc Register a service.
%% @spec register(Name, Type, Port, Txt) ->
%%           {ok, Ref} | {error, Error}
%% @equiv register(Name, Type, Port, Txt, <<>>, <<>>)
%% @end
%%--------------------------------------------------------------------
register(Name, Type, Port, Txt) ->
    register(Name, Type, Port, Txt, <<>>, <<>>).

%%--------------------------------------------------------------------
%% @doc Register a service.
%% @spec register(Name, Type, Port, Txt, Host, Domain) ->
%%           {ok, Ref} | {error, Error}
%% @equiv register(Name, Type, Port, Txt, <<>>, <<>>)
%% @end
%%--------------------------------------------------------------------
%% Coerce types if we can...
register(Name, Type, Port, Txt, Host, Domain)
  when is_list(Name) ->
    NameBin = iolist_to_binary(Name),
    register(NameBin, Type, Port, Txt, Host, Domain);
register(Name, Type, Port, Txt, Host, Domain)
  when is_list(Type) ->
    TypeBin = iolist_to_binary(Type),
    register(Name, TypeBin, Port, Txt, Host, Domain);
register(Name, Type, Port, Txt, Host, Domain)
  when is_list(Domain) ->
    DomainBin = iolist_to_binary(Domain),
    register(Name, Type, Port, Txt, Host, DomainBin);
register(Name, Type, Port, Txt, Host, Domain)
  when is_list(Host) ->
    HostBin = iolist_to_binary(Host),
    register(Name, Type, Port, Txt, HostBin, Domain);
%% Start catching errors
register(Name, _Type, _Port, _Txt, _Host, _Domain)
  when is_binary(Name), byte_size(Name) > 63 ->
    {error, bad_name};
register(_Name, Type, _Port, _Txt, _Host, _Domain)
  when is_binary(Type), byte_size(Type) < 7 ->
    {error, bad_type};
register(_Name, _Type, _Port, _Txt, _Host, Domain)
  when is_binary(Domain), byte_size(Domain) > 255 ->
    {error, bad_domain};
register(_Name, _Type, _Port, _Txt, Host, _Domain)
  when is_binary(Host), (byte_size(Host) > 256) ->
    {error, bad_hostname};
register(_Name, _Type, Port, _Txt, _Host, _Domain)
  when not is_integer(Port) orelse (Port < 0 orelse Port > 16#FFFF) ->
    %% Setting port as 0 creates a placeholder
    {error, bad_port};
register(Name, Type, Port, Txt, Host, Domain)
  when is_binary(Name),
       is_binary(Type),
       is_integer(Port),
       (is_list(Txt) orelse is_binary(Txt)),
       is_binary(Host),
       is_binary(Domain) ->
    %% Try and avoid going all the way to the driver to find a bad arg
    case ensure_safe_txt(Txt) of
	Error when is_tuple(Error) -> Error;
	SafeTxt ->
	    case ensure_safe_type(Type) of
		Error when is_tuple(Error) -> Error;
		SafeType ->
		    dnssd_server:register(
		      Name, SafeType, Domain, Host, Port, SafeTxt)
	    end
    end.

ensure_safe_type(<<$_, S, _/binary>> = RegType)
  when is_binary(RegType), byte_size(RegType) > 6, S =/= $_ ->
    case parse_type_t(RegType) of
	{Type, ProtoBin}
	  when Type =/= error ->
	    case parse_type_p(ProtoBin) of
		{Proto, Subtype}
		  when Proto =/= error ->
		    case valid_subtype(Subtype) of
			true ->
			    iolist_to_binary([$_, Type, "._", Proto, Subtype]);
			false ->
			    {error, bad_type}
		    end;
		Error -> Error
	    end;
	Error -> Error
    end;
ensure_safe_type(_) ->
    {error, bad_type}.

parse_type_t(<<$_, Protocol/binary>>) ->
    parse_type_t(<<>>, Protocol).

parse_type_t(Type, <<$., Rest/binary>>)
  when byte_size(Type) > 0 ->
    {Type, Rest};
parse_type_t(Type, <<S, Rest/binary>>)
  when S =/= $. ->
    Size = byte_size(Type),
    parse_type_t(<<Type:Size/binary, S>>, Rest);
parse_type_t(_,_) -> {error, bad_type}.

parse_type_p(<<$_, Proto:3/binary, SubtypeBin/binary>>)
  when Proto =:= <<"udp">> orelse Proto =:= <<"tcp">> ->
    {Proto, SubtypeBin};
parse_type_p(_) -> {error, bad_type}.

valid_subtype(<<>>) ->
    true;
valid_subtype(<<$., Subtype/binary>>) ->
    valid_subtype(Subtype);
valid_subtype(Subtype) ->
    valid_subtype(0, Subtype).

valid_subtype(0, <<$,, S, Rest/binary>>) when S =/= $. ->
    valid_subtype(2, Rest);
valid_subtype(0, _) -> false;
valid_subtype(I, <<>>) when I > 1, I < 65 -> true;
valid_subtype(I, <<$,, _/binary>> = Rest) when I > 1 ->
    valid_subtype(0, Rest);
valid_subtype(I, <<S, Rest/binary>>) when S =/= $., I < 64 ->
    valid_subtype(I + 1, Rest);
valid_subtype(_, _) ->
    false.

ensure_safe_txt(Txt) when is_list(Txt) ->
    try
	Strings = lists:map(fun txt_pair_to_bin/1, Txt),
	Bin = << <<(byte_size(String)), String/binary>> || String <- Strings >>,
	ensure_safe_txt(Bin)
    catch throw:bad_txt ->
	    {error, bad_txt}
    end;
ensure_safe_txt(Txt) when is_binary(Txt) andalso byte_size(Txt) < 16#FFFF ->
    case string_len_match(Txt) of
	true -> Txt;
	false -> {error, bad_txt}
    end;
ensure_safe_txt(_) -> {error, bad_txt}.

string_len_match(<<>>) -> true;
string_len_match(<<Size, _:Size/binary, Rest/binary>>) ->
    string_len_match(Rest);
string_len_match(_) -> false.

txt_pair_to_bin(Bin) when is_binary(Bin) andalso byte_size(Bin) =< 255 -> Bin;
txt_pair_to_bin(List) when is_list(List) ->
    txt_pair_to_bin(iolist_to_binary(List));
txt_pair_to_bin(Atom) when is_atom(Atom) ->
    txt_pair_to_bin(atom_to_binary(Atom, latin1));
txt_pair_to_bin({Atom, Value})
  when is_atom(Atom) ->
    Key = atom_to_binary(Atom, latin1),
    txt_pair_to_bin({Key, Value});
txt_pair_to_bin({Key, Atom})
  when is_atom(Atom) ->
    Value = atom_to_binary(Atom, latin1),
    txt_pair_to_bin({Key, Value});
txt_pair_to_bin({List, Value})
  when is_list(List) ->
    Key = iolist_to_binary(List),
    txt_pair_to_bin({Key, Value});
txt_pair_to_bin({Key, List})
  when is_list(List) ->
    Value = iolist_to_binary(List),
    txt_pair_to_bin({Key, Value});
txt_pair_to_bin({Key, Value})
  when is_binary(Key) andalso is_binary(Value) andalso
       (byte_size(Key) + byte_size(Value) < 255) ->
    %% character string is 255 chars max, "=" snags one
    <<Key/binary, $=, Value/binary>>;
txt_pair_to_bin(_) -> throw(bad_txt).

flush(Ref) when is_reference(Ref) ->
    receive {dnssd, Ref, _} ->
	    flush(Ref)
    after 0 -> ok end.

-ifdef(TEST).

ensure_safe_text_success_test_() ->
    Keys = [key, "key", <<"key">>],
    Values = [value, "value", <<"value">>, <<>>, ""],
    Flat = Keys ++ Values,
    Pairs = [ {Key, Value} || Key <- Keys, Value <- Values ],
    Cases = [lists:duplicate(255, $a)] ++ Flat ++ Pairs,
    ?_assert(is_binary(ensure_safe_txt(Cases))).

ensure_safe_text_fail_test_() ->
    Ax128 = lists:duplicate(128, $a),
    Ax256 = lists:duplicate(256, $a),
    Cases = [ [{Ax128,Ax128}],
	      [Ax256],
	      [{key, 123}],
	      [{key, make_ref()}],
	      <<1>>,
	      1 ],
    [ ?_assertEqual({error, bad_txt}, ensure_safe_txt(Case)) || Case <- Cases ].

ensure_safe_type_success_test_() ->
    Protocols = [ "_udp", "_tcp" ],
    Max = "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghij",
    Min = "a",
    ServiceTypes = [ [$_|Min], [$_|Max] ],
    BaseSubtypes = [ "", [$,|Min], [$,|Max] ++ "k"],
    MultiSubtype = iolist_to_binary(BaseSubtypes),
    Subtypes = [MultiSubtype|BaseSubtypes],
    Cases = [ iolist_to_binary([ServiceType, $., Protocol, Subtype])
	      || ServiceType <- ServiceTypes,
		 Protocol <- Protocols,
		 Subtype <- Subtypes ],
    [ {Case, ?_assertEqual(Case, ensure_safe_type(Case))} || Case <- Cases ].

ensure_safe_type_fail_test_() ->
    Cs = <<"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijkl">>,
    Cases = [ <<>>,
	      <<"_.......">>,
	      <<"_te.st._udp">>,
	      <<"_test._udp,a,.">>,
	      <<"_test._udp,", Cs/binary>> ],
    [ {Case, ?_assertEqual({error, bad_type}, ensure_safe_type(Case))}
      || Case <- Cases ].

-endif.
