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
%% @private
-module(dnssd_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([stop/1, enumerate/1, browse/2, resolve/3, register/6]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {subs = [], ops = []}).

-record(sub, {ref, mon_ref, pid, op_id}).
-record(op, {id, type, arg, pid}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Stop the operation
%% @spec stop(Ref) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
stop(Ref) when is_reference(Ref) ->
    gen_server:call(?SERVER, {stop, Ref}).

%%--------------------------------------------------------------------
%% @private
%% @doc Enumerate browse or registration domains
%% @spec enumerate(browse | reg) -> {ok, Ref, Results} | {error, Error}
%% @end
%%--------------------------------------------------------------------
enumerate(Type) when Type =:= reg orelse Type =:= browse ->
    gen_server:call(?SERVER, {start, {enumerate, Type}}).

%%--------------------------------------------------------------------
%% @private
%% @doc Browse for instances of a service
%% @spec browse(Type, Domain) -> {ok, Ref, Results} | {error, Error}
%% @end
%%--------------------------------------------------------------------
browse(Type, Domain) when is_binary(Type) andalso is_binary(Domain) ->
    gen_server:call(?SERVER, {start, {browse, Type, Domain}}).

%%--------------------------------------------------------------------
%% @private
%% @doc Resolve a service instance
%% @spec resolve(Name, Type, Domain) -> {ok, Ref, Results} | {error, Error}
%% @end
%%--------------------------------------------------------------------
resolve(Name, Type, Domain)
  when is_binary(Name) andalso is_binary(Type) andalso is_binary(Domain) ->
    gen_server:call(?SERVER, {start, {resolve, Name, Type, Domain}}).

%%--------------------------------------------------------------------
%% @private
%% @doc Register a service instance
%% @spec register(Name, Type, Domain, Host, Port, Txt) ->
%%           {ok, Ref, Results} | {error, Error}
%% @end
%%--------------------------------------------------------------------
register(Name, Type, Domain, Host, Port, Txt)
when is_binary(Name), is_binary(Type), is_binary(Domain), is_binary(Host),
     is_integer(Port), is_binary(Txt) ->
    Req = {start, {register, Name, Type, Domain, Host, Port, Txt}},
    gen_server:call(?SERVER, Req).

%%--------------------------------------------------------------------
%% @private
%% @doc Starts the server
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({stop, Ref}, _From, #state{subs = Subs, ops = Ops} = State)
  when is_reference(Ref) ->
    case lists:keytake(Ref, #sub.ref, Subs) of
	{value, #sub{ref = Ref,
		     mon_ref = MonRef,
		     op_id = OpId}, NewSubs} ->
	    erlang:demonitor(MonRef),
	    case lists:keymember(OpId, #sub.op_id, NewSubs) of
		true ->
		    NewState = State#state{subs = NewSubs},
		    {reply, ok, NewState};
		false ->
		    case lists:keytake(OpId, #op.id, Ops) of
			{value, #op{id = OpId, pid = OpPid}, NewOps} ->
			    dnssd_drv:stop(OpPid),
			    NewState = State#state{subs = NewSubs,
						   ops = NewOps},
			    {reply, ok, NewState};
			false ->
			    NewState = State#state{subs = NewSubs},
			    {reply, ok, NewState}
		    end
	    end;
	false ->
	    Reply = {error, unknown_ref},
	    {reply, Reply, State}
    end;
handle_call({start, Op}, {ClientPid, _Tag} = Client,
	    #state{subs = Subs, ops = Ops} = State) ->
    Type = element(1, Op),
    OpId = erlang:phash2(Op),
    Ref = make_ref(),
    MonRef = erlang:monitor(process, ClientPid),
    NewSub = #sub{ref = Ref, mon_ref = MonRef, pid = ClientPid, op_id = OpId},
    NewSubs = [NewSub|Subs],
    case lists:keyfind(OpId, #op.id, Ops) of
	#op{id = OpId, type = Type, pid = OpPid} ->
	    {ok, Results} = dnssd_drv:results(OpPid),
	    Reply = {ok, Ref},
	    NewState = State#state{subs = NewSubs},
	    gen_server:reply(Client, Reply),
	    [ ClientPid ! {dnssd, Ref, case Type of
					   resolve -> {Type, Result};
					   Type -> {Type, add, Result}
				       end}
	      || Result <- Results ],
	    {noreply, NewState};
	false ->
	    case dnssd_drv:start_link(Op) of
		{ok, OpPid} ->
		    Reply = {ok, Ref},
		    NewOp = #op{id = OpId,
				type = Type,
				arg = Op,
				pid = OpPid},
		    NewOps = [NewOp|Ops],
		    NewState = State#state{subs = NewSubs, ops = NewOps},
		    {reply, Reply, NewState};
		{error, _Error} = Reply ->
		    {reply, Reply, State}
	    end
    end;
handle_call(Request, _From, State) ->
    error_logger:info_msg(?MODULE_STRING " ~p ignoring call: ~p~n",
			  [ self(), Request]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    error_logger:info_msg(?MODULE_STRING " ~p ignoring cast: ~p~n",
			  [ self(), Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({dnssd_drv, DrvPid, Message},
	    #state{subs = Subs, ops = Ops} = State) when is_pid(DrvPid) ->
    case lists:keyfind(DrvPid, #op.pid, Ops) of
	#op{id = OpId} ->
	    [ SubPid ! {dnssd, Ref, Message}
	      || #sub{pid = SubPid, ref = Ref, op_id = SubOpId} <- Subs,
		 SubOpId =:= OpId ];
	false ->
	    error_logger:error_msg(
	      ?MODULE_STRING " ~p discarded dnssd_drv message from unknown pid "
	      "~p. Message: ~p~n", [self(), DrvPid, Message]
	     )
    end,
    {noreply, State};
handle_info({'DOWN', MonRef, process, Pid, _Reason},
	    #state{subs = Subs, ops = Ops} = State) ->
    case lists:keytake(MonRef, #sub.mon_ref, Subs) of
	{value, #sub{mon_ref = MonRef,
		     pid = Pid,
		     op_id = OpId}, NewSubs} ->
	    case lists:keymember(OpId, #sub.op_id, NewSubs) of
		true ->
		    NewState = State#state{subs = NewSubs},
		    {noreply, NewState};
		false ->
		    case lists:keytake(OpId, #op.id, Ops) of
			{value, #op{id = OpId, pid = OpPid}, NewOps} ->
			    dnssd_drv:stop(OpPid),
			    NewState = State#state{subs = NewSubs,
						   ops = NewOps},
			    {noreply, NewState};
			false ->
			    NewState = State#state{subs = NewSubs},
			    {noreply, NewState}
		    end
	    end;
	_ ->
	    {noreply, State}
    end;
handle_info({'EXIT', Pid, Reason}, #state{subs = Subs, ops = Ops} = State) ->
    if Reason =/= normal ->
	    error_logger:error_msg(
	      ?MODULE_STRING " ~p notified of non-normal exit of ~p:~n~p~n",
	      [ self(), Pid, Reason]
	     );
       true -> ok end,
    case lists:keytake(Pid, #op.pid, Ops) of
	{value, #op{id = OpId, pid = Pid}, NewOps} ->
	    case lists:partition(fun(#sub{op_id = SubOpId}) ->
					 SubOpId =:= OpId
				 end, Subs) of
		{[], Subs} ->
		    %% No clients affected
		    NewState = State#state{ops = NewOps},
		    {noreply, NewState};
		{DefunctSubs, NewSubs} ->
		    %% There was active clients - shouldn't happen
		    %% regardless of Reason so tell clients there
		    %% was a crash
		    [ SubPid ! {dnssd, Ref, crash}
		      || #sub{pid = SubPid,
			      ref = Ref,
			      op_id = SubOpId} <- DefunctSubs,
			 SubOpId =:= OpId ],
		    NewState = State#state{subs = NewSubs, ops = NewOps},
		    {noreply, NewState}
	    end;
	_ ->
	    {noreply, State}
    end;
handle_info(Info, State) ->
    error_logger:error_msg(?MODULE_STRING " ~p discarded message: ~p~n",
			  [ self(), Info]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
