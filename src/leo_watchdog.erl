%%======================================================================
%%
%% Leo Watchdog
%%
%% Copyright (c) 2012-2015 Rakuten, Inc.
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
%% @doc leo_watchdog
%% @reference
%% @end
%%======================================================================
-module(leo_watchdog).

-author('Yosuke Hara').

-behaviour(gen_server).

-include("leo_watchdog.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/4,
         stop/1,
         set_interval/2,
         update_property/3,
         suspend/1,
         resume/1,
         state/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          id :: atom(),
          callback_mod :: module(),
          properties = [] :: [{atom(), any()}],
          interval = ?DEF_WATCH_INTERVAL :: pos_integer(),
          is_suspending = false :: boolean()
         }).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Start the server
-spec(start_link(Id, CallbackMod, Props, IntervalTime) ->
             {ok,Pid} | ignore | {error,Error} when Id::atom(),
                                                    CallbackMod::module(),
                                                    Props::tuple(),
                                                    IntervalTime::pos_integer(),
                                                    Pid::pid(),
                                                    Error::{already_started,Pid} | term()).
start_link(Id, CallbackMod, Props, IntervalTime) ->
    %% Takes conflict execution measures
    timer:sleep(erlang:phash2(leo_date:clock(), timer:seconds(1))),
    gen_server:start_link({local, Id}, ?MODULE,
                          [Id, CallbackMod, Props, IntervalTime], []).


%% @doc Stop the server
-spec(stop(Id) ->
             ok when Id::atom()).
stop(Id) ->
    gen_server:call(Id, stop).


%% @doc Suspend the server
-spec(set_interval(Id, Interval) ->
             ok when Id::atom(),
                     Interval::pos_integer()).
set_interval(Id, Interval) ->
    gen_server:call(Id, {set_interval, Interval}).


%% @doc Suspend the server
-spec(update_property(Id, Item, Value) ->
             ok when Id::atom(),
                     Item::atom(),
                     Value::any()).
update_property(Id, Item, Value) ->
    gen_server:call(Id, {update_property, Item, Value}).


%% @doc Suspend the server
-spec(suspend(Id) ->
             ok when Id::atom()).
suspend(Id) ->
    gen_server:call(Id, suspend).


%% @doc Resume the server
-spec(resume(Id) ->
             ok when Id::atom()).
resume(Id) ->
    gen_server:call(Id, resume).


%% @doc Retrieve the state
-spec(state(Id) ->
             {ok, State}  when Id::atom(),
                               State::[{atom(), any()}]).
state(Id) ->
    gen_server:call(Id, state).


%%--------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------------------------------
%% @doc Initiates the server
init([Id, CallbackMod, Props, Interval]) ->
    Id = ets:new(Id, [named_table, set,
                      public, {read_concurrency, true}]),
    ok = CallbackMod:init(Props),
    {ok, #state{id = Id,
                callback_mod = CallbackMod,
                properties = Props,
                interval = Interval}, Interval}.

%% @doc gen_server callback - Module:handle_call(Request, From, State) -> Result
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

%% @doc Modify the interval
handle_call({set_interval, Interval},_From, State) ->
    {reply, ok, State#state{interval = Interval}, Interval};

%% @doc Update a property
handle_call({update_property, Item, Value},_From, #state{callback_mod = CallbackMod,
                                                properties = Props,
                                                interval = Interval} = State) ->
    Props_1 = erlang:apply(CallbackMod, update_property, [Item, Value, Props]),
    {reply, ok, State#state{properties = Props_1}, Interval};

%% @doc Suspend the server
handle_call(suspend,_From, State) ->
    {reply, ok, State#state{is_suspending = true}};

%% @doc Resume the server
handle_call(resume,_From, #state{interval = Interval,
                                 is_suspending = false} = State) ->
    {reply, ok, State, Interval};
handle_call(resume,_From, #state{interval = Interval} = State) ->
    {reply, ok, State#state{is_suspending = false}, Interval};

%% @doc Retrieve the state
handle_call(state,_From, #state{interval = Interval} = State) ->
    State_1 = lists:zip(record_info(fields, state),tl(tuple_to_list(State))),
    {reply, {ok, State_1}, State, Interval}.


%% @doc Handling cast message
%% <p>
%% gen_server callback - Module:handle_cast(Request, State) -> Result.
%% </p>
handle_cast(_Msg, #state{interval = Interval} = State) ->
    {noreply, State, Interval}.


%% @doc Handling all non call/cast messages
%% <p>
%% gen_server callback - Module:handle_info(Info, State) -> Result.
%% </p>
handle_info(timeout, #state{is_suspending = true} = State) ->
    {noreply, State};
handle_info(timeout, #state{id = Id,
                            callback_mod = CallbackMod,
                            properties = Props,
                            interval = Interval} = State) ->
    Props_1 = case catch erlang:apply(
                           CallbackMod, handle_call, [Id, Props]) of
                  {'EXIT', Cause} ->
                      error_logger:error_msg(
                        "~p,~p,~p,~p~n",
                        [{module, ?MODULE_STRING},
                         {function, "handle_info/2"},
                         {line, ?LINE}, {body, {CallbackMod, Cause}}]),
                      case catch erlang:apply(
                                   CallbackMod, handle_fail, [Id, Cause]) of
                          {_, Reason} ->
                              error_logger:info_msg(
                                "~p,~p,~p,~p~n",
                                [{module, ?MODULE_STRING},
                                 {function, "handle_info/2"},
                                 {line, ?LINE}, {body, {CallbackMod, Reason}}]),
                              Props;
                          _ ->
                              Props
                      end;
                  {_Ret, NewProps} ->
                      catch ets:insert(Id, NewProps),
                      NewProps
              end,
    {noreply, State#state{properties = Props_1}, Interval};

handle_info(_, State=#state{interval = Interval}) ->
    {noreply, State, Interval}.


%% @doc This function is called by a gen_server when it is about to
%%      terminate. It should be the opposite of Module:init/1 and do any necessary
%%      cleaning up. When it returns, the gen_server terminates with Reason.
terminate(_Reason, _State) ->
    ok.


%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
