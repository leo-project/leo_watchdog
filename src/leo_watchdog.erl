%%======================================================================
%%
%% Leo Watchdog
%%
%% Copyright (c) 2012-2014 Rakuten, Inc.
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
         stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          id :: atom(),
          callback_mod :: module(),
          properties = [] :: [{atom(), any()}],
          interval = ?DEF_WATCH_INTERVAL :: pos_integer()
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


%%--------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------------------------------
%% @doc Initiates the server
init([Id, CallbackMod, Props, Interval]) ->
    ok = CallbackMod:init(Props),
    {ok, #state{id = Id,
                callback_mod = CallbackMod,
                properties = Props,
                interval = Interval}, Interval}.


%% @doc gen_server callback - Module:handle_call(Request, From, State) -> Result
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.


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
handle_info(timeout, #state{id = Id,
                            callback_mod = CallbackMod,
                            properties = Props,
                            interval = Interval
                           } = State) ->
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
                          {'EXIT', Reason} ->
                              error_logger:info_msg(
                                "~p,~p,~p,~p~n",
                                [{module, ?MODULE_STRING},
                                 {function, "handle_info/2"},
                                 {line, ?LINE}, {body, {CallbackMod, Reason}}]),
                              Props;
                          {_, NewProps} ->
                              NewProps
                      end;
                  {_Ret, NewProps} ->
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
