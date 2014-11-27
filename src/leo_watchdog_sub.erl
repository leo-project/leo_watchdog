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
%% @doc leo_watchdog subscriber
%% @reference
%% @end
%%======================================================================
-module(leo_watchdog_sub).

-author('Yosuke Hara').

-behaviour(gen_server).

-include("leo_watchdog.hrl").
-include_lib("elarm/include/elarm.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/2,
         start_link/3,
         start_link/4,
         stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(MAX_SAFE_TIMES, 5).

-record(state, {
          id :: atom(),
          filter :: [atom()]|[{atom(), any()}],
          callback_mod :: module()|undefined,
          elarm_ref    :: reference(),
          consecutive_safe_times = 0 :: non_neg_integer(),
          max_safe_times = ?MAX_SAFE_TIMES :: non_neg_integer()
         }).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Start the server
-spec(start_link(Id, CallbackMod) ->
             {ok,Pid} | ignore | {error,Error} when Id::atom(),
                                                    CallbackMod::module()|undefined,
                                                    Pid::pid(),
                                                    Error::{already_started,Pid} | term()).
start_link(Id, CallbackMod) ->
    start_link(Id, [all], CallbackMod).

-spec(start_link(Id, Filters, CallbackMod) ->
             {ok,Pid} | ignore | {error,Error} when Id::atom(),
                                                    Filters::[atom()]|[{atom(),any()}],
                                                    CallbackMod::module()|undefined,
                                                    Pid::pid(),
                                                    Error::{already_started,Pid} | term()).
start_link(Id, Filters, CallbackMod) ->
    start_link(Id, Filters, ?MAX_SAFE_TIMES, CallbackMod).

-spec(start_link(Id, Filters, MaxSafeTimes, CallbackMod) ->
             {ok,Pid} | ignore | {error,Error} when Id::atom(),
                                                    Filters::[atom()]|[{atom(),any()}],
                                                    MaxSafeTimes::non_neg_integer(),
                                                    CallbackMod::module()|undefined,
                                                    Pid::pid(),
                                                    Error::{already_started,Pid} | term()).
start_link(Id, Filters, MaxSafeTimes, CallbackMod) ->
    gen_server:start_link({local, Id}, ?MODULE,
                          [Id, Filters, MaxSafeTimes, CallbackMod], []).


%% @doc Stop the server
-spec(stop(Id) ->
             ok when Id::atom()).
stop(Id) ->
    gen_server:call(Id, stop).


%%--------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------------------------------
%% @doc Initiates the server
init([Id, Filter, MaxSafeTimes, CallbackMod]) ->
    {ok, Ref, _} = elarm:subscribe(Filter, self()),
    {ok, #state{id = Id,
                filter = Filter,
                callback_mod = CallbackMod,
                elarm_ref = Ref,
                max_safe_times = MaxSafeTimes}, ?DEF_TIMEOUT}.


%% @doc gen_server callback - Module:handle_call(Request, From, State) -> Result
handle_call(stop, _From, #state{elarm_ref = Ref} = State) ->
    elarm:unsubscribe(Ref),
    {stop, normal, stopped, State}.


%% @doc Handling cast message
%% <p>
%% gen_server callback - Module:handle_cast(Request, State) -> Result.
%% </p>
handle_cast(_Msg, State) ->
    {noreply, State, ?DEF_TIMEOUT}.


%% @doc Handling all non call/cast messages
%% <p>
%% gen_server callback - Module:handle_info(Info, State) -> Result.
%% </p>
handle_info(_, #state{callback_mod = undefined} = State) ->
    {noreply, State, ?DEF_TIMEOUT};
handle_info(timeout, #state{id = Id,
                            callback_mod = Mod,
                            consecutive_safe_times = SafeTimes,
                            max_safe_times = MaxSafeTimes} = State) ->
    SafeTimes_1 = case (MaxSafeTimes =< SafeTimes) of
                      true ->
                          1;
                      false ->
                          SafeTimes + 1
                  end,
    catch erlang:apply(Mod, notify,
                       [Id, [], SafeTimes_1, leo_date:unixtime()]),
    {noreply, State#state{consecutive_safe_times = SafeTimes_1}, ?DEF_TIMEOUT};
handle_info({elarm, _Ref, Alarm}, #state{id = Id,
                                         callback_mod = Mod} = State) ->
    Alarm_1 = ?to_watchdog_alarm(Alarm),
    catch erlang:apply(Mod, notify,
                       [Id, Alarm_1, leo_date:unixtime()]),
    {noreply, State#state{consecutive_safe_times = 0}, ?DEF_TIMEOUT};
handle_info(_, State) ->
    {noreply, State, ?DEF_TIMEOUT}.


%% @doc This function is called by a gen_server when it is about to
%%      terminate. It should be the opposite of Module:init/1 and do any necessary
%%      cleaning up. When it returns, the gen_server terminates with Reason.
terminate(_Reason, _State) ->
    ok.


%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
