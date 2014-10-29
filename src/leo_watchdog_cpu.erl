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
%% @doc Watchdog for CPU load avg and utilization
%% @reference
%% @end
%%======================================================================
-module(leo_watchdog_cpu).

-author('Yosuke Hara').

-behaviour(leo_watchdog_behaviour).

-include("leo_watchdog.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/4,
         stop/0]).

%% Callback
-export([handle_call/2,
         handle_fail/2]).

-define(PROP_MAX_LOAD_AVG, 'max_load_avg').
-define(PROP_MAX_CPU_UTIL, 'max_cpu_util').
-define(DEF_LOAD_AVG, 90.0).
-define(DEF_CPU_UTIL, 90.0).

-record(state, {
          max_load_avg = 0.0 :: float(),
          max_cpu_util = 0.0 :: float(),
          callback_mod       :: module()
         }).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Start the server
-spec(start_link(MaxLoadAvg, MaxCPUUtil, CallbackMod, Interval) ->
             {ok,Pid} | ignore | {error,Error} when MaxLoadAvg::float(),
                                                    MaxCPUUtil::float(),
                                                    CallbackMod::function(),
                                                    Interval::pos_integer(),
                                                    Pid::pid(),
                                                    Error::{already_started,Pid} | term()).
start_link(MaxLoadAvg, MaxCPUUtil, CallbackMod, Interval) ->
    State = #state{max_load_avg  = MaxLoadAvg,
                   max_cpu_util  = MaxCPUUtil,
                   callback_mod  = CallbackMod},
    leo_watchdog:start_link(?MODULE, ?MODULE, State, Interval).


%% @doc Stop the server
-spec(stop() ->
             ok).
stop() ->
    leo_watchdog:stop(?MODULE).


%%--------------------------------------------------------------------
%% Callback
%%--------------------------------------------------------------------
%% @dog Call execution of the watchdog
-spec(handle_call(Id, State) ->
             ok | {error,Error} when Id::atom(),
                                     State::#state{},
                                     Error::any()).
handle_call(Id, #state{max_load_avg = MaxLoadAvg,
                       max_cpu_util = MaxCpuUtil,
                       callback_mod = CallbackMod} = State) ->
    try
        AVG_1    = erlang:round(cpu_sup:avg1() / 256 * 1000) / 10,
        AVG_5    = erlang:round(cpu_sup:avg5() / 256 * 1000) / 10,
        CPU_Util = erlang:round(cpu_sup:util() * 10) / 10,

        case (MaxLoadAvg < AVG_1 orelse
              MaxLoadAvg < AVG_5) of
            true when CPU_Util > MaxCpuUtil ->
                %% Nofify the message to the clients
                CurState = [{load_avg_1, AVG_1},
                            {load_avg_5, AVG_5},
                            {cpu_util,   CPU_Util}
                           ],
                error_logger:warning_msg(
                  "~p,~p,~p,~p~n",
                  [{module, ?MODULE_STRING},
                   {function, "handle_call/2"},
                   {line, ?LINE}, {body, CurState}]),
                case CallbackMod of
                    undefined ->
                        ok;
                    _ ->
                        erlang:apply(CallbackMod, notify, [Id, CurState])
                end;
            true ->
                error_logger:info_msg(
                  "~p,~p,~p,~p~n",
                  [{module, ?MODULE_STRING},
                   {function, "handle_call/2"},
                   {line, ?LINE}, {body, [{load_avg_1, AVG_1},
                                          {load_avg_5, AVG_5},
                                          {cpu_util,   CPU_Util}
                                         ]}]);
            false ->
                ok
        end
    catch
        _:_ ->
            ok
    end,
    {ok, State}.

%% @dog Call execution failed
-spec(handle_fail(Id, Cause) ->
             ok | {error,Error} when Id::atom(),
                                     Cause::any(),
                                     Error::any()).
handle_fail(_Id,_Cause) ->
    ok.
