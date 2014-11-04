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
-export([start_link/3,
         stop/0]).

%% Callback
-export([handle_call/2,
         handle_fail/2]).

-record(state, {
          threshold_load_avg = 0.0 :: float(),
          threshold_cpu_util = 0.0 :: float()
         }).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Start the server
-spec(start_link(ThresholdLoadAvg, ThresholdCPUUtil, Interval) ->
             {ok,Pid} |
             ignore |
             {error,Error} when ThresholdLoadAvg::float(),
                                ThresholdCPUUtil::float(),
                                Interval::pos_integer(),
                                Pid::pid(),
                                Error::{already_started,Pid} | term()).
start_link(ThresholdLoadAvg, ThresholdCPUUtil, Interval) ->
    State = #state{threshold_load_avg = ThresholdLoadAvg,
                   threshold_cpu_util = ThresholdCPUUtil},
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
             {ok, State} | {{error,Error}, State} when Id::atom(),
                                                       State::#state{},
                                                       Error::any()).
handle_call(Id, #state{threshold_load_avg = ThresholdLoadAvg,
                       threshold_cpu_util = ThresholdCpuUtil} = State) ->
    try
        AVG_1 = erlang:round(cpu_sup:avg1() / 256 * 1000) / 10,
        AVG_5 = erlang:round(cpu_sup:avg5() / 256 * 1000) / 10,
        CPU_Util = case os:type() of
                       {unix, linux} ->
                           erlang:round(cpu_sup:util() * 10) / 10;
                       _OtherOS ->
                           0
                   end,
        %% Load avg
        case (ThresholdLoadAvg * 100 < AVG_1 orelse
              ThresholdLoadAvg * 100 < AVG_5) of
            true ->
                elarm:raise(Id, ?WD_ITEM_LOAD_AVG,
                            [{level, ?WD_LEVEL_ERROR},
                             {load_avg_1, AVG_1},
                             {load_avg_5, AVG_5}
                            ]);
            false ->
                elarm:clear(Id, ?WD_ITEM_LOAD_AVG)
        end,

        %% CPU util
        case (CPU_Util > ThresholdCpuUtil) of
            true ->
                elarm:raise(Id, ?WD_ITEM_CPU_UTIL,
                            [{level, ?WD_LEVEL_ERROR},
                             {?WD_ITEM_CPU_UTIL, CPU_Util}
                            ]);
            false ->
                elarm:clear(Id, ?WD_ITEM_CPU_UTIL)
        end,
        ok
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
