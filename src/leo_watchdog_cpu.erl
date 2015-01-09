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
         start_link/4,
         stop/0,
         state/0
        ]).

%% Callback
-export([init/1,
         handle_call/2,
         handle_fail/2
        ]).

-record(state, {
          threshold_load_avg = 0.0 :: float(),
          threshold_cpu_util = 0.0 :: float(),
          raised_error_times = 3   :: non_neg_integer(),
          cur_error_times    = 0   :: non_neg_integer()
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
    start_link(ThresholdLoadAvg, ThresholdCPUUtil,
               ?DEF_RAISED_ERROR_TIMES, Interval).

%% @doc Start the server
-spec(start_link(ThresholdLoadAvg, ThresholdCPUUtil, RaisedErrorTimes, Interval) ->
             {ok,Pid} |
             ignore |
             {error,Error} when ThresholdLoadAvg::float(),
                                ThresholdCPUUtil::float(),
                                RaisedErrorTimes::non_neg_integer(),
                                Interval::pos_integer(),
                                Pid::pid(),
                                Error::{already_started,Pid} | term()).
start_link(ThresholdLoadAvg, ThresholdCPUUtil, RaisedErrorTimes, Interval) ->
    State = #state{threshold_load_avg = ThresholdLoadAvg,
                   threshold_cpu_util = ThresholdCPUUtil,
                   raised_error_times = RaisedErrorTimes},
    leo_watchdog:start_link(?MODULE, ?MODULE, State, Interval).


%% @doc Stop the server
-spec(stop() ->
             ok).
stop() ->
    leo_watchdog:stop(?MODULE).


%% @doc Retrieves state of the watchdog
-spec(state() ->
             {ok, State} when State::[{atom(), any()}]).
state() ->
    case ets:lookup(?MODULE, state) of
        [] ->
            not_found;
        [State|_] ->
            State_1 = lists:zip(record_info(fields, state),tl(tuple_to_list(State))),
            {ok, State_1}
    end.


%%--------------------------------------------------------------------
%% Callback
%%--------------------------------------------------------------------
%% @doc Initialize this process
-spec(init(State) ->
             ok | {error, Cause} when State::any(),
                                      Cause::any()).
init(_State) ->
    ok.


%% @dog Call execution of the watchdog
-spec(handle_call(Id, State) ->
             {ok, State} | {{error,Error}, State} when Id::atom(),
                                                       State::#state{},
                                                       Error::any()).
handle_call(Id, #state{threshold_load_avg = ThresholdLoadAvg,
                       threshold_cpu_util = ThresholdCpuUtil,
                       raised_error_times = RaisedErrorTimes,
                       cur_error_times    = CurErrorTimes} = State) ->
    try
        AVG_1 = erlang:round(cpu_sup:avg1() / 256 * 1000) / 10,
        AVG_5 = erlang:round(cpu_sup:avg5() / 256 * 1000) / 10,
        CPU_Util = case os:type() of
                       {unix, linux} ->
                           erlang:round(cpu_sup:util() * 10) / 10;
                       _OtherOS ->
                           0
                   end,
        ErrorLevel = case (CurErrorTimes + 1 >= RaisedErrorTimes) of
                         true  -> ?WD_LEVEL_ERROR;
                         false -> ?WD_LEVEL_WARN
                     end,

        %% Load avg
        Level_1 = case (ThresholdLoadAvg * 100 < AVG_1 orelse
                        ThresholdLoadAvg * 100 < AVG_5) of
                      true ->
                          elarm:raise(Id, ?WD_ITEM_LOAD_AVG,
                                      #watchdog_state{id = Id,
                                                      level = ErrorLevel,
                                                      src   = ?WD_ITEM_LOAD_AVG,
                                                      props = [{?WD_ITEM_LOAD_AVG_1M, AVG_1},
                                                               {?WD_ITEM_LOAD_AVG_5M, AVG_5}
                                                              ]}),
                          ErrorLevel;
                      false when (ThresholdLoadAvg * 80 < AVG_1 orelse
                                  ThresholdLoadAvg * 80 < AVG_5) ->
                          elarm:raise(Id, ?WD_ITEM_LOAD_AVG,
                                      #watchdog_state{id = Id,
                                                      level = ?WD_LEVEL_WARN,
                                                      src   = ?WD_ITEM_LOAD_AVG,
                                                      props = [{?WD_ITEM_LOAD_AVG_1M, AVG_1},
                                                               {?WD_ITEM_LOAD_AVG_5M, AVG_5}
                                                              ]}),
                          ?WD_LEVEL_WARN;
                      false ->
                          elarm:clear(Id, ?WD_ITEM_LOAD_AVG),
                          ?WD_LEVEL_SAFE
                      end,

        %% CPU util
        Level_2 = case (CPU_Util > ThresholdCpuUtil) of
                      true ->
                          elarm:raise(Id, ?WD_ITEM_CPU_UTIL,
                                      #watchdog_state{id = Id,
                                                      level = ErrorLevel,
                                                      src   = ?WD_ITEM_CPU_UTIL,
                                                      props = [{?WD_ITEM_CPU_UTIL, CPU_Util}
                                                              ]}),
                          ErrorLevel;
                      false ->
                          elarm:clear(Id, ?WD_ITEM_CPU_UTIL),
                          ?WD_LEVEL_SAFE
                  end,

        %% Check the result
        CurErrorTimes_1 =
            case (Level_1 >= ?WD_LEVEL_WARN orelse
                  Level_2 >= ?WD_LEVEL_WARN) of
                true when CurErrorTimes >= RaisedErrorTimes ->
                    0;
                true ->
                    CurErrorTimes + 1;
                false ->
                    0
            end,
        {ok, State#state{cur_error_times = CurErrorTimes_1}}
    catch
        _:_ ->
            {ok, State}
    end.

%% @dog Call execution failed
-spec(handle_fail(Id, Cause) ->
             ok | {error,Error} when Id::atom(),
                                     Cause::any(),
                                     Error::any()).
handle_fail(_Id,_Cause) ->
    ok.
