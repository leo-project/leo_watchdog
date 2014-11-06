%%======================================================================
%%
%% Leo Ordning & Reda
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
%%======================================================================
-author('Yosuke Hara').

-undef(DEF_WATCH_INTERVAL).
-define(DEF_WATCH_INTERVAL, 5000).
-undef(DEF_TIMEOUT).
-define(DEF_TIMEOUT, 5000).

-type(watchdog_id() :: atom()).

-define(WD_LEVEL_SAFE,       0).
-define(WD_LEVEL_WARN,      70).
-define(WD_LEVEL_ERROR,     85).
-define(WD_LEVEL_CRITICAL, 100).
-type(watchdog_level() :: ?WD_LEVEL_SAFE |
                          ?WD_LEVEL_WARN |
                          ?WD_LEVEL_ERROR |
                          ?WD_LEVEL_CRITICAL).

-type(watchdog_src() :: string()|
                        atom()|
                        term()).

-record(watchdog_state, {
          id :: watchdog_id(),
          level = ?WD_LEVEL_SAFE :: non_neg_integer(),
          src :: watchdog_src(),
          props = [] :: [{atom(), any()}]
         }).

-record(watchdog_alarm, {
          id :: watchdog_id(),
          state :: #watchdog_state{},
          event_time :: tuple()
         }).

%% defalut constants
-define(DEF_MEM_CAPACITY, 33554432).
-define(DEF_CPU_LOAD_AVG, 100.0).
-define(DEF_CPU_UTIL,      90.0).
-define(DEF_INPUT_PER_SEC,  134217728). %% 128MB
-define(DEF_OUTPUT_PER_SEC, 134217728). %% 128MB
-define(DEF_DISK_USE,       85).
-define(DEF_DISK_UTIL,      95.0).


-define(WD_WARN_USE_PERCENTAGE, 80).
-define(WD_ITEM_LOAD_AVG,  'load_avg').
-define(WD_ITEM_CPU_UTIL,  'cpu_util').
-define(WD_ITEM_IO,        'io_input').
-define(WD_ITEM_DISK_USE,  'disk_use_per').
-define(WD_ITEM_DISK_UTIL, 'disk_util').


%% macro - elarm#alarm{} to leo_watchdog#watchdog_alarm{}
-define(to_watchdog_alarm(_Alarm),
        begin
            #alarm{alarm_id = _WatchdogId,
                   additional_information = _Info,
                   event_time = _EventTime} = _Alarm,
            #watchdog_alarm{id = _WatchdogId,
                            state = _Info,
                            event_time = _EventTime}
        end).


%% ---------------------------------------------------------------------
%% REX
%% ---------------------------------------------------------------------
%%
%% @doc Watchdog - rex - Is enabled
-define(env_wd_rex_enabled(App),
        case application:get_env(App, wd_rex_enabled) of
            {ok, EnvWDRexEnabled} ->
                EnvWDRexEnabled;
            _ ->
                true
        end).
%% @doc Watchdog - rex - interval
-define(env_wd_rex_interval(App),
        case application:get_env(App, wd_rex_interval) of
            {ok, EnvWDRexInterval} ->
                EnvWDRexInterval;
            _ ->
                ?DEF_WATCH_INTERVAL
        end).
%% @doc Watchdog - rex - threshold memory capacity for binary
-define(env_wd_threshold_mem_capacity(App),
        case application:get_env(App, wd_threshold_mem_capacity) of
            {ok, EnvWDThresholdMemCapacity} ->
                EnvWDThresholdMemCapacity;
            _ ->
                ?DEF_MEM_CAPACITY
        end).

%% ---------------------------------------------------------------------
%% CPU
%% ---------------------------------------------------------------------
%% @doc Watchdog - cpu - is enabled
-define(env_wd_cpu_enabled(App),
        case application:get_env(App, wd_cpu_enabled) of
            {ok, EnvWDCpuEnabled} ->
                EnvWDCpuEnabled;
            _ ->
                true
        end).
%% @doc Watchdog - cpu - interval
-define(env_wd_cpu_interval(App),
        case application:get_env(App, wd_cpu_interval) of
            {ok, EnvWDCpuInterval} ->
                EnvWDCpuInterval;
            _ ->
                ?DEF_WATCH_INTERVAL
        end).
%% @doc Watchdog - cpu - threshold cpu load avg
-define(env_wd_threshold_cpu_load_avg(App),
        case application:get_env(App, wd_threshold_cpu_load_avg) of
            {ok, EnvWDThresholdCpuLoadAvg} when is_number(EnvWDThresholdCpuLoadAvg) ->
                EnvWDThresholdCpuLoadAvg;
            {ok, EnvWDThresholdCpuLoadAvg} ->
                case string:str(EnvWDThresholdCpuLoadAvg, ".") of
                    0 -> list_to_integer(EnvWDThresholdCpuLoadAvg);
                    _ -> list_to_float(EnvWDThresholdCpuLoadAvg)
                end;
            _ ->
                ?DEF_CPU_LOAD_AVG
        end).
%% @doc Watchdog - cpu - threshold cpu util
-define(env_wd_threshold_cpu_util(App),
        case application:get_env(App, wd_threshold_cpu_util) of
            {ok, EnvWDThresholdCpuUtil} ->
                EnvWDThresholdCpuUtil;
            _ ->
                ?DEF_CPU_UTIL
        end).

%% ---------------------------------------------------------------------
%% IO
%% ---------------------------------------------------------------------
%% @doc Watchdog - io - Is enabled
-define(env_wd_io_enabled(App),
        case application:get_env(App, wd_io_enabled) of
            {ok, EnvWDIOEnabled} ->
                EnvWDIOEnabled;
            _ ->
                true
        end).
%% @doc Watchdog - io - interval
-define(env_wd_io_interval(App),
        case application:get_env(App, wd_io_interval) of
            {ok, EnvWDIoInterval} ->
                EnvWDIoInterval;
            _ ->
                ?DEF_WATCH_INTERVAL
        end).
%% @doc Watchdog - io - threshold input/sec
-define(env_wd_threshold_input_per_sec(App),
        case application:get_env(App, wd_threshold_input_per_sec) of
            {ok, EnvWDThresholdInputPerSec} ->
                EnvWDThresholdInputPerSec;
            _ ->
                ?DEF_INPUT_PER_SEC
        end).
%% @doc Watchdog - io - threshold output/sec
-define(env_wd_threshold_output_per_sec(App),
        case application:get_env(App, wd_threshold_output_per_sec) of
            {ok, EnvWDThresholdOutputPerSec} ->
                EnvWDThresholdOutputPerSec;
            _ ->
                ?DEF_OUTPUT_PER_SEC
        end).


%% ---------------------------------------------------------------------
%% DISK
%% ---------------------------------------------------------------------
%% @doc Watchdog - disk - Is enabled
-define(env_wd_disk_enabled(App),
        case application:get_env(App, wd_disk_enabled) of
            {ok, EnvWDDiskEnabled} ->
                EnvWDDiskEnabled;
            _ ->
                true
        end).
%% @doc Watchdog - disk - interval
-define(env_wd_disk_interval(App),
        case application:get_env(App, wd_disk_interval) of
            {ok, EnvWDDiskInterval} ->
                EnvWDDiskInterval;
            _ ->
                ?DEF_WATCH_INTERVAL
        end).
%% @doc Watchdog - disk - threshold iowait
-define(env_wd_threshold_disk_use(App),
        case application:get_env(App, wd_threshold_disk_use) of
            {ok, EnvWDThresholdIoWait} ->
                EnvWDThresholdIoWait;
            _ ->
                ?DEF_DISK_USE
        end).
%% @doc Watchdog - disk - threshold disk utilization
-define(env_wd_threshold_disk_util(App),
        case application:get_env(App, wd_threshold_disk_util) of
            {ok, EnvWDThresholdDiskUtil} ->
                EnvWDThresholdDiskUtil;
            _ ->
                ?DEF_DISK_UTIL
        end).
