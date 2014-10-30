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

-define(WD_STATE_SAFE,  'safe').
-define(WD_STATE_WARN,  'warn').
-define(WD_STATE_ERROR, 'error').
-type(watchdog_state() :: ?WD_STATE_SAFE |
                          ?WD_STATE_WARN |
                          ?WD_STATE_ERROR).
-record(watchdog_state, {
          state = ?WD_STATE_SAFE :: watchdog_state(),
          props = [] :: [{atom(), any()}]
         }).

%% defalut constants
-define(DEF_MEM_CAPACITY, 33554432).
-define(DEF_CPU_LOAD_AVG, 100.0).
-define(DEF_CPU_UTIL,      90.0).
-define(DEF_INPUT_FOR_INTERVAL,  134217728). %% 128MB
-define(DEF_OUTPUT_FOR_INTERVAL, 134217728). %% 128MB
-define(DEF_DISK_UTIL,      90.0).

-define(env_watchdog_check_interval(App),
        case application:get_env(App, watchdog_check_interval) of
            {ok, EnvWDCheckInterval} ->
                EnvWDCheckInterval;
            _ ->
                ?DEF_WATCH_INTERVAL
        end).

-define(env_watchdog_rex_enabled(App),
        case application:get_env(App, watchdog_rex_enabled) of
            {ok, EnvWDRexEnabled} ->
                EnvWDRexEnabled;
            _ ->
                true
        end).
-define(env_watchdog_max_mem_capacity(App),
        case application:get_env(App, watchdog_max_mem_capacity) of
            {ok, EnvWDMaxMemCapacity} ->
                EnvWDMaxMemCapacity;
            _ ->
                ?DEF_MEM_CAPACITY
        end).


-define(env_watchdog_cpu_enabled(App),
        case application:get_env(App, watchdog_cpu_enabled) of
            {ok, EnvWDCpuEnabled} ->
                EnvWDCpuEnabled;
            _ ->
                true
        end).
-define(env_watchdog_max_cpu_load_avg(App),
        case application:get_env(App, watchdog_max_cpu_load_avg) of
            {ok, EnvWDMaxCpuLoadAvg} ->
                EnvWDMaxCpuLoadAvg;
            _ ->
                ?DEF_CPU_LOAD_AVG
        end).
-define(env_watchdog_max_cpu_util(App),
        case application:get_env(App, watchdog_max_cpu_util) of
            {ok, EnvWDMaxCpuUtil} ->
                EnvWDMaxCpuUtil;
            _ ->
                ?DEF_CPU_UTIL
        end).


-define(env_watchdog_io_enabled(App),
        case application:get_env(App, watchdog_io_enabled) of
            {ok, EnvWDIOEnabled} ->
                EnvWDIOEnabled;
            _ ->
                true
        end).
-define(env_watchdog_max_input_for_interval(App),
        case application:get_env(App, watchdog_max_input_for_interval) of
            {ok, EnvWDMaxInputForInterval} ->
                EnvWDMaxInputForInterval;
            _ ->
                ?DEF_INPUT_FOR_INTERVAL
        end).
-define(env_watchdog_max_output_for_interval(App),
        case application:get_env(App, watchdog_max_output_for_interval) of
            {ok, EnvWDMaxOutputForInterval} ->
                EnvWDMaxOutputForInterval;
            _ ->
                ?DEF_OUTPUT_FOR_INTERVAL
        end).
-define(env_watchdog_max_disk_util(App),
        case application:get_env(App, watchdog_max_disk_util) of
            {ok, EnvWDMaxDiskUtil} ->
                EnvWDMaxDiskUtil;
            _ ->
                ?DEF_DISK_UTIL
        end).
