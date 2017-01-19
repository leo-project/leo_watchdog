%%======================================================================
%%
%% Leo Watchdog
%%
%% Copyright (c) 2012-2017 Rakuten, Inc.
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
%%
%% Basic constants
%%
-undef(DEF_WATCH_INTERVAL).
-define(DEF_WATCH_INTERVAL, 5000).
-undef(DEF_TIMEOUT).
-define(DEF_TIMEOUT, 5000).

-define(WD_TARGET_REX,     'rex').
-define(WD_TARGET_CPU,     'cpu').
-define(WD_TARGET_DISK,    'disk').
-define(WD_TARGET_IO,      'io').
-define(WD_TARGET_IOSTAT,  'iostat').
-define(WD_TARGET_CLUSTER, 'cluster').
-define(WD_TARGET_ERROR,   'error').
-type(watchdog_target() :: ?WD_TARGET_REX |
                           ?WD_TARGET_CPU |
                           ?WD_TARGET_DISK |
                           ?WD_TARGET_IO |
                           ?WD_TARGET_IOSTAT |
                           ?WD_TARGET_CLUSTER |
                           ?WD_TARGET_ERROR).

-define(WD_LEVEL_SAFE,       0).
-define(WD_LEVEL_WARN,      70).
-define(WD_LEVEL_ERROR,     85).
-define(WD_LEVEL_CRITICAL, 100).
-type(watchdog_level() :: ?WD_LEVEL_SAFE |
                          ?WD_LEVEL_WARN |
                          ?WD_LEVEL_ERROR |
                          ?WD_LEVEL_CRITICAL).

-record(watchdog_state, {
          id :: atom(),
          level = ?WD_LEVEL_SAFE :: non_neg_integer(),
          src :: string()|atom()|term(),
          props = [] :: [{atom(), any()}]
         }).

-record(disk_stat, {
          util = 0.0 :: float(),
          rkb  = 0.0 :: float(),
          wkb  = 0.0 :: float()
         }).

-record(watchdog_alarm, {
          id :: atom(),
          state :: #watchdog_state{},
          event_time :: non_neg_integer()
         }).

%%
%% Defalut constants
%%
-define(DEF_MEM_CAPACITY, 33554432).
-define(DEF_CPU_LOAD_AVG, 100.0).
-define(DEF_CPU_UTIL,      90.0).
-define(DEF_INPUT_PER_SEC,  134217728). %% 128MB
-define(DEF_OUTPUT_PER_SEC, 134217728). %% 128MB
-define(DEF_DISK_USE,       85).
-define(DEF_DISK_UTIL,      95.0).
-define(DEF_DISK_READ_KB,   262144). %% 262144KB (256MB)
-define(DEF_DISK_WRITE_KB,  262144). %% 262144KB (256MB)
-define(DEF_RAISED_ERROR_TIMES, 3).
-define(DEF_CHECK_INTERVAL, 1). %% 1sec
-define(DEF_ERROR_THRESHOLD_COUNT, 100).

%%
%% Watchdog item's constants
%%
-define(WD_WARN_USE_PERCENTAGE, 80).
-define(WD_ITEM_CPU,         'load_avg_and_cpu_util').
-define(WD_ITEM_LOAD_AVG,    'load_avg').
-define(WD_ITEM_LOAD_AVG_1M, 'load_avg_1m').
-define(WD_ITEM_LOAD_AVG_5M, 'load_avg_5m').
-define(WD_ITEM_CPU_UTIL,    'cpu_util').
-define(WD_ITEM_IO,          'erlang_io').
-define(WD_ITEM_DISK_USE,    'disk_use_per').
-define(WD_ITEM_DISK_UTIL,   'disk_util').
-define(WD_ITEM_DISK_IO,     'disk_io').
-define(WD_ITEM_DISK_RKB,    'disk_rkb').
-define(WD_ITEM_DISK_WKB,    'disk_wkb').
-define(WD_ITEM_CLUSTER,     'cluster').
-define(WD_ITEM_ERRORS,      'errors').

-define(WD_GRP_CPU,  [?WD_ITEM_LOAD_AVG,
                      ?WD_ITEM_CPU_UTIL
                     ]).
-define(WD_GRP_DISK, [?WD_ITEM_DISK_USE,
                      ?WD_ITEM_DISK_UTIL,
                      ?WD_ITEM_DISK_IO
                     ]).
-define(WD_GRP_IO,     [?WD_ITEM_IO]).
-define(WD_GRP_CLUSER, [?WD_ITEM_CLUSTER]).

-define(WD_TBL_IOSTAT, 'leo_watchdog_iostat').


%%
%% macro - elarm#alarm{} to leo_watchdog#watchdog_alarm{}
%%
-define(to_watchdog_alarm(_Alarm),
        begin
            #alarm{alarm_id = _WatchdogId,
                   additional_information = _Info,
                   event_time = _EventTime} = _Alarm,
            #watchdog_alarm{id = _WatchdogId,
                            state = _Info,
                            event_time =
                                leo_date:greg_seconds_to_unixtime(
                                  calendar:datetime_to_gregorian_seconds(_EventTime))
                           }
        end).


%% ---------------------------------------------------------------------
%% Subscriber
%% ---------------------------------------------------------------------
-define(MAX_SAFE_TIMES, 1).

%% @doc Watchdog - When reach a number of safe, a watchdog loosen the control
-define(env_wd_loosen_control_at_safe_count(),
        case application:get_env(leo_watchdog, loosen_control_at_safe_count) of
            {ok, EnvWDSubSafeTimes} ->
                EnvWDSubSafeTimes;
            _ ->
                ?MAX_SAFE_TIMES
        end).

%% ---------------------------------------------------------------------
%% REX
%% ---------------------------------------------------------------------
%% @doc Watchdog - rex - Is enabled
-define(env_wd_rex_enabled(),
        case application:get_env(leo_watchdog, rex_enabled) of
            {ok, EnvWDRexEnabled} ->
                EnvWDRexEnabled;
            _ ->
                false
        end).

%% @doc Watchdog - rex - interval
-define(env_wd_rex_interval(),
        case application:get_env(leo_watchdog, rex_interval) of
            {ok, EnvWDRexInterval} ->
                EnvWDRexInterval;
            _ ->
                ?DEF_WATCH_INTERVAL
        end).

%% @doc Watchdog - rex - threshold memory capacity for binary
-define(env_wd_threshold_mem_capacity(),
        case application:get_env(leo_watchdog, rex_threshold_mem_capacity) of
            {ok, EnvWDThresholdMemCapacity} ->
                EnvWDThresholdMemCapacity;
            _ ->
                ?DEF_MEM_CAPACITY
        end).

%% ---------------------------------------------------------------------
%% CPU
%% ---------------------------------------------------------------------
%% @doc Watchdog - cpu - is enabled
-define(env_wd_cpu_enabled(),
        case application:get_env(leo_watchdog, cpu_enabled) of
            {ok, EnvWDCpuEnabled} ->
                EnvWDCpuEnabled;
            _ ->
                false
        end).

%% @doc Watchdog - cpu - interval
-define(env_wd_cpu_interval(),
        case application:get_env(leo_watchdog, cpu_interval) of
            {ok, EnvWDCpuInterval} ->
                EnvWDCpuInterval;
            _ ->
                ?DEF_WATCH_INTERVAL
        end).

%% @doc Watchdog - cpu - threshold cpu load avg
-define(env_wd_threshold_cpu_load_avg(),
        case application:get_env(leo_watchdog, cpu_threshold_load_avg) of
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
-define(env_wd_threshold_cpu_util(),
        case application:get_env(leo_watchdog, cpu_threshold_util) of
            {ok, EnvWDThresholdCpuUtil} ->
                EnvWDThresholdCpuUtil;
            _ ->
                ?DEF_CPU_UTIL
        end).

%% @doc Watchdog - cpu - raised error times
-define(env_wd_cpu_raised_error_times(),
        case application:get_env(leo_watchdog, cpu_raised_error_times) of
            {ok, EnvRaisedCPUErrorTimes} ->
                EnvRaisedCPUErrorTimes;
            _ ->
                ?DEF_RAISED_ERROR_TIMES
        end).

%% ---------------------------------------------------------------------
%% IO
%% ---------------------------------------------------------------------
%% @doc Watchdog - io - Is enabled
-define(env_wd_io_enabled(),
        case application:get_env(leo_watchdog, io_enabled) of
            {ok, EnvWDIOEnabled} ->
                EnvWDIOEnabled;
            _ ->
                false
        end).
%% @doc Watchdog - io - interval
-define(env_wd_io_interval(),
        case application:get_env(leo_watchdog, io_interval) of
            {ok, EnvWDIoInterval} ->
                EnvWDIoInterval;
            _ ->
                ?DEF_WATCH_INTERVAL
        end).
%% @doc Watchdog - io - threshold input/sec
-define(env_wd_threshold_input_per_sec(),
        case application:get_env(leo_watchdog, io_threshold_input_per_sec) of
            {ok, EnvWDThresholdInputPerSec} ->
                EnvWDThresholdInputPerSec;
            _ ->
                ?DEF_INPUT_PER_SEC
        end).
%% @doc Watchdog - io - threshold output/sec
-define(env_wd_threshold_output_per_sec(),
        case application:get_env(leo_watchdog, io_threshold_output_per_sec) of
            {ok, EnvWDThresholdOutputPerSec} ->
                EnvWDThresholdOutputPerSec;
            _ ->
                ?DEF_OUTPUT_PER_SEC
        end).


%% ---------------------------------------------------------------------
%% DISK
%% ---------------------------------------------------------------------
%% @doc Watchdog - disk - Is enabled
-define(env_wd_disk_enabled(),
        case application:get_env(leo_watchdog, disk_enabled) of
            {ok, EnvWDDiskEnabled} ->
                EnvWDDiskEnabled;
            _ ->
                false
        end).

%% @doc Watchdog - disk - interval
-define(env_wd_disk_interval(),
        case application:get_env(leo_watchdog, disk_interval) of
            {ok, EnvWDDiskInterval} ->
                EnvWDDiskInterval;
            _ ->
                ?DEF_WATCH_INTERVAL
        end).

%% @doc Watchdog - disk - raised error times
-define(env_wd_disk_raised_error_times(),
        case application:get_env(leo_watchdog, disk_raised_error_times) of
            {ok, EnvRaisedDiskErrorTimes} ->
                EnvRaisedDiskErrorTimes;
            _ ->
                ?DEF_RAISED_ERROR_TIMES
        end).

%% @doc Watchdog - disk - target paths
-define(env_wd_disk_target_paths(),
        case application:get_env(leo_watchdog, disk_target_paths) of
            {ok, EnvDiskTargetPaths} ->
                EnvDiskTargetPaths;
            _ ->
                ["/"]
        end).

%% @doc Watchdog - disk - target devices
-define(env_wd_disk_target_devices(),
        case application:get_env(leo_watchdog, disk_target_devices) of
            {ok, EnvDiskTargetDevices} ->
                EnvDiskTargetDevices;
            _ ->
                []
        end).

%% @doc Watchdog - disk - threshold iowait
-define(env_wd_threshold_disk_use(),
        case application:get_env(leo_watchdog, disk_threshold_use) of
            {ok, EnvWDThresholdIoWait} ->
                EnvWDThresholdIoWait;
            _ ->
                ?DEF_DISK_USE
        end).

%% @doc Watchdog - disk - threshold disk utilization
-define(env_wd_threshold_disk_util(),
        case application:get_env(leo_watchdog, disk_threshold_util) of
            {ok, EnvWDThresholdDiskUtil} ->
                EnvWDThresholdDiskUtil;
            _ ->
                ?DEF_DISK_UTIL
        end).

%% @doc Watchdog - disk - read kb/sec
-define(env_wd_threshold_disk_rkb(),
        case application:get_env(leo_watchdog, disk_threshold_rkb) of
            {ok, EnvWDThresholdRkb} ->
                EnvWDThresholdRkb;
            _ ->
                ?DEF_DISK_READ_KB
        end).

%% @doc Watchdog - disk - write kb/sec
-define(env_wd_threshold_disk_wkb(),
        case application:get_env(leo_watchdog, disk_threshold_wkb) of
            {ok, EnvWDThresholdWkb} ->
                EnvWDThresholdWkb;
            _ ->
                ?DEF_DISK_WRITE_KB
        end).


%% ---------------------------------------------------------------------
%% CLUSTER
%% ---------------------------------------------------------------------
%% @doc Watchdog - cluster - Is enabled
-define(env_wd_cluster_enabled(),
        case application:get_env(leo_watchdog, cluster_enabled) of
            {ok, EnvWDClusterEnabled} ->
                EnvWDClusterEnabled;
            _ ->
                false
        end).

%% @doc Watchdog - cluster - interval
-define(env_wd_cluster_interval(),
        case application:get_env(leo_watchdog, cluster_interval) of
            {ok, EnvWDClusterInterval} ->
                EnvWDClusterInterval;
            _ ->
                ?DEF_WATCH_INTERVAL
        end).

%% @doc Watchdog - cluster - mfa
-define(env_wd_cluster_check_state_of_members_mfa(),
        case application:get_env(leo_watchdog, cluster_check_state_of_members_mfa) of
            {ok, EnvWDClusterStateOfMembersMFA} ->
                EnvWDClusterStateOfMembersMFA;
            _ ->
                {undefind, undefined, []}
        end).


%% ---------------------------------------------------------------------
%% ERRORS
%% ---------------------------------------------------------------------
%% @doc Watchdog - errors - Is enabled
-define(env_wd_error_enabled(),
        case application:get_env(leo_watchdog, error_enabled) of
            {ok, EnvWDErrorEnabled} ->
                EnvWDErrorEnabled;
            _ ->
                false
        end).

%% @doc Watchdog - errors - interval
-define(env_wd_error_interval(),
        case application:get_env(leo_watchdog, error_interval) of
            {ok, EnvWDErrorInterval} ->
                EnvWDErrorInterval;
            _ ->
                ?DEF_WATCH_INTERVAL
        end).

%% @doc Watchdog - errors - interval
-define(env_wd_error_threshold_count(),
        case application:get_env(leo_watchdog, error_threshold_count) of
            {ok, EnvWDErrorThresholdCount} ->
                EnvWDErrorThresholdCount;
            _ ->
                ?DEF_ERROR_THRESHOLD_COUNT
        end).
