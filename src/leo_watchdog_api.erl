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
-module(leo_watchdog_api).

-author('Yosuke Hara').

-include("leo_watchdog.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start/1, stop/1,
         set_raised_error_times/2,
         set_check_interval/2,
         set_cpu_threshold_load_avg/1,
         set_cpu_threshold_util/1,
         set_disk_threshold_use/1,
         set_disk_threshold_util/1,
         set_disk_threshold_rkb/1,
         set_disk_threshold_wkb/1
         %% @TODO:v1.4
         %% set_disk_target_paths/1,
         %% set_disk_target_devices/1,
        ]).


%% @doc Start the watchdog
-spec(start(Target) ->
             ok | {error, any()} when Target::watchdog_target()).
start(?WD_TARGET_CPU = Target) ->
    ok = application:set_env(leo_watchdog, cpu_enabled, true),
    case whereis(leo_watchdog_cpu) of
        undefined ->
            start_1(Target);
        _ ->
            leo_watchdog:resume(leo_watchdog_cpu)
    end;
start(?WD_TARGET_DISK = Target) ->
    ok = application:set_env(leo_watchdog, disk_enabled, true),
    case whereis(leo_watchdog_disk) of
        undefined ->
            start_1(Target);
        _ ->
            leo_watchdog:resume(leo_watchdog_disk)
    end;
start(?WD_TARGET_CLUSTER = Target) ->
    ok = application:set_env(leo_watchdog, cluster_enabled, true),
    case whereis(leo_watchdog_cluster) of
        undefined ->
            start_1(Target);
        _ ->
            leo_watchdog:resume(leo_watchdog_cluster)
    end;
start(?WD_TARGET_ERROR = Target) ->
    ok = application:set_env(leo_watchdog, error_enabled, true),
    case whereis(leo_watchdog_error) of
        undefined ->
            start_1(Target);
        _ ->
            leo_watchdog:resume(leo_watchdog_error)
    end;
start(_) ->
    {error, invalid_parameter}.

%% @private
-spec(start_1(Target) ->
             ok | {error, any()} when Target::watchdog_target()).
start_1(?WD_TARGET_CPU) ->
    MaxCPULoadAvg = ?env_wd_threshold_cpu_load_avg(),
    MaxCPUUtil    = ?env_wd_threshold_cpu_util(),
    IntervalCpu   = ?env_wd_cpu_interval(),
    leo_watchdog_sup:start_child(
      cpu, [MaxCPULoadAvg, MaxCPUUtil], IntervalCpu);

start_1(?WD_TARGET_DISK) ->
    leo_watchdog_sup:start_child(
      disk, [?env_wd_disk_target_paths(),
             ?env_wd_disk_target_devices(),
             ?env_wd_threshold_disk_use(),
             ?env_wd_threshold_disk_util(),
             ?env_wd_threshold_disk_rkb(),
             ?env_wd_threshold_disk_wkb(),
             ?env_wd_disk_raised_error_times()
            ],
      ?env_wd_disk_interval());

start_1(?WD_TARGET_CLUSTER) ->
    leo_watchdog_sup:start_child(
      cluster, [?env_wd_cluster_check_state_of_members_mfa()],
      ?env_wd_cluster_interval());

start_1(?WD_TARGET_ERROR) ->
    leo_watchdog_sup:start_child(
      error, [?env_wd_error_threshold_count()],
      ?env_wd_error_interval()).


%% @doc Stop the watchdog
-spec(stop(Watchdog) ->
             ok | {error, any()} when Watchdog::watchdog_target()).
stop(?WD_TARGET_CPU) ->
    ok = application:set_env(leo_watchdog, cpu_enabled, false),
    case whereis(leo_watchdog_cpu) of
        undefined ->
            ok;
        _ ->
            leo_watchdog:suspend(leo_watchdog_cpu)
    end;
stop(?WD_TARGET_DISK) ->
    ok = application:set_env(leo_watchdog, disk_enabled, false),
    case whereis(leo_watchdog_disk) of
        undefined ->
            ok;
        _ ->
            leo_watchdog:suspend(leo_watchdog_disk)
    end;
stop(?WD_TARGET_CLUSTER) ->
    ok = application:set_env(leo_watchdog, cluster_enabled, false),
    case whereis(leo_watchdog_cluster) of
        undefined ->
            ok;
        _ ->
            leo_watchdog:suspend(leo_watchdog_cluster)
    end;
stop(_) ->
    {error, invalid_parameter}.


%% @doc Set the raised error times
-spec(set_raised_error_times(Watchdog, Value) ->
             ok | {error, any()} when Watchdog::watchdog_target(),
                                      Value::pos_integer()).
set_raised_error_times(?WD_TARGET_CPU, Value) when is_integer(Value) ->
    ok = application:set_env(leo_watchdog, cpu_raised_error_times, Value),
    leo_watchdog:update_property(leo_watchdog_cpu, raised_error_times, Value);
set_raised_error_times(?WD_TARGET_DISK, Value) when is_integer(Value) ->
    ok = application:set_env(leo_watchdog, disk_raised_error_times, Value),
    leo_watchdog:update_property(leo_watchdog_disk, raised_error_times, Value);
set_raised_error_times(_,_) ->
    {error, invalid_value}.


%% @doc Set check interval of the watchdog
-spec(set_check_interval(Watchdog, Interval) ->
             ok | {error, any()} when Watchdog::watchdog_target(),
                                      Interval::pos_integer()).
set_check_interval(?WD_TARGET_CPU, Interval) when is_integer(Interval) ->
    ok = application:set_env(leo_watchdog, cpu_interval, Interval),
    leo_watchdog:set_interval(leo_watchdog_cpu, Interval);
set_check_interval(?WD_TARGET_DISK, Interval) when is_integer(Interval) ->
    ok = application:set_env(leo_watchdog, disk_interval, Interval),
    leo_watchdog:set_interval(leo_watchdog_disk, Interval);
set_check_interval(?WD_TARGET_CLUSTER, Interval) when is_integer(Interval) ->
    ok = application:set_env(leo_watchdog, cluster_interval, Interval),
    leo_watchdog:set_interval(leo_watchdog_cluster, Interval);
set_check_interval(_,_) ->
    {error, invalid_value}.


%% @doc Set cpu's threshold load average
-spec(set_cpu_threshold_load_avg(Value) ->
             ok | {error, any()} when Value::number()).
set_cpu_threshold_load_avg(Value) when is_number(Value) ->
    ok = application:set_env(leo_watchdog, cpu_threshold_load_avg, Value),
    leo_watchdog:update_property(leo_watchdog_cpu, threshold_load_avg, Value);
set_cpu_threshold_load_avg(_) ->
    {error, invalid_value}.


%% @doc Set cpu's threshold util
-spec(set_cpu_threshold_util(Value) ->
             ok | {error, any()} when Value::pos_integer()).
set_cpu_threshold_util(Value) when is_integer(Value) ->
    ok = application:set_env(leo_watchdog, cpu_threshold_util, Value),
    leo_watchdog:update_property(leo_watchdog_cpu, threshold_cpu_util, Value);
set_cpu_threshold_util(_) ->
    {error, invalid_value}.


%% @doc Set disk's threshold use
-spec(set_disk_threshold_use(Value) ->
             ok | {error, any()} when Value::pos_integer()).
set_disk_threshold_use(Value) when is_integer(Value) ->
    ok = application:set_env(leo_watchdog, disk_threshold_use, Value),
    leo_watchdog:update_property(leo_watchdog_disk, threshold_disk_use, Value);
set_disk_threshold_use(_) ->
    {error, invalid_value}.


%% @doc Set disk's threshold util
-spec(set_disk_threshold_util(Value) ->
             ok | {error, any()} when Value::pos_integer()).
set_disk_threshold_util(Value) when is_integer(Value) ->
    ok = application:set_env(leo_watchdog, disk_threshold_util, Value),
    leo_watchdog:update_property(leo_watchdog_disk, threshold_disk_util, Value);
set_disk_threshold_util(_) ->
    {error, invalid_value}.


%% @doc Set disk's threshold read(kbytes)
-spec(set_disk_threshold_rkb(Value) ->
             ok | {error, any()} when Value::pos_integer()).
set_disk_threshold_rkb(Value) when is_integer(Value) ->
    ok = application:set_env(leo_watchdog, disk_threshold_rkb, Value),
    leo_watchdog:update_property(leo_watchdog_disk, threshold_disk_rkb, Value);
set_disk_threshold_rkb(_) ->
    {error, invalid_value}.


%% @doc Set disk's threshold write(kbytes)
-spec(set_disk_threshold_wkb(Value) ->
             ok | {error, any()} when Value::pos_integer()).
set_disk_threshold_wkb(Value) when is_integer(Value) ->
    ok = application:set_env(leo_watchdog, disk_threshold_wkb, Value),
    leo_watchdog:update_property(leo_watchdog_disk, threshold_disk_wkb, Value);
set_disk_threshold_wkb(_) ->
    {error, invalid_value}.
