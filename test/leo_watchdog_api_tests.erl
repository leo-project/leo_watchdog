%%====================================================================
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
%% -------------------------------------------------------------------
%% @doc
%% @end
%%====================================================================
-module(leo_watchdog_api_tests).
-author('yosuke hara').

-include("leo_watchdog.hrl").
-include_lib("eunit/include/eunit.hrl").

-ifdef(EUNIT).

%%======================================================================
%% TEST
%%======================================================================
suite_test_() ->
    {setup,
     fun ( ) ->
             application:start(sasl),
             application:start(os_mon),
             application:start(elarm),
             application:start(leo_watchdog),
             ok
     end,
     fun (_) ->
             application:stop(leo_watchdog),
             ok
     end,
     [
      {"test watchdog's api",
       {timeout, 30000, fun suite/0}}
     ]}.


suite() ->
    %% launch watchdogs
    ok = leo_watchdog_api:start(?WD_TARGET_CPU),
    ?assertEqual(true, whereis(leo_watchdog_cpu) /= undefined),

    ok = leo_watchdog_api:start(?WD_TARGET_DISK),
    ?assertEqual(true, whereis(leo_watchdog_disk) /= undefined),

    ok = leo_watchdog_api:start(?WD_TARGET_CLUSTER),
    ?assertEqual(true, whereis(leo_watchdog_cluster) /= undefined),

    %% raised error times
    ok = leo_watchdog_api:set_raised_error_times(?WD_TARGET_CPU, 1),
    {ok, 1} = application:get_env(leo_watchdog, cpu_raised_error_times),

    ok = leo_watchdog_api:set_raised_error_times(?WD_TARGET_DISK, 2),
    {ok, 2} = application:get_env(leo_watchdog, disk_raised_error_times),

    %% check interval
    ok = leo_watchdog_api:set_check_interval(?WD_TARGET_CPU, 1000),
    {ok, 1000} = application:get_env(leo_watchdog, cpu_interval),

    ok = leo_watchdog_api:set_check_interval(?WD_TARGET_DISK, 2000),
    {ok, 2000} = application:get_env(leo_watchdog, disk_interval),

    ok = leo_watchdog_api:set_check_interval(?WD_TARGET_CLUSTER, 3000),
    {ok, 3000} = application:get_env(leo_watchdog, cluster_interval),

    %% CPU related properties
    ok = leo_watchdog_api:set_cpu_threshold_load_avg(5.5),
    {ok, 5.5} = application:get_env(leo_watchdog, cpu_threshold_load_avg),

    ok = leo_watchdog_api:set_cpu_threshold_util(98),
    {ok, 98} = application:get_env(leo_watchdog, cpu_threshold_util),

    {ok, CPUState} = leo_watchdog_cpu:state(),
    1000 = leo_misc:get_value(interval, CPUState),
    1    = leo_misc:get_value(raised_error_times, CPUState),
    5.5  = leo_misc:get_value(threshold_load_avg, CPUState),
    98   = leo_misc:get_value(threshold_cpu_util, CPUState),

    %% Disk related properties
    ok = leo_watchdog_api:set_disk_threshold_use(80),
    {ok, 80} = application:get_env(leo_watchdog, disk_threshold_use),

    ok = leo_watchdog_api:set_disk_threshold_util(100),
    {ok, 100} = application:get_env(leo_watchdog, disk_threshold_util),

    Len_1 =  96 * 1024 * 1024,
    Len_2 = 128 * 1024 * 1024,
    ok = leo_watchdog_api:set_disk_threshold_rkb(Len_1),
    ok = leo_watchdog_api:set_disk_threshold_wkb(Len_2),
    {ok, Len_1} = application:get_env(leo_watchdog, disk_threshold_rkb),
    {ok, Len_2} = application:get_env(leo_watchdog, disk_threshold_wkb),

    {ok, DiskState} = leo_watchdog_disk:state(),
    2000  = leo_misc:get_value(interval, DiskState),
    2     = leo_misc:get_value(raised_error_times, DiskState),
    80    = leo_misc:get_value(threshold_disk_use, DiskState),
    100   = leo_misc:get_value(threshold_disk_util, DiskState),
    Len_1 = leo_misc:get_value(threshold_disk_rkb, DiskState),
    Len_2 = leo_misc:get_value(threshold_disk_wkb, DiskState),
    ok.

-endif.
