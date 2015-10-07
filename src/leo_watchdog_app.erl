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
%% ---------------------------------------------------------------------
%% Leo Watchdog - Application.
%% @doc
%% @end
%%======================================================================
-module(leo_watchdog_app).
-author('Yosuke Hara').

-behaviour(application).

-include("leo_watchdog.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Application and Supervisor callbacks
-export([start/2, prep_stop/1, stop/1,
         profile_output/0]).

%%----------------------------------------------------------------------
%% Application behaviour callbacks
%%----------------------------------------------------------------------
start(_Type, _Args) ->
    consider_profiling(),
    application:start(sasl),
    application:start(os_mon),
    application:start(elarm),

    case leo_watchdog_sup:start_link() of
        {ok, Pid} ->
            %% Watchdog for rex's binary usage
            case ?env_wd_rex_enabled() of
                true ->
                    MaxMemCapacity = ?env_wd_threshold_mem_capacity(),
                    IntervalRex = ?env_wd_rex_interval(),
                    leo_watchdog_sup:start_child(
                      rex, [MaxMemCapacity], IntervalRex);
                false ->
                    void
            end,

            %% Wachdog for CPU
            case ?env_wd_cpu_enabled() of
                true ->
                    MaxCPULoadAvg = ?env_wd_threshold_cpu_load_avg(),
                    MaxCPUUtil    = ?env_wd_threshold_cpu_util(),
                    IntervalCpu   = ?env_wd_cpu_interval(),
                    leo_watchdog_sup:start_child(
                      cpu, [MaxCPULoadAvg, MaxCPUUtil], IntervalCpu);
                false ->
                    void
            end,

            %% Wachdog for IO
            case ?env_wd_io_enabled() of
                true ->
                    MaxInput   = ?env_wd_threshold_input_per_sec(),
                    MaxOutput  = ?env_wd_threshold_output_per_sec(),
                    IntervalIo = ?env_wd_io_interval(),
                    leo_watchdog_sup:start_child(
                      io, [MaxInput, MaxOutput], IntervalIo);
                false ->
                    void
            end,

            %% Wachdog for Disk
            case ?env_wd_disk_enabled() of
                true ->
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
                false ->
                    void
            end,

            %% Watchdog for Cluster
            case ?env_wd_cluster_enabled() of
                true ->
                    leo_watchdog_sup:start_child(
                      cluster, [?env_wd_cluster_check_state_of_members_mfa()],
                      ?env_wd_cluster_interval());
                false ->
                    void
            end,

            %% Watchdog for Error
            case ?env_wd_error_enabled() of
                true ->
                    leo_watchdog_sup:start_child(
                      ?WD_TARGET_ERROR, [?env_wd_error_threshold_count()],
                      ?env_wd_error_interval());
                false ->
                    void
            end,

            {ok, Pid};
        Other ->
            Other
    end.


prep_stop(_State) ->
    leo_watchdog_sup:stop(),
    ok.

stop(_State) ->
    ok.


-spec profile_output() -> ok.
profile_output() ->
    eprof:stop_profiling(),
    eprof:log("leo_watchdog.procs.profile"),
    eprof:analyze(procs),
    eprof:log("leo_watchdog.total.profile"),
    eprof:analyze(total).


-spec consider_profiling() -> profiling | not_profiling | {error, any()}.
consider_profiling() ->
    case application:get_env(leo_watchdog, profile) of
        {ok, true} ->
            {ok, _Pid} = eprof:start(),
            eprof:start_profiling([self()]);
        _ ->
            not_profiling
    end.
