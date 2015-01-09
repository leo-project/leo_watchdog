%%====================================================================
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
%% -------------------------------------------------------------------
%% @doc
%% @end
%%====================================================================
-module(leo_watchdog_tests).
-author('yosuke hara').

-include("leo_watchdog.hrl").
-include_lib("eunit/include/eunit.hrl").

-ifdef(EUNIT).

-export([handle_notify/3,
         handle_notify/4,
         check_cluster_members/0
        ]).
handle_notify(Id, Alarm, Unixtime) ->
    ?debugVal({Id, Alarm, Unixtime}),
    ok.
handle_notify(Id, [], SafeTimes, Unixtime) ->
    ?debugVal({Id, SafeTimes, Unixtime}),
    ok.

check_cluster_members() ->
    Level = erlang:phash2(leo_date:clock(), 100),
    {ok, Level}.


%%======================================================================
%% TEST
%%======================================================================
suite_test_() ->
    {setup,
     fun ( ) ->
             application:start(sasl),
             application:start(os_mon),
             application:start(elarm),
             leo_watchdog_sup:start_link(),

             Interval = 3,
             MaxMemForBin = 1024 * 1024 * 32,
             MaxLoadAvg  = 2,
             MaxCPUUtil  = 30,
             MaxInput    = 64,
             MaxOutput   = 64,
             MaxDiskUtil = 30,
             MaxIoWait   = 30,
             ok = leo_watchdog_sup:start_child(rex,     [MaxMemForBin], Interval),
             ok = leo_watchdog_sup:start_child(cpu,     [MaxLoadAvg, MaxCPUUtil], Interval),
             ok = leo_watchdog_sup:start_child(io,      [MaxInput, MaxOutput], Interval),
             ok = leo_watchdog_sup:start_child(disk,    [["/"], MaxDiskUtil, MaxIoWait], Interval),
             ok = leo_watchdog_sup:start_child(cluster, [{?MODULE, check_cluster_members, []}], Interval),

             ok = leo_watchdog_sup:start_subscriber(
                    'leo_watchdog_sub_io', [?WD_ITEM_IO], ?MODULE),
             ok = leo_watchdog_sup:start_subscriber(
                    'leo_watchdog_sub_cpu', [?WD_ITEM_LOAD_AVG,
                                             ?WD_ITEM_CPU_UTIL], ?MODULE),
             ok = leo_watchdog_sup:start_subscriber(
                    'leo_watchdog_sub_disk', [?WD_ITEM_DISK_USE,
                                              ?WD_ITEM_DISK_UTIL,
                                              ?WD_ITEM_DISK_IO], ?MODULE),
             ok = leo_watchdog_sup:start_subscriber(
                    'leo_watchdog_sub_cluster', [?WD_ITEM_CLUSTER], ?MODULE),
             ok
     end,
     fun (_) ->
             application:stop(leo_watchdog),
             ok
     end,
     [
      {"test watchdog",
       {timeout, 30000, fun suite/0}}
     ]}.


suite() ->
    NumOfMsgs = 1000000,
    Pid = spawn(fun() ->
                        loop(NumOfMsgs)
                end),
    send_message(NumOfMsgs, Pid),
    ?debugVal(leo_watchdog_state:find_not_safe_items()),
    not_found = leo_watchdog_state:find_not_safe_items(['leo_watchdog_io',
                                                        'leo_watchdog_disk',
                                                        'leo_watchdog_cpu',
                                                        'leo_watchdog_cluster'
                                                       ]),
    ?debugVal(leo_watchdog_state:find_not_safe_items(['leo_watchdog_io'],
                                                     ?WD_LEVEL_WARN)),
    {ok, CPUState}  = leo_watchdog_cpu:state(),
    {ok, DiskState} = leo_watchdog_disk:state(),
    ?debugVal({CPUState, DiskState}),
    ok.

%% @private
loop(0) ->
    ok;
loop(Index) ->
    receive
        {put, _Msg} ->
            loop(Index - 1);
        _ ->
            ok
    end.

send_message(0,_Pid) ->
    timer:sleep(timer:seconds(10)),
    ok;
send_message(NumOfMsgs, Pid) ->
    case (NumOfMsgs rem 100000) of
        0 ->
            ?debugVal(leo_watchdog_state:find_not_safe_items()),
            timer:sleep(10);
        _ ->
            void
    end,

    MinLen = 256,
    MaxLen = 4096,
    Len_1 = erlang:phash2(leo_date:clock(), MaxLen),
    Len_2 = case Len_1 < MinLen of
                true  -> MinLen;
                false -> Len_1
            end,
    Msg = crypto:rand_bytes(Len_2),
    erlang:send(Pid, {put, Msg}),
    send_message(NumOfMsgs - 1, Pid).

-endif.
