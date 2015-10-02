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
             MaxLoadAvg  = 0.8,
             MaxCPUUtil  = 30,
             MaxInput    = 64,
             MaxOutput   = 64,
             MaxDiskUtil = 30,
             MaxDiskUse  = 90,
             MaxDiskRKB  = 512 * 1024 * 1024,
             MaxDiskWKB  = 512 * 1024 * 1024,
             MaxRaisedErrorTimes = 5,
             MaxNumOfErrors = 100,
             ok = leo_watchdog_sup:start_child(rex,     [MaxMemForBin], Interval),
             ok = leo_watchdog_sup:start_child(cpu,     [MaxLoadAvg, MaxCPUUtil], Interval),
             ok = leo_watchdog_sup:start_child(io,      [MaxInput, MaxOutput], Interval),
             ok = leo_watchdog_sup:start_child(disk,    [["/"], [], MaxDiskUse, MaxDiskUtil, MaxDiskRKB, MaxDiskWKB, MaxRaisedErrorTimes], Interval),
             ok = leo_watchdog_sup:start_child(cluster, [{?MODULE, check_cluster_members, []}], Interval),
             ok = leo_watchdog_sup:start_child(error,   [MaxNumOfErrors], Interval),

             ok = leo_watchdog_sup:start_subscriber(
                    'leo_watchdog_sub_io', ['leo_watchdog_io'], ?MODULE),
             ok = leo_watchdog_sup:start_subscriber(
                    'leo_watchdog_sub_cpu', ['leo_watchdog_cpu'], ?MODULE),
             ok = leo_watchdog_sup:start_subscriber(
                    'leo_watchdog_sub_disk', ['leo_watchdog_disk'], ?MODULE),
             ok = leo_watchdog_sup:start_subscriber(
                    'leo_watchdog_sub_cluster', ['leo_watchdog_cluster'], ?MODULE),
             ok = leo_watchdog_sup:start_subscriber(
                    'leo_watchdog_sub_error', ['leo_watchdog_error'], ?MODULE),
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
    %% Watchdog-errors:
    [leo_watchdog_collector:push({error, 'async-get'}) || _I <- lists:seq(1, 700)],
    [leo_watchdog_collector:push({error, 'other_1'}) || _I <- lists:seq(1, 200)],
    [leo_watchdog_collector:push({error, 'other_2'}) || _I <- lists:seq(1, 100)],

    %% Other watchdog:
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
    Ret_1 = leo_watchdog_cpu:state(),
    Ret_2 = leo_watchdog_disk:state(),
    ?debugVal({Ret_1, Ret_2}),
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
