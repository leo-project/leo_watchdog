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

-export([notify/2]).


notify(_, State) ->
    error_logger:warning_msg(
      "~p,~p,~p,~p~n",
      [{module, ?MODULE_STRING},
       {function, "notify/2"},
       {line, ?LINE}, {body, State}]),
    ok.

-ifdef(EUNIT).

%%======================================================================
%% TEST
%%======================================================================
suite_test_() ->
    {setup,
     fun ( ) ->
             application:start(leo_watchdog),
             Interval   = 1000,
             MaxLoadAvg = 50,
             MaxCPUUtil = 10,
             MaxInput   = 1024,
             MaxOutput  = 1024,
             ok = leo_watchdog_sup:start_child(cpu, [MaxLoadAvg, MaxCPUUtil, ?MODULE], Interval),
             ok = leo_watchdog_sup:start_child(io,  [MaxInput, MaxOutput, ?MODULE], Interval),
             ok
     end,
     fun (_) ->
             application:stop(leo_watchdog),
             ok
     end,
     [
      {"test watchdog",
       {timeout, 1000, fun suite/0}}
     ]}.


suite() ->
    NumOfMsgs = 1000000,
    Pid = spawn(fun() ->
                        loop(NumOfMsgs)
                end),
    send_message(NumOfMsgs, Pid),

    State_CPU = leo_watchdog_state:get('leo_watchdog_cpu'),
    State_IO  = leo_watchdog_state:get('leo_watchdog_io'),
    ?debugVal({State_CPU, State_IO}),
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
    ok;
send_message(NumOfMsgs, Pid) ->
    case (NumOfMsgs rem 100000) of
        0 ->
            State_CPU = leo_watchdog_state:get('leo_watchdog_cpu'),
            State_IO  = leo_watchdog_state:get('leo_watchdog_io'),
            ?debugVal({State_CPU, State_IO}),
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
