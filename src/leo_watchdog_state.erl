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
%% @doc leo_watchdog
%% @reference
%% @end
%%======================================================================
-module(leo_watchdog_state).

-author('Yosuke Hara').

-include("leo_watchdog.hrl").
-include_lib("elarm/include/elarm.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([find_by_id/1,
         find_not_safe_items/0
        ]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Retrieve the current status by id
-spec(find_by_id(WatchdogId) ->
             ok | not_found when WatchdogId::atom()).
find_by_id(WatchdogId) ->
    case elarm:get_alarms() of
        {ok, []} ->
            not_found;
        {ok, Items} ->
            find_by_id_1(Items, WatchdogId, [])
    end.

find_by_id_1([],_WatchdogId, Acc) ->
    {ok, Acc};
find_by_id_1([#alarm{alarm_id = WatchdogId} = Alarm|Rest], WatchdogId, Acc) ->
    WatchdogAlarm = ?to_watchdog_alarm(Alarm),
    find_by_id_1(Rest, WatchdogId, [WatchdogAlarm|Acc]);
find_by_id_1([_|Rest], WatchdogId, Acc) ->
    find_by_id_1(Rest, WatchdogId, Acc).


%% @doc Retrieve the states of not safe
-spec(find_not_safe_items() ->
             {ok, Items} |
             not_found when Items::[{atom(), #watchdog_state{}}]).
find_not_safe_items() ->
    case elarm:get_alarms() of
        {ok, []} ->
            not_found;
        {ok, Items} ->
            Items_1 = [?to_watchdog_alarm(Alarm) || Alarm <- Items],
            {ok, Items_1}
    end.
