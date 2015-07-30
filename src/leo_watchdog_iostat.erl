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
-module(leo_watchdog_iostat).

-author('Yosuke Hara').

-behaviour(gen_server).

-include("leo_watchdog.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/3,
         stop/0]).
-export([get/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          os_type :: {atom(), atom()},
          devices  = [] :: [string()],
          interval = ?DEF_WATCH_INTERVAL :: pos_integer()
         }).

-define(PROP_TARGET_PATHS,  'target_paths').
-define(DEF_DISK_USAGE, 90.0).
-define(DISK_ITEM_RKB,  'rkb').
-define(DISK_ITEM_WKB,  'wkb').
-define(DISK_ITEM_UTIL, 'util').

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Start the server
-spec(start_link(OSType, TargetDevices, IntervalTime) ->
             {ok,Pid} | ignore | {error,Error} when OSType::atom(),
                                                    TargetDevices::[string()],
                                                    IntervalTime::pos_integer(),
                                                    Pid::pid(),
                                                    Error::{already_started,Pid} | term()).
start_link(OSType, TargetDevices, IntervalTime) ->
    %% Takes conflict execution measures
    timer:sleep(erlang:phash2(leo_date:clock(), timer:seconds(1))),
    gen_server:start_link({local, ?MODULE}, ?MODULE,
                          [OSType, TargetDevices, IntervalTime], []).


%% @doc Stop the server
-spec(stop() ->
             ok).
stop() ->
    gen_server:call(?MODULE, stop).


%% @doc Retrieve latest iostat
-spec(get() ->
             {ok, Result} when Result::#disk_stat{}).
get() ->
    case catch ets:lookup(?WD_TBL_IOSTAT, 1) of
        [{_, Ret}|_] ->
            {ok, Ret};
        _ ->
            {ok, []}
    end.


%%--------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------------------------------
%% @doc Initiates the server
init([OSType, TargetDevices, Interval]) ->
    ?WD_TBL_IOSTAT = ets:new(?WD_TBL_IOSTAT, [named_table, set,
                                              public, {read_concurrency, true}]),
    {ok, #state{os_type = OSType,
                devices = TargetDevices,
                interval = Interval}, Interval}.


%% @doc gen_server callback - Module:handle_call(Request, From, State) -> Result
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.


%% @doc Handling cast message
%% <p>
%% gen_server callback - Module:handle_cast(Request, State) -> Result.
%% </p>
handle_cast(_Msg, #state{interval = Interval} = State) ->
    {noreply, State, Interval}.


%% @doc Handling all non call/cast messages
%% <p>
%% gen_server callback - Module:handle_info(Info, State) -> Result.
%% </p>
handle_info(timeout, #state{os_type = OSType,
                            devices = Devices,
                            interval = Interval
                           } = State) ->
    spawn(fun() ->
                  Ret = disk_stats(OSType, Devices),
                  true = ets:insert(?WD_TBL_IOSTAT, {1, Ret})
          end),
    {noreply, State, Interval};

handle_info(_, State=#state{interval = Interval}) ->
    {noreply, State, Interval}.


%% @doc This function is called by a gen_server when it is about to
%%      terminate. It should be the opposite of Module:init/1 and do any necessary
%%      cleaning up. When it returns, the gen_server terminates with Reason.
terminate(_Reason, _State) ->
    ok.


%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%% Inner Function
%%--------------------------------------------------------------------
%% @doc Retrieve io-wait for Linux(CentOS, Ubuntu)
%% @private
-spec(disk_stats(OSType, TargetDevices) ->
             #disk_stat{} when OSType::{atom(), atom()},
                               TargetDevices::[string()]).
disk_stats({unix, linux}, TargetDevices) ->
    case os:cmd("which iostat") of
        [] ->
            #disk_stat{};
        _ ->
            %% Execute os-command
            CmdRet = os:cmd("iostat -kx 1 2"),

            %% Parsing result of os-command
            Tokens_1 = string:tokens(
                         string:substr(
                           CmdRet,
                           string:rstr(CmdRet, "Device")), "\n"),
            HeaderTokens = string:tokens(hd(Tokens_1), " "),
            {_, PosOfItems} =
                lists:foldl(fun("rkB/s", {Idx, SoFar}) -> {Idx+1, [{rkb, Idx}|SoFar]};
                               ("wkB/s", {Idx, SoFar}) -> {Idx+1, [{wkb, Idx}|SoFar]};
                               ("%util", {Idx, SoFar}) -> {Idx+1, [{util,Idx}|SoFar]};
                               (_, {Idx, SoFar}) -> {Idx+1, SoFar}
                            end, {1,[]}, HeaderTokens),

            %% Retrieving data
            [_|Tokens_2] = Tokens_1,
            get_target_values(TargetDevices, Tokens_2, PosOfItems)
    end;

disk_stats({unix, Type}, TargetDevices) when Type =:= freebsd;
                                             Type =:= sunos ->
    case os:cmd("which iostat") of
        [] ->
            #disk_stat{};
        _ ->
            %% Execute os-command
            CmdRet = os:cmd("iostat -x 1 2"),

            %% Parsing result of os-command
            Tokens_1 = string:tokens(
                         string:substr(
                           CmdRet,
                           string:rstr(CmdRet, "device")), "\n"),
            HeaderTokens = string:tokens(hd(Tokens_1), " "),
            {_, PosOfItems} =
                lists:foldl(fun("kr/s", {Idx, SoFar}) -> {Idx+1, [{rkb, Idx}|SoFar]};
                               ("kw/s", {Idx, SoFar}) -> {Idx+1, [{wkb, Idx}|SoFar]};
                               ("%b",   {Idx, SoFar}) -> {Idx+1, [{util,Idx}|SoFar]};
                               (_, {Idx, SoFar}) -> {Idx+1, SoFar}
                            end, {1,[]}, HeaderTokens),

            %% Retrieving data
            [_|Tokens_2] = Tokens_1,
            get_target_values(TargetDevices, Tokens_2, PosOfItems)
    end;
disk_stats({unix, darwin},_TargetDevices) ->
    #disk_stat{};

%% Other OSes not supporeted
disk_stats(_,_) ->
    #disk_stat{}.


%% @private
get_target_values(TargetDevices, Tokens, PosOfItems) ->
    DiskStats = get_target_values_1(Tokens, PosOfItems, []),

    %% Retrieving target-data
    DiskStats_1 = get_target_values_2(TargetDevices, DiskStats, []),

    %% Retrieve max value of each item
    DiskStats_2 = max_value(DiskStats_1, #disk_stat{}),
    DiskStats_2.


%% @private
get_target_values_1([],_,SoFar) ->
    SoFar;
get_target_values_1([Items|Rest], PosOfItems, SoFar) ->
    RkbPos  = leo_misc:get_value(?DISK_ITEM_RKB,  PosOfItems),
    WkbPos  = leo_misc:get_value(?DISK_ITEM_WKB,  PosOfItems),
    UtilPos = leo_misc:get_value(?DISK_ITEM_UTIL, PosOfItems),

    Tokens = string:tokens(Items, " "),
    DevName = lists:nth(1, Tokens),
    RkbVal  = get_item(RkbPos,  Tokens),
    WkbVal  = get_item(WkbPos,  Tokens),
    UtilVal = get_item(UtilPos, Tokens),

    get_target_values_1(Rest, PosOfItems,
                        [{DevName, #disk_stat{util = UtilVal,
                                              rkb = RkbVal,
                                              wkb = WkbVal}}|SoFar]).

%% @private
get_item(KeyPos, Values) ->
    case string:chr(lists:nth(KeyPos, Values), $.) of
        0 ->
            get_item(integer, KeyPos, Values);
        _ ->
            get_item(float, KeyPos, Values)
    end.

%% @private
get_item(_, KeyPos, Values) when KeyPos > length(Values) ->
    0;
get_item(float, KeyPos, Values) ->
    list_to_float(lists:nth(KeyPos, Values));
get_item(integer, KeyPos, Values) ->
    list_to_integer(lists:nth(KeyPos, Values)).


%% @private
get_target_values_2([],DiskStats,[]) ->
    DiskStats;
get_target_values_2([],_,Acc) ->
    Acc;
get_target_values_2([Device|Rest], DiskStats, Acc) ->
    case leo_misc:get_value(Device, DiskStats) of
        undefined ->
            get_target_values_2(Rest, DiskStats, Acc);
        Stat ->
            get_target_values_2(Rest, DiskStats, [Stat|Acc])
    end.


%% @private
max_value([], SoFar) ->
    SoFar;
max_value([{_, #disk_stat{util = Util,
                          rkb  = Rkb,
                          wkb  = Wkb}}|Rest], #disk_stat{util = CurUtil,
                                                         rkb  = CurRkb,
                                                         wkb  = CurWkb}) ->
    RetUtil = case (Util > CurUtil) of
                  true  -> Util;
                  false -> CurUtil
              end,
    RetRkb  = case (Rkb > CurRkb) of
                  true  -> Rkb;
                  false -> CurRkb
              end,
    RetWkb  = case (Wkb > CurWkb) of
                  true  -> Wkb;
                  false -> CurWkb
              end,
    max_value(Rest, #disk_stat{util = RetUtil,
                               rkb  = RetRkb,
                               wkb  = RetWkb}).
