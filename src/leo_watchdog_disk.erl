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
%% @doc leo_watchdog for 'rex' process
%% @reference
%% @end
%%======================================================================
-module(leo_watchdog_disk).

-author('Yosuke Hara').

-behaviour(leo_watchdog_behaviour).

-include("leo_watchdog.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/4,
         start_link/8,
         stop/0]).

%% Callback
-export([handle_call/2,
         handle_fail/2]).

-export([disk_use/2,
         get_disk_data/0
        ]).

-define(PROP_TARGET_PATHS,  'target_paths').
-define(DEF_DISK_USAGE, 90.0).
-define(DISK_ITEM_RKB,  'rkb').
-define(DISK_ITEM_WKB,  'wkb').
-define(DISK_ITEM_UTIL, 'util').

-record(state, {
          id :: atom(),
          %% for disk-use
          target_paths = [] :: [string()],
          threshold_disk_use  = 100  :: non_neg_integer(),
          %% for other disk-stats
          target_devices = [] :: [string()],
          threshold_disk_util = 90.0 :: float(),
          threshold_disk_rkb  = 6400.0 :: float(),
          threshold_disk_wkb  = 6400.0 :: float(),
          raised_error_times  = 3 :: non_neg_integer(),
          cur_error_times     = 0 :: non_neg_integer()
         }).

-record(disk_data, {
          filesystem = [] :: string(),
          blocks = 0      :: non_neg_integer(),
          used = 0        :: non_neg_integer(),
          available = 0   :: non_neg_integer(),
          use_percentage = 0 :: non_neg_integer(),
          use_percentage_str = [] :: string(),
          mounted_on = [] :: string()
         }).

-record(disk_stat, {
          util = 0.0 :: float(),
          rkb  = 0.0 :: float(),
          wkb  = 0.0 :: float()
         }).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Start the server
-spec(start_link(TargetPaths, ThresholdDiskUse, ThresholdDiskUtil, IntervalTime) ->
             {ok,Pid} | ignore | {error,Error} when TargetPaths::[string()],
                                                    ThresholdDiskUse::non_neg_integer(),
                                                    ThresholdDiskUtil::float(),
                                                    IntervalTime::pos_integer(),
                                                    Pid::pid(),
                                                    Error::{already_started,Pid} | term()).
start_link(TargetPaths, ThresholdDiskUse, ThresholdDiskUtil, IntervalTime) ->
    leo_watchdog:start_link(?MODULE, ?MODULE,
                            #state{target_paths  = TargetPaths,
                                   threshold_disk_use  = ThresholdDiskUse,
                                   threshold_disk_util = ThresholdDiskUtil}, IntervalTime).


%% @doc Start the server
-spec(start_link(TargetPaths, ThresholdDiskUse,
                 TargetDevices, ThresholdDiskUtil,
                 ThresholdRkb, ThresholdWkb,
                 RaisedErrorTimes, IntervalTime) ->
             {ok,Pid} | ignore | {error,Error} when TargetPaths::[string()],
                                                    ThresholdDiskUse::non_neg_integer(),
                                                    TargetDevices::[string()],
                                                    ThresholdDiskUtil::float(),
                                                    ThresholdRkb::float(),
                                                    ThresholdWkb::float(),
                                                    RaisedErrorTimes::non_neg_integer(),
                                                    IntervalTime::pos_integer(),
                                                    Pid::pid(),
                                                    Error::{already_started,Pid} | term()).
start_link(TargetPaths, ThresholdDiskUse,
           TargetDevices, ThresholdDiskUtil,
           ThresholdRkb, ThresholdWkb, RaisedErrorTimes, IntervalTime) ->
    leo_watchdog:start_link(?MODULE, ?MODULE,
                            #state{id = ?MODULE,
                                   target_paths        = TargetPaths,
                                   threshold_disk_use  = ThresholdDiskUse,
                                   threshold_disk_util = ThresholdDiskUtil,
                                   target_devices      = TargetDevices,
                                   threshold_disk_rkb  = ThresholdRkb,
                                   threshold_disk_wkb  = ThresholdWkb,
                                   raised_error_times  = RaisedErrorTimes
                                  }, IntervalTime).


%% @doc Stop the server
-spec(stop() ->
             ok).
stop() ->
    leo_watchdog:stop(?MODULE).


%% @doc Retrieve disk data from os command
-spec(get_disk_data() ->
             DiskData when DiskData::[#disk_data{}]).
get_disk_data() ->
    Tokens_1 = string:tokens(os:cmd("df -lk"), "\n"),
    Tokens_2 = lists:delete(hd(Tokens_1), Tokens_1),
    Tokens_3 = [ string:tokens(Row, " ") || Row <- Tokens_2 ],
    get_disk_data(os:type(), Tokens_3).

-spec(get_disk_data(OSType, Tokens) ->
             DiskData when OSType::{atom(), atom()},
                           Tokens::[any()],
                           DiskData::[#disk_data{}]).
%% For Mac OS-X
get_disk_data({unix,darwin}, RetL) ->
    F = fun([Filesystem, Blocks, Used, Available,
             _Capacity, _IUsed, _Free, UsePer, MountedOn]) ->
                #disk_data{filesystem = Filesystem,
                           blocks    = list_to_integer(Blocks),
                           used      = list_to_integer(Used),
                           available = list_to_integer(Available),
                           use_percentage_str = UsePer,
                           mounted_on = MountedOn}
        end,
    [ F(Row) || Row <- RetL ];
%% For Ubuntu/Debian, CentOS/RHEL
get_disk_data({unix,linux}, RetL) ->
    F = fun([Filesystem, Blocks, Used, Available, UsePer, MountedOn]) ->
                #disk_data{filesystem = Filesystem,
                           blocks    = list_to_integer(Blocks),
                           used      = list_to_integer(Used),
                           available = list_to_integer(Available),
                           use_percentage_str = UsePer,
                           mounted_on = MountedOn}
        end,
    [ F(Row) || Row <- RetL ];
%% @TODO For FreeBSD
%% @TODO For Solaris/SmartOS
get_disk_data(_,_) ->
    [].


%%--------------------------------------------------------------------
%% Callback
%%--------------------------------------------------------------------
%% @dog Call execution of the watchdog
-spec(handle_call(Id, State) ->
             {ok, State} | {{error,Error}, State} when Id::atom(),
                                                       State::#state{},
                                                       Error::any()).
handle_call(Id, #state{target_paths  = TargetPaths} = State) ->
    %% @TODO: get new-state, then set it in the state
    spawn(fun() ->
                  ok = check(Id, TargetPaths, State, [])
          end),
    {ok, State}.


%% @dog Call execution failed
-spec(handle_fail(Id, Cause) ->
             ok | {error,Error} when Id::atom(),
                                     Cause::any(),
                                     Error::any()).
handle_fail(_Id,_Cause) ->
    ok.


%%--------------------------------------------------------------------
%% Internal Function
%%--------------------------------------------------------------------
%% @doc Check disk-related items
%% @private
check(Id, [], State, Acc) ->
    %% Summarize result of disk use%
    lists:foreach(
      fun(DiskData) ->
              L = leo_misc:get_value(level, DiskData),
              D = leo_misc:get_value(disk_data, DiskData, []),
              M = leo_misc:get_value(mounted_on, D),
              case L of
                  ?WD_LEVEL_SAFE->
                      elarm:clear(Id, {?WD_ITEM_DISK_USE, M});
                  _ ->
                      elarm:raise(Id, {?WD_ITEM_DISK_USE, M},
                                  #watchdog_state{id = Id,
                                                  level = L,
                                                  src   = M,
                                                  props = [{?WD_ITEM_DISK_USE, D}
                                                          ]})
              end
      end, Acc),

    %% Check disk-util
    {ok, _NewState} = disk_stats(State),
    %% @TODO: return state
    ok;
check(Id, [Path|Rest], #state{threshold_disk_use = ThresholdDiskUse} = State, Acc) ->
    Acc_1 = case get_disk_data() of
                [] ->
                    Acc;
                DiskData ->
                    %% Check disk use% of a target volume
                    DiskData_1 = disk_use(string:tokens(Path, "/"), DiskData),
                    UsePercentage = DiskData_1#disk_data.use_percentage,
                    Level = case UsePercentage > ThresholdDiskUse of
                                true ->
                                    ?WD_LEVEL_ERROR;
                                false when UsePercentage >= ?WD_WARN_USE_PERCENTAGE ->
                                    ?WD_LEVEL_WARN;
                                false ->
                                    ?WD_LEVEL_SAFE
                            end,
                    DiskData_2 = lists:zip(
                                   record_info(fields, disk_data),
                                   tl(tuple_to_list(DiskData_1))),
                    [ [{level, Level},
                       {disk_data, DiskData_2}] | Acc ]
            end,
    check(Id, Rest, State, Acc_1).

%% @doc Check disk use%
%% @private
disk_use(Tokens, DiskData) ->
    Len  = length(Tokens),
    Path = case Tokens of
               [] ->
                   "/";
               _ ->
                   "/" ++ filename:join(Tokens)
           end,
    case disk_use_1(DiskData, Path) of
        not_found when Len > 1 ->
            disk_use(lists:sublist(Tokens, Len - 1), DiskData);
        not_found ->
            disk_use([], DiskData);
        Ret ->
            Ret
    end.

%% @private
disk_use_1([],_) ->
    not_found;
disk_use_1([#disk_data{
               blocks     = Blocks,
               available  = Available,
               mounted_on = Path} = Data|_], Path) ->
    Data#disk_data{use_percentage =
                       (100 - erlang:round(Available/Blocks * 100))};
disk_use_1([_|Rest], Path) ->
    disk_use_1(Rest, Path).


%% @doc Check disk util
%% @private
disk_stats(#state{id = Id,
                  target_devices = TargetDevices,
                  threshold_disk_util = ThresholdDiskUtil,
                  threshold_disk_rkb  = ThresholdRkb,
                  threshold_disk_wkb  = ThresholdWkb,
                  raised_error_times  = _RaisedThreshold} = State) ->
    DiskStats = disk_stats_1(os:type(), TargetDevices),
    #disk_stat{util = Util,
               rkb  = Rkb,
               wkb  = Wkb} = DiskStats,

    State_1 = case (Util >  ThresholdDiskUtil) of
                  true  ->
                      elarm:raise(
                        Id, ?WD_ITEM_DISK_UTIL,
                        #watchdog_state{id = Id,
                                        level = ?WD_LEVEL_ERROR,
                                        src   = ?WD_ITEM_DISK_UTIL,
                                        props = [{?WD_ITEM_DISK_UTIL, Util}
                                                ]});
                  false->
                      elarm:clear(Id, ?WD_ITEM_DISK_UTIL),
                      State
              end,
    State_2 = case ((Rkb + Wkb) > (ThresholdRkb + ThresholdWkb)) of
                  true ->
                      elarm:raise(
                        Id, ?WD_ITEM_DISK_IO,
                        #watchdog_state{id = Id,
                                        level = ?WD_LEVEL_ERROR,
                                        src   = ?WD_ITEM_DISK_IO,
                                        props = [{?WD_ITEM_DISK_RKB, Rkb},
                                                 {?WD_ITEM_DISK_WKB, Wkb}
                                                ]});
                  false ->
                      elarm:clear(Id, ?WD_ITEM_DISK_IO),
                      State_1
              end,
    {ok, State_2}.


%% @doc Retrieve io-wait for Linux(CentOS, Ubuntu)
%% @private
disk_stats_1({unix, linux}, TargetDevices) ->
    case os:cmd("which iostat") of
        [] ->
            0.0;
        _ ->
            %% Execute os-command
            CmdRet = os:cmd("iostat -x 1 2"),

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

%% @TODO solaris/smartos
%% @TODO freebsd
disk_stats_1(_,_) ->
    0.


%% @private
get_target_values(TargetDevices, Tokens, PosOfItems) ->
    DiskStats = get_taget_values_1(Tokens, PosOfItems, []),

    %% Retrieving target-data
    DiskStats_1 = get_target_values_2(TargetDevices, DiskStats, []),

    %% Retrieve max value of each item
    DiskStats_2 = max_value(DiskStats_1, #disk_stat{}),
    DiskStats_2.


%% @private
get_taget_values_1([],_,SoFar) ->
    SoFar;
get_taget_values_1([Items|Rest], PosOfItems, SoFar) ->
    RkbPos  = leo_misc:get_value(?DISK_ITEM_RKB,  PosOfItems),
    WkbPos  = leo_misc:get_value(?DISK_ITEM_WKB,  PosOfItems),
    UtilPos = leo_misc:get_value(?DISK_ITEM_UTIL, PosOfItems),

    Tokens = string:tokens(Items, " "),
    DevName = lists:nth(1, Tokens),
    RkbVal  = get_item(float, RkbPos,  Tokens),
    WkbVal  = get_item(float, WkbPos,  Tokens),
    UtilVal = get_item(float, UtilPos, Tokens),

    get_taget_values_1(Rest, PosOfItems,
                       [{DevName, #disk_stat{util = UtilVal,
                                             rkb = RkbVal,
                                             wkb = WkbVal}}|SoFar]).

%% @private
get_item(_,undefined,_) ->
    0;
get_item(_, KeyPos, Values) when KeyPos > length(Values) ->
    0;
get_item(float, KeyPos, Values) ->
    list_to_float(lists:nth(KeyPos, Values));
get_item(integer, KeyPos, Values) ->
    list_to_integer(lists:nth(KeyPos, Values));
get_item(_,_,_) ->
    0.


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
