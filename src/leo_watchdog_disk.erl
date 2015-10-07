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
%% @doc leo_watchdog for Disk
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
         stop/0,
         state/0,
         disk_use/2,
         get_disk_data/0
        ]).

%% Callback
-export([init/1,
         update_property/3,
         handle_call/2,
         handle_fail/2
        ]).

-record(state, {
          id :: atom(),
          %% for disk-use
          target_paths = [] :: [string()],
          threshold_disk_use  = 100  :: non_neg_integer(),
          %% for other disk-stats
          target_devices = [] :: [string()],
          threshold_disk_util = 90.0 :: float(),
          threshold_disk_rkb  = 6400 :: non_neg_integer(),
          threshold_disk_wkb  = 6400 :: non_neg_integer(),
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
    start_link(TargetPaths, [], ThresholdDiskUse, ThresholdDiskUtil,
               ?DEF_DISK_READ_KB, ?DEF_DISK_WRITE_KB,
               ?DEF_RAISED_ERROR_TIMES,
               IntervalTime).


%% @doc Start the server
-spec(start_link(TargetPaths, TargetDevices,
                 ThresholdDiskUse, ThresholdDiskUtil,
                 ThresholdRkb, ThresholdWkb,
                 RaisedErrorTimes, IntervalTime) ->
             {ok,Pid} | ignore | {error,Error} when TargetPaths::[string()],
                                                    TargetDevices::[string()],
                                                    ThresholdDiskUse::non_neg_integer(),
                                                    ThresholdDiskUtil::float(),
                                                    ThresholdRkb::non_neg_integer(),
                                                    ThresholdWkb::non_neg_integer(),
                                                    RaisedErrorTimes::non_neg_integer(),
                                                    IntervalTime::pos_integer(),
                                                    Pid::pid(),
                                                    Error::{already_started,Pid} | term()).
start_link(TargetPaths, TargetDevices,
           ThresholdDiskUse, ThresholdDiskUtil,
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


%% @doc Retrieves state of the watchdog
-spec(state() ->
             {ok, State}|not_found when State::[{atom(), any()}]).
state() ->
    case catch leo_watchdog:state(leo_watchdog_disk) of
        {ok, State} ->
            Props = leo_misc:get_value('properties', State),
            Props_1 = lists:zip(record_info(fields, state),tl(tuple_to_list(Props))),
            {ok, State ++ Props_1};
        _ ->
            not_found
    end.


%% @doc Retrieve disk data from os command
-spec(get_disk_data() ->
             DiskData when DiskData::[#disk_data{}]).
get_disk_data() ->
    Tokens_1 = string:tokens(os:cmd("df -lkP"), "\n"),
    Tokens_2 = lists:delete(hd(Tokens_1), Tokens_1),
    Tokens_3 = [ string:tokens(Row, " ") || Row <- Tokens_2 ],
    get_disk_data(os:type(), Tokens_3).

-spec(get_disk_data(OSType, Tokens) ->
             DiskData when OSType::{atom(), atom()},
                           Tokens::[any()],
                           DiskData::[#disk_data{}]).
%% For Mac OS-X
get_disk_data({unix,darwin}, RetL) ->
    F = fun(Args) ->
                [Filesystem, Blocks, Used, Available, UsePer, MountedOn] =
                    case length(Args) of
                        9 ->
                            [El_1, El_2, El_3, El_4,_,_,_, El_5, El_6] = Args,
                            [El_1, El_2, El_3, El_4, El_5, El_6];
                        6 ->
                            Args;
                        _ ->
                            ["","0","0","0","0%",""]
                    end,

                #disk_data{filesystem = Filesystem,
                           blocks    = list_to_integer(Blocks),
                           used      = list_to_integer(Used),
                           available = list_to_integer(Available),
                           use_percentage_str = UsePer,
                           mounted_on = MountedOn}
        end,
    [ F(Row) || Row <- RetL ];
%% For Ubuntu/Debian, CentOS/RHEL, FreeBSD, Solaris/SmartOS
get_disk_data({unix, Type}, RetL) when Type =:= linux;
                                       Type =:= freebsd;
                                       Type =:= sunos ->
    F = fun([Filesystem, Blocks, Used, Available, UsePer, MountedOn]) ->
                ListToIntF = fun(S) ->
                                     case catch list_to_integer(S) of
                                         {'EXIT',_} -> 0;
                                         V -> V
                                     end
                             end,
                #disk_data{filesystem = Filesystem,
                           blocks = ListToIntF(Blocks),
                           used = ListToIntF(Used),
                           available = ListToIntF(Available),
                           use_percentage_str = UsePer,
                           mounted_on = MountedOn}
        end,
    [ F(Row) || Row <- RetL ];
%% Other OSes not supported
get_disk_data(_,_) ->
    [].


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


%%--------------------------------------------------------------------
%% Callback
%%--------------------------------------------------------------------
%% @doc Initialize this process
-spec(init(State) ->
             ok | {error, Cause} when State::any(),
                                      Cause::any()).
init(#state{target_devices = Devices}) ->
    spawn(fun() ->
                  leo_watchdog_sup:start_child(
                    iostat, [os:type(), Devices], ?DEF_CHECK_INTERVAL)
          end),
    ok.


%% @doc Update the item's value
-spec(update_property(Item, Value, State) ->
             #state{} when Item::atom(),
                           Value::any(),
                           State::#state{}).
update_property(raised_error_times, Value, State) ->
    State#state{raised_error_times = Value};
update_property(threshold_disk_use, Value, State) ->
    State#state{threshold_disk_use = Value};
update_property(threshold_disk_util, Value, State) ->
    State#state{threshold_disk_util = Value};
update_property(threshold_disk_rkb, Value, State) ->
    State#state{threshold_disk_rkb = Value};
update_property(threshold_disk_wkb, Value, State) ->
    State#state{threshold_disk_wkb = Value};
update_property(_,_, State) ->
    State.


%% @doc Call execution of the watchdog
-spec(handle_call(Id, State) ->
             {ok, State} | {{error,Error}, State} when Id::atom(),
                                                       State::#state{},
                                                       Error::any()).
handle_call(Id, #state{target_paths  = TargetPaths} = State) ->
    %% Retrieve new-state, then set it in the state
    {ok, State_1} = check(Id, TargetPaths, State, []),
    {ok, State_1}.


%% @doc Call execution failed
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
check(Id, [], #state{
                 raised_error_times = RaisedErrorTimes,
                 cur_error_times    = CurErrorTimes} = State, Acc) ->
    %% Summarize result of disk use%
    lists:foreach(
      fun(DiskData) ->
              L = leo_misc:get_value(level, DiskData),
              D = leo_misc:get_value(disk_data, DiskData, []),
              M = leo_misc:get_value(mounted_on, D),
              case L of
                  ?WD_LEVEL_SAFE->
                      catch elarm:clear(Id, ?WD_ITEM_DISK_USE);
                  _ ->
                      Props = [{?WD_ITEM_DISK_USE, D},
                               {mounted_on, M}],
                      error_logger:warning_msg("~p,~p,~p,~p~n",
                                               [{module, ?MODULE_STRING},
                                                {function, "check/4"},{line, ?LINE},
                                                {body, [{result, error}] ++ Props}]),
                      catch elarm:raise(Id, ?WD_ITEM_DISK_USE,
                                        #watchdog_state{id = Id,
                                                        level = L,
                                                        src   = M,
                                                        props = Props})
              end
      end, Acc),

    %% Check disk-util
    {ok, DiskStats} = disk_stats(State),
    LevelDiskUtil = leo_misc:get_value(?WD_ITEM_DISK_UTIL, DiskStats, 0),
    LevelDiskIO   = leo_misc:get_value(?WD_ITEM_DISK_IO,   DiskStats, 0),
    CurErrorTimes_1 =
        case (LevelDiskUtil >= ?WD_LEVEL_WARN orelse
              LevelDiskIO   >= ?WD_LEVEL_WARN) of
            true when CurErrorTimes >= RaisedErrorTimes ->
                0;
            true ->
                CurErrorTimes + 1;
            false ->
                0
        end,
    {ok, State#state{cur_error_times = CurErrorTimes_1}};

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


%% @doc Check disk util
%% @private
-spec(disk_stats(State) ->
             {ok, term()} when State::#state{}).
disk_stats(State) ->
    DiskStats = leo_watchdog_iostat:get(),
    disk_stats_1(DiskStats, State).

%% @private
disk_stats_1({ok, #disk_stat{util = Util,
                             rkb  = Rkb,
                             wkb  = Wkb} = DiskStats},
             #state{id = Id,
                    threshold_disk_util = ThresholdDiskUtil,
                    threshold_disk_rkb  = ThresholdRkb,
                    threshold_disk_wkb  = ThresholdWkb,
                    raised_error_times  = RaisedThreshold,
                    cur_error_times     = CurErrorTimes
                   }) ->
    ErrorLevel =  case (CurErrorTimes + 1 >= RaisedThreshold) of
                      true  -> ?WD_LEVEL_ERROR;
                      false -> ?WD_LEVEL_WARN
                  end,

    Props_1 = [{?WD_ITEM_DISK_UTIL, Util}],
    State_1 = case (Util >  ThresholdDiskUtil) of
                  true ->
                      error_logger:warning_msg("~p,~p,~p,~p~n",
                                               [{module, ?MODULE_STRING},
                                                {function, "disk_stats_1/2"},{line, ?LINE},
                                                {body, [{result, error}] ++ Props_1}]),
                      catch elarm:raise(
                              Id, ?WD_ITEM_DISK_UTIL,
                              #watchdog_state{id = Id,
                                              level = ErrorLevel,
                                              src   = ?WD_ITEM_DISK_UTIL,
                                              props = Props_1}),
                      [{?WD_ITEM_DISK_UTIL, ErrorLevel}];
                  false ->
                      catch elarm:clear(Id, ?WD_ITEM_DISK_UTIL),
                      [{?WD_ITEM_DISK_UTIL, ?WD_LEVEL_SAFE}]
              end,
    Props_2 = [{?WD_ITEM_DISK_RKB, Rkb},
               {?WD_ITEM_DISK_WKB, Wkb}],
    State_2 = case ((Rkb + Wkb) > (ThresholdRkb + ThresholdWkb)) of
                  true ->
                      error_logger:warning_msg("~p,~p,~p,~p~n",
                                               [{module, ?MODULE_STRING},
                                                {function, "disk_stats_1/2"},{line, ?LINE},
                                                {body, [{result, error}] ++ Props_2}]),
                      catch elarm:raise(
                              Id, ?WD_ITEM_DISK_IO,
                              #watchdog_state{id = Id,
                                              level = ErrorLevel,
                                              src   = ?WD_ITEM_DISK_IO,
                                              props = Props_2}),
                      [{?WD_ITEM_DISK_IO, ErrorLevel}|State_1];
                  false ->
                      catch elarm:clear(Id, ?WD_ITEM_DISK_IO),
                      [{?WD_ITEM_DISK_IO, ?WD_LEVEL_SAFE}|State_1]
              end,
    DiskStats_2 = lists:zip(
                    record_info(fields, disk_stat),
                    tl(tuple_to_list(DiskStats))),
    {ok, lists:merge(State_2, DiskStats_2)}.
