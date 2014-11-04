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
-export([start_link/5,
         stop/0]).

%% Callback
-export([handle_call/2,
         handle_fail/2]).

-export([disk_util/2,
         get_disk_data/0
        ]).

-define(PROP_TARGET_PATHS,  'target_paths').
-define(DEF_DISK_USAGE, 90.0).

-record(state, {
          target_paths = []   :: [string()],
          max_disk_util = 0.0 :: float(),
          max_io_wait   = 0   :: non_neg_integer(),
          callback_mod        :: module()
         }).

-record(disk_data, {
          filesystem = [] :: string(),
          blocks = 0 :: non_neg_integer(),
          used = 0   :: non_neg_integer(),
          available  :: non_neg_integer(),
          use_percentage = [] :: string(),
          mounted_on = [] :: string()
         }).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Start the server
-spec(start_link(TargetPaths, MaxDiskUtil, MaxIoWait,
                 CallbackMod, IntervalTime) ->
             {ok,Pid} | ignore | {error,Error} when TargetPaths::[string()],
                                                    MaxDiskUtil::float(),
                                                    MaxIoWait::non_neg_integer(),
                                                    CallbackMod::module(),
                                                    IntervalTime::pos_integer(),
                                                    Pid::pid(),
                                                    Error::{already_started,Pid} | term()).
start_link(TargetPaths, MaxDiskUtil, MaxIoWait, CallbackMod, IntervalTime) ->
    leo_watchdog:start_link(?MODULE, ?MODULE,
                            #state{target_paths  = TargetPaths,
                                   max_disk_util = MaxDiskUtil,
                                   max_io_wait   = MaxIoWait,
                                   callback_mod  = CallbackMod}, IntervalTime).


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
                           use_percentage = UsePer,
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
                           use_percentage = UsePer,
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
%% <pre>
%% Stored state of format:
%% [{level,error},
%%  {state, [{disk_util, [[{level, error},
%%                         {disk_data,
%%                           [{filesystem, "/dev/mapper/ubuntu--vg-root"},
%%                            {blocks, 114477656},
%%                            {used, 62649652},
%%                            {available, 45989772},
%%                            {use_percentage, 60},
%%                            {mounted_on, "/"}
%%                           ]
%%                         }]]},
%%           {io_wait, 0}]
%%  }]
%% </pre>
-spec(handle_call(Id, State) ->
             {ok, State} | {{error,Error}, State} when Id::atom(),
                                                       State::#state{},
                                                       Error::any()).
handle_call(_Id, #state{target_paths  = TargetPaths} = State) ->
    ok = check(TargetPaths, State, []),
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
check([], #state{callback_mod  = CallbackMod} = State, Acc) ->
    DiskUtilLevel = lists:foldl(
                      fun(DiskData, SoFar) ->
                              case leo_misc:get_value(level, DiskData) of
                                  ?WD_LEVEL_ERROR = L ->
                                      L;
                                  ?WD_LEVEL_WARN = L when SoFar == ?WD_LEVEL_SAFE ->
                                      L;
                                  _ ->
                                      SoFar
                              end
                      end, ?WD_LEVEL_SAFE, Acc),

    {IoWaitLevel, IoWait} = io_wait(State),
    RetLevel = case DiskUtilLevel of
                   ?WD_LEVEL_ERROR ->
                       DiskUtilLevel;
                   ?WD_LEVEL_WARN when IoWaitLevel == ?WD_LEVEL_ERROR ->
                       ?WD_LEVEL_ERROR;
                   ?WD_LEVEL_WARN ->
                       DiskUtilLevel;
                   ?WD_LEVEL_SAFE when IoWaitLevel == ?WD_LEVEL_ERROR ->
                       ?WD_LEVEL_ERROR;
                   ?WD_LEVEL_SAFE ->
                       DiskUtilLevel
               end,
    Ret = [{disk_util, Acc},
           {io_wait, IoWait}],

    ?notify_msg(?MODULE, CallbackMod, RetLevel, Ret),
    catch leo_watchdog_state:put(?MODULE, Ret),
    ok;
check([Path|Rest], #state{max_disk_util = MaxDiskUtil} = State, Acc) ->
    Acc_1 = case get_disk_data() of
                [] ->
                    Acc;
                DiskData ->
                    DiskData_1 = disk_util(string:tokens(Path, "/"), DiskData),
                    Level = case DiskData_1#disk_data.use_percentage > MaxDiskUtil of
                                true ->
                                    ?WD_LEVEL_ERROR;
                                false ->
                                    ?WD_LEVEL_SAFE
                            end,
                    DiskData_2 = lists:zip(
                                   record_info(fields, disk_data),
                                   tl(tuple_to_list(DiskData_1))),
                    [ [{level, Level},
                       {disk_data, DiskData_2}] | Acc ]
            end,
    check(Rest, State, Acc_1).

%% @doc
%% @private
disk_util(Tokens, DiskData) ->
    Len  = length(Tokens),
    Path = case Tokens of
               [] ->
                   "/";
               _ ->
                   "/" ++ filename:join(Tokens)
           end,
    case disk_util_1(DiskData, Path) of
        not_found when Len > 1 ->
            disk_util(lists:sublist(Tokens, Len - 1), DiskData);
        not_found ->
            disk_util([], DiskData);
        Ret ->
            Ret
    end.

%% @private
disk_util_1([],_) ->
    not_found;
disk_util_1([#disk_data{
                blocks     = Blocks,
                available  = Available,
                mounted_on = Path} = Data|_], Path) ->
    Data#disk_data{use_percentage =
                       (100 - erlang:round(Available/Blocks * 100))};
disk_util_1([_|Rest], Path) ->
    disk_util_1(Rest, Path).


%% @private
io_wait(#state{max_io_wait = MaxIoWait}) ->
    IoWait = io_wait_1(os:type()),
    Level  = case (IoWait >  MaxIoWait) of
                 true ->
                     ?WD_LEVEL_ERROR;
                 false ->
                     ?WD_LEVEL_SAFE
             end,
    {Level, IoWait}.

%% @doc Retrieve io-wait for Linux
%% @private
io_wait_1({unix, linux}) ->
    list_to_integer(
      erlang:hd(
        string:tokens(
          os:cmd("vmstat | tail -n 1 | awk '{print $16}'"), "\n")));
%% @TODO solaris/smartos
%% @TODO freebsd
io_wait_1(_) ->
    0.
