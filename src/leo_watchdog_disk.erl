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
-export([start_link/3,
         stop/0]).

%% Callback
-export([handle_call/2,
         handle_fail/2]).

-export([disk_util/2]).

-define(PROP_TARGET_PATHS,  'target_paths').
-define(PROP_MAX_DISK_UTIL, 'max_disk_util').
-define(DEF_DISK_USAGE, 90.0).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Start the server
-spec(start_link(TargetPaths, MaxDiskUtil, IntervalTime) ->
             {ok,Pid} | ignore | {error,Error} when TargetPaths::[string()],
                                                    MaxDiskUtil::float(),
                                                    IntervalTime::pos_integer(),
                                                    Pid::pid(),
                                                    Error::{already_started,Pid} | term()).
start_link(TargetPaths, MaxDiskUtil, IntervalTime) ->
    ok = disksup:set_check_interval(1),
    leo_watchdog:start_link(?MODULE, ?MODULE,
                            [{?PROP_TARGET_PATHS, TargetPaths},
                             {?PROP_MAX_DISK_UTIL, MaxDiskUtil}
                            ],
                            IntervalTime).


%% @doc Stop the server
-spec(stop() ->
             ok).
stop() ->
    leo_watchdog:stop(?MODULE).


%%--------------------------------------------------------------------
%% Callback
%%--------------------------------------------------------------------
%% @dog Call execution of the watchdog
-spec(handle_call(Id, State) ->
             ok | {error,Error} when Id::atom(),
                                     State::[{atom(), any()}],
                                     Error::any()).
handle_call(_Id, State) ->
    TargetPaths = leo_misc:get_value(?PROP_TARGET_PATHS,
                                     State, []),
    MaxDiskUtil = leo_misc:get_value(?PROP_MAX_DISK_UTIL,
                                     State, ?DEF_DISK_USAGE),
    ok = check_disk_usage(TargetPaths, MaxDiskUtil),
    {ok, State}.


%% @private
check_disk_usage([],_) ->
    ok;
check_disk_usage([Path|Rest], MaxDiskUtil) ->
    Tokens = string:tokens(Path, "/"),
    case disksup:get_disk_data() of
        [{"none",_,_}] ->
            void;
        DiskData ->
            case disk_util(Tokens, DiskData) of
                [] ->
                    void;
                {MountPath, TotalSize, UsedPercentage} ->
                    case UsedPercentage > MaxDiskUtil of
                        true ->
                            error_logger:warning_msg(
                              "~p,~p,~p,~p~n",
                              [{module, ?MODULE_STRING},
                               {function, "handle_call/2"},
                               {line, ?LINE}, {body, [{path,  MountPath},
                                                      {total_size, TotalSize},
                                                      {utilization, UsedPercentage}
                                                     ]}]),
                            %% @TODO:
                            %% Nofify the message to the clients
                            ok;
                        false ->
                            ok
                    end;
                _ ->
                    ok
            end
    end,
    check_disk_usage(Rest, MaxDiskUtil).

%% @private
disk_util(Tokens, DiskData) ->
    Len  = length(Tokens),
    Path = case Tokens of
               [] ->
                   "/";
               _ ->
                   "/" ++ filename:join(Tokens)
           end,
    case lists:keyfind(Path, 1, DiskData) of
        false when Len > 1 ->
            disk_util(lists:sublist(Tokens, Len - 1), DiskData);
        false ->
            disk_util([], DiskData);
        Ret ->
            Ret
    end.


%% @dog Call execution failed
-spec(handle_fail(Id, Cause) ->
             ok | {error,Error} when Id::atom(),
                                     Cause::any(),
                                     Error::any()).
handle_fail(_Id,_Cause) ->
    ok.
