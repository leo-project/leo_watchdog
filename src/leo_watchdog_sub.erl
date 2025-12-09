%%======================================================================
%%
%% Leo Watchdog
%%
%% Copyright (c) 2012-2018 Rakuten, Inc.
%% Copyright (c) 2019-2025 Lions Data, Ltd.
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
%%======================================================================
-module(leo_watchdog_sub).

-author('Yosuke Hara').

-behaviour(gen_server).

-include("leo_watchdog.hrl").
%% -include_lib("elarm/include/elarm.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/2,
         start_link/3,
         start_link/4,
         stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          id :: atom(),
          filter::[module()],
          callback_mod :: module()|undefined,
          consecutive_safe_times = 0 :: non_neg_integer(),
          max_safe_times = ?MAX_SAFE_TIMES :: non_neg_integer()
         }).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Start the server
-spec(start_link(Id, CallbackMod) ->
             {ok,Pid} | ignore | {error,Error} when Id::atom(),
                                                    CallbackMod::module()|undefined,
                                                    Pid::pid(),
                                                    Error::{already_started,Pid} | term()).
start_link(Id, CallbackMod) ->
    start_link(Id, [all], CallbackMod).

-spec(start_link(Id, Filters, CallbackMod) ->
             {ok,Pid} | ignore | {error,Error} when Id::atom(),
                                                    Filters::[atom()]|[{atom(),any()}],
                                                    CallbackMod::module()|undefined,
                                                    Pid::pid(),
                                                    Error::{already_started,Pid} | term()).
start_link(Id, Filters, CallbackMod) ->
    MaxSafeTimes = ?env_wd_loosen_control_at_safe_count(),
    start_link(Id, Filters, MaxSafeTimes, CallbackMod).

-spec(start_link(Id, Filters, MaxSafeTimes, CallbackMod) ->
             {ok,Pid} | ignore | {error,Error} when Id::atom(),
                                                    Filters::[atom()]|[{atom(),any()}],
                                                    MaxSafeTimes::non_neg_integer(),
                                                    CallbackMod::module()|undefined,
                                                    Pid::pid(),
                                                    Error::{already_started,Pid} | term()).
start_link(Id, Filters, MaxSafeTimes, CallbackMod) ->
    gen_server:start_link({local, Id}, ?MODULE,
                          [Id, Filters, MaxSafeTimes, CallbackMod], []).


%% @doc Stop the server
-spec(stop(Id) ->
             ok when Id::atom()).
stop(Id) ->
    gen_server:call(Id, stop).


%%--------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------------------------------
%% @doc Initiates the server
init([Id, Filter, MaxSafeTimes, CallbackMod]) ->
    erlang:send_after(?DEF_TIMEOUT, self(), trigger),
    {ok, #state{id = Id,
                filter = Filter,
                callback_mod = CallbackMod,
                max_safe_times = MaxSafeTimes}}.


%% @doc gen_server callback - Module:handle_call(Request, From, State) -> Result
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.


%% @doc Handling cast message
%% <p>
%% gen_server callback - Module:handle_cast(Request, State) -> Result.
%% </p>
handle_cast(_Msg, State) ->
    {noreply, State}.


%% @doc Handling all non call/cast messages
%% <p>
%% gen_server callback - Module:handle_info(Info, State) -> Result.
%% </p>
handle_info(_, #state{callback_mod = undefined} = State) ->
    {noreply, State};
handle_info(trigger, #state{id = Id,
                            callback_mod = Mod,
                            filter = Filter,
                            consecutive_safe_times = SafeTimes,
                            max_safe_times = MaxSafeTimes} = State) ->
    AlarmL = lists:flatten([A || {ok, A}
                                     <- [leo_watchdog_state:find_by_id(F)
                                         || {src, F} <- Filter]]),
    case AlarmL of
        %% there is no errors:
        [] ->
            SafeTimes_1 =
                case (MaxSafeTimes =< SafeTimes) of
                    true ->
                        case catch erlang:apply(Mod, handle_notify,
                                                [Id, [], MaxSafeTimes, leo_date:unixtime()]) of
                            {'EXIT', Cause} ->
                                error_logger:error_msg(
                                  "~p,~p,~p,~p~n",
                                  [{module, ?MODULE_STRING},
                                   {function, "handle_info/2"},
                                   {line, ?LINE}, {body, Cause}]);
                            _ ->
                                void
                        end,
                        1;
                    false ->
                        SafeTimes + 1
                end,
            erlang:send_after(?DEF_TIMEOUT, self(), trigger),
            {noreply, State#state{consecutive_safe_times = SafeTimes_1}};
        %% there are some errors:
        _ ->
            lists:foreach(
              fun(#watchdog_alarm{} = AItem) ->
                      case catch erlang:apply(Mod, handle_notify,
                                              [Id, AItem, leo_date:unixtime()]) of
                          {'EXIT', Cause} ->
                              error_logger:error_msg(
                                "~p,~p,~p,~p~n",
                                [{module, ?MODULE_STRING},
                                 {function, "handle_info/2"},
                                 {line, ?LINE}, {body, Cause}]);
                          _ ->
                              void
                      end,
                      ok;
                 (_) ->
                      ok
              end, AlarmL),
            erlang:send_after(?DEF_TIMEOUT, self(), trigger),
            {noreply, State#state{consecutive_safe_times = 0}}
    end;
handle_info(_, State) ->
    {noreply, State}.


%% @doc This function is called by a gen_server when it is about to
%%      terminate. It should be the opposite of Module:init/1 and do any necessary
%%      cleaning up. When it returns, the gen_server terminates with Reason.
terminate(_Reason, _State) ->
    ok.


%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
