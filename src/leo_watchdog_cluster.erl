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
%% @doc Watchdog for Cluster
%% @reference
%% @end
%%======================================================================
-module(leo_watchdog_cluster).

-author('Yosuke Hara').

-behaviour(leo_watchdog_behaviour).

-include("leo_watchdog.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/2,
         stop/0,
         state/0
        ]).

%% Callback
-export([init/1,
         update_property/3,
         handle_call/2,
         handle_fail/2]).

-record(state, {
          check_cluster_state_mfa :: {module(), atom(), [any()]},
          interval = timer:seconds(1) :: pos_integer()
         }).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Start the server
-spec(start_link(CheckClusterStateMFA, Interval) ->
             {ok,Pid} |
             ignore |
             {error,Error} when CheckClusterStateMFA::{Mod, Method, Args},
                                Mod::module(),
                                Method::atom(),
                                Args::[any()],
                                Interval::pos_integer(),
                                Pid::pid(),
                                Error::{already_started,Pid} | term()).
start_link(CheckClusterStateMFA, Interval) ->
    State = #state{check_cluster_state_mfa = CheckClusterStateMFA,
                   interval = Interval},
    leo_watchdog:start_link(?MODULE, ?MODULE, State, Interval).


%% @doc Stop the server
-spec(stop() ->
             ok).
stop() ->
    leo_watchdog:stop(?MODULE).


%% @doc Retrieves state of the watchdog
-spec(state() ->
             not_found).
state() ->
    not_found.


%%--------------------------------------------------------------------
%% Callback
%%--------------------------------------------------------------------
%% @doc Initialize this process
-spec(init(State) ->
             ok | {error, Cause} when State::any(),
                                      Cause::any()).
init(_State) ->
    ok.


%% @doc Update the item's value
-spec(update_property(Item, Value, State) ->
             #state{} when Item::atom(),
                           Value::any(),
                           State::#state{}).
update_property(_,_, State) ->
    State.


%% @doc Call execution of the watchdog
-spec(handle_call(Id, State) ->
             {ok, State} |
             {{error,Error}, State} when Id::atom(),
                                         State::#state{},
                                         Error::any()).
handle_call(Id, #state{check_cluster_state_mfa = CheckClusterStateMFA} = State) ->
    %% Retrieves a watchdog level from a cluster state
    case CheckClusterStateMFA of
        {undefined,_,_} ->
            void;
        {_,undefined,_} ->
            void;
        {Mod, Method, Args} ->
            case catch erlang:apply(Mod, Method, Args) of
                {ok, Level} when Level >= ?WD_LEVEL_ERROR ->
                    Props = [{level, Level}],
                    error_logger:warning_msg("~p,~p,~p,~p~n",
                                             [{module, ?MODULE_STRING},
                                              {function, "handle_call/2"},{line, ?LINE},
                                              {body, [{result, error}] ++ Props}]),
                    catch elarm:raise(Id, ?WD_ITEM_CLUSTER,
                                      #watchdog_state{id = Id,
                                                      level = Level,
                                                      src   = ?WD_ITEM_CLUSTER,
                                                      props = Props});
                {ok, Level} when Level >= ?WD_LEVEL_WARN ->
                    Props = [{level, Level}],
                    error_logger:warning_msg("~p,~p,~p,~p~n",
                                             [{module, ?MODULE_STRING},
                                              {function, "handle_call/2"},{line, ?LINE},
                                              {body, [{result, warn}] ++ Props}]),
                    catch elarm:raise(Id, ?WD_ITEM_CLUSTER,
                                      #watchdog_state{id = Id,
                                                      level = Level,
                                                      src   = ?WD_ITEM_CLUSTER,
                                                      props = Props});
                {ok,_Level} ->
                    catch elarm:clear(Id, ?WD_ITEM_CLUSTER);
                {_, Cause} ->
                    error_logger:error_msg("~p,~p,~p,~p~n",
                                           [{module, ?MODULE_STRING},
                                            {function, "handle_call/2"},
                                            {line, ?LINE}, {body, Cause}]),
                    catch elarm:clear(Id, ?WD_ITEM_CLUSTER)
            end
    end,
    {ok, State}.


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
