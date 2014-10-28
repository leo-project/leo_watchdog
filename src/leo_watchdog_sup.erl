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
-module(leo_watchdog_sup).

-author('Yosuke Hara').

-behaviour(supervisor).

-include("leo_watchdog.hrl").
-include_lib("eunit/include/eunit.hrl").

%% External API
-export([start_link/0,
         start_child/3
        ]).

%% Callbacks
-export([init/1]).


%%-----------------------------------------------------------------------
%% API-1
%%-----------------------------------------------------------------------
%% @doc Creates a supervisor process as part of a supervision tree
%% @end
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%%-----------------------------------------------------------------------
%% API-2
%%-----------------------------------------------------------------------
%% @doc Creates the gen_server process as part of a supervision tree
%% @end
-spec(start_child(rex, MaxMemCapacity, CheckInterval) ->
             ok | no_return() when MaxMemCapacity::pos_integer(),
                                   CheckInterval::pos_integer()).
start_child(rex, MaxMemCapacity, CheckInterval) ->
    ChildSpec = {leo_watchdog_rex,
                 {leo_watchdog_rex, start_link,
                  [MaxMemCapacity, CheckInterval]},
                 permanent,
                 2000,
                 worker,
                 [leo_watchdog_rex]},

    case supervisor:start_child(?MODULE, ChildSpec) of
        {ok, _Pid} ->
            ok;
        Cause ->
            {error, Cause}
    end.


%% ---------------------------------------------------------------------
%% Callbacks
%% ---------------------------------------------------------------------
%% @doc supervisor callback - Module:init(Args) -> Result
%% @end
init([]) ->
    {ok, {{one_for_one, 5, 60}, []}}.

