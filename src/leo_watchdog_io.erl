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
%% @doc Watchdog for IO
%% @reference
%% @end
%%======================================================================
-module(leo_watchdog_io).

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

-define(PROP_MAX_INPUT_FOR_INTERVAL,  'max_input').
-define(PROP_MAX_OUTPUT_FOR_INTERVAL, 'max_output').
-define(DEF_INPUT,  134217728). %% 128MB
-define(DEF_OUTPUT, 134217728). %% 128MB
-define(VAL_INPUT_FOR_INTERVAL,  'val_input').
-define(VAL_OUTPUT_FOR_INTERVAL, 'val_output').


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Start the server
-spec(start_link(MaxInputForInterval, MaxOutputForInterval, IntervalTime) ->
             {ok,Pid} | ignore | {error,Error} when MaxInputForInterval::non_neg_integer(),
                                                    MaxOutputForInterval::non_neg_integer(),
                                                    IntervalTime::pos_integer(),
                                                    Pid::pid(),
                                                    Error::{already_started,Pid} | term()).
start_link(MaxInputForInterval, MaxOutputForInterval, IntervalTime) ->
    leo_watchdog:start_link(?MODULE, ?MODULE,
                            [{?PROP_MAX_INPUT_FOR_INTERVAL,  MaxInputForInterval},
                             {?PROP_MAX_OUTPUT_FOR_INTERVAL, MaxOutputForInterval},
                             {?VAL_INPUT_FOR_INTERVAL,  MaxInputForInterval},
                             {?VAL_OUTPUT_FOR_INTERVAL, MaxOutputForInterval}
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
    MaxInput   = leo_misc:get_value(?PROP_MAX_INPUT_FOR_INTERVAL,  State, ?DEF_INPUT),
    MaxOutput  = leo_misc:get_value(?PROP_MAX_OUTPUT_FOR_INTERVAL, State, ?DEF_OUTPUT),
    PrevInput  = leo_misc:get_value(?VAL_INPUT_FOR_INTERVAL,  State, 0),
    PrevOutput = leo_misc:get_value(?VAL_OUTPUT_FOR_INTERVAL, State, 0),

    RetL = tuple_to_list(erlang:statistics(io)),
    CurInput  = leo_misc:get_value('input',  RetL, 0),
    CurOutput = leo_misc:get_value('output', RetL, 0),
    DiffInput  = CurInput  - PrevInput,
    DiffOutput = CurOutput - PrevOutput,
    ?debugVal({DiffInput, DiffOutput}),

    case (DiffInput  > MaxInput orelse
          DiffOutput > MaxOutput) of
        true ->
            error_logger:warning_msg(
              "~p,~p,~p,~p~n",
              [{module, ?MODULE_STRING},
               {function, "handle_call/2"},
               {line, ?LINE}, {body, [{diff_input,  DiffInput},
                                      {diff_output, DiffOutput}
                                     ]}]),
            %% @TODO:
            %% Nofify the message to the clients
            ?debugVal({'over_threshold', DiffInput, DiffOutput}),
            ok;
        false ->
            ok
    end,
    {ok, [{?PROP_MAX_INPUT_FOR_INTERVAL,  MaxInput},
          {?PROP_MAX_OUTPUT_FOR_INTERVAL, MaxOutput},
          {?VAL_INPUT_FOR_INTERVAL,  CurInput},
          {?VAL_OUTPUT_FOR_INTERVAL, CurOutput}
         ]}.

%% @dog Call execution failed
-spec(handle_fail(Id, Cause) ->
             ok | {error,Error} when Id::atom(),
                                     Cause::any(),
                                     Error::any()).
handle_fail(_Id,_Cause) ->
    ok.
