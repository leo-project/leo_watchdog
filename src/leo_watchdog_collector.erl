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
%% @doc leo_watchdog_collector
%% @reference
%% @end
%%======================================================================
-module(leo_watchdog_collector).

-author('Yosuke Hara').

-behaviour(gen_server).

-include("leo_watchdog.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0,
         start_link/1,
         stop/0,
         push/1,
         pull/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {count = 0 :: non_neg_integer(),
                set_errors = sets:new() :: [term()],
                interval = ?DEF_WATCH_INTERVAL :: pos_integer()
               }).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Start the server
-spec(start_link() ->
             {ok,Pid} | ignore | {error,Error} when Pid::pid(),
                                                    Error::{already_started,Pid} | term()).
start_link() ->
    start_link(?DEF_WATCH_INTERVAL).

-spec(start_link(IntervalTime) ->
             {ok,Pid} | ignore | {error,Error} when IntervalTime::pos_integer(),
                                                    Pid::pid(),
                                                    Error::{already_started,Pid} | term()).
start_link(IntervalTime) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
                          [IntervalTime], []).


%% @doc Stop the server
-spec(stop() ->
             ok).
stop() ->
    gen_server:call(?MODULE, stop).


%% @doc Resume the server
-spec(push(ErrorMsg) ->
             ok when ErrorMsg::term()).
push(ErrorMsg) ->
    gen_server:call(?MODULE, {push, ErrorMsg}).

%% @doc Resume the server
-spec(pull() ->
             {ok, [term()]}).
pull() ->
    gen_server:call(?MODULE, pull).


%%--------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------------------------------
%% @doc Initiates the server
init([Interval]) ->
    {ok, #state{count = 0,
                set_errors = sets:new(),
                interval = Interval}, Interval}.


%% @doc gen_server callback - Module:handle_call(Request, From, State) -> Result
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

%% @doc Suspend the server
handle_call({push, ErrorMsg},_From, #state{count = Count,
                                           set_errors = SetErrors,
                                           interval = Interval} = State) ->
    {reply, ok, State#state{count = Count + 1,
                            set_errors = sets:add_element(ErrorMsg, SetErrors)}, Interval};

handle_call(pull,_From, #state{count = Count,
                               set_errors = SetErrors,
                               interval = Interval} = State) ->
    Errors = sets:to_list(SetErrors),
    {reply, {ok, {Count, Errors}}, State#state{count = 0,
                                               set_errors = sets:new()}, Interval}.


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
handle_info(timeout, #state{interval = Interval} = State) ->
    {noreply, State#state{count = 0,
                          set_errors = sets:new()}, Interval};

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
