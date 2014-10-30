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
-module(leo_watchdog_state).

-author('Yosuke Hara').

-behaviour(gen_server).

-include("leo_watchdog.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0,
         stop/0]).
-export([get/1,
         put/2,
         find_not_safe_items/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          items = [] :: [{watchdog_id(), #watchdog_state{}}]
         }).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Start the server
-spec(start_link() ->
             {ok,Pid} | ignore | {error,Error} when Pid::pid(),
                                                    Error::{already_started,Pid} | term()).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% @doc Stop the server
-spec(stop() ->
             ok).
stop() ->
    gen_server:call(?MODULE, stop).


%% @doc Retrieve the current status by id
-spec(get(WatchdogId) ->
             ok when WatchdogId::atom()).
get(WatchdogId) ->
    gen_server:call(?MODULE, {get, WatchdogId}, ?DEF_TIMEOUT).


%% @doc Put the current status
-spec(put(WatchdogId, CurState) ->
             ok when WatchdogId::atom(),
                     CurState::#watchdog_state{}).
put(WatchdogId, CurState) ->
    gen_server:call(?MODULE, {put, WatchdogId, CurState}, ?DEF_TIMEOUT).


%% @doc Retrieve the states of not safe
-spec(find_not_safe_items() ->
             {ok, Items} |
             not_found when Items::[{atom(), #watchdog_state{}}]).
find_not_safe_items() ->
    gen_server:call(?MODULE, find_not_safe_items, ?DEF_TIMEOUT).


%%--------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------------------------------
%% @doc Initiates the server
init([]) ->
    {ok, #state{items = []}}.


%% @doc gen_server callback - Module:handle_call(Request, From, State) -> Result
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({get, WatchdogId}, _From, #state{items = Items} = State) ->
    Reply = leo_misc:get_value(WatchdogId, Items, not_found),
    {reply, Reply, State};

handle_call({put, WatchdogId, CurState}, _From, #state{items = Items} = State) ->
    NewItems =
        case leo_misc:get_value(WatchdogId, Items, not_found) of
            not_found ->
                [{WatchdogId, CurState}|Items];
            Val ->
                [{WatchdogId, CurState}|
                 lists:delete({WatchdogId, Val}, Items)]
        end,
    {reply, ok, State#state{items = NewItems}};

handle_call(find_not_safe_items, _From, #state{items = []} = State) ->
    {reply, not_found, State};
handle_call(find_not_safe_items, _From, #state{items = Items} = State) ->
    Ret = case [WatchdogId || {WatchdogId, #watchdog_state{
                                     state = ?WD_STATE_ERROR}} <- Items] of
              [] ->
                  not_found;
              Items_1 ->
                  {ok, Items_1}
          end,
    {reply, Ret, State}.


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
