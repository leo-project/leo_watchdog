%%======================================================================
%%
%% Leo Ordning & Reda
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
%%======================================================================
-author('Yosuke Hara').

-undef(DEF_WATCH_INTERVAL).
-define(DEF_WATCH_INTERVAL, 5000).
-undef(DEF_TIMEOUT).
-define(DEF_TIMEOUT, 5000).

-type(watchdog_id() :: atom()).

-define(WD_STATE_SAFE,  'safe').
-define(WD_STATE_WARN,  'warn').
-define(WD_STATE_ERROR, 'error').
-type(watchdog_state() :: ?WD_STATE_SAFE |
                          ?WD_STATE_WARN |
                          ?WD_STATE_ERROR).
-record(watchdog_state, {
          state = ?WD_STATE_SAFE :: watchdog_state(),
          props = [] :: [{atom(), any()}]
         }).
