%
% This file is part of AtomVM.
%
% Copyright 2021-2022 Davide Bettio <davide@uninstall.it>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0
%

-module(avm_scene).

-behavior(gen_server).

-export([
    start_link/3
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(state, {
    module = undefined, display_module = undefined, display = undefined, scene_state = undefined
}).

start_link(Module, Args, Options) ->
    NoDisplayOptions = lists:keydelete(display_server, 1, Options),
    GenServerOptions = lists:keydelete(input_server, 1, NoDisplayOptions),
    gen_server:start_link(?MODULE, [{Module, Options} | Args], GenServerOptions).

init([{Module, Options} | Args]) ->
    DisplayServer = proplists:get_value(display_server, Options),
    case DisplayServer of
        {DisplayModule, Display} when is_atom(DisplayModule) ->
            InputServer = proplists:get_value(input_server, Options),
            maybe_subscribe_input(InputServer),
            {ok, SceneState} = Module:init(Args),
            {ok, #state{
                module = Module,
                display_module = DisplayModule,
                display = Display,
                scene_state = SceneState
            }};
        _Invalid ->
            {stop, {error, invalid_display}}
    end.

maybe_subscribe_input(InputServer) when is_pid(InputServer) ->
    gen_server:call(InputServer, {subscribe_input});
maybe_subscribe_input(_NoInput) ->
    ok.

handle_call(Msg, From, State) ->
    Module = State#state.module,
    Result = Module:handle_call(Msg, From, State#state.scene_state),
    maybe_update_scene(Result, State).

handle_cast(Msg, State) ->
    Module = State#state.module,
    Result = Module:handle_cast(Msg, State#state.scene_state),
    maybe_update_scene(Result, State).

handle_info({input_event, Pid, Ts, EventData}, State) when is_pid(Pid) and is_integer(Ts) ->
    Module = State#state.module,
    case erlang:function_exported(Module, handle_input, 4) of
        true ->
            Result = Module:handle_input(EventData, Ts, Pid, State#state.scene_state),
            maybe_update_scene(Result, State);
        false ->
            {noreply, State}
    end;
handle_info(Msg, State) ->
    Module = State#state.module,
    Result = Module:handle_info(Msg, State#state.scene_state),
    maybe_update_scene(Result, State).

maybe_update_scene(Result, State) ->
    case Result of
        {Action, Reply, NewSceneState, [{push, Scene}]} ->
            DisplayModule = State#state.display_module,
            DisplayModule:call(State#state.display, {update, Scene}),
            {Action, Reply, State#state{scene_state = NewSceneState}};
        {Action, NewSceneState, [{push, Scene}]} ->
            DisplayModule = State#state.display_module,
            DisplayModule:call(State#state.display, {update, Scene}),
            {Action, State#state{scene_state = NewSceneState}};
        {Action, Reply, NewSceneState} ->
            {Action, Reply, State#state{scene_state = NewSceneState}};
        {Action, NewSceneState} ->
            {Action, State#state{scene_state = NewSceneState}}
    end.
