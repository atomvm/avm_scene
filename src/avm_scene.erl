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

%% @doc A GenServer wrapper that adds graphical display capabilities.
%%
%% `avm_scene' extends `gen_server' behavior by automatically handling display updates
%% when callbacks return `[{push, DisplayList}]'.
%% It manages the connection to a display driver and optionally subscribes to input events.
%%
%% This module acts as a layer between your scene logic and the display driver,
%% intercepting GenServer callback return values to check for display update commands.
%% When a callback returns `[{push, DisplayList}]' as the last element of its return
%% tuple, the display list is automatically sent to the configured display driver.
%%
%% For more information about display lists and supported primitives, see
%% <a href="https://github.com/atomvm/atomgl">AtomGL documentation</a>.
%%
%% == Example Usage (Erlang) ==
%%
%% ```
%% -module(my_scene).
%% -export([start_link/2, init/1, handle_info/2]).
%%
%% start_link(Args, Opts) ->
%%     avm_scene:start_link(?MODULE, Args, Opts).
%%
%% init(_Args) ->
%%     erlang:send_after(100, self(), update_display),
%%     {ok, #{width => 320, height => 240}}.
%%
%% handle_info(update_display, State = #{width := Width, height := Height}) ->
%%     Items = [
%%         {text, 10, 20, default16px, 16#000000, 16#FFFFFF, "Hello, World!"},
%%         {rect, 0, 0, Width, Height, 16#FFFFFF}
%%     ],
%%     {noreply, State, [{push, Items}]}.
%% '''
%%
%% == Example Usage (Elixir) ==
%%
%% ```
%% defmodule MyScene do
%%   def start_link(args, opts) do
%%     :avm_scene.start_link(__MODULE__, args, opts)
%%   end
%%
%%   def init(_args) do
%%     :erlang.send_after(100, self(), :update_display)
%%     {:ok, %{width: 320, height: 240}}
%%   end
%%
%%   def handle_info(:update_display, %{width: width, height: height} = state) do
%%     items = [
%%       {:text, 10, 20, :default16px, 0x000000, 0xFFFFFF, "Hello, World!"},
%%       {:rect, 0, 0, width, height, 0xFFFFFF}
%%     ]
%%     {:noreply, state, [{:push, items}]}
%%   end
%%
%%   # Optional: handle input events
%%   def handle_input(event_data, timestamp, pid, state) do
%%     IO.puts("Input event: #{inspect(event_data)}")
%%     {:noreply, state}
%%   end
%% end
%% '''
%%
%% == Starting a Scene ==
%%
%% Erlang:
%% ```
%% %% Open display port
%% Display = erlang:open_port({spawn, "display"}, DisplayOpts),
%%
%% %% Start scene with display
%% {ok, Pid} = my_scene:start_link([], [
%%     {display_server, {port, Display}},
%%     {input_server, InputPid}  % optional
%% ]).
%% '''
%%
%% Elixir:
%% ```
%% # Open display port
%% display = :erlang.open_port({:spawn, "display"}, display_opts)
%%
%% # Start scene with display
%% {:ok, pid} = MyScene.start_link([], [
%%   display_server: {:port, display},
%%   input_server: input_pid  # optional
%% ])
%% '''
%%
%% == Options ==
%%
%% <ul>
%% <li>`{display_server, {Module, Display}}' - Required. Where `Module' is the module
%%     to call for display updates (typically `port') and `Display' is the display
%%     reference (typically a port).</li>
%% <li>`{input_server, Pid}' - Optional. PID of a GenServer that supports
%%     `{subscribe_input}' call. When provided, the scene will receive input events.</li>
%% </ul>
%%
%% == Callbacks ==
%%
%% Your scene module should implement standard GenServer callbacks plus optionally:
%%
%% <ul>
%% <li>`handle_input/4' - Called when input events are received (if `input_server' is configured)</li>
%% </ul>
%%
%% All callbacks can return an additional `[{push, DisplayList}]' element to trigger
%% a display update.
%%
%% @end

-module(avm_scene).

-behavior(gen_server).

-export([
    start/3,
    start_link/3,
    start_monitor/3
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(state, {
    module = undefined,
    display_module = undefined,
    display = undefined,
    scene_state = undefined
}).

%% @doc Starts an avm_scene process.
%%
%% @param Module The callback module implementing your scene logic
%% @param Args Arguments passed to your module's `init/1' callback
%% @param Options Options including `display_server' (required) and `input_server' (optional),
%%                plus any standard GenServer options
%% @returns Same as `gen_server:start/3'
%%
%% @see start_link/3
%% @see start_monitor/3
%% @end
-spec start(Module :: module(), Args :: any(), Options :: list()) -> gen_server:start_ret().
start(Module, Args, Options) ->
    NoDisplayOptions = lists:keydelete(display_server, 1, Options),
    GenServerOptions = lists:keydelete(input_server, 1, NoDisplayOptions),
    gen_server:start(?MODULE, [{Module, Options} | Args], GenServerOptions).

%% @doc Starts an avm_scene process linked to the current process.
%%
%% @param Module The callback module implementing your scene logic
%% @param Args Arguments passed to your module's `init/1' callback
%% @param Options Options including `display_server' (required) and `input_server' (optional),
%%                plus any standard GenServer options
%% @returns Same as `gen_server:start_link/3'
%%
%% Erlang Example:
%% ```
%% avm_scene:start_link(my_scene, [], [
%%     {display_server, {port, Display}},
%%     {name, {local, my_scene}}
%% ]).
%% '''
%%
%% Elixir Example:
%% ```
%% :avm_scene.start_link(MyScene, [], [
%%   display_server: {:port, display},
%%   name: {:local, :my_scene}
%% ])
%% '''
%%
%% @see start/3
%% @see start_monitor/3
%% @end
-spec start_link(Module :: module(), Args :: any(), Options :: list()) -> gen_server:start_ret().
start_link(Module, Args, Options) ->
    NoDisplayOptions = lists:keydelete(display_server, 1, Options),
    GenServerOptions = lists:keydelete(input_server, 1, NoDisplayOptions),
    gen_server:start_link(?MODULE, [{Module, Options} | Args], GenServerOptions).

%% @doc Starts an avm_scene process and returns both PID and monitor reference.
%%
%% @param Module The callback module implementing your scene logic
%% @param Args Arguments passed to your module's `init/1' callback
%% @param Options Options including `display_server' (required) and `input_server' (optional),
%%                plus any standard GenServer options
%% @returns `{ok, {Pid, MonitorRef}}' or `{error, Reason}'
%%
%% @see start/3
%% @see start_link/3
%% @end
-spec start_monitor(Module :: module(), Args :: any(), Options :: list()) ->
    {ok, {pid(), reference()}} | {error, any()}.
start_monitor(Module, Args, Options) ->
    NoDisplayOptions = lists:keydelete(display_server, 1, Options),
    GenServerOptions = lists:keydelete(input_server, 1, NoDisplayOptions),
    gen_server:start_monitor(?MODULE, [{Module, Options} | Args], GenServerOptions).

%% @private
%% @doc GenServer callback - initializes the scene state.
%% @end
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

%% @private
maybe_subscribe_input(InputServer) when is_pid(InputServer) ->
    gen_server:call(InputServer, {subscribe_input});
maybe_subscribe_input(_NoInput) ->
    ok.

%% @private
%% @doc GenServer callback - handles synchronous calls.
%% @end
handle_call(Msg, From, State) ->
    Module = State#state.module,
    Result = Module:handle_call(Msg, From, State#state.scene_state),
    maybe_update_scene(Result, State).

%% @private
%% @doc GenServer callback - handles asynchronous casts.
%% @end
handle_cast(Msg, State) ->
    Module = State#state.module,
    Result = Module:handle_cast(Msg, State#state.scene_state),
    maybe_update_scene(Result, State).

%% @private
%% @doc GenServer callback - handles info messages and input events.
%%
%% This callback specially handles `{input_event, Pid, Timestamp, EventData}' messages
%% by forwarding them to the scene module's `handle_input/4' callback if it exists.
%% @end
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

%% @private
%% @doc Processes callback results and triggers display updates when needed.
%%
%% Checks if the callback returned a `[{push, Scene}]' directive and if so,
%% sends the scene to the display driver via `DisplayModule:call(Display, {update, Scene})'.
%% @end
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
