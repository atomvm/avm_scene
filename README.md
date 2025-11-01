# avm_scene

A GenServer wrapper that adds graphical display capabilities to AtomVM applications.

## Overview

`avm_scene` extends the standard `gen_server` behavior by automatically handling display updates
when callbacks return `[{:push, display_list}]`.
It acts as a bridge between your application logic and the display driver, making it easy to create
interactive graphical applications.

## Features

- **Declarative Display Updates**: Update the display by simply returning `[{:push, display_list}]`
from any GenServer callback
- **Input Handling**: Optional support for input events from touch screens, buttons, or other input
devices
- **GenServer Compatible**: Works with all standard GenServer callbacks and patterns
- **Display Driver Agnostic**: Works with any AtomGL-compatible display driver

## Installation

Add `avm_scene` to your AtomVM project's dependencies.

## Basic Usage

### Erlang Example

```erlang
-module(hello_scene).
-export([start_link/2, init/1, handle_info/2]).

start_link(Args, Opts) ->
    avm_scene:start_link(?MODULE, Args, Opts).

init(_Args) ->
    erlang:send_after(100, self(), update_display),
    {ok, #{width => 320, height => 240}}.

handle_info(update_display, State = #{width := Width, height := Height}) ->
    Items = [
        {text, 10, 20, default16px, 16#000000, 16#FFFFFF, "Hello, World!"},
        {rect, 0, 0, Width, Height, 16#FFFFFF}
    ],
    {noreply, State, [{push, Items}]}.
```

### Elixir Example

```elixir
defmodule HelloScene do
  def start_link(args, opts) do
    :avm_scene.start_link(__MODULE__, args, opts)
  end

  def init(_args) do
    :erlang.send_after(100, self(), :update_display)
    {:ok, %{width: 320, height: 240}}
  end

  def handle_info(:update_display, %{width: width, height: height} = state) do
    items = [
      {:text, 10, 20, :default16px, 0x000000, 0xFFFFFF, "Hello, World!"},
      {:rect, 0, 0, width, height, 0xFFFFFF}
    ]

    {:noreply, state, [{:push, items}]}
  end
end
```

## Starting a Scene

First, open a display port using AtomGL, then start your scene with the display reference:

### Erlang

```erlang
%% Configure and open display
DisplayOpts = [
    {width, 320},
    {height, 240},
    {compatible, "ilitek,ili9341"},
    {spi_host, SpiHost},
    {cs, 22},
    {dc, 21},
    {reset, 18}
],
Display = erlang:open_port({spawn, "display"}, DisplayOpts),

%% Start the scene
{ok, Pid} = hello_scene:start_link([], [
    {display_server, {port, Display}}
]).
```

### Elixir

```elixir
# Configure and open display
display_opts = [
  width: 320,
  height: 240,
  compatible: "ilitek,ili9341",
  spi_host: spi_host,
  cs: 22,
  dc: 21,
  reset: 18
]
display = :erlang.open_port({:spawn, "display"}, display_opts)

# Start the scene
{:ok, pid} = HelloScene.start_link([], [
  display_server: {:port, display}
])
```

## Display Lists

The display list is a declarative representation of what should be shown on screen. It consists of
primitive drawing elements that are rendered in order (last item is drawn first, first item appears
on top).

### Supported Primitives

- `{rect, X, Y, Width, Height, Color}` - Filled rectangle
- `{text, X, Y, Font, TextColor, BackgroundColor, Text}` - Text rendering
- `{image, X, Y, BackgroundColor, ImageTuple}` - Image display
- `{scaled_cropped_image, ...}` - Scaled and cropped image

For complete documentation on display lists and primitives, see the
[AtomGL documentation](https://github.com/atomvm/atomgl).

## Input Handling

To handle input events (touch, buttons, etc.), implement the optional `handle_input/4` callback:

### Erlang

```erlang
handle_input(EventData, Timestamp, Pid, State) ->
    io:format("Input event: ~p at ~p~n", [EventData, Timestamp]),
    {noreply, State}.
```

### Elixir

```elixir
def handle_input(event_data, timestamp, pid, state) do
  IO.puts("Input event: #{inspect(event_data)} at #{timestamp}")
  {:noreply, state}
end
```

Then provide an `input_server` when starting the scene:

```erlang
{ok, Pid} = hello_scene:start_link([], [
    {display_server, {port, Display}},
    {input_server, InputServerPid}
]).
```

## Advanced Example: Interactive Counter

Here's a more complete example showing state management and display updates:

### Elixir

```elixir
defmodule CounterScene do
  def start_link(args, opts) do
    :avm_scene.start_link(__MODULE__, args, opts)
  end

  def init(_args) do
    # Initial render
    self() |> send(:render)

    {:ok, %{
      counter: 0,
      width: 320,
      height: 240
    }}
  end

  def handle_info(:increment, state) do
    new_state = %{state | counter: state.counter + 1}
    self() |> send(:render)
    {:noreply, new_state}
  end

  def handle_info(:render, state) do
    items = [
      # Counter display
      {:text, 100, 100, :default16px, 0x000000, :transparent,
       "Count: #{state.counter}"},

      # Increment button
      {:rect, 50, 150, 100, 40, 0x4444FF},
      {:text, 70, 165, :default16px, 0xFFFFFF, :transparent, "Increment"},

      # Background
      {:rect, 0, 0, state.width, state.height, 0xFFFFFF}
    ]

    {:noreply, state, [{:push, items}]}
  end

  def handle_input({:touch, x, y}, _timestamp, _pid, state) do
    # Check if increment button was pressed
    if x >= 50 and x <= 150 and y >= 150 and y <= 190 do
      self() |> send(:increment)
    end

    {:noreply, state}
  end
end
```

## Configuration Options

When starting an `avm_scene`, you can provide the following options:

| Option | Type | Required | Description |
|--------|------|----------|-------------|
| `display_server` | `{Module, Display}` | Yes | Display driver reference, typically `{:port, DisplayPort}` |
| `input_server` | `pid()` | No | PID of input server for receiving input events |
| Standard GenServer options | Various | No | Any options supported by `GenServer` (e.g., `name`, `timeout`) |

## How It Works

1. **Initialization**: When started, `avm_scene` wraps your module and connects to the specified
display driver
2. **Callback Interception**: It intercepts all GenServer callbacks from your module
3. **Display Updates**: When a callback returns `[{:push, display_list}]`, it automatically sends
the display list to the display driver
4. **Input Events**: If configured with an input server, it forwards input events to your
`handle_input/4` callback

## Best Practices

1. **Always include a background**: Add a full-screen rectangle as the last item in your display
list to ensure proper clearing
2. **Manage redraws efficiently**: Only push updates when the display actually needs to change
3. **Keep display lists simple**: Complex scenes with many items may impact performance on
resource-constrained devices
4. **Use `:transparent` backgrounds**: For text and images that should overlay other elements

## Troubleshooting

### Display not updating
- Ensure you're returning `[{:push, items}]` from your callback
- Verify the display port is properly opened and configured
- Check that your display list items have valid parameters

### Input events not received
- Confirm an `input_server` is provided when starting the scene
- Verify the input server supports the `{:subscribe_input}` call
- Implement the `handle_input/4` callback in your module

## Related Projects

- [AtomGL](https://github.com/atomvm/atomgl) - The display driver framework
- [AtomVM](https://github.com/atomvm/AtomVM) - The Erlang/Elixir/Gleam virtual machine for
microcontrollers (and more)

## License

Apache-2.0
