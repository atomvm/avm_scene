# avm_scene

avm_scene adds graphical capabilities on top of `gen_server`, such as pushing graphical updates on
`handle_info` or `handle_call`.

```elixir
  [...]

  def handle_info(:show_hello, state) do
    # grapical items
    items = [...]

    {:noreply, state, [{:push, items}]}
  end

  [...]
```
