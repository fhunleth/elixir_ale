defmodule Gpio do
 use GenServer

  defmodule State do
    defstruct port: nil, pin: 0, direction: nil, callbacks: []
  end

  # Public API
  def start_link(pin, pin_direction) do
    GenServer.start_link(__MODULE__, [pin, pin_direction])
  end

  @doc """
  Free the resources associated with pin.
  """
  def release(pid) do
    GenServer.cast pid, :release
  end

  def write(pid, value) do
    GenServer.call pid, {:write, value}
  end

  def read(pid) do
    GenServer.call pid, :read
  end

  def set_int(pid, direction) do
    true = pin_interrupt_condition?(direction)
    GenServer.call pid, {:set_int, direction, self}
  end

  # gen_server callbacks
  def init([pin, pin_direction]) do
    executable = :code.priv_dir(:elixir_ale) ++ '/gpio_port'
    port = Port.open({:spawn_executable, executable},
    [{:args, ["#{pin}", Atom.to_string(pin_direction)]},
      {:packet, 2},
      :use_stdio,
      :binary,
      :exit_status])
    state = %State{port: port, pin: pin, direction: pin_direction}
    {:ok, state}
  end

  def handle_call(:read, _from, state) do
    {:ok, response} = call_port(state, :read, [])
    {:reply, response, state}
  end
  def handle_call({:write, value}, _from, state) do
    {:ok, response} = call_port(state, :write, [value])
    {:reply, response, state}
  end
  def handle_call({:set_int, direction, requestor}, _from, state) do
    {:ok, response} = call_port(state, :set_int, [direction])
    new_callbacks = insert_unique(state.callbacks, requestor)
    state = %{state | callbacks: new_callbacks }
    {:reply, response, state}
  end

  def handle_cast(:release, state) do
    {:stop, :normal, state}
  end

  def handle_info({_, {:data, message}}, state) do
    msg = :erlang.binary_to_term(message)
    handle_port(msg, state)
  end

  defp call_port(state, command, arguments) do
    msg = {command, arguments}
    send state.port, {self, {:command, :erlang.term_to_binary(msg)}}
    receive do
      {_, {:data, response}} ->
        {:ok, :erlang.binary_to_term(response)}
        _ -> :error
    end
  end

  defp handle_port({:gpio_interrupt, condition}, state) do
    #IO.puts "Got interrupt on pin #{state.pin}, #{condition}"
    msg = {:gpio_interrupt, state.pin, condition}
    for pid <- state.callbacks do
      send pid, msg
    end
    {:noreply, state}
  end

  defp pin_interrupt_condition?(:rising), do: true
  defp pin_interrupt_condition?(:falling), do: true
  defp pin_interrupt_condition?(:both), do: true
  defp pin_interrupt_condition?(:none), do: true
  defp pin_interrupt_condition?(_), do: false

  defp insert_unique(list, item) do
    if Enum.member?(list, item) do
      list
    else
      [item | list]
    end
  end

end
