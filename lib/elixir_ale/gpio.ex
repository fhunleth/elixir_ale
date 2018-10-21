defmodule ElixirALE.GPIO do
  use GenServer

  @moduledoc """
  This is an Elixir interface to Linux GPIOs. Each GPIO is an
  independent GenServer.
  """

  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, opts}
    }
  end

  defmodule State do
    @moduledoc false
    defstruct port: nil, pin: 0, direction: nil, callbacks: []
  end

  @type pin_number :: non_neg_integer
  @type pin_value :: 0 | 1 | true | false
  @type pin_direction :: :input | :output
  @type int_direction :: :rising | :falling | :both | :none

  # Public API
  @doc """
  Start and link a new GPIO GenServer. `pin` should be a valid
  GPIO pin number on the system and `pin_direction` should be
  `:input` or `:output`.

  Including `:start_value` in `opts` will explicitly write the
  GPIO value on start. Expected values are `1`, `0`, `true`, or `false`

      ElixirALE.GPIO.start_link(16, :output, start_value: 1)
  """
  @spec start_link(pin_number(), pin_direction, [term]) :: GenServer.on_start()
  def start_link(pin, pin_direction, opts \\ []) do
    {start_value, opts} = Keyword.pop(opts, :start_value)
    GenServer.start_link(__MODULE__, [pin, pin_direction, start_value], opts)
  end

  @doc """
  Helper method for reading the pin number that the GPIO GenServer
  is linked to.
  """
  @spec pin(GenServer.server()) :: pin_number()
  def pin(pid), do: GenServer.call(pid, :pin)

  @doc """
  Free the resources associated with pin and stop the GenServer.
  """
  @spec release(GenServer.server()) :: :ok
  def release(pid) do
    GenServer.cast(pid, :release)
  end

  @doc """
  Write the specified value to the GPIO. The GPIO should be configured
  as an output. Valid values are `0` or `false` for logic low and `1`
  or `true` for logic high. Other non-zero values will result in logic
  high being output.
  """
  @spec write(GenServer.server(), pin_value()) :: :ok | {:error, term}
  def write(pid, value) when is_integer(value) do
    GenServer.call(pid, {:write, value})
  end

  def write(pid, true), do: write(pid, 1)
  def write(pid, false), do: write(pid, 0)

  @doc """
  Read the current value of the pin.
  """
  @spec read(GenServer.server()) :: pin_value() | {:error, term}
  def read(pid) do
    GenServer.call(pid, :read)
  end

  @doc """
  Turn on "interrupts" on the input pin. The pin can be monitored for
  `:rising` transitions, `:falling` transitions, or `:both`. The process
  that calls this method will receive the messages.
  """
  @spec set_int(GenServer.server(), int_direction) :: :ok | {:error, term}
  def set_int(pid, direction) do
    true = pin_interrupt_condition?(direction)
    GenServer.call(pid, {:set_int, direction, self()})
  end

  # gen_server callbacks
  def init([pin, pin_direction]) do
    executable = :code.priv_dir(:elixir_ale) ++ '/ale'

    port =
      Port.open({:spawn_executable, executable}, [
        {:args, ["gpio", "#{pin}", Atom.to_string(pin_direction)]},
        {:packet, 2},
        :use_stdio,
        :binary,
        :exit_status
      ])

    state = %State{port: port, pin: pin, direction: pin_direction}
    {:ok, state}
  end

  def init([pin, pin_direction, nil]) do
    init([pin, pin_direction])
  end

  def init([pin, pin_direction, bool]) when is_boolean(bool) do
    start_value = if bool, do: 1, else: 0
    init([pin, pin_direction, start_value])
  end

  def init([pin, pin_direction, start_value]) when is_integer(start_value) do
    with {:ok, state} <- init([pin, pin_direction]),
         :ok <- call_port(state, :write, start_value) do
      {:ok, state}
    else
      {:error, reason} -> {:stop, reason}
      error -> error
    end
  end

  def handle_call(:pin, _from, state) do
    {:reply, state.pin, state}
  end

  def handle_call(:read, _from, state) do
    response = call_port(state, :read, [])
    {:reply, response, state}
  end

  def handle_call({:write, value}, _from, state) do
    response = call_port(state, :write, value)
    {:reply, response, state}
  end

  def handle_call({:set_int, direction, requestor}, _from, state) do
    response = call_port(state, :set_int, direction)
    new_callbacks = insert_unique(state.callbacks, requestor)
    state = %{state | callbacks: new_callbacks}
    {:reply, response, state}
  end

  def handle_cast(:release, state) do
    {:stop, :normal, state}
  end

  def handle_info({_, {:data, <<?n, message::binary>>}}, state) do
    msg = :erlang.binary_to_term(message)
    handle_port(msg, state)
  end

  defp call_port(state, command, arguments) do
    msg = {command, arguments}
    send(state.port, {self(), {:command, :erlang.term_to_binary(msg)}})

    receive do
      {_, {:data, <<?r, response::binary>>}} ->
        :erlang.binary_to_term(response)
    after
      500 -> :timedout
    end
  end

  defp handle_port({:gpio_interrupt, condition}, state) do
    # IO.puts "Got interrupt on pin #{state.pin}, #{condition}"
    msg = {:gpio_interrupt, state.pin, condition}

    for pid <- state.callbacks do
      send(pid, msg)
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
