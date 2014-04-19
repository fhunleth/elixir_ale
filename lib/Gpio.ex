defmodule Gpio do
  use GenServer.Behaviour

  defrecord State, port: nil

  # Public API
  def start_link(pin, pin_direction) do
    :gen_server.start_link({:local, server_name(pin)}, __MODULE__, [pin, pin_direction], [])
  end

	@doc """
  Free the resources associated with pin.
  """
  def release(pin) do
    :gen_server.cast server_name(pin), :release
  end

  def write(pin, value) do
    :gen_server.call server_name(pin), {:write, value}
  end

  def read(pin) do
    :gen_server.call server_name(pin), :read
  end

  # gen_server callbacks
  def init([pin, pin_direction]) do
    executable = :code.priv_dir(:elixir_ale) ++ '/gpio_port'
    port = Port.open({:spawn_executable, executable},
										 [{:args, ["#{pin}", atom_to_binary(pin_direction)]},
											{:packet, 2},
											:use_stdio,
											:binary,
										  :exit_status])
    state = State.new(port: port)
    { :ok, state }
  end

  def handle_call(:read, _from, state) do
    {:ok, response} = call_port(state, :read, [])
    {:reply, response, state }
  end

  def handle_call({:write, value}, _from, state) do
    {:ok, response} = call_port(state, :write, [value])
    {:reply, response, state }
  end

  def handle_cast(:release, state) do
    {:stop, :normal, state}
  end

  # Private helper functions
	defp server_name(pin) do
		Module.concat(__MODULE__, "#{pin}")
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

end
