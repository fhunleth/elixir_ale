defmodule I2c do
  use GenServer

  defmodule State do
    defstruct port: nil, devname: nil
  end

  # Public API
  def start_link(devname, address) do
    GenServer.start_link(__MODULE__, [devname, address])
  end

  def release(pid) do
    GenServer.cast pid, :release
  end

  def read(pid, count) do
    GenServer.call pid, {:read, count}
  end

  def write(pid, data) do
    GenServer.call pid, {:write, data}
  end

  # gen_server callbacks
  def init([devname, address]) do
    executable = :code.priv_dir(:elixir_ale) ++ '/ale'
    port = Port.open({:spawn_executable, executable},
      [{:args, ["i2c",
                "/dev/#{devname}",
                Integer.to_string(address)]},
       {:packet, 2},
       :use_stdio,
       :binary,
       :exit_status])
    state = %State{port: port, devname: devname}
    {:ok, state}
  end

  def handle_call({:read, count}, _from, state) do
    {:ok, response} = call_port(state, :read, count)
    {:reply, response, state}
  end
  def handle_call({:write, data}, _from, state) do
    {:ok, response} = call_port(state, :write, data)
    {:reply, response, state}
  end

  def handle_cast(:release, state) do
    {:stop, :normal, state}
  end

  # Private helper functions
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
