defmodule I2c do
  use GenServer

  @moduledoc """
  This module allows Elixir code to communicate with devices on an I2C bus.
  """

  defmodule State do
    defstruct port: nil, devname: nil
  end

  # Public API
  @doc """
  Start and link the I2c GenServer.

  `devname` should be the I2C bus name (e.g. "i2c-1")
  `address` should be the device's 7-bit address on the I2C bus.

  Note that the address parameter can be confusing when reading a datasheet since
  sometimes the datasheet specifies the 8-bit address where the least
  significant bit indicates read/write. This address refers to the upper
  7-bits that don't change between reads and writes.
  """
  def start_link(devname, address, opts \\ []) do
    GenServer.start_link(__MODULE__, [devname, address], opts)
  end

  @doc """
  Stop the GenServer and release all resources.
  """
  def release(pid) do
    GenServer.cast pid, :release
  end

  @doc """
  Initiate a read transaction on the I2C bus of `count` bytes.
  """
  def read(pid, count) do
    GenServer.call pid, {:read, count}
  end

  @doc """
  Write the specified `data` to the device.
  """
  def write(pid, data) do
    GenServer.call pid, {:write, data}
  end

  @doc """
  Write the specified `data` to the device and then read
  the specified number of bytes.
  """
  def write_read(pid, write_data, read_count) do
    GenServer.call pid, {:wrrd, write_data, read_count}
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
  def handle_call({:wrrd, write_data, read_count}, _from, state) do
    {:ok, response} = call_port(state, :wrrd, {write_data, read_count})
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
