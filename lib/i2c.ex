defmodule I2c do
  use GenServer

  @moduledoc """
  This module allows Elixir code to communicate with devices on an I2C bus.
  """

  defmodule State do
    @moduledoc false
    defstruct port: nil, address: 0, devname: nil
  end

  @type i2c_address :: 0..127

  # Public API
  @doc """
  Start and link the I2c GenServer.

  `devname` is the I2C bus name (e.g., "i2c-1")
  `address` is the device's 7-bit address on the I2C bus

  Note that `address` can be confusing when reading a datasheet
  since sometimes the datasheet mentions the 8-bit address. For an 8-bit
  address the least significant bit indicates whether the access is for a
  read or a write. Microcontrollers like those on Arduinos often use the 8-bit
  address. To convert an 8-bit address to a 7-bit one, divide the address by
  two.

  All calls to `read/2`, `write/2`, and `write_read/3` access the device
  specified by `address`. Some I2C devices can be switched into different
  modes where they respond to an alternate address. Rather than having to
  create a second `I2c` process, see `read_device/3` and related routines.
  """
  @spec start_link(binary, i2c_address, [term]) :: {:ok, pid}
  def start_link(devname, address, opts \\ []) do
    GenServer.start_link(__MODULE__, [devname, address], opts)
  end

  @doc """
  Stop the GenServer and release all resources.
  """
  @spec release(pid) :: :ok
  def release(pid) do
    GenServer.cast pid, :release
  end

  @doc """
  Initiate a read transaction on the I2C bus of `count` bytes.
  """
  @spec read(pid, integer) :: binary | {:error, term}
  def read(pid, count) do
    GenServer.call pid, {:read, count}
  end

  @doc """
  Write the specified `data` to the device.
  """
  @spec write(pid, binary) :: :ok | {:error, term}
  def write(pid, data) do
    GenServer.call pid, {:write, data}
  end

  @doc """
  Write the specified `data` to the device and then read
  the specified number of bytes.
  """
  @spec write_read(pid, binary, integer) :: binary | {:error, term}
  def write_read(pid, write_data, read_count) do
    GenServer.call pid, {:wrrd, write_data, read_count}
  end

  @doc """
  Initiate a read transaction to the device at the specified `address`. This
  is the same as `read/2` except that an arbitrary device address may be given.
  """
  @spec read_device(pid, i2c_address, integer) :: binary | {:error, term}
  def read_device(pid, address, count) do
    GenServer.call pid, {:read_device, address, count}
  end

  @doc """
  Write the specified `data` to the device at `address`.
  """
  @spec write_device(pid, i2c_address, binary) :: :ok | {:error, term}
  def write_device(pid, address, data) do
    GenServer.call pid, {:write_device, address, data}
  end

  @doc """
  Write the specified `data` to the device and then read
  the specified number of bytes. This is similar to `write_read/3` except
  with an I2C device address.
  """
  @spec write_read_device(pid, i2c_address, binary, integer) ::
    binary | {:error, term}
  def write_read_device(pid, address, write_data, read_count) do
    GenServer.call pid, {:wrrd_device, address, write_data, read_count}
  end

  @doc """
  Scan the I2C bus for devices by performing a read at each device address
  and returning a list of device addresses that respond.

  WARNING: This is intended to be a debugging aid. Reading bytes from devices
  can advance internal state machines and might cause them to get out of sync
  with other code.
  """
  @spec detect_devices(pid) :: [integer]
  def detect_devices(pid) do
    Enum.reject(0..127,
                &(I2c.read_device(pid, &1, 1) == {:error, :i2c_read_failed}))
  end

  # gen_server callbacks
  def init([devname, address]) do
    executable = :code.priv_dir(:elixir_ale) ++ '/ale'
    port = Port.open({:spawn_executable, executable},
      [{:args, ["i2c", "/dev/#{devname}"]},
       {:packet, 2},
       :use_stdio,
       :binary,
       :exit_status])
    state = %State{port: port, address: address, devname: devname}
    {:ok, state}
  end

  def handle_call({:read, count}, _from, state) do
    {:ok, response} = call_port(state, :read, state.address, count)
    {:reply, response, state}
  end
  def handle_call({:write, data}, _from, state) do
    {:ok, response} = call_port(state, :write, state.address, data)
    {:reply, response, state}
  end
  def handle_call({:wrrd, write_data, read_count}, _from, state) do
    {:ok, response} =
        call_port(state, :wrrd, state.address, {write_data, read_count})
    {:reply, response, state}
  end
  def handle_call({:read_device, address, count}, _from, state) do
    {:ok, response} = call_port(state, :read, address, count)
    {:reply, response, state}
  end
  def handle_call({:write_device, address, data}, _from, state) do
    {:ok, response} = call_port(state, :write, address, data)
    {:reply, response, state}
  end
  def handle_call({:wrrd_device, address, write_data, read_count}, _from, state) do
    {:ok, response} = call_port(state, :wrrd, address, {write_data, read_count})
    {:reply, response, state}
  end

  def handle_cast(:release, state) do
    {:stop, :normal, state}
  end

  # Private helper functions
  defp call_port(state, command, address, arguments) do
    msg = {command, address, arguments}
    send state.port, {self, {:command, :erlang.term_to_binary(msg)}}
    receive do
      {_, {:data, response}} ->
        {:ok, :erlang.binary_to_term(response)}
        _ -> :error
    end
  end
end
