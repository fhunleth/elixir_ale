defmodule ElixirALE.SPI do
  use GenServer

  @moduledoc """
  This module enables Elixir programs to interact with hardware that's connected
  via a SPI bus.
  """

  defmodule State do
    @moduledoc false
    defstruct port: nil, devname: nil
  end

  # NOTE: for :bits_per_word, 0 is interpreted as 8-bits
  @type spi_option ::
          {:mode, 0..3}
          | {:bits_per_word, 0..16}
          | {:speed_hz, pos_integer}
          | {:delay_us, non_neg_integer}

  # Public API
  @doc """
  Return a list of available SPI bus device names.  If nothing is returned,
  it's possible that the kernel driver for that SPI bus is not enabled or the
  kernel's device tree is not configured. On Raspbian, run `raspi-config` and
  look in the advanced options.

  ```
  iex> ElixirALE.SPI.device_names
  ["spidev0.0", "spidev0.1"]
  ```
  """
  @spec device_names() :: [binary]
  def device_names() do
    Path.wildcard("/dev/spidev*")
    |> Enum.map(fn p -> String.replace_prefix(p, "/dev/", "") end)
  end

  @doc """
  Start and link a SPI GenServer.

  SPI bus options include:
   * `mode`: This specifies the clock polarity and phase to use. (0)
   * `bits_per_word`: bits per word on the bus (8)
   * `speed_hz`: bus speed (1000000)
   * `delay_us`: delay between transaction (10)

  Parameters:
   * `devname` is the Linux device name for the bus (e.g., "spidev0.0")
   * `spi_opts` is a keyword list to configure the bus
   * `opts` are any options to pass to GenServer.start_link
  """
  @spec start_link(binary, [spi_option], [term]) :: {:ok, pid}
  def start_link(devname, spi_opts \\ [], opts \\ []) do
    GenServer.start_link(__MODULE__, {devname, spi_opts}, opts)
  end

  @doc """
  Stop the GenServer and release the SPI resources.
  """
  @spec release(pid) :: :ok
  def release(pid) do
    GenServer.cast(pid, :release)
  end

  @doc """
  Perform a SPI transfer. The `data` should be a binary containing the bytes to
  send. Since SPI transfers simultaneously send and receive, the return value
  will be a binary of the same length or an error.
  """
  @spec transfer(pid, binary) :: binary | {:error, term}
  def transfer(pid, data) do
    GenServer.call(pid, {:transfer, data})
  end

  # gen_server callbacks
  def init({devname, spi_opts}) do
    mode = Keyword.get(spi_opts, :mode, 0)
    bits_per_word = Keyword.get(spi_opts, :bits_per_word, 8)
    speed_hz = Keyword.get(spi_opts, :speed_hz, 1_000_000)
    delay_us = Keyword.get(spi_opts, :delay_us, 10)

    executable = :code.priv_dir(:elixir_ale) ++ '/ale'

    port =
      Port.open({:spawn_executable, executable}, [
        {
          :args,
          [
            "spi",
            "/dev/#{devname}",
            Integer.to_string(mode),
            Integer.to_string(bits_per_word),
            Integer.to_string(speed_hz),
            Integer.to_string(delay_us)
          ]
        },
        {:packet, 2},
        :use_stdio,
        :binary,
        :exit_status
      ])

    state = %State{port: port, devname: devname}
    {:ok, state}
  end

  def handle_call({:transfer, data}, _from, state) do
    response = call_port(state, :transfer, data)
    {:reply, response, state}
  end

  def handle_cast(:release, state) do
    {:stop, :normal, state}
  end

  # Private helper functions
  defp call_port(state, command, arguments) do
    msg = {command, arguments}
    send(state.port, {self(), {:command, :erlang.term_to_binary(msg)}})

    receive do
      {_, {:data, response}} ->
        :erlang.binary_to_term(response)
    after
      500 -> {:error, :timeout}
    end
  end
end
