defmodule Spi do
  use GenServer

  defmodule State do
    defstruct port: nil, devname: nil
  end

  # Public API
  def start_link(devname, mode \\ 0, bits_per_word \\ 8, speed_hz \\ 1000000, delay_us \\ 10) do
    GenServer.start_link(__MODULE__, [devname, mode, bits_per_word, speed_hz, delay_us])
  end

  def release(pid) do
    GenServer.cast pid, :release
  end

  def transfer(pid, data) do
    GenServer.call pid, {:transfer, data}
  end

  # gen_server callbacks
  def init([devname, mode, bits_per_word, speed_hz, delay_us]) do
    executable = :code.priv_dir(:elixir_ale) ++ '/spi_port'
    port = Port.open({:spawn_executable, executable},
      [{:args, ["/dev/#{devname}",
                Integer.to_string(mode),
                Integer.to_string(bits_per_word),
                Integer.to_string(speed_hz),
                Integer.to_string(delay_us)]},
       {:packet, 2},
       :use_stdio,
       :binary,
       :exit_status])
    state = %State{port: port, devname: devname}
    {:ok, state}
  end

  def handle_call({:transfer, data}, _from, state) do
    {:ok, response} = call_port(state, :transfer, data)
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
