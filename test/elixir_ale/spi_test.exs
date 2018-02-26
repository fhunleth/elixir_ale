defmodule ElixirALE.SPI.Test do
  use ExUnit.Case, async: false
  alias ElixirALE.SPI, as: Subject
  import Mox

  setup do
    defmock(MockSPIPort, for: ElixirALE.SPI.Port.Behaviour)
    Application.put_env(:elixir_ale, :spi_port, MockSPIPort)

    port = make_ref()
    stub(MockSPIPort, :open, fn(_, _) -> port end)
    set_mox_global()
    {:ok, pid} = start_supervised Subject
    {:ok, subject: pid, port: port}
  end

  test "calling the same port twice",
  %{subject: subject, port: port} do
    stub(MockSPIPort, :transfer, fn
      (^port, <<"one">>) ->
        Process.send_after(subject,
                           {:foo, {:data, :erlang.term_to_binary(:response_one)}},
                           100)
      (^port, <<"two">>) ->
        send(subject,
             {:foo, {:data, :erlang.term_to_binary(:response_two)}}
        )
    end)

    transfer_one = Task.async(fn ->
      Subject.transfer(subject, <<"one">>)
    end)

    transfer_two = Task.async(fn ->
      Subject.transfer(subject, <<"two">>)
    end)

    assert Task.await(transfer_one) == :response_one
    assert Task.await(transfer_two) == :response_two
  end
end
