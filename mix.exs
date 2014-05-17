defmodule Mix.Tasks.Compile.ElixirAle do
  @shortdoc "Compiles gpio_port"

  def run(_) do
    Mix.shell.info System.cmd("make priv/gpio_port priv/i2c_port priv/spi_port")
  end
end

defmodule ElixirAle.Mixfile do
  use Mix.Project

  def project do
    [ app: :elixir_ale,
      version: "0.0.1",
      elixir: ">= 0.12.5",
      compilers: [:ElixirAle, :elixir, :app],
      deps: deps ]
  end

  # Returns the list of dependencies in the format:
  # { :foobar, git: "https://github.com/elixir-lang/foobar.git", tag: "0.1" }
  #
  # To specify particular versions, regardless of the tag, do:
  # { :barbat, "~> 0.1", github: "elixir-lang/barbat" }
  defp deps do
    []
  end
end
