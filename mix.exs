defmodule Mix.Tasks.Compile.ElixirAle do
  @shortdoc "Compiles elixir_ale port binaries"
  def run(_) do
    0=Mix.Shell.IO.cmd("make priv/ale")
  end
end

defmodule ElixirAle.Mixfile do
  use Mix.Project

  def project do
    [
      app: :elixir_ale,
      version: "0.3.0",
      elixir: ">= 0.14.1",
      compilers: [:ElixirAle, :elixir, :app],
      deps: deps,
      package: package,
      description: description
     ]
  end

  def application, do: []

  defp description do
    """
    Elixir access to hardware I/O interfaces such as GPIO, I2C, and SPI.
    """
  end

  defp package do
    %{files: ["lib", "src/*.[ch]", "mix.exs", "README.md", "LICENSE", "Makefile"],
      contributors: ["Frank Hunleth"],
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => "https://github.com/fhunleth/elixir_ale"}}
  end

  defp deps do
    []
  end
end
