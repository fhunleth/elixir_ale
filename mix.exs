Code.ensure_loaded?(Hex) and Hex.start

defmodule Mix.Tasks.Compile.ElixirAle do
  @shortdoc "Compiles elixir_ale port binaries"
  def run(_) do
    0=Mix.Shell.IO.cmd("make priv/gpio_port priv/i2c_port priv/spi_port")
  end
end

defmodule ElixirAle.Mixfile do
  use Mix.Project

  def project do
    [
      app: :elixir_ale,
      version: "0.0.1",
      elixir: ">= 0.13.1",
      compilers: [:ElixirAle, :elixir, :app],
      deps: deps(Mix.env),
      package: package,
      description: description
     ]
  end

  def application, do: []

  defp description do
    """
    Elixir access to hardware I/O interfaces
    """
  end

  defp package do
    [
      contributors: ["Frank Hunleth"],
      license: "Apache",
      links: [
        { "GitHub", "https://github.com/fhunleth/elixir_ale" },
        { "Issues", "https://github.com/fhunleth/elixir_ale/issues" }
      ],
      files: [
        "lib",
        "src",
        "Makefile",
        "mix.exs",
        "README.md",
        "LICENSE"
      ]
    ]
  end

  defp deps(:docs) do
    [{ :ex_doc, github: "elixir-lang/ex_doc" }]
  end

  defp deps(_) do
    []
  end
end
