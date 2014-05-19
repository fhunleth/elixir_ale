Code.ensure_loaded?(Hex) and Hex.start

defmodule Mix.Tasks.Compile.ElixirAle do
  @shortdoc "Compiles elixir_ale port binaries"
  def run(_) do
    File.rm("priv/gpio_port")
    File.rm("priv/i2c_port")
    File.rm("priv/spi_port")
    Mix.shell.info System.cmd("make priv/gpio_port priv/i2c_port priv/spi_port")
    if !File.exists?("priv/gpio_port") || !File.exists?("priv/i2c_port") || !File.exists?("priv/spi_port") do
        raise Mix.Error, message: "error making elixir_ale port executables"
    end
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
