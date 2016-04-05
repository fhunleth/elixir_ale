defmodule Mix.Tasks.Compile.ElixirAle do
  @shortdoc "Compiles elixir_ale port binaries"
  def run(_) do
    0=Mix.Shell.IO.cmd("make priv/ale")
    Mix.Project.build_structure
    :ok
  end
end

defmodule ElixirAle.Mixfile do
  use Mix.Project

  def project do
    [
      app: :elixir_ale,
      version: "0.4.1",
      name: "elixir_ale",
      source_url: "https://github.com/fhunleth/elixir_ale",
      elixir: ">= 0.14.1",
      compilers: [:ElixirAle, :elixir, :app],
      deps: deps,
      docs: [extras: ["README.md"]],
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
      maintainers: ["Frank Hunleth"],
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => "https://github.com/fhunleth/elixir_ale"}}
  end

  defp deps do
    [
      {:earmark, "~> 0.1", only: :dev},
      {:ex_doc, "~> 0.11", only: :dev},
      {:credo, "~> 0.3", only: [:dev, :test]}
    ]
  end
end
