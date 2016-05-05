defmodule Mix.Tasks.Compile.ElixirAle do
  @shortdoc "Compiles elixir_ale port binary"
  def run(_) do
    {result, error_code} = System.cmd("make", ["all"], stderr_to_stdout: true)
    IO.binwrite result
    if error_code != 0 do
      raise Mix.Error, "Make returned an error"
    end
    Mix.Project.build_structure
  end
end

defmodule Mix.Tasks.CopyImages do
  @shortdoc "Copy the images referenced by README.md, since ex_doc doesn't do this."
  use Mix.Task
  def run(_) do
    File.cp_r "assets", "doc/assets"
  end
end

defmodule ElixirAle.Mixfile do
  use Mix.Project

  def project do
    [
      app: :elixir_ale,
      version: "0.5.2",
      name: "elixir_ale",
      source_url: "https://github.com/fhunleth/elixir_ale",
      elixir: ">= 0.14.1",
      compilers: [:ElixirAle, :elixir, :app],
      deps: deps,
      docs: [extras: ["README.md"]],
      package: package,
      description: description,
      aliases: ["docs": ["docs", "copy_images"]]
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
