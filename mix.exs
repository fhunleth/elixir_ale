defmodule ElixirAle.MixProject do
  use Mix.Project

  def project do
    [
      app: :elixir_ale,
      version: "1.0.3",
      elixir: "~> 1.2",
      name: "elixir_ale",
      description: description(),
      package: package(),
      source_url: "https://github.com/fhunleth/elixir_ale",
      compilers: [:elixir_make] ++ Mix.compilers(),
      make_clean: ["clean"],
      docs: [extras: ["README.md"]],
      aliases: [docs: ["docs", &copy_images/1]],
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application, do: []

  defp description do
    "Elixir access to hardware GPIO, I2C, and SPI interfaces."
  end

  defp package do
    %{
      files: [
        "lib",
        "src/*.[ch]",
        "src/linux/i2c-dev.h",
        "mix.exs",
        "README.md",
        "LICENSE",
        "Makefile"
      ],
      maintainers: ["Frank Hunleth"],
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => "https://github.com/fhunleth/elixir_ale"}
    }
  end

  defp deps do
    [
      {:elixir_make, "~> 0.4", runtime: false},
      {:ex_doc, "~> 0.11", only: :dev}
    ]
  end

  # Copy the images referenced by docs, since ex_doc doesn't do this.
  defp copy_images(_) do
    File.cp_r("assets", "doc/assets")
  end
end
