defmodule StdoutFormatter.MixProject do
  use Mix.Project

  def project do
    [
      app: :stdout_formatter,
      version: "0.2.3",
      elixir: "~> 1.6",
      start_permanent: Mix.env == :prod,
      deps: []
    ]
  end

  def application do
    []
  end
end
