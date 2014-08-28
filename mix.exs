defmodule NewrelicErlang.Mixfile do
  use Mix.Project

  def project do
    [app: :newrelic_erlang,
     version: "0.0.1",
     description: "A windows friendly, elixir friendly, version of wooga's newrelic_erlang",
     deps: deps]
  end

  def application do
    [applications: [:logger]]
  end

  defp deps do
    [
      {:jsx, "~> 2.0.4"},
      {:lhttpc, git: "git://github.com/ferd/lhttpc.git"},
      {:statman, git: "git://github.com/knutin/statman.git"}
    ]
  end
end