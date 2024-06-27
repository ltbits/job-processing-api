defmodule JobProcessor.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {
        Plug.Cowboy, 
        scheme: :http, 
        plug: JobProcessor.Router, 
        options: [port: Application.get_env(:job_processor, :port)]
      }
    ]

    opts = [strategy: :one_for_one, name: JobProcessor.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
