defmodule JobProcessor.Router do
  @moduledoc false

  use Plug.Router
  use Plug.ErrorHandler

  alias JobProcessor.Controller

  plug Plug.Logger
  plug :match

  plug Plug.Parsers,
       parsers: [:json],
       pass:  ["application/json"],
       json_decoder: Jason

  plug :dispatch

  post "/process" do
    IO.inspect conn.body_params
    {status, body, content_type} = Controller.process_tasks(conn.body_params)
    response(conn, status, body, content_type)
  end

  match _ do
    response(conn, 404, %{error: "Not found"})
  end

  @impl Plug.ErrorHandler
  def handle_errors(conn, %{kind: _kind, reason: _reason, stack: _stack}) do
    {_, error} = Controller.generic_error_response()
    response(conn, conn.status, error)
  end

  def response(conn, status, body, content_type \\ :json) do
    {res_content_type, res_body} = case content_type do
      :text -> {"text/plain", body}
      _     -> {"application/json", Jason.encode!(body)}
    end

    conn
    |> put_resp_content_type(res_content_type, "utf-8")
    |> send_resp(status, res_body)
  end
  
end