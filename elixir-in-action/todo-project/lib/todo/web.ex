defmodule Todo.Web do
  @moduledoc """
  Web server responsible for serving the todo list API.
  """

  use Plug.Router

  alias Plug.Conn

  plug :match
  plug :dispatch

  ##################
  ##  API Routes  ##
  ##################

  post "/add_entry" do
    conn = Conn.fetch_query_params(conn)
    list_name = Map.fetch!(conn.params, "list_name")
    title = Map.fetch!(conn.params, "title")
    date = conn.params |> Map.fetch!("date") |> Date.from_iso8601!()

    list_name
    |> Todo.Cache.start_or_retrieve_server_by_list_name()
    |> Todo.Server.add_entry(%{title: title, date: date})

    conn
    |> Conn.put_resp_content_type("text/plain")
    |> Conn.send_resp(200, "OK")
  end

  get "/entries" do
    conn = Conn.fetch_query_params(conn)
    list_name = Map.fetch!(conn.params, "list_name")
    date = conn.params |> Map.fetch!("date") |> Date.from_iso8601!()

    entries =
      list_name
      |> Todo.Cache.start_or_retrieve_server_by_list_name()
      |> Todo.Server.entries(date)
      |> Enum.map_join("\n", &"#{&1.date} #{&1.title}")

    conn
    |> Conn.put_resp_content_type("text/plain")
    |> Conn.send_resp(200, entries)
  end

  match _, do: send_resp(conn, 404, "Not Found")

  ##################
  ##  Server API  ##
  ##################

  @doc """
  Builds the child spec for the web server.
  """
  @spec child_spec(term()) :: Supervisor.child_spec()
  def child_spec(_opts) do
    Plug.Cowboy.child_spec(
      scheme: :http,
      options: [port: Application.get_env(:todo, :http_port)],
      plug: __MODULE__
    )
  end
end
