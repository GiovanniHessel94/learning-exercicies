import Config

is_test = config_env() == :test

http_port =
  if is_test do
    System.get_env("TODO_HTTP_PORT_TEST", "5455")
  else
    System.get_env("TODO_HTTP_PORT", "5454")
  end

db_folder =
  if is_test do
    System.get_env("TODO_DB_FOLDER_TEST", "./persist_test")
  else
    System.get_env("TODO_DB_FOLDER", "./persist")
  end

config :todo, http_port: String.to_integer(http_port), db_folder: db_folder
