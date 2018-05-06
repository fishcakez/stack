if not function_exported?(:logger, :i, 0) do
  defmodule :logger do
    @moduledoc false

    ## Shim until OTP 21's logger

    def set_process_metadata(map) do
      _ = :erlang.put(__MODULE__, map)
      :ok
    end

    def get_process_metadata() do
      :erlang.get(__MODULE__)
    end

    def unset_process_metadata() do
      :erlang.erase(__MODULE__)
      :ok
    end
  end
end