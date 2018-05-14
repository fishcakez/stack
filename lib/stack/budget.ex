defmodule Stack.Budget do
  @moduledoc """
  Behaviour for defining a failure budget.
  """

  @doc """
  Ask the budget if request can run.

  Returns `{:go, ets_tid}` to run, otherwise `:drop`.
  """
  @callback ask(name :: atom) :: {:go, ref :: :ets.tid()} | :drop

  @doc """
  Try to force the budget to allow request to run.

  Returns `{:go, ets_tid}` to run.
  """
  @callback force(name :: atom) :: {:go, ref :: :ets.tid()}

  @doc """
  Positiviely acknowledge the reply.
  """
  @callback ack(ref :: :ets.tid()) :: :ok

  @doc """
  Negatively acknowledge the reply.

  The second argument is the number of times the request has been tried so far.
  If the result is `true` the budget allows a retry, otherwise `false` does not allow
  a retry.
  """
  @callback nack(ref :: :ets.tid(), pos_integer) :: boolean
end
