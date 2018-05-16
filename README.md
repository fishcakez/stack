# Stack

Stack based services

```elixir
base_service =
  Service.new()
  |> Service.map(&add_auth/1)
  |> Service.map(&encode_body/1)
  |> Service.map(&Http.Client.call(:POST, &1))
  |> Service.map(&decode_request/1)
  |> Service.handle(&handle_400_exceptions/1)

service =
  Filter.new()
  |> Filter.transform(DeadlineFilter.new(1000))
  |> Filter.transform(CriticalityFilter.new(:critical))
  |> Filter.transform(FailureFilter.new(fn %ConnectionError{} -> :retry ; _ -> :cont end))
  |> Filter.transform(TraceFilter.new())
  |> Filter.into(base_service)
  |> Service.ensure(&log_request/0)
```
