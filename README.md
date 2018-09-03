# Stack

Stack is a protocol agnostic, functional framework for building fault tolerant, observable clients and
servers using composable data structures with parameterized typing. There is built in support for
request deadlines and criticiality, distributed tracing, concurrency/rate limiting and failure handling
(retry budgets, rejection budgets and backoff). The features combine naturally together, for example
a retry backoff won't occur if the backoff would go beyond a deadline. Pipelines are composed at runtime
(without macros) and can be cached as optimized modules.

Example

```elixir
base_service =
  Service.new()
  |> Service.map(&add_auth/1)
  |> Service.map(&encode_body/1)
  |> Service.map(&Http.Client.call(:POST, &1))
  |> Service.map(&decode_request/1)

service =
  Filter.new()
  |> Filter.transform(DeadlineFilter.new(1000))
  |> Filter.transform(CriticalityFilter.new(:critical))
  |> Filter.transform(FailureFilter.new(fn %ConnectionError{} -> :retry ; _ -> :cont end))
  |> Filter.transform(ConcurrencyFilter.new(RateLimiter))
  |> Filter.transform(TraceFilter.new("http_api_call"))
  |> Filter.defer(&new_log_entry/1, &log/1)
  |> Filter.handle(&handle_400_exceptions/2)
  |> Filter.into(base_service)
```
