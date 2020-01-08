# OpenTelemetry Zipkin Exporter

![](https://github.com/opentelemetry-beam/opentelemetry_zipkin/workflows/OpenTelemetry%20Zipkin%20Tests/badge.svg)

Zipkin exporter for OpenTelemetry Erlang/Elixir instrumentation.

## Setup

Easiest way to setup is to add configuration for the batch processor in OpenTelemetry application environment.

For an Erlang release in `sys.config`:

``` erlang
{opentelemetry,
  [{processors, 
    [{ot_batch_processor,
        [{exporter, {opentelemetry_zipkin, #{address => "http://localhost:9411/api/v2/spans",
                                             local_endpoint => #{<<"serviceName">> => <<"ServiceName">>}}}}]}]
}]}
```

An Elixir release uses `releases.exs`:

``` elixir
config :opentelemetry,
    :processors, [{:ot_batch_processor, [{:exporter, {:opentelemetry_zipkin, %{address: 'http://localhost:9411/api/v2/spans', local_endpoint: %{"serviceName" => "ServiceName"}}}}]}]
```
