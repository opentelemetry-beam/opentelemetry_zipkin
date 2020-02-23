-module(opentelemetry_zipkin).

-export([init/1,
         export/2,
         shutdown/1]).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry/include/ot_span.hrl").

-define(DEFAULT_ZIPKIN_ADDRESS, "http://localhost:9411/api/v2/spans").
-define(DEFAULT_LOCAL_ENDPOINT, #{<<"serviceName">> => node()}).

-record(state, {address :: string(),
                endpoint :: map()}).

init(Opts) ->
    Address = zipkin_address(Opts),
    LocalEndpoint = local_endpoint(Opts),
    {ok, #state{address=Address,
                endpoint=LocalEndpoint}}.

export(Tab, #state{address=Address,
                   endpoint=LocalEndpoint}) ->
    ZSpans = ets:foldl(fun(Span, Acc) ->
                               try zipkin_span(Span, LocalEndpoint) of
                                   ZipkinSpan ->
                                       [jsx:encode(ZipkinSpan) | Acc]
                               catch
                                   C:T:S ->
                                       %% failed to encode
                                       ?LOG_DEBUG("failed to encode span to Zipkin format ~p:~p ~p", [C, T, S]),
                                       Acc
                               end
                       end, [], Tab),

    Json = iolist_to_binary(["[", lists:join($,, ZSpans), "]"]),
    case httpc:request(post, {Address, [], "application/json", Json}, [], []) of
        {ok, {{_, Code, _}, _, _}} when Code >= 200 andalso Code =< 202 ->
            ok;
        {ok, {{_, Code, _}, _, Message}} ->
            ?LOG_INFO("error response from service exported to status=~p ~p",
                      [Code, Message]),
            error;
        {error, Reason} ->
            ?LOG_INFO("client error exporting spans ~p", [Reason]),
            error
    end.

shutdown(_) ->
    ok.

%%


zipkin_span(Span, LocalEndpoint) ->
    (optional_fields(Span))#{
       <<"traceId">> => iolist_to_binary(io_lib:format("~32.16.0b", [Span#span.trace_id])),
       <<"name">> => iolist_to_binary(Span#span.name),
       <<"id">> => iolist_to_binary(io_lib:format("~16.16.0b", [Span#span.span_id])),
       <<"timestamp">> => wts:to_absolute(Span#span.start_time),
       <<"duration">> => wts:duration(Span#span.start_time, Span#span.end_time),
       %% <<"debug">> => false, %% TODO: get from attributes?
       %% <<"shared">> => false, %% TODO: get from attributes?
       <<"localEndpoint">> => LocalEndpoint
       %% <<"remoteEndpoint">> =>  %% TODO: get from attributes?
     }.

to_annotations(TimeEvents) ->
    to_annotations(TimeEvents, []).

to_annotations([], Annotations) ->
    Annotations;
to_annotations([#timed_event{time_unixnano=Timestamp,
                             event=#event{name=Name,
                                          attributes=Attributes}} | Rest], Annotations) ->
    to_annotations(Rest, [#{<<"timestamp">> => wts:to_absolute(Timestamp),
                            <<"value">> => annotation_value(Name, Attributes)} | Annotations]).

annotation_value(Name, Attributes) ->
    AttrString = lists:join(", ", [[Key, ": ", to_string(Value)] ||
                                      {Key, Value} <- Attributes]),
    iolist_to_binary([Name, ": {", AttrString, "}"]).

to_string(Value) when is_function(Value) ->
    to_string(Value());
to_string(Value) when is_list(Value) ;
                      is_binary(Value) ->
    Value;
to_string(Value) ->
    io_lib:format("~p", [Value]).

to_binary_string(Value) when is_function(Value) ->
    to_binary_string(Value());
to_binary_string(Value) when is_list(Value) ->
    list_to_binary(Value);
to_binary_string(Value) when is_integer(Value) ->
    integer_to_binary(Value);
to_binary_string(Value) when is_float(Value) ->
    float_to_binary(Value);
to_binary_string(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
to_binary_string(Value) ->
    Value.

to_tags(Attributes) ->
    lists:foldl(fun({Name, Value}, Acc) ->
                     Acc#{to_binary_string(Name) => to_binary_string(Value)}
             end, #{}, Attributes).

optional_fields(Span) ->
    lists:foldl(fun(Field, Acc) ->
                        case span_field(Field, Span) of
                            undefined ->
                                Acc;
                            Value ->
                                maps:put(Field, Value, Acc)
                        end
                end, #{}, [<<"kind">>, <<"parentId">>, <<"annotations">>, <<"tags">>]).

span_field(<<"annotations">>, #span{timed_events=[]}) ->
    undefined;
span_field(<<"annotations">>, #span{timed_events=TimedEvents}) ->
    to_annotations(TimedEvents);
span_field(<<"tags">>, #span{attributes=[]}) ->
    undefined;
span_field(<<"tags">>, #span{attributes=Attributes}) ->
    to_tags(Attributes);
span_field(<<"parentId">>, #span{parent_span_id=undefined}) ->
    undefined;
span_field(<<"parentId">>, #span{parent_span_id=ParentId}) ->
    iolist_to_binary(io_lib:format("~16.16.0b", [ParentId]));
span_field(<<"kind">>, #span{kind=?SPAN_KIND_UNSPECIFIED}) ->
    undefined;
span_field(<<"kind">>, #span{kind=?SPAN_KIND_INTERNAL}) ->
    undefined;
span_field(<<"kind">>, #span{kind=?SPAN_KIND_PRODUCER}) ->
    <<"PRODUCER">>;
span_field(<<"kind">>, #span{kind=?SPAN_KIND_CONSUMER}) ->
    <<"CONSUMER">>;
span_field(<<"kind">>, #span{kind=?SPAN_KIND_SERVER}) ->
    <<"SERVER">>;
span_field(<<"kind">>, #span{kind=?SPAN_KIND_CLIENT}) ->
    <<"CLIENT">>.

zipkin_address(Options) ->
    maps:get(address, Options, ?DEFAULT_ZIPKIN_ADDRESS).

local_endpoint(Options) ->
    maps:get(local_endpoint, Options, ?DEFAULT_LOCAL_ENDPOINT).
