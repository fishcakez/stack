-module('Elixir.Stack.Filter').

%% API exports

-export_type([t/4]).

-export([new/0, new/1, new/2, transform/2, into/2, defer/3, map_before/2,
         map_after/2, handle/2, init/1, call/3]).

%%==============================================================================
%% API types
%%==============================================================================

-type t(ReqEx, RepEx, ReqIn, RepIn) ::
    stack_filter:t(ReqEx, RepEx, ReqIn, RepIn).

-callback init(_Args) -> _State.
-callback call(_ReqEx, fun((_ReqIn) -> _RepIn), _State) -> _RepEx.

%%==============================================================================
%% API functions
%%==============================================================================

-spec new() -> t(Req, Req, Req, Req).
new() ->
    stack_filter:new().

-spec new(fun((ReqEx, fun((ReqIn) -> RepIn)) -> RepEx)) ->
    t(ReqEx, RepEx, ReqIn, RepIn).
new(Into) when is_function(Into, 2) ->
    stack_filter:new(Into).

-spec new(Mod, _Args) -> t(_ReqEx, _RepEx, _ReqIn, _RepIn) when Mod :: module().
new(Mod, Args) ->
    stack_filter:new(Mod, Args).

-spec transform(t(ReqEx, RepEx, ReqIn, RepIn),
    fun((ReqIn, fun((Req) -> Res)) -> RepIn) | t(ReqIn, RepIn, Req, Res)) ->
        t(ReqEx, RepEx, Req, Res).
transform(Filter, Into) when is_function(Into, 2) ->
    stack_filter:cons(Into, Filter);
transform(Filter1, Filter2) ->
    stack_filter:append(Filter2, Filter1).

-spec into(t(ReqEx, RepEx, ReqIn, RepIn), stack_service:t(ReqIn, RepIn)) ->
        'Elixir.Stack.Service':t(ReqEx, RepEx).
into(Filter, Service) ->
    stack_filter:reverse(Filter, Service).

-spec defer(t(ReqEx, RepEx, ReqIn, RepIn), fun((ReqIn) -> Res),
    fun((Res) -> _Ignore)) -> t(ReqEx, RepEx, ReqIn, RepIn).
defer(Filter, Before, After)
        when is_function(Before, 1), is_function(After, 1) ->
    transform(Filter, fun(Req, Fun) ->
        Res = Before(Req),
        try
            Fun(Req)
        after
            After(Res)
        end
    end).

-spec map_before(t(ReqEx, RepEx, ReqIn, RepIn), fun((ReqIn) -> Res)) ->
    t(ReqEx, RepEx, Res, RepIn).
map_before(Filter, Map) when is_function(Map, 1) ->
    transform(Filter, fun(Req, Fun) -> Fun(Map(Req)) end).

-spec map_after(t(ReqEx, RepEx, ReqIn, RepIn), fun((RepIn) -> Res)) ->
    t(ReqEx, RepEx, ReqIn, Res).
map_after(Filter, Map) when is_function(Map, 1) ->
    transform(Filter, fun(Req, Fun) -> Map(Fun(Req)) end).

-spec handle(t(ReqEx, RepEx, ReqIn, RepIn),
    fun((ReqIn, Class, Reason, Stacktrace) -> RepIn)) ->
    t(ReqEx, RepEx, ReqIn, RepIn) when
    Class :: error | exit | throw,
    Reason :: term(),
    Stacktrace :: [erlang:stack_item()].
handle(Filter, Handle) when is_function(Handle, 4) ->
    transform(Filter, fun(Req, Fun) ->
        try Fun(Req) of
            Rep ->
               Rep
        catch
            error:Reason ->
                Stack = erlang:get_stacktrace(),
                Error = 'Elixir.Exception':normalize(error, Reason, Stack),
                Handle(Req, error, Error, Stack);
            Class:Reason ->
                Stack = erlang:get_stacktrace(),
                Handle(Req, Class, Reason, Stack)
        end
    end).

-spec init(t(ReqEx, RepEx, ReqIn, RepIn)) ->
    fun((ReqEx, fun((ReqIn) -> RepIn)) -> RepEx).
init(Filter) ->
    stack_filter:init(Filter).

-spec call(t(ReqEx, RepEx, ReqIn, RepIn), ReqEx, fun((ReqIn) -> RepIn)) ->
    RepEx.
call(Filter, Req, Fun) ->
    stack_filter:call(Req, Fun, Filter).