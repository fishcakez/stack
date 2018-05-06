-module('Elixir.Stack.Service').

%% API exports

-export_type([t/2]).

-export([new/0, new/1, new/2, map/2, each/2, init/1, call/2]).

%%==============================================================================
%% API types
%%==============================================================================

-type t(Req, Rep) :: stack_service:t(Req, Rep).

-callback init(_Args) -> _State.
-callback call(_Req, _State) -> _Rep.

%%==============================================================================
%% API functions
%%==============================================================================

-spec new() -> t(Req, Req).
new() ->
    stack_service:new().

-spec new(fun((Req) -> Rep)) -> t(Req, Rep).
new(Map) ->
    stack_service:new(Map).

-spec new(Mod, _Args) -> t(_Req, _Rep) when Mod :: module().
new(Mod, Args) ->
    stack_service:new(Mod, Args).

-spec map(t(Req, Rep), fun((Rep) -> Res)) -> t(Req, Res);
         (t(Req, Rep), t(Rep, Res)) -> t(Req, Res).
map(Service, Map) when is_function(Map, 1) ->
    stack_service:cons(Map, Service);
map(Service1, Service2) ->
    stack_service:append(Service2, Service1).

-spec each(t(Req, Rep), fun((Rep) -> _Res)) -> t(Req, Rep).
each(Service, Each) when is_function(Each, 1) ->
    map(Service, fun(Rep) ->
        _ = Each(Rep),
        Rep
    end).

-spec init(t(Req, Rep)) -> fun((Req) -> Rep).
init(Service) ->
    stack_service:init(Service).

-spec call(t(Req, Rep), Req) -> Rep.
call(Service, Req) ->
    stack_service:call(Req, Service).