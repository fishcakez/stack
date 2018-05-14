-module(stack_service).

%% API exports

-export_type([t/2]).

-export([new/0, new/1, new/2, cons/2, append/2, init/1, call/2, to_list/1,
         from_list/1]).

%%==============================================================================
%% API types
%%==============================================================================

-record(service, {stack}).

-opaque t(Req, Rep) :: #service{stack :: stack:t(Req, Rep)}.

-callback init(_Args) -> _State.
-callback call(_Req, _State) -> _Rep.

%%==============================================================================
%% API functions
%%==============================================================================

-spec new() -> t(Req, Req).
new() ->
    #service{stack=[]}.

-spec new(fun((Req) -> Rep)) -> t(Req, Rep).
new(Map) when is_function(Map, 1) ->
    #service{stack=[{map, Map}]}.

-spec new(Mod, _Args) -> t(_Req, _Rep) when Mod :: module().
new(Mod, Args) ->
    #service{stack=[{map, Mod, Mod:init(Args)}]}.

-spec cons(fun((Rep) -> Res), t(Req, Rep)) -> t(Req, Res);
         (t(Rep, Res), t(Req, Rep)) -> t(Req, Res).
cons(Map, #service{stack=Stack} = Service) when is_function(Map, 1) ->
    Service#service{stack=[{map, Map} | Stack]}.

-spec append(t(Rep, Res), t(Req, Rep)) -> t(Req, Res).
append(#service{stack=Stack2}, #service{stack=Stack1} = Service) ->
    Service#service{stack=(Stack2 ++ Stack1)}.

-spec init(t(Req, Rep)) -> fun((Req) -> Rep).
init(#service{stack=Stack}) ->
    fun(Req) -> stack:eval(Req, Stack) end.

-spec call(Req, t(Req, Rep)) -> Rep.
call(Req, #service{stack=Stack}) ->
    stack:eval(Req, Stack).

-spec to_list(t(Req, Rep)) -> stack:t(Req, Rep).
to_list(#service{stack=Stack}) ->
    Stack.

-spec from_list(Stack) -> t(Req, Rep) when Stack :: stack:t(Req, Rep).
from_list(Stack) ->
    #service{stack=Stack}.