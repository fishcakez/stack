-module(stack_filter).

%% API exports

-export_type([t/4]).

-export([new/0, new/1, new/2, cons/2, append/2, reverse/2, init/1, call/3,
         to_list/1, from_list/1]).

%%==============================================================================
%% API types
%%==============================================================================

-record(filter, {stack}).

-opaque t(ReqEx, RepEx, _ReqIn, _RepIn) ::
    #filter{stack :: stack:t(ReqEx, RepEx)}.

-callback init(_Args) -> _State.
-callback call(_ReqEx, fun((_ReqIn) -> _RepIn), _State) -> _RepEx.

%%==============================================================================
%% API functions
%%==============================================================================

-spec new() -> t(Req, Req, Req, Req).
new() ->
    #filter{stack=[]}.

-spec new(fun((ReqEx, fun((ReqIn) -> RepIn)) -> RepEx)) ->
    t(ReqEx, RepEx, ReqIn, RepIn).
new(Into) when is_function(Into, 2) ->
    #filter{stack=[{transform, Into}]}.

-spec new(Mod, _Args) -> t(_ReqEx, _RepEx, _ReqIn, _RepIn) when Mod :: module().
new(Mod, Args) ->
    #filter{stack=[{transform, Mod, Mod:init(Args)}]}.

-spec cons(fun((ReqIn, fun((Req) -> Res)) -> RepIn),
    t(ReqEx, RepEx, ReqIn, RepIn)) -> t(ReqEx, RepEx, Req, Res).
cons(Into, #filter{stack=Stack} = Filter) when is_function(Into, 2) ->
    Filter#filter{stack=[{transform, Into} | Stack]}.

-spec append(t(ReqIn, RepIn, Req, Res), t(ReqEx, RepEx, ReqIn, RepIn)) ->
    t(ReqEx, RepEx, Req, Res).
append(#filter{stack=Stack2}, #filter{stack=Stack1} = Filter) ->
    Filter#filter{stack=(Stack2 ++ Stack1)}.

-spec reverse(t(ReqEx, RepEx, ReqIn, RepIn), stack_service:t(ReqIn, RepIn)) ->
    stack_service:t(ReqEx, RepEx).
reverse(#filter{stack=Stack}, Service) ->
    Tail = stack_service:to_list(Service),
    stack_service:from_list(lists:reverse(Stack, Tail)).

-spec init(t(ReqEx, RepEx, ReqIn, RepIn)) ->
    fun((ReqEx, fun((ReqIn) -> RepIn)) -> RepEx).
init(#filter{stack=Stack}) ->
    fun(Req, Fun) ->
        NStack = lists:reverse(Stack, [{map, Fun}]),
        stack:eval(Req, NStack)
    end.

-spec call(ReqEx, fun((ReqIn) -> RepIn), t(ReqEx, RepEx, ReqIn, RepIn)) ->
    RepEx.
call(Req, Fun, #filter{stack=Stack}) ->
    stack:eval(Req, lists:reverse(Stack, [{map, Fun}])).

-spec to_list(t(ReqEx, RepEx, _ReqIn, _RepIn)) -> stack:t(ReqEx, RepEx).
to_list(#filter{stack=Stack}) ->
    Stack.

-spec from_list(Stack) -> t(ReqEx, RepEx, _ReqIn, _RepIn)
        when Stack :: stack:t(ReqEx, RepEx).
from_list(Stack) ->
    #filter{stack=Stack}.