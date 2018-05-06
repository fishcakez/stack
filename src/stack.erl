-module(stack).

%% API exports

-export_type([t/2, entry/0]).

-export([eval/2]).

%%==============================================================================
%% API types
%%==============================================================================

-type t(_Req, _Rep) :: [entry()].

-type entry() ::
    {map, fun((_Req) -> _Rep)} |
    {transform, fun((_ReqEx, fun((_ReqIn) -> _RepIn)) -> _RepEx)} |
    {map | transform, module(), _State}.

%%==============================================================================
%% API functions
%%==============================================================================

-spec eval(Req, t(Req, Rep)) -> Rep.
eval(Req, []) ->
    Req;
eval(Req, [{map, Map} | Stack]) ->
    Map(eval(Req, Stack));
eval(Req, [{map, Mod, State} | Stack]) ->
    Mod:call(eval(Req, Stack), State);
eval(ReqEx, [{transform, Into} | Stack]) ->
    Into(ReqEx, fun(ReqIn) -> eval(ReqIn, Stack) end);
eval(ReqEx, [{transform, Mod, State} | Stack]) ->
    Mod:call(ReqEx, fun(ReqIn) -> eval(ReqIn, Stack) end, State).