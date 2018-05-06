-module('Elixir.Stack.Context').

%% API exports

-export_type([t/0]).

-export([bind/2, bind/3, bind_defer/3, bind_defer/4, unbind/1, unbind/2,
         unbind_defer/2, unbind_defer/3, get/0, get/1, get/2, fetch/1,
         'fetch!'/1, 'has_key?'/1, keys/0]).

%%==============================================================================
%% API types
%%==============================================================================

-type t() :: logger:metadata().

%%==============================================================================
%% API functions
%%==============================================================================

-spec bind(t(), fun(() -> Result)) -> Result.
bind(Ctx, Fun) when is_map(Ctx), is_function(Fun, 0) ->
    case logger:get_process_metadata() of
        OldCtx when is_map(OldCtx) ->
            NCtx = maps:merge(OldCtx, Ctx),
            set(NCtx, Fun, OldCtx);
        undefined ->
            set(Ctx, Fun)
    end.

-spec bind(_Key, _Value, fun(() -> Result)) -> Result.
bind(Key, Value, Fun) when is_function(Fun, 0) ->
    case logger:get_process_metadata() of
        Ctx when is_map(Ctx) ->
            set(Ctx#{Key => Value}, Fun, Ctx);
        undefined ->
            set(#{Key => Value}, Fun)
    end.


-spec bind_defer(t(), fun(() -> Result), fun(() -> _)) -> Result.
bind_defer(Ctx, Fun, After)
        when is_map(Ctx), is_function(Fun, 0), is_function(After, 0) ->
    case logger:get_process_metadata() of
        OldCtx when is_map(OldCtx) ->
            NCtx = maps:merge(OldCtx, Ctx),
            set_defer(NCtx, Fun, OldCtx, After);
        undefined ->
            set_defer(Ctx, Fun, After)
    end.

-spec bind_defer(_Key, _Value, fun(() -> Result), fun(() -> _)) -> Result.
bind_defer(Key, Value, Fun, After)
        when is_function(Fun, 0), is_function(After, 0) ->
    case logger:get_process_metadata() of
        Ctx when is_map(Ctx) ->
            set_defer(Ctx#{Key => Value}, Fun, Ctx, After);
        undefined ->
           set_defer(#{Key => Value}, Fun, After)
    end.

-spec unbind(fun(() -> Result)) -> Result.
unbind(Fun) when is_function(Fun, 0) ->
    case logger:get_process_metadata() of
        Ctx when is_map(Ctx) ->
            unset(Fun, Ctx);
        undefined ->
            Fun()
    end.

-spec unbind([_Key], fun(() -> Result)) -> Result.
unbind(Keys, Fun) when is_list(Keys), is_function(Fun, 0) ->
    case logger:get_process_metadata() of
        Ctx when is_map(Ctx) ->
            NCtx = maps:without(Keys, Ctx),
            set(NCtx, Fun, Ctx);
        undefined ->
            Fun()
    end.

-spec unbind_defer(fun(() -> Result), fun(() -> _)) -> Result.
unbind_defer(Fun, After) when is_function(Fun, 0), is_function(After, 0) ->
    case logger:get_process_metadata() of
        Ctx when is_map(Ctx) ->
            unset_defer(Fun, Ctx, After);
        undefined ->
            Fun()
    end.

-spec unbind_defer([_Key], fun(() -> Result), fun(() -> _)) -> Result.
unbind_defer(Keys, Fun, After)
        when is_list(Keys), is_function(Fun, 0), is_function(After, 0) ->
    case logger:get_process_metadata() of
        Ctx when is_map(Ctx) ->
            NCtx = maps:without(Keys, Ctx),
            set_defer(NCtx, Fun, Ctx, After);
        undefined ->
            Fun()
    end.

-spec get() -> t().
get() ->
    case logger:get_process_metadata() of
        #{} = Ctx ->
            Ctx;
        undefined ->
            #{}
    end.

-spec get(_Key) -> _Value.
get(Key) ->
    get(Key, nil).

-spec get(_Key, Value) -> Value.
get(Key, Default) ->
    case logger:get_process_metadata() of
        #{Key := Value} ->
            Value;
        _ ->
            Default
    end.

-spec fetch(_Key) -> {ok, _Value} | error.
fetch(Key) ->
    case logger:get_process_metadata() of
        #{Key := Value} ->
            {ok, Value};
        _ ->
            error
    end.

-spec 'fetch!'(_Key) -> _Value.
'fetch!'(Key) ->
    case logger:get_process_metadata() of
        #{Key := Value} ->
            Value;
        _ ->
            erlang:error({badkey, Key})
    end.

-spec 'has_key?'(_Key) -> boolean().
'has_key?'(Key) ->
    case logger:get_process_metadata() of
        #{Key := _} ->
            true;
        _ ->
            false
    end.

-spec keys() -> [_Key].
keys() ->
    case logger:get_process_metadata() of
        Ctx when is_map(Ctx) ->
            maps:keys(Ctx);
        _ ->
            []
    end.

%% Internal

set(NewCtx, Fun, OldCtx) ->
    logger:set_process_metadata(NewCtx),
    try Fun() of
        Result ->
            Result
    after
        logger:set_process_metadata(OldCtx)
    end.

set(NewCtx, Fun) ->
    logger:set_process_metadata(NewCtx),
    try Fun() of
        Result ->
            Result
    after
        logger:unset_process_metadata()
    end.

set_defer(NewCtx, Fun, OldCtx, After) ->
    logger:set_process_metadata(NewCtx),
    try Fun() of
        Result ->
            Result
    after
        logger:set_process_metadata(OldCtx),
        _ = After()
    end.

set_defer(NewCtx, Fun, After) ->
    logger:set_process_metadata(NewCtx),
    try Fun() of
        Result ->
            Result
    after
        logger:unset_process_metadata(),
        _ = After()
    end.

unset(Fun, OldCtx) ->
    logger:unset_process_metadata(),
    try Fun() of
        Result ->
            Result
    after
        logger:set_process_metadata(OldCtx)
    end.

unset_defer(Fun, OldCtx, After) ->
    logger:unset_process_metadata(),
    try Fun() of
        Result ->
            Result
    after
        logger:set_process_metadata(OldCtx),
        _ = After()
    end.