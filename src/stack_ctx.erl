-module(stack_ctx).

%% API exports

-export_type([t/0]).

-export([merge/2, merge_defer/3, update/3, update_defer/4, with/2, with_defer/3,
         without/2, without_defer/3, find/1, get/1, get/2, is_key/1, keys/0]).

%%==============================================================================
%% API types
%%==============================================================================

-type t() :: logger:metadata().

%%==============================================================================
%% API functions
%%==============================================================================

-spec merge(t(), fun(() -> Result)) -> Result.
merge(Ctx, Fun) when is_map(Ctx), is_function(Fun, 0) ->
    case logger:get_process_metadata() of
        OldCtx when is_map(Ctx) ->
            NCtx = maps:merge(OldCtx, Ctx),
            set(NCtx, Fun, OldCtx);
        undefined ->
            set(Ctx, Fun)
    end.

-spec merge_defer(t(), fun(() -> Result), fun(() -> _)) -> Result.
merge_defer(Ctx, Fun, After)
        when is_map(Ctx), is_function(Fun, 0), is_function(After, 0) ->
    case logger:get_process_metadata() of
        OldCtx when is_map(Ctx) ->
            NCtx = maps:merge(OldCtx, Ctx),
            set_defer(NCtx, Fun, OldCtx, After);
        undefined ->
            set_defer(Ctx, Fun, After)
    end.

-spec update(_Key, _Value, fun(() -> Result)) -> Result.
update(Key, Value, Fun) when is_function(Fun, 0) ->
    case logger:get_process_metadata() of
        Ctx when is_map(Ctx) ->
            set(Ctx#{Key => Value}, Fun, Ctx);
        undefined ->
            set(#{Key => Value}, Fun)
    end.

-spec update_defer(_Key, _Value, fun(() -> Result), fun(() -> _)) -> Result.
update_defer(Key, Value, Fun, After)
        when is_function(Fun, 0), is_function(After, 0) ->
    case logger:get_process_metadata() of
        Ctx when is_map(Ctx) ->
            set_defer(Ctx#{Key => Value}, Fun, Ctx, After);
        undefined ->
            set_defer(#{Key => Value}, Fun, After)
    end.

-spec with([_Key], fun(() -> Result)) -> Result.
with(Keys, Fun) when is_list(Keys), is_function(Fun, 0) ->
    case logger:get_process_metadata() of
        Ctx when is_map(Ctx) ->
            NCtx = maps:with(Keys, Ctx),
            set(NCtx, Fun, Ctx);
        undefined ->
            Fun()
    end.

-spec with_defer([_Key], fun(() -> Result), fun(() -> _)) -> Result.
with_defer(Keys, Fun, After)
        when is_list(Keys), is_function(Fun, 0), is_function(After, 0) ->
    case logger:get_process_metadata() of
        Ctx when is_map(Ctx) ->
            NCtx = maps:with(Keys, Ctx),
            set_defer(NCtx, Fun, Ctx, After);
        undefined ->
            try Fun() of
                Result ->
                    Result
            after
                After()
            end
    end.

-spec without([_Key], fun(() -> Result)) -> Result.
without(Keys, Fun) when is_list(Keys), is_function(Fun, 0) ->
    case logger:get_process_metadata() of
        Ctx when is_map(Ctx) ->
            NCtx = maps:without(Keys, Ctx),
            set(NCtx, Fun, Ctx);
        undefined ->
            Fun()
    end.

-spec without_defer([_Key], fun(() -> Result), fun(() -> _)) -> Result.
without_defer(Keys, Fun, After)
        when is_list(Keys), is_function(Fun, 0), is_function(After, 0) ->
    case logger:get_process_metadata() of
        Ctx when is_map(Ctx) ->
            NCtx = maps:without(Keys, Ctx),
            set_defer(NCtx, Fun, Ctx, After);
        undefined ->
            try Fun() of
                Result ->
                    Result
            after
                After()
            end
    end.

-spec find(_Key) -> {ok, _Value} | error.
find(Key) ->
    case logger:get_process_metadata() of
        #{Key := Value} ->
            {ok, Value};
        _ ->
            error
    end.

-spec get(_Key) -> _Value.
get(Key) ->
    case logger:get_process_metadata() of
        #{Key := Value} ->
            Value;
        _ ->
            erlang:error({badkey, Key})
    end.

-spec get(_Key, Value) -> Value.
get(Key, Default) ->
    case logger:get_process_metadata() of
        #{Key := Value} ->
            Value;
        _ ->
            Default
    end.

-spec is_key(_Key) -> boolean().
is_key(Key) ->
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