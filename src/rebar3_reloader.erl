-module(rebar3_reloader).

-export([
    init/1,
    suspend/0,
    resume/0
]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_reloader_prv:init(State),
    {ok, State1}.

suspend() ->
    ?MODULE ! suspend.

resume() ->
    ?MODULE ! resume.