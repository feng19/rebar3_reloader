-module(rebar3_reloader_prv).

-export([init/1, do/1, format_error/1]).
-export([
    reloader_1/1,
    reloader/1,
    suspend/1
]).

-define(PROVIDER, reloader).
-define(DEPS, [compile]).

-record(state, {
    time = 500 :: integer(),
    cmd = "rebar3 compile" :: string(),
    listen_dirs = [] :: [file:filename()],
    valid_extensions = [] :: [binary()],
    last_time :: calendar:datetime(),
    modules = [] :: [module()],
    excluded_modules = [] :: [module()]
}).

-include_lib("kernel/include/file.hrl").

-define(VALID_EXTENSIONS, [<<".erl">>, <<".hrl">>, <<".src">>, <<".config">>, <<".lock">>,
    <<".c">>, <<".cpp">>, <<".h">>, <<".hpp">>, <<".cc">>]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},            % The 'user friendly' name of the task
        {module, ?MODULE},            % The module implementation of the task
        {bare, true},                 % The task can be run by the user, always true
        {deps, ?DEPS},                % The list of dependencies
        {example, "rebar3 reloader"}, % How to use the plugin
        {opts, [
            {config, undefined, "config", string,
                "Path to the config file to use. Defaults to "
                "{shell, [{config, File}]} and then the relx "
                "sys.config file if not specified."},
            {name, undefined, "name", atom, "Gives a long name to the node."},
            {sname, undefined, "sname", atom, "Gives a short name to the node."},
            {setcookie, undefined, "setcookie", atom, "Sets the cookie if the node is distributed."},
            {script_file, undefined, "script", string,
                "Path to an escript file to run before "
                "starting the project apps. Defaults to "
                "rebar.config {shell, [{script_file, File}]} "
                "if not specified."},
            {apps, undefined, "apps", string,
                "A list of apps to boot before starting the "
                "shell. (E.g. --apps app1,app2,app3) Defaults "
                "to rebar.config {shell, [{apps, Apps}]} or "
                "relx apps if not specified."}]},
        {short_desc, "auto compile & reload new beam"},
        {desc, "Automatically run compile task on change of source file and reload modules."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    spawn(
        fun() ->
            ?MODULE:reloader_1(State)
        end),
    rebar_prv_shell:do(State).

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

reloader_1(RebarState) ->
    case whereis(rebar_agent) of
        undefined ->
            timer:sleep(100),
            ?MODULE:reloader_1(RebarState);
        _ ->
            listen_on_project_apps(RebarState),
            Time = application:get_env(rebar, reloader_wait_stop_change_time, 500),
            CMD = application:get_env(rebar, reloader_compile_cmd, "rebar3 compile"),
            ValidExtensions = [Ext || Ext <- application:get_env(rebar, reloader_extra_exts, []), is_binary(Ext)]
                ++ ?VALID_EXTENSIONS,
            ExclApps = [rebar, erlware_commons, providers, cf, cth_readable]
                ++ application:get_env(rebar, reloader_excluded_apps, []),
            ExcludedModules0 = lists:flatten([
                begin
                    case application:get_key(ExclApp, modules) of
                        undefined -> [];
                        AppModules -> AppModules
                    end
                end || ExclApp <- ExclApps]) ++
                application:get_env(rebar, reloader_excluded_modules, []),
            {Modules, ExcludedModules1} = partition_modules(ExcludedModules0),
            ExcludedModules = lists:usort([M || {M, _} <- ExcludedModules1] ++ ExcludedModules0),
            State = #state{
                time = Time,
                cmd = CMD,
                valid_extensions = ValidExtensions,
                last_time = erlang:localtime(),
                modules = [M || {M, _} <- Modules],
                excluded_modules = ExcludedModules
            },
            ?MODULE:reloader(State)
    end.

reloader(State) ->
    receive
        {ChangedFile, _Events} ->
            Ext = filename:extension(unicode:characters_to_binary(ChangedFile)),
            case lists:member(Ext, State#state.valid_extensions) of
                false ->
                    ?MODULE:reloader(State);
                true ->
                    % sleep here so messages can bottle up
                    % or we can flush after compile?
                    timer:sleep(200),
                    flush(State#state.time),
                    compile_and_reload_beam(State)
            end;
        suspend ->
            ?MODULE:suspend();
        _ ->
            ?MODULE:reloader(State)
    end.

flush(Time) ->
    receive
        _ -> flush(Time)
    after Time -> ok
    end.

suspend(State) ->
    receive
        resume ->
            ?MODULE:reloader(State);
        _ ->
            ?MODULE:suspend(State)
    end.

compile_and_reload_beam(State) ->
    io:format(os:cmd(State#state.cmd)),
    Now = erlang:localtime(),
    ExcludedModules0 = State#state.excluded_modules,
    {Modules0, ExcludedModules1} = partition_modules(ExcludedModules0),
    OldMods = State#state.modules,
    {NeedModules, _NewAddModules} = lists:partition(
        fun({M, _File}) ->
            lists:member(M, OldMods) orelse lists:member(M, ExcludedModules0)
        end, Modules0),
    reload_modules(NeedModules, State#state.last_time),

    Modules = lists:usort([M || {M, _} <- Modules0] ++ OldMods),
    ExcludedModules = lists:usort([M || {M, _} <- ExcludedModules1] ++ ExcludedModules0),
    flush(State#state.time),
    ?MODULE:reloader(State#state{
        last_time = Now, modules = Modules, excluded_modules = ExcludedModules}).

listen_on_project_apps(State) ->
    CheckoutDeps = [AppInfo ||
        AppInfo <- rebar_state:all_deps(State),
        rebar_app_info:is_checkout(AppInfo) == true
    ],
    ProjectApps = rebar_state:project_apps(State),
    AppDirs = [rebar_app_info:dir(AppInfo) || AppInfo <- ProjectApps ++ CheckoutDeps],
    Dirs = lists:usort(["src", "c_src" |
        [Dir || Dir <- application:get_env(rebar, reloader_extra_dirs, []), is_list(Dir)]]),
    [begin
         NewDir = filename:join(AppDir, Dir),
         case filelib:is_dir(NewDir) of
             true -> enotify:start_link(NewDir);
             false -> ignore
         end
     end || AppDir <- AppDirs, Dir <- Dirs].

partition_modules(ExcludedModules0) ->
    LibDir = code:lib_dir(),
    Script = hd(init:get_plain_arguments()),
    Rebar3Dir = filename:absname(Script),
    lists:partition(
        fun({M, File}) ->
            not code:is_sticky(M)
                andalso not lists:member(M, ExcludedModules0)
                andalso not lists:prefix(LibDir, File)
                andalso not lists:prefix(Rebar3Dir, File)
        end, [E || {_, File} = E <- code:all_loaded(), filelib:is_file(File)]).

%% @private takes a list of modules and reloads them
-spec reload_modules([module()], calendar:datetime()) -> term().
reload_modules([], _LastTime) -> noop;
reload_modules(Modules0, LastTime) ->
    Modules = [M || {M, File} <- Modules0, is_changed(File, LastTime)],
    reload_modules_do(Modules, erlang:function_exported(code, prepare_loading, 1)).

is_changed(Filename, LastTime) ->
    case file:read_file_info(Filename) of
        {ok, #file_info{mtime = Mtime}} ->
            Mtime >= LastTime;
        _ ->
            false
    end.

%% @private reloading modules, when there are modules to actually reload
reload_modules_do([], _) -> noop;
reload_modules_do(Modules, true) ->
    %% OTP 19 and later -- use atomic loading and ignore unloadable mods
    case code:prepare_loading(Modules) of
        {ok, Prepared} ->
            rebar_api:info("Reloading Modules:~w.", [Modules]),
            [code:purge(M) || M <- Modules],
            code:finish_loading(Prepared);
        {error, ModRsns} ->
            Blacklist = lists:foldr(
                fun({ModError, Error}, Acc) ->
                    case Error of
                        % perhaps cover other cases of failure?
                        on_load_not_allowed ->
                            reload_modules([ModError], false),
                            [ModError | Acc];
                        _ ->
                            rebar_api:debug("Module ~p failed to atomic load because ~p", [ModError, Error]),
                            [ModError | Acc]
                    end
                end, [], ModRsns),
            reload_modules(Modules -- Blacklist, true)
    end;
reload_modules_do(Modules, false) ->
    %% Older versions, use a more ad-hoc mechanism.
    lists:foreach(
        fun(M) ->
            code:delete(M),
            code:purge(M),
            case code:load_file(M) of
                {module, M} ->
                    rebar_api:info("Reloading ~p ... ok.", [M]),
                    ok;
                {error, Error} ->
                    rebar_api:debug("Module ~p failed to load because ~p", [M, Error])
            end
        end, Modules).
