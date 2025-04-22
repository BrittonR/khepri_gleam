%% src/khepri_multi_node_debug.erl
-module(khepri_multi_node_debug).
-export([start_debug/4, list_modules/0, find_module/1, check_paths/0]).

%% Start a debug session
start_debug(Name, Cookie, Role, PrimaryNode) ->
    io:format("Starting DEBUG session~n"),
    io:format("Node name: ~s~n", [Name]),
    io:format("Cookie: ~s~n", [Cookie]),
    io:format("Role: ~s~n", [Role]),
    
    % Start distribution
    start_dist(Name, Cookie),
    
    % Check paths and modules
    io:format("~n=== CHECKING BEAM PATHS ===~n"),
    check_paths(),
    
    io:format("~n=== LISTING ALL MODULES ===~n"),
    list_modules(),
    
    % Look for our module
    io:format("~n=== LOOKING FOR MODULE ===~n"),
    find_module("khepri_multi_node"),
    
    % Check for variations
    io:format("~n=== CHECKING MODULE VARIATIONS ===~n"),
    find_module("khepri_multi"),
    find_module("khepri_gleam"),
    find_module("gleam@khepri_multi_node"),
    find_module("gleam_khepri_multi_node"),
    
    % Try loading via application
    io:format("~n=== TRYING TO ACCESS GLEAM CODE ===~n"),
    try_gleam_app().

%% Start distribution
start_dist(Name, Cookie) ->
    % Parse the node name
    NodeAtom = list_to_atom(Name),
    CookieAtom = list_to_atom(Cookie),
    
    % Check if we need longnames or shortnames
    NameType = case string:find(Name, ".") of
        nomatch -> 
            case string:find(Name, ":") of 
                nomatch -> shortnames;
                _ -> longnames
            end;
        _ -> longnames
    end,
    
    io:format("Using name type: ~p~n", [NameType]),
    
    % Start distribution
    case net_kernel:start([NodeAtom, NameType]) of
        {ok, _Pid} ->
            erlang:set_cookie(node(), CookieAtom),
            io:format("Distribution started successfully~n");
        {error, {already_started, _Pid}} ->
            erlang:set_cookie(node(), CookieAtom),
            io:format("Distribution already started~n");
        Error ->
            io:format("Failed to start distribution: ~p~n", [Error])
    end.

%% List all loaded modules
list_modules() ->
    Modules = [M || {M, _} <- code:all_loaded()],
    SortedModules = lists:sort(Modules),
    
    % Look for any modules that might be our Gleam module
    GleamModules = [M || M <- SortedModules, 
                     string:find(atom_to_list(M), "gleam") =/= nomatch orelse
                     string:find(atom_to_list(M), "khepri") =/= nomatch],
    
    io:format("Found ~p loaded modules~n", [length(Modules)]),
    io:format("Gleam/Khepri related modules:~n"),
    [io:format("  ~p~n", [M]) || M <- GleamModules],
    
    % Get code path
    io:format("~nLooking at modules in code path:~n"),
    scan_code_path_for_gleam().

%% Check path settings
check_paths() ->
    Paths = code:get_path(),
    io:format("Code paths:~n"),
    [io:format("  ~s~n", [P]) || P <- Paths],
    
    % Count .beam files in the path
    BeamFiles = count_beam_files(Paths),
    io:format("~nFound ~p .beam files in path~n", [BeamFiles]).

%% Count .beam files in all paths
count_beam_files(Paths) ->
    lists:sum([count_beam_files_in_dir(P) || P <- Paths]).

%% Count .beam files in one directory
count_beam_files_in_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            length([F || F <- Files, filename:extension(F) =:= ".beam"]);
        _ -> 0
    end.

%% Find a specific module
find_module(ModuleName) ->
    ModuleAtom = list_to_atom(ModuleName),
    
    % Check if it's loaded
    case code:is_loaded(ModuleAtom) of
        {file, _} -> 
            io:format("Module ~s is loaded~n", [ModuleName]);
        false -> 
            io:format("Module ~s is not loaded, trying to load...~n", [ModuleName]),
            
            % Try to load it
            case code:load_file(ModuleAtom) of
                {module, _} -> 
                    io:format("Successfully loaded module ~s~n", [ModuleName]),
                    
                    % List exports
                    try
                        Exports = ModuleAtom:module_info(exports),
                        io:format("Exports:~n"),
                        [io:format("  ~p/~p~n", [F, A]) || {F, A} <- Exports]
                    catch
                        _:_ -> io:format("Could not list exports~n")
                    end;
                    
                {error, Reason} -> 
                    io:format("Failed to load module ~s: ~p~n", [ModuleName, Reason])
            end
    end.

%% Scan code path for Gleam modules
scan_code_path_for_gleam() ->
    Paths = code:get_path(),
    scan_paths_for_gleam(Paths, []).

scan_paths_for_gleam([], Found) ->
    io:format("Found ~p Gleam-related .beam files:~n", [length(Found)]),
    [io:format("  ~s~n", [F]) || F <- lists:sort(Found)];
scan_paths_for_gleam([Path | Rest], Found) ->
    case file:list_dir(Path) of
        {ok, Files} ->
            GleamFiles = [filename:join(Path, F) || 
                          F <- Files, 
                          (string:find(F, "gleam") =/= nomatch orelse
                          string:find(F, "khepri") =/= nomatch) andalso
                          filename:extension(F) =:= ".beam"],
            scan_paths_for_gleam(Rest, Found ++ GleamFiles);
        _ ->
            scan_paths_for_gleam(Rest, Found)
    end.

%% Try to access Gleam app
try_gleam_app() ->
    % Try different ways to invoke the Gleam app
    io:format("Trying different ways to invoke the Gleam application...~n"),
    
    % Try with application:load
    case application:load(khepri_gleam) of
        ok -> 
            io:format("Successfully loaded khepri_gleam application~n"),
            Modules = application:get_key(khepri_gleam, modules),
            io:format("Application modules: ~p~n", [Modules]);
        {error, Reason} ->
            io:format("Failed to load khepri_gleam application: ~p~n", [Reason])
    end,
    
    % Try this a few different ways
    try_function(fun() -> 'Elixir.Gleam.Main':run(khepri_multi_node, main) end, "Elixir.Gleam.Main:run/2"),
    try_function(fun() -> khepri_multi_node_helper:start_node("localhost", "cookie", "primary", "") end, "khepri_multi_node_helper:start_node/4").

%% Try to call a function and catch any errors
try_function(Fun, Name) ->
    io:format("Trying ~s...~n", [Name]),
    try
        Fun(),
        io:format("~s succeeded!~n", [Name])
    catch
        E:R:S -> 
            io:format("~s failed: ~p:~p~n", [Name, E, R]),
            case S of
                [] -> ok;
                _ -> io:format("Stacktrace: ~p~n", [S])
            end
    end.
