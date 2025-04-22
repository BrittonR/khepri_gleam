%% src/khepri_multi_node_helper.erl
-module(khepri_multi_node_helper).
-export([start_node/4, start_erl_distribution/2]).

%% Start a node with the given distribution parameters and role
start_node(Name, Cookie, Role, PrimaryNode) ->
    io:format("Starting distributed node: ~s~n", [Name]),
    io:format("Using cookie: ~s~n", [Cookie]),
    io:format("Role: ~s~n", [Role]),
    
    % Start distribution
    case start_erl_distribution(Name, Cookie) of
        ok ->
            io:format("Distribution started successfully~n"),
            % Build arguments based on role
            Args = case Role of
                "primary" -> ["primary"];
                "secondary" -> ["secondary", PrimaryNode];
                "client" -> ["client", PrimaryNode];
                _ -> []
            end,
            
            % Set the application arguments - this is how Gleam gets its args
            application:set_env(kernel, gleam_start_args, Args),
            
            % Call the Gleam main function - note: main/0, not main_0
            try
                io:format("Calling main function with args: ~p~n", [Args]),
                khepri_multi_node:main()
            catch
                E:R:S ->
                    io:format("Error executing main: ~p:~p~n", [E, R]),
                    io:format("Stacktrace: ~p~n", [S])
            end;
        {error, Reason} ->
            io:format("Failed to start distribution: ~p~n", [Reason])
    end.

%% Start Erlang distribution with given node name and cookie
start_erl_distribution(Name, Cookie) ->
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
            % Set the cookie
            erlang:set_cookie(node(), CookieAtom),
            ok;
        {error, {already_started, _Pid}} ->
            % Already started is fine
            erlang:set_cookie(node(), CookieAtom),
            ok;
        Error ->
            Error
    end.
