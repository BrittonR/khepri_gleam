%% src/khepri_direct_helper.erl
-module(khepri_direct_helper).
-export([start_node/4]).

%% Start a node with direct function calls to bypass Gleam argument parsing
start_node(Name, Cookie, Role, PrimaryNode) ->
    io:format("Starting distributed node: ~s~n", [Name]),
    io:format("Using cookie: ~s~n", [Cookie]),
    io:format("Role: ~s~n", [Role]),
    
    % Start distribution
    NodeAtom = list_to_atom(Name),
    CookieAtom = list_to_atom(Cookie),
    
    % Determine name type
    NameType = case string:find(Name, ".") of
        nomatch -> shortnames;
        _ -> longnames
    end,
    
    io:format("Using name type: ~p~n", [NameType]),
    
    % Start distribution
    {ok, _} = net_kernel:start([NodeAtom, NameType]),
    erlang:set_cookie(node(), CookieAtom),
    io:format("Distribution started successfully~n"),
    
    % Start Khepri
    io:format("Starting Khepri...~n"),
    khepri:start(),
    
    % Start the Khepri cluster
    io:format("Starting Khepri cluster as ~s node~n", [Role]),
    
    % Run appropriate function based on role
    case Role of
        "primary" ->
            run_primary();
        "secondary" ->
            run_secondary(PrimaryNode);
        "client" ->
            run_client(PrimaryNode);
        _ ->
            io:format("Unknown role: ~s~n", [Role])
    end,
    
    % Keep the node running
    io:format("Node is running. Press Ctrl+C to stop.~n"),
    timer:sleep(infinity).

%% Run primary node
run_primary() ->
    % Wait for leader election
    io:format("Waiting for leader election...~n"),
    khepri_cluster:wait_for_leader(),
    io:format("Leader election completed~n"),
    
    % Store some test data
    write_test_data(),
    
    % Print cluster status
    print_cluster_status(),
    
    % Read test data to confirm it was stored
    read_test_data().

%% Run secondary node
run_secondary(PrimaryNode) ->
    io:format("Joining primary node: ~s~n", [PrimaryNode]),
    
    % Join the cluster
    case khepri_cluster:join(list_to_atom(PrimaryNode)) of
        ok ->
            io:format("Successfully joined the cluster!~n"),
            
            % Print cluster status
            print_cluster_status(),
            
            % Read data from the cluster
            io:format("~nReading data from the cluster:~n"),
            read_test_data(),
            
            % Write some data
            io:format("~nWriting data to the cluster:~n"),
            write_secondary_data();
        {error, Reason} ->
            io:format("Failed to join cluster: ~p~n", [Reason]),
            io:format("~nPossible reasons for failure:~n"),
            io:format("1. The primary node isn't running~n"),
            io:format("2. The node names are incorrect~n"),
            io:format("3. The cookie values don't match~n"),
            io:format("4. Network connectivity issues~n")
    end.

%% Run client node
run_client(PrimaryNode) ->
    io:format("Connecting to primary node: ~s~n", [PrimaryNode]),
    
    % Join the cluster
    case khepri_cluster:join(list_to_atom(PrimaryNode)) of
        ok ->
            io:format("Successfully joined the cluster!~n"),
            
            % Print cluster status
            print_cluster_status(),
            
            % Read all data
            io:format("~nReading data from the cluster:~n"),
            read_test_data(),
            read_secondary_data(),
            
            % Write client data
            write_client_data(),
            
            % Read again to verify
            io:format("~nReading all data after updates:~n"),
            read_test_data(),
            read_secondary_data(),
            read_client_data();
        {error, Reason} ->
            io:format("Failed to join cluster: ~p~n", [Reason])
    end.

%% Print cluster status (directly using khepri_cluster module)
print_cluster_status() ->
    io:format("~nCluster status:~n"),
    
    % Get cluster members
    case khepri_cluster:members() of
        {ok, Members} ->
            io:format("  Members: ~p~n", [Members]);
        {error, Reason} ->
            io:format("  Failed to get members: ~p~n", [Reason])
    end,
    
    % Get cluster nodes
    case khepri_cluster:nodes() of
        {ok, Nodes} ->
            io:format("  Nodes: ~p~n", [Nodes]);
        {error, Reason} ->
            io:format("  Failed to get nodes: ~p~n", [Reason])
    end.

%% Helper functions for data operations - using direct Khepri calls

write_test_data() ->
    io:format("~nWriting test data to the cluster...~n"),
    
    % Create paths using binary strings
    Paths = [
        [<<"cluster_test">>, <<"fruits">>, <<"apple">>],
        [<<"cluster_test">>, <<"fruits">>, <<"banana">>],
        [<<"cluster_test">>, <<"vegetables">>, <<"carrot">>]
    ],
    
    lists:foreach(fun(Path) ->
        % Create value based on the path
        Value = case Path of
            [_, _, <<"apple">>] -> <<"primary_value_0">>;
            [_, _, <<"banana">>] -> <<"primary_value_1">>;
            _ -> <<"primary_value_2">>
        end,
        
        % Format path for display
        PathStr = format_path(Path),
        io:format("Writing: ~s = ~s~n", [PathStr, Value]),
        
        % Write to Khepri
        khepri:put(Path, Value)
    end, Paths).

write_secondary_data() ->
    Path = [<<"cluster_test">>, <<"secondary">>, <<"data">>],
    Value = <<"secondary_node_data">>,
    
    PathStr = format_path(Path),
    io:format("Writing: ~s = ~s~n", [PathStr, Value]),
    
    khepri:put(Path, Value).

write_client_data() ->
    Path = [<<"cluster_test">>, <<"client">>, <<"data">>],
    Value = <<"client_node_data">>,
    
    PathStr = format_path(Path),
    io:format("~nWriting client data: ~s = ~s~n", [PathStr, Value]),
    
    khepri:put(Path, Value).

read_test_data() ->
    io:format("Reading primary node test data:~n"),
    
    Paths = [
        [<<"cluster_test">>, <<"fruits">>, <<"apple">>],
        [<<"cluster_test">>, <<"fruits">>, <<"banana">>],
        [<<"cluster_test">>, <<"vegetables">>, <<"carrot">>]
    ],
    
    lists:foreach(fun(Path) ->
        PathStr = format_path(Path),
        case khepri:get(Path) of
            {ok, Value} -> io:format("  ~s = ~p~n", [PathStr, Value]);
            {error, Reason} -> io:format("  ~s = ERROR: ~p~n", [PathStr, Reason])
        end
    end, Paths).

read_secondary_data() ->
    io:format("Reading secondary node data:~n"),
    
    Path = [<<"cluster_test">>, <<"secondary">>, <<"data">>],
    PathStr = format_path(Path),
    
    case khepri:get(Path) of
        {ok, Value} -> io:format("  ~s = ~p~n", [PathStr, Value]);
        {error, Reason} -> io:format("  ~s = ERROR: ~p~n", [PathStr, Reason])
    end.

read_client_data() ->
    io:format("Reading client node data:~n"),
    
    Path = [<<"cluster_test">>, <<"client">>, <<"data">>],
    PathStr = format_path(Path),
    
    case khepri:get(Path) of
        {ok, Value} -> io:format("  ~s = ~p~n", [PathStr, Value]);
        {error, Reason} -> io:format("  ~s = ERROR: ~p~n", [PathStr, Reason])
    end.

%% Format a path for display
format_path(Path) ->
    Parts = lists:map(fun(Part) when is_binary(Part) -> 
                          binary_to_list(Part);
                         (Part) -> 
                          io_lib:format("~p", [Part])
                      end, Path),
    "/" ++ string:join([":" ++ P || P <- Parts], "/").
