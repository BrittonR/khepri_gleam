%% src/khepri_direct_helper.erl
-module(khepri_direct_helper).
-export([start_node/4, node_info/0]).

%% Start a node with the given distribution parameters and role
start_node(Name, Cookie, Role, PrimaryNode) ->
    io:format("Starting distributed node: ~s~n", [Name]),
    io:format("Using cookie: ~s~n", [Cookie]),
    io:format("Role: ~s~n", [Role]),
    
    % Start Khepri
    io:format("Starting Khepri...~n"),
    Result = case khepri:start() of
        {ok, StoreId} -> 
            io:format("Khepri started successfully with store ID: ~p~n", [StoreId]),
            {ok, StoreId};
        Error -> 
            io:format("Error starting Khepri: ~p~n", [Error]),
            Error
    end,
    
    % Additional logic based on role
    case Role of
        "primary" ->
            io:format("Running as primary node~n"),
            % Store some test data
            Path = [<<"test">>, <<"data">>],
            Value = <<"test_value_from_primary">>,
            khepri:put(Path, Value),
            io:format("Stored test data at /test/data~n"),
            % Print node info
            node_info(),
            % Store more structured test data
            store_test_data();
        "secondary" ->
            io:format("Running as secondary node~n"),
            io:format("Joining primary node: ~s~n", [PrimaryNode]),
            % Join the cluster with retries
            join_primary(PrimaryNode, 5),
            % Print node info
            node_info();
        "client" ->
            io:format("Running as client node~n"),
            io:format("Connecting to primary node: ~s~n", [PrimaryNode]),
            % Just connect and read data
            case connect_to_primary(PrimaryNode, 5) of
                ok ->
                    % Try to read data from the cluster
                    read_test_data();
                error ->
                    io:format("Failed to connect to the cluster~n")
            end
    end,
    
    io:format("Node is running. Press Ctrl+C to stop.~n"),
    
    % Return result of Khepri start
    Result.

%% Get information about the current node
node_info() ->
    NodeName = node(),
    Nodes = nodes(),
    io:format("Current node: ~p~n", [NodeName]),
    io:format("Connected nodes: ~p~n", [Nodes]),
    
    % Get Khepri info if available
    try
        case khepri_cluster:members() of
            {ok, Members} -> 
                io:format("Khepri cluster members: ~p~n", [Members]);
            Error -> 
                io:format("Failed to get cluster members: ~p~n", [Error])
        end,
        
        case khepri_cluster:nodes() of
            {ok, KNodes} -> 
                io:format("Khepri cluster nodes: ~p~n", [KNodes]);
            Error2 -> 
                io:format("Failed to get cluster nodes: ~p~n", [Error2])
        end,
        ok
    catch
        E:R:ST ->
            io:format("Error getting Khepri info: ~p:~p~n", [E, R]),
            io:format("Stack trace: ~p~n", [ST]),
            error
    end.

%% Store structured test data for the cluster
store_test_data() ->
    io:format("Storing structured test data...~n"),
    % Create some test paths with data
    Paths = [
        {[<<"cluster_test">>, <<"fruits">>, <<"apple">>], <<"primary_value_0">>},
        {[<<"cluster_test">>, <<"fruits">>, <<"banana">>], <<"primary_value_1">>},
        {[<<"cluster_test">>, <<"vegetables">>, <<"carrot">>], <<"primary_value_2">>}
    ],
    
    % Store each item
    lists:foreach(fun({Path, Value}) ->
        % Format path for display
        PathStr = format_path(Path),
        io:format("Writing: ~s = ~s~n", [PathStr, Value]),
        % Store in Khepri
        khepri:put(Path, Value)
    end, Paths).

%% Read test data from the cluster
read_test_data() ->
    io:format("Reading test data from the cluster:~n"),
    % Try to read both simple and structured data
    try_read_path([<<"test">>, <<"data">>]),
    try_read_path([<<"cluster_test">>, <<"fruits">>, <<"apple">>]),
    try_read_path([<<"cluster_test">>, <<"fruits">>, <<"banana">>]),
    try_read_path([<<"cluster_test">>, <<"vegetables">>, <<"carrot">>]).

%% Try to read data at a path
try_read_path(Path) ->
    PathStr = format_path(Path),
    case khepri:get(Path) of
        {ok, Value} ->
            io:format("  ~s = ~p~n", [PathStr, Value]);
        {error, Reason} ->
            io:format("  ~s = ERROR: ~p~n", [PathStr, Reason])
    end.

%% Format a path for display
format_path(Path) ->
    Parts = lists:map(fun(Part) when is_binary(Part) -> 
                          binary_to_list(Part);
                         (Part) -> 
                          io_lib:format("~p", [Part])
                      end, Path),
    "/" ++ string:join([":" ++ P || P <- Parts], "/").

%% Connect to primary node with retries
connect_to_primary(PrimaryNode, Retries) ->
    io:format("Connecting to primary node ~s (attempts left: ~p)~n", [PrimaryNode, Retries]),
    try
        PrimaryAtom = list_to_atom(PrimaryNode),
        case net_kernel:connect_node(PrimaryAtom) of
            true ->
                io:format("Successfully connected to primary node!~n"),
                ok;
            false when Retries > 0 ->
                io:format("Failed to connect, retrying in 2 seconds...~n"),
                timer:sleep(2000),
                connect_to_primary(PrimaryNode, Retries - 1);
            false ->
                io:format("Failed to connect after all retries~n"),
                error
        end
    catch
        E:R:ST ->
            io:format("Error connecting to primary: ~p:~p~n", [E, R]),
            io:format("Stack trace: ~p~n", [ST]),
            case Retries > 0 of
                true ->
                    io:format("Retrying in 2 seconds...~n"),
                    timer:sleep(2000),
                    connect_to_primary(PrimaryNode, Retries - 1);
                false ->
                    io:format("Failed to connect after all retries~n"),
                    error
            end
    end.

%% Join Khepri cluster with retries
join_primary(PrimaryNode, Retries) ->
    % First ensure we're connected
    case connect_to_primary(PrimaryNode, Retries) of
        ok ->
            try_join_cluster(list_to_atom(PrimaryNode), Retries);
        error ->
            io:format("Cannot join cluster without connection to primary~n")
    end.

%% Try to join Khepri cluster
try_join_cluster(PrimaryAtom, Retries) ->
    io:format("Joining Khepri cluster via ~p (attempts left: ~p)~n", [PrimaryAtom, Retries]),
    try
        case khepri_cluster:join(PrimaryAtom) of
            ok -> 
                io:format("Successfully joined the Khepri cluster!~n"),
                % Try reading data to verify
                timer:sleep(1000), % Give cluster time to sync
                read_test_data();
            {error, Reason} when Retries > 0 ->
                io:format("Failed to join cluster: ~p, retrying in 2 seconds...~n", [Reason]),
                timer:sleep(2000),
                try_join_cluster(PrimaryAtom, Retries - 1);
            {error, Reason} ->
                io:format("Failed to join cluster after all retries: ~p~n", [Reason])
        end
    catch
        E:R:ST ->
            io:format("Error joining cluster: ~p:~p~n", [E, R]),
            io:format("Stack trace: ~p~n", [ST]),
            case Retries > 0 of
                true ->
                    io:format("Retrying in 2 seconds...~n"),
                    timer:sleep(2000),
                    try_join_cluster(PrimaryAtom, Retries - 1);
                false ->
                    io:format("Failed to join after all retries~n")
            end
    end.
