%% src/khepri_gleam_cluster_helper.erl
-module(khepri_gleam_cluster_helper).
-export([
    % Store management
    start_store/0,
    start_store_with_name/1, 
    start_store_with_path/2,
    start_store_with_timeout/3,
    stop_store/0,
    stop_store_with_name/1,
    
    % Cluster management
    join_cluster/1,
    join_cluster_with_timeout/2,
    reset_cluster/0,
    reset_cluster_with_timeout/1,
    
    % Cluster information
    get_store_id/0,
    get_cluster_members/0,
    get_cluster_members_with_name/1,
    get_cluster_nodes/0,
    get_cluster_nodes_with_name/1,
    wait_for_leader/0,
    wait_for_leader_with_timeout/1,
    
    % Helper functions
    is_store_running/1,
    list_store_ids/0,
    format_members/1,
    format_error/1
]).

%% -----------------------------------------------
%% Store Management Functions
%% -----------------------------------------------

%% Start a store with default settings
start_store() ->
    try
        Result = khepri:start(),
        case Result of
            {ok, StoreId} -> {ok, atom_to_list(StoreId)};
            {error, Reason} -> {error, format_error(Reason)}
        end
    catch
        error:Error -> {error, format_error(Error)}
    end.

%% Start a store with a specified name or data directory
start_store_with_name(NameOrPath) ->
    try
        % Convert NameOrPath to the appropriate type
        Arg = if 
            is_list(NameOrPath) -> list_to_binary(NameOrPath); % Path as binary
            true -> list_to_atom(NameOrPath) % Name as atom
        end,
        
        Result = khepri:start(Arg),
        case Result of
            {ok, StoreId} -> {ok, atom_to_list(StoreId)};
            {error, Reason} -> {error, format_error(Reason)}
        end
    catch
        error:Error -> {error, format_error(Error)}
    end.

%% Start a store with a specified path and store ID
start_store_with_path(Path, StoreId) ->
    try
        % Convert Path to binary and StoreId to atom
        PathBin = list_to_binary(Path),
        StoreIdAtom = list_to_atom(StoreId),
        
        Result = khepri:start(PathBin, StoreIdAtom),
        case Result of
            {ok, ResultStoreId} -> {ok, atom_to_list(ResultStoreId)};
            {error, Reason} -> {error, format_error(Reason)}
        end
    catch
        error:Error -> {error, format_error(Error)}
    end.

%% Start a store with a specified path, store ID, and timeout
start_store_with_timeout(Path, StoreId, Timeout) ->
    try
        % Convert Path to binary and StoreId to atom
        PathBin = list_to_binary(Path),
        StoreIdAtom = list_to_atom(StoreId),
        
        Result = khepri:start(PathBin, StoreIdAtom, Timeout),
        case Result of
            {ok, ResultStoreId} -> {ok, atom_to_list(ResultStoreId)};
            {error, Reason} -> {error, format_error(Reason)}
        end
    catch
        error:Error -> {error, format_error(Error)}
    end.

%% Stop a store with default settings
stop_store() ->
    try
        Result = khepri:stop(),
        case Result of
            ok -> {ok, nil};
            {error, Reason} -> {error, format_error(Reason)}
        end
    catch
        error:Error -> {error, format_error(Error)}
    end.

%% Stop a store with a specified name
stop_store_with_name(StoreId) ->
    try
        % Convert StoreId to atom
        StoreIdAtom = list_to_atom(StoreId),
        
        Result = khepri:stop(StoreIdAtom),
        case Result of
            ok -> {ok, nil};
            {error, Reason} -> {error, format_error(Reason)}
        end
    catch
        error:Error -> {error, format_error(Error)}
    end.

%% -----------------------------------------------
%% Cluster Management Functions
%% -----------------------------------------------

%% Join a remote cluster
join_cluster(RemoteNode) ->
    try
        % Convert RemoteNode to atom
        RemoteNodeAtom = list_to_atom(RemoteNode),
        
        Result = khepri_cluster:join(RemoteNodeAtom),
        case Result of
            ok -> {ok, nil};
            {error, Reason} -> {error, format_error(Reason)}
        end
    catch
        error:Error -> {error, format_error(Error)}
    end.

%% Join a remote cluster with timeout
join_cluster_with_timeout(RemoteNode, Timeout) ->
    try
        % Convert RemoteNode to atom
        RemoteNodeAtom = list_to_atom(RemoteNode),
        
        Result = khepri_cluster:join(RemoteNodeAtom, Timeout),
        case Result of
            ok -> {ok, nil};
            {error, Reason} -> {error, format_error(Reason)}
        end
    catch
        error:Error -> {error, format_error(Error)}
    end.

%% Reset the local store (leave the cluster)
reset_cluster() ->
    try
        Result = khepri_cluster:reset(),
        case Result of
            ok -> {ok, nil};
            {error, Reason} -> {error, format_error(Reason)}
        end
    catch
        error:Error -> {error, format_error(Error)}
    end.

%% Reset the local store with timeout
reset_cluster_with_timeout(Timeout) ->
    try
        Result = khepri_cluster:reset(Timeout),
        case Result of
            ok -> {ok, nil};
            {error, Reason} -> {error, format_error(Reason)}
        end
    catch
        error:Error -> {error, format_error(Error)}
    end.

%% -----------------------------------------------
%% Cluster Information Functions
%% -----------------------------------------------

%% Get the default Khepri store ID
get_store_id() ->
    StoreId = khepri_cluster:get_default_store_id(),
    atom_to_list(StoreId).

%% Check if a store is running
is_store_running(StoreId) when is_list(StoreId) ->
    AtomStoreId = list_to_atom(StoreId),
    khepri_cluster:is_store_running(AtomStoreId);
is_store_running(StoreId) when is_atom(StoreId) ->
    khepri_cluster:is_store_running(StoreId).

%% List all running store IDs
list_store_ids() ->
    StoreIds = khepri_cluster:get_store_ids(),
    [atom_to_list(Id) || Id <- StoreIds].

%% Get cluster members with default store ID
get_cluster_members() ->
    try
        Result = khepri_cluster:members(),
        format_members(Result)
    catch
        error:Error -> {error, format_error(Error)}
    end.

%% Get cluster members with specified store ID
get_cluster_members_with_name(StoreId) ->
    try
        % Convert StoreId to atom
        StoreIdAtom = list_to_atom(StoreId),
        
        Result = khepri_cluster:members(StoreIdAtom),
        format_members(Result)
    catch
        error:Error -> {error, format_error(Error)}
    end.

%% Get cluster nodes with default store ID
get_cluster_nodes() ->
    try
        Result = khepri_cluster:nodes(),
        case Result of
            {ok, Nodes} -> {ok, [atom_to_list(Node) || Node <- Nodes]};
            {error, Reason} -> {error, format_error(Reason)}
        end
    catch
        error:Error -> {error, format_error(Error)}
    end.

%% Get cluster nodes with specified store ID
get_cluster_nodes_with_name(StoreId) ->
    try
        % Convert StoreId to atom
        StoreIdAtom = list_to_atom(StoreId),
        
        Result = khepri_cluster:nodes(StoreIdAtom),
        case Result of
            {ok, Nodes} -> {ok, [atom_to_list(Node) || Node <- Nodes]};
            {error, Reason} -> {error, format_error(Reason)}
        end
    catch
        error:Error -> {error, format_error(Error)}
    end.

%% Wait for leader election with default settings
wait_for_leader() ->
    try
        Result = khepri_cluster:wait_for_leader(),
        case Result of
            ok -> {ok, nil};
            {error, Reason} -> {error, format_error(Reason)}
        end
    catch
        error:Error -> {error, format_error(Error)}
    end.

%% Wait for leader election with timeout
wait_for_leader_with_timeout(Timeout) ->
    try
        Result = khepri_cluster:wait_for_leader(Timeout),
        case Result of
            ok -> {ok, nil};
            {error, Reason} -> {error, format_error(Reason)}
        end
    catch
        error:Error -> {error, format_error(Error)}
    end.

%% -----------------------------------------------
%% Helper Functions
%% -----------------------------------------------

%% Format members list for Gleam
format_members({ok, Members}) ->
    % Convert each member to a string format
    FormattedMembers = lists:map(
        fun(Member) ->
            case Member of
                {StoreName, Node} when is_atom(StoreName), is_atom(Node) ->
                    atom_to_list(StoreName) ++ "@" ++ atom_to_list(Node);
                _ ->
                    io_lib:format("~p", [Member])
            end
        end,
        Members),
    {ok, FormattedMembers};
format_members({error, Reason}) ->
    {error, format_error(Reason)}.

%% Format error messages for Gleam
format_error(Error) ->
    lists:flatten(io_lib:format("~p", [Error])).
