%% src/khepri_gleam_cluster_helper.erl
-module(khepri_gleam_cluster_helper).
-export([
    get_store_id/0,
    is_store_running/1,
    format_members/1,
    format_error/1,
    join_with_timeout/2
]).

%% Get the default store ID (name)
get_store_id() ->
    StoreId = khepri_cluster:get_default_store_id(),
    atom_to_list(StoreId).

%% Check if a store is running
is_store_running(StoreId) when is_list(StoreId) ->
    AtomStoreId = list_to_atom(StoreId),
    khepri_cluster:is_store_running(AtomStoreId);
is_store_running(StoreId) when is_atom(StoreId) ->
    khepri_cluster:is_store_running(StoreId).

%% Format members list for Gleam
format_members({ok, Members}) ->
    FormattedMembers = lists:map(
        fun(Member) ->
            case Member of
                {Name, Node} when is_atom(Name), is_atom(Node) ->
                    atom_to_list(Name) ++ "@" ++ atom_to_list(Node);
                {Name, Node} ->
                    io_lib:format("~p@~p", [Name, Node]);
                Other ->
                    io_lib:format("~p", [Other])
            end
        end,
        Members),
    {ok, FormattedMembers};
format_members(Error) ->
    Error.

%% Format error for Gleam
format_error(Error) ->
    io_lib:format("~p", [Error]).

%% Join with timeout
join_with_timeout(Node, Timeout) when is_list(Node), is_integer(Timeout) ->
    try
        % Convert string node name to atom
        AtomNode = list_to_atom(Node),
        % Join the cluster
        Result = khepri_cluster:join(AtomNode, Timeout),
        case Result of
            ok -> {ok, ok};
            {error, Reason} -> {error, format_error(Reason)}
        end
    catch
        error:Error ->
            {error, format_error(Error)}
    end.
