%% src/simple_cluster_test_helper.erl
-module(simple_cluster_test_helper).
-export([ping_node/1, join_cluster/1]).

%% Ping a remote node
ping_node(Node) when is_list(Node) ->
    try
        % Convert string to atom
        NodeAtom = list_to_atom(Node),
        % Use net_adm:ping for reliable ping
        case net_adm:ping(NodeAtom) of
            pong -> true;
            pang -> false
        end
    catch
        _:_ -> false
    end.

%% Join a Khepri cluster
join_cluster(Node) when is_list(Node) ->
    try
        % Convert string to atom
        NodeAtom = list_to_atom(Node),
        % Try to join the cluster
        Result = khepri_cluster:join(NodeAtom),
        case Result of
            ok -> {ok, ok};
            {error, Reason} -> {error, Reason}
        end
    catch
        error:Error -> {error, Error}
    end.
