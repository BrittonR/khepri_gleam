%% src/khepri_pattern_erlang.erl
-module(khepri_pattern_erlang).
-include_lib("khepri/include/khepri.hrl").
-export([
    if_node_exists/1,
    if_data_matches_pattern/2,
    if_payload_version/2,
    if_child_list_version/2,
    if_child_list_length/2,
    if_not/1,
    wildcard_star/0,
    wildcard_star_star/0,
    has_data/1,
    get_many/1,
    delete_many/1,
    count/1,
    compare_and_swap/3,
    path_with_star/2,
    path_with_star_star/2,
    find_with_attribute/3
]).

%% Node existence condition
if_node_exists(Exists) ->
    case Exists of
        true -> #{
            '__struct__' => 'Elixir.Khepri.Condition.IfNodeExists',
            exists => true
        };
        false -> #{
            '__struct__' => 'Elixir.Khepri.Condition.IfNodeExists',
            exists => false
        }
    end.

%% Data pattern matching with match specifications
if_data_matches_pattern(Pattern, Conditions) ->
    #{
        '__struct__' => 'Elixir.Khepri.Condition.IfDataMatches',
        pattern => Pattern,
        conditions => Conditions
    }.

%% Payload version condition
if_payload_version(Version, Op) ->
    #{
        '__struct__' => 'Elixir.Khepri.Condition.IfPayloadVersion',
        version => Version,
        operator => convert_op(Op)
    }.

%% Child list version condition
if_child_list_version(Version, Op) ->
    #{
        '__struct__' => 'Elixir.Khepri.Condition.IfChildListVersion',
        version => Version,
        operator => convert_op(Op)
    }.

%% Child list length condition
if_child_list_length(Count, Op) ->
    #{
        '__struct__' => 'Elixir.Khepri.Condition.IfChildListLength',
        count => Count,
        operator => convert_op(Op)
    }.

%% Negation condition
if_not(Condition) ->
    #{
        '__struct__' => 'Elixir.Khepri.Condition.IfNot',
        condition => Condition
    }.

%% Wildcard that matches any single node
wildcard_star() ->
    ?KHEPRI_WILDCARD_STAR.

%% Wildcard that matches any number of nodes recursively
wildcard_star_star() ->
    ?KHEPRI_WILDCARD_STAR_STAR.

%% Check if a node has data
has_data(Path) ->
    try
        khepri:has_data(Path)
    catch
        _:_ -> false
    end.

%% Get multiple nodes matching a pattern
get_many(Path) ->
    try
        case khepri_adv:get_many(Path) of
            {ok, Nodes} -> {ok, Nodes};
            {error, Reason} -> {error, io_lib:format("~p", [Reason])}
        end
    catch
        error:Error ->
            {error, io_lib:format("Error in get_many: ~p", [Error])}
    end.

%% Delete multiple nodes matching a pattern
delete_many(Path) ->
    try
        khepri_adv:delete_many(Path),
        ok
    catch
        error:Error ->
            io:format("Error in delete_many: ~p~n", [Error]),
            ok
    end.

%% Count nodes matching a pattern
count(Path) ->
    try
        khepri:count(Path)
    catch
        _:_ -> 0
    end.

%% Compare and swap operation
compare_and_swap(Path, DataPattern, NewData) ->
    try
        case khepri_adv:compare_and_swap(Path, DataPattern, NewData) of
            {ok, Result} -> {ok, Result};
            {error, Reason} -> {error, io_lib:format("~p", [Reason])}
        end
    catch
        error:Error ->
            {error, io_lib:format("Error in compare_and_swap: ~p", [Error])}
    end.

%% Create a path with a wildcard_star at the specified position
path_with_star(Path, Position) ->
    insert_at_position(Path, ?KHEPRI_WILDCARD_STAR, Position).

%% Create a path with a wildcard_star_star at the specified position
path_with_star_star(Path, Position) ->
    insert_at_position(Path, ?KHEPRI_WILDCARD_STAR_STAR, Position).

%% Find all nodes with a specific attribute value
find_with_attribute(Path, AttrName, AttrValue) ->
    %% Create a path with a wildcard at the end and a data condition
    FullPath = Path ++ [?KHEPRI_WILDCARD_STAR],
    try
        case khepri_adv:get_many(FullPath, #{favor => {AttrName, AttrValue}}) of
            {ok, Nodes} -> {ok, Nodes};
            {error, Reason} -> {error, io_lib:format("~p", [Reason])}
        end
    catch
        error:Error ->
            {error, io_lib:format("Error in find_with_attribute: ~p", [Error])}
    end.

%% ----- HELPER FUNCTIONS -----

%% Insert an element at a specific position in a list
insert_at_position(List, Element, Position) when Position =< length(List) ->
    {Left, Right} = lists:split(Position, List),
    Left ++ [Element] ++ Right;
insert_at_position(List, Element, _) ->
    List ++ [Element].

%% Convert Gleam CompareOp to Erlang operator
convert_op(greater_than) -> 'gt';
convert_op(less_than) -> 'lt';
convert_op(equal) -> 'eq';
convert_op(greater_than_or_equal) -> 'ge';
convert_op(less_than_or_equal) -> 'le';
convert_op(_) -> 'eq'.  % Default to equals
