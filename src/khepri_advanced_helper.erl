%% src/khepri_advanced_helper.erl
-module(khepri_advanced_helper).
-compile(export_all).
-export([
    advanced_condition_to_erlang/1,
    to_node_condition/1,
    apply_advanced_pattern/3,
    convert_match_spec/2
]).

%% Convert AdvancedCondition to Erlang term format used by Khepri
advanced_condition_to_erlang({name_matches, Pattern}) ->
    %% Create an if_name_matches condition with regex pattern
    io:format("Creating if_name_matches with pattern: ~p~n", [Pattern]),
    #{'__struct__' => 'Elixir.Khepri.Condition.IfNameMatches', regex => Pattern};

advanced_condition_to_erlang({node_exists, Exists}) ->
    %% Create an if_node_exists condition
    io:format("Creating if_node_exists with exists=~p~n", [Exists]),
    #{'__struct__' => 'Elixir.Khepri.Condition.IfNodeExists', exists => Exists};

advanced_condition_to_erlang({payload_version, Version, Op}) ->
    %% Create an if_payload_version condition
    io:format("Creating if_payload_version with version=~p, op=~p~n", [Version, Op]),
    #{'__struct__' => 'Elixir.Khepri.Condition.IfPayloadVersion', 
      version => Version, 
      operator => convert_compare_op(Op)};

advanced_condition_to_erlang({child_list_version, Version, Op}) ->
    %% Create an if_child_list_version condition
    io:format("Creating if_child_list_version with version=~p, op=~p~n", [Version, Op]),
    #{'__struct__' => 'Elixir.Khepri.Condition.IfChildListVersion', 
      version => Version, 
      operator => convert_compare_op(Op)};

advanced_condition_to_erlang({data_spec, Pattern, Conditions}) ->
    %% Create an if_data_matches condition with match spec
    io:format("Creating if_data_matches with pattern: ~p~n", [Pattern]),
    #{'__struct__' => 'Elixir.Khepri.Condition.IfDataMatches', 
      pattern => Pattern, 
      conditions => Conditions};

advanced_condition_to_erlang({'not', Condition}) ->
    %% Create an if_not condition - 'not' is quoted because it's a reserved word
    io:format("Creating if_not condition~n"),
    InnerCondition = advanced_condition_to_erlang(Condition),
    #{'__struct__' => 'Elixir.Khepri.Condition.IfNot', 
      condition => InnerCondition};

advanced_condition_to_erlang({keep_while, Path, Condition}) ->
    %% Create a keep_while condition
    io:format("Creating keep_while with path: ~p~n", [Path]),
    %% Convert path string to Khepri path
    {ok, KhepriPath} = khepri_path:from_string(Path),
    InnerCondition = advanced_condition_to_erlang(Condition),
    #{KhepriPath => InnerCondition};

advanced_condition_to_erlang({parent_is, Path}) ->
    %% Create a condition to check parent path
    io:format("Creating parent check with path: ~p~n", [Path]),
    %% This would need a more complex implementation based on
    %% how Khepri handles parent path checks
    #{'__struct__' => 'Elixir.Khepri.Condition.IfParentPathIs', 
      path => Path};

advanced_condition_to_erlang(Unknown) ->
    io:format("Unknown condition type: ~p~n", [Unknown]),
    %% Default to wildcard/any match
    '_'.

%% Convert a Gleam CompareOp to an Erlang comparison operator
convert_compare_op(greater_than) -> 'gt';
convert_compare_op(less_than) -> 'lt';
convert_compare_op(equal) -> 'eq';
convert_compare_op(greater_than_or_equal) -> 'ge';
convert_compare_op(less_than_or_equal) -> 'le';
convert_compare_op(_) -> 'eq'.  % Default to equals

%% Convert AdvancedCondition to a NodeCondition (for integration with existing code)
to_node_condition(AdvancedCondition) ->
    %% Here we convert the advanced condition to match the NodeCondition format
    %% expected by the existing Gleam code
    
    %% First convert to Erlang format
    ErlangCondition = advanced_condition_to_erlang(AdvancedCondition),
    
    %% Then wrap in a DataMatches to be compatible with existing NodeCondition type
    {data_matches, ErlangCondition}.

%% Apply an advanced pattern as part of a query or operation
apply_advanced_pattern(Operation, Path, AdvancedCondition) ->
    %% Convert the advanced condition to Erlang format
    ErlangCondition = advanced_condition_to_erlang(AdvancedCondition),
    
    %% Apply the path pattern with the condition
    case Operation of
        get ->
            khepri:get(Path ++ [ErlangCondition]);
        delete ->
            khepri:delete(Path ++ [ErlangCondition]);
        exists ->
            khepri:exists(Path ++ [ErlangCondition]);
        _ ->
            {error, invalid_operation}
    end.

%% Convert a Gleam match spec to Erlang format
convert_match_spec(Pattern, Guards) ->
    %% This is a simplified implementation
    %% In a real implementation, this would need to handle the translation
    %% of Gleam match spec syntax to Erlang match spec syntax
    {Pattern, Guards}.
