%% src/khepri_gleam_helper.erl
-module(khepri_gleam_helper).
-export([condition_to_erlang/1, 
         to_pattern_path/1,
         get_pattern/1,
         delete_pattern/1,
         exists_pattern/1,
         list_children/1]).

%% Convert Gleam condition to Erlang term
condition_to_erlang({name_is, Name}) ->
    Name;
condition_to_erlang(any) ->
    '_'; % Use underscore for wildcard
condition_to_erlang({data_matches, Pattern}) ->
    Pattern;
condition_to_erlang({all, Conditions}) ->
    {'$and', lists:map(fun condition_to_erlang/1, Conditions)};
condition_to_erlang({any_of, Conditions}) ->
    {'$or', lists:map(fun condition_to_erlang/1, Conditions)};
condition_to_erlang({child_count, Count, Op}) ->
    Count;
condition_to_erlang(Unknown) ->
    io:format("Unknown condition: ~p~n", [Unknown]),
    '_'.

%% Convert path with conditions to Erlang format
to_pattern_path(PathWithConditions) ->
    io:format("Converting path: ~p~n", [PathWithConditions]),
    Path = lists:map(
        fun({Name, Condition}) ->
            case Condition of
                {name_is, _} -> Name;
                _ -> condition_to_erlang(Condition)
            end
        end,
        PathWithConditions
    ),
    io:format("Converted to: ~p~n", [Path]),
    Path.

%% Get with pattern
get_pattern(Path) ->
    try
        io:format("Trying to get with path: ~p~n", [Path]),
        % Check if path contains wildcards
        HasWildcard = lists:any(fun(Part) -> Part =:= '_' end, Path),
        
        Result = case HasWildcard of
            true ->
                % For wildcard patterns, try a different approach
                % First get the parent path (without the wildcard)
                ParentPath = lists:droplast(Path),
                io:format("Using parent path: ~p~n", [ParentPath]),
                
                % Check if parent exists
                case khepri:get(ParentPath) of
                    {ok, _} ->
                        % Try using list_children
                        io:format("Parent found, getting children~n"),
                        % Return the parent itself for now
                        khepri:get(ParentPath);
                    Error ->
                        io:format("Parent not found: ~p~n", [Error]),
                        Error
                end;
            false ->
                % Use regular get for direct paths
                io:format("Using get for direct path~n"),
                khepri:get(Path)
        end,
        
        io:format("Get result: ~p~n", [Result]),
        Result
    catch
        error:Reason -> 
            io:format("Get pattern error: ~p~n", [Reason]),
            {error, unknown_error}
    end.

%% Delete with pattern
delete_pattern(Path) ->
    try
        io:format("Trying to delete with path: ~p~n", [Path]),
        khepri:delete(Path)
    catch
        error:Reason -> 
            io:format("Delete pattern error: ~p~n", [Reason]),
            ok
    end.

%% Exists with pattern
exists_pattern(Path) ->
    try
        khepri:exists(Path)
    catch
        error:Reason -> 
            io:format("Exists pattern error: ~p~n", [Reason]),
            false
    end.


%% src/khepri_gleam_helper.erl - updated list_children function

%% List children of a path
list_children(Path) ->
    try
        io:format("Listing children for path: ~p~n", [Path]),
        % Use a wildcard pattern to match direct children
        ChildPath = Path ++ ['_'],
        io:format("Using child path pattern: ~p~n", [ChildPath]),
        
        % Use get_many to retrieve all matching children
        case khepri:get_many(ChildPath) of
            {ok, ChildrenMap} ->
                % Convert map entries to list of tuples for Gleam
                ChildList = maps:fold(
                    fun(ChildFullPath, ChildProps, Acc) ->
                        % Extract child name (last path component)
                        ChildName = lists:last(ChildFullPath),
                        % Extract data from child properties
                        Data = maps:get(data, ChildProps, undefined),
                        [{ChildName, Data} | Acc]
                    end,
                    [],
                    ChildrenMap
                ),
                io:format("Found children: ~p~n", [ChildList]),
                {ok, ChildList};
            Error ->
                io:format("Error getting children: ~p~n", [Error]),
                {ok, []} % Return empty list if path doesn't exist or on error
        end
    catch
        error:Reason ->
            io:format("List children error: ~p~n", [Reason]),
            {error, Reason}
    end.
