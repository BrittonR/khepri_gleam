%% src/khepri_gleam_helper.erl
-module(khepri_gleam_helper).
-include_lib("khepri/include/khepri.hrl").
-export([condition_to_erlang/1, 
         to_pattern_path/1,
         get_pattern/1,
         delete_pattern/1,
         exists_pattern/1,
         list_children/1,
         get_children_direct/1,
         do_transaction_put/2, 
         do_transaction_get/1, 
         do_transaction_delete/1, 
         do_transaction_exists/1,
         export_data/3,
         import_data/2,
         put_with_path/2,
         start/0,
         get_registered_paths/0,
         clear_all/0,
         safe_list_directory/1,
         get_many/1,
         list_directory_direct/1]).

start() ->
    io:format("Starting Khepri with default configuration...~n"),
    Result = khepri:start(),
    io:format("Khepri start result: ~p~n", [Result]),
    ok.

%% Get all paths from Khepri directly instead of using ETS registry
get_registered_paths() ->
    io:format("Getting all paths from Khepri...~n"),
    case khepri:get_many([?KHEPRI_WILDCARD_STAR_STAR]) of
        {ok, TreeMap} ->
            % Extract paths from the tree map
            Paths = maps:keys(TreeMap),
            io:format("Found ~p paths in Khepri~n", [length(Paths)]),
            Paths;
        {error, _Reason} ->
            io:format("Failed to get paths from Khepri~n"),
            []
    end.

%% Enhanced put function without separate path registry
put_with_path(Path, Data) ->
    io:format("Putting data at path: ~p~n", [Path]),
    io:format("Data: ~p~n", [Data]),
    
    % Convert path strings to binaries if they aren't already
    BinaryPath = lists:map(
        fun(Part) when is_binary(Part) -> Part;
           (Part) when is_list(Part) -> list_to_binary(Part);
           (Part) -> atom_to_binary(Part, utf8)
        end,
        Path
    ),
    
    io:format("Converted binary path: ~p~n", [BinaryPath]),
    
    % Do the actual put
    Result = khepri:put(BinaryPath, Data),
    io:format("Put result: ~p~n", [Result]),
    
    Result.

%% Get multiple values matching a pattern
get_many(Pattern) ->
    io:format("Getting many with pattern: ~p~n", [Pattern]),
    
    % Convert string pattern elements to binaries
    BinaryPattern = lists:map(
        fun("_") -> ?KHEPRI_WILDCARD_STAR;  % Single level wildcard
           ("**") -> ?KHEPRI_WILDCARD_STAR_STAR;  % Multi-level wildcard
           (Part) when is_list(Part) -> list_to_binary(Part);
           (Part) when is_binary(Part) -> Part;  % Already binary
           (Part) -> Part
        end,
        Pattern
    ),
    
    case khepri:get_many(BinaryPattern) of
        {ok, Results} ->
            % Convert the map to a list of {Path, Value} tuples
            ResultList = maps:to_list(Results),
            % Convert paths back to strings
            ConvertedResults = lists:map(fun({Path, Value}) ->
                StringPath = lists:map(fun(P) when is_binary(P) -> binary_to_list(P);
                                         (P) when is_atom(P) -> atom_to_list(P);
                                         (P) -> P
                                      end, Path),
                {StringPath, Value}
            end, ResultList),
            {ok, ConvertedResults};
        {error, Reason} ->
            {error, io_lib:format("~p", [Reason])}
    end.

%% List directory contents directly using Khepri's native functions
list_directory_direct(Path) ->
    io:format("Listing directory: ~p~n", [Path]),
    
    % Convert path to binary list
    BinaryPath = case Path of
        "/:services/" -> [<<"services">>];
        _ -> string_to_khepri_path(Path)
    end,
    
    % Build a pattern to match all children
    Pattern = BinaryPath ++ [?KHEPRI_WILDCARD_STAR],
    
    io:format("Using pattern: ~p~n", [Pattern]),
    
    case khepri:get_many(Pattern) of
        {ok, Results} ->
            io:format("Raw results map: ~p~n", [Results]),
            
            % Convert map to list and extract children
            Children = lists:filtermap(fun({FullPath, Value}) ->
                io:format("Processing path: ~p~n", [FullPath]),
                
                % Check if this is a direct child
                case FullPath of
                    % Match paths that are exactly one level deeper
                    _ when length(FullPath) == length(BinaryPath) + 1 ->
                        % Get the child name (last element)
                        ChildName = lists:last(FullPath),
                        
                        % Convert binary to string if needed
                        ChildNameStr = case ChildName of
                            Bin when is_binary(Bin) -> binary_to_list(Bin);
                            Atom when is_atom(Atom) -> atom_to_list(Atom);
                            Other -> io_lib:format("~p", [Other])
                        end,
                        
                        io:format("Found child: ~s with value: ~p~n", [ChildNameStr, Value]),
                        {true, {ChildNameStr, Value}};
                    _ ->
                        false
                end
            end, maps:to_list(Results)),
            
            io:format("Found ~p children~n", [length(Children)]),
            {ok, Children};
        {error, Reason} ->
            io:format("Error listing directory: ~p~n", [Reason]),
            {error, io_lib:format("Failed to list directory: ~p", [Reason])}
    end.

%% Helper to convert string path to Khepri path format
string_to_khepri_path(Path) when is_binary(Path) ->
    % Convert binary to list first
    string_to_khepri_path(binary_to_list(Path));
string_to_khepri_path(Path) when is_list(Path) ->
    % Remove leading slashes and colons
    CleanPath = case Path of
        "/:services/" -> "services";
        "/:services" -> "services";
        "/:" ++ Rest -> Rest;
        "/" ++ Rest -> Rest;
        Other -> Other
    end,
    
    % Split by "/" and convert to binaries
    Parts = string:split(CleanPath, "/", all),
    lists:map(fun(Part) -> 
        case Part of
            "" -> skip;
            P -> list_to_binary(P)
        end
    end, Parts) -- [skip].

%% Convert Gleam condition to Erlang term
condition_to_erlang({name_is, Name}) ->
    Name;
condition_to_erlang(any) ->
    '_'; % Use underscore for wildcard
condition_to_erlang({data_matches, Pattern}) ->
    %% Create proper IfDataMatches condition
    #{'__struct__' => 'Elixir.Khepri.Condition.IfDataMatches',
      pattern => Pattern,
      conditions => []};
condition_to_erlang({all, Conditions}) ->
    %% Create proper IfAll condition 
    #{'__struct__' => 'Elixir.Khepri.Condition.IfAll',
      conditions => lists:map(fun condition_to_erlang/1, Conditions)};
condition_to_erlang({any_of, Conditions}) ->
    %% Create proper IfAnyOf condition
    #{'__struct__' => 'Elixir.Khepri.Condition.IfAnyOf',
      conditions => lists:map(fun condition_to_erlang/1, Conditions)};
condition_to_erlang({child_count, Count, Op}) ->
    %% Create proper IfChildListLength condition
    #{'__struct__' => 'Elixir.Khepri.Condition.IfChildListLength',
      count => Count,
      operator => convert_compare_op(Op)};
condition_to_erlang({node_exists, Exists}) ->
    %% Create proper IfNodeExists condition
    #{'__struct__' => 'Elixir.Khepri.Condition.IfNodeExists',
      exists => Exists};
condition_to_erlang({'not', Condition}) ->
    %% Create proper IfNot condition 
    #{'__struct__' => 'Elixir.Khepri.Condition.IfNot',
      condition => condition_to_erlang(Condition)};
condition_to_erlang({data_matches_with_conditions, Pattern, Conditions}) ->
    %% Create proper IfDataMatches with conditions
    #{'__struct__' => 'Elixir.Khepri.Condition.IfDataMatches',
      pattern => Pattern,
      conditions => Conditions};
condition_to_erlang({payload_version, Version, Op}) ->
    %% Create proper IfPayloadVersion condition
    #{'__struct__' => 'Elixir.Khepri.Condition.IfPayloadVersion',
      version => Version,
      operator => Op};
condition_to_erlang({child_list_version, Version, Op}) ->
    %% Create proper IfChildListVersion condition
    #{'__struct__' => 'Elixir.Khepri.Condition.IfChildListVersion',
      version => Version,
      operator => Op};
condition_to_erlang({child_list_length, Count, Op}) ->
    %% Create proper IfChildListLength condition
    #{'__struct__' => 'Elixir.Khepri.Condition.IfChildListLength',
      count => Count,
      operator => Op};
condition_to_erlang(Unknown) ->
    io:format("Unknown condition: ~p~n", [Unknown]),
    '_'.

%% Convert path with conditions to Erlang format
to_pattern_path(PathWithConditions) ->
    io:format("Converting path: ~p~n", [PathWithConditions]),
    % Check if the path is already converted (list of binaries)
    % or needs conversion (list of tuples)
    Path = case PathWithConditions of
        [] -> 
            [];
        [{_, _}|_] -> 
            % This is a tuple format path that needs conversion
            lists:map(
                fun({Name, Condition}) ->
                    case Condition of
                        {name_is, _} -> Name;
                        _ -> condition_to_erlang(Condition)
                    end
                end,
                PathWithConditions
            );
        _ -> 
            % Already in the right format
            PathWithConditions
    end,
    
    io:format("Converted to: ~p~n", [Path]),
    Path.

%% Get with pattern
get_pattern(Path) ->
    try
        io:format("Trying to get with path: ~p~n", [Path]),
        
        % Check if path needs conversion
        ConvertedPath = case Path of
            [] -> 
                [];
            [{_, _}|_] -> 
                % This is a pattern path that needs conversion
                to_pattern_path(Path);
            _ -> 
                % Already in the right format
                Path
        end,
        
        khepri:get(ConvertedPath)
    catch
        error:Reason -> 
            io:format("Get pattern error: ~p~n", [Reason]),
            {error, unknown_error}
    end.

%% Delete with pattern
delete_pattern(Path) ->
    try
        io:format("Trying to delete with path: ~p~n", [Path]),
        
        % Check if path needs conversion
        ConvertedPath = case Path of
            [] -> 
                [];
            [{_, _}|_] -> 
                % This is a pattern path that needs conversion
                to_pattern_path(Path);
            _ -> 
                % Already in the right format
                Path
        end,
        
        khepri:delete(ConvertedPath)
    catch
        error:Reason -> 
            io:format("Delete pattern error: ~p~n", [Reason]),
            ok
    end.

%% Exists with pattern
exists_pattern(Path) ->
    try
        io:format("Trying exists pattern with path: ~p~n", [Path]),
        
        % Check if path needs conversion
        ConvertedPath = case Path of
            [] -> 
                [];
            [{_, _}|_] -> 
                % This is a pattern path that needs conversion
                to_pattern_path(Path);
            _ -> 
                % Already in the right format
                io:format("Path is already in raw format~n"),
                Path
        end,
        
        io:format("Converted path: ~p~n", [ConvertedPath]),
        
        khepri:exists(ConvertedPath)
    catch
        error:Reason -> 
            io:format("Exists pattern error: ~p~n", [Reason]),
            false
    end.

%% List children of a path
list_children(Path) ->
    try
        io:format("Listing children for path: ~p~n", [Path]),
        
        % Use get_children_direct since it's working correctly
        get_children_direct(Path)
    catch
        error:Reason ->
            io:format("List children error: ~p~n", [Reason]),
            {error, Reason}
    end.

get_children_direct(Path) ->
    io:format("Getting children for path: ~p~n", [Path]),
    
    % Build a pattern to match all direct children
    Pattern = Path ++ [?KHEPRI_WILDCARD_STAR],
    
    case khepri:get_many(Pattern) of
        {ok, Results} ->
            % Extract direct children
            Children = lists:filtermap(
                fun({FullPath, Value}) ->
                    case FullPath of
                        % Check if this is a direct child
                        _ when length(FullPath) == length(Path) + 1 ->
                            ChildName = lists:last(FullPath),
                            {true, {ChildName, Value}};
                        _ ->
                            false
                    end
                end,
                maps:to_list(Results)
            ),
            {ok, Children};
        {error, Reason} ->
            io:format("Error getting children: ~p~n", [Reason]),
            {error, Reason}
    end.

do_transaction_put(Path, Data) ->
    try
        Result = khepri:transaction(
            fun() ->
                khepri_tx:put(Path, Data),
                ok
            end),
        {ok, Result}
    catch
        error:Reason -> 
            io:format("Transaction put failed: ~p~n", [Reason]),
            {error, Reason}
    end.

do_transaction_get(Path) ->
    try
        Result = khepri:transaction(
            fun() ->
                khepri_tx:get(Path)
            end),
        Result
    catch
        error:Reason -> 
            io:format("Transaction get failed: ~p~n", [Reason]),
            {error, Reason}
    end.

do_transaction_delete(Path) ->
    try
        Result = khepri:transaction(
            fun() ->
                khepri_tx:delete(Path),
                ok
            end),
        {ok, Result}
    catch
        error:Reason -> 
            io:format("Transaction delete failed: ~p~n", [Reason]),
            {error, Reason}
    end.

do_transaction_exists(Path) ->
    try
        Result = khepri:transaction(
            fun() ->
                khepri_tx:exists(Path)
            end),
        
        % Ensure we return a proper boolean value that Gleam can decode
        BoolValue = case Result of
            true -> true;
            false -> false;
            {ok, true} -> true;
            {ok, false} -> false;
            {ok, Bool} when is_boolean(Bool) -> Bool;
            _ -> 
                io:format("Unexpected result type: ~p~n", [Result]),
                false  % Default to false for unexpected types
        end,
        
        {ok, BoolValue}
    catch
        error:Reason -> 
            io:format("Transaction exists failed: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Export data from Khepri to a file - using native Khepri functions
export_data(Path, CallbackModule, Filename) ->
    try
        io:format("Exporting data from path: ~p using ~p to file: ~p~n", [Path, CallbackModule, Filename]),
        
        % Get all data under the path
        Pattern = Path ++ [?KHEPRI_WILDCARD_STAR_STAR],
        case khepri:get_many(Pattern) of
            {ok, DataMap} ->
                io:format("Data to export: ~p~n", [DataMap]),
                % Write the data to a file
                file:write_file(Filename, io_lib:format("~p.~n", [DataMap])),
                {ok, ok};
            {error, Reason} ->
                {error, io_lib:format("Export failed: ~p", [Reason])}
        end
    catch
        error:Reason -> 
            io:format("Export error: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Import data from a file into Khepri
import_data(CallbackModule, Filename) ->
    try
        io:format("Importing data from file: ~p~n", [Filename]),
        
        % Read the data from the file
        {ok, Binary} = file:read_file(Filename),
        String = binary_to_list(Binary),
        {ok, Tokens, _} = erl_scan:string(String),
        {ok, Term} = erl_parse:parse_term(Tokens),
        
        io:format("IMPORT - Data read from file: ~p~n", [Term]),
        
        % Import the data
        maps:fold(
            fun(Path, Value, _Acc) ->
                io:format("Importing path: ~p~n", [Path]),
                khepri:put(Path, Value),
                ok
            end,
            ok,
            Term
        ),
        
        {ok, ok}
    catch
        error:Reason -> 
            io:format("Import error: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Clear everything
clear_all() ->
    % Delete the root node in Khepri
    khepri:delete([]),
    
    {ok, ok}.

%% Safe directory listing that won't crash if items disappear
safe_list_directory(Path) ->
    try
        case Path of
            "/:cluster_events/" ->
                % Get all cluster events
                Pattern = [<<"cluster_events">>, ?KHEPRI_WILDCARD_STAR],
                case khepri:get_many(Pattern) of
                    {ok, Results} ->
                        Events = lists:map(
                            fun({FullPath, Value}) ->
                                EventName = lists:last(FullPath),
                                {EventName, Value}
                            end,
                            maps:to_list(Results)
                        ),
                        {ok, Events};
                    {error, _} ->
                        {ok, []}
                end;
            _ ->
                % Regular path handling
                {ok, []}
        end
    catch
        _:Reason -> 
            io:format("Safe listing error: ~p~n", [Reason]),
            {ok, []}  % Return empty list instead of error
    end.

%% Helper function for operator conversion
convert_compare_op(greater_than) -> 'gt';
convert_compare_op(less_than) -> 'lt';
convert_compare_op(equal) -> 'eq';
convert_compare_op(greater_than_or_equal) -> 'ge';
convert_compare_op(less_than_or_equal) -> 'le';
convert_compare_op(_) -> 'eq'.  % Default to equals
