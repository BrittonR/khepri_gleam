#!/bin/bash
# khepri_shortname_test.sh - Simplified script using short names

set -e  # Exit on error

# Color output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Create logs directory
mkdir -p logs

# Stop any running nodes
function stop_nodes {
    echo -e "${BLUE}Stopping any running nodes...${NC}"
    for node in node1 node2 node3; do
        if pgrep -f "sname $node" > /dev/null; then
            echo -e "${YELLOW}Stopping $node...${NC}"
            pkill -f "sname $node"
            sleep 1
        fi
    done
}

# Start the primary node
function start_primary {
    echo -e "${BLUE}Starting primary node...${NC}"
    erl -pa build/dev/erlang/*/ebin -pa build/dev/erlang/*/_gleam_artefacts \
        -sname node1 \
        -setcookie khepri_test \
        -noinput \
        -eval "
            io:format(\"Starting Khepri on primary node~n\"),
            case khepri:start() of
                {ok, StoreId} -> 
                    io:format(\"Khepri started with ID: ~p~n\", [StoreId]),
                    % Store test data
                    Path = [<<\"test\">>, <<\"data\">>],
                    khepri:put(Path, <<\"test_value\">>),
                    io:format(\"Stored test data~n\"),
                    % Print info
                    io:format(\"Primary node running on ~p~n\", [node()]),
                    io:format(\"Waiting for connections...~n\"),
                    timer:sleep(infinity);
                Error -> 
                    io:format(\"Failed to start Khepri: ~p~n\", [Error]),
                    init:stop(1)
            end.
        " > logs/primary.log 2>&1 &
    
    # Wait for node to start
    sleep 3
    echo -e "${GREEN}Primary node started (see logs/primary.log)${NC}"
}

# Start the secondary node
function start_secondary {
    echo -e "${BLUE}Starting secondary node...${NC}"
    erl -pa build/dev/erlang/*/ebin -pa build/dev/erlang/*/_gleam_artefacts \
        -sname node2 \
        -setcookie khepri_test \
        -noinput \
        -eval "
            io:format(\"Starting Khepri on secondary node~n\"),
            case khepri:start() of
                {ok, StoreId} -> 
                    io:format(\"Khepri started with ID: ~p~n\", [StoreId]),
                    io:format(\"Connecting to primary node~n\"),
                    % Connect to primary
                    case net_kernel:connect_node('node1@$(hostname -s)') of
                        true ->
                            io:format(\"Connected to primary node~n\"),
                            % Join Khepri cluster
                            case khepri_cluster:join('node1@$(hostname -s)') of
                                ok -> 
                                    io:format(\"Successfully joined Khepri cluster!~n\"),
                                    % Test reading data
                                    timer:sleep(1000),  % Give time for data to sync
                                    Path = [<<\"test\">>, <<\"data\">>],
                                    case khepri:get(Path) of
                                        {ok, Value} ->
                                            io:format(\"Successfully read test data: ~p~n\", [Value]);
                                        {error, Reason} ->
                                            io:format(\"Failed to read test data: ~p~n\", [Reason])
                                    end;
                                {error, JoinError} ->
                                    io:format(\"Failed to join Khepri cluster: ~p~n\", [JoinError])
                            end;
                        false ->
                            io:format(\"Failed to connect to primary node~n\")
                    end,
                    io:format(\"Secondary node running on ~p~n\", [node()]),
                    timer:sleep(infinity);
                Error -> 
                    io:format(\"Failed to start Khepri: ~p~n\", [Error]),
                    init:stop(1)
            end.
        " > logs/secondary.log 2>&1 &
    
    # Wait for node to start
    sleep 3
    echo -e "${GREEN}Secondary node started (see logs/secondary.log)${NC}"
}

# Check cluster status
function check_status {
    echo -e "${BLUE}Checking cluster status...${NC}"
    erl -pa build/dev/erlang/*/ebin -pa build/dev/erlang/*/_gleam_artefacts \
        -sname checker \
        -setcookie khepri_test \
        -noshell \
        -eval "
            io:format(\"Checking cluster status...~n\"),
            % Connect to primary
            case net_kernel:connect_node('node1@$(hostname -s)') of
                true ->
                    io:format(\"Connected to primary node~n\"),
                    % Get connected nodes
                    Nodes = rpc:call('node1@$(hostname -s)', erlang, nodes, []),
                    io:format(\"Connected nodes: ~p~n\", [Nodes]),
                    % Get Khepri cluster info
                    try
                        Members = rpc:call('node1@$(hostname -s)', khepri_cluster, members, []),
                        io:format(\"Khepri cluster members: ~p~n\", [Members]),
                        Knodes = rpc:call('node1@$(hostname -s)', khepri_cluster, nodes, []),
                        io:format(\"Khepri cluster nodes: ~p~n\", [Knodes])
                    catch
                        E:R -> io:format(\"Error getting cluster info: ~p:~p~n\", [E,R])
                    end;
                false ->
                    io:format(\"Failed to connect to primary node~n\")
            end,
            init:stop().
        "
}

# Print usage
function print_usage {
    echo -e "${BLUE}Khepri Short Name Cluster Test${NC}"
    echo "Usage:"
    echo "  $0 start       # Start primary and secondary nodes"
    echo "  $0 primary     # Start only primary node"
    echo "  $0 secondary   # Start only secondary node"
    echo "  $0 status      # Check cluster status"
    echo "  $0 stop        # Stop all nodes"
}

# Main script
case "$1" in
    "start")
        stop_nodes
        start_primary
        sleep 3  # Give primary time to initialize
        start_secondary
        sleep 2
        check_status
        ;;
    "primary")
        stop_nodes
        start_primary
        ;;
    "secondary")
        start_secondary
        ;;
    "status")
        check_status
        ;;
    "stop")
        stop_nodes
        ;;
    *)
        print_usage
        ;;
esac

echo -e "${GREEN}Done!${NC}"
