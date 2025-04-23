#!/bin/bash
# khepri_cluster_test_improved.sh - Robust Khepri clustering test script

set -e  # Exit on error

# Color output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Create logs directory
mkdir -p logs

# More thorough node stopping function
function stop_nodes {
    echo -e "${BLUE}Stopping any running nodes...${NC}"
    
    # First check for nodes with -sname
    for node in node1 node2 node3 checker; do
        if pgrep -f "sname $node" > /dev/null; then
            echo -e "${YELLOW}Stopping $node (short name)...${NC}"
            pkill -f "sname $node"
            sleep 1
        fi
    done
    
    # Also check for nodes with the hostname suffix
    HOST=$(hostname -s)
    for node in node1@$HOST node2@$HOST node3@$HOST; do
        if pgrep -f "$node" > /dev/null; then
            echo -e "${YELLOW}Stopping $node (with hostname)...${NC}"
            pkill -f "$node"
            sleep 1
        fi
    done
    
    # To be extra thorough, check for any beam processes that might be our nodes
    for pid in $(pgrep beam); do
        if ps -p $pid -o args | grep -q "node[1-3]"; then
            echo -e "${YELLOW}Stopping beam process $pid...${NC}"
            kill $pid
        fi
    done
    
    # Give processes time to fully terminate
    sleep 2
}

# Check if a node is already running
function check_node_running {
    local NODE_NAME=$1
    if pgrep -f "sname $NODE_NAME" > /dev/null || pgrep -f "$NODE_NAME@" > /dev/null; then
        return 0  # Node is running
    else
        return 1  # Node is not running
    fi
}

# Start the primary node
function start_primary {
    echo -e "${BLUE}Starting primary node...${NC}"
    
    # Check if already running
    if check_node_running "node1"; then
        echo -e "${YELLOW}Primary node already running, stopping it first...${NC}"
        pkill -f "sname node1" || pkill -f "node1@" || true
        sleep 2
    fi
    
    # Clear the log file
    echo "===== Primary Node Log - $(date) =====" > logs/primary.log
    
    # Start the node
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
                    io:format(\"Stored test data at /test/data~n\"),
                    % Print info
                    io:format(\"Primary node running on ~p~n\", [node()]),
                    io:format(\"Waiting for connections...~n\"),
                    timer:sleep(infinity);
                Error -> 
                    io:format(\"Failed to start Khepri: ~p~n\", [Error]),
                    timer:sleep(5000),
                    init:stop(1)
            end.
        " >> logs/primary.log 2>&1 &
    
    # Wait for node to start
    sleep 3
    
    # Check if node started successfully
    if check_node_running "node1"; then
        echo -e "${GREEN}Primary node started successfully${NC}"
        # Show last few log lines
        echo -e "${BLUE}Last few log lines:${NC}"
        tail -n 5 logs/primary.log
    else
        echo -e "${RED}Failed to start primary node${NC}"
        echo -e "${RED}Log file:${NC}"
        cat logs/primary.log
        exit 1
    fi
}

# Start the secondary node - SIMPLIFIED to avoid function definitions
function start_secondary {
    echo -e "${BLUE}Starting secondary node...${NC}"
    
    # Check if primary is running
    if ! check_node_running "node1"; then
        echo -e "${RED}Primary node is not running! Start it first.${NC}"
        exit 1
    fi
    
    # Check if already running
    if check_node_running "node2"; then
        echo -e "${YELLOW}Secondary node already running, stopping it first...${NC}"
        pkill -f "sname node2" || pkill -f "node2@" || true
        sleep 2
    fi
    
    # Clear the log file
    echo "===== Secondary Node Log - $(date) =====" > logs/secondary.log
    
    # Use hostname -s to get short hostname
    HOST=$(hostname -s)
    
    # Start the node with simplified logic - no function definitions
    erl -pa build/dev/erlang/*/ebin -pa build/dev/erlang/*/_gleam_artefacts \
        -sname node2 \
        -setcookie khepri_test \
        -noinput \
        -eval "
            io:format(\"Starting Khepri on secondary node~n\"),
            case khepri:start() of
                {ok, StoreId} -> 
                    io:format(\"Khepri started with ID: ~p~n\", [StoreId]),
                    io:format(\"Secondary node running on ~p~n\", [node()]),
                    io:format(\"Connecting to primary node~n\"),
                    % Try to connect
                    PrimaryNode = 'node1@$HOST',
                    io:format(\"Trying to connect to ~p~n\", [PrimaryNode]),
                    
                    % Attempt 1
                    case net_kernel:connect_node(PrimaryNode) of
                        true ->
                            io:format(\"Connected to primary node~n\"),
                            io:format(\"Joining Khepri cluster...~n\"),
                            case khepri_cluster:join(PrimaryNode) of
                                ok -> 
                                    io:format(\"Successfully joined Khepri cluster!~n\"),
                                    timer:sleep(infinity);
                                {error, JoinError} ->
                                    io:format(\"Failed to join Khepri cluster: ~p~n\", [JoinError]),
                                    timer:sleep(infinity)
                            end;
                        false ->
                            io:format(\"Attempt 1: Failed to connect, retrying in 2 seconds...~n\"),
                            timer:sleep(2000),
                            
                            % Attempt 2
                            case net_kernel:connect_node(PrimaryNode) of
                                true ->
                                    io:format(\"Connected to primary node~n\"),
                                    io:format(\"Joining Khepri cluster...~n\"),
                                    case khepri_cluster:join(PrimaryNode) of
                                        ok -> 
                                            io:format(\"Successfully joined Khepri cluster!~n\"),
                                            timer:sleep(infinity);
                                        {error, JoinError} ->
                                            io:format(\"Failed to join Khepri cluster: ~p~n\", [JoinError]),
                                            timer:sleep(infinity)
                                    end;
                                false ->
                                    io:format(\"Attempt 2: Failed to connect, retrying in 2 seconds...~n\"),
                                    timer:sleep(2000),
                                    
                                    % Attempt 3
                                    case net_kernel:connect_node(PrimaryNode) of
                                        true ->
                                            io:format(\"Connected to primary node~n\"),
                                            io:format(\"Joining Khepri cluster...~n\"),
                                            case khepri_cluster:join(PrimaryNode) of
                                                ok -> 
                                                    io:format(\"Successfully joined Khepri cluster!~n\"),
                                                    timer:sleep(infinity);
                                                {error, JoinError} ->
                                                    io:format(\"Failed to join Khepri cluster: ~p~n\", [JoinError]),
                                                    timer:sleep(infinity)
                                            end;
                                        false ->
                                            io:format(\"Failed to connect after all retries~n\"),
                                            timer:sleep(infinity)
                                    end
                            end
                    end;
                Error -> 
                    io:format(\"Failed to start Khepri: ~p~n\", [Error]),
                    timer:sleep(5000),
                    init:stop(1)
            end.
        " >> logs/secondary.log 2>&1 &
    
    # Wait for node to start
    sleep 3
    
    # Check if node started successfully
    if check_node_running "node2"; then
        echo -e "${GREEN}Secondary node started successfully${NC}"
        # Show last few log lines
        echo -e "${BLUE}Last few log lines:${NC}"
        tail -n 5 logs/secondary.log
    else
        echo -e "${RED}Failed to start secondary node${NC}"
        echo -e "${RED}Log file:${NC}"
        cat logs/secondary.log
        exit 1
    fi
}

# Check cluster status
function check_status {
    echo -e "${BLUE}Checking cluster status...${NC}"
    HOST=$(hostname -s)
    
    # First check if the nodes are running
    for node in node1 node2; do
        if check_node_running $node; then
            echo -e "${GREEN}✓ $node is running${NC}"
        else
            echo -e "${RED}✗ $node is not running${NC}"
        fi
    done
    
    # Generate a unique name for the checker node
    CHECKER_NAME="checker_$$"
    
    # Start a temporary node to check status
    erl -pa build/dev/erlang/*/ebin -pa build/dev/erlang/*/_gleam_artefacts \
        -sname $CHECKER_NAME \
        -setcookie khepri_test \
        -noshell \
        -eval "
            io:format(\"~nCluster status check:~n\"),
            % Connect to primary
            case net_kernel:connect_node('node1@$HOST') of
                true ->
                    io:format(\"Connected to primary node~n\"),
                    % Get connected nodes
                    Nodes = rpc:call('node1@$HOST', erlang, nodes, []),
                    io:format(\"Connected nodes: ~p~n\", [Nodes]),
                    % Get Khepri cluster info
                    try
                        {ok, Members} = rpc:call('node1@$HOST', khepri_cluster, members, []),
                        io:format(\"Khepri cluster members: ~p~n\", [Members]),
                        {ok, Knodes} = rpc:call('node1@$HOST', khepri_cluster, nodes, []),
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
    echo -e "${BLUE}Khepri Cluster Test Script${NC}"
    echo "Usage:"
    echo "  $0 start       # Start primary and secondary nodes"
    echo "  $0 primary     # Start only primary node"
    echo "  $0 secondary   # Start only secondary node"
    echo "  $0 status      # Check cluster status"
    echo "  $0 stop        # Stop all nodes"
    echo "  $0 logs        # Show node logs"
}

# Show logs
function show_logs {
    echo -e "${BLUE}Primary node log:${NC}"
    echo -e "${YELLOW}======================${NC}"
    cat logs/primary.log
    echo -e "\n${BLUE}Secondary node log:${NC}"
    echo -e "${YELLOW}======================${NC}"
    cat logs/secondary.log
}

# Main script
case "$1" in
    "start")
        stop_nodes
        start_primary
        sleep 5  # Give primary more time to initialize
        start_secondary
        sleep 3
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
    "logs")
        show_logs
        ;;
    *)
        print_usage
        ;;
esac

echo -e "${GREEN}Done!${NC}"
