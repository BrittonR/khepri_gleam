#!/bin/bash
# khepri_cluster_test.sh - Robust script for testing Khepri clustering

set -e  # Exit on error

# Configuration
COOKIE="khepri_test"
HOST="$(hostname -s)"  # Get short hostname

# Color output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Directories and paths
PROJECT_DIR="$(pwd)"
HELPER_FILE="$PROJECT_DIR/src/khepri_direct_helper.erl"
LOG_DIR="$PROJECT_DIR/logs"

# Ensure log directory exists
mkdir -p "$LOG_DIR"

# Print usage information
function print_usage {
    echo -e "${BLUE}Khepri Cluster Test Script${NC}"
    echo "Usage:"
    echo "  $0 start <role> [primary_node]"
    echo "  $0 start-all                 # Start primary, secondary and client nodes"
    echo "  $0 stop-all                  # Stop all running nodes"
    echo "  $0 status                    # Check status of running nodes"
    echo "  $0 logs <node_name>          # Show logs for a specific node"
    echo ""
    echo "Roles:"
    echo "  primary                      # Start a primary node"
    echo "  secondary <primary_node>     # Start a secondary node connected to primary"
    echo "  client <primary_node>        # Start a client node connected to primary"
    echo ""
    echo "Examples:"
    echo "  $0 start primary             # Start primary node"
    echo "  $0 start secondary node1@$HOST  # Start secondary node"
    echo "  $0 start-all                 # Start all three node types"
    echo "  $0 logs node1                # Show logs for node1"
}

# Start a single node
function start_node {
    local ROLE=$1
    local PRIMARY_NODE=$2
    local NODE_NAME=""
    local NODE_SHORT=""
    
    # Determine node name based on role
    case "$ROLE" in
        "primary")
            NODE_SHORT="node1"
            NODE_NAME="${NODE_SHORT}@$HOST"
            ;;
        "secondary")
            NODE_SHORT="node2"
            NODE_NAME="${NODE_SHORT}@$HOST"
            if [ -z "$PRIMARY_NODE" ]; then
                echo -e "${RED}Error: When using role 'secondary', you must provide the primary_node parameter${NC}"
                echo "Example: $0 start secondary node1@$HOST"
                exit 1
            fi
            ;;
        "client")
            NODE_SHORT="node3"
            NODE_NAME="${NODE_SHORT}@$HOST"
            if [ -z "$PRIMARY_NODE" ]; then
                echo -e "${RED}Error: When using role 'client', you must provide the primary_node parameter${NC}"
                echo "Example: $0 start client node1@$HOST"
                exit 1
            fi
            ;;
        *)
            echo -e "${RED}Error: Unknown role '$ROLE'. Must be primary, secondary, or client.${NC}"
            print_usage
            exit 1
            ;;
    esac
    
    # Check if node is already running
    if pgrep -f "name $NODE_NAME" > /dev/null; then
        echo -e "${YELLOW}Node $NODE_NAME is already running${NC}"
        return 0
    fi
    
    # Log file path
    local LOG_FILE="$LOG_DIR/${NODE_SHORT}.log"
    
    # Start the node in the background
    echo -e "${GREEN}Starting $ROLE node: $NODE_NAME${NC}"
    echo -e "${BLUE}Log file: $LOG_FILE${NC}"
    
    # Run Erlang node in background with output to log file
    (
        echo "===== Khepri $ROLE Node: $NODE_NAME =====" > "$LOG_FILE"
        echo "Started at: $(date)" >> "$LOG_FILE"
        echo "" >> "$LOG_FILE"
        
        cd "$PROJECT_DIR"
        erl -pa build/dev/erlang/*/ebin -pa build/dev/erlang/*/_gleam_artefacts \
            -name "$NODE_NAME" \
            -setcookie "$COOKIE" \
            -noinput \
            -eval "io:format(\"Starting Khepri node...~n\"), 
                   khepri_direct_helper:start_node(\"$NODE_NAME\", \"$COOKIE\", \"$ROLE\", \"$PRIMARY_NODE\"),
                   io:format(\"Node is running. Press Ctrl+C to stop.~n\"),
                   timer:sleep(infinity)." >> "$LOG_FILE" 2>&1 &
    )
    
    # Wait a bit for the node to start
    sleep 3
    
    # Check if node started
    if pgrep -f "name $NODE_NAME" > /dev/null; then
        echo -e "${GREEN}✓ Node $NODE_NAME started successfully${NC}"
        # Display last few log lines
        echo -e "${BLUE}Last few log lines:${NC}"
        tail -n 5 "$LOG_FILE"
        echo -e "${BLUE}Use '$0 logs $NODE_SHORT' to see full logs${NC}"
    else
        echo -e "${RED}✗ Failed to start node $NODE_NAME${NC}"
        echo -e "${RED}Check the log file for errors: $LOG_FILE${NC}"
        cat "$LOG_FILE"
    fi
}

# Start all three types of nodes
function start_all_nodes {
    echo -e "${BLUE}Starting a complete Khepri cluster with three nodes...${NC}"
    
    # Start primary node
    start_node "primary"
    sleep 5  # Give primary node more time to initialize
    
    # Start secondary node
    start_node "secondary" "node1@$HOST"
    sleep 3
    
    # Start client node
    start_node "client" "node1@$HOST"
    
    echo -e "${GREEN}All nodes started. Cluster should be forming.${NC}"
    echo "You can check the status with: $0 status"
    echo "You can stop all nodes with: $0 stop-all"
}

# Stop all running nodes
function stop_all_nodes {
    echo -e "${BLUE}Stopping all Khepri cluster nodes...${NC}"
    
    # Find and kill all Erlang nodes with our cookie
    local KILLED=0
    for pid in $(pgrep -f "name node[1-3]@$HOST"); do
        local node_name=$(ps -p $pid -o args | grep -o "name node[1-3]@$HOST" | cut -d' ' -f2)
        echo -e "${YELLOW}Stopping node $node_name (PID: $pid)${NC}"
        kill $pid
        KILLED=$((KILLED+1))
    done
    
    if [ $KILLED -eq 0 ]; then
        echo -e "${YELLOW}No running Khepri nodes found${NC}"
    else
        echo -e "${GREEN}Stopped $KILLED Khepri nodes${NC}"
    fi
}

# Check status of running nodes
function check_status {
    echo -e "${BLUE}Checking status of Khepri cluster nodes...${NC}"
    
    local COUNT=0
    for role in "primary" "secondary" "client"; do
        local node_num=$((COUNT+1))
        local node_name="node${node_num}@$HOST"
        
        if pgrep -f "name $node_name" > /dev/null; then
            local pid=$(pgrep -f "name $node_name")
            echo -e "${GREEN}✓ $role node $node_name is running (PID: $pid)${NC}"
        else
            echo -e "${YELLOW}✗ $role node $node_name is not running${NC}"
        fi
        
        COUNT=$((COUNT+1))
    done
    
    # Try to get more detailed status from primary node if running
    if pgrep -f "name node1@$HOST" > /dev/null; then
        echo -e "\n${BLUE}Getting cluster status from primary node...${NC}"
        erl -name status_check_$$@$HOST -setcookie $COOKIE -noshell \
            -eval "io:format(\"~nCluster status:~n\"),
                   case net_kernel:connect_node('node1@$HOST') of
                     true -> 
                       io:format(\"Connected to primary node~n\"),
                       rpc:call('node1@$HOST', erlang, nodes, []),
                       rpc:call('node1@$HOST', erlang, is_alive, []),
                       % Try to check khepri status if available
                       try
                         Members = rpc:call('node1@$HOST', khepri_cluster, members, []),
                         io:format(\"Khepri cluster members: ~p~n\", [Members])
                       catch
                         _:_ -> io:format(\"Could not retrieve Khepri status~n\")
                       end;
                     false -> 
                       io:format(\"Failed to connect to primary node~n\")
                   end,
                   init:stop()."
    fi
}

# Show logs for a node
function show_logs {
    local NODE_SHORT=$1
    local LOG_FILE="$LOG_DIR/${NODE_SHORT}.log"
    
    if [ -f "$LOG_FILE" ]; then
        echo -e "${BLUE}Log file for $NODE_SHORT:${NC}"
        echo -e "${YELLOW}======================${NC}"
        cat "$LOG_FILE"
        echo -e "${YELLOW}======================${NC}"
    else
        echo -e "${RED}Log file not found: $LOG_FILE${NC}"
    fi
}

# Ensure helper module exists
function ensure_helper_module {
    if [ ! -f "$HELPER_FILE" ]; then
        echo -e "${YELLOW}Creating helper module at $HELPER_FILE${NC}"
        mkdir -p "$PROJECT_DIR/src"
        cat > "$HELPER_FILE" << 'EOF'
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
            io:format("Stored test data~n");
        "secondary" ->
            io:format("Running as secondary node~n"),
            io:format("Joining primary node: ~s~n", [PrimaryNode]),
            % Join the cluster
            case PrimaryNode of
                "" -> 
                    io:format("No primary node specified~n");
                _ ->
                    % Try to join the cluster
                    io:format("Connecting to ~s~n", [PrimaryNode]),
                    case net_kernel:connect_node(list_to_atom(PrimaryNode)) of
                        true ->
                            io:format("Connected to primary node~n"),
                            case khepri_cluster:join(list_to_atom(PrimaryNode)) of
                                ok -> 
                                    io:format("Successfully joined the cluster!~n");
                                {error, Reason} ->
                                    io:format("Failed to join cluster: ~p~n", [Reason])
                            end;
                        false ->
                            io:format("Failed to connect to primary node~n")
                    end
            end;
        "client" ->
            io:format("Running as client node~n"),
            io:format("Connecting to primary node: ~s~n", [PrimaryNode]),
            % Just connect and read data
            case PrimaryNode of
                "" -> 
                    io:format("No primary node specified~n");
                _ ->
                    case net_kernel:connect_node(list_to_atom(PrimaryNode)) of
                        true ->
                            io:format("Connected to primary node~n"),
                            % Read test data
                            Path = [<<"test">>, <<"data">>],
                            case khepri:get(Path) of
                                {ok, Value} ->
                                    io:format("Read test data: ~p~n", [Value]);
                                {error, Reason} ->
                                    io:format("Failed to read test data: ~p~n", [Reason])
                            end;
                        false ->
                            io:format("Failed to connect to primary node~n")
                    end
            end
    end,
    
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
        Members = case khepri_cluster:members() of
            {ok, M} -> M;
            _ -> []
        end,
        io:format("Khepri cluster members: ~p~n", [Members]),
        ok
    catch
        _:_ ->
            io:format("Khepri not available or not started~n"),
            error
    end.
EOF
        echo -e "${GREEN}Helper module created.${NC}"
        echo "Rebuilding project to include the new module..."
        gleam build
    else
        echo -e "${GREEN}Helper module already exists.${NC}"
    fi
}

# Make sure our helper module exists
ensure_helper_module

# Main script logic
case "$1" in
    "start")
        if [ -z "$2" ]; then
            echo -e "${RED}Error: Missing role parameter${NC}"
            print_usage
            exit 1
        fi
        start_node "$2" "$3"
        ;;
    "start-all")
        start_all_nodes
        ;;
    "stop-all")
        stop_all_nodes
        ;;
    "status")
        check_status
        ;;
    "logs")
        if [ -z "$2" ]; then
            echo -e "${RED}Error: Missing node name parameter${NC}"
            echo "Example: $0 logs node1"
            exit 1
        fi
        show_logs "$2"
        ;;
    *)
        print_usage
        exit 1
        ;;
esac

exit 0
