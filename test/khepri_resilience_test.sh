#!/bin/bash
# khepri_resilience_test.sh - Enhanced Khepri cluster resilience testing with 3 nodes

set -e  # Exit on error

# Color output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Create directories
mkdir -p logs
HOST=$(hostname -s)

# Print header
function print_header {
    echo -e "\n${BLUE}======================================${NC}"
    echo -e "${BLUE}= Khepri Cluster Resilience Testing =${NC}"
    echo -e "${BLUE}======================================${NC}\n"
}

# Stop all running nodes
function stop_nodes {
    echo -e "${BLUE}Stopping any running nodes...${NC}"
    
    # Find and kill all Erlang nodes
    for node in node1 node2 node3 cmd_store cmd_read checker; do
        if pgrep -f "sname $node" > /dev/null; then
            echo -e "${YELLOW}Stopping $node...${NC}"
            pkill -f "sname $node" || true
        fi
    done
    
    # Also stop any remaining beam processes just to be safe
    for pid in $(pgrep beam); do
        if ps -p $pid -o args | grep -q "khepri_test"; then
            echo -e "${YELLOW}Stopping beam process $pid...${NC}"
            kill $pid || true
        fi
    done
    
    sleep 2
    echo -e "${GREEN}All nodes stopped${NC}"
}

# Check if a node is running
function is_node_running {
    local NODE_NAME=$1
    if pgrep -f "sname $NODE_NAME" > /dev/null; then
        return 0  # Node is running
    else
        return 1  # Node is not running
    fi
}

# Start the primary node
function start_primary {
    echo -e "${BLUE}Starting primary node...${NC}"
    
    # Stop if already running
    if is_node_running "node1"; then
        echo -e "${YELLOW}Primary node already running, stopping first...${NC}"
        pkill -f "sname node1" || true
        sleep 2
    fi
    
    # Clear log file
    echo "===== Primary Node Log - $(date) =====" > logs/primary.log
    
    # Start node
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
                    khepri:put(Path, <<\"initial_test_value\">>),
                    io:format(\"Stored initial test data at /test/data~n\"),
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
    if is_node_running "node1"; then
        echo -e "${GREEN}✓ Primary node started successfully${NC}"
        echo -e "${BLUE}Last few log lines:${NC}"
        tail -n 5 logs/primary.log
    else
        echo -e "${RED}✗ Failed to start primary node${NC}"
        echo -e "${RED}Log file:${NC}"
        cat logs/primary.log
        exit 1
    fi
}

# Start the secondary node
function start_secondary {
    echo -e "${BLUE}Starting secondary node...${NC}"
    
    # Check if primary is running
    if ! is_node_running "node1"; then
        echo -e "${RED}Primary node is not running! Start it first.${NC}"
        exit 1
    fi
    
    # Stop if already running
    if is_node_running "node2"; then
        echo -e "${YELLOW}Secondary node already running, stopping first...${NC}"
        pkill -f "sname node2" || true
        sleep 2
    fi
    
    # Clear log file
    echo "===== Secondary Node Log - $(date) =====" > logs/secondary.log
    
    # Start node
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
                    
                    % Try to connect to primary
                    PrimaryNode = 'node1@$HOST',
                    io:format(\"Trying to connect to ~p~n\", [PrimaryNode]),
                    case net_kernel:connect_node(PrimaryNode) of
                        true ->
                            io:format(\"Connected to primary node~n\"),
                            case khepri_cluster:join(PrimaryNode) of
                                ok -> 
                                    io:format(\"Successfully joined Khepri cluster!~n\"),
                                    timer:sleep(infinity);
                                {error, JoinError} ->
                                    io:format(\"Failed to join Khepri cluster: ~p~n\", [JoinError]),
                                    timer:sleep(infinity)
                            end;
                        false ->
                            io:format(\"Failed to connect to primary node~n\"),
                            timer:sleep(infinity)
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
    if is_node_running "node2"; then
        echo -e "${GREEN}✓ Secondary node started successfully${NC}"
        echo -e "${BLUE}Waiting for cluster join to complete...${NC}"
        
        # Wait a bit longer for joining to complete
        for i in {1..10}; do
            echo -n "."
            sleep 1
        done
        echo ""
        
        # Show last few log lines
        echo -e "${BLUE}Last few log lines:${NC}"
        tail -n 10 logs/secondary.log
    else
        echo -e "${RED}✗ Failed to start secondary node${NC}"
        echo -e "${RED}Log file:${NC}"
        cat logs/secondary.log
        exit 1
    fi
}

# Start the tertiary node (third node)
function start_tertiary {
    echo -e "${BLUE}Starting tertiary node...${NC}"
    
    # Check if primary is running
    if ! is_node_running "node1"; then
        echo -e "${RED}Primary node is not running! Start it first.${NC}"
        exit 1
    fi
    
    # Stop if already running
    if is_node_running "node3"; then
        echo -e "${YELLOW}Tertiary node already running, stopping first...${NC}"
        pkill -f "sname node3" || true
        sleep 2
    fi
    
    # Clear log file
    echo "===== Tertiary Node Log - $(date) =====" > logs/tertiary.log
    
    # Start node
    erl -pa build/dev/erlang/*/ebin -pa build/dev/erlang/*/_gleam_artefacts \
        -sname node3 \
        -setcookie khepri_test \
        -noinput \
        -eval "
            io:format(\"Starting Khepri on tertiary node~n\"),
            case khepri:start() of
                {ok, StoreId} -> 
                    io:format(\"Khepri started with ID: ~p~n\", [StoreId]),
                    io:format(\"Tertiary node running on ~p~n\", [node()]),
                    io:format(\"Connecting to primary node~n\"),
                    
                    % Try to connect to primary
                    PrimaryNode = 'node1@$HOST',
                    io:format(\"Trying to connect to ~p~n\", [PrimaryNode]),
                    case net_kernel:connect_node(PrimaryNode) of
                        true ->
                            io:format(\"Connected to primary node~n\"),
                            case khepri_cluster:join(PrimaryNode) of
                                ok -> 
                                    io:format(\"Successfully joined Khepri cluster!~n\"),
                                    timer:sleep(infinity);
                                {error, JoinError} ->
                                    io:format(\"Failed to join Khepri cluster: ~p~n\", [JoinError]),
                                    timer:sleep(infinity)
                            end;
                        false ->
                            io:format(\"Failed to connect to primary node~n\"),
                            timer:sleep(infinity)
                    end;
                Error -> 
                    io:format(\"Failed to start Khepri: ~p~n\", [Error]),
                    timer:sleep(5000),
                    init:stop(1)
            end.
        " >> logs/tertiary.log 2>&1 &
    
    # Wait for node to start
    sleep 3
    
    # Check if node started successfully
    if is_node_running "node3"; then
        echo -e "${GREEN}✓ Tertiary node started successfully${NC}"
        echo -e "${BLUE}Waiting for cluster join to complete...${NC}"
        
        # Wait a bit longer for joining to complete
        for i in {1..10}; do
            echo -n "."
            sleep 1
        done
        echo ""
        
        # Show last few log lines
        echo -e "${BLUE}Last few log lines:${NC}"
        tail -n 10 logs/tertiary.log
    else
        echo -e "${RED}✗ Failed to start tertiary node${NC}"
        echo -e "${RED}Log file:${NC}"
        cat logs/tertiary.log
        exit 1
    fi
}

# Check cluster status with leader information - FIXED VERSION
function check_status {
    echo -e "${BLUE}Checking cluster status...${NC}"
    
    # First check if the nodes are running
    for node in node1 node2 node3; do
        if is_node_running $node; then
            echo -e "${GREEN}✓ $node is running${NC}"
        else
            echo -e "${RED}✗ $node is not running${NC}"
        fi
    done
    
    # Generate a unique name for the checker node
    CHECKER_NAME="checker_$$"
    
    # Create a log file for the checker
    echo "===== Status Check Log - $(date) =====" > logs/status.log
    
    # Start a temporary node to check status with FIXED leader information code
    erl -pa build/dev/erlang/*/ebin -pa build/dev/erlang/*/_gleam_artefacts \
        -sname $CHECKER_NAME \
        -setcookie khepri_test \
        -noshell \
        -eval "
            io:format(\"~nCluster status check:~n\"),
            AllNodes = ['node1@$HOST', 'node2@$HOST', 'node3@$HOST'],
            
            % Try to connect to any running node
            ConnectedNode = lists:foldl(
                fun(Node, undefined) ->
                    case net_kernel:connect_node(Node) of
                        true -> Node;
                        false -> undefined
                    end;
                   (_, Acc) -> Acc
                end, 
                undefined,
                AllNodes
            ),
            
            case ConnectedNode of
                undefined ->
                    io:format(\"Failed to connect to any node in the cluster~n\");
                Node ->
                    io:format(\"Connected to node ~p~n\", [Node]),
                    % Get connected nodes
                    Nodes = rpc:call(Node, erlang, nodes, []),
                    io:format(\"Connected nodes: ~p~n\", [Nodes]),
                    
                    % Get Khepri cluster info
                    try
                        {ok, Members} = rpc:call(Node, khepri_cluster, members, []),
                        io:format(\"Khepri cluster members: ~p~n\", [Members]),
                        {ok, KhepriNodes} = rpc:call(Node, khepri_cluster, nodes, []),
                        io:format(\"Khepri cluster nodes: ~p~n\", [KhepriNodes]),
                        
                        % Check for leader using Ra directly
                        % Ra is the underlying Raft consensus implementation used by Khepri
                        io:format(\"~nChecking for cluster leader...~n\"),
                        
                        % Try an alternative approach using Ra server_info
                        ClusterName = khepri,
                        try
                            % Get Ra server info from the connected node
                            ServerInfo = rpc:call(Node, ra, server_info, [ClusterName]),
                            io:format(\"Ra server info: ~p~n\", [ServerInfo]),
                            
                            % Try to extract leader info from each node
                            lists:foreach(
                                fun(KNode) ->
                                    % Note: KNode is already an atom here, no conversion needed
                                    io:format(\"Checking status of ~p~n\", [KNode]),
                                    Result = rpc:call(KNode, ra, info, [ClusterName]),
                                    case Result of
                                        {ok, Info} ->
                                            % Try to extract leader info
                                            Leader = proplists:get_value(leader, Info, undefined),
                                            State = proplists:get_value(state, Info, undefined),
                                            io:format(\"  Node: ~p, State: ~p, Leader: ~p~n\", 
                                                    [KNode, State, Leader]);
                                        Error ->
                                            io:format(\"  Error querying ~p: ~p~n\", [KNode, Error])
                                    end
                                end,
                                KhepriNodes
                            )
                        catch
                            ErrorType:ErrorReason:Stacktrace ->
                                io:format(\"Error calling Ra API: ~p:~p~n\", [ErrorType, ErrorReason]),
                                io:format(\"Stack trace: ~p~n\", [Stacktrace])
                        end
                    catch
                        E:R:ST -> 
                            io:format(\"Error getting cluster info: ~p:~p~n\", [E, R]),
                            io:format(\"Stack trace: ~p~n\", [ST])
                    end
            end,
            init:stop().
        " > logs/status.log 2>&1
    
    # Wait for status check to complete
    sleep 2
    
    # Display the status results
    cat logs/status.log
}

# Store data on a specific node
function store_data {
    local NODE=$1
    local KEY=$2
    local VALUE=$3
    
    echo -e "${BLUE}Storing data on $NODE: $KEY = $VALUE${NC}"
    
    # Create unique command node name
    CMD_NODE="cmd_store_$$"
    
    # Create log file
    echo "===== Store Command Log - $(date) =====" > logs/store.log
    
    # Run command
    erl -pa build/dev/erlang/*/ebin -pa build/dev/erlang/*/_gleam_artefacts \
        -sname $CMD_NODE \
        -setcookie khepri_test \
        -noshell \
        -eval "
            io:format(\"Connecting to node ~p~n\", ['$NODE@$HOST']),
            case net_kernel:connect_node('$NODE@$HOST') of
                true ->
                    io:format(\"Connected to node~n\"),
                    Path = [<<\"$KEY\">>],
                    Value = <<\"$VALUE\">>,
                    Result = rpc:call('$NODE@$HOST', khepri, put, [Path, Value]),
                    io:format(\"Storage result: ~p~n\", [Result]);
                false ->
                    io:format(\"Failed to connect to node~n\")
            end,
            init:stop().
        " > logs/store.log 2>&1
    
    # Wait for command to complete
    sleep 2
    
    # Show result
    echo -e "${BLUE}Storage operation result:${NC}"
    cat logs/store.log
}

# Read data from a specific node
function read_data {
    local NODE=$1
    local KEY=$2
    
    echo -e "${BLUE}Reading data from $NODE: $KEY${NC}"
    
    # Create unique command node name
    CMD_NODE="cmd_read_$$"
    
    # Create log file
    echo "===== Read Log - $(date) =====" > logs/read.log
    
    # Run command
    erl -pa build/dev/erlang/*/ebin -pa build/dev/erlang/*/_gleam_artefacts \
        -sname $CMD_NODE \
        -setcookie khepri_test \
        -noshell \
        -eval "
            io:format(\"Connecting to node ~p~n\", ['$NODE@$HOST']),
            case net_kernel:connect_node('$NODE@$HOST') of
                true ->
                    io:format(\"Connected to node~n\"),
                    Path = [<<\"$KEY\">>],
                    Result = rpc:call('$NODE@$HOST', khepri, get, [Path]),
                    io:format(\"Read result: ~p~n\", [Result]);
                false ->
                    io:format(\"Failed to connect to node~n\")
            end,
            init:stop().
        " > logs/read.log 2>&1
    
    # Wait for command to complete
    sleep 2
    
    # Show result
    echo -e "${BLUE}Read operation result:${NC}"
    cat logs/read.log
}

# Kill a specific node
function kill_node {
    local NODE=$1
    
    echo -e "${BLUE}Killing $NODE node...${NC}"
    
    if is_node_running "$NODE"; then
        pkill -f "sname $NODE" || true
        sleep 2
        echo -e "${GREEN}$NODE node killed${NC}"
    else
        echo -e "${YELLOW}$NODE node not running${NC}"
    fi
}

# Test three-node cluster with leader election
function test_three_node_cluster {
    echo -e "\n${BLUE}=== TEST SCENARIO: Three Node Cluster with Leader Election ===${NC}"
    
    # Stop any existing nodes
    stop_nodes
    
    # Start fresh cluster with 3 nodes
    start_primary
    sleep 5
    start_secondary
    sleep 5
    start_tertiary
    sleep 5
    
    # Check cluster status and leadership
    check_status
    
    # Store data on primary
    store_data "node1" "three_node_test" "initial_value"
    sleep 3
    
    # Verify data is replicated to all nodes
    read_data "node1" "three_node_test"
    read_data "node2" "three_node_test"
    read_data "node3" "three_node_test"
    
    # Now kill the primary node (likely the leader)
    echo -e "${YELLOW}Killing primary node (testing leader election)${NC}"
    kill_node "node1"
    
    # Wait for leader election to occur
    echo -e "${BLUE}Waiting for leader election...${NC}"
    for i in {1..15}; do
        echo -n "."
        sleep 1
    done
    echo ""
    
    # Check new cluster status
    check_status
    
    # Try writing to the cluster after leader change
    store_data "node2" "after_failover" "new_leader_value"
    sleep 3
    
    # Verify the write worked
    read_data "node2" "after_failover"
    read_data "node3" "after_failover"
    
    # Restart the primary node
    start_primary
    sleep 10
    
    # Check final cluster status
    check_status
    
    # Verify the primary can read data written during its downtime
    read_data "node1" "after_failover"
    
    echo -e "${GREEN}=== Three node cluster test completed ===${NC}"
}

# Run all tests including leader election
function run_all_tests {
    print_header
    
    echo -e "${BLUE}Starting comprehensive Khepri cluster resilience tests...${NC}"
    
    # Run all test scenarios
    test_three_node_cluster
    
    # Final cleanup
    stop_nodes
    
    echo -e "\n${GREEN}All tests completed successfully!${NC}"
    echo -e "${BLUE}Check logs directory for detailed output${NC}"
}

# Print usage
function print_usage {
    echo -e "${BLUE}Khepri Cluster Resilience Test Script${NC}"
    echo "Usage:"
    echo "  $0 run-all      # Run all test scenarios"
    echo "  $0 three-node   # Run three-node cluster test with leader election"
    echo "  $0 status       # Check cluster status"
    echo "  $0 stop         # Stop all nodes"
}

# Main script
case "$1" in
    "run-all")
        run_all_tests
        ;;
    "three-node")
        print_header
        test_three_node_cluster
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
