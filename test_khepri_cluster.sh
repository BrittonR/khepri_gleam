#!/bin/bash
# khepri_cluster_test.sh - Improved script for testing Khepri clustering

set -e  # Exit on error

# Configuration
COOKIE="khepri_test"
HOST="$(hostname -s)"  # Get short hostname
BASE_IP="127.0.0.1"

# Color output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Print usage information
function print_usage {
  echo -e "${BLUE}Khepri Cluster Test Script${NC}"
  echo "Usage:"
  echo "  $0 start <role> [primary_node]"
  echo "  $0 start-all                 # Start primary, secondary and client nodes"
  echo "  $0 stop-all                  # Stop all running nodes"
  echo "  $0 status                    # Check status of running nodes"
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
}

# Start a single node
function start_node {
  local ROLE=$1
  local PRIMARY_NODE=$2
  local NODE_NAME=""
  
  # Determine node name based on role
  case "$ROLE" in
    "primary")
      NODE_NAME="node1@$HOST"
      ;;
    "secondary")
      NODE_NAME="node2@$HOST"
      if [ -z "$PRIMARY_NODE" ]; then
        echo -e "${RED}Error: When using role 'secondary', you must provide the primary_node parameter${NC}"
        echo "Example: $0 start secondary node1@$HOST"
        exit 1
      fi
      ;;
    "client")
      NODE_NAME="node3@$HOST"
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
  
  # Start the node in the background
  echo -e "${GREEN}Starting $ROLE node: $NODE_NAME${NC}"
  
  # Build the Erlang command
  local CMD="erl -pa build/dev/erlang/*/ebin -pa build/dev/erlang/*/_gleam_artefacts"
  CMD="$CMD -name $NODE_NAME -setcookie $COOKIE -detached"
  
  if [ "$ROLE" == "primary" ]; then
    CMD="$CMD -eval 'khepri_direct_helper:start_node(\"$NODE_NAME\", \"$COOKIE\", \"$ROLE\", \"\").'"
  else
    CMD="$CMD -eval 'khepri_direct_helper:start_node(\"$NODE_NAME\", \"$COOKIE\", \"$ROLE\", \"$PRIMARY_NODE\").'"
  fi
  
  # Execute the command
  echo "Running: $CMD"
  eval $CMD
  
  # Wait a bit for the node to initialize
  sleep 2
  
  # Check if the node started successfully
  if pgrep -f "name $NODE_NAME" > /dev/null; then
    echo -e "${GREEN}✓ Node $NODE_NAME started successfully${NC}"
  else
    echo -e "${RED}✗ Failed to start node $NODE_NAME${NC}"
    exit 1
  fi
}

# Start all three types of nodes
function start_all_nodes {
  echo -e "${BLUE}Starting a complete Khepri cluster with three nodes...${NC}"
  
  # Start primary node
  start_node "primary"
  sleep 3  # Give primary node time to initialize
  
  # Start secondary node
  start_node "secondary" "node1@$HOST"
  sleep 2
  
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
  for pid in $(pgrep -f "name node[1-3]@$HOST.*cookie $COOKIE"); do
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
  
  # Try to get more detailed status using Erlang remote calls, if possible
  if pgrep -f "name node1@$HOST" > /dev/null; then
    echo -e "\n${BLUE}Attempting to get cluster status from primary node...${NC}"
    erl -name status_check@$HOST -setcookie $COOKIE -noshell \
        -eval "rpc:call('node1@$HOST', io, format, [\"Remote call successful~n\"]), 
               io:format(\"Cluster members: ~p~n\", [rpc:call('node1@$HOST', khepri_cluster, members, [])]),
               io:format(\"Cluster nodes: ~p~n\", [rpc:call('node1@$HOST', khepri_cluster, nodes, [])]),
               init:stop()."
  fi
}

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
  *)
    print_usage
    exit 1
    ;;
esac

exit 0
