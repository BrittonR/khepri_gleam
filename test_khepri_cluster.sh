#!/bin/bash
# Script to test Khepri clustering with multiple Erlang nodes

# Define colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Print section header
section() {
  echo -e "\n${GREEN}=== $1 ===${NC}"
}

# Print subsection header
subsection() {
  echo -e "\n${YELLOW}--- $1 ---${NC}"
}

# Print error message
error() {
  echo -e "${RED}ERROR: $1${NC}"
}

# Print info message
info() {
  echo -e "$1"
}

# Path to the compiled Erlang code
BEAM_PATH="./build/dev/erlang"

# Check if the Gleam project is compiled
if [ ! -d "$BEAM_PATH" ]; then
  error "Compiled Erlang files not found. Please run 'gleam build' first."
  exit 1
fi

# Ensure we're in the project root directory
if [ ! -f "gleam.toml" ]; then
  error "Please run this script from the project root directory (where gleam.toml is located)."
  exit 1
fi

# Check if epmd is running, start it if not
subsection "Checking EPMD status"
if ! epmd -names > /dev/null 2>&1; then
  info "EPMD not running. Starting EPMD..."
  epmd -daemon
  sleep 1
fi

# Verify EPMD is now running
if ! epmd -names > /dev/null 2>&1; then
  error "Failed to start EPMD. Please check your Erlang installation."
  exit 1
fi
info "EPMD is running"

# Create a temp directory for cookie files
TEMP_DIR=$(mktemp -d)
COOKIE_FILE="$TEMP_DIR/erlang_cookie"
echo "khepri_test_cookie" > "$COOKIE_FILE"
chmod 600 "$COOKIE_FILE"

# Clean up temp files and processes on exit
trap 'rm -rf "$TEMP_DIR"; pkill -f "node1@127.0.0.1" 2>/dev/null; pkill -f "node2@127.0.0.1" 2>/dev/null; echo "Cleanup complete"' EXIT INT TERM

section "Starting Khepri Cluster Test"

# Kill any existing test nodes
pkill -f "node1@127.0.0.1" 2>/dev/null
pkill -f "node2@127.0.0.1" 2>/dev/null
sleep 1

# First, make sure all the required files are in the path
subsection "Preparing environment"
info "Adding required paths..."
KHEPRI_ARTEFACTS="$BEAM_PATH/khepri_gleam/_gleam_artefacts"
if [ ! -d "$KHEPRI_ARTEFACTS" ]; then
  error "Khepri artifacts not found at $KHEPRI_ARTEFACTS. Please ensure the project is built correctly."
  exit 1
fi
info "Khepri artifacts found at $KHEPRI_ARTEFACTS"

# Start the first node (which will be our primary node)
subsection "Starting primary node (node1)"
info "Launching Erlang node..."
erl -pa "$BEAM_PATH" -pa "$KHEPRI_ARTEFACTS" -setcookie "khepri_test_cookie" -name "node1@127.0.0.1" -detached -eval "
  io:format(\"Starting primary node~n\"),
  % Add required paths
  code:add_paths(filelib:wildcard(\"./build/dev/erlang/*/ebin\")),
  code:add_paths(filelib:wildcard(\"./build/dev/erlang/*/_gleam_artefacts\")),
  % Start Khepri
  io:format(\"Starting Khepri on primary node~n\"),
  khepri:start(),
  io:format(\"Khepri started on primary node~n\"),
  % Store some test data
  Path = [<<\"test\">>, <<\"clustering\">>],
  khepri:put(Path, <<\"primary_node_value\">>),
  io:format(\"Stored test data on primary node~n\"),
  io:format(\"Primary node initialization complete~n\"),
  % Keep the node running
  timer:sleep(120000)
"

# Wait a moment for the first node to initialize
sleep 3

# Check if node1 is running
if ! epmd -names | grep -q "name node1 at port"; then
  error "Failed to start primary node. Please check the Erlang environment."
  info "Current EPMD registered names:"
  epmd -names
  exit 1
fi
info "Primary node started successfully"

# Start the second node that will join the cluster
subsection "Starting secondary node (node2)"
info "Launching Erlang node..."
erl -pa "$BEAM_PATH" -pa "$KHEPRI_ARTEFACTS" -setcookie "khepri_test_cookie" -name "node2@127.0.0.1" -detached -eval "
  io:format(\"Starting secondary node~n\"),
  % Add required paths
  code:add_paths(filelib:wildcard(\"./build/dev/erlang/*/ebin\")),
  code:add_paths(filelib:wildcard(\"./build/dev/erlang/*/_gleam_artefacts\")),
  % Start Khepri
  io:format(\"Starting Khepri on secondary node~n\"),
  khepri:start(),
  io:format(\"Khepri started on secondary node~n\"),
  % Sleep a bit to ensure the other node is ready
  timer:sleep(1000),
  % Join the cluster
  io:format(\"Joining the cluster~n\"),
  JoinResult = khepri_cluster:join('node1@127.0.0.1'),
  io:format(\"Join result: ~p~n\", [JoinResult]),
  % Store some data
  Path = [<<\"secondary\">>, <<\"test\">>],
  khepri:put(Path, <<\"secondary_node_value\">>),
  io:format(\"Stored test data on secondary node~n\"),
  io:format(\"Secondary node initialization complete~n\"),
  % Keep the node running
  timer:sleep(120000)
"

# Wait a moment for the second node to join
sleep 5

# Check if node2 is running
if ! epmd -names | grep -q "name node2 at port"; then
  error "Failed to start secondary node. Please check the Erlang environment."
  info "Current EPMD registered names:"
  epmd -names
  exit 1
fi
info "Secondary node started successfully"

# Start a client node that will verify cluster status
subsection "Starting client node to verify cluster"
info "Launching client node..."
erl -pa "$BEAM_PATH" -pa "$KHEPRI_ARTEFACTS" -setcookie "khepri_test_cookie" -name "client@127.0.0.1" -eval "
  io:format(\"Starting client node~n\"),
  % Add required paths
  code:add_paths(filelib:wildcard(\"./build/dev/erlang/*/ebin\")),
  code:add_paths(filelib:wildcard(\"./build/dev/erlang/*/_gleam_artefacts\")),
  
  % Connect to the cluster nodes
  io:format(\"Connecting to cluster nodes...~n\"),
  pong = net_adm:ping('node1@127.0.0.1'),
  pong = net_adm:ping('node2@127.0.0.1'),
  io:format(\"Connected to nodes~n\"),
  
  % Define test functions
  TestCluster = fun() ->
    io:format(\"~n~n~n\"),
    io:format(\"===== CLUSTER TEST RESULTS =====~n~n\"),
    
    % Check connected nodes
    ConnectedNodes = nodes(),
    io:format(\"Connected nodes: ~p~n\", [ConnectedNodes]),
    
    % Start Khepri client
    io:format(\"Starting Khepri client~n\"),
    khepri:start(),
    
    % Check cluster members
    {ok, Members} = khepri_cluster:members(),
    io:format(\"Cluster members: ~p~n\", [Members]),
    
    % Check if we can read the test data from primary node
    PrimaryPath = [<<\"test\">>, <<\"clustering\">>],
    PrimaryResult = khepri:get(PrimaryPath),
    io:format(\"Primary data get result: ~p~n\", [PrimaryResult]),
    
    % Check if we can read the test data from secondary node
    SecondaryPath = [<<\"secondary\">>, <<\"test\">>],
    SecondaryResult = khepri:get(SecondaryPath),
    io:format(\"Secondary data get result: ~p~n\", [SecondaryResult]),
    
    % Create some data on this node
    ClientPath = [<<\"client\">>, <<\"test\">>],
    khepri:put(ClientPath, <<\"client_value\">>),
    io:format(\"Wrote client test data~n\"),
    
    % Check cluster nodes from Khepri perspective
    {ok, Nodes} = khepri_cluster:nodes(),
    io:format(\"Khepri cluster nodes: ~p~n\", [Nodes]),
    
    % Get all data to verify replication
    io:format(\"~nVerifying data replication across the cluster:~n\"),
    TestPaths = [
      [<<\"test\">>, <<\"clustering\">>],
      [<<\"secondary\">>, <<\"test\">>],
      [<<\"client\">>, <<\"test\">>]
    ],
    
    lists:foreach(fun(TestPath) ->
      GetRes = khepri:get(TestPath),
      io:format(\"Path ~p: ~p~n\", [TestPath, GetRes])
    end, TestPaths),
    
    case {PrimaryResult, SecondaryResult} of
      {{ok, <<\"primary_node_value\">>}, {ok, <<\"secondary_node_value\">>}} ->
        io:format(\"~n~n✅ CLUSTERING TEST PASSED! Data is replicated correctly.~n~n\");
      _ ->
        io:format(\"~n~n❌ CLUSTERING TEST FAILED! Data replication issues detected.~n~n\")
    end,
    
    io:format(\"~n===== TEST COMPLETE =====~n\"),
    io:format(\"You can connect to the running nodes with:~n\"),
    io:format(\"erl -setcookie khepri_test_cookie -name console@127.0.0.1 -remsh node1@127.0.0.1~n\"),
    io:format(\"~n\"),
    
    % Clean up: terminate the detached nodes
    rpc:call('node1@127.0.0.1', init, stop, []),
    rpc:call('node2@127.0.0.1', init, stop, []),
    io:format(\"Terminated cluster nodes~n\")
  end,
  
  % Wait a bit before testing
  timer:sleep(2000),
  TestCluster(),
  
  % Exit after testing
  init:stop()
"

section "Test Complete"
info "Check the output above for test results."
