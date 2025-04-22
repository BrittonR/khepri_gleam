#!/bin/bash
# run_khepri_node.sh - Script to run Khepri distributed nodes

# Check arguments
if [ $# -lt 1 ]; then
  echo "Usage: $0 <role> [primary_node]"
  echo "  role: primary, secondary, or client"
  echo "  primary_node: required for secondary and client roles (e.g., node1@127.0.0.1)"
  exit 1
fi

ROLE=$1
PRIMARY_NODE=$2

# Ensure required secondary/client argument is provided
if [[ "$ROLE" == "secondary" || "$ROLE" == "client" ]] && [ -z "$PRIMARY_NODE" ]; then
  echo "Error: When using role '$ROLE', you must provide the primary_node parameter"
  echo "Example: $0 $ROLE node1@127.0.0.1"
  exit 1
fi

# Build the project first
echo "Building project..."
gleam build

# Set up the node name based on role
NODE_NAME=""
case "$ROLE" in
  "primary")
    NODE_NAME="node1@127.0.0.1"
    ;;
  "secondary")
    NODE_NAME="node2@127.0.0.1"
    ;;
  "client")
    NODE_NAME="node3@127.0.0.1"
    ;;
  *)
    echo "Error: Unknown role '$ROLE'. Must be primary, secondary, or client."
    exit 1
    ;;
esac

# Create a temporary file to hold our Erlang initialization code
TEMP_FILE=$(mktemp)
trap "rm -f $TEMP_FILE" EXIT

# Start khepri_multi_node with the given arguments
if [[ "$ROLE" == "primary" ]]; then
  cat > $TEMP_FILE <<EOF
io:format("Starting Khepri node with role: $ROLE~n"),
case file:consult("$TEMP_FILE.args") of
    {ok, [Args]} ->
        khepri_multi_node:main_0();
    _ ->
        io:format("Failed to read arguments~n")
end.
EOF
  # Store the arguments for Gleam
  echo "[\"$ROLE\"]." > $TEMP_FILE.args
else
  cat > $TEMP_FILE <<EOF
io:format("Starting Khepri node with role: $ROLE~n"),
case file:consult("$TEMP_FILE.args") of
    {ok, [Args]} ->
        khepri_multi_node:main_0();
    _ ->
        io:format("Failed to read arguments~n")
end.
EOF
  # Store the arguments for Gleam
  echo "[\"$ROLE\", \"$PRIMARY_NODE\"]." > $TEMP_FILE.args
fi

# Start the Erlang node with distribution enabled
echo "Starting Erlang node: $NODE_NAME"
echo "Role: $ROLE"
if [[ "$ROLE" != "primary" ]]; then
  echo "Connecting to primary node: $PRIMARY_NODE"
fi

# Run Erlang with the correct distribution settings and our initialization code
erl -pa build/dev/erlang/*/ebin -pa build/dev/erlang/*/_gleam_artefacts \
    -name $NODE_NAME \
    -setcookie khepri_test \
    -noshell \
    -eval "case file:script(\"$TEMP_FILE\") of ok -> ok; _ -> init:stop(1) end."

# If we get here, something went wrong
echo "Node terminated."
