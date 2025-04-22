// test_khepri_cluster.gleam
// A standalone script to test clustering between multiple nodes
// This is meant to be run manually, not as part of automated tests

import gleam/erlang
import gleam/erlang/process
import gleam/io
import gleam/result
import gleam/string
import khepri_gleam
import khepri_gleam_cluster

pub fn main() {
  io.println("\n=== Khepri Cluster Manual Test ===\n")

  // Check if we're running as a distributed Erlang node
  let node_name = erlang.node()
  case string.contains(node_name, "@") {
    True -> {
      io.println("Running on distributed Erlang node: " <> node_name)

      // Try to determine our role based on node name
      case string.contains(node_name, "node1") {
        True -> run_primary_node()
        False ->
          case string.contains(node_name, "node2") {
            True -> run_secondary_node()
            False -> run_client_node()
          }
      }
    }
    False -> {
      io.println(
        "ERROR: Not running as a distributed Erlang node. Please start with:\n"
        <> "gleam run -m test_khepri_cluster -- --name node1@127.0.0.1 --cookie khepri_test_cookie\n"
        <> "Or for node2:\n"
        <> "gleam run -m test_khepri_cluster -- --name node2@127.0.0.1 --cookie khepri_test_cookie\n",
      )
      panic("Not a distributed node")
    }
  }
}

fn run_primary_node() {
  io.println("\n--- Running as PRIMARY node ---\n")

  // Start Khepri
  io.println("Starting Khepri on primary node...")
  khepri_gleam.start()

  // Store some test data
  let path = "/:test/clustering"
  khepri_gleam.put(khepri_gleam.to_khepri_path(path), "primary_node_value")
  io.println("Stored test data on primary node")

  // Wait for incoming connections
  io.println("\nPrimary node is running. Other nodes can now join.")
  io.println("Press Ctrl+C to stop.")

  // Keep the process running
  process.sleep_forever()
}

fn run_secondary_node() {
  io.println("\n--- Running as SECONDARY node ---\n")

  // Start Khepri
  io.println("Starting Khepri on secondary node...")
  khepri_gleam.start()

  // Join the cluster - assuming node1 is the primary
  io.println("Joining the primary node (node1@127.0.0.1)...")
  let join_result = khepri_gleam_cluster.join("node1@127.0.0.1")

  case join_result {
    Ok(_) -> io.println("Successfully joined the cluster!")
    Error(err) -> io.println("Failed to join cluster: " <> err)
  }

  // Wait for leader election
  io.println("Waiting for leader election...")
  let _ = khepri_gleam_cluster.wait_for_leader()

  // Get cluster members to verify joining worked
  io.println("\nCluster members:")
  case khepri_gleam_cluster.members() {
    Ok(members) -> {
      io.println(string.inspect(members))
    }
    Error(err) -> io.println("Failed to get members: " <> err)
  }

  // Store some data from this node
  let path = "/:secondary/test"
  khepri_gleam.put(khepri_gleam.to_khepri_path(path), "secondary_node_value")
  io.println("Stored test data on secondary node")

  // Try to read primary node data to verify replication
  io.println("\nTrying to read primary node data...")
  let primary_data = khepri_gleam.get_string("/:test/clustering")
  case primary_data {
    Ok(value) -> {
      io.println("Successfully read data from primary node: " <> value)
      io.println("Clustering is working correctly!")
    }
    Error(err) -> {
      io.println("Failed to read data from primary node: " <> err)
      io.println("Clustering may not be working properly.")
    }
  }

  io.println("\nSecondary node is running.")
  io.println("Press Ctrl+C to stop.")

  // Keep the process running
  process.sleep_forever()
}

fn run_client_node() {
  io.println("\n--- Running as CLIENT node ---\n")

  // Try to connect to the cluster nodes
  io.println("Connecting to cluster nodes...")
  let connected_node1 = erlang.connect_node("node1@127.0.0.1")
  let connected_node2 = erlang.connect_node("node2@127.0.0.1")

  io.println("Connected to node1: " <> string.inspect(connected_node1))
  io.println("Connected to node2: " <> string.inspect(connected_node2))

  // Start Khepri
  io.println("\nStarting Khepri client...")
  khepri_gleam.start()

  // Wait for leader election
  io.println("Waiting for leader election...")
  let _ = khepri_gleam_cluster.wait_for_leader()

  // Get cluster members
  io.println("\nCluster members:")
  case khepri_gleam_cluster.members() {
    Ok(members) -> {
      io.println(string.inspect(members))
    }
    Error(err) -> io.println("Failed to get members: " <> err)
  }

  // Get cluster nodes
  io.println("\nCluster nodes:")
  case khepri_gleam_cluster.nodes() {
    Ok(nodes) -> {
      io.println(string.inspect(nodes))
    }
    Error(err) -> io.println("Failed to get nodes: " <> err)
  }

  // Try to read data from both nodes to verify replication
  io.println("\nVerifying data replication across the cluster:")

  let primary_data = khepri_gleam.get_string("/:test/clustering")
  case primary_data {
    Ok(value) -> {
      io.println("Primary node data: " <> value)
    }
    Error(err) -> io.println("Failed to read primary node data: " <> err)
  }

  let secondary_data = khepri_gleam.get_string("/:secondary/test")
  case secondary_data {
    Ok(value) -> {
      io.println("Secondary node data: " <> value)
    }
    Error(err) -> io.println("Failed to read secondary node data: " <> err)
  }

  // Create some data from the client
  let path = "/:client/test"
  khepri_gleam.put(khepri_gleam.to_khepri_path(path), "client_node_value")
  io.println("\nStored test data from client node")

  io.println("\nClient node test complete. Press Ctrl+C to stop.")

  // Keep the process running
  process.sleep_forever()
}
