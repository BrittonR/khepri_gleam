//// Khepri multi-node cluster example
////
//// This module demonstrates how to create and use a multi-node Khepri cluster.
//// You'll need to run this module on multiple terminals with different node names.

// src/khepri_multi_node.gleam

import gleam/erlang
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None}
import gleam/result
import gleam/string
import khepri_gleam
import khepri_gleam_cluster

/// Mode of operation for the node
type NodeRole {
  /// First node in the cluster (will be joined by others)
  Primary

  /// Secondary node that joins the cluster
  Secondary(primary_node: String)

  /// Node that just queries data
  Client(primary_node: String)

  /// Invalid arguments provided
  InvalidArgs
}

/// Main entry point - run with different arguments to test multi-node clustering
pub fn main() -> Nil {
  // Get command line arguments
  let args = erlang.start_arguments()

  // Parse arguments to determine node role
  let role = case args {
    ["primary"] -> Primary
    ["secondary", primary_node] -> Secondary(primary_node)
    ["client", primary_node] -> Client(primary_node)
    _ -> {
      print_usage()
      InvalidArgs
    }
  }

  // Exit if args were invalid
  case role {
    InvalidArgs -> {
      io.println("Exiting due to invalid arguments.")
      Nil
    }
    _ -> run_node(role)
  }
}

/// Run the node with the specified role
fn run_node(role: NodeRole) -> Nil {
  // Start Khepri and clustering
  io.println("\n=== Khepri Multi-Node Cluster Example ===\n")
  io.println("Starting Khepri...")
  khepri_gleam.start()

  // Start the cluster actor
  let assert Ok(cluster) = khepri_gleam_cluster.start_with_logger(custom_logger)

  // Different behavior based on role
  case role {
    Primary -> run_primary_node(cluster)
    Secondary(primary_node) -> run_secondary_node(cluster, primary_node)
    Client(primary_node) -> run_client_node(cluster, primary_node)
    InvalidArgs -> Nil
    // Should never reach here
  }

  // Keep running to maintain the cluster
  process.sleep_forever()
}

fn print_usage() -> Nil {
  io.println("Usage:")
  io.println("  Primary node:    gleam run -m khepri_multi_node -- primary")
  io.println(
    "  Secondary node:  gleam run -m khepri_multi_node -- secondary primary_node@host",
  )
  io.println(
    "  Client node:     gleam run -m khepri_multi_node -- client primary_node@host",
  )
  io.println("\nExample:")
  io.println(
    "  Terminal 1: gleam run -m khepri_multi_node -- --name node1@127.0.0.1 --cookie khepri_test primary",
  )
  io.println(
    "  Terminal 2: gleam run -m khepri_multi_node -- --name node2@127.0.0.1 --cookie khepri_test secondary node1@127.0.0.1",
  )
  io.println(
    "  Terminal 3: gleam run -m khepri_multi_node -- --name node3@127.0.0.1 --cookie khepri_test client node1@127.0.0.1",
  )
}

/// Running as the primary node
fn run_primary_node(
  cluster: process.Subject(khepri_gleam_cluster.ClusterMessage),
) -> Nil {
  io.println("Running as PRIMARY node")

  // Wait for leader election
  case khepri_gleam_cluster.wait_for_leader(5000) {
    Ok(_) -> io.println("Leader election successful")
    Error(err) -> io.println("Leader election issue: " <> err)
  }

  // Store some data
  write_test_data()

  // Get cluster status
  print_cluster_status(cluster)

  // Listen for updates from other nodes
  process.sleep(1000)
  read_test_data()

  io.println("\nPrimary node running. Other nodes can join now.")
}

/// Running as a secondary node
fn run_secondary_node(
  cluster: process.Subject(khepri_gleam_cluster.ClusterMessage),
  primary_node: String,
) -> Nil {
  io.println("Running as SECONDARY node")
  io.println("Joining primary node: " <> primary_node)

  // Join the cluster
  case khepri_gleam_cluster.join(cluster, primary_node, 5000) {
    Ok(_) -> {
      io.println("Successfully joined the cluster!")

      // Get cluster status
      print_cluster_status(cluster)

      // Read data from the cluster
      io.println("\nReading data from the cluster:")
      read_test_data()

      // Write some data
      io.println("\nWriting data to the cluster:")
      write_secondary_data()
    }
    Error(err) -> {
      io.println("Failed to join cluster: " <> string.inspect(err))
    }
  }

  io.println("\nSecondary node running.")
}

/// Running as a client node
fn run_client_node(
  cluster: process.Subject(khepri_gleam_cluster.ClusterMessage),
  primary_node: String,
) -> Nil {
  io.println("Running as CLIENT node")
  io.println("Joining primary node: " <> primary_node)

  // Join the cluster
  case khepri_gleam_cluster.join(cluster, primary_node, 5000) {
    Ok(_) -> {
      io.println("Successfully joined the cluster!")

      // Get cluster status
      print_cluster_status(cluster)

      // Read all data
      io.println("\nReading all data from the cluster:")
      read_test_data()
      read_secondary_data()

      // Write client data
      write_client_data()

      // Read again to verify
      io.println("\nReading all data after updates:")
      read_test_data()
      read_secondary_data()
      read_client_data()
    }
    Error(err) -> {
      io.println("Failed to join cluster: " <> string.inspect(err))
    }
  }

  io.println("\nClient node running.")
}

fn write_test_data() -> Nil {
  io.println("\nWriting test data to the cluster...")

  // Create test paths
  let paths = [
    "/:cluster_test/fruits/apple", "/:cluster_test/fruits/banana",
    "/:cluster_test/vegetables/carrot",
  ]

  // Store data at each path - using each instead of index_map for correct return type
  list.each(paths, fn(path) {
    let index = case path {
      "/:cluster_test/fruits/apple" -> 0
      "/:cluster_test/fruits/banana" -> 1
      _ -> 2
    }
    let value = "primary_value_" <> int.to_string(index)
    io.println("Writing: " <> path <> " = " <> value)

    khepri_gleam.put(khepri_gleam.to_khepri_path(path), value)
  })
}

fn write_secondary_data() -> Nil {
  // Create test path for secondary node
  let path = "/:cluster_test/secondary/data"
  let value = "secondary_node_data"

  io.println("Writing: " <> path <> " = " <> value)
  khepri_gleam.put(khepri_gleam.to_khepri_path(path), value)
}

fn write_client_data() -> Nil {
  // Create test path for client node
  let path = "/:cluster_test/client/data"
  let value = "client_node_data"

  io.println("\nWriting client data: " <> path <> " = " <> value)
  khepri_gleam.put(khepri_gleam.to_khepri_path(path), value)
}

fn read_test_data() -> Nil {
  io.println("Reading primary node test data:")

  // Read data from each path
  let paths = [
    "/:cluster_test/fruits/apple", "/:cluster_test/fruits/banana",
    "/:cluster_test/vegetables/carrot",
  ]

  list.each(paths, fn(path) {
    case khepri_gleam.get_string(path) {
      Ok(value) -> io.println("  " <> path <> " = " <> value)
      Error(err) -> io.println("  " <> path <> " = ERROR: " <> err)
    }
  })
}

fn read_secondary_data() -> Nil {
  io.println("Reading secondary node data:")

  let path = "/:cluster_test/secondary/data"
  case khepri_gleam.get_string(path) {
    Ok(value) -> io.println("  " <> path <> " = " <> value)
    Error(err) -> io.println("  " <> path <> " = ERROR: " <> err)
  }
}

fn read_client_data() -> Nil {
  io.println("Reading client node data:")

  let path = "/:cluster_test/client/data"
  case khepri_gleam.get_string(path) {
    Ok(value) -> io.println("  " <> path <> " = " <> value)
    Error(err) -> io.println("  " <> path <> " = ERROR: " <> err)
  }
}

fn print_cluster_status(
  cluster: process.Subject(khepri_gleam_cluster.ClusterMessage),
) -> Nil {
  io.println("\nCluster status:")

  // Get cluster nodes
  case khepri_gleam_cluster.list_nodes(cluster, 2000) {
    Ok(nodes) -> {
      io.println("  Nodes in cluster: " <> string.inspect(nodes))
    }
    Error(_) -> {
      io.println("  Failed to get cluster nodes")
    }
  }

  // Get cluster status
  case khepri_gleam_cluster.get_status(cluster, 2000) {
    Ok(status) -> {
      io.println("  Current node: " <> status.node)
      io.println(
        "  Connected nodes: " <> string.inspect(status.connected_nodes),
      )
      io.println("  Is leader: " <> string.inspect(status.is_leader))
    }
    Error(_) -> {
      io.println("  Failed to get cluster status")
    }
  }
}

/// Custom logger for better multi-node output
fn custom_logger(level: String, msg: String) -> Nil {
  let level_color = case level {
    "error" -> "\u{001b}[31m"
    // Red
    "warn" -> "\u{001b}[33m"
    // Yellow
    "info" -> "\u{001b}[36m"
    // Cyan
    _ -> "\u{001b}[0m"
    // Normal
  }

  let reset = "\u{001b}[0m"
  io.println("[Khepri Cluster] " <> level_color <> level <> reset <> " " <> msg)
}
