// src/cluster.gleam
import gleam/dynamic
import gleam/erlang
import gleam/erlang/atom
import gleam/erlang/node
import gleam/erlang/process
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import khepri_gleam
import khepri_gleam_cluster

// Main entry point
pub fn main() {
  // Get command line arguments
  let args = erlang.start_arguments()

  // Start distribution if needed
  start_distribution()

  // Get current node name for logging
  let current_node = node.self()
  let node_name = atom.to_string(node.to_atom(current_node))
  io.println("Running on node: " <> node_name)

  // Parse arguments to determine node role
  case args {
    ["primary"] -> {
      start_primary_node()
      Nil
    }
    ["secondary", primary_node] -> {
      start_secondary_node(primary_node)
      Nil
    }
    ["status"] -> {
      let _ = check_cluster_status()
      Nil
    }
    _ -> {
      print_usage()
      process.sleep(100)
      Nil
    }
  }
}

// Start Erlang distribution properly
fn start_distribution() {
  let current_node = node.self()
  let node_name = atom.to_string(node.to_atom(current_node))

  case node_name == "nonode@nohost" {
    False -> {
      io.println("Already running in distributed mode: " <> node_name)
    }
    True -> {
      io.println("Starting Erlang distribution...")

      // Create node name
      let node_atom = atom.create_from_string("node1@localhost")

      // Using a tuple with the correct format for net_kernel:start/1
      let name_mode_tuple =
        create_name_mode_tuple(node_atom, atom.create_from_string("longnames"))

      // Start distribution
      case start_distribution_raw(name_mode_tuple) {
        Ok(_) -> {
          // ONLY set cookie AFTER successfully starting distribution
          set_cookie(atom.create_from_string("khepri_cookie"))

          // Check if it worked
          let new_node = node.self()
          let new_name = atom.to_string(node.to_atom(new_node))
          io.println("Distribution started successfully! Node: " <> new_name)
        }
        Error(err) -> {
          io.println("Failed to start distribution: " <> string.inspect(err))

          // Retry with shortnames as fallback
          let shortname_tuple =
            create_name_mode_tuple(
              node_atom,
              atom.create_from_string("shortnames"),
            )
          case start_distribution_raw(shortname_tuple) {
            Ok(_) -> {
              // ONLY set cookie AFTER successfully starting distribution
              set_cookie(atom.create_from_string("khepri_cookie"))

              let new_node = node.self()
              let new_name = atom.to_string(node.to_atom(new_node))
              io.println(
                "Distribution started successfully with shortnames! Node: "
                <> new_name,
              )
            }
            Error(_) -> {
              io.println(
                "Failed to start distribution with both longnames and shortnames.",
              )
              io.println(
                "Try running directly with: erl -name node1@localhost -setcookie khepri_cookie",
              )
            }
          }
        }
      }
    }
  }
}

// Create the tuple that net_kernel:start/1 expects
@external(erlang, "erlang", "make_tuple")
fn make_tuple(size: Int, default_value: dynamic.Dynamic) -> dynamic.Dynamic

// Set a value in a tuple
@external(erlang, "erlang", "setelement")
fn set_element(
  index: Int,
  tuple: dynamic.Dynamic,
  value: dynamic.Dynamic,
) -> dynamic.Dynamic

// Create a proper {NodeName, longnames|shortnames} tuple for net_kernel:start/1
fn create_name_mode_tuple(
  node_name: atom.Atom,
  mode: atom.Atom,
) -> dynamic.Dynamic {
  // Create a 2-element tuple
  let tuple = make_tuple(2, dynamic.from(atom.create_from_string("undefined")))

  // Set the values
  let tuple = set_element(1, tuple, dynamic.from(node_name))
  let tuple = set_element(2, tuple, dynamic.from(mode))

  tuple
}

// External function to start distribution
@external(erlang, "net_kernel", "start")
fn start_distribution_raw(
  name_mode_tuple: dynamic.Dynamic,
) -> Result(dynamic.Dynamic, dynamic.Dynamic)

// Set the Erlang distribution cookie
@external(erlang, "erlang", "set_cookie")
fn set_cookie(cookie: atom.Atom) -> atom.Atom

// Rest of your implementation stays the same...
// // Print usage information
fn print_usage() {
  io.println("\nKhepri Cluster Node")
  io.println("==================")
  io.println("\nUsage:")
  io.println("  gleam run -m cluster -- primary")
  io.println("      Start a primary node that others can join")
  io.println("\n  gleam run -m cluster -- secondary <primary_node>")
  io.println("      Start a secondary node and join the primary")
  io.println("      Example: gleam run -m cluster -- secondary node1@localhost")
  io.println("\n  gleam run -m cluster -- status")
  io.println("      Check cluster status")
}

// Start a primary node
fn start_primary_node() {
  io.println("\nStarting as PRIMARY node")

  // Start Khepri
  io.println("Starting Khepri...")
  khepri_gleam.start()
  io.println("Khepri started")

  // Store some test data
  let test_path = "/:cluster/test"
  khepri_gleam.put(khepri_gleam.to_khepri_path(test_path), "primary_node_data")
  io.println("Stored test data at " <> test_path)

  // Start the cluster actor
  let assert Ok(cluster) = khepri_gleam_cluster.start()
  io.println("Cluster coordinator started")

  // Wait for leader election
  let _ = khepri_gleam_cluster.wait_for_leader(5000)
  io.println("Leader election complete")

  // Print status and wait
  print_node_status(cluster)
  io.println("\nPrimary node running. Secondary nodes can now join.")
  io.println("Press Ctrl+C to stop.")

  // Keep the process running
  process.sleep_forever()
}

// Start a secondary node and join the primary
fn start_secondary_node(primary_node: String) {
  io.println("\nStarting as SECONDARY node")
  io.println("Primary node: " <> primary_node)

  // First, start Khepri
  io.println("Starting Khepri...")
  khepri_gleam.start()
  io.println("Khepri started")

  // Start the cluster actor
  io.println("Starting cluster coordinator...")
  let assert Ok(cluster) = khepri_gleam_cluster.start()
  io.println("Cluster coordinator started")

  // Join the primary node with retry logic
  join_primary(cluster, primary_node, 5)

  // Keep the process running
  process.sleep_forever()
}

// Join the primary node with retries
fn join_primary(
  cluster: process.Subject(khepri_gleam_cluster.ClusterMessage),
  primary_node: String,
  retries: Int,
) {
  case retries <= 0 {
    True -> {
      io.println("Failed to join primary node after multiple attempts")
      Nil
    }
    False -> {
      io.println("Attempting to join " <> primary_node <> "...")

      case khepri_gleam_cluster.join(cluster, primary_node, 5000) {
        Ok(_) -> {
          io.println("Successfully joined the cluster!")

          // Check if we can read data from the primary
          io.println("Checking data from primary...")
          case khepri_gleam.get_string("/:cluster/test") {
            Ok(value) -> {
              io.println("Read primary data: " <> value)
              io.println("Cluster is fully operational")
            }
            Error(err) -> {
              io.println("Warning: Could not read primary data: " <> err)
              io.println("Data replication may not be working yet")
            }
          }

          // Get status and print
          print_node_status(cluster)
        }
        Error(err) -> {
          io.println("Join failed: " <> string.inspect(err))
          io.println("Retrying in 3 seconds...")

          process.sleep(3000)
          join_primary(cluster, primary_node, retries - 1)
        }
      }
    }
  }
}

// Check cluster status
fn check_cluster_status() -> Result(Nil, process.CallError(Nil)) {
  io.println("Checking Khepri cluster status...")

  // Start Khepri if not already running
  khepri_gleam.start()
  io.println("Khepri started or was already running")

  // Start cluster actor
  let assert Ok(cluster) = khepri_gleam_cluster.start()

  // Print status
  print_node_status(cluster)

  // Stop the cluster actor when done
  khepri_gleam_cluster.stop(cluster, 1000)
}

// Print cluster node status
fn print_node_status(
  cluster: process.Subject(khepri_gleam_cluster.ClusterMessage),
) {
  io.println("\nCluster Status:")

  // Get node list
  case khepri_gleam_cluster.list_nodes(cluster, 2000) {
    Ok(nodes) -> {
      io.println("Connected nodes: " <> string.inspect(nodes))
    }
    Error(_) -> {
      io.println("Failed to get node list")
    }
  }

  // Get cluster status
  case khepri_gleam_cluster.get_status(cluster, 2000) {
    Ok(status) -> {
      io.println("Current node: " <> status.node)
      io.println("Is leader: " <> string.inspect(status.is_leader))
    }
    Error(_) -> {
      io.println("Failed to get cluster status")
    }
  }
}
