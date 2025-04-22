// simple_cluster_test.gleam
// A simplified test for Khepri clustering
import gleam/dynamic
import gleam/erlang/process
import gleam/io
import gleam/string
import khepri_gleam

// External function declarations for direct Erlang access
@external(erlang, "khepri", "start")
fn khepri_start() -> Result(dynamic.Dynamic, dynamic.Dynamic)

@external(erlang, "khepri_cluster", "join")
fn khepri_join(node: String) -> Result(dynamic.Dynamic, dynamic.Dynamic)

@external(erlang, "khepri_cluster", "members")
fn khepri_members() -> Result(dynamic.Dynamic, dynamic.Dynamic)

@external(erlang, "khepri", "put")
fn khepri_put(
  path: List(String),
  data: String,
) -> Result(dynamic.Dynamic, dynamic.Dynamic)

@external(erlang, "khepri", "get")
fn khepri_get(path: List(String)) -> Result(dynamic.Dynamic, dynamic.Dynamic)

// Additional Erlang functions needed
@external(erlang, "erlang", "node")
fn erlang_node() -> String

// Helper functions
@external(erlang, "simple_cluster_test_helper", "ping_node")
fn ping_node(node: String) -> Bool

@external(erlang, "simple_cluster_test_helper", "join_cluster")
fn join_cluster(node: String) -> Result(dynamic.Dynamic, dynamic.Dynamic)

pub fn main() {
  io.println("\n=== Simple Khepri Cluster Test ===\n")

  // Check if we're running as a distributed Erlang node
  let node_name = erlang_node()
  io.println("Running on node: " <> node_name)

  // Start Khepri directly using Erlang module
  io.println("\nStarting Khepri...")
  let start_result = khepri_start()
  io.println("Start result: " <> string.inspect(start_result))

  // Check if this is the second node
  case string.contains(node_name, "node2") {
    True -> {
      io.println("\nThis is node2, attempting to join node1...")

      // Try to ping node1 first
      let remote_node = "node1@127.0.0.1"
      let ping_result = ping_node(remote_node)
      io.println(
        "Ping to " <> remote_node <> " result: " <> string.inspect(ping_result),
      )

      // Now try to join
      io.println("Joining cluster...")
      let join_result = join_cluster(remote_node)
      io.println("Join result: " <> string.inspect(join_result))
    }
    False -> {
      io.println("\nThis is " <> node_name <> ", not attempting to join.")
    }
  }

  // Get cluster members
  io.println("\nGetting cluster members...")
  let members_result = khepri_members()
  io.println("Members: " <> string.inspect(members_result))

  // Test data operations
  io.println("\nTesting data operations...")

  // Create a simple path
  let path = ["test", "data"]

  // Store some data
  let value = "Data from " <> node_name
  io.println("Storing: " <> value)
  let put_result = khepri_put(path, value)
  io.println("Put result: " <> string.inspect(put_result))

  // Wait a moment for replication
  process.sleep(1000)

  // Read the data back
  io.println("Reading data...")
  let get_result = khepri_get(path)
  io.println("Get result: " <> string.inspect(get_result))

  // Keep the process running
  io.println("\nTest complete. Node will remain running.")
  io.println("Press Ctrl+C to exit.")
  process.sleep_forever()
}
