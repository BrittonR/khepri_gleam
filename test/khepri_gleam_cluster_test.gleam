// test/khepri_gleam_cluster_test.gleam
import gleam/io
import gleam/result
import gleam/string
import khepri_gleam
import khepri_gleam_cluster
import test_helper

pub fn cluster_basics_test() {
  test_helper.section("Testing Khepri Clustering Basics")

  // Start Khepri using the regular start function
  khepri_gleam.start()
  io.println("Started Khepri")

  // Verify we can run a basic operation
  test_helper.subsection("Testing basic operations")
  let path = "/:test/clustering"
  khepri_gleam.put(khepri_gleam.to_khepri_path(path), "works")

  let get_result = khepri_gleam.get_string(path)
  test_helper.check_ok("Should be able to get stored value", get_result)

  case get_result {
    Ok(value) -> {
      test_helper.assert_equal("Value should be 'works'", value, "works")
    }
    Error(_) -> Nil
  }

  // Test starting the cluster actor
  test_helper.subsection("Testing cluster actor")

  // Start the cluster actor
  let cluster_result = khepri_gleam_cluster.start()
  test_helper.check_ok("Starting cluster actor", cluster_result)

  case cluster_result {
    Ok(cluster) -> {
      // Check if we can list nodes (avoid get_status since it uses is_leader)
      let nodes_result = khepri_gleam_cluster.list_nodes(cluster, 1000)
      test_helper.check_ok("Listing cluster nodes", nodes_result)

      // Stop the cluster actor
      let stop_result = khepri_gleam_cluster.stop(cluster, 1000)
      test_helper.check_ok("Stopping cluster actor", stop_result)
    }
    Error(_) -> Nil
  }

  // Clean up
  khepri_gleam.delete(khepri_gleam.to_khepri_path(path))

  io.println(
    "Note: Full cluster functionality requires a real distributed Erlang environment.",
  )
  io.println(
    "The khepri_gleam_cluster module provides the following functions:",
  )
  io.println("- join: Join an existing cluster")
  io.println("- leave: Leave a cluster")
  io.println("- list_nodes: List cluster nodes")
  io.println("- get_status: Get cluster status")
  io.println("- wait_for_leader: Wait for leader election")

  test_helper.section("Khepri Clustering Basics Test Completed")
}
