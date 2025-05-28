//// Khepri clustering functionality for Gleam applications.
////
//// This module provides functions for connecting Khepri nodes into a cluster,
//// allowing replication of data between nodes.

import gleam/erlang/atom
import gleam/erlang/node
import gleam/erlang/process.{type Subject}
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/string

/// Error that can occur when connecting to a node
pub type NodeConnectError {
  NodeConnectError(
    /// The node name that failed to connect
    node: String,
    /// The error message
    error: String,
  )
}

/// Status of the cluster
pub type ClusterStatus {
  ClusterStatus(
    /// The current node name
    node: String,
    /// List of connected nodes in the cluster
    connected_nodes: List(String),
    /// Whether this node is the leader
    is_leader: Bool,
  )
}

/// Messages handled by the cluster actor
pub opaque type ClusterMessage {
  Join(node: String, client: Subject(Result(Nil, NodeConnectError)))
  Leave(client: Subject(Result(Nil, String)))
  ListNodes(client: Subject(Result(List(String), String)))
  GetMembers(client: Subject(Result(List(String), String)))
  GetStatus(client: Subject(Result(ClusterStatus, String)))
  Stop(client: Subject(Nil))
  // Changed: client expects simple Nil, not a Result
}

/// A simple logger type
pub type Logger =
  fn(String, String) -> Nil

/// The main cluster actor state
type ClusterState {
  ClusterState(
    self: Subject(ClusterMessage),
    log_prefix: String,
    logger: Logger,
  )
}

/// External function declarations for Khepri cluster management
@external(erlang, "khepri_gleam_cluster_helper", "join_cluster")
fn join_cluster_raw(node: String) -> Result(Nil, String)

@external(erlang, "khepri_gleam_cluster_helper", "join_cluster_with_timeout")
fn join_cluster_with_timeout(node: String, timeout: Int) -> Result(Nil, String)

@external(erlang, "khepri_gleam_cluster_helper", "reset_cluster")
fn reset_cluster_raw() -> Result(Nil, String)

@external(erlang, "khepri_gleam_cluster_helper", "reset_cluster_with_timeout")
fn reset_cluster_with_timeout(timeout: Int) -> Result(Nil, String)

@external(erlang, "khepri_gleam_cluster_helper", "get_cluster_members")
fn get_cluster_members_raw() -> Result(List(String), String)

@external(erlang, "khepri_gleam_cluster_helper", "get_cluster_nodes")
fn get_cluster_nodes_raw() -> Result(List(String), String)

@external(erlang, "khepri_gleam_cluster_helper", "wait_for_leader")
fn wait_for_leader_raw() -> Result(Nil, String)

@external(erlang, "khepri_gleam_cluster_helper", "wait_for_leader_with_timeout")
fn wait_for_leader_with_timeout_raw(timeout: Int) -> Result(Nil, String)

/// Store management
@external(erlang, "khepri_gleam_cluster_helper", "start_store")
pub fn start_store() -> Result(String, String)

@external(erlang, "khepri_gleam_cluster_helper", "start_store_with_name")
pub fn start_store_with_name(name_or_path: String) -> Result(String, String)

@external(erlang, "khepri_gleam_cluster_helper", "start_store_with_path")
pub fn start_store_with_path(
  path: String,
  store_id: String,
) -> Result(String, String)

@external(erlang, "khepri_gleam_cluster_helper", "start_store_with_timeout")
pub fn start_store_with_timeout(
  path: String,
  store_id: String,
  timeout: Int,
) -> Result(String, String)

@external(erlang, "khepri_gleam_cluster_helper", "stop_store")
pub fn stop_store() -> Result(Nil, String)

@external(erlang, "khepri_gleam_cluster_helper", "stop_store_with_name")
pub fn stop_store_with_name(store_id: String) -> Result(Nil, String)

/// Start a Khepri cluster actor
///
/// ## Returns
/// - `Ok(Subject)` with the cluster actor's subject if successful
/// - `Error(actor.StartError)` if the actor fails to start
pub fn start() -> Result(Subject(ClusterMessage), actor.StartError) {
  start_with_logger(default_logger("[khepri_cluster]"))
}

/// Start a Khepri cluster actor with a custom logger
///
/// ## Parameters
/// - `logger`: A custom logging function
///
/// ## Returns
/// - `Ok(Subject)` with the cluster actor's subject if successful
/// - `Error(actor.StartError)` if the actor fails to start
pub fn start_with_logger(
  logger: Logger,
) -> Result(Subject(ClusterMessage), actor.StartError) {
  actor.start(
    ClusterState(
      self: process.new_subject(),
      log_prefix: "[khepri_cluster]",
      logger: logger,
    ),
    handle_message,
  )
}

/// Join a remote Khepri cluster
///
/// ## Parameters
/// - `cluster`: The cluster actor's subject
/// - `node`: The remote node to join (e.g., "node1@127.0.0.1")
/// - `timeout`: Timeout in milliseconds
///
/// ## Returns
/// - `Ok(Nil)` if joining was successful
/// - `Error(NodeConnectError)` if joining failed
pub fn join(
  cluster: Subject(ClusterMessage),
  node: String,
  timeout: Int,
) -> Result(Nil, NodeConnectError) {
  case process.try_call(cluster, fn(client) { Join(node, client) }, timeout) {
    Ok(result) -> result
    Error(_) ->
      Error(NodeConnectError(
        node: node,
        error: "Call to cluster actor timed out",
      ))
  }
}

/// Leave the Khepri cluster
///
/// ## Parameters
/// - `cluster`: The cluster actor's subject
/// - `timeout`: Timeout in milliseconds
///
/// ## Returns
/// - `Ok(Nil)` if leaving was successful
/// - `Error(String)` with error message if leaving failed
pub fn leave(
  cluster: Subject(ClusterMessage),
  timeout: Int,
) -> Result(Nil, String) {
  case process.try_call(cluster, fn(client) { Leave(client) }, timeout) {
    Ok(result) -> result
    Error(_) -> Error("Call to cluster actor timed out")
  }
}

/// List all nodes in the Khepri cluster
///
/// ## Parameters
/// - `cluster`: The cluster actor's subject
/// - `timeout`: Timeout in milliseconds
///
/// ## Returns
/// - `Ok(List(String))` list of node names in the cluster
/// - `Error(String)` if the call failed
pub fn list_nodes(
  cluster: Subject(ClusterMessage),
  timeout: Int,
) -> Result(List(String), String) {
  case process.try_call(cluster, fn(client) { ListNodes(client) }, timeout) {
    Ok(result) -> result
    Error(_) -> Error("Call to cluster actor timed out")
  }
}

/// Get members of the Khepri cluster
///
/// ## Parameters
/// - `cluster`: The cluster actor's subject
/// - `timeout`: Timeout in milliseconds
///
/// ## Returns
/// - `Ok(List(String))` list of member names in the cluster
/// - `Error(String)` if the call failed
pub fn list_members(
  cluster: Subject(ClusterMessage),
  timeout: Int,
) -> Result(List(String), String) {
  case process.try_call(cluster, fn(client) { GetMembers(client) }, timeout) {
    Ok(result) -> result
    Error(_) -> Error("Call to cluster actor timed out")
  }
}

/// Get the status of the Khepri cluster
///
/// ## Parameters
/// - `cluster`: The cluster actor's subject
/// - `timeout`: Timeout in milliseconds
///
/// ## Returns
/// - `Ok(ClusterStatus)` with cluster status information
/// - `Error(String)` if the call failed
pub fn get_status(
  cluster: Subject(ClusterMessage),
  timeout: Int,
) -> Result(ClusterStatus, String) {
  case process.try_call(cluster, fn(client) { GetStatus(client) }, timeout) {
    Ok(result) -> result
    Error(_) -> Error("Call to cluster actor timed out")
  }
}

/// Stop the cluster actor
///
/// ## Parameters
/// - `cluster`: The cluster actor's subject
/// - `timeout`: Timeout in milliseconds
///
/// ## Returns
/// - `Ok(Nil)` if stopping was successful
/// - `Error(process.CallError)` if the call failed
pub fn stop(
  cluster: Subject(ClusterMessage),
  timeout: Int,
) -> Result(Nil, process.CallError(Nil)) {
  // This now directly returns the expected type
  process.try_call(cluster, Stop, timeout)
}

fn validate_node_name(node_name: String) -> Result(String, String) {
  case string.contains(node_name, "@") {
    True -> Ok(node_name)
    False -> Error("Invalid node name format: missing '@' character")
  }
}

/// Wait for a leader to be elected in the cluster
///
/// ## Parameters
/// - `timeout`: Timeout in milliseconds
///
/// ## Returns
/// - `Ok(Nil)` if a leader was elected
/// - `Error(String)` with an error message if waiting failed
pub fn wait_for_leader(timeout: Int) -> Result(Nil, String) {
  wait_for_leader_with_timeout_raw(timeout)
}

/// Default logger function
fn default_logger(prefix: String) -> Logger {
  fn(level, msg) { io.println(prefix <> " [" <> level <> "] " <> msg) }
}

/// Handler for cluster actor messages
fn handle_message(
  message: ClusterMessage,
  state: ClusterState,
) -> actor.Next(ClusterMessage, ClusterState) {
  case message {
    Join(node_name, client) -> {
      // Log the join attempt
      state.logger("info", "Attempting to join node: " <> node_name)

      // Validate node name first
      case validate_node_name(node_name) {
        Ok(valid_name) -> {
          // Try to join the node
          case join_cluster_raw(valid_name) {
            Ok(_) -> {
              // Successfully joined
              state.logger("info", "Successfully joined node: " <> valid_name)
              process.send(client, Ok(Nil))
            }
            Error(err) -> {
              // Failed to join
              state.logger(
                "error",
                "Failed to join node: " <> valid_name <> " - " <> err,
              )
              process.send(
                client,
                Error(NodeConnectError(node: valid_name, error: err)),
              )
            }
          }
        }
        Error(err) -> {
          // Node name validation failed
          state.logger(
            "error",
            "Invalid node name: " <> node_name <> " - " <> err,
          )
          process.send(
            client,
            Error(NodeConnectError(node: node_name, error: err)),
          )
        }
      }

      // Continue with the same state
      actor.continue(state)
    }

    Leave(client) -> {
      // Log the leave attempt
      state.logger("info", "Leaving Khepri cluster")

      // Try to reset the node (leave the cluster)
      case reset_cluster_raw() {
        Ok(_) -> {
          // Successfully left
          state.logger("info", "Successfully left the cluster")
          process.send(client, Ok(Nil))
        }
        Error(err) -> {
          // Failed to leave
          state.logger("error", "Failed to leave cluster: " <> err)
          process.send(client, Error(err))
        }
      }

      // Continue with the same state
      actor.continue(state)
    }

    ListNodes(client) -> {
      // Get current nodes from Khepri
      case get_cluster_nodes_raw() {
        Ok(nodes) -> {
          // Successfully got nodes
          process.send(client, Ok(nodes))
        }
        Error(err) -> {
          // Failed to get nodes
          state.logger("error", "Failed to get nodes from Khepri: " <> err)
          process.send(client, Error(err))
        }
      }

      // Continue with unchanged state
      actor.continue(state)
    }

    GetMembers(client) -> {
      // Get current members from Khepri
      case get_cluster_members_raw() {
        Ok(members) -> {
          // Successfully got members
          process.send(client, Ok(members))
        }
        Error(err) -> {
          // Failed to get members
          state.logger("error", "Failed to get members from Khepri: " <> err)
          process.send(client, Error(err))
        }
      }

      // Continue with unchanged state
      actor.continue(state)
    }

    GetStatus(client) -> {
      // Get current nodes
      case get_cluster_nodes_raw() {
        Ok(nodes) -> {
          // Get current node name - using node.self() and converting to string
          let current_node = atom.to_string(node.to_atom(node.self()))

          // Simple (but not accurate) leader detection - assume we're leader if we're first in list
          let is_leader = case list.first(nodes) {
            Ok(first_node) -> first_node == current_node
            Error(_) -> False
          }

          // Create and send status
          let status =
            ClusterStatus(
              node: current_node,
              connected_nodes: nodes,
              is_leader: is_leader,
            )
          process.send(client, Ok(status))
        }
        Error(err) -> {
          // Failed to get nodes
          state.logger("error", "Failed to get status: " <> err)
          process.send(client, Error(err))
        }
      }

      // Continue with unchanged state
      actor.continue(state)
    }

    Stop(client) -> {
      // Log stopping
      state.logger("info", "Stopping Khepri cluster actor")

      // Notify client without wrapping in Result
      process.send(client, Nil)

      // Stop the actor
      actor.Stop(process.Normal)
    }
  }
}

/// Check if a store is running
///
/// ## Parameters
/// - `store_id`: The store ID to check
///
/// ## Returns
/// - `True` if the store is running
/// - `False` if the store is not running
@external(erlang, "khepri_gleam_cluster_helper", "is_store_running")
pub fn is_store_running(store_id: String) -> Bool
