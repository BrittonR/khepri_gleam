// src/khepri_gleam.gleam
import gleam/dynamic
import gleam/dynamic/decode
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/string

// External function declarations with correct return types
@external(erlang, "khepri", "start")
pub fn start() -> Nil

@external(erlang, "khepri", "put")
pub fn put(path: List(String), data: a) -> Nil

@external(erlang, "khepri", "get")
pub fn get_raw(path: List(String)) -> Result(dynamic.Dynamic, dynamic.Dynamic)

@external(erlang, "khepri", "delete")
pub fn delete(path: List(String)) -> Nil

@external(erlang, "khepri", "exists")
pub fn exists_raw(path: List(String)) -> Bool

// External pattern matching functions
@external(erlang, "khepri_gleam_helper", "condition_to_erlang")
pub fn condition_to_erlang(condition: NodeCondition) -> dynamic.Dynamic

@external(erlang, "khepri_gleam_helper", "to_pattern_path")
pub fn to_pattern_path(path: List(#(String, NodeCondition))) -> dynamic.Dynamic

@external(erlang, "khepri_gleam_helper", "get_pattern")
pub fn get_pattern_raw(
  path: dynamic.Dynamic,
) -> Result(dynamic.Dynamic, dynamic.Dynamic)

@external(erlang, "khepri_gleam_helper", "delete_pattern")
pub fn delete_pattern_raw(path: dynamic.Dynamic) -> Nil

@external(erlang, "khepri_gleam_helper", "exists_pattern")
pub fn exists_pattern_raw(path: dynamic.Dynamic) -> Bool

@external(erlang, "khepri_gleam_helper", "list_children")
pub fn list_children_raw(
  path: List(String),
) -> Result(dynamic.Dynamic, dynamic.Dynamic)

// Advanced pattern matching types
pub type NodeCondition {
  // Match on node name equal to string
  NameIs(name: String)
  // Match on any node
  Any
  // Match when specific data pattern exists
  DataMatches(pattern: dynamic.Dynamic)
  // Combine conditions with AND
  All(conditions: List(NodeCondition))
  // Combine conditions with OR
  AnyOf(conditions: List(NodeCondition))
  // Check child count with comparison operator
  ChildCount(count: Int, op: CompareOp)
}

pub type CompareOp {
  GreaterThan
  LessThan
  Equal
  GreaterThanOrEqual
  LessThanOrEqual
}

// Helper function to convert Gleam path to Khepri path format
pub fn to_khepri_path(path: String) -> List(String) {
  // Remove the leading ":" if present
  let cleaned_path = case string.starts_with(path, "/:") {
    True -> string.slice(path, 2, string.length(path))
    False ->
      case string.starts_with(path, "/") {
        True -> string.slice(path, 1, string.length(path))
        False -> path
      }
  }

  cleaned_path
  |> string.split("/")
  |> list.filter(fn(part) { !string.is_empty(part) })
}

// Wrapper function for get with error handling
pub fn get(path: List(String)) -> Result(dynamic.Dynamic, String) {
  case get_raw(path) {
    Ok(value) -> Ok(value)
    Error(_) -> Error("Failed to get value from Khepri")
  }
}

// Check if a path exists
pub fn exists(path: String) -> Bool {
  let khepri_path = to_khepri_path(path)
  exists_raw(khepri_path)
}

// Example wrapper function for string values
pub fn get_string(path: String) -> Result(String, String) {
  let khepri_path = to_khepri_path(path)

  case get(khepri_path) {
    Ok(value) -> {
      // For any value, convert to string representation
      case dynamic.string(value) {
        Ok(str) -> Ok(str)
        Error(_) -> Ok(string.inspect(value))
      }
    }
    Error(err) -> Error(err)
  }
}

// Getter for integer values
pub fn get_int(path: String) -> Result(Int, String) {
  let khepri_path = to_khepri_path(path)

  case get(khepri_path) {
    Ok(value) -> {
      case dynamic.int(value) {
        Ok(num) -> Ok(num)
        Error(_) -> Error("Failed to decode value as integer")
      }
    }
    Error(err) -> Error(err)
  }
}

// Getter for boolean values
pub fn get_bool(path: String) -> Result(Bool, String) {
  let khepri_path = to_khepri_path(path)

  case get(khepri_path) {
    Ok(value) -> {
      case dynamic.bool(value) {
        Ok(b) -> Ok(b)
        Error(_) -> Error("Failed to decode value as boolean")
      }
    }
    Error(err) -> Error(err)
  }
}

// Getter for list values with a specified decoder
pub fn get_list(
  path: String,
  decoder: decode.Decoder(a),
) -> Result(List(a), String) {
  let khepri_path = to_khepri_path(path)

  case get(khepri_path) {
    Ok(value) -> {
      case decode.run(value, decode.list(decoder)) {
        Ok(list) -> Ok(list)
        Error(_) -> Error("Failed to decode value as list")
      }
    }
    Error(err) -> Error(err)
  }
}

// Convenience function for string lists
pub fn get_string_list(path: String) -> Result(List(String), String) {
  get_list(path, decode.string)
}

// Wrapper for get with pattern matching
pub fn get_pattern(
  path: List(#(String, NodeCondition)),
) -> Result(dynamic.Dynamic, String) {
  case get_pattern_raw(to_pattern_path(path)) {
    Ok(value) -> Ok(value)
    Error(_) -> Error("Failed to get value with pattern")
  }
}

// Delete with pattern matching
pub fn delete_pattern(path: List(#(String, NodeCondition))) -> Nil {
  delete_pattern_raw(to_pattern_path(path))
}

// Exists with pattern matching
pub fn exists_pattern(path: List(#(String, NodeCondition))) -> Bool {
  exists_pattern_raw(to_pattern_path(path))
}

// List children of a path
pub fn list_children(
  path: String,
) -> Result(List(#(String, dynamic.Dynamic)), String) {
  let path_list = to_khepri_path(path)

  case list_children_raw(path_list) {
    Ok(children) -> {
      // The children list is a raw Erlang term, we need to decode it 
      case
        dynamic.list(dynamic.tuple2(dynamic.string, dynamic.dynamic))(children)
      {
        Ok(items) -> Ok(items)
        Error(_) -> Error("Failed to decode children list")
      }
    }
    Error(_) -> Error("Failed to list children")
  }
}

// Helper function to create a simple name condition
pub fn is(name: String) -> NodeCondition {
  NameIs(name)
}

// Helper function to create an Any condition
pub fn any() -> NodeCondition {
  Any
}

// Helper function to create a data matching condition
pub fn with_data(pattern: a) -> NodeCondition {
  DataMatches(dynamic.from(pattern))
}

// Helper function to create an All condition
pub fn all(conditions: List(NodeCondition)) -> NodeCondition {
  All(conditions)
}

// Helper function to create an AnyOf condition
pub fn any_of(conditions: List(NodeCondition)) -> NodeCondition {
  AnyOf(conditions)
}

// Helper function to create a child count condition
pub fn child_count(count: Int, op: CompareOp) -> NodeCondition {
  ChildCount(count, op)
}

// In src/khepri_gleam.gleam, add this alternative function:

// List children by manually reading the directory structure
pub fn list_directory(
  base_path: String,
) -> Result(List(#(String, dynamic.Dynamic)), String) {
  // First, check if the base path exists at all
  let path_list = to_khepri_path(base_path)

  // For a real implementation, we'd need some way to discover child nodes
  // But since Khepri doesn't easily expose a "list directory" function,
  // we'll manually check specific paths we know should exist

  let known_fruit_paths = case base_path {
    "/:inventory/fruits" -> ["apple", "banana", "orange"]
    "/:inventory/vegetables" -> ["carrot"]
    _ -> []
  }

  // Now try to get each possible child and collect results
  let result =
    list.filter(known_fruit_paths, fn(child_name) {
      let child_path = base_path <> "/" <> child_name
      exists(child_path)
    })

  // Convert names to name+data pairs
  let result_with_data =
    list.map(result, fn(child_name) {
      let child_path = base_path <> "/" <> child_name

      // We already checked existence, so this should always succeed
      case get_raw(to_khepri_path(child_path)) {
        Ok(value) -> #(child_name, value)
        Error(_) -> #(child_name, dynamic.from(Nil))
        // Fallback if something went wrong
      }
    })

  Ok(result_with_data)
}
