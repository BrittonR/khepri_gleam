// src/khepri_gleam.gleam
import gleam/dynamic
import gleam/dynamic/decode
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/result
import gleam/string

// External function declarations with correct return types

@external(erlang, "khepri_gleam_helper", "get_children_direct")
pub fn get_children_direct(
  path: List(String),
) -> Result(dynamic.Dynamic, dynamic.Dynamic)

@external(erlang, "khepri", "start")
pub fn start() -> Nil

// @external(erlang, "khepri", "put")
// pub fn put(path: List(String), data: a) -> Nil
@external(erlang, "khepri_gleam_helper", "put_with_path")
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

// Transaction support - external function declarations
@external(erlang, "khepri_gleam_helper", "do_transaction_put")
fn do_transaction_put(
  path: List(String),
  data: dynamic.Dynamic,
) -> Result(dynamic.Dynamic, dynamic.Dynamic)

@external(erlang, "khepri_gleam_helper", "do_transaction_get")
fn do_transaction_get(
  path: List(String),
) -> Result(dynamic.Dynamic, dynamic.Dynamic)

@external(erlang, "khepri_gleam_helper", "do_transaction_delete")
fn do_transaction_delete(
  path: List(String),
) -> Result(dynamic.Dynamic, dynamic.Dynamic)

@external(erlang, "khepri_gleam_helper", "do_transaction_exists")
fn do_transaction_exists(
  path: List(String),
) -> Result(dynamic.Dynamic, dynamic.Dynamic)

// Import/Export support - external function declarations
@external(erlang, "khepri_gleam_helper", "export_data")
pub fn export_raw(
  path: List(String),
  callback_module: String,
  filename: String,
) -> Result(dynamic.Dynamic, dynamic.Dynamic)

@external(erlang, "khepri_gleam_helper", "import_data")
pub fn import_raw(
  callback_module: String,
  filename: String,
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
      // Use decode.run with decode.string decoder
      case decode.run(value, decode.string) {
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
      // Use decode.run with decode.int decoder
      case decode.run(value, decode.int) {
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
      // Use decode.run with decode.bool decoder
      case decode.run(value, decode.bool) {
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
      // Use decode.run with the appropriate decoder
      let decoder = decode.list(decode.tuple2(decode.string, decode.dynamic))
      case decode.run(children, decoder) {
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

pub fn list_directory(
  base_path: String,
) -> Result(List(#(String, dynamic.Dynamic)), String) {
  let path_list = to_khepri_path(base_path)

  // For non-existent paths, just return an empty list
  case exists_raw(path_list) {
    False -> {
      io.println("Path does not exist, returning empty list: " <> base_path)
      Ok([])
      // Return empty list instead of error
    }
    True -> {
      io.println("Getting children for path: " <> string.inspect(path_list))

      case get_children_direct(path_list) {
        Ok(children) -> {
          io.println("Raw children result: " <> string.inspect(children))

          // Parse the result with proper decoder
          let decoder =
            decode.list(decode.tuple2(decode.string, decode.dynamic))
          case decode.run(children, decoder) {
            Ok(items) -> Ok(items)
            Error(_) -> Error("Failed to decode children list")
          }
        }
        Error(_) -> Error("Failed to get children")
      }
    }
  }
}

// Transaction operation wrappers
pub fn tx_put_path(path: String, data: a) -> Result(Nil, String) {
  let path_list = to_khepri_path(path)
  case do_transaction_put(path_list, dynamic.from(data)) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("Failed to put in transaction")
  }
}

pub fn tx_get_path(path: String) -> Result(dynamic.Dynamic, String) {
  let path_list = to_khepri_path(path)
  case do_transaction_get(path_list) {
    Ok(value) -> Ok(value)
    Error(_) -> Error("Failed to get in transaction")
  }
}

pub fn tx_delete_path(path: String) -> Result(Nil, String) {
  let path_list = to_khepri_path(path)
  case do_transaction_delete(path_list) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("Failed to delete in transaction")
  }
}

pub fn tx_exists_path(path: String) -> Result(Bool, String) {
  let path_list = to_khepri_path(path)
  case do_transaction_exists(path_list) {
    Ok(result) -> {
      // Use decode.run with decode.bool decoder
      case decode.run(result, decode.bool) {
        Ok(exists) -> Ok(exists)
        Error(_) -> Error("Failed to decode boolean result")
      }
    }
    Error(_) -> Error("Failed to check exists in transaction")
  }
}

/// Export data from the Khepri database to a file
///
/// ## Parameters
/// - `path`: The path to export from (use "/" for the entire database)
/// - `filename`: The filename to export to
///
/// ## Returns
/// - `Ok(Nil)` if the export was successful
/// - `Error(String)` with the error message if the export failed
pub fn export(path: String, filename: String) -> Result(Nil, String) {
  let path_list = to_khepri_path(path)

  // Use the Erlang export module for serialization
  case export_raw(path_list, "khepri_export_erlang", filename) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("Failed to export data to " <> filename)
  }
}

/// Import data from a file into the Khepri database
///
/// Note: This will not clear existing data. If you want to replace all data,
/// you should manually clear the database first.
///
/// ## Parameters
/// - `filename`: The filename to import from
///
/// ## Returns
/// - `Ok(Nil)` if the import was successful
/// - `Error(String)` with the error message if the import failed
pub fn import_from_file(filename: String) -> Result(Nil, String) {
  // Use the Erlang import module for deserialization
  case import_raw("khepri_export_erlang", filename) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("Failed to import data from " <> filename)
  }
}

// /// Clear the entire database
// ///
// /// This is useful before importing data if you want to replace all existing data.
// ///
// /// ## Returns
// /// - `Ok(Nil)` if the clear was successful
// /// - `Error(String)` with the error message if the clear failed
// pub fn clear_all() -> Result(Nil, String) {
//   // Delete the root node to clear everything
//   delete([])

//   // Check if the database is empty
//   case list_children("/") {
//     Ok(children) ->
//       case children {
//         [] -> Ok(Nil)
//         _ -> Error("Failed to clear all data, some nodes still exist")
//       }
//     Error(_) -> Error("Failed to verify database clearing")
//   }
// }
@external(erlang, "khepri_gleam_helper", "clear_all")
pub fn clear_all() -> Result(Nil, String)
