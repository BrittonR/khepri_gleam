// src/khepri_pattern.gleam
// Enhanced pattern matching module for Khepri
// Leveraging Khepri's advanced pattern matching capabilities

import gleam/dynamic
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import khepri_gleam

// Re-export the basic types from khepri_gleam
pub type NodeCondition =
  khepri_gleam.NodeCondition

pub type CompareOp =
  khepri_gleam.CompareOp

// Constants for wildcard patterns
// These will be converted to the appropriate Erlang terms
pub const wildcard_star = "KHEPRI_WILDCARD_STAR"

pub const wildcard_star_star = "KHEPRI_WILDCARD_STAR_STAR"

// ----- NODE CONDITION BUILDERS -----

// Basic equality conditions
pub fn is(name: String) -> NodeCondition {
  khepri_gleam.is(name)
}

pub fn any() -> NodeCondition {
  khepri_gleam.any()
}

// Data matching conditions
pub fn data_matches(pattern: a) -> NodeCondition {
  khepri_gleam.DataMatches(dynamic.from(pattern))
}

// Child count conditions
pub fn child_count(count: Int, op: CompareOp) -> NodeCondition {
  khepri_gleam.ChildCount(count, op)
}

// Combined conditions
pub fn all(conditions: List(NodeCondition)) -> NodeCondition {
  khepri_gleam.All(conditions)
}

pub fn any_of(conditions: List(NodeCondition)) -> NodeCondition {
  khepri_gleam.AnyOf(conditions)
}

// Existence conditions
@external(erlang, "khepri_pattern_erlang", "if_node_exists")
pub fn if_node_exists(exists: Bool) -> NodeCondition

// Data match conditions with match patterns
@external(erlang, "khepri_pattern_erlang", "if_data_matches_pattern")
pub fn if_data_matches_pattern(
  pattern: dynamic.Dynamic,
  conditions: List(dynamic.Dynamic),
) -> NodeCondition

// Match on node's payload version
@external(erlang, "khepri_pattern_erlang", "if_payload_version")
pub fn if_payload_version(version: Int, op: CompareOp) -> NodeCondition

// Match on node's child list version
@external(erlang, "khepri_pattern_erlang", "if_child_list_version")
pub fn if_child_list_version(version: Int, op: CompareOp) -> NodeCondition

// Match on child list length 
@external(erlang, "khepri_pattern_erlang", "if_child_list_length")
pub fn if_child_list_length(count: Int, op: CompareOp) -> NodeCondition

// Negate a condition
@external(erlang, "khepri_pattern_erlang", "if_not")
pub fn if_not(condition: NodeCondition) -> NodeCondition

// ----- WILDCARD PATTERNS -----

// Create a path component that matches any single node
@external(erlang, "khepri_pattern_erlang", "wildcard_star")
pub fn wildcard_star_component() -> dynamic.Dynamic

// Create a path component that matches any number of nodes recursively
@external(erlang, "khepri_pattern_erlang", "wildcard_star_star")
pub fn wildcard_star_star_component() -> dynamic.Dynamic

// ----- SINGLE-NODE OPERATIONS -----

// Check if a pattern exists
pub fn exists(path: List(#(String, NodeCondition))) -> Bool {
  khepri_gleam.exists_pattern(path)
}

// Get value matching a pattern
pub fn get(
  path: List(#(String, NodeCondition)),
) -> Result(dynamic.Dynamic, String) {
  khepri_gleam.get_pattern(path)
}

// Delete node matching a pattern
pub fn delete(path: List(#(String, NodeCondition))) -> Nil {
  khepri_gleam.delete_pattern_raw(khepri_gleam.to_pattern_path(path))
}

// Check if node at path has data
@external(erlang, "khepri_pattern_erlang", "has_data")
pub fn has_data(path: List(String)) -> Bool

// ----- MULTI-NODE OPERATIONS -----

// Get multiple nodes matching a pattern
@external(erlang, "khepri_pattern_erlang", "get_many")
pub fn get_many(path: List(String)) -> Result(dynamic.Dynamic, String)

// Delete multiple nodes matching a pattern
@external(erlang, "khepri_pattern_erlang", "delete_many")
pub fn delete_many(path: List(String)) -> Nil

// Count nodes matching a pattern 
@external(erlang, "khepri_pattern_erlang", "count")
pub fn count(path: List(String)) -> Int

// ----- COMPARE-AND-SWAP OPERATIONS -----

// Updates a node only if its data matches a pattern
@external(erlang, "khepri_pattern_erlang", "compare_and_swap")
pub fn compare_and_swap(
  path: List(String),
  data_pattern: dynamic.Dynamic,
  new_data: a,
) -> Result(dynamic.Dynamic, String)

// ----- PATH HELPERS -----

// Convert a Unix-style path to a pattern
pub fn path_to_pattern(path: String) -> List(#(String, NodeCondition)) {
  let components = khepri_gleam.to_khepri_path(path)

  // Convert to pattern format with exact name conditions
  list.map(components, fn(component) { #(component, is(component)) })
}

// Create a pattern with the wildcard_star at a specific position
@external(erlang, "khepri_pattern_erlang", "path_with_star")
pub fn path_with_star(path: List(String), position: Int) -> dynamic.Dynamic

// Create a pattern with the wildcard_star_star at a specific position
@external(erlang, "khepri_pattern_erlang", "path_with_star_star")
pub fn path_with_star_star(path: List(String), position: Int) -> dynamic.Dynamic

// ----- CONVENIENCE FUNCTIONS -----

// Check if a node has exactly N children
pub fn has_exact_children(path: List(String), count: Int) -> Bool {
  // Convert the path to a pattern format
  let path_pattern =
    list.map(list.take(path, list.length(path) - 1), fn(component) {
      #(component, is(component))
    })

  // Add the last component with the child_count condition
  let pattern = case list.last(path) {
    Ok(last) ->
      list.append(path_pattern, [
        #(last, child_count(count, khepri_gleam.Equal)),
      ])
    Error(_) -> []
  }

  exists(pattern)
}

// Check if a node has children in a range
pub fn has_children_in_range(path: List(String), min: Int, max: Int) -> Bool {
  // Create the range condition
  let range_condition =
    all([
      child_count(min, khepri_gleam.GreaterThanOrEqual),
      child_count(max, khepri_gleam.LessThanOrEqual),
    ])

  // Convert the path to a pattern format
  let path_pattern =
    list.map(list.take(path, list.length(path) - 1), fn(component) {
      #(component, is(component))
    })

  // Add the last component with the range condition
  let pattern = case list.last(path) {
    Ok(last) -> list.append(path_pattern, [#(last, range_condition)])
    Error(_) -> []
  }

  exists(pattern)
}

// Find all nodes with a specific attribute value
@external(erlang, "khepri_pattern_erlang", "find_with_attribute")
pub fn find_with_attribute(
  path: List(String),
  attr_name: String,
  attr_value: a,
) -> Result(dynamic.Dynamic, String)
