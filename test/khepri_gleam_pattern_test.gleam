// test/khepri_gleam_pattern_test.gleam - simplified version
import gleam/io
import gleam/list
import gleam/string
import gleeunit
import khepri_gleam
import test_helper

pub fn pattern_matching_test() {
  // Start Khepri and clear any previous data
  test_helper.section("Pattern Matching Tests")
  khepri_gleam.start()
  let _ = khepri_gleam.clear_all()
  test_helper.assert_pass("Khepri started successfully", True)

  // Set up test data
  setup_test_data()

  // Test basic name pattern matching
  test_helper.subsection("Testing name pattern matching")
  test_name_patterns()

  // Test simple data matching
  test_helper.subsection("Testing simple data matching")
  test_simple_data_matching()

  // Test path manipulation
  test_helper.subsection("Testing path navigation")
  test_path_navigation()

  // Clean up
  let _ = khepri_gleam.clear_all()
  test_helper.section("Pattern Matching Tests Completed")
}

fn setup_test_data() {
  io.println("Setting up pattern matching test data...")

  // Create a nested data structure - simplified for testing
  khepri_gleam.put(["store", "fruits", "apple"], #("color", "red", "price", 1.5))

  khepri_gleam.put(["store", "fruits", "banana"], #(
    "color",
    "yellow",
    "price",
    0.8,
  ))

  io.println("Test data setup complete")
}

fn test_name_patterns() {
  // Test direct exists check first to validate test data
  let apple_exists = khepri_gleam.exists_raw(["store", "fruits", "apple"])
  test_helper.assert_equal("Apple should exist directly", apple_exists, True)

  // Test exact name pattern matching
  let name_pattern = [
    #("store", khepri_gleam.is("store")),
    #("fruits", khepri_gleam.is("fruits")),
    #("apple", khepri_gleam.is("apple")),
  ]
  let exists_result = khepri_gleam.exists_pattern(name_pattern)
  test_helper.assert_equal(
    "Apple should exist with pattern",
    exists_result,
    True,
  )

  // Test non-existent path
  let missing_pattern = [
    #("store", khepri_gleam.is("store")),
    #("fruits", khepri_gleam.is("fruits")),
    #("missing", khepri_gleam.is("missing")),
  ]
  let missing_result = khepri_gleam.exists_pattern(missing_pattern)
  test_helper.assert_equal(
    "Missing fruit should not exist",
    missing_result,
    False,
  )
}

fn test_simple_data_matching() {
  // Instead of combined conditions, test simple data matching first
  // First verify that the path exists via basic API
  let get_result = khepri_gleam.get_raw(["store", "fruits", "banana"])
  test_helper.check_ok("Should get banana directly", get_result)

  // Now test if we can match at all using a simple condition
  let banana_pattern = [
    #("store", khepri_gleam.is("store")),
    #("fruits", khepri_gleam.is("fruits")),
    #("banana", khepri_gleam.is("banana")),
  ]
  let exists_result = khepri_gleam.exists_pattern(banana_pattern)
  test_helper.assert_equal(
    "Banana should exist with pattern",
    exists_result,
    True,
  )

  // Test get_pattern to check if we can retrieve data
  let get_pattern_result = khepri_gleam.get_pattern(banana_pattern)
  test_helper.check_ok("Should get banana via pattern", get_pattern_result)
}

fn test_path_navigation() {
  // Test list_children function which is more basic than pattern matching
  let list_result = khepri_gleam.list_directory("/:store/fruits")
  test_helper.check_ok("Should list fruits", list_result)

  case list_result {
    Ok(items) -> {
      test_helper.assert_equal(
        "Should find fruits",
        list.length(items) >= 2,
        True,
      )
      io.println("Found fruits: " <> string.inspect(items))
    }
    Error(_) -> {
      test_helper.assert_pass("Should find fruits", False)
    }
  }

  // Test delete with a specific path
  khepri_gleam.delete(["store", "fruits", "apple"])
  let apple_deleted = !khepri_gleam.exists_raw(["store", "fruits", "apple"])
  test_helper.assert_equal("Apple should be deleted", apple_deleted, True)
}
