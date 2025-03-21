// test/khepri_gleam_test.gleam
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import gleeunit
import khepri_gleam

pub fn main() {
  io.println("\n=== Running Basic Operations Test ===\n")
  basic_operations_test()

  io.println("\n=== Running Pattern Matching Test ===\n")
  pattern_matching_test()

  io.println("\n=== All Tests Completed ===\n")
}

pub fn basic_operations_test() {
  // Start Khepri
  io.println("Starting Khepri...")
  khepri_gleam.start()
  io.println("Khepri started successfully")

  // Test put and get
  let test_path = "/:test/data"
  let test_value = "hello world"

  // Convert the path
  let path = khepri_gleam.to_khepri_path(test_path)

  // Try to write data
  io.println("Writing value...")
  khepri_gleam.put(path, test_value)
  io.println("Successfully wrote value")

  // Try to read it back
  io.println("Reading value...")
  case khepri_gleam.get_string(test_path) {
    Ok(value) -> io.println("Read value: " <> value)
    Error(err) -> io.println("Failed to read value: " <> err)
  }

  // Test additional operations
  let int_path = "/:test/int_value"
  let int_path_list = khepri_gleam.to_khepri_path(int_path)
  khepri_gleam.put(int_path_list, 42)

  io.println("Reading integer value...")
  case khepri_gleam.get_int(int_path) {
    Ok(value) -> io.println("Read integer: " <> int.to_string(value))
    Error(err) -> io.println("Failed to read integer: " <> err)
  }

  // Test exists and delete
  io.println("Testing exists...")
  case khepri_gleam.exists(test_path) {
    True -> io.println("Path exists")
    False -> io.println("Path does not exist")
  }

  io.println("Deleting value...")
  khepri_gleam.delete(path)

  case khepri_gleam.exists(test_path) {
    True -> io.println("Path still exists after deletion (unexpected)")
    False -> io.println("Path successfully deleted")
  }
}

pub fn pattern_matching_test() {
  // Start Khepri
  io.println("Starting Khepri...")
  khepri_gleam.start()
  io.println("Khepri started successfully")

  // Create test data
  io.println("\n--- Creating test data ---")

  khepri_gleam.put(khepri_gleam.to_khepri_path("/:inventory/fruits/apple"), #(
    "count",
    10,
    "color",
    "red",
  ))

  khepri_gleam.put(khepri_gleam.to_khepri_path("/:inventory/fruits/banana"), #(
    "count",
    5,
    "color",
    "yellow",
  ))

  khepri_gleam.put(
    khepri_gleam.to_khepri_path("/:inventory/vegetables/carrot"),
    #("count", 15, "color", "orange"),
  )

  // Verify initial data
  io.println("\n--- Verifying initial data ---")

  io.println("Checking apple data:")
  case
    khepri_gleam.get_raw(khepri_gleam.to_khepri_path("/:inventory/fruits/apple"))
  {
    Ok(value) -> io.println("Apple data: " <> string.inspect(value))
    Error(err) -> io.println("Cannot access apple: " <> string.inspect(err))
  }

  io.println("Checking banana data:")
  case
    khepri_gleam.get_raw(khepri_gleam.to_khepri_path(
      "/:inventory/fruits/banana",
    ))
  {
    Ok(value) -> io.println("Banana data: " <> string.inspect(value))
    Error(err) -> io.println("Cannot access banana: " <> string.inspect(err))
  }

  io.println("Checking carrot data:")
  case
    khepri_gleam.get_raw(khepri_gleam.to_khepri_path(
      "/:inventory/vegetables/carrot",
    ))
  {
    Ok(value) -> io.println("Carrot data: " <> string.inspect(value))
    Error(err) -> io.println("Cannot access carrot: " <> string.inspect(err))
  }

  io.println("Checking fruits directory:")
  case khepri_gleam.get_raw(khepri_gleam.to_khepri_path("/:inventory/fruits")) {
    Ok(value) -> io.println("Fruits directory: " <> string.inspect(value))
    Error(err) ->
      io.println("Cannot access fruits directory: " <> string.inspect(err))
  }

  // Test direct path matching
  io.println("\n--- Testing direct path patterns ---")

  io.println("Getting apple with pattern:")
  case
    khepri_gleam.get_pattern([
      #("inventory", khepri_gleam.is("inventory")),
      #("fruits", khepri_gleam.is("fruits")),
      #("apple", khepri_gleam.is("apple")),
    ])
  {
    Ok(apple) -> io.println("Found apple: " <> string.inspect(apple))
    Error(err) -> io.println("Failed to find apple: " <> err)
  }

  // Test specific path deletion
  io.println("\n--- Testing specific path deletion ---")

  let vegetable_path = [
    #("inventory", khepri_gleam.is("inventory")),
    #("vegetables", khepri_gleam.is("vegetables")),
    #("carrot", khepri_gleam.is("carrot")),
  ]

  io.println("Deleting carrot...")
  khepri_gleam.delete_pattern(vegetable_path)

  // Verify carrot deletion
  io.println("Checking if carrot was deleted:")
  case
    khepri_gleam.get_raw(khepri_gleam.to_khepri_path(
      "/:inventory/vegetables/carrot",
    ))
  {
    Ok(value) ->
      io.println("Carrot still exists (unexpected): " <> string.inspect(value))
    Error(_) -> io.println("Carrot was deleted (expected)")
  }

  // Verify other items still exist
  io.println("Checking if banana still exists:")
  case
    khepri_gleam.get_raw(khepri_gleam.to_khepri_path(
      "/:inventory/fruits/banana",
    ))
  {
    Ok(value) ->
      io.println("Banana exists with value: " <> string.inspect(value))
    Error(err) -> io.println("Cannot find banana: " <> string.inspect(err))
  }

  io.println("Checking if apple still exists:")
  case
    khepri_gleam.get_raw(khepri_gleam.to_khepri_path("/:inventory/fruits/apple"))
  {
    Ok(value) ->
      io.println("Apple exists with value: " <> string.inspect(value))
    Error(err) -> io.println("Cannot find apple: " <> string.inspect(err))
  }

  // Verify with string getters
  io.println("\n--- Verifying with string getters ---")

  case khepri_gleam.get_string("/:inventory/fruits/apple") {
    Ok(value) -> io.println("Apple still exists via get_string (expected)")
    Error(_) -> io.println("Apple not found via get_string (unexpected)")
  }

  case khepri_gleam.get_string("/:inventory/fruits/banana") {
    Ok(value) -> io.println("Banana still exists via get_string (expected)")
    Error(_) -> io.println("Banana not found via get_string (unexpected)")
  }

  // Test directory listing
  io.println("\n--- Testing directory listing ---")

  // First, create a structure with more data to test
  khepri_gleam.put(khepri_gleam.to_khepri_path("/:inventory/fruits/orange"), #(
    "count",
    7,
    "color",
    "orange",
  ))
  // In test/khepri_gleam_test.gleam
  // Replace the list_children call with list_directory

  // Now list all fruits
  io.println("Listing all fruits in inventory:")
  case khepri_gleam.list_directory("/:inventory/fruits") {
    Ok(items) -> {
      io.println("Found fruit items: " <> string.inspect(items))

      // Verify the expected count
      case list.length(items) {
        3 -> io.println("Found expected number of fruits (3)")
        other -> io.println("Unexpected fruit count: " <> int.to_string(other))
      }

      // Print each fruit with its data
      list.each(items, fn(item) {
        let #(name, data) = item
        io.println("Fruit: " <> name <> " - Data: " <> string.inspect(data))
      })
    }
    Error(err) -> io.println("Failed to list fruits: " <> err)
  }

  // Test listing vegetables (should be empty after deletion)
  io.println("\nListing all vegetables in inventory:")
  case khepri_gleam.list_directory("/:inventory/vegetables") {
    Ok(items) -> {
      case list.length(items) {
        0 -> io.println("Vegetables directory is empty (expected)")
        count ->
          io.println("Unexpected vegetables found: " <> int.to_string(count))
      }
    }
    Error(err) -> io.println("Failed to list vegetables: " <> err)
  }
  // Test wildcard pattern matching - still developing this feature
  io.println("\n--- Testing wildcard pattern matching ---")

  io.println("Trying to match all fruits:")
  case
    khepri_gleam.get_pattern([
      #("inventory", khepri_gleam.is("inventory")),
      #("fruits", khepri_gleam.is("fruits")),
      #("*", khepri_gleam.any()),
    ])
  {
    Ok(fruits) -> io.println("Found fruits: " <> string.inspect(fruits))
    Error(err) -> io.println("Failed to find fruits: " <> err)
  }
}
