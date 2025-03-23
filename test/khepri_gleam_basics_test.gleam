// test/khepri_gleam_basics_test.gleam
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import gleeunit
import gleeunit/should
import khepri_gleam

pub fn list_directory_test() {
  // Start Khepri
  io.println("\n=== Running list_directory Tests ===\n")
  khepri_gleam.start()

  // Set up test data
  setup_test_data()

  // Test listing fruits directory
  io.println("Testing fruits directory listing:")
  case khepri_gleam.list_directory("/:inventory/fruits") {
    Ok(items) -> {
      io.println("Found " <> int.to_string(list.length(items)) <> " items")

      // Check item count
      list.length(items)
      |> should.equal(3)

      // Verify specific items exist
      let item_names =
        list.map(items, fn(item) {
          let #(name, _) = item
          name
        })

      list.contains(item_names, "apple")
      |> should.be_true()

      list.contains(item_names, "banana")
      |> should.be_true()

      list.contains(item_names, "orange")
      |> should.be_true()

      // Verify data content for an item
      let apple_data = find_item_data(items, "apple")
      case apple_data {
        Ok(data) -> {
          io.println("Found apple data: " <> string.inspect(data))
          // Data assertions could go here if needed
        }
        Error(_) -> {
          // Print an error message instead of using should.fail with a message
          io.println("ERROR: Apple data not found or invalid")
          False |> should.be_true()
          // This will fail the test
        }
      }
    }
    Error(err) -> {
      // Print an error message instead of using should.fail with a message
      io.println("ERROR: Failed to list fruits: " <> err)
      False |> should.be_true()
      // This will fail the test
    }
  }

  // Test listing vegetables directory when empty
  io.println("\nTesting empty vegetables directory listing:")
  case khepri_gleam.list_directory("/:inventory/vegetables") {
    Ok(items) -> {
      // Should be empty
      list.length(items)
      |> should.equal(0)

      io.println("Correctly found empty vegetables directory")
    }
    Error(err) -> {
      // Print an error message instead of using should.fail with a message
      io.println("ERROR: Failed to list vegetables: " <> err)
      False |> should.be_true()
      // This will fail the test
    }
  }

  // Test listing non-existent directory
  io.println("\nTesting non-existent directory listing:")
  case khepri_gleam.list_directory("/:inventory/not_real") {
    Ok(items) -> {
      // Should be empty
      list.length(items)
      |> should.equal(0)

      io.println("Correctly handled non-existent directory")
    }
    Error(_) -> {
      // Print an error message instead of using should.fail with a message
      io.println("ERROR: Should return empty list for non-existent directory")
      False |> should.be_true()
      // This will fail the test
    }
  }

  io.println("\n=== list_directory Tests Completed ===\n")
}

fn setup_test_data() {
  io.println("Setting up test data...")

  // Create fruits
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

  khepri_gleam.put(khepri_gleam.to_khepri_path("/:inventory/fruits/orange"), #(
    "count",
    7,
    "color",
    "orange",
  ))

  // Create vegetables (single item that will be later verified as empty)
  khepri_gleam.put(
    khepri_gleam.to_khepri_path("/:inventory/vegetables/carrot"),
    #("count", 15, "color", "orange"),
  )

  // Delete the carrot to test empty directory handling
  khepri_gleam.delete(khepri_gleam.to_khepri_path(
    "/:inventory/vegetables/carrot",
  ))

  io.println("Test data setup complete")
}

fn find_item_data(items, name) {
  list.find(items, fn(item) {
    let #(item_name, _) = item
    item_name == name
  })
  |> result.map(fn(found) {
    let #(_, data) = found
    data
  })
}
