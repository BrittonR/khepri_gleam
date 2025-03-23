// test/khepri_gleam_basics_test.gleam
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import gleeunit
import khepri_gleam
import test_helper

pub fn list_directory_test() {
  // Start Khepri
  test_helper.section("Running list_directory Tests")
  khepri_gleam.start()

  // Set up test data
  setup_test_data()

  // Test listing fruits directory
  test_helper.subsection("Testing fruits directory listing")
  let fruits_result = khepri_gleam.list_directory("/:inventory/fruits")
  test_helper.check_ok("List fruits directory should succeed", fruits_result)

  case fruits_result {
    Ok(items) -> {
      io.println("Found " <> int.to_string(list.length(items)) <> " items")

      // Check item count
      test_helper.assert_equal(
        "Fruits directory should contain 3 items",
        list.length(items),
        3,
      )

      // Verify specific items exist
      let item_names =
        list.map(items, fn(item) {
          let #(name, _) = item
          name
        })

      test_helper.assert_contains(
        "Apple should be in the fruits list",
        item_names,
        "apple",
      )

      test_helper.assert_contains(
        "Banana should be in the fruits list",
        item_names,
        "banana",
      )

      test_helper.assert_contains(
        "Orange should be in the fruits list",
        item_names,
        "orange",
      )

      // Verify data content for an item
      case find_item_data(items, "apple") {
        Ok(data) -> {
          io.println("Found apple data: " <> string.inspect(data))
          test_helper.assert_pass("Apple data found", True)
        }
        Error(_) -> {
          test_helper.assert_pass("Apple data found", False)
        }
      }
    }
    Error(err) -> {
      io.println("ERROR: " <> err)
    }
  }

  // Test listing vegetables directory when empty
  test_helper.subsection("Testing empty vegetables directory listing")
  let veggies_result = khepri_gleam.list_directory("/:inventory/vegetables")
  test_helper.check_ok(
    "List vegetables directory should succeed",
    veggies_result,
  )

  case veggies_result {
    Ok(items) -> {
      // Should be empty
      test_helper.assert_equal(
        "Vegetables directory should be empty",
        list.length(items),
        0,
      )
    }
    Error(err) -> {
      io.println("ERROR: " <> err)
    }
  }

  // Test listing non-existent directory
  test_helper.subsection("Testing non-existent directory listing")
  let nonexistent_result = khepri_gleam.list_directory("/:inventory/not_real")
  test_helper.check_ok(
    "Handling non-existent directory should succeed",
    nonexistent_result,
  )

  case nonexistent_result {
    Ok(items) -> {
      // Should be empty
      test_helper.assert_equal(
        "Non-existent directory should return empty list",
        list.length(items),
        0,
      )
    }
    Error(err) -> {
      io.println("ERROR: " <> err)
    }
  }

  test_helper.section("list_directory Tests Completed")
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
