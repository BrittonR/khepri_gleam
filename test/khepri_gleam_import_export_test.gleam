// test/khepri_gleam_import_export_test.gleam
import gleam/io
import gleam/list
import gleam/string
import gleeunit
import khepri_gleam
import test_helper

pub fn main() {
  test_helper.section("Running Import/Export Test")
  import_export_test()
}

pub fn import_export_test() {
  // Start Khepri
  io.println("Starting Khepri...")
  khepri_gleam.start()
  test_helper.assert_pass("Khepri started successfully", True)

  // Add some test data
  test_helper.subsection("Adding test data")

  // Create a simple data structure
  khepri_gleam.put(["inventory", "fruits", "apple"], #(
    "color",
    "red",
    "price",
    1.2,
    "quantity",
    100,
  ))

  khepri_gleam.put(["inventory", "fruits", "banana"], #(
    "color",
    "yellow",
    "price",
    0.6,
    "quantity",
    150,
  ))

  khepri_gleam.put(["inventory", "vegetables", "carrot"], #(
    "color",
    "orange",
    "price",
    0.8,
    "quantity",
    75,
  ))

  // Verify data was added
  test_helper.subsection("Verifying data was added")
  let fruits_result = khepri_gleam.list_children("/:inventory/fruits")
  test_helper.check_ok("List fruits directory should succeed", fruits_result)

  case fruits_result {
    Ok(fruits) -> {
      test_helper.assert_equal("Should find 2 fruits", list.length(fruits), 2)
      io.println("Fruit list: " <> string.inspect(fruits))
    }
    Error(_) -> Nil
  }

  // Export the database
  test_helper.subsection("Exporting database")
  let export_result = khepri_gleam.export("/", "khepri_export.erl")
  test_helper.check_ok("Database export should succeed", export_result)

  // Clear the database
  test_helper.subsection("Clearing database")
  let clear_result = khepri_gleam.clear_all()
  test_helper.check_ok("Database clear should succeed", clear_result)

  // Verify database is empty
  test_helper.subsection("Verifying database is empty")
  let empty_check = khepri_gleam.list_children("/:inventory")
  // This could return an error or an empty list - both are valid for "empty database"
  case empty_check {
    Ok([]) -> test_helper.assert_pass("Database is empty", True)
    Ok(nodes) -> {
      test_helper.assert_pass("Database is empty", False)
      io.println("Database still contains: " <> string.inspect(nodes))
    }
    Error(_) ->
      test_helper.assert_pass("Inventory path doesn't exist (expected)", True)
  }

  // Import the database
  test_helper.subsection("Importing database")
  let import_result = khepri_gleam.import_from_file("khepri_export.erl")
  test_helper.check_ok("Database import should succeed", import_result)

  // Verify data was restored
  test_helper.subsection("Verifying data was restored")
  let restored_check = khepri_gleam.list_children("/:inventory/fruits")
  test_helper.check_ok(
    "List fruits after import should succeed",
    restored_check,
  )

  case restored_check {
    Ok(fruits) -> {
      test_helper.assert_equal(
        "Should find 2 fruits after import",
        list.length(fruits),
        2,
      )
      io.println("Fruit list: " <> string.inspect(fruits))
    }
    Error(_) -> Nil
  }

  test_helper.section("Import/Export Test Completed")
}
