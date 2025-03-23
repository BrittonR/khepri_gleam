// test/khepri_gleam_import_export_test.gleam
import gleam/io
import gleam/list
import gleam/string
import gleeunit
import khepri_gleam

pub fn main() {
  io.println("\n=== Running Import/Export Test ===\n")
  import_export_test()
}

pub fn import_export_test() {
  // Start Khepri
  io.println("Starting Khepri...")
  khepri_gleam.start()
  io.println("Khepri started successfully")

  // Add some test data
  io.println("\n--- Adding test data ---")

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
  io.println("\nVerifying data was added:")
  case khepri_gleam.list_children("/:inventory/fruits") {
    Ok(fruits) -> {
      io.println("Found " <> string.inspect(list.length(fruits)) <> " fruits")
      io.println("Fruit list: " <> string.inspect(fruits))
    }
    Error(err) -> io.println("Error listing fruits: " <> err)
  }

  // Export the database
  io.println("\n--- Exporting database ---")
  let export_result = khepri_gleam.export("/", "khepri_export.erl")

  case export_result {
    Ok(_) -> io.println("Database exported successfully")
    Error(err) -> io.println("Export failed: " <> err)
  }

  // Clear the database
  io.println("\n--- Clearing database ---")
  case khepri_gleam.clear_all() {
    Ok(_) -> io.println("Database cleared successfully")
    Error(err) -> io.println("Failed to clear database: " <> err)
  }

  // Verify database is empty
  io.println("\nVerifying database is empty:")
  case khepri_gleam.list_children("/:inventory") {
    Ok([]) -> io.println("Database is empty (expected)")
    Ok(nodes) -> io.println("Database not empty: " <> string.inspect(nodes))
    Error(_) -> io.println("Inventory path doesn't exist (expected)")
  }

  // Import the database
  io.println("\n--- Importing database ---")
  let import_result = khepri_gleam.import_from_file("khepri_export.erl")

  case import_result {
    Ok(_) -> io.println("Database imported successfully")
    Error(err) -> io.println("Import failed: " <> err)
  }

  // Verify data was restored
  io.println("\nVerifying data was restored:")
  case khepri_gleam.list_children("/:inventory/fruits") {
    Ok(fruits) -> {
      io.println("Found " <> string.inspect(list.length(fruits)) <> " fruits")
      io.println("Fruit list: " <> string.inspect(fruits))
    }
    Error(err) -> io.println("Error listing fruits: " <> err)
  }

  io.println("\n=== Import/Export Test Completed ===\n")
}
