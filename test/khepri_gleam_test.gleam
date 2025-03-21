// test/khepri_gleam_transaction_test.gleam
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import gleeunit
import khepri_gleam

pub fn main() {
  io.println("\n=== Running Transaction Test ===\n")
  transaction_test()
}

pub fn transaction_test() {
  // Start Khepri
  io.println("Starting Khepri...")
  khepri_gleam.start()
  io.println("Khepri started successfully")

  // Test put transaction
  io.println("\n--- Testing put transaction ---")

  let put_result =
    khepri_gleam.tx_put_path("/:shop/items/apple", #(
      "price",
      1.5,
      "quantity",
      10,
    ))

  case put_result {
    Ok(_) -> io.println("Put transaction completed successfully")
    Error(err) -> io.println("Put transaction failed: " <> err)
  }

  // Verify item exists
  io.println("\nVerifying items after transaction:")

  case khepri_gleam.get_string("/:shop/items/apple") {
    Ok(_) -> io.println("Apple exists (expected)")
    Error(_) -> io.println("Apple not found (unexpected)")
  }

  // Test get transaction
  io.println("\n--- Testing get transaction ---")

  let get_result = khepri_gleam.tx_get_path("/:shop/items/apple")

  case get_result {
    Ok(value) ->
      io.println("Get transaction succeeded: " <> string.inspect(value))
    Error(err) -> io.println("Get transaction failed: " <> err)
  }

  // Test exists transaction
  io.println("\n--- Testing exists transaction ---")

  let exists_result = khepri_gleam.tx_exists_path("/:shop/items/apple")

  case exists_result {
    Ok(True) ->
      io.println("Exists transaction confirmed item exists (expected)")
    Ok(False) ->
      io.println("Exists transaction says item doesn't exist (unexpected)")
    Error(err) -> io.println("Exists transaction failed: " <> err)
  }

  // Test delete transaction
  io.println("\n--- Testing delete transaction ---")

  let delete_result = khepri_gleam.tx_delete_path("/:shop/items/apple")

  case delete_result {
    Ok(_) -> io.println("Delete transaction completed successfully")
    Error(err) -> io.println("Delete transaction failed: " <> err)
  }

  // Verify item was deleted
  case khepri_gleam.get_string("/:shop/items/apple") {
    Ok(_) -> io.println("Apple still exists (unexpected)")
    Error(_) -> io.println("Apple was deleted (expected)")
  }

  io.println("\n=== Transaction Test Completed ===\n")
}
