// test/khepri_gleam_transaction_test.gleam
import gleam/io
import gleam/string
import khepri_gleam
import test_helper

pub fn transaction_test() {
  // Start Khepri
  test_helper.section("Transaction Tests")
  io.println("Starting Khepri...")
  khepri_gleam.start()
  test_helper.assert_pass("Khepri started successfully", True)

  // Test put transaction
  test_helper.subsection("Testing put transaction")
  let put_result =
    khepri_gleam.tx_put_path("/:shop/items/apple", #(
      "price",
      1.5,
      "quantity",
      10,
    ))
  test_helper.check_ok("Put transaction completed successfully", put_result)

  // Verify item exists
  test_helper.subsection("Verifying items after transaction")
  let exists_result = khepri_gleam.get_string("/:shop/items/apple")
  test_helper.check_ok("Apple should exist after transaction", exists_result)

  // Test get transaction
  test_helper.subsection("Testing get transaction")
  let get_result = khepri_gleam.tx_get_path("/:shop/items/apple")
  test_helper.check_ok("Get transaction should succeed", get_result)
  case get_result {
    Ok(value) -> io.println("Got value: " <> string.inspect(value))
    Error(_) -> Nil
  }

  // Test exists transaction
  test_helper.subsection("Testing exists transaction")
  let exists_tx_result = khepri_gleam.tx_exists_path("/:shop/items/apple")
  test_helper.check_ok("Exists transaction should succeed", exists_tx_result)
  case exists_tx_result {
    Ok(exists) -> test_helper.assert_pass("Apple should exist", exists)
    Error(_) -> Nil
  }

  // Test delete transaction
  test_helper.subsection("Testing delete transaction")
  let delete_result = khepri_gleam.tx_delete_path("/:shop/items/apple")
  test_helper.check_ok("Delete transaction should succeed", delete_result)

  // Verify item was deleted
  let verify_deleted = khepri_gleam.get_string("/:shop/items/apple")
  test_helper.check_error("Apple should be deleted", verify_deleted)

  test_helper.section("Transaction Test Completed")
}
