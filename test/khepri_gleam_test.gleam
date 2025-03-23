// test/khepri_gleam_test.gleam
import gleam/io
import gleeunit
import khepri_gleam_basics_test
import khepri_gleam_transaction_test

pub fn main() {
  io.println("\n=== Running Khepri Gleam Tests ===\n")

  // Run the basics tests
  khepri_gleam_basics_test.list_directory_test()

  // Run transaction tests
  khepri_gleam_transaction_test.transaction_test()

  // Add more test imports here as you develop them

  io.println("\n=== All Tests Completed ===\n")
}
