// test/test_helper.gleam
import gleam/io
import gleam/list
import gleam/string
import gleam_community/ansi
import gleeunit/should

pub fn assert_pass(message: String, condition: Bool) -> Nil {
  case condition {
    True -> io.println(ansi.green("✓ PASS: " <> message))
    False -> {
      io.println(ansi.red("✗ FAIL: " <> message))
      // Use gleeunit's should.equal to fail the test
      should.equal(True, False)
      Nil
    }
  }
}

pub fn assert_equal(message: String, actual, expected) -> Nil {
  let condition = actual == expected
  case condition {
    True -> io.println(ansi.green("✓ PASS: " <> message))
    False -> {
      let error =
        "Expected: "
        <> string.inspect(expected)
        <> ", Got: "
        <> string.inspect(actual)
      io.println(ansi.red("✗ FAIL: " <> message <> "\n  " <> error))
      should.equal(expected, actual)
      Nil
    }
  }
}

pub fn assert_contains(message: String, lst, item) -> Nil {
  let condition = list.contains(lst, item)
  case condition {
    True -> io.println(ansi.green("✓ PASS: " <> message))
    False -> {
      let error =
        "List does not contain expected item: "
        <> string.inspect(item)
        <> "\nList: "
        <> string.inspect(lst)
      io.println(ansi.red("✗ FAIL: " <> message <> "\n  " <> error))
      should.be_true(False)
      Nil
    }
  }
}

// Let's completely rewrite the result handling functions
pub fn check_ok(message: String, result) -> Nil {
  case result {
    Ok(_) -> {
      io.println(ansi.green("✓ PASS: " <> message))
    }
    Error(_) -> {
      io.println(ansi.red("✗ FAIL: " <> message <> "\n  Result was an Error"))
      should.be_true(False)
    }
  }
}

pub fn check_error(message: String, result) -> Nil {
  case result {
    Error(_) -> {
      io.println(ansi.green("✓ PASS: " <> message))
    }
    Ok(_) -> {
      io.println(ansi.red(
        "✗ FAIL: " <> message <> "\n  Expected Error but got Ok",
      ))
      should.be_true(False)
    }
  }
}

pub fn section(title: String) -> Nil {
  io.println("\n" <> ansi.bold(ansi.blue("=== " <> title <> " ===")))
}

pub fn subsection(title: String) -> Nil {
  io.println("\n" <> ansi.yellow("--- " <> title <> " ---"))
}
