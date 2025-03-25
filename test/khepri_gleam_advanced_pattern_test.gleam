// test/khepri_pattern_test.gleam
import gleam/dynamic
import gleam/io
import gleam/list
import gleam/string
import gleeunit
import khepri_gleam
import khepri_pattern
import test_helper

pub fn main() {
  test_helper.section("Running Enhanced Pattern Matching Tests")
  enhanced_pattern_test()
}

pub fn enhanced_pattern_test() {
  // Start Khepri
  io.println("Starting Khepri...")
  khepri_gleam.start()
  test_helper.assert_pass("Khepri started successfully", True)

  // Clear any leftover data
  let _ = khepri_gleam.clear_all()

  // Set up test data
  test_helper.subsection("Setting up test data")
  setup_test_data()

  // Test basic pattern matching
  test_helper.subsection("Testing basic pattern matching")
  test_basic_patterns()

  // Test existence conditions
  test_helper.subsection("Testing existence conditions")
  test_existence_conditions()

  // Test child count conditions
  test_helper.subsection("Testing child count conditions")
  test_child_count()

  // Test data matching
  test_helper.subsection("Testing data matching")
  test_data_matching()

  // Test combined conditions
  test_helper.subsection("Testing combined conditions")
  test_combined_conditions()

  // Clean up after test
  let _ = khepri_gleam.clear_all()
  test_helper.section("Enhanced Pattern Matching Tests Completed")
}

fn setup_test_data() {
  // Create a variety of test data with different structures and attributes

  // Products category with subcategories
  // Electronics products
  khepri_gleam.put(
    ["store", "products", "electronics", "laptop"],
    #("brand", "TechBrand", "price", 999.99, "stock", 15, "specs", #(
      "cpu",
      "i7",
      "ram",
      16,
      "storage",
      512,
    )),
  )

  khepri_gleam.put(
    ["store", "products", "electronics", "phone"],
    #("brand", "MobileX", "price", 699.99, "stock", 30, "specs", #(
      "screen",
      6.7,
      "ram",
      8,
      "storage",
      256,
    )),
  )

  khepri_gleam.put(
    ["store", "products", "electronics", "tablet"],
    #("brand", "TechBrand", "price", 499.99, "stock", 20, "specs", #(
      "screen",
      10.2,
      "ram",
      4,
      "storage",
      128,
    )),
  )

  // Food products
  khepri_gleam.put(["store", "products", "food", "apples"], #(
    "category",
    "fruit",
    "price",
    2.99,
    "stock",
    100,
    "organic",
    True,
  ))

  khepri_gleam.put(["store", "products", "food", "bread"], #(
    "category",
    "bakery",
    "price",
    3.49,
    "stock",
    50,
    "organic",
    False,
  ))

  khepri_gleam.put(["store", "products", "food", "carrots"], #(
    "category",
    "vegetable",
    "price",
    1.99,
    "stock",
    80,
    "organic",
    True,
  ))

  // Clothing products
  khepri_gleam.put(["store", "products", "clothing", "shirt"], #(
    "size",
    "M",
    "color",
    "blue",
    "price",
    24.99,
    "stock",
    45,
  ))

  khepri_gleam.put(["store", "products", "clothing", "jeans"], #(
    "size",
    "L",
    "color",
    "black",
    "price",
    49.99,
    "stock",
    25,
  ))

  // Customer data
  khepri_gleam.put(["store", "customers", "123"], #(
    "name",
    "John Doe",
    "email",
    "john@example.com",
    "vip",
    True,
  ))

  khepri_gleam.put(["store", "customers", "456"], #(
    "name",
    "Jane Smith",
    "email",
    "jane@example.com",
    "vip",
    False,
  ))

  io.println("Test data setup complete")
}

fn test_basic_patterns() {
  // Test simple path pattern matching
  let laptop_pattern = [
    #("store", khepri_pattern.is("store")),
    #("products", khepri_pattern.is("products")),
    #("electronics", khepri_pattern.is("electronics")),
    #("laptop", khepri_pattern.is("laptop")),
  ]

  let laptop_exists = khepri_pattern.exists(laptop_pattern)
  test_helper.assert_pass("Laptop path should match", laptop_exists)

  // Test with 'any' pattern
  let any_electronic_pattern = [
    #("store", khepri_pattern.is("store")),
    #("products", khepri_pattern.is("products")),
    #("electronics", khepri_pattern.is("electronics")),
    #("product", khepri_pattern.any()),
  ]

  let any_electronic_exists = khepri_pattern.exists(any_electronic_pattern)
  test_helper.assert_pass("Any electronic should match", any_electronic_exists)

  // Test path that shouldn't match
  let missing_pattern = [
    #("store", khepri_pattern.is("store")),
    #("products", khepri_pattern.is("products")),
    #("missing", khepri_pattern.is("missing")),
    #("item", khepri_pattern.is("item")),
  ]

  let missing_exists = khepri_pattern.exists(missing_pattern)
  test_helper.assert_pass("Missing path should not match", !missing_exists)

  // Test pattern with path helper
  let path_pattern =
    khepri_pattern.path_to_pattern("/:store/products/food/apples")
  let apples_exists = khepri_pattern.exists(path_pattern)
  test_helper.assert_pass(
    "Path conversion should work for apples",
    apples_exists,
  )
}

fn test_existence_conditions() {
  // Test node existence condition (true)
  let exists_pattern = [
    #("store", khepri_pattern.is("store")),
    #("products", khepri_pattern.is("products")),
    #("electronics", khepri_pattern.if_node_exists(True)),
  ]

  let exists_result = khepri_pattern.exists(exists_pattern)
  test_helper.assert_pass("Electronics should exist", exists_result)

  // Test node existence condition (false)
  let not_exists_pattern = [
    #("store", khepri_pattern.is("store")),
    #("products", khepri_pattern.is("products")),
    #("missing", khepri_pattern.if_node_exists(False)),
  ]

  let not_exists_result = khepri_pattern.exists(not_exists_pattern)
  test_helper.assert_pass("Missing node should not exist", not_exists_result)

  // Test has_data condition
  let has_data_result =
    khepri_pattern.has_data(["store", "products", "electronics", "laptop"])
  test_helper.assert_pass("Laptop should have data", has_data_result)

  // Test negation condition
  let not_pattern = [
    #("store", khepri_pattern.is("store")),
    #("products", khepri_pattern.is("products")),
    #("category", khepri_pattern.if_not(khepri_pattern.is("missing"))),
  ]

  let not_result = khepri_pattern.exists(not_pattern)
  test_helper.assert_pass("NOT condition should match", not_result)
}

fn test_child_count() {
  // Test exact child count
  let has_four =
    khepri_pattern.has_exact_children(["store", "products", "electronics"], 3)
  test_helper.assert_pass(
    "Electronics should have exactly 3 children",
    has_four,
  )

  // Test child count range
  let in_range =
    khepri_pattern.has_children_in_range(["store", "products", "food"], 2, 4)
  test_helper.assert_pass("Food should have 2-4 children", in_range)

  // Test child list length condition
  let length_pattern = [
    #("store", khepri_pattern.is("store")),
    #("products", khepri_pattern.is("products")),
    #("clothing", khepri_pattern.if_child_list_length(2, khepri_gleam.Equal)),
  ]

  let length_result = khepri_pattern.exists(length_pattern)
  test_helper.assert_pass("Clothing should have 2 children", length_result)
}

fn test_data_matching() {
  // Test finding item with specific data attribute
  let has_techbrand_laptop =
    khepri_pattern.find_with_attribute(
      ["store", "products", "electronics"],
      "brand",
      "TechBrand",
    )

  test_helper.check_ok("Should find TechBrand items", has_techbrand_laptop)

  // Print what was found (if anything)
  case has_techbrand_laptop {
    Ok(data) -> {
      io.println("Found TechBrand items: " <> string.inspect(data))
      test_helper.assert_pass("TechBrand items found", True)
    }
    Error(err) -> {
      io.println("Error finding TechBrand items: " <> err)
      test_helper.assert_pass("TechBrand items found", False)
    }
  }

  // Test data matching in pattern
  let organic_pattern = [
    #("store", khepri_pattern.is("store")),
    #("products", khepri_pattern.is("products")),
    #("food", khepri_pattern.is("food")),
    #("apples", khepri_pattern.data_matches(#("organic", True))),
  ]

  let organic_exists = khepri_pattern.exists(organic_pattern)
  test_helper.assert_pass("Organic apples pattern should match", organic_exists)
}

fn test_combined_conditions() {
  // Test combining conditions with 'all'
  let all_pattern = [
    #("store", khepri_pattern.is("store")),
    #(
      "products",
      khepri_pattern.all([
        khepri_pattern.if_child_list_length(3, khepri_gleam.Equal),
        khepri_pattern.is("products"),
      ]),
    ),
  ]

  let all_match = khepri_pattern.exists(all_pattern)
  test_helper.assert_pass("AND condition should match", all_match)

  // Test combining conditions with 'any_of'
  let any_pattern = [
    #("store", khepri_pattern.is("store")),
    #(
      "category",
      khepri_pattern.any_of([
        khepri_pattern.is("products"),
        khepri_pattern.is("customers"),
      ]),
    ),
  ]

  let any_match = khepri_pattern.exists(any_pattern)
  test_helper.assert_pass("OR condition should match", any_match)

  // Test complex condition combining multiple types
  let complex_pattern = [
    #("store", khepri_pattern.is("store")),
    #(
      "products",
      khepri_pattern.all([
        khepri_pattern.if_node_exists(True),
        khepri_pattern.if_child_list_length(3, khepri_gleam.Equal),
        khepri_pattern.is("products"),
      ]),
    ),
  ]

  let complex_match = khepri_pattern.exists(complex_pattern)
  test_helper.assert_pass("Complex condition should match", complex_match)
}
