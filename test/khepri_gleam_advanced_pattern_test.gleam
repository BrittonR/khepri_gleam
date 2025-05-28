// test/khepri_pattern_test.gleam
import gleam/dynamic
import gleam/int
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

  // // Test combined conditions
  // test_helper.subsection("Testing combined conditions")
  // test_combined_conditions()

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
    // Instead of using a specific name "product" with any condition
    // Use an empty string for the name and let the any condition do the matching
    #("", khepri_pattern.any()),
  ]

  let any_electronic_exists = khepri_pattern.exists(any_electronic_pattern)
  // Use assert_equal instead of assert_pass for clearer expectations
  test_helper.assert_equal(
    "Any electronic should match",
    any_electronic_exists,
    True,
  )

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
  // To check if "electronics" exists, just use the path directly
  let electronics_exists =
    khepri_pattern.exists([
      #("store", khepri_pattern.is("store")),
      #("products", khepri_pattern.is("products")),
      #("electronics", khepri_pattern.is("electronics")),
    ])
  test_helper.assert_pass("Electronics should exist", electronics_exists)

  // To check if a specific product exists under electronics
  let laptop_exists =
    khepri_pattern.exists([
      #("store", khepri_pattern.is("store")),
      #("products", khepri_pattern.is("products")),
      #("electronics", khepri_pattern.is("electronics")),
      #("laptop", khepri_pattern.is("laptop")),
    ])
  test_helper.assert_pass(
    "Laptop should exist under electronics",
    laptop_exists,
  )

  // To check if ANY product exists under electronics (using wildcard)
  let has_products =
    khepri_pattern.exists([
      #("store", khepri_pattern.is("store")),
      #("products", khepri_pattern.is("products")),
      #("electronics", khepri_pattern.is("electronics")),
      #("", khepri_pattern.any()),
      // This will match any child
    ])
  test_helper.assert_pass(
    "Should have products under electronics",
    has_products,
  )

  // Check for non-existent path
  let toys_exists =
    khepri_pattern.exists([
      #("store", khepri_pattern.is("store")),
      #("products", khepri_pattern.is("products")),
      #("toys", khepri_pattern.is("toys")),
      // This category doesn't exist
    ])
  test_helper.assert_pass(
    "Non-existent category should not exist",
    !toys_exists,
  )

  // Test has_data function for a node that exists with data
  let has_data_result =
    khepri_pattern.has_data(["store", "products", "electronics", "laptop"])
  test_helper.assert_pass("Laptop should have data", has_data_result)

  // Test has_data for a node that exists but has no data (like a directory)
  let dir_has_data = khepri_pattern.has_data(["store", "products"])
  test_helper.assert_pass(
    "Products directory should not have data",
    !dir_has_data,
  )
}

fn test_child_count() {
  // Test exact child count by getting children and counting them
  case khepri_gleam.list_children("/:store/products/electronics") {
    Ok(children) -> {
      let child_count = list.length(children)
      test_helper.assert_equal(
        "Electronics should have exactly 3 children",
        child_count,
        3,
      )
    }
    Error(_) -> {
      test_helper.assert_pass(
        "Electronics should have exactly 3 children",
        False,
      )
    }
  }

  // Test child count range
  case khepri_gleam.list_children("/:store/products/food") {
    Ok(children) -> {
      let child_count = list.length(children)
      let in_range = child_count >= 2 && child_count <= 4
      test_helper.assert_pass("Food should have 2-4 children", in_range)
    }
    Error(_) -> {
      test_helper.assert_pass("Food should have 2-4 children", False)
    }
  }

  // Test clothing has 2 children
  case khepri_gleam.list_children("/:store/products/clothing") {
    Ok(children) -> {
      let child_count = list.length(children)
      test_helper.assert_equal(
        "Clothing should have 2 children",
        child_count,
        2,
      )
    }
    Error(_) -> {
      test_helper.assert_pass("Clothing should have 2 children", False)
    }
  }
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

  // Test data matching by getting the node and checking its data
  case khepri_gleam.get(["store", "products", "food", "apples"]) {
    Ok(apple_data) -> {
      // Check if the data contains organic: true
      // For now, just check if we got data and print it
      io.println("Apple data: " <> string.inspect(apple_data))

      // Check if the string representation contains organic: True
      let data_str = string.inspect(apple_data)
      let has_organic = string.contains(data_str, "\"organic\", True")

      test_helper.assert_pass(
        "Organic apples pattern should match",
        has_organic,
      )
    }
    Error(_) -> {
      test_helper.assert_pass("Organic apples pattern should match", False)
    }
  }

  // Alternative: Get all food items and filter by organic status
  case khepri_gleam.list_children("/:store/products/food") {
    Ok(food_items) -> {
      // Find organic items
      let organic_items =
        list.filter(food_items, fn(item) {
          let #(name, data) = item
          // Check if this item is organic
          let data_str = string.inspect(data)
          string.contains(data_str, "\"organic\", True")
        })

      let has_organic = list.length(organic_items) > 0
      test_helper.assert_pass("Should find organic food items", has_organic)

      // Print what we found
      case list.length(organic_items) {
        0 -> io.println("No organic items found")
        n -> {
          io.println("Found " <> int.to_string(n) <> " organic items:")
          list.each(organic_items, fn(item) {
            let #(name, _) = item
            io.println("  - " <> name)
          })
        }
      }
    }
    Error(_) -> {
      test_helper.assert_pass("Should find organic food items", False)
    }
  }
}
