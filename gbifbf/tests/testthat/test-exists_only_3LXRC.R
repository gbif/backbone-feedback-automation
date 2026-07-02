library(testthat)
library(gbifbf)

# Test exists_only_3LXRC function
test_that("exists_only_3LXRC identifies taxa only in 3LXRC", {
  
  # Test case: VT3QF exists only in 3LXRC (not in 3LXR)
  result_vt3qf <- exists_only_3LXRC("VT3QF")
  
  expect_true(result_vt3qf$exists)
  expect_s3_class(result_vt3qf$usage, "tbl_df")
  expect_gt(nrow(result_vt3qf$usage), 0)
  expect_true("id" %in% names(result_vt3qf$usage))
  expect_equal(result_vt3qf$usage$id, "VT3QF")
  
  # Test case: 9WLSS does not exist only in 3LXRC (either in 3LXR or not found)
  result_9wlss <- exists_only_3LXRC("9WLSS")
  
  expect_false(result_9wlss$exists)
  expect_s3_class(result_9wlss$usage, "tbl_df")
  expect_equal(nrow(result_9wlss$usage), 0)
})

test_that("exists_only_3LXRC returns correct structure", {
  
  result <- exists_only_3LXRC("VT3QF")
  
  # Check that result is a list with two elements
  expect_type(result, "list")
  expect_length(result, 2)
  expect_named(result, c("exists", "usage"))
  
  # Check that exists is logical
  expect_type(result$exists, "logical")
  
  # Check that usage is a tibble
  expect_s3_class(result$usage, "tbl_df")
})

test_that("exists_only_3LXRC usage contains expected columns when found", {
  
  result <- exists_only_3LXRC("VT3QF")
  
  if(result$exists) {
    expected_cols <- c("id", "status", "labelHtml", "label", 
                      "parentId", "rank", "name", "authorship")
    expect_true(all(expected_cols %in% names(result$usage)))
  }
})
