library(testthat)
library(gbifbf)

# Test exists_only_3LXRC function
test_that("exists_only_3LXRC identifies taxa only in 3LXRC", {
  
  # Test case: VT3QF exists only in 3LXRC (not in 3LXR)
  result_vt3qf <- exists_only_3LXRC("VT3QF")
  
  expect_true(result_vt3qf$exists_3LXRC)
  expect_true(result_vt3qf$exists_only_3LXRC)
  expect_false(result_vt3qf$exists_3LXR)
  expect_s3_class(result_vt3qf$usage_3LXRC, "tbl_df")
  expect_gt(nrow(result_vt3qf$usage_3LXRC), 0)
  expect_true("id" %in% names(result_vt3qf$usage_3LXRC))
  expect_equal(result_vt3qf$usage_3LXRC$id, "VT3QF")
  
  # Test case: 9WLSS does not exist only in 3LXRC (either in 3LXR or not found)
  result_9wlss <- exists_only_3LXRC("9WLSS")
  
  expect_false(result_9wlss$exists_only_3LXRC)
  expect_s3_class(result_9wlss$usage_3LXRC, "tbl_df")
  expect_equal(nrow(result_9wlss$usage_3LXRC), 0)
})

test_that("exists_only_3LXRC returns correct structure", {
  
  result <- exists_only_3LXRC("VT3QF")
  
  # Check that result is a list with four elements
  expect_type(result, "list")
  expect_length(result, 4)
  expect_named(result, c("exists_3LXRC", "exists_only_3LXRC", "exists_3LXR", "usage_3LXRC"))
  
  # Check that all exists fields are logical
  expect_type(result$exists_3LXRC, "logical")
  expect_type(result$exists_only_3LXRC, "logical")
  expect_type(result$exists_3LXR, "logical")
  
  # Check that usage_3LXRC is a tibble
  expect_s3_class(result$usage_3LXRC, "tbl_df")
})

test_that("exists_only_3LXRC usage_3LXRC contains expected columns when found", {
  
  result <- exists_only_3LXRC("VT3QF")
  
  if(result$exists_3LXRC) {
    expected_cols <- c("id", "status", "labelHtml", "label", 
                      "parentId", "rank", "name", "authorship")
    expect_true(all(expected_cols %in% names(result$usage_3LXRC)))
  }
})

test_that("exists_only_3LXRC logic is correct", {
  
  result <- exists_only_3LXRC("VT3QF")
  
  # exists_only_3LXRC should be TRUE only if exists_3LXRC is TRUE AND exists_3LXR is FALSE
  expect_equal(result$exists_only_3LXRC, result$exists_3LXRC && !result$exists_3LXR)
})
