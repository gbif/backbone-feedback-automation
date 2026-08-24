library(testthat)
library(gbifbf)

# These tests are commented out because they depend on specific taxa existing in the COL
# 3LXRC dataset, which can change over time as taxa are merged, removed, or moved between datasets.
# The test taxa (VT3QF, 9WLSS) are no longer reliable for testing this functionality.

# # Test exists_only_3LXRC function
# test_that("exists_only_3LXRC identifies taxa only in 3LXRC", {
#   skip_on_ci()
#   skip_if_offline()
#   
#   # Test case: VT3QF previously existed only in 3LXRC (not in 3LXR)
#   # Note: This test may fail if the taxon has been removed or merged in COL
#   result_vt3qf <- exists_only_3LXRC("VT3QF")
#   
#   # Skip test if VT3QF no longer exists in 3LXRC
#   skip_if(!result_vt3qf$exists_3LXRC, "Test taxon VT3QF no longer exists in 3LXRC dataset")
#   
#   expect_true(result_vt3qf$exists_3LXRC)
#   expect_true(result_vt3qf$exists_only_3LXRC)
#   expect_false(result_vt3qf$exists_3LXR)
#   expect_s3_class(result_vt3qf$usage_3LXRC, "tbl_df")
#   expect_gt(nrow(result_vt3qf$usage_3LXRC), 0)
#   expect_true("id" %in% names(result_vt3qf$usage_3LXRC))
#   expect_equal(result_vt3qf$usage_3LXRC$id, "VT3QF")
#   
#   # Test case: 9WLSS does not exist only in 3LXRC (exists in both 3LXR and 3LXRC)
#   result_9wlss <- exists_only_3LXRC("9WLSS")
#   
#   expect_false(result_9wlss$exists_only_3LXRC)
#   expect_s3_class(result_9wlss$usage_3LXRC, "tbl_df")
#   # If exists_only_3LXRC is FALSE, it could be because:
#   # 1. Taxon exists in both datasets (usage_3LXRC has data, exists_3LXR is TRUE)
#   # 2. Taxon doesn't exist in 3LXRC (usage_3LXRC is empty, exists_3LXRC is FALSE)
#   if(result_9wlss$exists_3LXR && result_9wlss$exists_3LXRC) {
#     expect_gt(nrow(result_9wlss$usage_3LXRC), 0)
#   } else {
#     expect_equal(nrow(result_9wlss$usage_3LXRC), 0)
#   }
# })
# 
# test_that("exists_only_3LXRC returns correct structure", {
#   skip_on_ci()
#   skip_if_offline()
#   
#   result <- exists_only_3LXRC("VT3QF")
#   
#   # Check that result is a list with four elements
#   expect_type(result, "list")
#   expect_length(result, 4)
#   expect_named(result, c("exists_3LXRC", "exists_only_3LXRC", "exists_3LXR", "usage_3LXRC"))
#   
#   # Check that all exists fields are logical
#   expect_type(result$exists_3LXRC, "logical")
#   expect_type(result$exists_only_3LXRC, "logical")
#   expect_type(result$exists_3LXR, "logical")
#   
#   # Check that usage_3LXRC is a tibble
#   expect_s3_class(result$usage_3LXRC, "tbl_df")
# })
# 
# test_that("exists_only_3LXRC usage_3LXRC contains expected columns when found", {
#   skip_on_ci()
#   skip_if_offline()
#   
#   result <- exists_only_3LXRC("VT3QF")
#   
#   # Skip if taxon no longer exists
#   skip_if(!result$exists_3LXRC, "Test taxon VT3QF no longer exists in 3LXRC dataset")
#   
#   if(result$exists_3LXRC) {
#     expected_cols <- c("id", "status", "labelHtml", "label", 
#                       "parentId", "rank", "name", "authorship")
#     expect_true(all(expected_cols %in% names(result$usage_3LXRC)))
#   }
# })
# 
# test_that("exists_only_3LXRC logic is correct", {
#   skip_on_ci()
#   skip_if_offline()
#   
#   result <- exists_only_3LXRC("VT3QF")
#   
#   # exists_only_3LXRC should be TRUE only if exists_3LXRC is TRUE AND exists_3LXR is FALSE
#   expect_equal(result$exists_only_3LXRC, result$exists_3LXRC && !result$exists_3LXR)
# })
