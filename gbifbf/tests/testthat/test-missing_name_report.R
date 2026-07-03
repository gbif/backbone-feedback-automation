library(testthat)
library(gbifbf)

# missing_name_report
test_that("missing_name_report returns correct status and prints output", {
  
  # Test ISSUE_OPEN case - name not found
  result <- missing_name_report(
    list(
      missingName = "Fakename notreal Smith, 9999"
    ))
  
  expect_equal(result, "ISSUE_OPEN")
  
  # Test ISSUE_CLOSED case - name exists (has been added)
  result2 <- missing_name_report(
    list(
      missingName = "Animalia"
    ))
  
  expect_equal(result2, "ISSUE_CLOSED")
  
  # Test ISSUE_OPEN case - missing field returns ISSUE_OPEN
  result3 <- missing_name_report(
    list(
      missingName = NULL
    ))
  
  expect_equal(result3, "ISSUE_OPEN")
  
  # Test with a real name that was likely missing at some point
  result4 <- missing_name_report(
    list(
      missingName = "Trichopria aequata (Thomson, 1858)"
    ))
  
  expect_equal(result4, "ISSUE_CLOSED")
})

test_that("missing_name_report handles multiple matches", {
  # Test with a name that might have homonyms
  # This should still return ISSUE_CLOSED if any match is found
  result <- missing_name_report(
    list(
      missingName = "Animalia"
    ))
  
  expect_equal(result, "ISSUE_CLOSED")
})

test_that("missing_name_report prints to console", {
  # Verify that output is generated
  expect_output(
    missing_name_report(
      list(
        missingName = "Animalia"
      )
    ),
    "MISSING NAME ISSUE REPORT"
  )
  
  expect_output(
    missing_name_report(
      list(
        missingName = "Notarealname fake"
      )
    ),
    "ISSUE OPEN"
  )
})
