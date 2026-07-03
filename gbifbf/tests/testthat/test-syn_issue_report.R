library(testthat)
library(gbifbf)

# syn_issue_report
test_that("syn_issue_report returns correct status and prints output", {
  
  # Test ISSUE_CLOSED case
  result <- syn_issue_report(
    list(
      name = "Agrion splendens (Harris, 1780)",
      wrongStatus = "ACCEPTED",
      rightStatus = "SYNONYM",
      rightParent = "Calopteryx splendens (Harris, 1780)",
      wrongParent = NULL
    ))
  
  expect_equal(result, "ISSUE_CLOSED")
  
  # Test ISSUE_OPEN case
  result2 <- syn_issue_report(
    list(
      name = "Agrion splendens (Harris, 1780)",
      wrongStatus = NULL,
      rightStatus = "ACCEPTED",
      rightParent = NULL,
      wrongParent = "Calopteryx splendens (Harris, 1780)"
    ))
  
  expect_equal(result2, "ISSUE_OPEN")
  
  # Test with status only
  result3 <- syn_issue_report(
    list(
      name = "Agrion splendens (Harris, 1780)",
      wrongStatus = "ACCEPTED",
      rightStatus = "SYNONYM",
      rightParent = NULL,
      wrongParent = NULL
    ))
  
  expect_equal(result3, "ISSUE_CLOSED")
  
  # Test with parent only
  result4 <- syn_issue_report(
    list(
      name = "Agrion splendens (Harris, 1780)",
      wrongStatus = NULL,
      rightStatus = NULL,
      rightParent = "Calopteryx splendens (Harris, 1780)",
      wrongParent = NULL
    ))
  
  expect_equal(result4, "ISSUE_CLOSED")
})

test_that("syn_issue_report handles non-existent names", {
  
  # Test with non-existent name
  result <- syn_issue_report(
    list(
      name = "Nonexistent taxon name xyz123",
      wrongStatus = "ACCEPTED",
      rightStatus = "SYNONYM",
      rightParent = NULL,
      wrongParent = NULL
    ))
  
  expect_equal(result, "JSON-TAG-ERROR")
})
