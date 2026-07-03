library(testthat)
library(gbifbf)

# name_change_report
test_that("name_change_report returns correct status and prints output", {
  
  # Test ERROR case - identical names
  result <- name_change_report(
    list(
      currentName = "Dog dog Waller 2025",
      proposedName = "Dog dog Waller 2025"
    ))
  
  expect_equal(result, "JSON-TAG-ERROR")
  
  # Test ISSUE_OPEN case - current name still exists, proposed doesn't
  result2 <- name_change_report(
    list(
      currentName = "Animalia",
      proposedName = "Dog"
    ))
  
  expect_equal(result2, "ISSUE_OPEN")
  
  # Test ISSUE_CLOSED case - current is now synonym of proposed
  result3 <- name_change_report(
    list(
      currentName = "Cryptophyta",
      proposedName = "Cryptista Cavalier-Smith, 1989"
    ))
  
  expect_equal(result3, "ISSUE_CLOSED")
  
  # Test ISSUE_OPEN - existing current, non-existent proposed
  result4 <- name_change_report(
    list(
      currentName = "Amphibia",
      proposedName = "Notarealname taxonomicus"
    ))
  
  expect_equal(result4, "ISSUE_OPEN")
  
  # Test ISSUE_CLOSED - current removed, proposed exists
  result5 <- name_change_report(
    list(
      currentName = "Notarealname fake",
      proposedName = "Animalia"
    ))
  
  expect_equal(result5, "ISSUE_CLOSED")
})

test_that("name_change_report handles invalid input", {
  
  # Test both names non-existent
  result <- name_change_report(
    list(
      currentName = "Fakeus one",
      proposedName = "Fakeus two"
    ))
  
  expect_equal(result, "JSON-TAG-ERROR")
  
  # Test missing proposedName
  result2 <- name_change_report(
    list(
      currentName = "Animalia"
    ))
  
  expect_equal(result2, "JSON-TAG-ERROR")
  
  # Test missing currentName
  result3 <- name_change_report(
    list(
      proposedName = "Animalia"
    ))
  
  expect_equal(result3, "JSON-TAG-ERROR")
})
