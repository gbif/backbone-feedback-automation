library(testthat)
library(gbifbf)

# wrong_group_report
test_that("wrong_group_report returns correct status and prints output", {
  
  # Test ISSUE_CLOSED case - taxon in right group
  result <- wrong_group_report(
    list(
      name = "Amphibia",
      wrongGroup = "Plantae", 
      rightGroup = "Animalia"
    ))
  
  expect_equal(result, "ISSUE_CLOSED")
  
  # Test ISSUE_OPEN case - taxon still in wrong group
  result2 <- wrong_group_report(
    list(
      name = "Amphibia",
      wrongGroup = "Animalia",
      rightGroup = "Plantae"
    ))
  
  expect_equal(result2, "ISSUE_OPEN")
  
  # Test with only rightGroup specified
  result3 <- wrong_group_report(
    list(
      name = "Amphibia",
      wrongGroup = NULL,
      rightGroup = "Animalia"
    ))
  
  expect_equal(result3, "ISSUE_CLOSED")
  
  # Test with only wrongGroup specified - not in wrong group
  result4 <- wrong_group_report(
    list(
      name = "Amphibia",
      wrongGroup = "Plantae",
      rightGroup = NULL
    ))
  
  expect_equal(result4, "ISSUE_CLOSED")
  
  # Test with only wrongGroup specified - still in wrong group
  result5 <- wrong_group_report(
    list(
      name = "Amphibia",
      wrongGroup = "Animalia",
      rightGroup = NULL
    ))
  
  expect_equal(result5, "ISSUE_OPEN")
  
  # Test HTML tag handling
  result6 <- wrong_group_report(
    list(
      name = "Lycaena helloides (Boisduval, 1852)",
      wrongGroup = "Sesia Fabricius, 1775",
      rightGroup = "Epidemia Scudder, 1876"
    ))
  
  expect_equal(result6, "ISSUE_CLOSED")
})

test_that("wrong_group_report handles non-existent names", {
  
  # Test with non-existent name
  result <- wrong_group_report(
    list(
      name = "Doggg",
      wrongGroup = "Animalia",
      rightGroup = "Plantae"
    ))
  
  expect_equal(result, "JSON-TAG-ERROR")
})
