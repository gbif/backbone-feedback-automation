library(testthat)
library(gbifbf)

# wrong_rank_report
test_that("wrong_rank_report returns correct status and prints output", {
  
  # Test ISSUE_OPEN case - taxon has wrong rank
  result <- wrong_rank_report(
    list(
      name = "Animalia",
      wrongRank = "kingdom",
      rightRank = "phylum"
    ))
  
  expect_equal(result, "ISSUE_OPEN")
  
  # Test ISSUE_CLOSED case - taxon has right rank
  result2 <- wrong_rank_report(
    list(
      name = "Animalia",
      wrongRank = "phylum",
      rightRank = "kingdom"
    ))
  
  expect_equal(result2, "ISSUE_CLOSED")
  
  # Test with only wrongRank - issue open
  result3 <- wrong_rank_report(
    list(
      name = "Amphibia",
      wrongRank = "class",
      rightRank = NULL
    ))
  
  expect_equal(result3, "ISSUE_OPEN")
  
  # Test with only rightRank - issue closed
  result4 <- wrong_rank_report(
    list(
      name = "Amphibia",
      wrongRank = NULL,
      rightRank = "class"
    ))
  
  expect_equal(result4, "ISSUE_CLOSED")
})

test_that("wrong_rank_report handles errors", {
  
  # Test with only rightRank - error (doesn't match)
  result <- wrong_rank_report(
    list(
      name = "Amphibia",
      wrongRank = NULL,
      rightRank = "kingdom"
    ))
  
  expect_equal(result, "JSON-TAG-ERROR")
  
  # Test with non-existent name
  result2 <- wrong_rank_report(
    list(
      name = "Doggggg",
      wrongRank = "species",
      rightRank = "genus"
    ))
  
  expect_equal(result2, "JSON-TAG-ERROR")
  
  # Test with only wrongRank - error (doesn't match)
  result3 <- wrong_rank_report(
    list(
      name = "Animalia",
      wrongRank = "phylum",
      rightRank = NULL
    ))
  
  expect_equal(result3, "JSON-TAG-ERROR")
})
