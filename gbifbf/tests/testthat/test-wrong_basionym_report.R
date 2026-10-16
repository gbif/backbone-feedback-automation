library(testthat)
library(gbifbf)

test_that("wrong_basionym_report returns correct status and prints output", {
  skip_on_ci()

  expect_output(result <- wrong_basionym_report(list(
    name = "Calopteryx splendens (Harris, 1780)",
    wrongBasionym = "Agrion splendens",
    rightBasionym = "Libellula splendens Harris, 1780"
  )), "WRONG BASIONYM ISSUE REPORT")
  expect_equal(result, "ISSUE_CLOSED")

  expect_output(result2 <- wrong_basionym_report(list(
    name = "Calopteryx splendens (Harris, 1780)",
    wrongBasionym = "Libellula splendens Harris, 1780"
  )), "Libellula splendens Harris, 1780")
  expect_equal(result2, "ISSUE_OPEN")

  expect_output(result3 <- wrong_basionym_report(list(
    name = "Doggggg",
    rightBasionym = "Libellula splendens Harris, 1780"
  )), "Name not found")
  expect_equal(result3, "JSON-TAG-ERROR")
})
