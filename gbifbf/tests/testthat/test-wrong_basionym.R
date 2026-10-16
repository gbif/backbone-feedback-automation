library(testthat)
library(gbifbf)

# Calopteryx splendens (Harris, 1780) has basionym Libellula splendens Harris, 1780
test_that("wrong_basionym detects the right basionym", {
  skip_on_ci()

  expect_equal(wrong_basionym(list(
    name = "Calopteryx splendens (Harris, 1780)",
    rightBasionym = "Libellula splendens Harris, 1780"
  )), "ISSUE_CLOSED")

  # Name without authorship does not match
  expect_equal(wrong_basionym(list(
    name = "Calopteryx splendens (Harris, 1780)",
    rightBasionym = "Libellula splendens"
  )), "ISSUE_OPEN")

  # Exact comparison: case and whitespace differences do not match
  expect_equal(wrong_basionym(list(
    name = "Calopteryx splendens (Harris, 1780)",
    rightBasionym = "libellula splendens Harris, 1780"
  )), "ISSUE_OPEN")

  expect_equal(wrong_basionym(list(
    name = "Calopteryx splendens (Harris, 1780)",
    rightBasionym = "Libellula  splendens Harris, 1780"
  )), "ISSUE_OPEN")

  expect_equal(wrong_basionym(list(
    name = "Calopteryx splendens (Harris, 1780)",
    wrongBasionym = "Libellula splendens Harris, 1780 "
  )), "ISSUE_CLOSED")

  expect_equal(wrong_basionym(list(
    name = "Calopteryx splendens (Harris, 1780)",
    wrongBasionym = "Agrion splendens",
    rightBasionym = "Libellula splendens Harris, 1780"
  )), "ISSUE_CLOSED")

  # Empty wrongBasionym is ignored
  expect_equal(wrong_basionym(list(
    name = "Calopteryx splendens (Harris, 1780)",
    wrongBasionym = "",
    rightBasionym = "Libellula splendens Harris, 1780"
  )), "ISSUE_CLOSED")
})

test_that("wrong_basionym detects a wrong basionym", {
  skip_on_ci()

  expect_equal(wrong_basionym(list(
    name = "Calopteryx splendens (Harris, 1780)",
    wrongBasionym = "Libellula splendens Harris, 1780"
  )), "ISSUE_OPEN")

  expect_equal(wrong_basionym(list(
    name = "Calopteryx splendens (Harris, 1780)",
    rightBasionym = "Agrion splendens"
  )), "ISSUE_OPEN")

  expect_equal(wrong_basionym(list(
    name = "Calopteryx splendens (Harris, 1780)",
    wrongBasionym = "Libellula splendens Harris, 1780",
    rightBasionym = "Agrion splendens"
  )), "ISSUE_OPEN")

  # Only wrongBasionym, and it is not the basionym -> closed
  expect_equal(wrong_basionym(list(
    name = "Calopteryx splendens (Harris, 1780)",
    wrongBasionym = "Agrion splendens"
  )), "ISSUE_CLOSED")
})

test_that("wrong_basionym treats NULL basionym fields as not provided", {
  skip_on_ci()

  expect_equal(wrong_basionym(list(
    name = "Calopteryx splendens (Harris, 1780)",
    wrongBasionym = NULL,
    rightBasionym = "Libellula splendens Harris, 1780"
  )), "ISSUE_CLOSED")

  expect_equal(wrong_basionym(list(
    name = "Calopteryx splendens (Harris, 1780)",
    wrongBasionym = NULL,
    rightBasionym = "Agrion splendens"
  )), "ISSUE_OPEN")

  expect_equal(wrong_basionym(list(
    name = "Calopteryx splendens (Harris, 1780)",
    wrongBasionym = "Libellula splendens Harris, 1780",
    rightBasionym = NULL
  )), "ISSUE_OPEN")

  expect_equal(wrong_basionym(list(
    name = "Calopteryx splendens (Harris, 1780)",
    wrongBasionym = "Agrion splendens",
    rightBasionym = NULL
  )), "ISSUE_CLOSED")
})

test_that("wrong_basionym rejects two NULL basionym fields", {
  expect_equal(wrong_basionym(list(
    name = "Calopteryx splendens (Harris, 1780)",
    wrongBasionym = NULL,
    rightBasionym = NULL
  )), "JSON-TAG-ERROR")
})

test_that("wrong_basionym handles errors", {
  skip_on_ci()

  expect_equal(wrong_basionym(list(
    name = "Doggggg",
    rightBasionym = "Libellula splendens Harris, 1780"
  )), "JSON-TAG-ERROR")

  expect_equal(wrong_basionym(list(
    name = "Calopteryx splendens (Harris, 1780)",
    wrongBasionym = "",
    rightBasionym = ""
  )), "JSON-TAG-ERROR")
})
