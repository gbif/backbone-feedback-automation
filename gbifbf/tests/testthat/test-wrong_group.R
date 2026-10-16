library(testthat)
library(gbifbf)

# wrong group 
test_that("wrong_group", {
    skip_on_ci()

expect_equal(
    wrong_group(
    list(
    name = "Amphibia",
    wrongGroup = "Plantae", 
    rightGroup = "Animalia"
    )),
    "ISSUE_CLOSED")

expect_equal(
    wrong_group(
    list(
    name = "Amphibia",
    wrongGroup = "Animalia",
    rightGroup = "Plantae"
    )),
    "ISSUE_OPEN")

expect_equal(
    wrong_group(
    list(
    name = "Doggg",
    wrongGroup = "Animalia",
    rightGroup = "Plantae"
    )),
    "JSON-TAG-ERROR")
    
expect_equal(
    wrong_group(
    list(
    name = "Amphibia",
    wrongGroup = NULL,
    rightGroup = "Animalia"
    )),
    "ISSUE_CLOSED")

# Test with only wrongGroup specified
expect_equal(
    wrong_group(
    list(
    name = "Amphibia",
    wrongGroup = "Plantae",
    rightGroup = NULL
    )),
    "ISSUE_CLOSED")

# Test with wrongGroup that is actually in the classification
expect_equal(
    wrong_group(
    list(
    name = "Amphibia",
    wrongGroup = "Animalia",
    rightGroup = NULL
    )),
    "ISSUE_OPEN")

# Check the current COL classification for the synonym Lycaena helloides
expect_equal(
    wrong_group(
    list(
    name = "Lycaena helloides (Boisduval, 1852)",
    wrongGroup = "Sesia Fabricius, 1775",
    rightGroup = "Tharsalea (Epidemia) Scudder, 1876"
    )),
    "ISSUE_CLOSED")

})
