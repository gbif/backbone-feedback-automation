library(testthat)
library(gbifbf)

# syn issue
test_that("syn_issue", {

  expect_equal(
    syn_issue(
    list(
        name = "Agrion splendens (Harris, 1780)",
        wrongStatus = "ACCEPTED",
        rightStatus = "SYNONYM",
        rightParent = "Calopteryx splendens (Harris, 1780)",
        wrongParent = NULL
    )),  
    "ISSUE_CLOSED")

    expect_equal(
    syn_issue(
    list(
        name = "Agrion splendens (Harris, 1780)",
        wrongStatus = NULL,
        rightStatus = "ACCEPTED",
        rightParent = NULL,
        wrongParent = "Calopteryx splendens (Harris, 1780)"
    )),   
    "ISSUE_OPEN")

    expect_equal(
    syn_issue(
    list(
        name = "Agrion splendens (Harris, 1780)",
        wrongStatus = "ACCEPTED",
        rightStatus = "SYNONYM",
        rightParent = NULL,
        wrongParent = NULL
    )),  
    "ISSUE_CLOSED")

    expect_equal(
    syn_issue(
    list(
        name = "Agrion splendens (Harris, 1780)",
        wrongStatus = NULL,
        rightStatus = NULL,
        rightParent = "Calopteryx splendens (Harris, 1780)",
        wrongParent = NULL
    )),  
    "ISSUE_CLOSED")

    expect_equal(
      syn_issue(
        list(
          name = "Dog",
          wrongStatus = "ACCEPTED",
          rightStatus = "SYNONYM",
          rightParent = NULL,
          wrongParent = NULL
        )),  
      "JSON-TAG-ERROR"
    )

})

# Test synonym status variants (synonym genus, synonym species, etc.)
test_that("syn_issue handles synonym status variants in JSON", {
  
  # Test 1: rightStatus "SYNONYM" should match when actual is any synonym variant
  # Using a known synonym that should still be closed
  expect_equal(
    syn_issue(
      list(
        name = "Agrion splendens (Harris, 1780)",
        wrongStatus = "ACCEPTED",
        rightStatus = "SYNONYM",
        rightParent = "Calopteryx splendens (Harris, 1780)",
        wrongParent = NULL
      )),  
    "ISSUE_CLOSED"
  )
  
  # Test 2: JSON specifies "synonym genus" as rightStatus - should still match
  expect_equal(
    syn_issue(
      list(
        name = "Agrion splendens (Harris, 1780)",
        wrongStatus = "ACCEPTED",
        rightStatus = "synonym genus",
        rightParent = "Calopteryx splendens (Harris, 1780)",
        wrongParent = NULL
      )),  
    "ISSUE_CLOSED"
  )
  
  # Test 3: JSON specifies "synonym species" as rightStatus - should still match
  expect_equal(
    syn_issue(
      list(
        name = "Agrion splendens (Harris, 1780)",
        wrongStatus = "ACCEPTED",
        rightStatus = "synonym species",
        rightParent = "Calopteryx splendens (Harris, 1780)",
        wrongParent = NULL
      )),  
    "ISSUE_CLOSED"
  )
  
  # Test 4: JSON specifies "SYNONYM GENUS" (uppercase) - should still match
  expect_equal(
    syn_issue(
      list(
        name = "Agrion splendens (Harris, 1780)",
        wrongStatus = "ACCEPTED",
        rightStatus = "SYNONYM GENUS",
        rightParent = "Calopteryx splendens (Harris, 1780)",
        wrongParent = NULL
      )),  
    "ISSUE_CLOSED"
  )
  
  # Test 5: wrongStatus with variant "synonym genus" - should detect when NOT matching
  expect_equal(
    syn_issue(
      list(
        name = "Calopteryx splendens (Harris, 1780)",
        wrongStatus = "synonym genus",
        rightStatus = "ACCEPTED",
        rightParent = NULL,
        wrongParent = NULL
      )),  
    "ISSUE_CLOSED"
  )
  
})
