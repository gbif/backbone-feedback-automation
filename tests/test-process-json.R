library(testthat)

# Evaluate the production parsing block without loading the API client or checking taxa.
expressions <- parse("process_json.R")
assignment_names <- vapply(expressions, function(expr) {
  if (is.call(expr) && as.character(expr[[1]]) %in% c("<-", "=")) {
    return(as.character(expr[[2]]))
  }
  ""
}, character(1))
parsing <- expressions[seq(
  match("link", assignment_names),
  match("list_depth", assignment_names) - 1L
)]

parse_comment <- function(comment) {
  env <- new.env()
  env$original_string <- comment
  eval(parsing, envir = env)
  env$xx
}

links <- c(
  "https://github.com/gbif/backbone-feedback-automation/blob/master/JSON-comments-for-automation.md",
  "https://github.com/gbif/backbone-feedback/wiki/JSON-comments-for-automation-%E2%80%90-Experimental",
  "https://example.org/future-documentation"
)
json <- '{\n"missingName": "Heteropsectralis Hampson, 1897"\n}'
expected <- list(missingName = "Heteropsectralis Hampson, 1897")
marker <- "// json for auto-checking"

test_that("multiline comments accept current, legacy and future documentation links", {
  for (url in links) {
    link <- sprintf("[why is this here?](%s)", url)
    for (newline in c("\n", "\r\n")) {
      for (fenced in c(FALSE, TRUE)) {
        body <- paste(marker, json, sep = "\n")
        if (fenced) body <- paste("```json", body, "```", sep = "\n")
        body <- paste(body, link, "Additional explanation.", sep = "\n")
        body <- gsub("\n", newline, body, fixed = TRUE)
        expect_equal(parse_comment(body), expected)
      }
    }
  }
})

test_that("bare JSON and marker comments without links still work", {
  expect_equal(parse_comment(json), expected)
  expect_equal(parse_comment(paste(marker, json, sep = "\n")), expected)
  expect_equal(parse_comment(paste("```json", marker, json, "```", sep = "\n")), expected)
  expect_equal(parse_comment(paste(json, sprintf("[why is this here?](%s)", links[1]))), expected)
})

test_that("arrays of assertions are preserved", {
  body <- paste("```json", marker, paste0("[", json, ",", json, "]"), "```",
                sprintf("[why is this here?](%s)", links[1]), sep = "\n")
  expect_equal(parse_comment(body), list(expected, expected))
})

test_that("malformed JSON still raises a parsing error", {
  body <- paste(marker, '{"missingName": }',
                sprintf("[why is this here?](%s)", links[1]), sep = "\n")
  expect_error(parse_comment(body), "parse error")
})
