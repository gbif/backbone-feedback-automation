#' Check if a taxon has the wrong basionym
#'
#' Internal function to verify whether a taxon has the correct basionym in the
#' COL Backbone. The basionym is read from the taxon's explicit "basionym" name
#' relation in ChecklistBank, not inferred from its synonyms.
#'
#' @param xx A list containing name, wrongBasionym, and/or rightBasionym
#' @return A character string indicating the issue status: "ISSUE_CLOSED" if the
#'   taxon has the expected basionym, "ISSUE_OPEN" if it does not yet, or
#'   "JSON-TAG-ERROR" if validation fails
#'
#' @details
#' A JSON basionym matches a COL basionym only when it equals the full label
#' including authorship. The comparison is exact: case and whitespace must
#' match.
#' \itemize{
#'   \item With both fields: closed when the basionym is \code{rightBasionym}
#'         and not \code{wrongBasionym}, otherwise open
#'   \item With only \code{wrongBasionym}: open while it is still the basionym
#'   \item With only \code{rightBasionym}: closed once it is the basionym
#' }
#' A taxon with no recorded basionym counts as having neither basionym. When
#' the name matches several taxa (homonyms), all are checked and the issue is
#' only closed when every match is closed.
#'
#' @keywords internal
#' @export
wrong_basionym = function(xx) {
    wrong_b = basionym_field(xx$wrongBasionym)
    right_b = basionym_field(xx$rightBasionym)
    if(is.null(wrong_b) && is.null(right_b)) {
        gbif_message("Neither wrongBasionym nor rightBasionym provided")
        return("JSON-TAG-ERROR")
    }

    result = name_exists(xx$name)
    if(!result$exists) return("JSON-TAG-ERROR")

    check_single_id = function(id) {
        b = get_basionyms(id)
        if(is.null(b)) return(NULL)

        has_wrong = basionym_matches(wrong_b, b)
        has_right = basionym_matches(right_b, b)

        if(!is.null(wrong_b) && !is.null(right_b)) {
            return(if(has_right && !has_wrong) "ISSUE_CLOSED" else "ISSUE_OPEN")
        }
        if(!is.null(wrong_b)) {
            return(if(has_wrong) "ISSUE_OPEN" else "ISSUE_CLOSED")
        }
        if(has_right) "ISSUE_CLOSED" else "ISSUE_OPEN"
    }

    ids = if(result$multiple) result$ids else result$id
    if(result$multiple) {
        gbif_message("WARNING: Multiple matches found for '", xx$name, "'. IDs: ", paste(ids, collapse = ", "))
    }

    statuses = unlist(lapply(ids, check_single_id))
    if(length(statuses) == 0) return("JSON-TAG-ERROR")

    # Conservative: if ANY match is still open, the issue is open
    if("ISSUE_OPEN" %in% statuses) return("ISSUE_OPEN")
    "ISSUE_CLOSED"
}

# Treat NULL, NA and empty strings in the JSON as "not provided"
basionym_field = function(x) {
    if(is.null(x) || length(x) == 0 || is.na(x[1]) || trimws(x[1]) == "") return(NULL)
    x[1]
}

# TRUE if the JSON basionym exactly equals any COL basionym label (full name
# with authorship). No case or whitespace normalization: issues may be about
# exactly that.
basionym_matches = function(json_value, basionyms) {
    if(is.null(json_value) || is.null(basionyms) || nrow(basionyms) == 0) return(FALSE)
    candidates = basionyms$label
    json_value %in% candidates[!is.na(candidates)]
}

#' Generate Readable Report for Wrong Basionym Issue
#'
#' Creates a human-readable report showing the basionym currently recorded for
#' a taxon in COL and whether it matches the expected basionym.
#'
#' @param xx A list containing issue data with \code{name}, and optionally
#'   \code{wrongBasionym} and/or \code{rightBasionym} fields
#'
#' @return Invisibly returns the result status ("ISSUE_CLOSED", "ISSUE_OPEN",
#'   or "JSON-TAG-ERROR")
#'
#' @export
#' @examples
#' \dontrun{
#' issue_data <- list(
#'   name = "Muusoctopus pseudonymus (Grimpe, 1922)",
#'   wrongBasionym = "Octopus levis Hoyle",
#'   rightBasionym = "Atlantoctopus pseudonymus Grimpe, 1922"
#' )
#' wrong_basionym_report(issue_data)
#' }
wrong_basionym_report <- function(xx) {
    cat("\n")
    cat("========================================\n")
    cat("WRONG BASIONYM ISSUE REPORT\n")
    cat("========================================\n\n")

    cat("Name:                 ", xx$name, "\n\n")

    result <- wrong_basionym(xx)

    cat("EXPECTED:\n")
    cat("----------------------------------------\n")
    wrong_b <- basionym_field(xx$wrongBasionym)
    right_b <- basionym_field(xx$rightBasionym)
    if(!is.null(wrong_b)) cat("  Wrong Basionym:     ", wrong_b, "\n")
    if(!is.null(right_b)) cat("  Right Basionym:     ", right_b, "\n")
    cat("\n")

    name_result <- name_exists(xx$name)
    if(!name_result$exists) {
        cat("STATUS:               ERROR - Name not found in COL\n")
        cat("\n========================================\n\n")
        return(invisible(result))
    }

    cat("FOUND IN COL:\n")
    cat("----------------------------------------\n")
    ids <- if(name_result$multiple) name_result$ids else name_result$id
    for(id in ids) {
        cat("  COL ID:             ", id, "\n")
        b <- get_basionyms(id)
        if(is.null(b)) {
            cat("  Basionym:           (Unable to retrieve)\n\n")
            next
        }
        if(nrow(b) == 0) {
            cat("  Basionym:           (none recorded)\n")
        } else {
            cat("  Basionym:           ", paste(b$label, collapse = "; "), "\n")
        }
        if(!is.null(wrong_b)) {
            cat("  Has wrong basionym: ", ifelse(basionym_matches(wrong_b, b), "YES", "NO"), "\n")
        }
        if(!is.null(right_b)) {
            cat("  Has right basionym: ", ifelse(basionym_matches(right_b, b), "YES", "NO"), "\n")
        }
        cat("\n")
    }

    cat("RESULT:               ")
    if(result == "ISSUE_CLOSED") {
        cat("OK ISSUE CLOSED - Taxon has correct basionym\n")
    } else if(result == "ISSUE_OPEN") {
        cat("ERROR ISSUE OPEN - Taxon does not have the expected basionym\n")
    } else {
        cat("WARNING ERROR - Unable to validate basionym\n")
    }

    cat("\n========================================\n\n")

    invisible(result)
}
