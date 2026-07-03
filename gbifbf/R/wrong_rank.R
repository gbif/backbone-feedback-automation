#' Check if a taxon has the wrong rank
#'
#' Internal function to verify whether a taxon has the correct taxonomic rank
#' in the COL Backbone. Checks if the taxon has moved from wrongRank to rightRank.
#'
#' @param xx A list containing name, wrongRank, and/or rightRank
#' @return A character string indicating the issue status: "ISSUE_CLOSED" if the
#'   taxon has the correct rank, "ISSUE_OPEN" if it still has the wrong rank,
#'   or "JSON-TAG-ERROR" if validation fails
#'
#' @details
#' This function handles multiple matching taxa when the same name exists across
#' different nomenclatural codes (homonyms). When multiple matches exist:
#' \itemize{
#'   \item All matching IDs are checked for rank consistency
#'   \item Conservative approach: issue is only marked ISSUE_CLOSED when ALL matches
#'         have the correct rank
#'   \item If ANY match still has ISSUE_OPEN, the overall status is ISSUE_OPEN
#' }
#'
#' @keywords internal
#' @export
wrong_rank = function(xx) {
    # Check if name exists using multi-strategy search
    result = name_exists(xx$name)
    if(!result$exists) return("JSON-TAG-ERROR")
    
    # Internal helper to check a single ID
    check_single_id = function(id, name_for_msg = NULL) {
        if(!is.null(name_for_msg)) {
            gbif_message("Checking ID: ", id, " for '", name_for_msg, "'")
        }
        
        # Get taxon details by ID
        n = cb_get_taxon_by_id(id)
        if(nrow(n) == 0 || !("rank" %in% names(n))) return(NULL)
        
        r = n$rank[1]
        if(is.null(r) || is.na(r)) return(NULL)
        
        if(!is.null(xx$wrongRank) & !is.null(xx$rightRank)) {
            if(toupper(r) == toupper(xx$wrongRank)) {
                return("ISSUE_OPEN")
            } else if (toupper(r) == toupper(xx$rightRank)) {
                return("ISSUE_CLOSED")
            } else {
                return("JSON-TAG-ERROR")
            }
        }
        if(!is.null(xx$wrongRank) & is.null(xx$rightRank)) {
            if(toupper(r) == toupper(xx$wrongRank)) {
                return("ISSUE_OPEN")
            } else {
                return("JSON-TAG-ERROR")
            }
        }
        if(is.null(xx$wrongRank) & !is.null(xx$rightRank)) {
            if(toupper(r) == toupper(xx$rightRank)) {
                return("ISSUE_CLOSED")
            } else {
                return("JSON-TAG-ERROR")
            }
        }
        return(NULL)
    }
    
    # Check for multiple matches - verify ALL matching taxa
    if(result$multiple) {
        gbif_message("WARNING: Multiple matches found for '", xx$name, "'. IDs: ", paste(result$ids, collapse = ", "))
        gbif_message("Checking all ", length(result$ids), " matches to ensure consistency")
        
        # Check each ID
        statuses = sapply(result$ids, function(id) check_single_id(id, xx$name))
        
        # Filter out NULL results (errors)
        valid_statuses = statuses[!sapply(statuses, is.null)]
        
        if(length(valid_statuses) == 0) {
            gbif_message("ERROR: Could not verify any of the multiple matches")
            return("JSON-TAG-ERROR")
        }
        
        # Check if all valid statuses agree
        unique_statuses = unique(valid_statuses)
        if(length(unique_statuses) > 1) {
            gbif_message("WARNING: Multiple matches have different rank statuses!")
            gbif_message("Statuses: ", paste(result$ids, "=", statuses, collapse = ", "))
            gbif_message("Conservative approach: Issue only closed when ALL matches are closed.")
            
            # If ANY match has ISSUE_OPEN, the overall issue is still OPEN
            # Only return ISSUE_CLOSED if all matches are ISSUE_CLOSED
            if("ISSUE_OPEN" %in% valid_statuses) {
                gbif_message("At least one match still has ISSUE_OPEN - returning ISSUE_OPEN")
                return("ISSUE_OPEN")
            } else if("JSON-TAG-ERROR" %in% valid_statuses) {
                # If some are ISSUE_CLOSED and some are ERROR, return ERROR
                gbif_message("Some matches have errors - returning JSON-TAG-ERROR")
                return("JSON-TAG-ERROR")
            } else {
                # All are ISSUE_CLOSED
                gbif_message("All matches are ISSUE_CLOSED - returning ISSUE_CLOSED")
                return("ISSUE_CLOSED")
            }
        } else {
            gbif_message("All ", length(valid_statuses), " matches agree: ", unique_statuses[1])
            return(unique_statuses[1])
        }
    }
    
    # Single match case - use original logic
    out = check_single_id(result$id)
    if(is.null(out)) return("JSON-TAG-ERROR")
    
    return(out)
}

#' Generate Readable Report for Wrong Rank Issue
#'
#' Creates a human-readable report showing the status of a taxonomic rank
#' issue, including the taxon's current rank and whether it has been changed
#' to the correct rank.
#'
#' @param xx A list containing issue data with \code{name}, and optionally
#'   \code{wrongRank} and/or \code{rightRank} fields
#'
#' @return Invisibly returns the result status ("ISSUE_CLOSED", "ISSUE_OPEN",
#'   or "JSON-TAG-ERROR")
#'
#' @details
#' This function prints a formatted report to the console showing:
#' \itemize{
#'   \item The taxon name being checked
#'   \item Expected wrong and right ranks
#'   \item Whether the name exists in COL
#'   \item Current rank assigned to the taxon
#'   \item Whether the rank matches expectations
#'   \item Overall result (ISSUE_CLOSED/OPEN/ERROR)
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' issue_data <- list(
#'   name = "Aus bus",
#'   wrongRank = "GENUS",
#'   rightRank = "SPECIES"
#' )
#' wrong_rank_report(issue_data)
#' }
wrong_rank_report <- function(xx) {
    # Print header
    cat("\n")
    cat("========================================\n")
    cat("WRONG RANK ISSUE REPORT\n")
    cat("========================================\n\n")
    
    # Print name being checked
    cat("Name:                 ", xx$name, "\n\n")
    
    # Get the result from wrong_rank
    result <- wrong_rank(xx)
    
    # Check if name exists
    name_result <- name_exists(xx$name)
    if(!name_result$exists) {
        cat("STATUS:               ERROR - Name not found in COL\n\n")
        
        # Try base name search
        cat("TRYING BASE NAME SEARCH:\n")
        cat("----------------------------------------\n")
        parsed <- cb_name_parser(q = xx$name)
        base_name <- parsed$scientificName
        
        if(!is.null(base_name) && base_name != "" && base_name != xx$name) {
            cat("  Base Name:          ", base_name, "\n")
            
            # Search with base name
            base_result <- name_exists(base_name)
            
            if(base_result$exists) {
                cat("  Found in COL:       YES\n")
                if(base_result$multiple) {
                    cat("  Multiple Matches:   ", paste(base_result$ids, collapse = ", "), "\n")
                } else {
                    cat("  COL ID:             ", base_result$id, "\n")
                }
                
                # Get details for the first match
                n <- cb_get_taxon_by_id(base_result$id)
                if(nrow(n) > 0 && "rank" %in% names(n)) {
                    cat("  Rank:               ", toupper(n$rank[1]), "\n")
                }
                
                cat("\n  Note: This is the base name without authorship.\n")
                cat("        The exact name '", xx$name, "' was not found.\n", sep = "")
            } else {
                cat("  Found in COL:       NO\n")
                cat("  Base name also not found.\n")
            }
        } else {
            cat("  Unable to extract base name or base name same as original.\n")
        }
        
        cat("\n========================================\n\n")
        return(invisible("JSON-TAG-ERROR"))
    }
    
    # Print expectations section
    cat("EXPECTED:\n")
    cat("----------------------------------------\n")
    if(!is.null(xx$wrongRank)) {
        cat("  Wrong Rank:         ", toupper(xx$wrongRank), "\n")
    }
    if(!is.null(xx$rightRank)) {
        cat("  Right Rank:         ", toupper(xx$rightRank), "\n")
    }
    cat("\n")
    
    # Get current information from COL
    cat("FOUND IN COL:\n")
    cat("----------------------------------------\n")
    
    if(name_result$multiple) {
        cat("  Multiple Matches:   ", paste(name_result$ids, collapse = ", "), "\n\n")
        
        # Check each ID
        for(id in name_result$ids) {
            cat("  ID ", id, ":\n", sep = "")
            
            # Get taxon details
            n <- cb_get_taxon_by_id(id)
            
            if(nrow(n) > 0 && "rank" %in% names(n)) {
                r <- n$rank[1]
                cat("    Current Rank:     ", toupper(r), "\n")
                
                # Check wrong rank
                if(!is.null(xx$wrongRank)) {
                    has_wrong_rank <- toupper(r) == toupper(xx$wrongRank)
                    cat("    Has wrong rank:   ", ifelse(has_wrong_rank, "YES", "NO"), "\n")
                }
                
                # Check right rank
                if(!is.null(xx$rightRank)) {
                    has_right_rank <- toupper(r) == toupper(xx$rightRank)
                    cat("    Has right rank:   ", ifelse(has_right_rank, "YES", "NO"), "\n")
                }
            } else {
                cat("    Current Rank:     (Unable to retrieve)\n")
            }
            cat("\n")
        }
    } else {
        cat("  COL ID:             ", name_result$id, "\n")
        
        # Get taxon details
        n <- cb_get_taxon_by_id(name_result$id)
        
        if(nrow(n) == 0 || !("rank" %in% names(n))) {
            cat("  Current Rank:       (Unable to retrieve)\n\n")
            cat("RESULT:               ⚠ ERROR - Unable to validate rank\n")
            cat("\n========================================\n\n")
            return(invisible("JSON-TAG-ERROR"))
        }
        
        r <- n$rank[1]
        
        if(is.null(r) || is.na(r)) {
            cat("  Current Rank:       (Unable to retrieve)\n\n")
            cat("RESULT:               ⚠ ERROR - Unable to validate rank\n")
            cat("\n========================================\n\n")
            return(invisible("JSON-TAG-ERROR"))
        }
        
        cat("  Current Rank:       ", toupper(r), "\n\n")
    }
    
    # Print validation results
    cat("VALIDATION:\n")
    cat("----------------------------------------\n")
    
    if(!name_result$multiple) {
        # Single match
        n <- cb_get_taxon_by_id(name_result$id)
        r <- n$rank[1]
        
        if(!is.null(xx$wrongRank)) {
            has_wrong_rank <- toupper(r) == toupper(xx$wrongRank)
            cat("  Has wrong rank:     ", ifelse(has_wrong_rank, "FAIL (still wrong rank)", "PASS (not wrong rank)"), "\n")
        }
        
        if(!is.null(xx$rightRank)) {
            has_right_rank <- toupper(r) == toupper(xx$rightRank)
            cat("  Has right rank:     ", ifelse(has_right_rank, "PASS", "FAIL (not right rank)"), "\n")
        }
    } else {
        # Multiple matches
        cat("  Checked all ", length(name_result$ids), " matching IDs\n", sep = "")
        
        # Check each ID's status
        for(id in name_result$ids) {
            n <- cb_get_taxon_by_id(id)
            
            if(nrow(n) > 0 && "rank" %in% names(n)) {
                r <- n$rank[1]
                
                if(!is.null(xx$wrongRank) && !is.null(xx$rightRank)) {
                    has_wrong_rank <- toupper(r) == toupper(xx$wrongRank)
                    has_right_rank <- toupper(r) == toupper(xx$rightRank)
                    
                    if(has_right_rank) {
                        cat("    ID ", id, ": has right rank - PASS\n", sep = "")
                    } else if(has_wrong_rank) {
                        cat("    ID ", id, ": still has wrong rank - FAIL\n", sep = "")
                    } else {
                        cat("    ID ", id, ": unexpected rank - ERROR\n", sep = "")
                    }
                } else if(!is.null(xx$rightRank)) {
                    has_right_rank <- toupper(r) == toupper(xx$rightRank)
                    cat("    ID ", id, ": ", ifelse(has_right_rank, "has right rank - PASS", "does not have right rank - FAIL"), "\n", sep = "")
                } else if(!is.null(xx$wrongRank)) {
                    has_wrong_rank <- toupper(r) == toupper(xx$wrongRank)
                    cat("    ID ", id, ": ", ifelse(!has_wrong_rank, "not wrong rank - PASS", "still wrong rank - FAIL"), "\n", sep = "")
                }
            }
        }
    }
    
    cat("\n")
    
    # Print final result
    cat("RESULT:               ")
    if(result == "ISSUE_CLOSED") {
        cat("✓ ISSUE CLOSED - Taxon has correct rank\n")
    } else if(result == "ISSUE_OPEN") {
        cat("✗ ISSUE OPEN - Taxon still has wrong rank\n")
    } else {
        cat("⚠ ERROR - Unable to validate rank\n")
    }
    
    cat("\n========================================\n\n")
    
    invisible(result)
}
