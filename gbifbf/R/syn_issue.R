#' Check Synonym Status Issues
#'
#' Internal function to verify whether a taxon's synonym status or parent
#' relationship has been corrected in the GBIF Backbone. Handles complex
#' validation of taxonomic status and parent-child relationships.
#'
#' @param xx A list containing issue data with \code{name}, and optionally
#'   \code{wrongStatus}, \code{rightStatus}, \code{wrongParent}, and/or
#'   \code{rightParent} fields
#' 
#' @return Character string: "ISSUE_CLOSED" if the synonym issue has been
#'   resolved, "ISSUE_OPEN" if the problem persists, or "JSON-TAG-ERROR"
#'   if the name cannot be resolved
#'
#' @details
#' This function validates whether a taxon has the correct taxonomic status
#' (e.g., accepted, synonym) and/or is listed as a synonym of the correct
#' parent taxon. It implements sophisticated logic to handle:
#' \itemize{
#'   \item Status validation (wrongStatus vs rightStatus)
#'   \item Parent relationship validation (wrongParent vs rightParent)
#'   \item Base name fallback for author string issues
#'   \item Alternative name lookup
#'   \item Combined status + parent validation
#' }
#'
#' The function checks if a name is listed as a synonym under the specified
#' parent taxa and whether it has the expected taxonomic status.
#'
#' @keywords internal
#' @export
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck
#' @importFrom tibble tibble
syn_issue = function(xx) {
    # Check if the name exists using multi-strategy search
    name_result = name_exists(xx$name)
    if(!name_result$exists) return("JSON-TAG-ERROR")
    
    # Handle multiple matches - check all IDs conservatively
    if(name_result$multiple) {
        gbif_message("Multiple matches found for name '", xx$name, "'. Checking all IDs: ", paste(name_result$ids, collapse = ", "))
        
        # Helper function to check a single ID
        check_single_id <- function(id, name_for_msg) {
            n = cb_get_taxon_by_id(id)
            if(nrow(n) == 0) return("JSON-TAG-ERROR")
            
            current_status = n$status[1]
            
            # check right parent 
            if(!is.null(xx$rightParent)) {
                rp_result = name_exists(xx$rightParent)
                if(!rp_result$exists) {
                    gbif_message("rightParent not found in backbone")
                    return("JSON-TAG-ERROR")
                }
                # Check if xx$name is in the synonyms of rightParent
                rp = xx$name %in% get_syns(rp_result$id)
            } else {
                rp = NULL
            }

            # check wrong parent 
            if(!is.null(xx$wrongParent)) {
                wp_result = name_exists(xx$wrongParent)
                if(!wp_result$exists) {
                    gbif_message("wrongParent not found in backbone - treating as FALSE (issue may be fixed)")
                    wp = FALSE
                } else {
                    # Check if xx$name is in the synonyms of wrongParent
                    wp = xx$name %in% get_syns(wp_result$id)
                }
            } else {
                wp = NULL
            }
            if(!is.null(xx$wrongStatus)) {
                ws = current_status == tolower(xx$wrongStatus)
            } else {
                ws = NULL
            }
            if(!is.null(xx$rightStatus)) {
                rs = current_status == tolower(xx$rightStatus)
            } else {
                rs = NULL
            }
            
            # get right status 
            if(is.null(rs) && is.null(ws)) {
                rrs = NULL
            }
            if(is.null(rs) && !is.null(wp)) {
                rrs = ifelse(!wp, TRUE, FALSE)
            }
            if(!is.null(rs) && is.null(ws)) {
                rrs = ifelse(rs, TRUE, FALSE)
            } 
            if(!is.null(rs) && !is.null(ws)) {
                rrs = ifelse(rs && !ws, TRUE, FALSE)
            }

            if(is.null(rp) && is.null(wp)) {
                rrp = NULL
            }
            # get right parent
            if(is.null(rp) && !is.null(wp)) {
                rrp = ifelse(!wp, TRUE, FALSE)
            }
            if(!is.null(rp) && is.null(wp)) {
                rrp = ifelse(rp, TRUE, FALSE)
            }
            if(!is.null(rp) && !is.null(wp)) {
                rrp = ifelse(rp && !wp, TRUE, FALSE)
            }

            # issue open or closed logic 
            if(is.null(rrp)) {
                out = ifelse(rrs, "ISSUE_CLOSED", "ISSUE_OPEN")    
            }
            if(!is.null(rrp) && !is.null(rrs)) {
              out = ifelse(rrs && rrp, "ISSUE_CLOSED", "ISSUE_OPEN")
            } 
            if(!is.null(rrp) && is.null(rrs)) {
                out = ifelse(rrp, "ISSUE_CLOSED", "ISSUE_OPEN")
            }
            return(out)
        }
        
        # Check all matching IDs - conservative: if ANY is OPEN, return OPEN
        statuses <- sapply(name_result$ids, check_single_id)
        valid_statuses <- statuses[statuses %in% c("ISSUE_OPEN", "ISSUE_CLOSED")]
        
        if(length(valid_statuses) == 0) {
            return("JSON-TAG-ERROR")
        }
        
        # Conservative: if ANY has ISSUE_OPEN, return ISSUE_OPEN
        if("ISSUE_OPEN" %in% valid_statuses) {
            gbif_message("At least one matching ID has ISSUE_OPEN - issue remains open")
            return("ISSUE_OPEN")
        }
        
        return("ISSUE_CLOSED")
    }
    
    # Single match - original logic
    # Get full details by ID
    n = cb_get_taxon_by_id(name_result$id)
    if(nrow(n) == 0) return("JSON-TAG-ERROR")
    
    current_status = n$status[1]
    
    if(is.null(xx$rightStatus) && is.null(xx$wrongStatus)) {
        gbif_message("Ignoring rightStatus and wrongStatus")
    }
    
    # check right parent 
    if(!is.null(xx$rightParent)) {
        rp_result = name_exists(xx$rightParent)
        if(!rp_result$exists) {
            gbif_message("rightParent not found in backbone")
            return("JSON-TAG-ERROR")
        }
        # Check if xx$name is in the synonyms of rightParent
        rp = xx$name %in% get_syns(rp_result$id)
    } else {
        rp = NULL
    }

    # check wrong parent 
    if(!is.null(xx$wrongParent)) {
        wp_result = name_exists(xx$wrongParent)
        if(!wp_result$exists) {
            gbif_message("wrongParent not found in backbone - treating as FALSE (issue may be fixed)")
            wp = FALSE
        } else {
            # Check if xx$name is in the synonyms of wrongParent
            wp = xx$name %in% get_syns(wp_result$id)
        }
    } else {
        wp = NULL
    }
    if(!is.null(xx$wrongStatus)) {
        ws = current_status == tolower(xx$wrongStatus)
    } else {
        ws = NULL
    }
    if(!is.null(xx$rightStatus)) {
        rs = current_status == tolower(xx$rightStatus)
    } else {
        rs = NULL
    }
    
    # get right status 
    if(is.null(rs) && is.null(ws)) {
        rrs = NULL
    }
    if(is.null(rs) && !is.null(wp)) {
        rrs = ifelse(!wp, TRUE, FALSE)
    }
    if(!is.null(rs) && is.null(ws)) {
        rrs = ifelse(rs, TRUE, FALSE)
    } 
    if(!is.null(rs) && !is.null(ws)) {
        rrs = ifelse(rs && !ws, TRUE, FALSE)
    }
    
    # if(!is.null(rrs)) cat("right right status: ",rrs,"\n")

    if(is.null(rp) && is.null(wp)) {
        rrp = NULL
    }
    # get right parent
    if(is.null(rp) && !is.null(wp)) {
        rrp = ifelse(!wp, TRUE, FALSE)
    }
    if(!is.null(rp) && is.null(wp)) {
        rrp = ifelse(rp, TRUE, FALSE)
    }
    if(!is.null(rp) && !is.null(wp)) {
        rrp = ifelse(rp && !wp, TRUE, FALSE)
    }

    # cat("right right parent: ",rrp,"\n")

    # issue open or closed logic 
    if(is.null(rrp)) {
        out = ifelse(rrs, "ISSUE_CLOSED", "ISSUE_OPEN")    
    }
    if(!is.null(rrp) && !is.null(rrs)) {
      out = ifelse(rrs && rrp, "ISSUE_CLOSED", "ISSUE_OPEN")
    } 
    if(!is.null(rrp) && is.null(rrs)) {
        out = ifelse(rrp, "ISSUE_CLOSED", "ISSUE_OPEN")
    }
    return(out)
}

#' Generate Readable Report for Synonym Issue
#'
#' Creates a human-readable report showing the status of a synonym issue,
#' including what was expected and what was actually found in the GBIF Backbone.
#'
#' @param xx A list containing issue data with \code{name}, and optionally
#'   \code{wrongStatus}, \code{rightStatus}, \code{wrongParent}, and/or
#'   \code{rightParent} fields
#'
#' @return Invisibly returns the result status ("ISSUE_CLOSED", "ISSUE_OPEN",
#'   or "JSON-TAG-ERROR")
#'
#' @details
#' This function prints a formatted report to the console showing:
#' \itemize{
#'   \item The name being checked
#'   \item Expected status and parent relationships
#'   \item Current status and parent relationships found in GBIF
#'   \item Whether each validation passed or failed
#'   \item Overall result (ISSUE_CLOSED/OPEN/ERROR)
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' issue_data <- list(
#'   name = "Aus bus",
#'   wrongStatus = "ACCEPTED",
#'   rightStatus = "SYNONYM",
#'   rightParent = "Aus cus"
#' )
#' syn_issue_report(issue_data)
#' }
syn_issue_report <- function(xx) {
    # Print header
    cat("\n")
    cat("========================================\n")
    cat("SYNONYM ISSUE REPORT\n")
    cat("========================================\n\n")
    
    # Print name being checked
    cat("Name:                 ", xx$name, "\n\n")
    
    # Get the result from syn_issue
    result <- syn_issue(xx)
    
    # If name doesn't exist, print error and return
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
                if(nrow(n) > 0 && "status" %in% names(n)) {
                    cat("  Status:             ", toupper(n$status[1]), "\n")
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
    if(!is.null(xx$wrongStatus)) {
        cat("  Wrong Status:       ", xx$wrongStatus, "\n")
    }
    if(!is.null(xx$rightStatus)) {
        cat("  Right Status:       ", xx$rightStatus, "\n")
    }
    if(!is.null(xx$wrongParent)) {
        cat("  Wrong Parent:       ", xx$wrongParent, "\n")
    }
    if(!is.null(xx$rightParent)) {
        cat("  Right Parent:       ", xx$rightParent, "\n")
    }
    cat("\n")
    
    # Get current information from GBIF
    cat("FOUND IN COL:\n")
    cat("----------------------------------------\n")
    
    if(name_result$multiple) {
        cat("  Multiple Matches:   ", paste(name_result$ids, collapse = ", "), "\n")
    } else {
        cat("  COL ID:             ", name_result$id, "\n")
    }
    
    # Get taxon details
    n <- cb_get_taxon_by_id(name_result$id)
    if(nrow(n) > 0) {
        current_status <- n$status[1]
        cat("  Current Status:     ", toupper(current_status), "\n")
        
        # Check parent relationships
        if(!is.null(xx$rightParent)) {
            rp_result <- name_exists(xx$rightParent)
            if(rp_result$exists) {
                is_syn_of_right <- xx$name %in% get_syns(rp_result$id)
                cat("  Is synonym of '", xx$rightParent, "': ", 
                    ifelse(is_syn_of_right, "YES", "NO"), "\n", sep = "")
                right_syns <- get_syns(rp_result$id)
                if(length(right_syns) > 0) {
                    cat("    Synonyms of '", xx$rightParent, "':\n", sep = "")
                    for(syn in right_syns) {
                        cat("      - ", syn, "\n", sep = "")
                    }
                } else {
                    cat("    (No synonyms listed)\n")
                }
            } else {
                cat("  Right parent not found in backbone\n")
            }
        }
        
        if(!is.null(xx$wrongParent)) {
            wp_result <- name_exists(xx$wrongParent)
            if(wp_result$exists) {
                is_syn_of_wrong <- xx$name %in% get_syns(wp_result$id)
                cat("  Is synonym of '", xx$wrongParent, "': ", 
                    ifelse(is_syn_of_wrong, "YES", "NO"), "\n", sep = "")
                wrong_syns <- get_syns(wp_result$id)
                if(length(wrong_syns) > 0) {
                    cat("    Synonyms of '", xx$wrongParent, "':\n", sep = "")
                    for(syn in wrong_syns) {
                        cat("      - ", syn, "\n", sep = "")
                    }
                } else {
                    cat("    (No synonyms listed)\n")
                }
            } else {
                cat("  Wrong parent not found in backbone (may be fixed)\n")
            }
        }
    }
    cat("\n")
    
    # Print validation results
    cat("VALIDATION:\n")
    cat("----------------------------------------\n")
    
    if(!is.null(xx$wrongStatus) && nrow(n) > 0) {
        has_wrong_status <- current_status == tolower(xx$wrongStatus)
        cat("  Has wrong status:   ", ifelse(has_wrong_status, "FAIL (still wrong)", "PASS (not wrong anymore)"), "\n")
    }
    
    if(!is.null(xx$rightStatus) && nrow(n) > 0) {
        has_right_status <- current_status == tolower(xx$rightStatus)
        cat("  Has right status:   ", ifelse(has_right_status, "PASS", "FAIL"), "\n")
    }
    
    if(!is.null(xx$rightParent)) {
        rp_result <- name_exists(xx$rightParent)
        if(rp_result$exists) {
            is_syn_of_right <- xx$name %in% get_syns(rp_result$id)
            cat("  Under right parent: ", ifelse(is_syn_of_right, "PASS", "FAIL"), "\n")
        }
    }
    
    if(!is.null(xx$wrongParent)) {
        wp_result <- name_exists(xx$wrongParent)
        if(wp_result$exists) {
            is_syn_of_wrong <- xx$name %in% get_syns(wp_result$id)
            cat("  Under wrong parent: ", ifelse(is_syn_of_wrong, "FAIL (still under wrong parent)", "PASS (removed from wrong parent)"), "\n")
        }
    }
    
    cat("\n")
    
    # Print final result
    cat("RESULT:               ")
    if(result == "ISSUE_CLOSED") {
        cat("✓ ISSUE CLOSED - All checks passed\n")
    } else if(result == "ISSUE_OPEN") {
        cat("✗ ISSUE OPEN - Problem persists\n")
    } else {
        cat("⚠ ERROR - Unable to validate\n")
    }
    
    cat("\n========================================\n\n")
    
    invisible(result)
}


