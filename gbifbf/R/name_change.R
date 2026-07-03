#' Check if a Name Change Has Been Implemented
#'
#' Internal function to verify whether a requested taxonomic name change
#' has been implemented in the GBIF Backbone. Handles complex validation
#' including fuzzy matching, base name fallbacks, and synonym checking.
#'
#' @param xx A list containing issue data with \code{currentName} and
#'   \code{proposedName} fields
#' 
#' @return Character string: "ISSUE_CLOSED" if the change has been implemented,
#'   "ISSUE_OPEN" if the current name still exists, or "JSON-TAG-ERROR" if
#'   the tag data is invalid or both names cannot be resolved
#'
#' @details
#' This function implements sophisticated name change detection with multiple
#' strategies:
#' \itemize{
#'   \item Direct exact match verification
#'   \item Fuzzy matching when authorship differs
#'   \item Base name fallback (stripping authorship)
#'   \item Alternative name checking
#'   \item Synonym relationship validation
#' }
#'
#' The function handles 8 distinct cases to determine issue status, including
#' scenarios where names are removed, renamed, or established as synonyms.
#'
#' @keywords internal
#' @export
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck
#' @importFrom tibble tibble
name_change = function(xx) {
    
    # Validate input
    if(is.null(xx$proposedName) || is.null(xx$currentName)) {
        return("JSON-TAG-ERROR")
    }
    if(xx$proposedName == xx$currentName) {
        return("JSON-TAG-ERROR")
    }
    
    # Check if both names exist using multi-strategy search
    cn_result = name_exists(xx$currentName)
    pn_result = name_exists(xx$proposedName)
    
    if(cn_result$multiple) {
        gbif_message("Multiple matches found for currentName '", xx$currentName, "'. Checking all IDs: ", paste(cn_result$ids, collapse = ", "))
    }
    if(pn_result$multiple) {
        gbif_message("Multiple matches found for proposedName '", xx$proposedName, "'. Checking all IDs: ", paste(pn_result$ids, collapse = ", "))
    }
    
    cn_exists = cn_result$exists
    pn_exists = pn_result$exists
    
    # CASE 1: currentName removed (doesn't exist) AND proposedName exists → CLOSED
    if(!cn_exists && pn_exists) {
        return("ISSUE_CLOSED")
    }
    
    # CASE 2: Neither name exists → ERROR (can't validate the change)
    if(!cn_exists && !pn_exists) {
        return("JSON-TAG-ERROR")
    }
    
    # CASE 3: currentName exists AND proposedName doesn't exist → OPEN
    if(cn_exists && !pn_exists) {
        return("ISSUE_OPEN")
    }
    
    # CASE 4: Both names exist - check all ID combinations
    if(cn_exists && pn_exists) {
        # Helper function to check a single ID pair
        check_synonym_relationship <- function(cn_id, pn_id) {
            # Get synonyms of the proposedName (accepted name)
            syns = get_syns(pn_id)
            # Check if currentName is listed as a synonym of proposedName
            return(xx$currentName %in% syns)
        }
        
        # Check all combinations of current and proposed name IDs
        # Conservative: issue is CLOSED only if ALL currentName IDs are synonyms of at least one proposedName ID
        all_closed = TRUE
        for(cn_id in cn_result$ids) {
            # Check if this currentName ID is a synonym of ANY proposedName ID
            is_syn_of_any = any(sapply(pn_result$ids, function(pn_id) {
                check_synonym_relationship(cn_id, pn_id)
            }))
            
            if(!is_syn_of_any) {
                # gbif_message("currentName ID ", cn_id, " is not a synonym of any proposedName ID")
                all_closed = FALSE
                break
            }
        }
        
        if(all_closed) {
            return("ISSUE_CLOSED")
        } else {
            return("ISSUE_OPEN")
        }
    }
    
    # Fallback (shouldn't reach here)
    return("JSON-TAG-ERROR")
}

#' Generate Readable Report for Name Change Issue
#'
#' Creates a human-readable report showing the status of a name change issue,
#' including whether the current name has been replaced by the proposed name
#' in the COL Backbone.
#'
#' @param xx A list containing issue data with \code{currentName} and
#'   \code{proposedName} fields
#'
#' @return Invisibly returns the result status ("ISSUE_CLOSED", "ISSUE_OPEN",
#'   or "JSON-TAG-ERROR")
#'
#' @details
#' This function prints a formatted report to the console showing:
#' \itemize{
#'   \item The current and proposed names being checked
#'   \item Whether each name exists in COL
#'   \item COL IDs for names that exist
#'   \item Whether current name is listed as a synonym of proposed name
#'   \item Overall result (ISSUE_CLOSED/OPEN/ERROR)
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' issue_data <- list(
#'   currentName = "Aus bus",
#'   proposedName = "Aus cus"
#' )
#' name_change_report(issue_data)
#' }
name_change_report <- function(xx) {
    # Print header
    cat("\n")
    cat("========================================\n")
    cat("NAME CHANGE ISSUE REPORT\n")
    cat("========================================\n\n")
    
    # Validate input
    if(is.null(xx$proposedName) || is.null(xx$currentName)) {
        cat("STATUS:               ERROR - Missing currentName or proposedName\n\n")
        cat("========================================\n\n")
        return(invisible("JSON-TAG-ERROR"))
    }
    
    if(xx$proposedName == xx$currentName) {
        cat("STATUS:               ERROR - currentName and proposedName are identical\n\n")
        cat("========================================\n\n")
        return(invisible("JSON-TAG-ERROR"))
    }
    
    # Print names being checked
    cat("Current Name:         ", xx$currentName, "\n")
    cat("Proposed Name:        ", xx$proposedName, "\n\n")
    
    # Get the result from name_change
    result <- name_change(xx)
    
    # Check existence of both names
    cn_result <- name_exists(xx$currentName)
    pn_result <- name_exists(xx$proposedName)
    
    # Print current name status
    cat("CURRENT NAME STATUS:\n")
    cat("----------------------------------------\n")
    if(cn_result$exists) {
        cat("  Exists in COL:      YES\n")
        if(cn_result$multiple) {
            cat("  Multiple Matches:   ", paste(cn_result$ids, collapse = ", "), "\n")
        } else {
            cat("  COL ID:             ", cn_result$id, "\n")
        }
    } else {
        cat("  Exists in COL:      NO (name has been removed)\n")
        
        # Try base name search for current name
        cat("\n  Base Name Search:\n")
        parsed <- cb_name_parser(q = xx$currentName)
        base_name <- parsed$scientificName
        
        if(!is.null(base_name) && base_name != "" && base_name != xx$currentName) {
            cat("    Base Name:        ", base_name, "\n")
            base_result <- name_exists(base_name)
            
            if(base_result$exists) {
                cat("    Found in COL:     YES\n")
                if(base_result$multiple) {
                    cat("    Multiple Matches: ", paste(base_result$ids, collapse = ", "), "\n")
                } else {
                    cat("    COL ID:           ", base_result$id, "\n")
                }
                cat("    Note: Base name without authorship\n")
            } else {
                cat("    Found in COL:     NO\n")
            }
        } else {
            cat("    (Unable to extract base name)\n")
        }
    }
    cat("\n")
    
    # Print proposed name status
    cat("PROPOSED NAME STATUS:\n")
    cat("----------------------------------------\n")
    if(pn_result$exists) {
        cat("  Exists in COL:      YES\n")
        if(pn_result$multiple) {
            cat("  Multiple Matches:   ", paste(pn_result$ids, collapse = ", "), "\n")
        } else {
            cat("  COL ID:             ", pn_result$id, "\n")
        }
        
        # Show synonyms of proposed name
        if(pn_result$multiple) {
            cat("  Checking synonyms for all IDs...\n")
            for(id in pn_result$ids) {
                syns <- get_syns(id)
                if(length(syns) > 0) {
                    cat("    Synonyms of ID ", id, ":\n", sep = "")
                    for(syn in syns) {
                        cat("      - ", syn, "\n", sep = "")
                    }
                } else {
                    cat("    (No synonyms listed for ID ", id, ")\n", sep = "")
                }
            }
        } else {
            syns <- get_syns(pn_result$id)
            if(length(syns) > 0) {
                cat("  Synonyms:\n")
                for(syn in syns) {
                    cat("    - ", syn, "\n", sep = "")
                }
            } else {
                cat("  Synonyms:           (None listed)\n")
            }
        }
    } else {
        cat("  Exists in COL:      NO (proposed name not found)\n")
        
        # Try base name search for proposed name
        cat("\n  Base Name Search:\n")
        parsed <- cb_name_parser(q = xx$proposedName)
        base_name <- parsed$scientificName
        
        if(!is.null(base_name) && base_name != "" && base_name != xx$proposedName) {
            cat("    Base Name:        ", base_name, "\n")
            base_result <- name_exists(base_name)
            
            if(base_result$exists) {
                cat("    Found in COL:     YES\n")
                if(base_result$multiple) {
                    cat("    Multiple Matches: ", paste(base_result$ids, collapse = ", "), "\n")
                } else {
                    cat("    COL ID:           ", base_result$id, "\n")
                }
                cat("    Note: Base name without authorship\n")
            } else {
                cat("    Found in COL:     NO\n")
            }
        } else {
            cat("    (Unable to extract base name)\n")
        }
    }
    cat("\n")
    
    # Print validation
    cat("VALIDATION:\n")
    cat("----------------------------------------\n")
    
    if(!cn_result$exists && pn_result$exists) {
        cat("  Current name removed:    PASS\n")
        cat("  Proposed name exists:    PASS\n")
    } else if(!cn_result$exists && !pn_result$exists) {
        cat("  Current name removed:    PASS\n")
        cat("  Proposed name exists:    FAIL (neither name found)\n")
    } else if(cn_result$exists && !pn_result$exists) {
        cat("  Current name removed:    FAIL (still exists)\n")
        cat("  Proposed name exists:    FAIL (not found)\n")
    } else if(cn_result$exists && pn_result$exists) {
        # Check synonym relationships
        cat("  Both names exist in COL\n")
        cat("  Checking if current is synonym of proposed...\n")
        
        all_are_synonyms <- TRUE
        for(cn_id in cn_result$ids) {
            is_syn_of_any <- any(sapply(pn_result$ids, function(pn_id) {
                syns <- get_syns(pn_id)
                xx$currentName %in% syns
            }))
            
            if(is_syn_of_any) {
                cat("    ID ", cn_id, ": is synonym of proposed - PASS\n", sep = "")
            } else {
                cat("    ID ", cn_id, ": NOT synonym of proposed - FAIL\n", sep = "")
                all_are_synonyms <- FALSE
            }
        }
    }
    
    cat("\n")
    
    # Print final result
    cat("RESULT:               ")
    if(result == "ISSUE_CLOSED") {
        cat("✓ ISSUE CLOSED - Name change implemented\n")
    } else if(result == "ISSUE_OPEN") {
        cat("✗ ISSUE OPEN - Name change not yet implemented\n")
    } else {
        cat("⚠ ERROR - Unable to validate name change\n")
    }
    
    cat("\n========================================\n\n")
    
    invisible(result)
}
