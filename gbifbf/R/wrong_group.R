#' Check if a Taxon is in the Wrong Taxonomic Group
#'
#' Internal function to verify whether a taxon is classified under the wrong
#' higher taxonomic group in the GBIF Backbone. Checks the classification
#' hierarchy to determine if the taxon is under the wrong group or has been
#' moved to the right group.
#'
#' @param xx A list containing issue data with \code{name}, \code{wrongGroup},
#'   and/or \code{rightGroup} fields
#' 
#' @return Character string: "ISSUE_OPEN" if taxon is in the wrong group,
#'   "ISSUE_CLOSED" if it's been moved to the right group, or "JSON-TAG-ERROR"
#'   if the name cannot be resolved or the logic is inconsistent
#'
#' @details
#' This function queries the ChecklistBank API to retrieve the full
#' classification hierarchy for a taxon, then checks whether specified
#' parent groups appear in that hierarchy. It supports:
#' \itemize{
#'   \item Checking if taxon is under \code{wrongGroup}
#'   \item Checking if taxon is under \code{rightGroup}
#'   \item Base name fallback for group names
#'   \item Alternative name lookup if exact match fails
#'   \item Multiple name handling: when multiple taxa match the exact name,
#'         all matches are checked and verified for consistency
#' }
#'
#' When multiple exact name matches exist, the function checks the group
#' classification for ALL matching taxa. If all agree on the status, that
#' status is returned. If they disagree, a conservative approach is taken:
#' the issue is only marked as ISSUE_CLOSED when ALL matches are closed.
#' If ANY match still has ISSUE_OPEN, the overall status is ISSUE_OPEN.
#' This ensures that homonyms across different nomenclatural codes are
#' all verified before closing an issue.
#'
#' HTML tags are stripped from parent names before comparison.
#'
#' @keywords internal
#' @export
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck
wrong_group = function(xx) {
    # Check if name exists using multi-strategy search
    result = name_exists(xx$name)
    if(!result$exists) return("JSON-TAG-ERROR")
    
    # Internal helper to check a single ID
    check_single_id = function(id, name_for_msg = NULL) {
        if(!is.null(name_for_msg)) {
            gbif_message("Checking ID: ", id, " for '", name_for_msg, "'")
        }
        
        # Get classification by traversing parent chain from the ID
        parents = cb_get_classification_by_id(id)
        if(is.null(parents) || length(parents) == 0) return(NULL)
        
        wg = xx$wrongGroup
        rg = xx$rightGroup
        
        if(!is.null(wg)) {
            wg_check = wg %in% parents
            if(!wg_check) {
                # try basename search 
                # gbif_message("trying basename search for wrongGroup")
                wg_base = cb_name_parser(q=wg)$uninomial
                wg_check = wg_base %in% parents
            }
        } else {
            wg_check = NULL
        }
        
        if(!is.null(rg)) {
            rg_check = rg %in% parents
            if(!rg_check) {
                # try basename search
                # gbif_message("trying basename search for rightGroup")
                rg_base = cb_name_parser(q=rg)$uninomial
                rg_check = rg_base %in% parents
            }
        } else {
            rg_check = NULL
        }
        
        if(!is.null(wg_check) && !is.null(rg_check)) {
            # Both wrongGroup and rightGroup specified
            # The key question: is it in the rightGroup?
            if(rg_check) {
                return("ISSUE_CLOSED")  # Successfully moved to correct group
            } else {
                return("ISSUE_OPEN")     # Not yet in correct group
            }
        } 
        
        if(is.null(wg_check) && !is.null(rg_check)) {
            # Only rightGroup specified
            if(rg_check) {
                return("ISSUE_CLOSED")   # In the correct group
            } else {
                return("ISSUE_OPEN")     # Not in the correct group
            }
        }
        
        if(is.null(rg_check) && !is.null(wg_check)) {
            # Only wrongGroup specified
            if(wg_check) {
                return("ISSUE_OPEN")     # Still in wrong group
            } else {
                return("ISSUE_CLOSED")   # No longer in wrong group
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
            gbif_message("WARNING: Multiple matches have different group classifications!")
            gbif_message("Statuses: ", paste(result$ids, "=", statuses, collapse = ", "))
            gbif_message("Conservative approach: Issue only closed when ALL matches are closed.")
            
            # If ANY match has ISSUE_OPEN, the overall issue is still OPEN
            # Only return ISSUE_CLOSED if all matches are ISSUE_CLOSED
            if("ISSUE_OPEN" %in% valid_statuses) {
                gbif_message("At least one match still has ISSUE_OPEN - returning ISSUE_OPEN")
                return("ISSUE_OPEN")
            } else {
                # All are ISSUE_CLOSED (no ISSUE_OPEN found)
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

#' Generate Readable Report for Wrong Group Issue
#'
#' Creates a human-readable report showing the status of a taxonomic group
#' classification issue, including the taxon's current classification hierarchy
#' and whether it's been moved to the correct group.
#'
#' @param xx A list containing issue data with \code{name}, \code{wrongGroup},
#'   and/or \code{rightGroup} fields
#'
#' @return Invisibly returns the result status ("ISSUE_CLOSED", "ISSUE_OPEN",
#'   or "JSON-TAG-ERROR")
#'
#' @details
#' This function prints a formatted report to the console showing:
#' \itemize{
#'   \item The taxon name being checked
#'   \item Expected wrong and right groups
#'   \item Whether the name exists in COL
#'   \item Full classification hierarchy of the taxon
#'   \item Whether the taxon is under wrongGroup or rightGroup
#'   \item Overall result (ISSUE_CLOSED/OPEN/ERROR)
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' issue_data <- list(
#'   name = "Aus bus",
#'   wrongGroup = "Animalia",
#'   rightGroup = "Plantae"
#' )
#' wrong_group_report(issue_data)
#' }
wrong_group_report <- function(xx) {
    # Print header
    cat("\n")
    cat("========================================\n")
    cat("WRONG GROUP ISSUE REPORT\n")
    cat("========================================\n\n")
    
    # Print name being checked
    cat("Name:                 ", xx$name, "\n\n")
    
    # Get the result from wrong_group
    result <- wrong_group(xx)
    
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
                
                # Get classification for the first match
                parents <- cb_get_classification_by_id(base_result$id)
                if(!is.null(parents) && length(parents) > 0) {
                    cat("  Classification:     ", paste(parents, collapse = " > "), "\n")
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
    if(!is.null(xx$wrongGroup)) {
        cat("  Wrong Group:        ", xx$wrongGroup, "\n")
    }
    if(!is.null(xx$rightGroup)) {
        cat("  Right Group:        ", xx$rightGroup, "\n")
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
            
            # Get classification
            parents <- cb_get_classification_by_id(id)
            
            if(!is.null(parents) && length(parents) > 0) {
                cat("    Classification:   ", paste(parents, collapse = " > "), "\n")
                
                # Check wrong group
                if(!is.null(xx$wrongGroup)) {
                    wg_check <- xx$wrongGroup %in% parents
                    if(!wg_check) {
                        wg_base <- cb_name_parser(q=xx$wrongGroup)$uninomial
                        wg_check <- wg_base %in% parents
                    }
                    cat("    In wrong group:   ", ifelse(wg_check, "YES", "NO"), "\n")
                }
                
                # Check right group
                if(!is.null(xx$rightGroup)) {
                    rg_check <- xx$rightGroup %in% parents
                    if(!rg_check) {
                        rg_base <- cb_name_parser(q=xx$rightGroup)$uninomial
                        rg_check <- rg_base %in% parents
                    }
                    cat("    In right group:   ", ifelse(rg_check, "YES", "NO"), "\n")
                }
            } else {
                cat("    Classification:   (Unable to retrieve)\n")
            }
            cat("\n")
        }
    } else {
        cat("  COL ID:             ", name_result$id, "\n")
        
        # Get classification
        parents <- cb_get_classification_by_id(name_result$id)
        
        if(!is.null(parents) && length(parents) > 0) {
            cat("  Classification:     ", paste(parents, collapse = " > "), "\n")
        } else {
            cat("  Classification:     (Unable to retrieve)\n")
        }
        cat("\n")
    }
    
    # Print validation results
    cat("VALIDATION:\n")
    cat("----------------------------------------\n")
    
    if(!name_result$multiple) {
        # Single match
        parents <- cb_get_classification_by_id(name_result$id)
        
        if(!is.null(xx$wrongGroup) && !is.null(parents)) {
            wg_check <- xx$wrongGroup %in% parents
            if(!wg_check) {
                wg_base <- cb_name_parser(q=xx$wrongGroup)$uninomial
                wg_check <- wg_base %in% parents
            }
            cat("  Under wrong group:  ", ifelse(wg_check, "FAIL (still in wrong group)", "PASS (not in wrong group)"), "\n")
        }
        
        if(!is.null(xx$rightGroup) && !is.null(parents)) {
            rg_check <- xx$rightGroup %in% parents
            if(!rg_check) {
                rg_base <- cb_name_parser(q=xx$rightGroup)$uninomial
                rg_check <- rg_base %in% parents
            }
            cat("  Under right group:  ", ifelse(rg_check, "PASS", "FAIL (not in right group)"), "\n")
        }
    } else {
        # Multiple matches
        cat("  Checked all ", length(name_result$ids), " matching IDs\n", sep = "")
        
        # Check each ID's status
        for(id in name_result$ids) {
            parents <- cb_get_classification_by_id(id)
            
            if(!is.null(parents) && length(parents) > 0) {
                if(!is.null(xx$wrongGroup)) {
                    wg_check <- xx$wrongGroup %in% parents
                    if(!wg_check) {
                        wg_base <- cb_name_parser(q=xx$wrongGroup)$uninomial
                        wg_check <- wg_base %in% parents
                    }
                }
                
                if(!is.null(xx$rightGroup)) {
                    rg_check <- xx$rightGroup %in% parents
                    if(!rg_check) {
                        rg_base <- cb_name_parser(q=xx$rightGroup)$uninomial
                        rg_check <- rg_base %in% parents
                    }
                    
                    if(rg_check) {
                        cat("    ID ", id, ": in right group - PASS\n", sep = "")
                    } else {
                        cat("    ID ", id, ": NOT in right group - FAIL\n", sep = "")
                    }
                } else if(!is.null(xx$wrongGroup)) {
                    if(!wg_check) {
                        cat("    ID ", id, ": not in wrong group - PASS\n", sep = "")
                    } else {
                        cat("    ID ", id, ": still in wrong group - FAIL\n", sep = "")
                    }
                }
            }
        }
    }
    
    cat("\n")
    
    # Print final result
    cat("RESULT:               ")
    if(result == "ISSUE_CLOSED") {
        cat("✓ ISSUE CLOSED - Taxon in correct group\n")
    } else if(result == "ISSUE_OPEN") {
        cat("✗ ISSUE OPEN - Taxon still in wrong group\n")
    } else {
        cat("⚠ ERROR - Unable to validate group classification\n")
    }
    
    cat("\n========================================\n\n")
    
    invisible(result)
}
