#' Check if a Missing Name Has Been Added
#'
#' Internal function to verify whether a name reported as missing has been
#' added to the GBIF Backbone taxonomy. Used for processing GitHub issue tags.
#'
#' @param xx A list containing issue data with a \code{missingName} field
#' 
#' @return Character string: "ISSUE_CLOSED" if the name has been added,
#'   "ISSUE_OPEN" if it's still missing
#'
#' @details
#' This function queries the ChecklistBank API to check if a name that was
#' reported as missing now exists in the backbone. If the name returns results
#' and matches exactly, the issue is considered closed.
#'
#' @keywords internal
#' @export
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck
missing_name = function(xx) {
    # Handle empty or null missingName
    if(is.null(xx$missingName) || length(xx$missingName) == 0) return("ISSUE_OPEN")
    
    result = name_exists(xx$missingName)
    return(ifelse(result$exists, "ISSUE_CLOSED", "ISSUE_OPEN"))
}

#' Generate Readable Report for Missing Name Issue
#'
#' Creates a human-readable report showing the status of a missing name issue,
#' including whether the name has been added to the COL Backbone.
#'
#' @param xx A list containing issue data with a \code{missingName} field
#'
#' @return Invisibly returns the result status ("ISSUE_CLOSED" or "ISSUE_OPEN")
#'
#' @details
#' This function prints a formatted report to the console showing:
#' \itemize{
#'   \item The missing name being checked
#'   \item Whether the name now exists in COL
#'   \item COL ID(s) if the name is found
#'   \item Taxonomic status and classification details
#'   \item Base name search results if exact name not found
#'   \item Overall result (ISSUE_CLOSED if added, ISSUE_OPEN if still missing)
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' issue_data <- list(
#'   missingName = "Aus bus Smith, 1900"
#' )
#' missing_name_report(issue_data)
#' }
missing_name_report <- function(xx) {
    # Print header
    cat("\n")
    cat("========================================\n")
    cat("MISSING NAME ISSUE REPORT\n")
    cat("========================================\n\n")
    
    # Validate input
    if(is.null(xx$missingName) || length(xx$missingName) == 0) {
        cat("STATUS:               ERROR - Missing missingName field\n\n")
        cat("========================================\n\n")
        return(invisible("ISSUE_OPEN"))
    }
    
    # Print name being checked
    cat("Missing Name:         ", xx$missingName, "\n\n")
    
    # Get the result from missing_name
    result <- missing_name(xx)
    
    # Check if name exists
    name_result <- name_exists(xx$missingName)
    
    if(name_result$exists) {
        cat("FOUND IN COL:\n")
        cat("----------------------------------------\n")
        cat("  Exists in COL:      YES\n")
        
        if(name_result$multiple) {
            cat("  Multiple Matches:   ", paste(name_result$ids, collapse = ", "), "\n\n")
            
            # Show details for each ID
            for(id in name_result$ids) {
                cat("  ID ", id, ":\n", sep = "")
                
                # Get taxon details
                n <- cb_get_taxon_by_id(id)
                if(nrow(n) > 0) {
                    if("status" %in% names(n)) {
                        cat("    Status:           ", toupper(n$status[1]), "\n")
                    }
                    if("rank" %in% names(n)) {
                        cat("    Rank:             ", n$rank[1], "\n")
                    }
                }
                
                # Get classification
                parents <- cb_get_classification_by_id(id)
                if(!is.null(parents) && length(parents) > 0) {
                    cat("    Classification:   ", paste(parents, collapse = " > "), "\n")
                }
                cat("\n")
            }
        } else {
            cat("  COL ID:             ", name_result$id, "\n")
            
            # Get taxon details
            n <- cb_get_taxon_by_id(name_result$id)
            if(nrow(n) > 0) {
                if("status" %in% names(n)) {
                    cat("  Status:             ", toupper(n$status[1]), "\n")
                }
                if("rank" %in% names(n)) {
                    cat("  Rank:               ", n$rank[1], "\n")
                }
            }
            
            # Get classification
            parents <- cb_get_classification_by_id(name_result$id)
            if(!is.null(parents) && length(parents) > 0) {
                cat("  Classification:     ", paste(parents, collapse = " > "), "\n")
            }
            cat("\n")
        }
    } else {
        cat("FOUND IN COL:\n")
        cat("----------------------------------------\n")
        cat("  Exists in COL:      NO\n\n")
        
        # Try base name search
        cat("TRYING BASE NAME SEARCH:\n")
        cat("----------------------------------------\n")
        parsed <- cb_name_parser(q = xx$missingName)
        base_name <- parsed$scientificName
        
        if(!is.null(base_name) && base_name != "" && base_name != xx$missingName) {
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
                if(nrow(n) > 0) {
                    if("status" %in% names(n)) {
                        cat("  Status:             ", toupper(n$status[1]), "\n")
                    }
                    if("rank" %in% names(n)) {
                        cat("  Rank:               ", n$rank[1], "\n")
                    }
                }
                
                # Get classification
                parents <- cb_get_classification_by_id(base_result$id)
                if(!is.null(parents) && length(parents) > 0) {
                    cat("  Classification:     ", paste(parents, collapse = " > "), "\n")
                }
                
                cat("\n  Note: This is the base name without authorship.\n")
                cat("        The exact name '", xx$missingName, "' was not found.\n", sep = "")
            } else {
                cat("  Found in COL:       NO\n")
                cat("  Base name also not found.\n")
            }
        } else {
            cat("  Unable to extract base name or base name same as original.\n")
        }
        cat("\n")
    }
    
    # Print final result
    cat("RESULT:               ")
    if(result == "ISSUE_CLOSED") {
        cat("✓ ISSUE CLOSED - Name has been added to COL\n")
    } else {
        cat("✗ ISSUE OPEN - Name still missing from COL\n")
    }
    
    cat("\n========================================\n\n")
    
    invisible(result)
}
