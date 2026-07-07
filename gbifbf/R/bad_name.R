#' Check if a Bad Name Exists in the Backbone
#'
#' Internal function to verify whether a reported bad name still exists
#' in the GBIF Backbone taxonomy. Used for processing GitHub issue tags.
#'
#' @param xx A list containing issue data with a \code{badName} field
#' 
#' @return Character string: "ISSUE_OPEN" if the bad name exists,
#'   "ISSUE_CLOSED" if it has been removed or doesn't match exactly
#'
#' @details
#' This function queries the ChecklistBank API to check if a name flagged
#' as incorrect still exists in the backbone. If the name returns no results
#' or doesn't match exactly, the issue is considered closed.
#'
#' @keywords internal
#' @export
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck
bad_name = function(xx) {
    # Handle empty or null badName
    if(is.null(xx$badName) || length(xx$badName) == 0) return("ISSUE_CLOSED")
    
    result = name_exists(xx$badName)
    return(ifelse(result$exists, "ISSUE_OPEN", "ISSUE_CLOSED"))
}

#' Generate Readable Report for Bad Name Issue
#'
#' Creates a human-readable report showing the status of a bad name issue,
#' including whether the bad name has been removed from the COL Backbone.
#'
#' @param xx A list containing issue data with a \code{badName} field
#'
#' @return Invisibly returns the result status ("ISSUE_CLOSED" if removed, "ISSUE_OPEN" if still exists)
#'
#' @details
#' This function prints a formatted report to the console showing:
#' \itemize{
#'   \item The bad name being checked
#'   \item Whether the name still exists in COL (should be removed)
#'   \item COL ID(s) and details if the name is still found
#'   \item Taxonomic status and classification details
#'   \item Base name search results if exact name not found
#'   \item Overall result (ISSUE_CLOSED if removed, ISSUE_OPEN if still exists)
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' issue_data <- list(
#'   badName = "Aus bus Smith, 1900"
#' )
#' bad_name_report(issue_data)
#' }
bad_name_report <- function(xx) {
    # Print header
    cat("\n")
    cat("========================================\n")
    cat("BAD NAME ISSUE REPORT\n")
    cat("========================================\n\n")
    
    # Validate input
    if(is.null(xx$badName) || length(xx$badName) == 0) {
        cat("STATUS:               ERROR - Missing badName field\n\n")
        cat("========================================\n\n")
        return(invisible("ISSUE_CLOSED"))
    }
    
    # Print name being checked
    cat("Bad Name:             ", xx$badName, "\n\n")
    
    # Get the result from bad_name
    result <- bad_name(xx)
    
    # Check if name exists
    name_result <- name_exists(xx$badName)
    
    if(name_result$exists) {
        cat("STILL IN COL:\n")
        cat("----------------------------------------\n")
        cat("  Exists in COL:      YES (Should be removed)\n")
        
        if(name_result$multiple) {
            cat("  Multiple Matches:   ", paste(name_result$ids, collapse = ", "), "\n\n")
            
            # Show details for each ID
            for(id in name_result$ids) {
                cat("  ID ", id, ":\n", sep = "")
                
                # Get taxon details
                n <- gbifbf:::cb_get_taxon_by_id(id)
                if(nrow(n) > 0) {
                    if("status" %in% names(n)) {
                        cat("    Status:           ", toupper(n$status[1]), "\n")
                    }
                    if("rank" %in% names(n)) {
                        cat("    Rank:             ", n$rank[1], "\n")
                    }
                }
                
                # Get classification
                parents <- gbifbf:::cb_get_classification_by_id(id)
                if(!is.null(parents) && length(parents) > 0) {
                    cat("    Classification:   ", paste(parents, collapse = " > "), "\n")
                }
                cat("\n")
            }
        } else {
            cat("  COL ID:             ", name_result$id, "\n")
            
            # Get taxon details
            n <- gbifbf:::cb_get_taxon_by_id(name_result$id)
            if(nrow(n) > 0) {
                if("status" %in% names(n)) {
                    cat("  Status:             ", toupper(n$status[1]), "\n")
                }
                if("rank" %in% names(n)) {
                    cat("  Rank:               ", n$rank[1], "\n")
                }
            }
            
            # Get classification
            parents <- gbifbf:::cb_get_classification_by_id(name_result$id)
            if(!is.null(parents) && length(parents) > 0) {
                cat("  Classification:     ", paste(parents, collapse = " > "), "\n")
            }
            cat("\n")
        }
    } else {
        cat("REMOVED FROM COL:\n")
        cat("----------------------------------------\n")
        cat("  Exists in COL:      NO (Good - removed)\n\n")
        
        # Try base name search
        cat("BASE NAME SEARCH:\n")
        cat("----------------------------------------\n")
        parsed <- gbifbf:::cb_name_parser(q = xx$badName)
        base_name <- parsed$scientificName
        
        if(!is.null(base_name) && base_name != "" && base_name != xx$badName) {
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
                n <- gbifbf:::cb_get_taxon_by_id(base_result$id)
                if(nrow(n) > 0) {
                    if("status" %in% names(n)) {
                        cat("  Status:             ", toupper(n$status[1]), "\n")
                    }
                    if("rank" %in% names(n)) {
                        cat("  Rank:               ", n$rank[1], "\n")
                    }
                }
                
                # Get classification
                parents <- gbifbf:::cb_get_classification_by_id(base_result$id)
                if(!is.null(parents) && length(parents) > 0) {
                    cat("  Classification:     ", paste(parents, collapse = " > "), "\n")
                }
                
                cat("\n  Note: This is the base name without authorship.\n")
                cat("        The exact name '", xx$badName, "' was not found.\n", sep = "")
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
        cat("✓ ISSUE CLOSED - Bad name has been removed from COL\n")
    } else {
        cat("✗ ISSUE OPEN - Bad name still exists in COL\n")
    }
    
    cat("\n========================================\n\n")
    
    invisible(result)
}
