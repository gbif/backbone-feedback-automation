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
                gbif_message("trying basename search for wrongGroup")
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
                gbif_message("trying basename search for rightGroup")
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
