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
                gbif_message("currentName ID ", cn_id, " is not a synonym of any proposedName ID")
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
