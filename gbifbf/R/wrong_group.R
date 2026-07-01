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
#' }
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
    
    # Get classification by traversing parent chain from the ID
    parents = cb_get_classification_by_id(result$id)
    if(is.null(parents) || length(parents) == 0) return("JSON-TAG-ERROR")
    wg = xx$wrongGroup
    rg = xx$rightGroup


if(!is.null(wg)) {
wg_check = wg %in% parents
if(!wg_check) {
    # try basename search 
    gbif_message("trying basename search for wrongGroup")
    wg = cb_name_parser(q=wg)$uninomial
    wg_check = wg %in% parents
}
} else {
    wg_check = NULL
}

if(!is.null(rg)) {
    rg_check = rg %in% parents
if(!rg_check) {
    # try basename search
    gbif_message("trying basename search for rightGroup")
    rg = cb_name_parser(q=rg)$uninomial
    rg_check = rg %in% parents
}
} else {
    rg_check = NULL
}


if(!is.null(wg_check) & !is.null(rg_check)) {
    # Both wrongGroup and rightGroup specified
    # The key question: is it in the rightGroup?
    if(rg_check) {
        out = "ISSUE_CLOSED"  # Successfully moved to correct group
    } else {
        out = "ISSUE_OPEN"     # Not yet in correct group
    }
} 

if(is.null(wg_check) & !is.null(rg_check)) {
    # Only rightGroup specified
    if(rg_check) {
        out = "ISSUE_CLOSED"   # In the correct group
    } else {
        out = "ISSUE_OPEN"     # Not in the correct group
    }
}

if(is.null(rg_check) & !is.null(wg_check)) {
    # Only wrongGroup specified
    if(wg_check) {
        out = "ISSUE_OPEN"     # Still in wrong group
    } else {
        out = "ISSUE_CLOSED"   # No longer in wrong group
    }
}

return(out)
}
