#' Check if a taxonomic name exists in ChecklistBank
#'
#' General purpose function to verify whether a taxonomic name string exists
#' exactly in the GBIF Backbone (ChecklistBank). Uses multiple search strategies
#' but only returns TRUE if the exact input string is found.
#'
#' @param name Character string of the taxonomic name to search for
#' @param verbose Logical; if TRUE, print diagnostic messages about search strategies.
#'   Defaults to FALSE. Messages also respect the global \code{gbifbf.verbose} option.
#'
#' @return A list with four elements:
#'   \describe{
#'     \item{exists}{Logical: TRUE if the exact name string is found, FALSE otherwise}
#'     \item{id}{Character: The ChecklistBank ID (e.g., "TJ8H5") if found, NA_character_ if not found. If multiple matches exist, this is the first one.}
#'     \item{ids}{Character vector: All matching ChecklistBank IDs if found, NA_character_ if not found}
#'     \item{multiple}{Logical: TRUE if multiple exact matches were found, FALSE otherwise}
#'   }
#'
#' @details
#' This function employs multiple search strategies to locate a name and
#' **always runs all strategies** to ensure all exact duplicates are found
#' (e.g., homonyms across different nomenclatural codes):
#' \enumerate{
#'   \item Direct lookup via \code{cb_name_usage()} in primary results
#'   \item Search in alternative name matches
#'   \item Parse name to extract base name (scientific name without author),
#'         search with base name, then verify exact match in results
#'   \item Strip special characters and search, then verify exact match
#'   \item Use the search endpoint which may find name variants not returned by match endpoint
#'   \item Use the ChecklistBank suggest endpoint which excels at finding homonyms across nomenclatural codes
#' }
#'
#' All strategies validate that the EXACT input string appears in the results.
#' Results from all strategies are consolidated and deduplicated. Partial matches
#' or similar names do not count.
#'
#' When multiple exact matches exist (e.g., homonyms under different nomenclatural codes),
#' all matching IDs are returned in the \code{ids} field and \code{multiple} is set to TRUE.
#' This ensures taxonomic homonyms are properly detected and can be handled appropriately.
#'
#' @examples
#' \dontrun{
#' result <- name_exists("Trichopria carinata (Thomson, 1858)")
#' # result$exists = TRUE, result$id = "TJ8P3", result$multiple = FALSE
#' 
#' result <- name_exists("Fake name that does not exist")
#' # result$exists = FALSE, result$id = NA, result$multiple = FALSE
#' 
#' result <- name_exists("Trichopria aequata (Thomson, 1858)", verbose = TRUE)
#' # result$exists = TRUE, result$id = "TJ8H5", result$multiple = FALSE
#' 
#' # Check for multiple matches
#' result <- name_exists("Some ambiguous name")
#' if(result$multiple) {
#'   message("Multiple matches found: ", paste(result$ids, collapse = ", "))
#' }
#' }
#'
#' @export
name_exists <- function(name, verbose = FALSE) {
  # Store original verbose setting and set if requested
  if(verbose) {
    original_verbose <- getOption("gbifbf.verbose", default = TRUE)
    options(gbifbf.verbose = TRUE)
    on.exit(options(gbifbf.verbose = original_verbose))
  }
  
  gbif_message("Searching for name: ", name)
  
  # Collect all matching IDs from different strategies
  all_ids <- character(0)
  
  # Strategy 1: Direct lookup
  gbif_message("Strategy 1: Direct lookup")
  n <- cb_name_usage(name)
  
  # Check primary results for exact match
  if(nrow(n$usage) > 0) {
    match_idx <- which(n$usage$labelHtml == name)
    if(length(match_idx) > 0) {
      strategy1_ids <- unique(n$usage$id[match_idx])
      all_ids <- c(all_ids, strategy1_ids)
      gbif_message("  Found ", length(strategy1_ids), " match(es) in primary results")
    }
  }
  
  # Strategy 2: Check alternatives
  gbif_message("Strategy 2: Check alternatives")
  if(nrow(n$alternatives) > 0) {
    match_idx <- which(n$alternatives$labelHtml == name)
    if(length(match_idx) > 0) {
      strategy2_ids <- unique(n$alternatives$id[match_idx])
      all_ids <- c(all_ids, strategy2_ids)
      gbif_message("  Found ", length(strategy2_ids), " match(es) in alternatives")
    }
  }
  
  # Strategy 3: Parse to base name and search
  gbif_message("Strategy 3: Base name parsing")
  parsed <- cb_name_parser(q = name)
  base_name <- parsed$scientificName
  
  if(!is.null(base_name) && base_name != "" && base_name != name) {
    n_base <- cb_name_usage(base_name)
    
    # Check if exact name is in base name search results
    if(nrow(n_base$usage) > 0) {
      match_idx <- which(n_base$usage$labelHtml == name)
      if(length(match_idx) > 0) {
        strategy3a_ids <- unique(n_base$usage$id[match_idx])
        all_ids <- c(all_ids, strategy3a_ids)
      }
    }
    
    # Check alternatives from base name search
    if(nrow(n_base$alternatives) > 0) {
      match_idx <- which(n_base$alternatives$labelHtml == name)
      if(length(match_idx) > 0) {
        strategy3b_ids <- unique(n_base$alternatives$id[match_idx])
        all_ids <- c(all_ids, strategy3b_ids)
      }
    }
  }
  
  # Strategy 4: Strip special characters (conservative approach)
  # Only strip parentheses and extra spaces as these are common formatting differences
  normalized_name <- gsub("\\s+", " ", name)  # Normalize whitespace
  normalized_name <- trimws(normalized_name)  # Trim edges
  
  if(normalized_name != name) {
    n_norm <- cb_name_usage(normalized_name)
    
    # Check if exact ORIGINAL name is in normalized search results
    if(nrow(n_norm$usage) > 0) {
      match_idx <- which(n_norm$usage$labelHtml == name)
      if(length(match_idx) > 0) {
        strategy4a_ids <- unique(n_norm$usage$id[match_idx])
        all_ids <- c(all_ids, strategy4a_ids)
      }
    }
    
    if(nrow(n_norm$alternatives) > 0) {
      match_idx <- which(n_norm$alternatives$labelHtml == name)
      if(length(match_idx) > 0) {
        strategy4b_ids <- unique(n_norm$alternatives$id[match_idx])
        all_ids <- c(all_ids, strategy4b_ids)
      }
    }
  }
  
  # Strategy 5: Use search endpoint (broader search that may find variants)
  # Use high limit to ensure we capture all exact matches
  tryCatch({
    url <- "https://api.checklistbank.org/dataset/3LXR/nameusage/search"
    user <- Sys.getenv("GBIF_USER")
    pwd <- Sys.getenv("GBIF_PWD")
    
    search_result <- httr::GET(url,
                               httr::authenticate(user, pwd),
                               query = list(q = name, limit = 1000)) |>
      httr::content(as = "text", encoding = "UTF-8") |>
      jsonlite::fromJSON(flatten = TRUE)
    
    if(!is.null(search_result$result) && nrow(search_result$result) > 0) {
      # Strip HTML from usage.labelHtml to compare
      search_result$result$usage.labelHtml_stripped <- strip_html(search_result$result$usage.labelHtml)
      
      # Look for exact match in labelHtml
      match_idx <- which(search_result$result$usage.labelHtml_stripped == name)
      
      # Also check usage.label field if no match found
      if(length(match_idx) == 0 && "usage.label" %in% names(search_result$result)) {
        match_idx <- which(search_result$result$usage.label == name)
      }
      
      # Also check usage.name.scientificName with authorship if no match found
      if(length(match_idx) == 0 && "usage.name.scientificName" %in% names(search_result$result) && 
         "usage.name.authorship" %in% names(search_result$result)) {
        full_names <- paste(search_result$result$usage.name.scientificName, 
                           search_result$result$usage.name.authorship)
        match_idx <- which(full_names == name)
      }
      
      if(length(match_idx) > 0) {
        strategy5_ids <- unique(search_result$result$id[match_idx])
        all_ids <- c(all_ids, strategy5_ids)
      }
    }
  }, error = function(e) {
    # Silently continue if search endpoint fails
  })
  
  # Strategy 6: Use ChecklistBank suggest endpoint (excellent for finding homonyms across nomenclatural codes)
  tryCatch({
    suggest_result <- cb_name_suggest(name, limit = 50)
    
    if(!is.null(suggest_result) && nrow(suggest_result) > 0) {
      # Check if any match exactly (strip HTML from match field)
      suggest_result$match_stripped <- strip_html(suggest_result$match)
      match_idx <- which(suggest_result$match_stripped == name)
      
      if(length(match_idx) > 0) {
        strategy6_ids <- unique(suggest_result$usageId[match_idx])
        all_ids <- c(all_ids, strategy6_ids)
      }
    }
  }, error = function(e) {
    # Silently continue if suggest endpoint fails
  })
  
  # Consolidate results from all strategies
  all_ids <- unique(all_ids)
  
  if(length(all_ids) == 0) {
    # Name not found
    gbif_message("Result: Name not found")
    return(list(exists = FALSE, id = NA_character_, ids = NA_character_, multiple = FALSE))
  } else if(length(all_ids) == 1) {
    # Single match found
    gbif_message("Result: Single match found with ID: ", all_ids[1])
    return(list(exists = TRUE, id = all_ids[1], ids = all_ids, multiple = FALSE))
  } else {
    # Multiple matches found - keep this warning as it's important
    gbif_message("Multiple exact matches found for '", name, "' (likely homonyms). IDs: ", paste(all_ids, collapse = ", "))
    return(list(exists = TRUE, id = all_ids[1], ids = all_ids, multiple = TRUE))
  }
}
