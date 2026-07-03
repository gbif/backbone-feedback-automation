# Internal helper functions for gbifbf package

# Verbose message helper - respects global option
gbif_message <- function(...) {
  if (getOption("gbifbf.verbose", default = TRUE)) {
    message(...)
  }
}

# Null-coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Strip HTML tags from text
#'
#' Helper function to remove HTML tags from labelHtml fields
#' @param html_text Character vector containing HTML
#' @return Character vector with HTML tags removed
#' @export
strip_html <- function(html_text) {
  if(is.null(html_text) || length(html_text) == 0) return(html_text)
  # Remove HTML tags
  gsub("<[^>]+>", "", html_text)
}

# Query ChecklistBank suggest endpoint
cb_name_suggest <- function(q, key = "3LXR", limit = 20) {
  # https://api.checklistbank.org/dataset/3LXR/nameusage/suggest?q=Mallomonas%20Perty,%201852
  url <- paste0("https://api.checklistbank.org/dataset/", key, "/nameusage/suggest")
  
  user <- Sys.getenv("GBIF_USER")
  pwd <- Sys.getenv("GBIF_PWD")
  
  result <- httr::GET(url,
                      httr::authenticate(user, pwd),
                      query = list(q = q, limit = limit)) |>
    httr::content(as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON(flatten = TRUE)
  
  # Return as tibble if we got results
  if(!is.null(result) && length(result) > 0) {
    return(tibble::as_tibble(result))
  }
  
  # Return empty tibble if no results
  return(tibble::tibble())
}

# Get taxon details by ID
cb_get_taxon_by_id <- function(id, key = "3LXR") {
  # https://api.checklistbank.org/dataset/3LXR/nameusage/8HRN9
  url <- paste0("https://api.checklistbank.org/dataset/", key, "/nameusage/", id)
  
  user <- Sys.getenv("GBIF_USER")
  pwd <- Sys.getenv("GBIF_PWD")
  
  result <- httr::GET(url,
                      httr::authenticate(user, pwd)) |>
    httr::content(as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON(flatten = TRUE)
  
  # Extract usage information and format like cb_name_usage does
  if(!is.null(result)) {
    usage <- tibble::tibble(
      id = result$id,
      status = result$status,
      labelHtml = strip_html(result$labelHtml),
      label = result$label,
      parentId = result$parentId %||% NA_character_,
      rank = result$name$rank %||% NA_character_,
      name = result$name$scientificName %||% NA_character_,
      authorship = result$name$authorship %||% NA_character_
    )
    return(usage)
  }
  
  # Return empty tibble if not found
  return(tibble::tibble())
}

# Check if taxon exists only in 3LXRC (not in 3LXR)
exists_only_3LXRC <- function(id) {
  # First check if it exists in 3LXR
  usage_3lxr <- cb_get_taxon_by_id(id, key = "3LXR")
  exists_3lxr <- nrow(usage_3lxr) > 0 && "id" %in% names(usage_3lxr) && !is.na(usage_3lxr$id[1])
  
  # Check 3LXRC
  url <- paste0("https://api.checklistbank.org/dataset/3LXRC/nameusage/", id)
  
  user <- Sys.getenv("GBIF_USER")
  pwd <- Sys.getenv("GBIF_PWD")
  
  result <- httr::GET(url,
                      httr::authenticate(user, pwd)) |>
    httr::content(as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON(flatten = TRUE)
  
  # Extract usage information if found in 3LXRC
  usage_3lxrc <- tibble::tibble()
  exists_3lxrc <- FALSE
  
  if(!is.null(result) && !is.null(result$id)) {
    exists_3lxrc <- TRUE
    usage_3lxrc <- tibble::tibble(
      id = result$id,
      status = result$status,
      labelHtml = strip_html(result$labelHtml),
      label = result$label,
      parentId = result$parentId %||% NA_character_,
      rank = result$name$rank %||% NA_character_,
      name = result$name$scientificName %||% NA_character_,
      authorship = result$name$authorship %||% NA_character_
    )
  }
  
  # Return list with 4 slots
  return(list(
    exists_3LXRC = exists_3lxrc,
    exists_only_3LXRC = exists_3lxrc && !exists_3lxr,
    exists_3LXR = exists_3lxr,
    usage_3LXRC = usage_3lxrc
  ))
}

# Build classification by traversing parent chain
cb_get_classification_by_id <- function(id, key = "3LXR") {
  classification <- character()
  user <- Sys.getenv("GBIF_USER")
  pwd <- Sys.getenv("GBIF_PWD")
  
  # Get starting taxon
  taxon <- cb_get_taxon_by_id(id, key = key)
  if(nrow(taxon) == 0) return(character())
  
  current_id <- taxon$parentId
  
  # Traverse up the parent chain
  while(!is.na(current_id) && current_id != "" && !is.null(current_id)) {
    url <- paste0("https://api.checklistbank.org/dataset/", key, "/nameusage/", current_id)
    parent <- tryCatch({
      httr::GET(url, httr::authenticate(user, pwd)) |>
        httr::content(as = "text", encoding = "UTF-8") |>
        jsonlite::fromJSON(flatten = TRUE)
    }, error = function(e) NULL)
    
    if(!is.null(parent) && !is.null(parent$label)) {
      classification <- c(classification, parent$label)
      # gbif_message("Added parent: ", parent$label)
      current_id <- parent$parentId
    } else {
      break
    }
    
    # Safety limit to prevent infinite loops
    if(length(classification) > 50) break
  }
  
  return(classification)
}

# Parse a taxonomic name
cb_name_parser <- function(q=NULL) {
  # https://api.checklistbank.org/parser/name?q=Tiphiidae%20Leach%2C%201915
  url = "https://api.checklistbank.org/parser/name?"
  
  user <- Sys.getenv("GBIF_USER")
  pwd <- Sys.getenv("GBIF_PWD")

  tt <- httr::GET(url,
                  httr::authenticate(user, pwd),
                  query = list(q = q)) |>
    httr::content(as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON(flatten = TRUE)

  return(tt)
}

# Get synonyms for a taxon
get_syns <- function(col_id = NULL) {

url = paste0("https://api.checklistbank.org/dataset/3LXR/taxon/", col_id, "/info")

s = httr::GET(url,
  httr::authenticate(Sys.getenv("GBIF_USER"), Sys.getenv("GBIF_PWD"))) |> 
  httr::content(as = "text", encoding = "UTF-8") |>
  jsonlite::fromJSON(flatten = TRUE) |>
  purrr::pluck("synonyms")

ss = c()
if(!is.null(s$homotypic)) ss = c(ss, strip_html(s$homotypic$labelHtml))
if(!is.null(s$heterotypic)) ss = c(ss, strip_html(s$heterotypic$labelHtml))

return(ss)
}

# Get dataset source information
get_dataset_source <- function(
  id = NULL,
  key = "3LXR"
) {
  # https://api.checklistbank.org/dataset/308637/nameusage/DRGCD/source
  url <- paste0("https://api.checklistbank.org/dataset/", key, "/nameusage/", id, "/source")

  s = httr::GET(url,
    httr::authenticate(Sys.getenv("GBIF_USER"), Sys.getenv("GBIF_PWD"))) |> 
    httr::content(as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON(flatten = TRUE) 

  s$sourceDatasetKey
  # https://api.checklistbank.org/dataset/2041
  url = paste0("https://api.checklistbank.org/dataset/", s$sourceDatasetKey)

  tt = httr::GET(url,
    httr::authenticate(Sys.getenv("GBIF_USER"), Sys.getenv("GBIF_PWD"))) |> 
    httr::content(as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON(flatten = TRUE)
  tt

}
