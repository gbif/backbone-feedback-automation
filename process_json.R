suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(purrr))

if (!requireNamespace('httr', quietly = TRUE)) { stop('httr is NOT installed') }

# Load gbifbf package (installed by issue_check.sh)
suppressMessages(library(gbifbf))

args <- commandArgs(trailingOnly = TRUE)

original_string <- args[1]
issue = args[2]
report_file = ifelse(length(args) >= 3, args[3], "report.tsv")
enable_report = ifelse(length(args) >= 4 && args[4] == "--report", TRUE, FALSE)
show_reports = ifelse("--verbose" %in% args, TRUE, FALSE)
report_output_mode = "--report-output" %in% args

# Check if original_string is a file path and read it if so
if (file.exists(original_string)) {
  if(!report_output_mode) message("Reading JSON from file: ", original_string)
  original_string <- readLines(original_string, warn = FALSE) %>% 
    paste(collapse = "\n")
} else {
  # If it's a JSON-escaped string from jq, parse it first
  # Remove surrounding quotes if present
  if (grepl('^".*"$', original_string)) {
    original_string <- substr(original_string, 2, nchar(original_string) - 1)
  }
  # Unescape JSON string (convert \n to newlines, \" to quotes, etc.)
  original_string <- gsub('\\\\n', '\n', original_string)
  original_string <- gsub('\\\\t', '\t', original_string)
  original_string <- gsub('\\\\"', '"', original_string)
  original_string <- gsub('\\\\\\\\', '\\\\', original_string)
}

link <- "\\[why is this here\\?\\]\\(https://github.com/gbif/backbone-feedback/wiki/JSON-comments-for-automation-%E2%80%90-Experimental\\)"

# Extract just the JSON portion between "// json for auto-checking" and the wiki link
json_pattern <- "// json for auto-checking\\s*\\n(.*?)\\[why is this here\\?\\]"
json_match <- regmatches(original_string, regexec(json_pattern, original_string, perl = TRUE))[[1]]

if(length(json_match) > 1) {
  json_text <- json_match[2]  # Get the captured group
} else {
  # Fallback to old method if pattern doesn't match
  json_text <- gsub(link, "", original_string) %>%
    gsub("// json for auto-checking", "", .) %>%
    gsub("## 🤖 Proposed JSON Tags", "", .) %>%
    gsub("###.*", "", .)  # Remove everything after ### headers
}

# Clean up the JSON text: remove carriage returns and trim whitespace
json_text <- gsub("\r", "", json_text)  # Remove Windows line endings
json_text <- trimws(json_text)  # Trim leading/trailing whitespace

xx = jsonlite::fromJSON(json_text, simplifyVector = FALSE)

list_depth <- function(this) ifelse(is.list(this), 1L + max(sapply(this, list_depth)), 0L)

# Helper function to create COL taxon link
col_link <- function(id, text = NULL) {
  if(is.null(text)) text <- id
  sprintf("[%s](https://www.checklistbank.org/dataset/3LXR/taxon/%s)", text, id)
}

# Helper function for fuzzy base name search (returns any matches, not just exact)
fuzzy_base_name_search <- function(base_name, limit = 10) {
  tryCatch({
    url <- "https://api.checklistbank.org/dataset/3LXR/nameusage/search"
    user <- Sys.getenv("GBIF_USER")
    pwd <- Sys.getenv("GBIF_PWD")
    
    search_result <- httr::GET(url,
                               httr::authenticate(user, pwd),
                               query = list(q = base_name, limit = limit)) |>
      httr::content(as = "text", encoding = "UTF-8") |>
      jsonlite::fromJSON(flatten = TRUE)
    
    if(!is.null(search_result$result) && nrow(search_result$result) > 0) {
      # Return results with scientificName matching the base name (case-insensitive)
      results <- search_result$result
      
      # Extract scientific names (without authorship)
      if("usage.name.scientificName" %in% names(results)) {
        # Filter to results where the scientific name matches (fuzzy)
        matching_indices <- grep(base_name, results$usage.name.scientificName, ignore.case = TRUE)
        
        if(length(matching_indices) > 0) {
          matches <- results[matching_indices, ]
          ids <- matches$id
          
          # Build name strings with authorship if available
          names <- if("usage.name.authorship" %in% names(matches)) {
            paste(matches$usage.name.scientificName, matches$usage.name.authorship)
          } else {
            matches$usage.name.scientificName
          }
          
          return(list(
            found = TRUE,
            ids = ids,
            names = names,
            count = length(ids)
          ))
        }
      }
    }
    
    return(list(found = FALSE, ids = character(0), names = character(0), count = 0))
  }, error = function(e) {
    return(list(found = FALSE, ids = character(0), names = character(0), count = 0))
  })
}

# Markdown formatter for GitHub comment reports
format_markdown_report <- function(xx, issue_type) {
  output <- c()
  
  if(issue_type == "missingName") {
    name_result <- suppressMessages(name_exists(xx$missingName))
    output <- c(output, sprintf("**Name:** %s\n\n", xx$missingName))
    
    if(name_result$exists) {
      output <- c(output, sprintf("**Status:** ✅ Name found in COL\n"))
      if(name_result$multiple) {
        output <- c(output, sprintf("**COL IDs:** %s (multiple matches)\n", paste(sapply(name_result$ids, function(id) col_link(id)), collapse=", ")))
      } else {
        output <- c(output, sprintf("**COL ID:** %s\n", col_link(name_result$id)))
        # Get taxon details
        taxon <- suppressMessages(gbifbf:::cb_get_taxon_by_id(name_result$id))
        if(!is.null(taxon) && nrow(taxon) > 0) {
          if(!is.null(taxon$status) && !is.na(taxon$status) && length(taxon$status) > 0) {
            output <- c(output, sprintf("**Taxonomic Status:** %s\n", taxon$status))
          }
          if(!is.null(taxon$rank) && !is.na(taxon$rank) && length(taxon$rank) > 0) {
            output <- c(output, sprintf("**Rank:** %s\n", taxon$rank))
          }
          # Get classification
          classif <- suppressMessages(gbifbf:::cb_get_classification_by_id(name_result$id))
          if(!is.null(classif) && length(classif) > 0) {
            output <- c(output, sprintf("**Classification:** %s\n", paste(classif, collapse=" > ")))
          }
        }
      }
    } else {
      output <- c(output, sprintf("**Status:** ❌ Name not found in COL\n"))
    }
    
    # Always try fuzzy base name search for missing name validation (even if already a base name)
    # This helps catch spelling variations
    if(!name_result$exists) {
      parsed <- suppressMessages(gbifbf:::cb_name_parser(xx$missingName))
      base_name <- parsed$scientificName
      if(!is.null(base_name) && base_name != "") {
        base_result <- fuzzy_base_name_search(base_name)
        output <- c(output, sprintf("\n**Base name search:** Searching for '%s'\n", base_name))
        if(base_result$found) {
          output <- c(output, sprintf("- Result: ✅ Found %d match(es) in COL\n", base_result$count))
          for(i in 1:min(base_result$count, 5)) {
            output <- c(output, sprintf("- %s: %s\n", col_link(base_result$ids[i]), base_result$names[i]))
          }
          if(base_result$count > 5) {
            output <- c(output, sprintf("- ... and %d more\n", base_result$count - 5))
          }
        } else {
          output <- c(output, sprintf("- Result: ❌ Not found in COL\n"))
        }
      }
    }
  }
  
  if(issue_type == "nameChange") {
    current_result <- suppressMessages(name_exists(xx$currentName))
    proposed_result <- suppressMessages(name_exists(xx$proposedName))
    
    output <- c(output, sprintf("**Current Name:** %s  \n", xx$currentName))
    output <- c(output, sprintf("**Proposed Name:** %s\n\n", xx$proposedName))
    
    # Current name status
    if(current_result$exists) {
      output <- c(output, sprintf("**Current name:** Found in COL\n"))
      ids_to_check <- if(current_result$multiple) current_result$ids else list(current_result$id)
      output <- c(output, sprintf("- COL ID(s): %s\n", paste(sapply(ids_to_check, function(id) col_link(id)), collapse=", ")))
      
      # Check synonyms - get_syns returns character vector of synonym names
      all_syns <- lapply(ids_to_check, function(id) suppressMessages(gbifbf:::get_syns(id)))
      has_proposed_as_syn <- any(sapply(all_syns, function(s) !is.null(s) && any(grepl(xx$proposedName, s, ignore.case=TRUE))))
      output <- c(output, sprintf("- Status check: %s\n", if(has_proposed_as_syn) "✅ Removed/synonymized" else "❌ Still accepted"))
    } else {
      output <- c(output, sprintf("**Current name:** ❌ Not found in COL\n"))
      # Try base name search (fuzzy)
      parsed <- suppressMessages(gbifbf:::cb_name_parser(xx$currentName))
      base_name <- parsed$scientificName
      if(!is.null(base_name) && base_name != "" && base_name != xx$currentName) {
        base_result <- fuzzy_base_name_search(base_name)
        output <- c(output, sprintf("- Base name search: Searching for '%s'\n", base_name))
        if(base_result$found) {
          output <- c(output, sprintf("  - Result: ✅ Found %d match(es) in COL\n", base_result$count))
          for(i in 1:min(base_result$count, 5)) {
            output <- c(output, sprintf("  - %s: %s\n", col_link(base_result$ids[i]), base_result$names[i]))
          }
          if(base_result$count > 5) {
            output <- c(output, sprintf("  - ... and %d more\n", base_result$count - 5))
          }
        } else {
          output <- c(output, sprintf("  - Result: ❌ Not found in COL\n"))
        }
      }
    }
    
    # Proposed name status
    if(proposed_result$exists) {
      output <- c(output, sprintf("\n**Proposed name:** Found in COL\n"))
      ids_to_check <- if(proposed_result$multiple) proposed_result$ids else list(proposed_result$id)
      output <- c(output, sprintf("- COL ID(s): %s\n", paste(sapply(ids_to_check, function(id) col_link(id)), collapse=", ")))
    } else {
      output <- c(output, sprintf("\n**Proposed name:** ❌ Not found in COL\n"))
      # Try base name search (fuzzy)
      parsed <- suppressMessages(gbifbf:::cb_name_parser(xx$proposedName))
      base_name <- parsed$scientificName
      if(!is.null(base_name) && base_name != "" && base_name != xx$proposedName) {
        base_result <- fuzzy_base_name_search(base_name)
        output <- c(output, sprintf("- Base name search: Searching for '%s'\n", base_name))
        if(base_result$found) {
          output <- c(output, sprintf("  - Result: ✅ Found %d match(es) in COL\n", base_result$count))
          for(i in 1:min(base_result$count, 5)) {
            output <- c(output, sprintf("  - %s: %s\n", col_link(base_result$ids[i]), base_result$names[i]))
          }
          if(base_result$count > 5) {
            output <- c(output, sprintf("  - ... and %d more\n", base_result$count - 5))
          }
        } else {
          output <- c(output, sprintf("  - Result: ❌ Not found in COL\n"))
        }
      }
    }
  }
  
  if(issue_type == "wrongGroup") {
    name_result <- suppressMessages(name_exists(xx$name))
    output <- c(output, sprintf("**Name:** %s  \n", xx$name))
    if(!is.null(xx$wrongGroup) && !is.na(xx$wrongGroup)) {
      output <- c(output, sprintf("**Wrong Group:** %s  \n", xx$wrongGroup))
    }
    if(!is.null(xx$rightGroup) && !is.na(xx$rightGroup)) {
      output <- c(output, sprintf("**Right Group:** %s\n\n", xx$rightGroup))
    } else {
      # If no rightGroup, still need proper spacing
      output <- c(output, "\n")
    }
    
    if(name_result$exists) {
      ids_to_check <- if(name_result$multiple) name_result$ids else list(name_result$id)
      output <- c(output, sprintf("**Found in COL:** %s\n", paste(sapply(ids_to_check, function(id) col_link(id)), collapse=", ")))
      
      for(id in ids_to_check) {
        classif <- suppressMessages(gbifbf:::cb_get_classification_by_id(id))
        if(!is.null(classif) && length(classif) > 0) {
          # Only check wrongGroup if it's specified and not NULL
          has_wrong <- if(!is.null(xx$wrongGroup) && !is.na(xx$wrongGroup)) {
            any(grepl(xx$wrongGroup, classif, ignore.case = TRUE))
          } else { NA }
          
          # Only check rightGroup if it's specified and not NULL
          has_right <- if(!is.null(xx$rightGroup) && !is.na(xx$rightGroup)) {
            any(grepl(xx$rightGroup, classif, ignore.case = TRUE))
          } else { NA }
          
          # Only show ID header if multiple IDs
          if(name_result$multiple) {
            output <- c(output, sprintf("\n**%s:**\n", col_link(id, paste0("ID ", id))))
          } else {
            output <- c(output, "\n")
          }
          output <- c(output, sprintf("- Classification: %s\n", paste(classif, collapse=" > ")))
          
          if(!is.na(has_wrong)) {
            output <- c(output, sprintf("- Contains wrong group (%s): %s\n", xx$wrongGroup, if(has_wrong) "❌ Yes" else "✅ No"))
          }
          if(!is.na(has_right)) {
            output <- c(output, sprintf("- Contains right group (%s): %s\n", xx$rightGroup, if(has_right) "✅ Yes" else "❌ No"))
          }
        }
      }
    } else {
      output <- c(output, sprintf("**Status:** ❌ Name not found in COL\n"))
      # Try base name search (fuzzy)
      parsed <- suppressMessages(gbifbf:::cb_name_parser(xx$name))
      base_name <- parsed$scientificName
      if(!is.null(base_name) && base_name != "" && base_name != xx$name) {
        base_result <- fuzzy_base_name_search(base_name)
        output <- c(output, sprintf("\n**Base name search:** Searching for '%s'\n", base_name))
        if(base_result$found) {
          output <- c(output, sprintf("- Result: ✅ Found %d match(es) in COL\n", base_result$count))
          for(i in 1:min(base_result$count, 5)) {
            output <- c(output, sprintf("- %s: %s\n", col_link(base_result$ids[i]), base_result$names[i]))
          }
          if(base_result$count > 5) {
            output <- c(output, sprintf("- ... and %d more\n", base_result$count - 5))
          }
        } else {
          output <- c(output, sprintf("- Result: ❌ Not found in COL\n"))
        }
      }
    }
  }
  
  if(issue_type == "wrongRank") {
    name_result <- suppressMessages(name_exists(xx$name))
    output <- c(output, sprintf("**Name:** %s  \n", xx$name))
    output <- c(output, sprintf("**Wrong Rank:** %s  \n", xx$wrongRank))
    output <- c(output, sprintf("**Right Rank:** %s\n\n", xx$rightRank))
    
    if(name_result$exists) {
      ids_to_check <- if(name_result$multiple) name_result$ids else list(name_result$id)
      output <- c(output, sprintf("**Found in COL:** %s\n", paste(sapply(ids_to_check, function(id) col_link(id)), collapse=", ")))
      
      for(id in ids_to_check) {
        taxon <- suppressMessages(gbifbf:::cb_get_taxon_by_id(id))
        if(!is.null(taxon) && nrow(taxon) > 0) {
          # Only show ID header if multiple IDs
          if(name_result$multiple) {
            output <- c(output, sprintf("\n**%s:**\n", col_link(id, paste0("ID ", id))))
          } else {
            output <- c(output, "\n")
          }
          
          if(!is.null(taxon$rank) && !is.na(taxon$rank) && length(taxon$rank) > 0) {
            current_rank <- tolower(taxon$rank)
            # Check if rightRank is provided (not null/NA)
            has_expected_rank <- !is.null(xx$rightRank) && !is.na(xx$rightRank) && length(xx$rightRank) > 0
            
            if(has_expected_rank) {
              expected_rank <- tolower(xx$rightRank)
              output <- c(output, sprintf("- Current rank: **%s** %s\n", taxon$rank, 
                                          if(current_rank == expected_rank) "✅" else "❌"))
              output <- c(output, sprintf("- Expected rank: **%s**\n", xx$rightRank))
            } else {
              output <- c(output, sprintf("- Current rank: **%s**\n", taxon$rank))
              output <- c(output, sprintf("- Expected rank: **not specified**\n"))
            }
          } else {
            if(!is.null(xx$rightRank) && !is.na(xx$rightRank) && length(xx$rightRank) > 0) {
              output <- c(output, sprintf("- Current rank: **unknown** ❌\n"))
              output <- c(output, sprintf("- Expected rank: **%s**\n", xx$rightRank))
            } else {
              output <- c(output, sprintf("- Current rank: **unknown**\n"))
              output <- c(output, sprintf("- Expected rank: **not specified**\n"))
            }
          }
        }
      }
    } else {
      output <- c(output, sprintf("**Status:** ❌ Name not found in COL\n"))
      # Try base name search (fuzzy)
      parsed <- suppressMessages(gbifbf:::cb_name_parser(xx$name))
      base_name <- parsed$scientificName
      if(!is.null(base_name) && base_name != "" && base_name != xx$name) {
        base_result <- fuzzy_base_name_search(base_name)
        output <- c(output, sprintf("\n**Base name search:** Searching for '%s'\n", base_name))
        if(base_result$found) {
          output <- c(output, sprintf("- Result: ✅ Found %d match(es) in COL\n", base_result$count))
          for(i in 1:min(base_result$count, 5)) {
            output <- c(output, sprintf("- %s: %s\n", col_link(base_result$ids[i]), base_result$names[i]))
          }
          if(base_result$count > 5) {
            output <- c(output, sprintf("- ... and %d more\n", base_result$count - 5))
          }
        } else {
          output <- c(output, sprintf("- Result: ❌ Not found in COL\n"))
        }
      }
    }
  }
  
  if(issue_type == "wrongStatus") {
    name_result <- suppressMessages(name_exists(xx$name))
    output <- c(output, sprintf("**Name:** %s  \n", xx$name))
    
    # Use rightStatus/wrongStatus fields from JSON
    if(!is.null(xx$wrongStatus) && !is.na(xx$wrongStatus)) {
      output <- c(output, sprintf("**Wrong Status:** %s  \n", xx$wrongStatus))
    }
    if(!is.null(xx$rightStatus) && !is.na(xx$rightStatus)) {
      output <- c(output, sprintf("**Expected Status:** %s  \n", xx$rightStatus))
    }
    if(!is.null(xx$rightParent) && !is.na(xx$rightParent)) {
      output <- c(output, sprintf("**Expected Parent:** %s\n\n", xx$rightParent))
    } else {
      # Add spacing if no parent field
      output <- c(output, "\n")
    }
    
    if(name_result$exists) {
      ids_to_check <- if(name_result$multiple) name_result$ids else list(name_result$id)
      output <- c(output, sprintf("**Found in COL:** %s\n", paste(sapply(ids_to_check, function(id) col_link(id)), collapse=", ")))
      
      for(id in ids_to_check) {
        taxon <- suppressMessages(gbifbf:::cb_get_taxon_by_id(id))
        if(!is.null(taxon) && nrow(taxon) > 0) {
          # Only show ID header if multiple IDs
          if(name_result$multiple) {
            output <- c(output, sprintf("\n**%s:**\n", col_link(id, paste0("ID ", id))))
          } else {
            output <- c(output, "\n")
          }
          
          # Check if status field exists and is not NA
          if(!is.null(taxon$status) && !is.na(taxon$status) && length(taxon$status) > 0) {
            # Compare with rightStatus if provided
            status_match <- if(!is.null(xx$rightStatus) && !is.na(xx$rightStatus)) {
              tolower(taxon$status) == tolower(xx$rightStatus)
            } else {
              FALSE
            }
            output <- c(output, sprintf("- Current status: **%s** %s\n", taxon$status,
                                        if(status_match) "✅" else "❌"))
          } else {
            output <- c(output, sprintf("- Current status: **unknown** ❌\n"))
          }
          
          # Check parent if rightParent is provided
          if(!is.null(xx$rightParent) && !is.na(xx$rightParent)) {
            # For synonyms, check if it's a synonym of the expected parent
            if(!is.null(xx$rightStatus) && tolower(xx$rightStatus) == "synonym") {
              # Search for the expected parent
              expected_parent_result <- suppressMessages(name_exists(xx$rightParent))
              if(expected_parent_result$exists) {
                parent_id <- if(expected_parent_result$multiple) expected_parent_result$ids[[1]] else expected_parent_result$id
                parent_taxon <- suppressMessages(gbifbf:::cb_get_taxon_by_id(parent_id))
                
                # Check if current taxon is listed as a synonym of the expected parent
                if(!is.null(parent_taxon) && nrow(parent_taxon) > 0) {
                  syns <- suppressMessages(gbifbf:::get_syns(parent_id))
                  is_synonym_of_expected <- !is.null(syns) && any(grepl(xx$name, syns, ignore.case=TRUE))
                  
                  output <- c(output, sprintf("- Expected parent: %s %s\n", 
                                            col_link(parent_id, xx$rightParent),
                                            if(is_synonym_of_expected) "✅" else "❌"))
                  
                  # Show current synonyms of the expected parent
                  if(!is.null(syns) && length(syns) > 0) {
                    output <- c(output, sprintf("  - Current synonyms (%d): %s\n", length(syns), paste(syns, collapse=", ")))
                  } else {
                    output <- c(output, "  - Current synonyms: none\n")
                  }
                }
              } else {
                output <- c(output, sprintf("- Expected parent: %s ❌ (not found in COL)\n", xx$rightParent))
              }
            } else {
              # For non-synonym cases, check direct parent relationship
              if(!is.na(taxon$parentId) && !is.null(taxon$parentId)) {
                parent <- suppressMessages(gbifbf:::cb_get_taxon_by_id(taxon$parentId))
                if(!is.null(parent) && nrow(parent) > 0) {
                  parent_matches <- grepl(xx$rightParent, parent$name, ignore.case=TRUE)
                  output <- c(output, sprintf("- Current parent: %s %s\n", 
                                            col_link(taxon$parentId, parent$name),
                                            if(parent_matches) "✅" else "❌"))
                  
                  # Also check if expected parent exists
                  if(!parent_matches) {
                    expected_parent_result <- suppressMessages(name_exists(xx$rightParent))
                    if(expected_parent_result$exists) {
                      output <- c(output, sprintf("- Expected parent exists: %s ✅\n", xx$rightParent))
                    }
                  }
                }
              }
            }
          }
        }
      }
    } else {
      output <- c(output, sprintf("**Status:** ❌ Name not found in COL\n"))
      # Try base name search (fuzzy)
      parsed <- suppressMessages(gbifbf:::cb_name_parser(xx$name))
      base_name <- parsed$scientificName
      if(!is.null(base_name) && base_name != "" && base_name != xx$name) {
        base_result <- fuzzy_base_name_search(base_name)
        output <- c(output, sprintf("\n**Base name search:** Searching for '%s'\n", base_name))
        if(base_result$found) {
          output <- c(output, sprintf("- Result: ✅ Found %d match(es) in COL\n", base_result$count))
          for(i in 1:min(base_result$count, 5)) {
            output <- c(output, sprintf("- %s: %s\n", col_link(base_result$ids[i]), base_result$names[i]))
          }
          if(base_result$count > 5) {
            output <- c(output, sprintf("- ... and %d more\n", base_result$count - 5))
          }
        } else {
          output <- c(output, sprintf("- Result: ❌ Not found in COL\n"))
        }
      }
    }
  }
  
  if(issue_type == "badName") {
    name_result <- suppressMessages(name_exists(xx$badName))
    output <- c(output, sprintf("**Bad Name:** %s\n", xx$badName))
    
    if(name_result$exists) {
      output <- c(output, sprintf("**Status:** ❌ Still exists in COL (should be removed)\n"))
      if(name_result$multiple) {
        output <- c(output, sprintf("**COL IDs:** %s (multiple matches)\n", paste(sapply(name_result$ids, function(id) col_link(id)), collapse=", ")))
      } else {
        output <- c(output, sprintf("**COL ID:** %s\n", col_link(name_result$id)))
        # Get taxon details
        taxon <- suppressMessages(gbifbf:::cb_get_taxon_by_id(name_result$id))
        if(!is.null(taxon) && nrow(taxon) > 0) {
          output <- c(output, sprintf("**Taxonomic Status:** %s\n", taxon$status))
          output <- c(output, sprintf("**Rank:** %s\n", taxon$rank))
          # Get classification
          classif <- suppressMessages(gbifbf:::cb_get_classification_by_id(name_result$id))
          if(!is.null(classif) && length(classif) > 0) {
            output <- c(output, sprintf("**Classification:** %s\n", paste(classif, collapse=" > ")))
          }
        }
      }
    } else {
      output <- c(output, sprintf("**Status:** ✅ Not found in COL (removed)\n"))
      # Try base name search (fuzzy)
      parsed <- suppressMessages(gbifbf:::cb_name_parser(xx$badName))
      base_name <- parsed$scientificName
      if(!is.null(base_name) && base_name != "" && base_name != xx$badName) {
        base_result <- fuzzy_base_name_search(base_name)
        if(base_result$found) {
          output <- c(output, sprintf("\n**Base name search:** Found %d match(es) for '%s'\n", base_result$count, base_name))
          for(i in 1:min(base_result$count, 5)) {
            output <- c(output, sprintf("- %s: %s\n", col_link(base_result$ids[i]), base_result$names[i]))
          }
          if(base_result$count > 5) {
            output <- c(output, sprintf("- ... and %d more\n", base_result$count - 5))
          }
        }
      }
    }
  }
  
  return(paste(output, collapse=""))
}

fun_picker = function(xx) {
names = names(xx)
report_text = ""

if("missingName" %in% names) {
   issue_status = if(report_output_mode) suppressMessages(missing_name(xx)) else missing_name(xx)
   if(show_reports) missing_name_report(xx)
   if(report_output_mode) report_text = format_markdown_report(xx, "missingName")
   issue_type = "missingName"
} 
if("badName" %in% names) {
   issue_status = if(report_output_mode) suppressMessages(bad_name(xx)) else bad_name(xx)
   if(show_reports) bad_name_report(xx)
   if(report_output_mode) report_text = format_markdown_report(xx, "badName")
   issue_type = "badName"
} 
if("currentName" %in% names) {
   issue_status = if(report_output_mode) suppressMessages(name_change(xx)) else name_change(xx)
   if(show_reports) name_change_report(xx)
   if(report_output_mode) report_text = format_markdown_report(xx, "nameChange")
   issue_type = "nameChange"
} 
if("wrongGroup" %in% names) {
   issue_status = if(report_output_mode) suppressMessages(wrong_group(xx)) else wrong_group(xx)
   if(show_reports) wrong_group_report(xx)
   if(report_output_mode) report_text = format_markdown_report(xx, "wrongGroup")
   issue_type = "wrongGroup"
} 
if("wrongRank" %in% names) {
   issue_status = if(report_output_mode) suppressMessages(wrong_rank(xx)) else wrong_rank(xx)
   if(show_reports) wrong_rank_report(xx)
   if(report_output_mode) report_text = format_markdown_report(xx, "wrongRank")
   issue_type = "wrongRank"
}
if("wrongStatus" %in% names) {
   issue_status = if(report_output_mode) suppressMessages(syn_issue(xx)) else syn_issue(xx)
   if(show_reports) syn_issue_report(xx)
   if(report_output_mode) report_text = format_markdown_report(xx, "wrongStatus")
   issue_type = "wrongStatus"
}
if(is.null(issue_status)) { issue_status = "JSON-TAG-ERROR" }
return(list(issue_status=issue_status,issue_type=issue_type,report=report_text))
}

if(list_depth(xx) == 1) {
ff = fun_picker(xx)
} else if(list_depth(xx) > 1) {
# when json array provided 
ff = map(xx,~ fun_picker(.x))
statuses = unique(map_chr(ff,~ .x$issue_status))
types = unique(map_chr(ff,~ .x$issue_type))
issue_type = "ARRAY"

# Combine reports for array items
if(report_output_mode) {
  reports <- map_chr(seq_along(ff), function(i) {
    item_report <- ff[[i]]$report
    item_type <- ff[[i]]$issue_type
    item_status <- ff[[i]]$issue_status
    
    # Format status with emoji
    status_display <- if(item_status == "ISSUE_CLOSED") {
      "✅ CLOSED"
    } else if(item_status == "ISSUE_OPEN") {
      "❌ OPEN"
    } else if(item_status == "JSON-TAG-ERROR") {
      "⚠️ ERROR"
    } else {
      paste0("❓ ", item_status)  # Unknown status
    }
    
    if(!is.null(item_report) && item_report != "") {
      paste0("#### Item ", i, " (", item_type, ") - ", status_display, "\n\n", item_report)
    } else {
      paste0("#### Item ", i, " (", item_type, ") - ", status_display, "\n\nNo report available.\n")
    }
  })
  combined_report <- paste(reports, collapse = "\n---\n\n")
} else {
  combined_report <- ""
}

if(length(statuses) > 1) {
   final_status = "ISSUE_OPEN"
} else {
  final_status = statuses
}

if(length(unique(statuses)) > 1) { 
   final_status = "ISSUE_OPEN" 
} else {
   final_status = unique(statuses)
}

ff = list(issue_status = final_status, issue_type = issue_type, report = combined_report)
} else if (list_depth(xx) == 0) {
ff = list(issue_status = "ISSUE_OPEN", issue_type = "EMPTY", report = "")
}

df = data.frame(issue = issue, issue_status = ff$issue_status, issue_type = ff$issue_type)

# If in report output mode, print the report and exit
if(report_output_mode) {
  if(!is.null(ff$report) && ff$report != "") {
    cat(ff$report)
  } else {
    cat("No detailed report available for this issue type.\n")
  }
  quit(status = 0)
}

# Output parseable format: issue|status|type
cat(paste(issue, ff$issue_status, ff$issue_type, sep = "|"), "\n")

# Only write report if --report flag was provided
if(enable_report) {
  write.table(df, file = report_file, append = TRUE, row.names = FALSE, col.names = !file.exists(report_file), sep = "\t")
}

quit(status = 0)
