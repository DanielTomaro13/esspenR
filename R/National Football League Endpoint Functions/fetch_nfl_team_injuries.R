#' Fetch NFL team injuries data using Core API
#'
#' @param team_id Character or Numeric. ESPN team ID (e.g., "12" for Kansas City Chiefs).
#' @param raw Logical. If TRUE, assigns raw JSON to global environment as 'nfl_injuries_raw'. Default FALSE.
#' @return Invisibly returns the data, but main purpose is global environment assignment.
#' @export
#'
#' @examples
#' # Get current injuries for Kansas City Chiefs
#' fetch_nfl_team_injuries(team_id = "12")
#' head(nfl_team_injuries)
#'
#' # Get Buffalo Bills injuries
#' fetch_nfl_team_injuries(team_id = 2)
#'
#' # Get raw data for custom processing
#' fetch_nfl_team_injuries(team_id = "12", raw = TRUE)
#' str(nfl_injuries_raw, max.level = 2)
#'
fetch_nfl_team_injuries <- function(team_id, raw = FALSE) {
  # Validate inputs
  if (missing(team_id)) {
    stop("'team_id' is a required parameter")
  }

  # Convert team_id to character for URL building
  team_id <- as.character(team_id)

  # Build URL
  url <- sprintf("https://sports.core.api.espn.com/v2/sports/football/leagues/nfl/teams/%s/injuries", team_id)

  # Fetch and parse
  tryCatch({
    resp <- httr::GET(url, httr::timeout(60))
    if (httr::status_code(resp) != 200) {
      stop(sprintf("API request failed: HTTP %d - %s",
                   httr::status_code(resp),
                   httr::http_status(resp)$message))
    }

    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content_text, simplifyVector = FALSE, simplifyDataFrame = FALSE)

    # Handle raw data assignment
    if (isTRUE(raw)) {
      assign("nfl_injuries_raw", data, envir = .GlobalEnv)
      message("Raw NFL injuries data assigned to: nfl_injuries_raw")
      return(invisible(data))
    }

    # Create clean injuries dataset
    injuries_df <- create_clean_team_injuries_dataset(data, team_id)

    # Assign to global environment
    assign("nfl_team_injuries", injuries_df, envir = .GlobalEnv)

    message(sprintf("NFL team injuries assigned to: nfl_team_injuries (Team ID: %s, %d injuries)",
                    team_id, nrow(injuries_df)))

    return(invisible(injuries_df))

  }, error = function(e) {
    stop(sprintf("Failed to fetch NFL injuries for team %s: %s", team_id, e$message))
  })
}

#' Create clean team injuries dataset from ESPN Core API response
#'
#' @param data Raw JSON response from ESPN Core API injuries endpoint
#' @param team_id Team ID used in request
#' @return Clean data frame with injury information
create_clean_team_injuries_dataset <- function(data, team_id) {
  # Initialize result data frame
  result_df <- data.frame(
    request_team_id = character(0),
    # Injury info
    injury_id = character(0),
    injury_ref = character(0),
    injury_date = character(0),
    injury_status = character(0),
    injury_type_name = character(0),
    injury_type_description = character(0),
    injury_type_abbreviation = character(0),
    short_comment = character(0),
    long_comment = character(0),
    # Source info
    source_id = character(0),
    source_description = character(0),
    source_state = character(0),
    # Reference URLs
    athlete_ref = character(0),
    team_ref = character(0),
    stringsAsFactors = FALSE
  )

  # Process injuries from data$items
  injuries <- NULL
  if ("items" %in% names(data) && length(data[["items"]]) > 0) {
    injuries <- data[["items"]]
  }

  if (is.null(injuries) || length(injuries) == 0) {
    return(result_df)
  }

  # Process each injury reference
  for (i in seq_along(injuries)) {
    injury_ref <- injuries[[i]]

    # Get the reference URL
    ref_url <- if ("$ref" %in% names(injury_ref)) injury_ref[["$ref"]] else NA_character_

    if (is.na(ref_url)) {
      next  # Skip if no reference URL
    }

    # Fetch the actual injury data from the reference URL
    tryCatch({
      injury_resp <- httr::GET(ref_url, httr::timeout(30))
      if (httr::status_code(injury_resp) != 200) {
        next  # Skip if can't fetch this injury
      }

      injury_content <- httr::content(injury_resp, as = "text", encoding = "UTF-8")
      injury_data <- jsonlite::fromJSON(injury_content, simplifyVector = FALSE, simplifyDataFrame = FALSE)

      # Basic injury info
      injury_id <- if ("id" %in% names(injury_data)) injury_data[["id"]] else NA_character_
      injury_date <- if ("date" %in% names(injury_data)) injury_data[["date"]] else NA_character_
      injury_status <- if ("status" %in% names(injury_data)) injury_data[["status"]] else NA_character_
      short_comment <- if ("shortComment" %in% names(injury_data)) injury_data[["shortComment"]] else NA_character_
      long_comment <- if ("longComment" %in% names(injury_data)) injury_data[["longComment"]] else NA_character_

      # Type info
      injury_type_name <- injury_type_description <- injury_type_abbreviation <- NA_character_
      if ("type" %in% names(injury_data)) {
        type_info <- injury_data[["type"]]
        injury_type_name <- if ("name" %in% names(type_info)) type_info[["name"]] else NA_character_
        injury_type_description <- if ("description" %in% names(type_info)) type_info[["description"]] else NA_character_
        injury_type_abbreviation <- if ("abbreviation" %in% names(type_info)) type_info[["abbreviation"]] else NA_character_
      }

      # Source info
      source_id <- source_description <- source_state <- NA_character_
      if ("source" %in% names(injury_data)) {
        source_info <- injury_data[["source"]]
        source_id <- if ("id" %in% names(source_info)) source_info[["id"]] else NA_character_
        source_description <- if ("description" %in% names(source_info)) source_info[["description"]] else NA_character_
        source_state <- if ("state" %in% names(source_info)) source_info[["state"]] else NA_character_
      }

      # Reference URLs
      athlete_ref <- NA_character_
      team_ref <- NA_character_

      if ("athlete" %in% names(injury_data)) {
        athlete_info <- injury_data[["athlete"]]
        athlete_ref <- if ("$ref" %in% names(athlete_info)) athlete_info[["$ref"]] else NA_character_
      }

      if ("team" %in% names(injury_data)) {
        team_info <- injury_data[["team"]]
        team_ref <- if ("$ref" %in% names(team_info)) team_info[["$ref"]] else NA_character_
      }

      # Create injury row
      injury_row <- data.frame(
        request_team_id = team_id,
        injury_id = injury_id,
        injury_ref = ref_url,
        injury_date = injury_date,
        injury_status = injury_status,
        injury_type_name = injury_type_name,
        injury_type_description = injury_type_description,
        injury_type_abbreviation = injury_type_abbreviation,
        short_comment = short_comment,
        long_comment = long_comment,
        source_id = source_id,
        source_description = source_description,
        source_state = source_state,
        athlete_ref = athlete_ref,
        team_ref = team_ref,
        stringsAsFactors = FALSE
      )

      # Add to result
      result_df <- rbind(result_df, injury_row)

    }, error = function(e) {
      # Skip this injury if there's an error fetching it
      next
    })
  }

  # Clean up row names and sort by injury status and date
  if (nrow(result_df) > 0) {
    rownames(result_df) <- NULL
    # Sort by injury status, then by date (most recent first)
    # Convert date to proper format for sorting
    result_df$date_sort <- as.POSIXct(result_df$injury_date, format = "%Y-%m-%dT%H:%M")
    result_df <- result_df[order(result_df$injury_status, -as.numeric(result_df$date_sort), na.last = TRUE), ]
    result_df$date_sort <- NULL
    rownames(result_df) <- NULL
  }

  return(result_df)
}
