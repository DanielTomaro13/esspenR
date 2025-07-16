#' Fetch NFL team depth chart data using Core API
#'
#' @param team_id Character or Numeric. ESPN team ID (e.g., "12" for Kansas City Chiefs).
#' @param year Numeric. Season year (e.g., 2023, 2024). Default current year.
#' @param raw Logical. If TRUE, assigns raw JSON to global environment as 'nfl_depthchart_raw'. Default FALSE.
#' @return Invisibly returns the data, but main purpose is global environment assignment.
#' @export
#'
#' @examples
#' # Get current depth chart for Kansas City Chiefs
#' fetch_nfl_team_depthchart(team_id = "12")
#' head(nfl_team_depthchart)
#'
#' # Get 2024 depth chart for Buffalo Bills
#' fetch_nfl_team_depthchart(team_id = 2, year = 2024)
#'
#' # Get raw data for custom processing
#' fetch_nfl_team_depthchart(team_id = "12", year = 2024, raw = TRUE)
#' str(nfl_depthchart_raw, max.level = 2)
#'
fetch_nfl_team_depthchart <- function(team_id, year = NULL, raw = FALSE) {
  # Validate inputs
  if (missing(team_id)) {
    stop("'team_id' is a required parameter")
  }

  # Convert team_id to character for URL building
  team_id <- as.character(team_id)

  # Use current year if not specified
  if (is.null(year)) {
    year <- as.numeric(format(Sys.Date(), "%Y"))
  }

  # Validate year
  if (!is.numeric(year) || year < 1990 || year > as.numeric(format(Sys.Date(), "%Y")) + 2) {
    stop("year must be a valid numeric year between 1990 and two years in the future")
  }

  # Build URL
  url <- sprintf("https://sports.core.api.espn.com/v2/sports/football/leagues/nfl/seasons/%d/teams/%s/depthcharts",
                 year, team_id)

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
      assign("nfl_depthchart_raw", data, envir = .GlobalEnv)
      message("Raw NFL depth chart data assigned to: nfl_depthchart_raw")
      return(invisible(data))
    }

    # Create clean depth chart dataset
    depthchart_df <- create_clean_team_depthchart_dataset(data, team_id, year)

    # Assign to global environment
    assign("nfl_team_depthchart", depthchart_df, envir = .GlobalEnv)

    message(sprintf("NFL team depth chart assigned to: nfl_team_depthchart (Team ID: %s, Year: %d, %d positions)",
                    team_id, year, nrow(depthchart_df)))

    return(invisible(depthchart_df))

  }, error = function(e) {
    stop(sprintf("Failed to fetch NFL depth chart for team %s in %d: %s", team_id, year, e$message))
  })
}

#' Create clean team depth chart dataset from ESPN Core API response
#'
#' @param data Raw JSON response from ESPN Core API depth chart endpoint
#' @param team_id Team ID used in request
#' @param year Year used in request
#' @return Clean data frame with depth chart information
create_clean_team_depthchart_dataset <- function(data, team_id, year) {
  # Initialize result data frame
  result_df <- data.frame(
    request_team_id = character(0),
    request_year = integer(0),
    # Formation/Group info
    formation_id = character(0),
    formation_name = character(0),
    # Position info
    position_id = character(0),
    position_name = character(0),
    position_abbreviation = character(0),
    position_display_name = character(0),
    # Depth info
    slot = integer(0),
    rank = integer(0),
    # Athlete info
    athlete_id = character(0),
    athlete_ref = character(0),
    stringsAsFactors = FALSE
  )

  # Process depth chart formations from data$items (no need to fetch references)
  formations <- NULL
  if ("items" %in% names(data) && length(data[["items"]]) > 0) {
    formations <- data[["items"]]
  }

  if (is.null(formations) || length(formations) == 0) {
    return(result_df)
  }

  # Process each formation (like "Base 4-3 D", "Special Teams", "3WR 1TE")
  for (i in seq_along(formations)) {
    formation <- formations[[i]]

    # Formation info
    formation_id <- if ("id" %in% names(formation)) formation[["id"]] else NA_character_
    formation_name <- if ("name" %in% names(formation)) formation[["name"]] else NA_character_

    # Process positions within this formation
    if ("positions" %in% names(formation) && length(formation[["positions"]]) > 0) {
      positions <- formation[["positions"]]

      # Each position has a name (like "lde", "qb", etc.) and contains position info + athletes
      for (pos_name in names(positions)) {
        position_data <- positions[[pos_name]]

        # Position info
        position_id <- position_name <- position_abbreviation <- position_display_name <- NA_character_
        if ("position" %in% names(position_data)) {
          position_info <- position_data[["position"]]
          position_id <- if ("id" %in% names(position_info)) position_info[["id"]] else NA_character_
          position_name <- if ("name" %in% names(position_info)) position_info[["name"]] else NA_character_
          position_abbreviation <- if ("abbreviation" %in% names(position_info)) position_info[["abbreviation"]] else NA_character_
          position_display_name <- if ("displayName" %in% names(position_info)) position_info[["displayName"]] else NA_character_
        }

        # Process athletes at this position
        if ("athletes" %in% names(position_data) && length(position_data[["athletes"]]) > 0) {
          athletes <- position_data[["athletes"]]

          for (j in seq_along(athletes)) {
            athlete_entry <- athletes[[j]]

            # Depth info
            slot <- if ("slot" %in% names(athlete_entry)) as.integer(athlete_entry[["slot"]]) else NA_integer_
            rank <- if ("rank" %in% names(athlete_entry)) as.integer(athlete_entry[["rank"]]) else j

            # Athlete reference
            athlete_id <- athlete_ref <- NA_character_
            if ("athlete" %in% names(athlete_entry)) {
              athlete_info <- athlete_entry[["athlete"]]
              athlete_ref <- if ("$ref" %in% names(athlete_info)) athlete_info[["$ref"]] else NA_character_
              # Extract athlete ID from the reference URL
              if (!is.na(athlete_ref)) {
                athlete_id <- gsub(".*athletes/([0-9]+).*", "\\1", athlete_ref)
              }
            }

            # Create depth chart row
            depth_row <- data.frame(
              request_team_id = team_id,
              request_year = year,
              formation_id = formation_id,
              formation_name = formation_name,
              position_id = position_id,
              position_name = position_name,
              position_abbreviation = position_abbreviation,
              position_display_name = position_display_name,
              slot = slot,
              rank = rank,
              athlete_id = athlete_id,
              athlete_ref = athlete_ref,
              stringsAsFactors = FALSE
            )

            # Add to result
            result_df <- rbind(result_df, depth_row)
          }
        }
      }
    }
  }

  # Clean up row names and sort by formation, position, and rank
  if (nrow(result_df) > 0) {
    rownames(result_df) <- NULL
    # Sort by formation name, then position abbreviation, then rank
    result_df <- result_df[order(result_df$formation_name, result_df$position_abbreviation, result_df$rank, na.last = TRUE), ]
    rownames(result_df) <- NULL
  }

  return(result_df)
}
