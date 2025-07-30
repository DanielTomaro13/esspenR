#' Safe nested data extraction helper function for eventlog
#'
#' Safely extracts values from nested list structures with error handling
#'
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_eventlog <- function(data, path, default = NA) {
  tryCatch({
    result <- data
    for (key in path) {
      if (is.null(result) || !key %in% names(result)) {
        return(default)
      }
      result <- result[[key]]
    }
    if (is.null(result)) default else result
  }, error = function(e) {
    default
  })
}

#' Fetch data from ESPN Core API reference URL
#'
#' Helper function to fetch data from ESPN Core API reference URLs
#'
#' @param ref_url Character. The reference URL to fetch
#' @return List containing the API response data, or NULL if failed
#' @keywords internal
fetch_ref_data <- function(ref_url) {
  if (is.null(ref_url) || is.na(ref_url) || ref_url == "") {
    return(NULL)
  }

  tryCatch({
    resp <- httr::GET(ref_url, httr::timeout(30))

    if (httr::status_code(resp) != 200) {
      return(NULL)
    }

    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content_text, simplifyVector = FALSE, simplifyDataFrame = FALSE)
    return(data)
  }, error = function(e) {
    return(NULL)
  })
}

#' Create athlete eventlog data frame from API response
#'
#' Processes raw JSON response from ESPN Core API into a structured data frame
#' containing event log data for an NFL athlete
#'
#' @param data Raw JSON response from ESPN Core API athlete eventlog endpoint
#' @param athlete_id Character. Athlete ID used in request
#' @param season Character. Season year used in request
#' @param fetch_details Logical. Whether to fetch detailed event/competition data
#' @return data.frame with athlete eventlog information
#' @keywords internal
create_athlete_eventlog_dataset <- function(data, athlete_id, season, fetch_details = FALSE) {
  # Initialize result data frame
  result_df <- data.frame(
    athlete_id = character(0),
    season_year = character(0),
    team_id = character(0),
    event_id = character(0),
    event_ref = character(0),
    competition_ref = character(0),
    statistics_ref = character(0),
    played = character(0),
    event_name = character(0),
    event_date = character(0),
    week_number = character(0),
    season_type = character(0),
    competition_id = character(0),
    venue_name = character(0),
    home_away = character(0),
    opponent_id = character(0),
    opponent_name = character(0),
    score = character(0),
    result = character(0),
    stringsAsFactors = FALSE
  )

  # Extract events data
  events_data <- extract_nested_eventlog(data, c("events"), list())
  items <- extract_nested_eventlog(events_data, c("items"), list())

  if (length(items) == 0) {
    return(result_df)
  }

  # Extract team information
  teams_data <- extract_nested_eventlog(data, c("teams"), list())
  team_id <- NA_character_
  if (length(teams_data) > 0) {
    # Get the first team (should be athlete's team)
    team_keys <- names(teams_data)
    if (length(team_keys) > 0) {
      team_id <- extract_nested_eventlog(teams_data[[team_keys[1]]], c("id"), NA_character_)
    }
  }

  # Process each event item
  for (item in items) {
    event_ref <- extract_nested_eventlog(item, c("event", "$ref"), NA_character_)
    competition_ref <- extract_nested_eventlog(item, c("competition", "$ref"), NA_character_)
    statistics_ref <- extract_nested_eventlog(item, c("statistics", "$ref"), NA_character_)
    played <- extract_nested_eventlog(item, c("played"), NA)
    item_team_id <- extract_nested_eventlog(item, c("teamId"), NA_character_)

    # Extract event ID from reference URL if possible
    event_id <- NA_character_
    if (!is.na(event_ref)) {
      # Extract event ID from URL pattern
      id_match <- regmatches(event_ref, regexpr("/events/([0-9]+)", event_ref))
      if (length(id_match) > 0) {
        event_id <- gsub("/events/", "", id_match)
      }
    }

    # Initialize variables for detailed data
    event_name <- event_date <- week_number <- season_type <- NA_character_
    competition_id <- venue_name <- home_away <- opponent_id <- opponent_name <- NA_character_
    score <- result <- NA_character_

    # Fetch detailed event data if requested
    if (fetch_details && !is.na(event_ref)) {
      event_data <- fetch_ref_data(event_ref)
      if (!is.null(event_data)) {
        event_name <- extract_nested_eventlog(event_data, c("name"), NA_character_)
        event_date <- extract_nested_eventlog(event_data, c("date"), NA_character_)

        # Extract week information
        week_data <- extract_nested_eventlog(event_data, c("week"), list())
        week_number <- extract_nested_eventlog(week_data, c("number"), NA_character_)

        # Extract season type
        season_type_data <- extract_nested_eventlog(event_data, c("seasonType"), list())
        season_type <- extract_nested_eventlog(season_type_data, c("name"), NA_character_)

        # Fetch competition data
        competitions <- extract_nested_eventlog(event_data, c("competitions"), list())
        if (length(competitions) > 0) {
          comp_ref <- extract_nested_eventlog(competitions[[1]], c("$ref"), NA_character_)
          if (!is.na(comp_ref)) {
            comp_data <- fetch_ref_data(comp_ref)
            if (!is.null(comp_data)) {
              competition_id <- extract_nested_eventlog(comp_data, c("id"), NA_character_)

              # Extract venue
              venue_data <- extract_nested_eventlog(comp_data, c("venue"), list())
              venue_name <- extract_nested_eventlog(venue_data, c("fullName"), NA_character_)

              # Extract competitors and determine opponent
              competitors <- extract_nested_eventlog(comp_data, c("competitors"), list())
              if (length(competitors) >= 2) {
                for (comp in competitors) {
                  comp_team_id <- extract_nested_eventlog(comp, c("id"), "")
                  comp_home_away <- extract_nested_eventlog(comp, c("homeAway"), "")

                  if (comp_team_id == team_id) {
                    home_away <- comp_home_away
                  } else {
                    opponent_id <- comp_team_id
                    team_data <- extract_nested_eventlog(comp, c("team"), list())
                    opponent_name <- extract_nested_eventlog(team_data, c("displayName"), NA_character_)
                  }
                }
              }

              # Extract score
              score_data <- extract_nested_eventlog(comp_data, c("status", "displayClock"), NA_character_)
              if (is.na(score_data)) {
                score <- extract_nested_eventlog(comp_data, c("status", "type", "description"), NA_character_)
              }
            }
          }
        }
      }
    }

    # Create row for this event
    event_row <- data.frame(
      athlete_id = as.character(athlete_id),
      season_year = as.character(season),
      team_id = as.character(team_id),
      event_id = as.character(event_id),
      event_ref = as.character(event_ref),
      competition_ref = as.character(competition_ref),
      statistics_ref = as.character(statistics_ref),
      played = as.character(played),
      event_name = as.character(event_name),
      event_date = as.character(event_date),
      week_number = as.character(week_number),
      season_type = as.character(season_type),
      competition_id = as.character(competition_id),
      venue_name = as.character(venue_name),
      home_away = as.character(home_away),
      opponent_id = as.character(opponent_id),
      opponent_name = as.character(opponent_name),
      score = as.character(score),
      result = as.character(result),
      stringsAsFactors = FALSE
    )

    result_df <- rbind(result_df, event_row)
  }

  # Clean up row names
  if (nrow(result_df) > 0) {
    rownames(result_df) <- NULL
  }

  return(result_df)
}

#' Fetch NFL athlete event log data using Core API
#'
#' Retrieves event log data for NFL players from ESPN's Core API.
#' The function fetches event reference data and optionally retrieves
#' detailed information by following the reference URLs.
#'
#' @param athlete_id Character or Numeric. ESPN athlete ID (required).
#'   The unique identifier for the athlete in ESPN's database.
#' @param season Character or Numeric. Season year (required).
#'   The season for which to fetch event log data. Format: "2024" or 2024.
#' @param fetch_details Logical. Whether to fetch detailed event information
#'   by following reference URLs (default: FALSE). Setting to TRUE will make
#'   additional API calls for each event to get comprehensive data.
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'nfl_athlete_eventlog_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data frame. The main purpose is global
#'   environment assignment:
#'   \itemize{
#'     \item \code{nfl_athlete_eventlog} - Event log data frame
#'     \item \code{nfl_athlete_eventlog_raw} - Raw JSON response (if raw = TRUE)
#'   }
#'
#' @details
#' The ESPN Core API returns reference URLs rather than complete data. This function
#' can operate in two modes:
#'
#' **Basic Mode (fetch_details = FALSE):**
#' \itemize{
#'   \item Fast execution with minimal API calls
#'   \item Returns event IDs, reference URLs, team info, and played status
#'   \item Suitable for getting event lists and basic information
#' }
#'
#' **Detailed Mode (fetch_details = TRUE):**
#' \itemize{
#'   \item Makes additional API calls to fetch complete event details
#'   \item Returns comprehensive event, competition, and venue information
#'   \item Much slower but provides complete data
#'   \item Use with caution due to rate limiting
#' }
#'
#' Data frame columns include:
#' \itemize{
#'   \item athlete_id, season_year, team_id, event_id
#'   \item event_ref, competition_ref, statistics_ref (API reference URLs)
#'   \item played (boolean indicating if event was played)
#'   \item Detailed fields (when fetch_details = TRUE): event_name, event_date,
#'         week_number, season_type, venue_name, opponent information
#' }
#'
#' @examples
#' \dontrun{
#' # Get basic event log (fast, minimal API calls)
#' fetch_nfl_athlete_eventlog(athlete_id = "3918298", season = "2024")
#' head(nfl_athlete_eventlog)
#'
#' # Get detailed event log (slow, many API calls)
#' fetch_nfl_athlete_eventlog(athlete_id = "3918298", season = "2024",
#'                            fetch_details = TRUE)
#'
#' # Check reference URLs for manual API calls
#' refs <- nfl_athlete_eventlog[c("event_ref", "competition_ref", "statistics_ref")]
#' head(refs)
#'
#' # Get raw data for debugging
#' fetch_nfl_athlete_eventlog(athlete_id = "3918298", season = "2024", raw = TRUE)
#' str(nfl_athlete_eventlog_raw)
#' }
#'
#' @seealso \code{\link{fetch_multiple_nfl_athlete_eventlogs}} for fetching
#'   multiple athletes' data
#'
#' @export
fetch_nfl_athlete_eventlog <- function(athlete_id, season, fetch_details = FALSE, raw = FALSE) {
  # Input validation
  if (missing(athlete_id)) {
    stop("'athlete_id' is a required parameter")
  }

  if (missing(season)) {
    stop("'season' is a required parameter")
  }

  # Convert inputs to character for URL building
  athlete_id <- as.character(athlete_id)
  season <- as.character(season)

  # Build API URL for Core API
  url <- sprintf("https://sports.core.api.espn.com/v2/sports/football/leagues/nfl/seasons/%s/athletes/%s/eventlog",
                 season, athlete_id)

  # Fetch and parse data
  tryCatch({
    resp <- httr::GET(url, httr::timeout(60))

    if (httr::status_code(resp) != 200) {
      stop(sprintf("API request failed: HTTP %d - %s",
                   httr::status_code(resp),
                   httr::http_status(resp)$message))
    }

    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content_text, simplifyVector = FALSE, simplifyDataFrame = FALSE)

    # Handle raw data assignment for debugging
    if (isTRUE(raw)) {
      assign("nfl_athlete_eventlog_raw", data, envir = .GlobalEnv)
      message("Raw NFL athlete eventlog data assigned to: nfl_athlete_eventlog_raw")
      message("Data structure preview:")

      events_data <- extract_nested_eventlog(data, c("events"), list())
      items <- extract_nested_eventlog(events_data, c("items"), list())

      message("- Events count: ", extract_nested_eventlog(events_data, c("count"), "Unknown"))
      message("- Items count: ", length(items))
      message("- Page count: ", extract_nested_eventlog(events_data, c("pageCount"), "Unknown"))

      # Show sample reference URLs
      if (length(items) > 0) {
        sample_event_ref <- extract_nested_eventlog(items[[1]], c("event", "$ref"), "None")
        sample_stats_ref <- extract_nested_eventlog(items[[1]], c("statistics", "$ref"), "None")
        message("- Sample event ref: ", sample_event_ref)
        message("- Sample stats ref: ", sample_stats_ref)
      }

      return(invisible(data))
    }

    # Warn user about detailed mode
    if (fetch_details) {
      events_data <- extract_nested_eventlog(data, c("events"), list())
      event_count <- extract_nested_eventlog(events_data, c("count"), 0)
      message(sprintf("WARNING: fetch_details=TRUE will make %d additional API calls. This may be slow and could hit rate limits.", event_count * 2))
    }

    # Create dataset
    eventlog_df <- create_athlete_eventlog_dataset(data, athlete_id, season, fetch_details)
    assign("nfl_athlete_eventlog", eventlog_df, envir = .GlobalEnv)

    # Calculate summary statistics for user feedback
    unique_events <- length(unique(eventlog_df$event_id[!is.na(eventlog_df$event_id)]))
    played_games <- sum(eventlog_df$played == "TRUE", na.rm = TRUE)

    mode_desc <- if (fetch_details) "detailed mode" else "basic mode"
    message(sprintf("NFL athlete eventlog data assigned to: nfl_athlete_eventlog (%s: %d events, %d played games, %d total records)",
                    mode_desc, unique_events, played_games, nrow(eventlog_df)))

    return(invisible(eventlog_df))

  }, error = function(e) {
    stop(sprintf("Failed to fetch NFL athlete eventlog for athlete %s, season %s: %s",
                 athlete_id, season, e$message))
  })
}

#' Fetch multiple NFL athletes' event log data
#'
#' Retrieves event log data for multiple NFL athletes with rate limiting to
#' be respectful to ESPN's Core API. This function calls \code{\link{fetch_nfl_athlete_eventlog}}
#' for each athlete and combines the results.
#'
#' @param athlete_ids Character or Numeric vector. ESPN athlete IDs.
#'   Vector of unique identifiers for athletes in ESPN's database.
#' @param season Character or Numeric. Season year (required).
#'   The season for which to fetch event log data for all athletes.
#' @param fetch_details Logical. Whether to fetch detailed event information
#'   (default: FALSE). Use with caution as this significantly increases API calls.
#' @param delay Numeric. Delay in seconds between API requests (default: 0.3).
#'   Used to be respectful to ESPN's servers. Increase for detailed mode.
#' @param raw Logical. If TRUE, assigns raw JSON to global environment
#'   for the first athlete only (default: FALSE).
#'
#' @return Invisibly returns the combined data frame. The main purpose is global
#'   environment assignment:
#'   \itemize{
#'     \item \code{nfl_athlete_eventlog} - Combined event log data from all athletes
#'   }
#'
#' @details
#' The function processes athletes sequentially with a configurable delay
#' between requests. When fetch_details = TRUE, the function will make many
#' additional API calls (2+ per event per athlete), so use with caution.
#'
#' Consider using longer delays and smaller athlete batches when fetch_details = TRUE
#' to avoid hitting ESPN's rate limits.
#'
#' @examples
#' \dontrun{
#' # Get basic event logs for multiple quarterbacks (fast)
#' qb_ids <- c("3918298", "3139477", "4035671")  # Allen, Mahomes, Burrow
#' fetch_multiple_nfl_athlete_eventlogs(qb_ids, season = "2024")
#'
#' # Get detailed event logs (slow, many API calls)
#' fetch_multiple_nfl_athlete_eventlogs(qb_ids, season = "2024",
#'                                      fetch_details = TRUE, delay = 1.0)
#'
#' # Process smaller batches with details
#' fetch_multiple_nfl_athlete_eventlogs(qb_ids[1:2], season = "2024",
#'                                      fetch_details = TRUE)
#' }
#'
#' @seealso \code{\link{fetch_nfl_athlete_eventlog}} for single athlete data
#'
#' @export
fetch_multiple_nfl_athlete_eventlogs <- function(athlete_ids, season, fetch_details = FALSE,
                                                 delay = 0.3, raw = FALSE) {
  # Input validation
  if (length(athlete_ids) == 0) {
    stop("'athlete_ids' must contain at least one athlete ID")
  }

  if (missing(season)) {
    stop("'season' is a required parameter")
  }

  # Warn about detailed mode
  if (fetch_details) {
    message(sprintf("WARNING: fetch_details=TRUE with %d athletes may make 100+ API calls. Consider smaller batches or basic mode.",
                    length(athlete_ids)))
  }

  # Initialize combined data container
  all_eventlog_data <- data.frame()

  message(sprintf("Starting to fetch eventlog data for %d athletes (season %s)...",
                  length(athlete_ids), season))

  # Process each athlete sequentially
  for (i in seq_along(athlete_ids)) {
    athlete_id <- athlete_ids[i]
    message(sprintf("Fetching eventlog data for athlete %s (%d/%d)...", athlete_id, i, length(athlete_ids)))

    tryCatch({
      # Fetch individual athlete data
      athlete_data <- fetch_nfl_athlete_eventlog(
        athlete_id = athlete_id,
        season = season,
        fetch_details = fetch_details,
        raw = raw
      )

      # If raw data requested, return after first athlete
      if (isTRUE(raw)) {
        return(invisible(athlete_data))
      }

      # Combine data
      eventlog_df <- get("nfl_athlete_eventlog", envir = .GlobalEnv)
      all_eventlog_data <- rbind(all_eventlog_data, eventlog_df)

    }, error = function(e) {
      message(sprintf("Failed to fetch eventlog data for athlete %s: %s", athlete_id, e$message))
    })

    # Be respectful to the Core API with delay between requests
    if (i < length(athlete_ids)) {
      Sys.sleep(delay)
    }
  }

  # Remove duplicates and assign combined dataset to global environment
  if (nrow(all_eventlog_data) > 0) {
    # Remove duplicate records based on athlete and event
    all_eventlog_data <- all_eventlog_data[!duplicated(paste(all_eventlog_data$athlete_id,
                                                             all_eventlog_data$event_id)), ]
    assign("nfl_athlete_eventlog", all_eventlog_data, envir = .GlobalEnv)

    # Calculate summary statistics for user feedback
    unique_events <- length(unique(all_eventlog_data$event_id[!is.na(all_eventlog_data$event_id)]))
    unique_athletes <- length(unique(all_eventlog_data$athlete_id))
    played_games <- sum(all_eventlog_data$played == "TRUE", na.rm = TRUE)

    mode_desc <- if (fetch_details) "detailed mode" else "basic mode"
    message(sprintf("Completed! NFL athlete eventlog data assigned to: nfl_athlete_eventlog (%s: %d athletes, %d events, %d played games, %d total records)",
                    mode_desc, unique_athletes, unique_events, played_games, nrow(all_eventlog_data)))
  } else {
    message("No eventlog data was successfully retrieved for any athlete.")
  }

  return(invisible(all_eventlog_data))
}
