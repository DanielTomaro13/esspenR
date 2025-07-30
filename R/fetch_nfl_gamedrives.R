#' Safe nested data extraction helper function for game drives
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_drives <- function(data, path, default = NA) {
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

#' Create game drives data frames from Core API response
#'
#' Processes raw JSON response from ESPN Core API into structured data frames
#' containing detailed drive information
#'
#' @param data Raw JSON response from ESPN Core API drives endpoint
#' @param event_id Character. Event ID used in request
#' @return List containing multiple data frames with drive information
#' @keywords internal
create_gamedrives_datasets <- function(data, event_id) {

  # Initialize drives data frame
  drives_df <- data.frame(
    event_id = character(0),
    drive_id = character(0),
    drive_number = character(0),
    team_ref = character(0),
    start_period_number = character(0),
    start_clock_value = character(0),
    start_clock_display = character(0),
    start_yard_line = character(0),
    start_yards_to_endzone = character(0),
    start_team_ref = character(0),
    end_period_number = character(0),
    end_clock_value = character(0),
    end_clock_display = character(0),
    end_yard_line = character(0),
    end_yards_to_endzone = character(0),
    end_team_ref = character(0),
    plays_count = character(0),
    yards_gained = character(0),
    time_elapsed_seconds = character(0),
    time_elapsed_display = character(0),
    is_scoring_drive = character(0),
    result = character(0),
    short_display_result = character(0),
    display_result = character(0),
    modified_date = character(0),
    stringsAsFactors = FALSE
  )

  # Initialize summary data frame
  summary_df <- data.frame(
    event_id = character(0),
    total_drives = character(0),
    total_pages = character(0),
    page_index = character(0),
    page_size = character(0),
    request_url = character(0),
    stringsAsFactors = FALSE
  )

  # Extract pagination info
  page_info <- extract_nested_drives(data, c("pageInfo"), list())
  page_index <- extract_nested_drives(page_info, c("pageIndex"), NA_character_)
  page_size <- extract_nested_drives(page_info, c("pageSize"), NA_character_)
  total_pages <- extract_nested_drives(page_info, c("totalPages"), NA_character_)

  # Extract items (drives)
  items <- extract_nested_drives(data, c("items"), list())
  total_drives <- length(items)

  # Add summary row
  summary_row <- data.frame(
    event_id = as.character(event_id),
    total_drives = as.character(total_drives),
    total_pages = as.character(total_pages),
    page_index = as.character(page_index),
    page_size = as.character(page_size),
    request_url = NA_character_,
    stringsAsFactors = FALSE
  )

  summary_df <- rbind(summary_df, summary_row)

  if (length(items) > 0) {
    for (i in seq_along(items)) {
      drive_data <- items[[i]]

      # Extract basic drive information
      drive_id <- extract_nested_drives(drive_data, c("id"), as.character(i))
      drive_number <- extract_nested_drives(drive_data, c("displayValue"), as.character(i))

      # Team information
      team_info <- extract_nested_drives(drive_data, c("team"), list())
      team_ref <- extract_nested_drives(team_info, c("$ref"), NA_character_)

      # Start situation
      start_info <- extract_nested_drives(drive_data, c("start"), list())
      start_period <- extract_nested_drives(start_info, c("period"), list())
      start_period_number <- extract_nested_drives(start_period, c("number"), NA_character_)

      start_clock <- extract_nested_drives(start_info, c("clock"), list())
      start_clock_value <- extract_nested_drives(start_clock, c("value"), NA_character_)
      start_clock_display <- extract_nested_drives(start_clock, c("displayValue"), NA_character_)

      start_yard_line <- extract_nested_drives(start_info, c("yardLine"), NA_character_)
      start_yards_to_endzone <- extract_nested_drives(start_info, c("yardsToEndzone"), NA_character_)

      start_team <- extract_nested_drives(start_info, c("team"), list())
      start_team_ref <- extract_nested_drives(start_team, c("$ref"), NA_character_)

      # End situation
      end_info <- extract_nested_drives(drive_data, c("end"), list())
      end_period <- extract_nested_drives(end_info, c("period"), list())
      end_period_number <- extract_nested_drives(end_period, c("number"), NA_character_)

      end_clock <- extract_nested_drives(end_info, c("clock"), list())
      end_clock_value <- extract_nested_drives(end_clock, c("value"), NA_character_)
      end_clock_display <- extract_nested_drives(end_clock, c("displayValue"), NA_character_)

      end_yard_line <- extract_nested_drives(end_info, c("yardLine"), NA_character_)
      end_yards_to_endzone <- extract_nested_drives(end_info, c("yardsToEndzone"), NA_character_)

      end_team <- extract_nested_drives(end_info, c("team"), list())
      end_team_ref <- extract_nested_drives(end_team, c("$ref"), NA_character_)

      # Drive statistics
      plays_count <- extract_nested_drives(drive_data, c("offensivePlays"), NA_character_)
      yards_gained <- extract_nested_drives(drive_data, c("yards"), NA_character_)

      # Time elapsed
      time_elapsed <- extract_nested_drives(drive_data, c("timeElapsed"), list())
      time_elapsed_seconds <- extract_nested_drives(time_elapsed, c("value"), NA_character_)
      time_elapsed_display <- extract_nested_drives(time_elapsed, c("displayValue"), NA_character_)

      # Drive results
      is_scoring_drive <- extract_nested_drives(drive_data, c("isScore"), "false")
      result <- extract_nested_drives(drive_data, c("result"), NA_character_)
      short_display_result <- extract_nested_drives(drive_data, c("shortDisplayResult"), NA_character_)
      display_result <- extract_nested_drives(drive_data, c("displayResult"), NA_character_)

      # Metadata
      modified_date <- extract_nested_drives(drive_data, c("modified"), NA_character_)

      # Add drive row
      drive_row <- data.frame(
        event_id = as.character(event_id),
        drive_id = as.character(drive_id),
        drive_number = as.character(drive_number),
        team_ref = as.character(team_ref),
        start_period_number = as.character(start_period_number),
        start_clock_value = as.character(start_clock_value),
        start_clock_display = as.character(start_clock_display),
        start_yard_line = as.character(start_yard_line),
        start_yards_to_endzone = as.character(start_yards_to_endzone),
        start_team_ref = as.character(start_team_ref),
        end_period_number = as.character(end_period_number),
        end_clock_value = as.character(end_clock_value),
        end_clock_display = as.character(end_clock_display),
        end_yard_line = as.character(end_yard_line),
        end_yards_to_endzone = as.character(end_yards_to_endzone),
        end_team_ref = as.character(end_team_ref),
        plays_count = as.character(plays_count),
        yards_gained = as.character(yards_gained),
        time_elapsed_seconds = as.character(time_elapsed_seconds),
        time_elapsed_display = as.character(time_elapsed_display),
        is_scoring_drive = as.character(is_scoring_drive),
        result = as.character(result),
        short_display_result = as.character(short_display_result),
        display_result = as.character(display_result),
        modified_date = as.character(modified_date),
        stringsAsFactors = FALSE
      )

      drives_df <- rbind(drives_df, drive_row)
    }
  }

  # Clean up row names
  if (nrow(summary_df) > 0) rownames(summary_df) <- NULL
  if (nrow(drives_df) > 0) rownames(drives_df) <- NULL

  return(list(
    summary = summary_df,
    drives = drives_df
  ))
}

#' Fetch NFL game drives data using Core API
#'
#' Retrieves detailed drive information from ESPN's Core API.
#' The function fetches comprehensive drive data including start/end
#' field position, time elapsed, and drive results for a specific NFL game.
#'
#' @param event_id Character or Numeric. ESPN event ID (required).
#'   The unique identifier for the game in ESPN's database.
#' @param return_type Character. Type of data to return (default: "all").
#'   Options are:
#'   \itemize{
#'     \item "summary" - Basic summary information
#'     \item "drives" - Individual drive data
#'     \item "all" - All data types combined
#'   }
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'nfl_drives_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data. The main purpose is global
#'   environment assignment based on return_type:
#'   \itemize{
#'     \item \code{nfl_drives_summary} - Summary data frame
#'     \item \code{nfl_drives_drives} - Drives data frame
#'     \item \code{nfl_drives_raw} - Raw JSON response (if raw = TRUE)
#'   }
#'
#' @details
#' The function creates structured data frames with comprehensive drive information:
#'
#' **Summary** (\code{nfl_drives_summary}):
#' \itemize{
#'   \item Request metadata: total_drives, total_pages, page_index, page_size
#'   \item Event identification and pagination information
#' }
#'
#' **Drives** (\code{nfl_drives_drives}):
#' \itemize{
#'   \item Drive identification: drive_id, drive_number, team_ref
#'   \item Start situation: period, clock, yard_line, yards_to_endzone
#'   \item End situation: period, clock, yard_line, yards_to_endzone
#'   \item Drive statistics: plays_count, yards_gained, time_elapsed
#'   \item Drive outcome: is_scoring_drive, result, display_result
#' }
#'
#' This provides detailed drive-level analysis capabilities including
#' field position trends, time management, and drive efficiency metrics.
#' Essential for understanding team offensive and defensive performance
#' in different game situations.
#'
#' @examples
#' \dontrun{
#' # Get complete drives data for a specific game
#' fetch_nfl_gamedrives(event_id = "401671617")
#'
#' # Check what data was created
#' head(nfl_drives_summary)
#' head(nfl_drives_drives)
#'
#' # Get only drives data
#' fetch_nfl_gamedrives(event_id = "401671617", return_type = "drives")
#'
#' # Analyze scoring drives
#' scoring_drives <- nfl_drives_drives[
#'   nfl_drives_drives$is_scoring_drive == "true",
#'   c("drive_number", "plays_count", "yards_gained", "time_elapsed_display", "result")
#' ]
#' print(scoring_drives)
#'
#' # Red zone efficiency (drives starting within 20 yards)
#' red_zone_drives <- nfl_drives_drives[
#'   !is.na(nfl_drives_drives$start_yards_to_endzone) &
#'   as.numeric(nfl_drives_drives$start_yards_to_endzone) <= 20,
#'   c("drive_number", "start_yards_to_endzone", "result", "is_scoring_drive")
#' ]
#'
#' # Drive efficiency by result
#' drive_results <- table(nfl_drives_drives$result)
#' print(drive_results)
#'
#' # Average drive statistics
#' drives_numeric <- nfl_drives_drives[!is.na(nfl_drives_drives$plays_count), ]
#' if(nrow(drives_numeric) > 0) {
#'   avg_plays <- mean(as.numeric(drives_numeric$plays_count), na.rm = TRUE)
#'   avg_yards <- mean(as.numeric(drives_numeric$yards_gained), na.rm = TRUE)
#'   cat("Average plays per drive:", round(avg_plays, 1), "\n")
#'   cat("Average yards per drive:", round(avg_yards, 1), "\n")
#' }
#'
#' # Get raw data for debugging
#' fetch_nfl_gamedrives(event_id = "401671617", raw = TRUE)
#' str(nfl_drives_raw, max.level = 2)
#' }
#'
#' @seealso \code{\link{fetch_multiple_nfl_gamedrives}} for fetching
#'   multiple games' data
#'
#' @export
fetch_nfl_gamedrives <- function(event_id, return_type = "all", raw = FALSE) {

  # Input validation
  if (missing(event_id)) {
    stop("'event_id' is a required parameter")
  }

  valid_types <- c("summary", "drives", "all")
  if (!return_type %in% valid_types) {
    stop(sprintf("'return_type' must be one of: %s", paste(valid_types, collapse = ", ")))
  }

  # Convert event_id to character for URL building
  event_id <- as.character(event_id)

  # Build API URL for Core API
  url <- sprintf("https://sports.core.api.espn.com/v2/sports/football/leagues/nfl/events/%s/competitions/%s/drives",
                 event_id, event_id)

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
      assign("nfl_drives_raw", data, envir = .GlobalEnv)
      message("Raw NFL drives data assigned to: nfl_drives_raw")
      message("Data structure preview:")

      # Show available sections
      sections <- names(data)
      message("- Available sections: ", paste(sections, collapse = ", "))

      # Show pagination info
      page_info <- extract_nested_drives(data, c("pageInfo"), list())
      if (length(page_info) > 0) {
        page_sections <- names(page_info)
        message("- Page info sections: ", paste(page_sections, collapse = ", "))

        total_pages <- extract_nested_drives(page_info, c("totalPages"), "unknown")
        page_size <- extract_nested_drives(page_info, c("pageSize"), "unknown")
        message("- Total pages: ", total_pages, ", Page size: ", page_size)
      }

      # Show items info
      items <- extract_nested_drives(data, c("items"), list())
      message("- Total drives retrieved: ", length(items))

      return(invisible(data))
    }

    # Create datasets
    datasets <- create_gamedrives_datasets(data, event_id)

    # Update summary with request URL
    datasets$summary$request_url <- url

    # Assign data based on return_type
    result_data <- list()

    if (return_type %in% c("summary", "all")) {
      assign("nfl_drives_summary", datasets$summary, envir = .GlobalEnv)
      result_data$summary <- datasets$summary
      message(sprintf("Game drives summary assigned to: nfl_drives_summary (%d records)", nrow(datasets$summary)))
    }

    if (return_type %in% c("drives", "all")) {
      assign("nfl_drives_drives", datasets$drives, envir = .GlobalEnv)
      result_data$drives <- datasets$drives
      message(sprintf("Game drives assigned to: nfl_drives_drives (%d drives)", nrow(datasets$drives)))
    }

    # Return appropriate data structure
    if (return_type == "all") {
      return(invisible(result_data))
    } else {
      return(invisible(result_data[[return_type]]))
    }

  }, error = function(e) {
    stop(sprintf("Failed to fetch NFL game drives for event %s: %s",
                 event_id, e$message))
  })
}

#' Fetch multiple NFL game drives data
#'
#' Retrieves drives data for multiple NFL games with rate limiting to
#' be respectful to ESPN's Core API. This function calls \code{\link{fetch_nfl_gamedrives}}
#' for each game and combines the results.
#'
#' @param event_ids Character or Numeric vector. ESPN event IDs.
#'   Vector of unique identifiers for games in ESPN's database.
#' @param return_type Character. Type of data to return (default: "all").
#'   Same options as \code{\link{fetch_nfl_gamedrives}}.
#' @param delay Numeric. Delay in seconds between API requests (default: 0.2).
#'   Used to be respectful to ESPN's servers and avoid rate limiting.
#' @param raw Logical. If TRUE, assigns raw JSON to global environment
#'   for the first game only (default: FALSE).
#'
#' @return Invisibly returns the combined data frames. The main purpose is global
#'   environment assignment of combined datasets from all games.
#'
#' @details
#' The function processes games sequentially with a configurable delay
#' between requests. Failed requests for individual games are logged but
#' do not stop the overall process. The final datasets contain data from
#' all successfully processed games.
#'
#' This is particularly useful for analyzing drive efficiency trends
#' across multiple games, comparing team offensive capabilities,
#' and studying situational football performance.
#'
#' @examples
#' \dontrun{
#' # Get drives for multiple games from same week
#' week1_games <- c("401671617", "401671618", "401671619")
#' fetch_multiple_nfl_gamedrives(week1_games)
#'
#' # Get only drive data for multiple games
#' fetch_multiple_nfl_gamedrives(week1_games, return_type = "drives")
#'
#' # Use longer delay for larger requests
#' fetch_multiple_nfl_gamedrives(week1_games, delay = 0.5)
#'
#' # Analyze combined results
#' unique_games <- unique(nfl_drives_summary$event_id)
#' cat("Retrieved drives for", length(unique_games), "games\n")
#'
#' # Drive efficiency analysis across multiple games
#' scoring_efficiency <- nfl_drives_drives %>%
#'   group_by(team_ref) %>%
#'   summarise(
#'     total_drives = n(),
#'     scoring_drives = sum(is_scoring_drive == "true", na.rm = TRUE),
#'     efficiency = scoring_drives / total_drives * 100,
#'     avg_yards = mean(as.numeric(yards_gained), na.rm = TRUE),
#'     .groups = 'drop'
#'   ) %>%
#'   arrange(desc(efficiency))
#'
#' head(scoring_efficiency)
#' }
#'
#' @seealso \code{\link{fetch_nfl_gamedrives}} for single game data
#' @export
fetch_multiple_nfl_gamedrives <- function(event_ids, return_type = "all",
                                          delay = 0.2, raw = FALSE) {
  # Input validation
  if (length(event_ids) == 0) {
    stop("'event_ids' must contain at least one event ID")
  }

  valid_types <- c("summary", "drives", "all")
  if (!return_type %in% valid_types) {
    stop(sprintf("'return_type' must be one of: %s", paste(valid_types, collapse = ", ")))
  }

  # Initialize combined data containers
  all_summary <- data.frame()
  all_drives <- data.frame()

  message(sprintf("Starting to fetch game drives data for %d games...", length(event_ids)))

  # Process each game sequentially
  for (i in seq_along(event_ids)) {
    event_id <- event_ids[i]
    message(sprintf("Fetching game drives for event %s (%d/%d)...", event_id, i, length(event_ids)))

    tryCatch({
      # Fetch individual game data
      game_data <- fetch_nfl_gamedrives(
        event_id = event_id,
        return_type = return_type,
        raw = raw
      )

      # If raw data requested, return after first game
      if (isTRUE(raw)) {
        return(invisible(game_data))
      }

      # Combine data based on return type
      if (return_type %in% c("summary", "all")) {
        summary_df <- get("nfl_drives_summary", envir = .GlobalEnv)
        all_summary <- rbind(all_summary, summary_df)
      }

      if (return_type %in% c("drives", "all")) {
        drives_df <- get("nfl_drives_drives", envir = .GlobalEnv)
        all_drives <- rbind(all_drives, drives_df)
      }

    }, error = function(e) {
      message(sprintf("Failed to fetch game drives for event %s: %s", event_id, e$message))
    })

    # Be respectful to the API with delay between requests
    if (i < length(event_ids)) {
      Sys.sleep(delay)
    }
  }

  # Remove duplicates and assign combined datasets to global environment
  result_data <- list()

  if (return_type %in% c("summary", "all") && nrow(all_summary) > 0) {
    all_summary <- all_summary[!duplicated(all_summary$event_id), ]
    assign("nfl_drives_summary", all_summary, envir = .GlobalEnv)
    result_data$summary <- all_summary

    message(sprintf("Combined game drives summary assigned to: nfl_drives_summary (%d games)", nrow(all_summary)))
  }

  if (return_type %in% c("drives", "all") && nrow(all_drives) > 0) {
    all_drives <- all_drives[!duplicated(paste(all_drives$event_id, all_drives$drive_id)), ]
    assign("nfl_drives_drives", all_drives, envir = .GlobalEnv)
    result_data$drives <- all_drives

    unique_games <- length(unique(all_drives$event_id))
    total_drives <- nrow(all_drives)
    message(sprintf("Combined game drives assigned to: nfl_drives_drives (%d games, %d drives)",
                    unique_games, total_drives))
  }

  # Return appropriate data structure
  if (return_type == "all") {
    return(invisible(result_data))
  } else {
    return(invisible(result_data[[return_type]]))
  }
}
