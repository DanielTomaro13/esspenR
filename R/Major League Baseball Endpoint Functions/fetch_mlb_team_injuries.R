#' Fetch mlb team injuries data using Core API
#'
#' Retrieves current injury information for a specific mlb team from ESPN's Core API.
#' The function fetches detailed injury reports including player status, injury type,
#' and recovery timeline information.
#'
#' @param team_id Character or Numeric. ESPN mlb team ID (required).
#'   Examples: "1" for Atlanta Hawks, "2" for Boston Celtics, "5" for Chicago Bulls.
#'   See ESPN's mlb team directory for complete team ID list.
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'mlb_injuries_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed injury data frame. The main purpose is global
#'   environment assignment of:
#'   \itemize{
#'     \item \code{mlb_team_injuries} - Clean injury data frame with player details
#'     \item \code{mlb_injuries_raw} - Raw JSON response (if raw = TRUE)
#'   }
#'
#' @details
#' The function creates a comprehensive injury report containing:
#'
#' **Injury Information**:
#' \itemize{
#'   \item Injury ID, date, and current status
#'   \item Injury type classification and description
#'   \item Short and long injury descriptions
#'   \item Expected return timeline (if available)
#' }
#'
#' **Player Information**:
#' \itemize{
#'   \item Player name, position, and jersey number
#'   \item Reference URLs for detailed player information
#'   \item Team affiliation details
#' }
#'
#' **Source Information**:
#' \itemize{
#'   \item Injury report source and verification status
#'   \item Last updated timestamp
#'   \item Source reliability indicators
#' }
#'
#' The data is automatically sorted by injury status (active injuries first)
#' and then by date (most recent first) for easy analysis.
#'
#' @examples
#' \dontrun{
#' # Get current injuries for Boston Celtics
#' fetch_mlb_team_injuries(team_id = "2")
#' head(mlb_team_injuries)
#'
#' # Get Los Angeles Lakers injuries
#' fetch_mlb_team_injuries(team_id = "13")
#'
#' # View injury types and statuses
#' table(mlb_team_injuries$injury_status)
#' table(mlb_team_injuries$injury_type_name)
#'
#' # Get raw data for custom processing
#' fetch_mlb_team_injuries(team_id = "2", raw = TRUE)
#' str(mlb_injuries_raw, max.level = 2)
#'
#' # Analyze current active injuries
#' active_injuries <- mlb_team_injuries[
#'   mlb_team_injuries$injury_status %in% c("Out", "Questionable", "Doubtful"),
#'   c("player_name", "injury_type_name", "short_comment", "injury_status")
#' ]
#' print(active_injuries)
#' }
#'
#' @importFrom httr GET status_code content timeout http_status
#' @importFrom jsonlite fromJSON
#' @export
fetch_mlb_team_injuries <- function(team_id, raw = FALSE) {
  # Validate inputs
  if (missing(team_id)) {
    stop("'team_id' is a required parameter")
  }

  if (!is.logical(raw)) {
    stop("'raw' parameter must be logical (TRUE or FALSE)")
  }

  # Convert team_id to character for URL building
  team_id <- as.character(team_id)

  # Build URL for mlb Core API
  url <- sprintf("https://sports.core.api.espn.com/v2/sports/baseball/leagues/mlb/teams/%s/injuries", team_id)

  message(sprintf("Fetching mlb injuries for team ID: %s...", team_id))

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
      assign("mlb_injuries_raw", data, envir = .GlobalEnv)
      message("Raw mlb injuries data assigned to: mlb_injuries_raw")

      # Show structure info
      if ("items" %in% names(data)) {
        message(sprintf("Found %d injury references to process", length(data[["items"]])))
      }

      return(invisible(data))
    }

    # Create clean injuries dataset
    injuries_df <- create_clean_mlb_team_injuries_dataset(data, team_id)

    # Assign to global environment
    assign("mlb_team_injuries", injuries_df, envir = .GlobalEnv)
    message(sprintf("mlb team injuries assigned to: mlb_team_injuries (Team ID: %s, %d injuries)",
                    team_id, nrow(injuries_df)))

    # Show summary if there are injuries
    if (nrow(injuries_df) > 0) {
      status_summary <- table(injuries_df$injury_status)
      message("Injury status summary:")
      for (status in names(status_summary)) {
        message(sprintf("  %s: %d", status, status_summary[status]))
      }
    }

    return(invisible(injuries_df))

  }, error = function(e) {
    stop(sprintf("Failed to fetch mlb injuries for team %s: %s", team_id, e$message))
  })
}

#' Create clean mlb team injuries dataset from ESPN Core API response
#'
#' Processes raw JSON injury data and fetches detailed information for each injury
#' @param data Raw JSON response from ESPN Core API injuries endpoint
#' @param team_id Team ID used in request
#' @return Clean data frame with comprehensive injury information
#' @keywords internal
create_clean_mlb_team_injuries_dataset <- function(data, team_id) {
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
    expected_return = character(0),
    # Player info
    player_id = character(0),
    player_name = character(0),
    player_display_name = character(0),
    player_short_name = character(0),
    player_jersey = character(0),
    player_position = character(0),
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
    message("No injuries found for this team")
    return(result_df)
  }

  message(sprintf("Processing %d injury records...", length(injuries)))

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
      expected_return <- if ("expectedReturn" %in% names(injury_data)) injury_data[["expectedReturn"]] else NA_character_

      # Type info
      injury_type_name <- injury_type_description <- injury_type_abbreviation <- NA_character_
      if ("type" %in% names(injury_data)) {
        type_info <- injury_data[["type"]]
        injury_type_name <- if ("name" %in% names(type_info)) type_info[["name"]] else NA_character_
        injury_type_description <- if ("description" %in% names(type_info)) type_info[["description"]] else NA_character_
        injury_type_abbreviation <- if ("abbreviation" %in% names(type_info)) type_info[["abbreviation"]] else NA_character_
      }

      # Player info - fetch from athlete reference
      player_id <- player_name <- player_display_name <- player_short_name <- NA_character_
      player_jersey <- player_position <- NA_character_
      athlete_ref <- NA_character_

      if ("athlete" %in% names(injury_data)) {
        athlete_info <- injury_data[["athlete"]]
        athlete_ref <- if ("$ref" %in% names(athlete_info)) athlete_info[["$ref"]] else NA_character_

        # Fetch player details if reference exists
        if (!is.na(athlete_ref)) {
          tryCatch({
            player_resp <- httr::GET(athlete_ref, httr::timeout(30))
            if (httr::status_code(player_resp) == 200) {
              player_content <- httr::content(player_resp, as = "text", encoding = "UTF-8")
              player_data <- jsonlite::fromJSON(player_content, simplifyVector = FALSE, simplifyDataFrame = FALSE)

              player_id <- if ("id" %in% names(player_data)) player_data[["id"]] else NA_character_
              player_name <- if ("name" %in% names(player_data)) player_data[["name"]] else NA_character_
              player_display_name <- if ("displayName" %in% names(player_data)) player_data[["displayName"]] else NA_character_
              player_short_name <- if ("shortName" %in% names(player_data)) player_data[["shortName"]] else NA_character_
              player_jersey <- if ("jersey" %in% names(player_data)) player_data[["jersey"]] else NA_character_

              # Position info
              if ("position" %in% names(player_data)) {
                position_info <- player_data[["position"]]
                player_position <- if ("abbreviation" %in% names(position_info)) position_info[["abbreviation"]] else NA_character_
              }
            }
          }, error = function(e) {
            # Continue with NA values if player data fetch fails
          })
        }
      }

      # Source info
      source_id <- source_description <- source_state <- NA_character_
      if ("source" %in% names(injury_data)) {
        source_info <- injury_data[["source"]]
        source_id <- if ("id" %in% names(source_info)) source_info[["id"]] else NA_character_
        source_description <- if ("description" %in% names(source_info)) source_info[["description"]] else NA_character_
        source_state <- if ("state" %in% names(source_info)) source_info[["state"]] else NA_character_
      }

      # Team reference
      team_ref <- NA_character_
      if ("team" %in% names(injury_data)) {
        team_info <- injury_data[["team"]]
        team_ref <- if ("$ref" %in% names(team_info)) team_info[["$ref"]] else NA_character_
      }

      # Create injury row
      injury_row <- data.frame(
        request_team_id = team_id,
        injury_id = as.character(injury_id),
        injury_ref = as.character(ref_url),
        injury_date = as.character(injury_date),
        injury_status = as.character(injury_status),
        injury_type_name = as.character(injury_type_name),
        injury_type_description = as.character(injury_type_description),
        injury_type_abbreviation = as.character(injury_type_abbreviation),
        short_comment = as.character(short_comment),
        long_comment = as.character(long_comment),
        expected_return = as.character(expected_return),
        player_id = as.character(player_id),
        player_name = as.character(player_name),
        player_display_name = as.character(player_display_name),
        player_short_name = as.character(player_short_name),
        player_jersey = as.character(player_jersey),
        player_position = as.character(player_position),
        source_id = as.character(source_id),
        source_description = as.character(source_description),
        source_state = as.character(source_state),
        athlete_ref = as.character(athlete_ref),
        team_ref = as.character(team_ref),
        stringsAsFactors = FALSE
      )

      # Add to result
      result_df <- rbind(result_df, injury_row)

    }, error = function(e) {
      # Skip this injury if there's an error fetching it
      message(sprintf("Warning: Could not fetch injury data from reference %d", i))
    })
  }

  # Clean up row names and sort by injury status and date
  if (nrow(result_df) > 0) {
    rownames(result_df) <- NULL

    # Sort by injury status (active injuries first), then by date (most recent first)
    # Define priority order for injury statuses
    status_priority <- c("Out", "Doubtful", "Questionable", "Probable", "Available", "Day-to-Day")
    result_df$status_order <- match(result_df$injury_status, status_priority)
    result_df$status_order[is.na(result_df$status_order)] <- 999  # Put unknown statuses last

    # Convert date to proper format for sorting
    result_df$date_sort <- as.POSIXct(result_df$injury_date, format = "%Y-%m-%dT%H:%M", tz = "UTC")

    # Sort by status priority, then by date (most recent first)
    result_df <- result_df[order(result_df$status_order, -as.numeric(result_df$date_sort), na.last = TRUE), ]

    # Remove helper columns
    result_df$status_order <- NULL
    result_df$date_sort <- NULL
    rownames(result_df) <- NULL
  }

  return(result_df)
}

#' Fetch multiple mlb team injuries data
#'
#' Retrieves injury information for multiple mlb teams with rate limiting to
#' be respectful to ESPN's Core API. This function calls \code{\link{fetch_mlb_team_injuries}}
#' for each team and combines the results.
#'
#' @param team_ids Character or Numeric vector. ESPN mlb team IDs.
#'   Vector of unique identifiers for teams in ESPN's database.
#' @param delay Numeric. Delay in seconds between API requests (default: 0.3).
#'   Used to be respectful to ESPN's servers and avoid rate limiting.
#' @param raw Logical. If TRUE, assigns raw JSON to global environment
#'   for the first team only (default: FALSE).
#'
#' @return Invisibly returns the combined injury data frame. The main purpose is global
#'   environment assignment of combined injury data from all teams.
#'
#' @details
#' The function processes teams sequentially with a configurable delay
#' between requests. Failed requests for individual teams are logged but
#' do not stop the overall process. The final dataset contains injury data from
#' all successfully processed teams.
#'
#' This is particularly useful for league-wide injury analysis,
#' fantasy baseball research, or medical staff benchmarking studies.
#'
#' @examples
#' \dontrun{
#' # Get injuries for multiple teams
#' east_teams <- c("1", "2", "17")  # Hawks, Celtics, Nets
#' fetch_multiple_mlb_team_injuries(east_teams)
#'
#' # Use longer delay for larger requests
#' all_teams <- c("1", "2", "3", "4", "5")
#' fetch_multiple_mlb_team_injuries(all_teams, delay = 0.5)
#'
#' # Analyze combined results
#' unique_teams <- unique(mlb_team_injuries$request_team_id)
#' cat("Retrieved injury data for", length(unique_teams), "teams\n")
#'
#' # League-wide injury analysis
#' injury_summary <- table(mlb_team_injuries$injury_type_name)
#' print(injury_summary)
#'
#' # Teams with most injuries
#' team_injury_counts <- table(mlb_team_injuries$request_team_id)
#' print(sort(team_injury_counts, decreasing = TRUE))
#' }
#'
#' @importFrom httr GET status_code content timeout
#' @export
fetch_multiple_mlb_team_injuries <- function(team_ids, delay = 0.3, raw = FALSE) {
  # Input validation
  if (length(team_ids) == 0) {
    stop("'team_ids' must contain at least one team ID")
  }

  if (!is.numeric(delay) || delay < 0) {
    stop("'delay' must be a non-negative number")
  }

  if (!is.logical(raw)) {
    stop("'raw' parameter must be logical (TRUE or FALSE)")
  }

  # Initialize combined data container
  all_injuries <- data.frame()

  message(sprintf("Starting to fetch mlb injury data for %d teams...", length(team_ids)))

  # Process each team sequentially
  for (i in seq_along(team_ids)) {
    team_id <- team_ids[i]
    message(sprintf("Fetching mlb injuries for team %s (%d/%d)...", team_id, i, length(team_ids)))

    tryCatch({
      # Fetch individual team data
      team_data <- fetch_mlb_team_injuries(
        team_id = team_id,
        raw = raw
      )

      # If raw data requested, return after first team
      if (isTRUE(raw)) {
        return(invisible(team_data))
      }

      # Combine data
      injuries_df <- get("mlb_team_injuries", envir = .GlobalEnv)
      all_injuries <- rbind(all_injuries, injuries_df)

    }, error = function(e) {
      message(sprintf("Failed to fetch mlb injuries for team %s: %s", team_id, e$message))
    })

    # Be respectful to the API with delay between requests
    if (i < length(team_ids)) {
      Sys.sleep(delay)
    }
  }

  # Remove duplicates and assign combined dataset to global environment
  if (nrow(all_injuries) > 0) {
    all_injuries <- all_injuries[!duplicated(all_injuries$injury_id), ]
    assign("mlb_team_injuries", all_injuries, envir = .GlobalEnv)

    unique_teams <- length(unique(all_injuries$request_team_id))
    total_injuries <- nrow(all_injuries)

    message(sprintf("Combined mlb team injuries assigned to: mlb_team_injuries (%d teams, %d injuries)",
                    unique_teams, total_injuries))

    # Show combined summary
    status_summary <- table(all_injuries$injury_status)
    message("Combined injury status summary:")
    for (status in names(status_summary)) {
      message(sprintf("  %s: %d", status, status_summary[status]))
    }
  } else {
    message("No injury data retrieved for any teams")
  }

  return(invisible(all_injuries))
}
