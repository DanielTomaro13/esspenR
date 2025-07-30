#' Safe nested data extraction helper function for MLB play-by-play
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_mlb_pbp <- function(data, path, default = NA) {
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

#' Create MLB play-by-play data frames from API response
#'
#' Processes raw JSON response from ESPN MLB API into structured data frames
#' containing detailed play-by-play information
#'
#' @param data Raw JSON response from ESPN MLB API play-by-play endpoint
#' @param event_id Character. Event ID used in request
#' @return List containing multiple data frames with play-by-play information
#' @keywords internal
create_mlb_playbyplay_datasets <- function(data, event_id) {
  # Initialize simple data frames for summaries
  game_summary_df <- data.frame(
    event_id = character(0),
    game_date = character(0),
    game_status = character(0),
    season_year = character(0),
    season_type = character(0),
    home_team_id = character(0),
    home_team_name = character(0),
    home_team_score = character(0),
    away_team_id = character(0),
    away_team_name = character(0),
    away_team_score = character(0),
    total_plays = character(0),
    total_innings = character(0),
    venue_name = character(0),
    attendance = character(0),
    stringsAsFactors = FALSE
  )

  inning_summary_df <- data.frame(
    event_id = character(0),
    inning = character(0),
    inning_half = character(0),
    home_score = character(0),
    away_score = character(0),
    runs_scored = character(0),
    play_count = character(0),
    stringsAsFactors = FALSE
  )

  # Extract game header information for summaries
  header <- extract_nested_mlb_pbp(data, c("header"), list())
  season_info <- extract_nested_mlb_pbp(header, c("season"), list())
  season_year <- extract_nested_mlb_pbp(season_info, c("year"), NA_character_)
  season_type <- extract_nested_mlb_pbp(season_info, c("type"), NA_character_)
  venue_name <- attendance <- NA_character_

  # Extract team information from competition
  competitions <- extract_nested_mlb_pbp(header, c("competitions"), list())
  home_team_id <- away_team_id <- NA_character_
  home_team_name <- away_team_name <- NA_character_
  home_team_score <- away_team_score <- NA_character_
  game_date <- game_status <- NA_character_

  if (length(competitions) > 0) {
    competition <- competitions[[1]]
    game_date <- extract_nested_mlb_pbp(competition, c("date"), NA_character_)

    # Venue information
    venue <- extract_nested_mlb_pbp(competition, c("venue"), list())
    venue_name <- extract_nested_mlb_pbp(venue, c("fullName"), NA_character_)
    attendance <- extract_nested_mlb_pbp(competition, c("attendance"), NA_character_)

    # Status
    status <- extract_nested_mlb_pbp(competition, c("status"), list())
    status_type <- extract_nested_mlb_pbp(status, c("type"), list())
    game_status <- extract_nested_mlb_pbp(status_type, c("name"), NA_character_)

    # Teams
    competitors <- extract_nested_mlb_pbp(competition, c("competitors"), list())
    for (competitor in competitors) {
      team_info <- extract_nested_mlb_pbp(competitor, c("team"), list())
      team_id <- extract_nested_mlb_pbp(team_info, c("id"), NA_character_)
      team_name <- extract_nested_mlb_pbp(team_info, c("displayName"), NA_character_)
      team_score <- extract_nested_mlb_pbp(competitor, c("score"), NA_character_)
      home_away <- extract_nested_mlb_pbp(competitor, c("homeAway"), NA_character_)

      if (home_away == "home") {
        home_team_id <- team_id
        home_team_name <- team_name
        home_team_score <- team_score
      } else if (home_away == "away") {
        away_team_id <- team_id
        away_team_name <- team_name
        away_team_score <- team_score
      }
    }
  }

  # Extract plays data frame directly - it's already perfectly structured!
  plays_df <- extract_nested_mlb_pbp(data, c("plays"), data.frame())

  # Just add the event_id column to the existing plays data frame
  if (nrow(plays_df) > 0) {
    plays_df$event_id <- as.character(event_id)

    # Reorder columns to put event_id first
    col_order <- c("event_id", setdiff(names(plays_df), "event_id"))
    plays_df <- plays_df[, col_order]

    total_plays <- nrow(plays_df)

    # Calculate total innings from the data
    if ("period" %in% names(plays_df) && is.data.frame(plays_df$period)) {
      max_inning <- max(plays_df$period$number, na.rm = TRUE)
      total_innings <- if (is.finite(max_inning)) max_inning else 9
    } else {
      total_innings <- 9
    }
  } else {
    plays_df <- data.frame(event_id = character(0))
    total_plays <- 0
    total_innings <- 9
  }

  # Create simple inning summaries if plays data exists
  if (nrow(plays_df) > 0 && "period" %in% names(plays_df) && is.data.frame(plays_df$period)) {
    # Get unique inning/half combinations
    unique_periods <- unique(plays_df$period[, c("number", "type")])
    unique_periods <- unique_periods[!is.na(unique_periods$number), ]

    for (i in seq_len(nrow(unique_periods))) {
      period_combo <- unique_periods[i, ]

      # Find plays in this inning half
      period_plays <- plays_df[
        plays_df$period$number == period_combo$number &
          plays_df$period$type == period_combo$type,
      ]

      if (nrow(period_plays) > 0) {
        last_play <- period_plays[nrow(period_plays), ]

        # Calculate runs scored in this half-inning
        first_play <- period_plays[1, ]
        runs_scored <- 0

        if (!is.na(first_play$homeScore) && !is.na(last_play$homeScore) &&
            !is.na(first_play$awayScore) && !is.na(last_play$awayScore)) {
          if (period_combo$type == "Top") {
            runs_scored <- last_play$awayScore - first_play$awayScore
          } else {
            runs_scored <- last_play$homeScore - first_play$homeScore
          }
        }

        inning_row <- data.frame(
          event_id = as.character(event_id),
          inning = as.character(period_combo$number),
          inning_half = as.character(period_combo$type),
          home_score = as.character(last_play$homeScore),
          away_score = as.character(last_play$awayScore),
          runs_scored = as.character(runs_scored),
          play_count = as.character(nrow(period_plays)),
          stringsAsFactors = FALSE
        )

        inning_summary_df <- rbind(inning_summary_df, inning_row)
      }
    }
  }

  # Add game summary row
  summary_row <- data.frame(
    event_id = as.character(event_id),
    game_date = as.character(game_date),
    game_status = as.character(game_status),
    season_year = as.character(season_year),
    season_type = as.character(season_type),
    home_team_id = as.character(home_team_id),
    home_team_name = as.character(home_team_name),
    home_team_score = as.character(home_team_score),
    away_team_id = as.character(away_team_id),
    away_team_name = as.character(away_team_name),
    away_team_score = as.character(away_team_score),
    total_plays = as.character(total_plays),
    total_innings = as.character(total_innings),
    venue_name = as.character(venue_name),
    attendance = as.character(attendance),
    stringsAsFactors = FALSE
  )
  game_summary_df <- rbind(game_summary_df, summary_row)

  # Clean up row names
  if (nrow(game_summary_df) > 0) rownames(game_summary_df) <- NULL
  if (nrow(inning_summary_df) > 0) rownames(inning_summary_df) <- NULL
  if (nrow(plays_df) > 0) rownames(plays_df) <- NULL

  return(list(
    game_summary = game_summary_df,
    inning_summary = inning_summary_df,
    plays = plays_df
  ))
}

#' Fetch MLB game play-by-play data using ESPN API
#'
#' Retrieves detailed play-by-play information from ESPN's MLB API.
#' The function fetches comprehensive play data for a specific MLB game.
#'
#' @param event_id Character or Numeric. ESPN event ID (required).
#'   The unique identifier for the game in ESPN's database.
#' @param return_type Character. Type of data to return (default: "all").
#'   Options are:
#'   \itemize{
#'     \item "game_summary" - Basic game information and totals
#'     \item "inning_summary" - Inning-by-inning scoring summary
#'     \item "plays" - Individual play-by-play data
#'     \item "all" - All data types combined
#'   }
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'mlb_pbp_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data. The main purpose is global
#'   environment assignment based on return_type:
#'   \itemize{
#'     \item \code{mlb_pbp_game_summary} - Game summary data frame
#'     \item \code{mlb_pbp_inning_summary} - Inning summary data frame
#'     \item \code{mlb_pbp_plays} - Complete plays data frame with all ESPN fields
#'     \item \code{mlb_pbp_raw} - Raw JSON response (if raw = TRUE)
#'   }
#'
#' @details
#' The function creates structured data frames with comprehensive play-by-play information:
#'
#' **Game Summary** (\code{mlb_pbp_game_summary}):
#' \itemize{
#'   \item Game details: event_id, game_date, game_status, season info
#'   \item Team matchup: home_team vs away_team with final scores
#'   \item Venue, attendance, and total plays/innings information
#' }
#'
#' **Inning Summary** (\code{mlb_pbp_inning_summary}):
#' \itemize{
#'   \item Inning-by-inning scoring breakdown (top/bottom of each inning)
#'   \item Runs scored per half-inning
#'   \item Play counts per half-inning
#'   \item Running score after each half-inning
#' }
#'
#' **Plays** (\code{mlb_pbp_plays}):
#' \itemize{
#'   \item Complete ESPN plays data frame with all 30+ columns preserved
#'   \item Nested data frames: period, type, participants, pitchCount, etc.
#'   \item Pitch tracking: velocity, type, coordinates, trajectory
#'   \item Hit data: coordinates, distance, launch angle
#'   \item Baserunner tracking: onFirst, onSecond, onThird athlete IDs
#'   \item Game situation: outs, scores, at-bat details
#' }
#'
#' **Rich Data Access Examples**:
#' \itemize{
#'   \item \code{mlb_pbp_plays$pitchVelocity} - Pitch speeds
#'   \item \code{mlb_pbp_plays$pitchType$text} - Pitch type names
#'   \item \code{mlb_pbp_plays$participants} - Batter/pitcher details
#'   \item \code{mlb_pbp_plays$onSecond$athlete$id} - Runner on second
#'   \item \code{mlb_pbp_plays$pitchCoordinate} - Pitch location (x,y)
#' }
#'
#' @examples
#' \dontrun{
#' # Get complete play-by-play for a specific game
#' fetch_mlb_game_playbyplay(event_id = "401581132")
#'
#' # Check what data was created
#' head(mlb_pbp_game_summary)
#' head(mlb_pbp_inning_summary)
#' head(mlb_pbp_plays)
#'
#' # Access nested data structures
#' mlb_pbp_plays$period$number          # Inning numbers
#' mlb_pbp_plays$pitchType$text         # Pitch type names
#' mlb_pbp_plays$pitchCoordinate$x      # Pitch location X
#'
#' # Get only plays data
#' fetch_mlb_game_playbyplay(event_id = "401581132", return_type = "plays")
#'
#' # Analyze four-seam fastballs over 95 mph
#' fastballs <- mlb_pbp_plays[
#'   mlb_pbp_plays$pitchType$text == "Four-seam FB" &
#'   mlb_pbp_plays$pitchVelocity > 95 &
#'   !is.na(mlb_pbp_plays$pitchVelocity),
#' ]
#'
#' # Find all scoring plays
#' scoring_plays <- mlb_pbp_plays[
#'   mlb_pbp_plays$scoringPlay == TRUE,
#'   c("text", "homeScore", "awayScore", "scoreValue")
#' ]
#'
#' # Analyze pitch sequences in specific counts
#' two_strike_pitches <- mlb_pbp_plays[
#'   mlb_pbp_plays$pitchCount$strikes == 2 &
#'   !is.na(mlb_pbp_plays$pitchType$text),
#' ]
#'
#' # View baserunner situations
#' risp_situations <- mlb_pbp_plays[
#'   !is.na(mlb_pbp_plays$onSecond$athlete$id) |
#'   !is.na(mlb_pbp_plays$onThird$athlete$id),
#' ]
#'
#' # Hit coordinate analysis
#' hits_with_coords <- mlb_pbp_plays[
#'   !is.na(mlb_pbp_plays$hitCoordinate$x) &
#'   !is.na(mlb_pbp_plays$hitCoordinate$y),
#' ]
#'
#' # Get raw data for debugging
#' fetch_mlb_game_playbyplay(event_id = "401581132", raw = TRUE)
#' }
#'
#' @importFrom httr GET status_code content timeout
#' @importFrom jsonlite fromJSON
#' @export
fetch_mlb_game_playbyplay <- function(event_id, return_type = "all", raw = FALSE) {
  # Input validation
  if (missing(event_id)) {
    stop("'event_id' is a required parameter")
  }

  valid_types <- c("game_summary", "inning_summary", "plays", "all")
  if (!return_type %in% valid_types) {
    stop(sprintf("'return_type' must be one of: %s", paste(valid_types, collapse = ", ")))
  }

  if (!is.logical(raw)) {
    stop("'raw' parameter must be logical (TRUE or FALSE)")
  }

  # Convert event_id to character for URL building
  event_id <- as.character(event_id)

  # Build API URL for ESPN MLB play-by-play
  url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/baseball/mlb/summary?event=%s", event_id)

  message(sprintf("Fetching MLB play-by-play for game %s...", event_id))

  # Fetch and parse data
  tryCatch({
    resp <- httr::GET(url, httr::timeout(60))

    if (httr::status_code(resp) != 200) {
      stop(sprintf("API request failed: HTTP %d - %s",
                   httr::status_code(resp),
                   httr::http_status(resp)$message))
    }

    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content_text, simplifyVector = TRUE, simplifyDataFrame = TRUE)

    # Handle raw data assignment for debugging
    if (isTRUE(raw)) {
      assign("mlb_pbp_raw", data, envir = .GlobalEnv)
      message("Raw MLB play-by-play data assigned to: mlb_pbp_raw")
      message("Data structure preview:")

      # Show available sections
      sections <- names(data)
      message("- Available sections: ", paste(sections, collapse = ", "))

      # Show plays info if available
      if ("plays" %in% names(data)) {
        plays <- data$plays
        if (is.data.frame(plays)) {
          message("- Total plays found: ", nrow(plays))
          message("- Plays columns: ", paste(names(plays), collapse = ", "))
        }
      }

      return(invisible(data))
    }

    # Create datasets
    datasets <- create_mlb_playbyplay_datasets(data, event_id)

    # Assign data based on return_type
    result_data <- list()

    if (return_type %in% c("game_summary", "all")) {
      assign("mlb_pbp_game_summary", datasets$game_summary, envir = .GlobalEnv)
      result_data$game_summary <- datasets$game_summary
      message(sprintf("MLB play-by-play game summary assigned to: mlb_pbp_game_summary (%d records)",
                      nrow(datasets$game_summary)))
    }

    if (return_type %in% c("inning_summary", "all")) {
      assign("mlb_pbp_inning_summary", datasets$inning_summary, envir = .GlobalEnv)
      result_data$inning_summary <- datasets$inning_summary
      message(sprintf("MLB play-by-play inning summary assigned to: mlb_pbp_inning_summary (%d half-innings)",
                      nrow(datasets$inning_summary)))
    }

    if (return_type %in% c("plays", "all")) {
      assign("mlb_pbp_plays", datasets$plays, envir = .GlobalEnv)
      result_data$plays <- datasets$plays
      message(sprintf("MLB play-by-play plays assigned to: mlb_pbp_plays (%d plays)",
                      nrow(datasets$plays)))

      # Show sample data structure info
      if (nrow(datasets$plays) > 0) {
        total_cols <- ncol(datasets$plays)
        message(sprintf("  - Total columns: %d", total_cols))

        # Show pitch data availability
        if ("pitchVelocity" %in% names(datasets$plays)) {
          pitch_count <- sum(!is.na(datasets$plays$pitchVelocity))
          message(sprintf("  - Pitches with velocity data: %d", pitch_count))
        }

        # Show play types
        if ("type" %in% names(datasets$plays) && is.data.frame(datasets$plays$type)) {
          play_types <- table(datasets$plays$type$text)
          if (length(play_types) > 0) {
            message("Sample play types:")
            for (play_type in names(play_types)[1:min(3, length(play_types))]) {
              message(sprintf("    %s: %d", play_type, play_types[play_type]))
            }
          }
        }
      }
    }

    # Return appropriate data structure
    if (return_type == "all") {
      return(invisible(result_data))
    } else {
      return(invisible(result_data[[return_type]]))
    }

  }, error = function(e) {
    stop(sprintf("Failed to fetch MLB play-by-play for event %s: %s",
                 event_id, e$message))
  })
}

#' Fetch multiple MLB game play-by-play data
#'
#' Retrieves play-by-play data for multiple MLB games with rate limiting to
#' be respectful to ESPN's API. This function calls \code{\link{fetch_mlb_game_playbyplay}}
#' for each game and combines the results.
#'
#' @param event_ids Character or Numeric vector. ESPN event IDs.
#'   Vector of unique identifiers for games in ESPN's database.
#' @param return_type Character. Type of data to return (default: "all").
#'   Same options as \code{\link{fetch_mlb_game_playbyplay}}.
#' @param delay Numeric. Delay in seconds between API requests (default: 0.3).
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
#' This is particularly useful for building comprehensive baseball databases,
#' analyzing pitching strategies across multiple games, studying hitting
#' patterns over time, or conducting advanced sabermetric research.
#'
#' @examples
#' \dontrun{
#' # Get play-by-play for multiple games
#' game_ids <- c("401581132", "401581133", "401581134")
#' fetch_multiple_mlb_game_playbyplay(game_ids)
#'
#' # Get only plays data for multiple games
#' fetch_multiple_mlb_game_playbyplay(game_ids, return_type = "plays")
#'
#' # Use longer delay for larger requests
#' fetch_multiple_mlb_game_playbyplay(game_ids, delay = 0.5)
#'
#' # Analyze combined results
#' unique_games <- unique(mlb_pbp_game_summary$event_id)
#' cat("Retrieved play-by-play for", length(unique_games), "games\n")
#'
#' # Home run analysis across multiple games
#' home_runs <- mlb_pbp_plays[
#'   grepl("home run", mlb_pbp_plays$text, ignore.case = TRUE),
#'   c("event_id", "text", "pitchVelocity", "pitchType")
#' ]
#'
#' # Clutch hitting analysis (runners in scoring position, 2 outs)
#' clutch_situations <- mlb_pbp_plays[
#'   mlb_pbp_plays$outs == 2 &
#'   (!is.na(mlb_pbp_plays$onSecond$athlete$id) |
#'    !is.na(mlb_pbp_plays$onThird$athlete$id)),
#' ]
#'
#' # Pitching velocity trends
#' pitch_velo <- mlb_pbp_plays[
#'   !is.na(mlb_pbp_plays$pitchVelocity) &
#'   !is.na(mlb_pbp_plays$pitchType$text),
#'   c("event_id", "pitchType", "pitchVelocity")
#' ]
#' }
#'
#' @seealso \code{\link{fetch_mlb_game_playbyplay}} for single game data
#' @importFrom httr GET status_code content timeout
#' @export
fetch_multiple_mlb_game_playbyplay <- function(event_ids, return_type = "all",
                                               delay = 0.3, raw = FALSE) {
  # Input validation
  if (length(event_ids) == 0) {
    stop("'event_ids' must contain at least one event ID")
  }

  valid_types <- c("game_summary", "inning_summary", "plays", "all")
  if (!return_type %in% valid_types) {
    stop(sprintf("'return_type' must be one of: %s", paste(valid_types, collapse = ", ")))
  }

  if (!is.numeric(delay) || delay < 0) {
    stop("'delay' must be a non-negative number")
  }

  if (!is.logical(raw)) {
    stop("'raw' parameter must be logical (TRUE or FALSE)")
  }

  # Initialize combined data containers
  all_game_summary <- data.frame()
  all_inning_summary <- data.frame()
  all_plays <- data.frame()

  message(sprintf("Starting to fetch MLB play-by-play data for %d games...", length(event_ids)))

  # Process each game sequentially
  for (i in seq_along(event_ids)) {
    event_id <- event_ids[i]
    message(sprintf("Fetching MLB play-by-play for event %s (%d/%d)...", event_id, i, length(event_ids)))

    tryCatch({
      # Fetch individual game data
      game_data <- fetch_mlb_game_playbyplay(
        event_id = event_id,
        return_type = return_type,
        raw = raw
      )

      # If raw data requested, return after first game
      if (isTRUE(raw)) {
        return(invisible(game_data))
      }

      # Combine data based on return type
      if (return_type %in% c("game_summary", "all")) {
        summary_df <- get("mlb_pbp_game_summary", envir = .GlobalEnv)
        all_game_summary <- rbind(all_game_summary, summary_df)
      }

      if (return_type %in% c("inning_summary", "all")) {
        inning_df <- get("mlb_pbp_inning_summary", envir = .GlobalEnv)
        all_inning_summary <- rbind(all_inning_summary, inning_df)
      }

      if (return_type %in% c("plays", "all")) {
        plays_df <- get("mlb_pbp_plays", envir = .GlobalEnv)
        all_plays <- rbind(all_plays, plays_df)
      }

    }, error = function(e) {
      message(sprintf("Failed to fetch MLB play-by-play for event %s: %s", event_id, e$message))
    })

    # Be respectful to the API with delay between requests
    if (i < length(event_ids)) {
      Sys.sleep(delay)
    }
  }

  # Remove duplicates and assign combined datasets to global environment
  result_data <- list()

  if (return_type %in% c("game_summary", "all") && nrow(all_game_summary) > 0) {
    all_game_summary <- all_game_summary[!duplicated(all_game_summary$event_id), ]
    assign("mlb_pbp_game_summary", all_game_summary, envir = .GlobalEnv)
    result_data$game_summary <- all_game_summary
    message(sprintf("Combined MLB play-by-play game summary assigned to: mlb_pbp_game_summary (%d games)",
                    nrow(all_game_summary)))
  }

  if (return_type %in% c("inning_summary", "all") && nrow(all_inning_summary) > 0) {
    all_inning_summary <- all_inning_summary[!duplicated(paste(all_inning_summary$event_id,
                                                               all_inning_summary$inning,
                                                               all_inning_summary$inning_half)), ]
    assign("mlb_pbp_inning_summary", all_inning_summary, envir = .GlobalEnv)
    result_data$inning_summary <- all_inning_summary
    unique_games <- length(unique(all_inning_summary$event_id))
    message(sprintf("Combined MLB play-by-play inning summary assigned to: mlb_pbp_inning_summary (%d games, %d half-innings)",
                    unique_games, nrow(all_inning_summary)))
  }

  if (return_type %in% c("plays", "all") && nrow(all_plays) > 0) {
    all_plays <- all_plays[!duplicated(paste(all_plays$event_id, all_plays$id)), ]
    assign("mlb_pbp_plays", all_plays, envir = .GlobalEnv)
    result_data$plays <- all_plays
    unique_games <- length(unique(all_plays$event_id))
    total_plays <- nrow(all_plays)
    message(sprintf("Combined MLB play-by-play plays assigned to: mlb_pbp_plays (%d games, %d plays)",
                    unique_games, total_plays))

    # Show combined analytics
    if (nrow(all_plays) > 0) {
      # Show pitch data availability
      if ("pitchVelocity" %in% names(all_plays)) {
        pitch_count <- sum(!is.na(all_plays$pitchVelocity))
        message(sprintf("  - Total pitches with velocity data: %d", pitch_count))
      }

      # Show scoring plays
      scoring_plays <- sum(all_plays$scoringPlay, na.rm = TRUE)
      message(sprintf("  - Total scoring plays: %d", scoring_plays))

      # Show hit analysis
      if ("hitCoordinate" %in% names(all_plays) && is.data.frame(all_plays$hitCoordinate)) {
        hits_with_coords <- sum(!is.na(all_plays$hitCoordinate$x) & !is.na(all_plays$hitCoordinate$y))
        message(sprintf("  - Hits with coordinate data: %d", hits_with_coords))
      }
    }
  }

  # Return appropriate data structure
  if (return_type == "all") {
    return(invisible(result_data))
  } else {
    return(invisible(result_data[[return_type]]))
  }
}
