#' Safe nested data extraction helper function for NHL play-by-play
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_nhl_pbp <- function(data, path, default = NA) {
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

#' Create NHL play-by-play data frames from API response
#'
#' Processes raw JSON response from ESPN NHL API into structured data frames
#' containing detailed play-by-play information
#'
#' @param data Raw JSON response from ESPN NHL API play-by-play endpoint
#' @param event_id Character. Event ID used in request
#' @return List containing multiple data frames with play-by-play information
#' @keywords internal
create_nhl_playbyplay_datasets <- function(data, event_id) {
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
    total_periods = character(0),
    venue_name = character(0),
    attendance = character(0),
    stringsAsFactors = FALSE
  )

  period_summary_df <- data.frame(
    event_id = character(0),
    period = character(0),
    period_type = character(0),
    home_score = character(0),
    away_score = character(0),
    goals_scored = character(0),
    play_count = character(0),
    stringsAsFactors = FALSE
  )

  # Extract game header information for summaries
  header <- extract_nested_nhl_pbp(data, c("header"), list())
  season_info <- extract_nested_nhl_pbp(header, c("season"), list())
  season_year <- extract_nested_nhl_pbp(season_info, c("year"), NA_character_)
  season_type <- extract_nested_nhl_pbp(season_info, c("type"), NA_character_)
  venue_name <- attendance <- NA_character_

  # Extract team information from competition
  competitions <- extract_nested_nhl_pbp(header, c("competitions"), list())
  home_team_id <- away_team_id <- NA_character_
  home_team_name <- away_team_name <- NA_character_
  home_team_score <- away_team_score <- NA_character_
  game_date <- game_status <- NA_character_

  if (length(competitions) > 0) {
    competition <- competitions[[1]]
    game_date <- extract_nested_nhl_pbp(competition, c("date"), NA_character_)

    # Venue information
    venue <- extract_nested_nhl_pbp(competition, c("venue"), list())
    venue_name <- extract_nested_nhl_pbp(venue, c("fullName"), NA_character_)
    attendance <- extract_nested_nhl_pbp(competition, c("attendance"), NA_character_)

    # Status
    status <- extract_nested_nhl_pbp(competition, c("status"), list())
    status_type <- extract_nested_nhl_pbp(status, c("type"), list())
    game_status <- extract_nested_nhl_pbp(status_type, c("name"), NA_character_)

    # Teams
    competitors <- extract_nested_nhl_pbp(competition, c("competitors"), list())
    for (competitor in competitors) {
      team_info <- extract_nested_nhl_pbp(competitor, c("team"), list())
      team_id <- extract_nested_nhl_pbp(team_info, c("id"), NA_character_)
      team_name <- extract_nested_nhl_pbp(team_info, c("displayName"), NA_character_)
      team_score <- extract_nested_nhl_pbp(competitor, c("score"), NA_character_)
      home_away <- extract_nested_nhl_pbp(competitor, c("homeAway"), NA_character_)

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
  plays_df <- extract_nested_nhl_pbp(data, c("plays"), data.frame())

  # The plays data frame is already complete with all nested structures intact
  if (nrow(plays_df) > 0) {
    # Just add the event_id column to the existing plays data frame
    plays_df$event_id <- as.character(event_id)
    # Reorder columns to put event_id first
    col_order <- c("event_id", setdiff(names(plays_df), "event_id"))
    plays_df <- plays_df[, col_order]
    total_plays <- nrow(plays_df)

    # Calculate total periods from the data
    if ("period" %in% names(plays_df) && is.data.frame(plays_df$period)) {
      max_period <- max(plays_df$period$number, na.rm = TRUE)
      total_periods <- if (is.finite(max_period)) max_period else 3
    } else {
      total_periods <- 3
    }
  } else {
    plays_df <- data.frame(event_id = character(0))
    total_plays <- 0
    total_periods <- 3
  }

  # Create simple period summaries if plays data exists
  if (nrow(plays_df) > 0 && "period" %in% names(plays_df) && is.data.frame(plays_df$period)) {
    # Get unique period numbers - NHL doesn't typically have period "type" like baseball innings
    unique_periods <- unique(plays_df$period$number)
    unique_periods <- unique_periods[!is.na(unique_periods)]

    for (period_num in unique_periods) {
      # Find plays in this period
      period_plays <- plays_df[plays_df$period$number == period_num & !is.na(plays_df$period$number), ]

      if (nrow(period_plays) > 0) {
        last_play <- period_plays[nrow(period_plays), ]
        first_play <- period_plays[1, ]

        # Calculate goals scored in this period
        goals_scored <- 0
        if (!is.na(first_play$homeScore) && !is.na(last_play$homeScore) &&
            !is.na(first_play$awayScore) && !is.na(last_play$awayScore)) {
          home_goals <- last_play$homeScore - first_play$homeScore
          away_goals <- last_play$awayScore - first_play$awayScore
          goals_scored <- home_goals + away_goals
        }

        # Determine period type based on period number
        period_type <- if (period_num <= 3) {
          "Regular"
        } else if (period_num == 4) {
          "Overtime"
        } else if (period_num == 5) {
          "Shootout"
        } else {
          "Extended"
        }

        period_row <- data.frame(
          event_id = as.character(event_id),
          period = as.character(period_num),
          period_type = as.character(period_type),
          home_score = as.character(last_play$homeScore),
          away_score = as.character(last_play$awayScore),
          goals_scored = as.character(goals_scored),
          play_count = as.character(nrow(period_plays)),
          stringsAsFactors = FALSE
        )
        period_summary_df <- rbind(period_summary_df, period_row)
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
    total_periods = as.character(total_periods),
    venue_name = as.character(venue_name),
    attendance = as.character(attendance),
    stringsAsFactors = FALSE
  )
  game_summary_df <- rbind(game_summary_df, summary_row)

  # Clean up row names
  if (nrow(game_summary_df) > 0) rownames(game_summary_df) <- NULL
  if (nrow(period_summary_df) > 0) rownames(period_summary_df) <- NULL
  if (nrow(plays_df) > 0) rownames(plays_df) <- NULL

  return(list(
    game_summary = game_summary_df,
    period_summary = period_summary_df,
    plays = plays_df
  ))
}

#' Fetch NHL game play-by-play data using ESPN API
#'
#' Retrieves detailed play-by-play information from ESPN's NHL API.
#' The function fetches comprehensive play data for a specific NHL game.
#'
#' @param event_id Character or Numeric. ESPN event ID (required).
#'   The unique identifier for the game in ESPN's database.
#' @param return_type Character. Type of data to return (default: "all").
#'   Options are:
#'   \itemize{
#'     \item "game_summary" - Basic game information and totals
#'     \item "period_summary" - Period-by-period scoring summary
#'     \item "plays" - Individual play-by-play data
#'     \item "all" - All data types combined
#'   }
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'nhl_pbp_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data. The main purpose is global
#'   environment assignment based on return_type:
#'   \itemize{
#'     \item \code{nhl_pbp_game_summary} - Game summary data frame
#'     \item \code{nhl_pbp_period_summary} - Period summary data frame
#'     \item \code{nhl_pbp_plays} - Complete plays data frame with all ESPN fields
#'     \item \code{nhl_pbp_raw} - Raw JSON response (if raw = TRUE)
#'   }
#'
#' @details
#' The function creates structured data frames with comprehensive play-by-play information:
#'
#' **Game Summary** (\code{nhl_pbp_game_summary}):
#' \itemize{
#'   \item Game details: event_id, game_date, game_status, season info
#'   \item Team matchup: home_team vs away_team with final scores
#'   \item Venue, attendance, and total plays/periods information
#' }
#'
#' **Period Summary** (\code{nhl_pbp_period_summary}):
#' \itemize{
#'   \item Period-by-period scoring breakdown (1st, 2nd, 3rd, OT, SO)
#'   \item Goals scored per period
#'   \item Play counts per period
#'   \item Running score after each period
#' }
#'
#' **Plays** (\code{nhl_pbp_plays}):
#' \itemize{
#'   \item Complete ESPN plays data frame with all 25+ columns preserved
#'   \item Nested data frames: period, type, participants, coordinate, etc.
#'   \item Shot tracking: coordinates, shot type, save/goal details
#'   \item Penalty data: infraction type, duration, player details
#'   \item Faceoff tracking: zone, winner details
#'   \item Game situation: score, time remaining, strength
#' }
#'
#' **Rich Data Access Examples**:
#' \itemize{
#'   \item \code{nhl_pbp_plays$shotType$text} - Shot type (wrist shot, slap shot, etc.)
#'   \item \code{nhl_pbp_plays$participants} - Player details for each play
#'   \item \code{nhl_pbp_plays$coordinate} - Play location coordinates (x,y)
#'   \item \code{nhl_pbp_plays$strength$abbreviation} - Game strength (EV, PP, SH)
#'   \item \code{nhl_pbp_plays$penaltyType$text} - Penalty infraction details
#' }
#'
#' @examples
#' \dontrun{
#' # Get complete play-by-play for a specific game
#' fetch_nhl_game_playbyplay(event_id = "401589281")
#'
#' # Check what data was created
#' head(nhl_pbp_game_summary)
#' head(nhl_pbp_period_summary)
#' head(nhl_pbp_plays)
#'
#' # Access nested data structures
#' nhl_pbp_plays$period$number          # Period numbers
#' nhl_pbp_plays$shotType$text          # Shot type names
#' nhl_pbp_plays$coordinate$x           # Play location X
#'
#' # Get only plays data
#' fetch_nhl_game_playbyplay(event_id = "401589281", return_type = "plays")
#'
#' # Analyze power play goals
#' pp_goals <- nhl_pbp_plays[
#'   nhl_pbp_plays$scoringPlay == TRUE &
#'   nhl_pbp_plays$strength$abbreviation == "PP" &
#'   !is.na(nhl_pbp_plays$strength$abbreviation),
#' ]
#'
#' # Find all penalty plays
#' penalties <- nhl_pbp_plays[
#'   nhl_pbp_plays$type$text == "Penalty",
#'   c("text", "penaltyType", "participants")
#' ]
#'
#' # Analyze shot locations for goals
#' goals_with_coords <- nhl_pbp_plays[
#'   nhl_pbp_plays$scoringPlay == TRUE &
#'   !is.na(nhl_pbp_plays$coordinate$x) &
#'   !is.na(nhl_pbp_plays$coordinate$y),
#' ]
#'
#' # Faceoff analysis by zone
#' faceoffs <- nhl_pbp_plays[
#'   nhl_pbp_plays$type$text == "Faceoff",
#'   c("text", "coordinate", "participants")
#' ]
#'
#' # Power play opportunities
#' power_plays <- nhl_pbp_plays[
#'   nhl_pbp_plays$strength$abbreviation %in% c("PP", "SH") &
#'   !is.na(nhl_pbp_plays$strength$abbreviation),
#' ]
#'
#' # Get raw data for debugging
#' fetch_nhl_game_playbyplay(event_id = "401589281", raw = TRUE)
#' }
#'
#' @importFrom httr GET status_code content timeout
#' @importFrom jsonlite fromJSON
#' @export
fetch_nhl_game_playbyplay <- function(event_id, return_type = "all", raw = FALSE) {
  # Input validation
  if (missing(event_id)) {
    stop("'event_id' is a required parameter")
  }

  valid_types <- c("game_summary", "period_summary", "plays", "all")
  if (!return_type %in% valid_types) {
    stop(sprintf("'return_type' must be one of: %s", paste(valid_types, collapse = ", ")))
  }

  if (!is.logical(raw)) {
    stop("'raw' parameter must be logical (TRUE or FALSE)")
  }

  # Convert event_id to character for URL building
  event_id <- as.character(event_id)

  # Build API URL for ESPN NHL play-by-play
  url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/hockey/nhl/summary?event=%s", event_id)

  message(sprintf("Fetching NHL play-by-play for game %s...", event_id))

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
      assign("nhl_pbp_raw", data, envir = .GlobalEnv)
      message("Raw NHL play-by-play data assigned to: nhl_pbp_raw")
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
    datasets <- create_nhl_playbyplay_datasets(data, event_id)

    # Assign data based on return_type
    result_data <- list()

    if (return_type %in% c("game_summary", "all")) {
      assign("nhl_pbp_game_summary", datasets$game_summary, envir = .GlobalEnv)
      result_data$game_summary <- datasets$game_summary
      message(sprintf("NHL play-by-play game summary assigned to: nhl_pbp_game_summary (%d records)",
                      nrow(datasets$game_summary)))
    }

    if (return_type %in% c("period_summary", "all")) {
      assign("nhl_pbp_period_summary", datasets$period_summary, envir = .GlobalEnv)
      result_data$period_summary <- datasets$period_summary
      message(sprintf("NHL play-by-play period summary assigned to: nhl_pbp_period_summary (%d periods)",
                      nrow(datasets$period_summary)))
    }

    if (return_type %in% c("plays", "all")) {
      assign("nhl_pbp_plays", datasets$plays, envir = .GlobalEnv)
      result_data$plays <- datasets$plays
      message(sprintf("NHL play-by-play plays assigned to: nhl_pbp_plays (%d plays)",
                      nrow(datasets$plays)))

      # Show sample data structure info
      if (nrow(datasets$plays) > 0) {
        total_cols <- ncol(datasets$plays)
        message(sprintf("  - Total columns: %d", total_cols))

        # Show scoring plays
        if ("scoringPlay" %in% names(datasets$plays)) {
          scoring_count <- sum(datasets$plays$scoringPlay, na.rm = TRUE)
          message(sprintf("  - Scoring plays: %d", scoring_count))
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

        # Show penalty data
        if ("penaltyType" %in% names(datasets$plays) && is.data.frame(datasets$plays$penaltyType)) {
          penalty_count <- sum(!is.na(datasets$plays$penaltyType$text))
          message(sprintf("  - Penalty plays: %d", penalty_count))
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
    stop(sprintf("Failed to fetch NHL play-by-play for event %s: %s",
                 event_id, e$message))
  })
}

#' Fetch multiple NHL game play-by-play data
#'
#' Retrieves play-by-play data for multiple NHL games with rate limiting to
#' be respectful to ESPN's API. This function calls \code{\link{fetch_nhl_game_playbyplay}}
#' for each game and combines the results.
#'
#' @param event_ids Character or Numeric vector. ESPN event IDs.
#'   Vector of unique identifiers for games in ESPN's database.
#' @param return_type Character. Type of data to return (default: "all").
#'   Same options as \code{\link{fetch_nhl_game_playbyplay}}.
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
#' This is particularly useful for building comprehensive hockey databases,
#' analyzing team strategies across multiple games, studying player
#' performance patterns over time, or conducting advanced hockey analytics.
#'
#' @examples
#' \dontrun{
#' # Get play-by-play for multiple games
#' game_ids <- c("401589281", "401589282", "401589283")
#' fetch_multiple_nhl_game_playbyplay(game_ids)
#'
#' # Get only plays data for multiple games
#' fetch_multiple_nhl_game_playbyplay(game_ids, return_type = "plays")
#'
#' # Use longer delay for larger requests
#' fetch_multiple_nhl_game_playbyplay(game_ids, delay = 0.5)
#'
#' # Analyze combined results
#' unique_games <- unique(nhl_pbp_game_summary$event_id)
#' cat("Retrieved play-by-play for", length(unique_games), "games\n")
#'
#' # Power play analysis across multiple games
#' pp_goals <- nhl_pbp_plays[
#'   nhl_pbp_plays$scoringPlay == TRUE &
#'   nhl_pbp_plays$strength$abbreviation == "PP" &
#'   !is.na(nhl_pbp_plays$strength$abbreviation),
#'   c("event_id", "text", "participants")
#' ]
#'
#' # Penalty analysis across games
#' penalties <- nhl_pbp_plays[
#'   nhl_pbp_plays$type$text == "Penalty" &
#'   !is.na(nhl_pbp_plays$penaltyType$text),
#'   c("event_id", "penaltyType", "participants")
#' ]
#'
#' # Shot location analysis
#' shots_with_coords <- nhl_pbp_plays[
#'   nhl_pbp_plays$type$text %in% c("Shot", "Goal") &
#'   !is.na(nhl_pbp_plays$coordinate$x) &
#'   !is.na(nhl_pbp_plays$coordinate$y),
#'   c("event_id", "coordinate", "shotType", "scoringPlay")
#' ]
#'
#' # Faceoff win percentages by zone
#' faceoffs <- nhl_pbp_plays[
#'   nhl_pbp_plays$type$text == "Faceoff",
#'   c("event_id", "coordinate", "participants")
#' ]
#' }
#'
#' @seealso \code{\link{fetch_nhl_game_playbyplay}} for single game data
#' @importFrom httr GET status_code content timeout
#' @export
fetch_multiple_nhl_game_playbyplay <- function(event_ids, return_type = "all",
                                               delay = 0.3, raw = FALSE) {
  # Input validation
  if (length(event_ids) == 0) {
    stop("'event_ids' must contain at least one event ID")
  }

  valid_types <- c("game_summary", "period_summary", "plays", "all")
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
  all_period_summary <- data.frame()
  all_plays <- data.frame()

  message(sprintf("Starting to fetch NHL play-by-play data for %d games...", length(event_ids)))

  # Process each game sequentially
  for (i in seq_along(event_ids)) {
    event_id <- event_ids[i]
    message(sprintf("Fetching NHL play-by-play for event %s (%d/%d)...", event_id, i, length(event_ids)))

    tryCatch({
      # Fetch individual game data
      game_data <- fetch_nhl_game_playbyplay(
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
        summary_df <- get("nhl_pbp_game_summary", envir = .GlobalEnv)
        all_game_summary <- rbind(all_game_summary, summary_df)
      }

      if (return_type %in% c("period_summary", "all")) {
        period_df <- get("nhl_pbp_period_summary", envir = .GlobalEnv)
        all_period_summary <- rbind(all_period_summary, period_df)
      }

      if (return_type %in% c("plays", "all")) {
        plays_df <- get("nhl_pbp_plays", envir = .GlobalEnv)
        all_plays <- rbind(all_plays, plays_df)
      }

    }, error = function(e) {
      message(sprintf("Failed to fetch NHL play-by-play for event %s: %s", event_id, e$message))
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
    assign("nhl_pbp_game_summary", all_game_summary, envir = .GlobalEnv)
    result_data$game_summary <- all_game_summary
    message(sprintf("Combined NHL play-by-play game summary assigned to: nhl_pbp_game_summary (%d games)",
                    nrow(all_game_summary)))
  }

  if (return_type %in% c("period_summary", "all") && nrow(all_period_summary) > 0) {
    all_period_summary <- all_period_summary[!duplicated(paste(all_period_summary$event_id,
                                                               all_period_summary$period,
                                                               all_period_summary$period_type)), ]
    assign("nhl_pbp_period_summary", all_period_summary, envir = .GlobalEnv)
    result_data$period_summary <- all_period_summary
    unique_games <- length(unique(all_period_summary$event_id))
    message(sprintf("Combined NHL play-by-play period summary assigned to: nhl_pbp_period_summary (%d games, %d periods)",
                    unique_games, nrow(all_period_summary)))
  }

  if (return_type %in% c("plays", "all") && nrow(all_plays) > 0) {
    all_plays <- all_plays[!duplicated(paste(all_plays$event_id, all_plays$id)), ]
    assign("nhl_pbp_plays", all_plays, envir = .GlobalEnv)
    result_data$plays <- all_plays
    unique_games <- length(unique(all_plays$event_id))
    total_plays <- nrow(all_plays)
    message(sprintf("Combined NHL play-by-play plays assigned to: nhl_pbp_plays (%d games, %d plays)",
                    unique_games, total_plays))

    # Show combined analytics
    if (nrow(all_plays) > 0) {
      # Show scoring plays
      scoring_plays <- sum(all_plays$scoringPlay, na.rm = TRUE)
      message(sprintf("  - Total scoring plays: %d", scoring_plays))

      # Show penalty data
      if ("penaltyType" %in% names(all_plays) && is.data.frame(all_plays$penaltyType)) {
        penalty_count <- sum(!is.na(all_plays$penaltyType$text))
        message(sprintf("  - Total penalty plays: %d", penalty_count))
      }

      # Show shot analysis
      if ("coordinate" %in% names(all_plays) && is.data.frame(all_plays$coordinate)) {
        shots_with_coords <- sum(!is.na(all_plays$coordinate$x) & !is.na(all_plays$coordinate$y))
        message(sprintf("  - Plays with coordinate data: %d", shots_with_coords))
      }

      # Show power play data
      if ("strength" %in% names(all_plays) && is.data.frame(all_plays$strength)) {
        pp_plays <- sum(all_plays$strength$abbreviation == "PP", na.rm = TRUE)
        message(sprintf("  - Power play situations: %d", pp_plays))
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
