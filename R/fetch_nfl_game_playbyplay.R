#' Safe nested data extraction helper function for play-by-play
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_pbp <- function(data, path, default = NA) {
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

#' Create play-by-play data frames from API response
#'
#' Processes raw JSON response from ESPN CDN API into structured data frames
#' containing detailed play-by-play information
#'
#' @param data Raw JSON response from ESPN CDN API play-by-play endpoint
#' @param event_id Character. Event ID used in request
#' @return List containing multiple data frames with play-by-play information
#' @keywords internal
create_playbyplay_datasets <- function(data, event_id) {

  # Initialize plays data frame
  plays_df <- data.frame(
    event_id = character(0),
    drive_id = character(0),
    play_id = character(0),
    sequence_number = character(0),
    period = character(0),
    clock = character(0),
    down = character(0),
    distance = character(0),
    yard_line = character(0),
    yards_to_goal = character(0),
    play_type = character(0),
    play_text = character(0),
    score_value = character(0),
    team_id = character(0),
    team_abbreviation = character(0),
    home_score = character(0),
    away_score = character(0),
    home_team_id = character(0),
    away_team_id = character(0),
    stringsAsFactors = FALSE
  )

  # Initialize drives data frame
  drives_df <- data.frame(
    event_id = character(0),
    drive_id = character(0),
    drive_number = character(0),
    team_id = character(0),
    team_abbreviation = character(0),
    start_period = character(0),
    start_clock = character(0),
    start_yard_line = character(0),
    start_yards_to_goal = character(0),
    end_period = character(0),
    end_clock = character(0),
    end_yard_line = character(0),
    end_yards_to_goal = character(0),
    plays_count = character(0),
    yards = character(0),
    time_elapsed = character(0),
    result = character(0),
    stringsAsFactors = FALSE
  )

  # Initialize game summary data frame
  game_summary_df <- data.frame(
    event_id = character(0),
    game_date = character(0),
    game_status = character(0),
    season_year = character(0),
    season_type = character(0),
    week = character(0),
    home_team_id = character(0),
    home_team_name = character(0),
    home_team_score = character(0),
    away_team_id = character(0),
    away_team_name = character(0),
    away_team_score = character(0),
    total_plays = character(0),
    total_drives = character(0),
    stringsAsFactors = FALSE
  )

  # Extract game header information
  header <- extract_nested_pbp(data, c("gamepackageJSON", "header"), list())

  # Extract basic game info
  game_id <- extract_nested_pbp(header, c("id"), event_id)
  season_info <- extract_nested_pbp(header, c("season"), list())
  season_year <- extract_nested_pbp(season_info, c("year"), NA_character_)
  season_type <- extract_nested_pbp(season_info, c("type"), NA_character_)
  week <- extract_nested_pbp(header, c("week"), NA_character_)

  # Extract team information from competition
  competitions <- extract_nested_pbp(header, c("competitions"), list())
  home_team_id <- away_team_id <- NA_character_
  home_team_name <- away_team_name <- NA_character_
  home_team_score <- away_team_score <- NA_character_
  game_date <- game_status <- NA_character_

  if (length(competitions) > 0) {
    competition <- competitions[[1]]
    game_date <- extract_nested_pbp(competition, c("date"), NA_character_)

    # Status
    status <- extract_nested_pbp(competition, c("status"), list())
    game_status <- extract_nested_pbp(status, c("type", "name"), NA_character_)

    # Teams
    competitors <- extract_nested_pbp(competition, c("competitors"), list())
    for (competitor in competitors) {
      team_info <- extract_nested_pbp(competitor, c("team"), list())
      team_id <- extract_nested_pbp(team_info, c("id"), NA_character_)
      team_name <- extract_nested_pbp(team_info, c("displayName"), NA_character_)
      team_score <- extract_nested_pbp(competitor, c("score"), NA_character_)
      home_away <- extract_nested_pbp(competitor, c("homeAway"), NA_character_)

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

  # Extract drives data from the 'previous' structure based on your data
  drives_data <- extract_nested_pbp(data, c("gamepackageJSON", "drives", "previous"), list())

  total_plays <- 0
  total_drives <- length(drives_data)

  if (length(drives_data) > 0) {
    for (i in seq_along(drives_data)) {
      drive_data <- drives_data[[i]]

      # Extract drive information
      drive_id <- extract_nested_pbp(drive_data, c("id"), as.character(i))
      drive_description <- extract_nested_pbp(drive_data, c("description"), "")

      # Drive team info
      team <- extract_nested_pbp(drive_data, c("team"), list())
      team_id <- extract_nested_pbp(team, c("id"), NA_character_)
      team_abbreviation <- extract_nested_pbp(team, c("abbreviation"), NA_character_)

      # Drive start info
      start_info <- extract_nested_pbp(drive_data, c("start"), list())
      start_period <- extract_nested_pbp(start_info, c("period", "number"), NA_character_)
      start_clock <- extract_nested_pbp(start_info, c("clock", "displayValue"), NA_character_)
      start_yard_line <- extract_nested_pbp(start_info, c("yardLine"), NA_character_)
      start_text <- extract_nested_pbp(start_info, c("text"), NA_character_)

      # Calculate yards to goal from start position
      start_yards_to_goal <- NA_character_
      if (!is.na(start_yard_line) && is.numeric(start_yard_line)) {
        start_yards_to_goal <- as.character(100 - start_yard_line)
      }

      # Drive end info
      end_info <- extract_nested_pbp(drive_data, c("end"), list())
      end_period <- extract_nested_pbp(end_info, c("period", "number"), NA_character_)
      end_clock <- extract_nested_pbp(end_info, c("clock", "displayValue"), NA_character_)
      end_yard_line <- extract_nested_pbp(end_info, c("yardLine"), NA_character_)
      end_text <- extract_nested_pbp(end_info, c("text"), NA_character_)

      # Calculate yards to goal from end position
      end_yards_to_goal <- NA_character_
      if (!is.na(end_yard_line) && is.numeric(end_yard_line)) {
        end_yards_to_goal <- as.character(100 - end_yard_line)
      }

      # Drive stats
      plays_count <- extract_nested_pbp(drive_data, c("offensivePlays"), NA_character_)
      yards <- extract_nested_pbp(drive_data, c("yards"), NA_character_)
      time_elapsed <- extract_nested_pbp(drive_data, c("timeElapsed", "displayValue"), NA_character_)
      result <- extract_nested_pbp(drive_data, c("result"), NA_character_)

      # Add drive row
      drive_row <- data.frame(
        event_id = as.character(event_id),
        drive_id = as.character(drive_id),
        drive_number = as.character(i),
        team_id = as.character(team_id),
        team_abbreviation = as.character(team_abbreviation),
        start_period = as.character(start_period),
        start_clock = as.character(start_clock),
        start_yard_line = as.character(start_yard_line),
        start_yards_to_goal = as.character(start_yards_to_goal),
        end_period = as.character(end_period),
        end_clock = as.character(end_clock),
        end_yard_line = as.character(end_yard_line),
        end_yards_to_goal = as.character(end_yards_to_goal),
        plays_count = as.character(plays_count),
        yards = as.character(yards),
        time_elapsed = as.character(time_elapsed),
        result = as.character(result),
        stringsAsFactors = FALSE
      )

      drives_df <- rbind(drives_df, drive_row)

      # Extract plays for this drive
      plays <- extract_nested_pbp(drive_data, c("plays"), list())

      for (j in seq_along(plays)) {
        play_data <- plays[[j]]

        # Extract play information
        play_id <- extract_nested_pbp(play_data, c("id"), paste(drive_id, j, sep = "_"))
        sequence_number <- extract_nested_pbp(play_data, c("sequenceNumber"), as.character(j))

        # Play situation
        period <- extract_nested_pbp(play_data, c("period", "number"), NA_character_)
        clock <- extract_nested_pbp(play_data, c("clock", "displayValue"), NA_character_)
        down <- extract_nested_pbp(play_data, c("down"), NA_character_)
        distance <- extract_nested_pbp(play_data, c("distance"), NA_character_)
        yard_line <- extract_nested_pbp(play_data, c("yardLine"), NA_character_)

        # Calculate yards to goal
        yards_to_goal <- NA_character_
        if (!is.na(yard_line) && is.numeric(yard_line)) {
          yards_to_goal <- as.character(100 - yard_line)
        }

        # Play details
        play_type <- extract_nested_pbp(play_data, c("type", "text"), NA_character_)
        play_text <- extract_nested_pbp(play_data, c("text"), NA_character_)

        # Scoring
        score_value <- extract_nested_pbp(play_data, c("scoreValue"), "0")

        # Current score
        home_score <- extract_nested_pbp(play_data, c("homeScore"), NA_character_)
        away_score <- extract_nested_pbp(play_data, c("awayScore"), NA_character_)

        # Play team (if different from drive team)
        play_team <- extract_nested_pbp(play_data, c("team"), list())
        play_team_id <- extract_nested_pbp(play_team, c("id"), team_id)
        play_team_abbreviation <- extract_nested_pbp(play_team, c("abbreviation"), team_abbreviation)

        # Add play row
        play_row <- data.frame(
          event_id = as.character(event_id),
          drive_id = as.character(drive_id),
          play_id = as.character(play_id),
          sequence_number = as.character(sequence_number),
          period = as.character(period),
          clock = as.character(clock),
          down = as.character(down),
          distance = as.character(distance),
          yard_line = as.character(yard_line),
          yards_to_goal = as.character(yards_to_goal),
          play_type = as.character(play_type),
          play_text = as.character(play_text),
          score_value = as.character(score_value),
          team_id = as.character(play_team_id),
          team_abbreviation = as.character(play_team_abbreviation),
          home_score = as.character(home_score),
          away_score = as.character(away_score),
          home_team_id = as.character(home_team_id),
          away_team_id = as.character(away_team_id),
          stringsAsFactors = FALSE
        )

        plays_df <- rbind(plays_df, play_row)
        total_plays <- total_plays + 1
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
    week = as.character(week),
    home_team_id = as.character(home_team_id),
    home_team_name = as.character(home_team_name),
    home_team_score = as.character(home_team_score),
    away_team_id = as.character(away_team_id),
    away_team_name = as.character(away_team_name),
    away_team_score = as.character(away_team_score),
    total_plays = as.character(total_plays),
    total_drives = as.character(total_drives),
    stringsAsFactors = FALSE
  )

  game_summary_df <- rbind(game_summary_df, summary_row)

  # Clean up row names
  if (nrow(game_summary_df) > 0) rownames(game_summary_df) <- NULL
  if (nrow(drives_df) > 0) rownames(drives_df) <- NULL
  if (nrow(plays_df) > 0) rownames(plays_df) <- NULL

  return(list(
    game_summary = game_summary_df,
    drives = drives_df,
    plays = plays_df
  ))
}

#' Fetch NFL game play-by-play data using CDN API
#'
#' Retrieves detailed play-by-play information from ESPN's CDN API.
#' The function fetches comprehensive drive and play data
#' for a specific NFL game.
#'
#' @param event_id Character or Numeric. ESPN event ID (required).
#'   The unique identifier for the game in ESPN's database.
#' @param return_type Character. Type of data to return (default: "all").
#'   Options are:
#'   \itemize{
#'     \item "game_summary" - Basic game information and totals
#'     \item "drives" - Drive-level information
#'     \item "plays" - Individual play-by-play data
#'     \item "all" - All data types combined
#'   }
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'nfl_pbp_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data. The main purpose is global
#'   environment assignment based on return_type:
#'   \itemize{
#'     \item \code{nfl_pbp_game_summary} - Game summary data frame
#'     \item \code{nfl_pbp_drives} - Drives data frame
#'     \item \code{nfl_pbp_plays} - Plays data frame
#'     \item \code{nfl_pbp_raw} - Raw JSON response (if raw = TRUE)
#'   }
#'
#' @details
#' The function creates structured data frames with comprehensive play-by-play information:
#'
#' **Game Summary** (\code{nfl_pbp_game_summary}):
#' \itemize{
#'   \item Game details: event_id, game_date, game_status, season info
#'   \item Team matchup: home_team vs away_team with final scores
#'   \item Totals: total_plays, total_drives
#' }
#'
#' **Drives** (\code{nfl_pbp_drives}):
#' \itemize{
#'   \item Drive-level information: start/end field position and time
#'   \item Drive statistics: plays count, yards gained, time elapsed
#'   \item Drive results: touchdown, field goal, punt, turnover, etc.
#' }
#'
#' **Plays** (\code{nfl_pbp_plays}):
#' \itemize{
#'   \item Individual play details: down, distance, yard line, clock
#'   \item Play description and type classification
#'   \item Scoring plays and running score after each play
#'   \item Field position and game situation context
#' }
#'
#' This provides the most detailed game flow information available,
#' including every play call, field position changes, and scoring events.
#' Perfect for advanced analytics, win probability models, and detailed
#' game analysis.
#'
#' @examples
#' \dontrun{
#' # Get complete play-by-play for a specific game
#' fetch_nfl_game_playbyplay(event_id = "401671617")
#'
#' # Check what data was created
#' head(nfl_pbp_game_summary)
#' head(nfl_pbp_drives)
#' head(nfl_pbp_plays)
#'
#' # Get only plays data
#' fetch_nfl_game_playbyplay(event_id = "401671617", return_type = "plays")
#'
#' # Analyze scoring plays
#' scoring_plays <- nfl_pbp_plays[
#'   as.numeric(nfl_pbp_plays$score_value) > 0,
#'   c("period", "clock", "team_abbreviation", "play_text", "score_value")
#' ]
#' print(scoring_plays)
#'
#' # View red zone drives
#' red_zone_drives <- nfl_pbp_drives[
#'   as.numeric(nfl_pbp_drives$start_yards_to_goal) <= 20 &
#'   !is.na(nfl_pbp_drives$start_yards_to_goal),
#'   c("team_abbreviation", "start_yards_to_goal", "result", "yards")
#' ]
#' print(red_zone_drives)
#'
#' # Third down conversion analysis
#' third_downs <- nfl_pbp_plays[
#'   nfl_pbp_plays$down == "3",
#'   c("team_abbreviation", "distance", "play_type", "play_text")
#' ]
#' head(third_downs)
#'
#' # Get raw data for debugging
#' fetch_nfl_game_playbyplay(event_id = "401671617", raw = TRUE)
#' str(nfl_pbp_raw, max.level = 2)
#' }
#'
#' @seealso \code{\link{fetch_multiple_nfl_game_playbyplay}} for fetching
#'   multiple games' data
#'
#' @export
fetch_nfl_game_playbyplay <- function(event_id, return_type = "all", raw = FALSE) {

  # Input validation
  if (missing(event_id)) {
    stop("'event_id' is a required parameter")
  }

  valid_types <- c("game_summary", "drives", "plays", "all")
  if (!return_type %in% valid_types) {
    stop(sprintf("'return_type' must be one of: %s", paste(valid_types, collapse = ", ")))
  }

  # Convert event_id to character for URL building
  event_id <- as.character(event_id)

  # Build API URL for CDN API
  url <- sprintf("https://cdn.espn.com/core/nfl/playbyplay?xhr=1&gameId=%s", event_id)

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
      assign("nfl_pbp_raw", data, envir = .GlobalEnv)
      message("Raw NFL play-by-play data assigned to: nfl_pbp_raw")
      message("Data structure preview:")

      # Show available sections
      sections <- names(data)
      message("- Available sections: ", paste(sections, collapse = ", "))

      # Show gamepackageJSON structure if available
      gamepackage <- extract_nested_pbp(data, c("gamepackageJSON"), list())
      if (length(gamepackage) > 0) {
        gp_sections <- names(gamepackage)
        message("- GamepackageJSON sections: ", paste(gp_sections, collapse = ", "))

        # Show drives info
        drives <- extract_nested_pbp(gamepackage, c("drives"), list())
        if (length(drives) > 0) {
          message("- Total drives structure found")
          if ("previous" %in% names(drives)) {
            previous_drives <- extract_nested_pbp(drives, c("previous"), list())
            message("- Previous drives: ", length(previous_drives))
          }
        }
      }

      return(invisible(data))
    }

    # Create datasets
    datasets <- create_playbyplay_datasets(data, event_id)

    # Assign data based on return_type
    result_data <- list()

    if (return_type %in% c("game_summary", "all")) {
      assign("nfl_pbp_game_summary", datasets$game_summary, envir = .GlobalEnv)
      result_data$game_summary <- datasets$game_summary
      message(sprintf("Play-by-play game summary assigned to: nfl_pbp_game_summary (%d records)", nrow(datasets$game_summary)))
    }

    if (return_type %in% c("drives", "all")) {
      assign("nfl_pbp_drives", datasets$drives, envir = .GlobalEnv)
      result_data$drives <- datasets$drives
      message(sprintf("Play-by-play drives assigned to: nfl_pbp_drives (%d drives)", nrow(datasets$drives)))
    }

    if (return_type %in% c("plays", "all")) {
      assign("nfl_pbp_plays", datasets$plays, envir = .GlobalEnv)
      result_data$plays <- datasets$plays
      message(sprintf("Play-by-play plays assigned to: nfl_pbp_plays (%d plays)", nrow(datasets$plays)))
    }

    # Return appropriate data structure
    if (return_type == "all") {
      return(invisible(result_data))
    } else {
      return(invisible(result_data[[return_type]]))
    }

  }, error = function(e) {
    stop(sprintf("Failed to fetch NFL play-by-play for event %s: %s",
                 event_id, e$message))
  })
}

#' Fetch multiple NFL game play-by-play data
#'
#' Retrieves play-by-play data for multiple NFL games with rate limiting to
#' be respectful to ESPN's CDN API. This function calls \code{\link{fetch_nfl_game_playbyplay}}
#' for each game and combines the results.
#'
#' @param event_ids Character or Numeric vector. ESPN event IDs.
#'   Vector of unique identifiers for games in ESPN's database.
#' @param return_type Character. Type of data to return (default: "all").
#'   Same options as \code{\link{fetch_nfl_game_playbyplay}}.
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
#' This is particularly useful for building comprehensive play databases,
#' analyzing coaching tendencies across multiple games, or studying
#' situational football patterns over time.
#'
#' @examples
#' \dontrun{
#' # Get play-by-play for multiple games from same week
#' week1_games <- c("401671617", "401671618", "401671619")
#' fetch_multiple_nfl_game_playbyplay(week1_games)
#'
#' # Get only plays data for multiple games
#' fetch_multiple_nfl_game_playbyplay(week1_games, return_type = "plays")
#'
#' # Use longer delay for larger requests
#' fetch_multiple_nfl_game_playbyplay(week1_games, delay = 0.5)
#'
#' # Analyze combined results
#' unique_games <- unique(nfl_pbp_game_summary$event_id)
#' cat("Retrieved play-by-play for", length(unique_games), "games\n")
#'
#' # Fourth down decision analysis across multiple games
#' fourth_downs <- nfl_pbp_plays[
#'   nfl_pbp_plays$down == "4",
#'   c("event_id", "team_abbreviation", "distance", "yards_to_goal", "play_type")
#' ]
#' table(fourth_downs$play_type)
#' }
#'
#' @seealso \code{\link{fetch_nfl_game_playbyplay}} for single game data
#' @export
fetch_multiple_nfl_game_playbyplay <- function(event_ids, return_type = "all",
                                               delay = 0.2, raw = FALSE) {
  # Input validation
  if (length(event_ids) == 0) {
    stop("'event_ids' must contain at least one event ID")
  }

  valid_types <- c("game_summary", "drives", "plays", "all")
  if (!return_type %in% valid_types) {
    stop(sprintf("'return_type' must be one of: %s", paste(valid_types, collapse = ", ")))
  }

  # Initialize combined data containers
  all_game_summary <- data.frame()
  all_drives <- data.frame()
  all_plays <- data.frame()

  message(sprintf("Starting to fetch play-by-play data for %d games...", length(event_ids)))

  # Process each game sequentially
  for (i in seq_along(event_ids)) {
    event_id <- event_ids[i]
    message(sprintf("Fetching play-by-play for event %s (%d/%d)...", event_id, i, length(event_ids)))

    tryCatch({
      # Fetch individual game data
      game_data <- fetch_nfl_game_playbyplay(
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
        summary_df <- get("nfl_pbp_game_summary", envir = .GlobalEnv)
        all_game_summary <- rbind(all_game_summary, summary_df)
      }

      if (return_type %in% c("drives", "all")) {
        drives_df <- get("nfl_pbp_drives", envir = .GlobalEnv)
        all_drives <- rbind(all_drives, drives_df)
      }

      if (return_type %in% c("plays", "all")) {
        plays_df <- get("nfl_pbp_plays", envir = .GlobalEnv)
        all_plays <- rbind(all_plays, plays_df)
      }

    }, error = function(e) {
      message(sprintf("Failed to fetch play-by-play for event %s: %s", event_id, e$message))
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
    assign("nfl_pbp_game_summary", all_game_summary, envir = .GlobalEnv)
    result_data$game_summary <- all_game_summary

    message(sprintf("Combined play-by-play game summary assigned to: nfl_pbp_game_summary (%d games)", nrow(all_game_summary)))
  }

  if (return_type %in% c("drives", "all") && nrow(all_drives) > 0) {
    all_drives <- all_drives[!duplicated(paste(all_drives$event_id, all_drives$drive_id)), ]
    assign("nfl_pbp_drives", all_drives, envir = .GlobalEnv)
    result_data$drives <- all_drives

    unique_games <- length(unique(all_drives$event_id))
    message(sprintf("Combined play-by-play drives assigned to: nfl_pbp_drives (%d games, %d drives)",
                    unique_games, nrow(all_drives)))
  }

  if (return_type %in% c("plays", "all") && nrow(all_plays) > 0) {
    all_plays <- all_plays[!duplicated(paste(all_plays$event_id, all_plays$play_id)), ]
    assign("nfl_pbp_plays", all_plays, envir = .GlobalEnv)
    result_data$plays <- all_plays

    unique_games <- length(unique(all_plays$event_id))
    total_plays <- nrow(all_plays)
    message(sprintf("Combined play-by-play plays assigned to: nfl_pbp_plays (%d games, %d plays)",
                    unique_games, total_plays))
  }

  # Return appropriate data structure
  if (return_type == "all") {
    return(invisible(result_data))
  } else {
    return(invisible(result_data[[return_type]]))
  }
}
