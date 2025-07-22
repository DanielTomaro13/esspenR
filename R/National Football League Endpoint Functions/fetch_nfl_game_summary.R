#' Safe nested data extraction helper function for game summary
#'
#' Safely extracts values from nested list structures with error handling
#'
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_summary <- function(data, path, default = NA) {
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

#' Create game summary data frame from API response
#'
#' Processes raw JSON response from ESPN Site API into structured data frames
#' containing comprehensive game summary information
#'
#' @param data Raw JSON response from ESPN Site API game summary endpoint
#' @param event_id Character. Event ID used in request
#' @return List containing multiple data frames with game information
#' @keywords internal
create_game_summary_datasets <- function(data, event_id) {

  # Initialize game info data frame
  game_info_df <- data.frame(
    event_id = character(0),
    game_date = character(0),
    game_status = character(0),
    game_status_detail = character(0),
    season_year = character(0),
    season_type = character(0),
    week = character(0),
    neutral_site = character(0),
    attendance = character(0),
    venue_id = character(0),
    venue_name = character(0),
    venue_city = character(0),
    venue_state = character(0),
    venue_capacity = character(0),
    broadcast_network = character(0),
    stringsAsFactors = FALSE
  )

  # Initialize team info data frame
  team_info_df <- data.frame(
    event_id = character(0),
    team_id = character(0),
    team_name = character(0),
    team_display_name = character(0),
    team_abbreviation = character(0),
    team_color = character(0),
    team_alternate_color = character(0),
    team_logo = character(0),
    home_away = character(0),
    score = character(0),
    winner = character(0),
    stringsAsFactors = FALSE
  )

  # Initialize scoring plays data frame
  scoring_df <- data.frame(
    event_id = character(0),
    play_id = character(0),
    period = character(0),
    clock = character(0),
    team_id = character(0),
    team_abbreviation = character(0),
    play_type = character(0),
    play_description = character(0),
    score_value = character(0),
    home_score = character(0),
    away_score = character(0),
    stringsAsFactors = FALSE
  )

  # Extract basic game header information
  header <- extract_nested_summary(data, c("header"), list())

  if (length(header) > 0) {
    # Basic game info
    game_id <- extract_nested_summary(header, c("id"), event_id)

    # Season information
    season_year <- extract_nested_summary(header, c("season", "year"), NA_character_)
    season_type <- extract_nested_summary(header, c("season", "type"), NA_character_)
    week <- extract_nested_summary(header, c("week"), NA_character_)

    # Competition details
    competitions <- extract_nested_summary(header, c("competitions"), list())
    if (length(competitions) > 0) {
      competition <- competitions[[1]]

      game_date <- extract_nested_summary(competition, c("date"), NA_character_)
      neutral_site <- extract_nested_summary(competition, c("neutralSite"), NA_character_)

      # Status information
      status <- extract_nested_summary(competition, c("status"), list())
      game_status <- extract_nested_summary(status, c("type", "name"), NA_character_)
      game_status_detail <- extract_nested_summary(status, c("type", "detail"), NA_character_)

      # Broadcast information
      broadcasts <- extract_nested_summary(competition, c("broadcasts"), list())
      broadcast_network <- NA_character_
      if (length(broadcasts) > 0) {
        broadcast_network <- extract_nested_summary(broadcasts[[1]], c("media", "shortName"), NA_character_)
      }

      # Team information from competition
      competitors <- extract_nested_summary(competition, c("competitors"), list())
      for (competitor in competitors) {
        team <- extract_nested_summary(competitor, c("team"), list())
        team_id <- extract_nested_summary(team, c("id"), NA_character_)
        team_name <- extract_nested_summary(team, c("name"), NA_character_)
        team_display_name <- extract_nested_summary(team, c("displayName"), NA_character_)
        team_abbreviation <- extract_nested_summary(team, c("abbreviation"), NA_character_)
        team_color <- extract_nested_summary(team, c("color"), NA_character_)
        team_alternate_color <- extract_nested_summary(team, c("alternateColor"), NA_character_)
        team_logo <- extract_nested_summary(team, c("logo"), NA_character_)

        home_away <- extract_nested_summary(competitor, c("homeAway"), NA_character_)
        score <- extract_nested_summary(competitor, c("score"), NA_character_)
        winner <- extract_nested_summary(competitor, c("winner"), NA_character_)

        # Add team info row
        team_row <- data.frame(
          event_id = as.character(event_id),
          team_id = as.character(team_id),
          team_name = as.character(team_name),
          team_display_name = as.character(team_display_name),
          team_abbreviation = as.character(team_abbreviation),
          team_color = as.character(team_color),
          team_alternate_color = as.character(team_alternate_color),
          team_logo = as.character(team_logo),
          home_away = as.character(home_away),
          score = as.character(score),
          winner = as.character(winner),
          stringsAsFactors = FALSE
        )

        team_info_df <- rbind(team_info_df, team_row)
      }
    } else {
      # Default values if no competition data
      game_date <- neutral_site <- game_status <- game_status_detail <- broadcast_network <- NA_character_
    }

    # Extract venue and attendance from gameInfo section
    game_info <- extract_nested_summary(data, c("gameInfo"), list())
    venue <- extract_nested_summary(game_info, c("venue"), list())
    venue_id <- extract_nested_summary(venue, c("id"), NA_character_)
    venue_name <- extract_nested_summary(venue, c("fullName"), NA_character_)
    venue_address <- extract_nested_summary(venue, c("address"), list())
    venue_city <- extract_nested_summary(venue_address, c("city"), NA_character_)
    venue_state <- extract_nested_summary(venue_address, c("state"), NA_character_)
    venue_capacity <- extract_nested_summary(venue, c("capacity"), NA_character_)
    attendance <- extract_nested_summary(game_info, c("attendance"), NA_character_)

    # Add game info row
    game_row <- data.frame(
      event_id = as.character(event_id),
      game_date = as.character(game_date),
      game_status = as.character(game_status),
      game_status_detail = as.character(game_status_detail),
      season_year = as.character(season_year),
      season_type = as.character(season_type),
      week = as.character(week),
      neutral_site = as.character(neutral_site),
      attendance = as.character(attendance),
      venue_id = as.character(venue_id),
      venue_name = as.character(venue_name),
      venue_city = as.character(venue_city),
      venue_state = as.character(venue_state),
      venue_capacity = as.character(venue_capacity),
      broadcast_network = as.character(broadcast_network),
      stringsAsFactors = FALSE
    )

    game_info_df <- rbind(game_info_df, game_row)
  }


  # Extract scoring plays
  scoring_plays <- extract_nested_summary(data, c("scoringPlays"), list())
  for (play in scoring_plays) {
    play_id <- extract_nested_summary(play, c("id"), NA_character_)
    period <- extract_nested_summary(play, c("period", "number"), NA_character_)
    clock <- extract_nested_summary(play, c("clock", "displayValue"), NA_character_)

    team <- extract_nested_summary(play, c("team"), list())
    team_id <- extract_nested_summary(team, c("id"), NA_character_)
    team_abbreviation <- extract_nested_summary(team, c("abbreviation"), NA_character_)

    type_info <- extract_nested_summary(play, c("type"), list())
    play_type <- extract_nested_summary(type_info, c("text"), NA_character_)

    play_description <- extract_nested_summary(play, c("text"), NA_character_)

    home_score <- extract_nested_summary(play, c("homeScore"), NA_character_)
    away_score <- extract_nested_summary(play, c("awayScore"), NA_character_)

    # Calculate score value (points scored on this play)
    score_value <- NA_character_
    scoring_type <- extract_nested_summary(play, c("scoringType"), list())
    scoring_type_name <- extract_nested_summary(scoring_type, c("name"), "")

    if (scoring_type_name == "touchdown") {
      score_value <- "6"  # Touchdown is 6 points (plus potential extra point)
    } else if (scoring_type_name == "field-goal") {
      score_value <- "3"  # Field goal is 3 points
    } else if (grepl("safety", scoring_type_name, ignore.case = TRUE)) {
      score_value <- "2"  # Safety is 2 points
    }

    # Add scoring play row
    scoring_row <- data.frame(
      event_id = as.character(event_id),
      play_id = as.character(play_id),
      period = as.character(period),
      clock = as.character(clock),
      team_id = as.character(team_id),
      team_abbreviation = as.character(team_abbreviation),
      play_type = as.character(play_type),
      play_description = as.character(play_description),
      score_value = as.character(score_value),
      home_score = as.character(home_score),
      away_score = as.character(away_score),
      stringsAsFactors = FALSE
    )

    scoring_df <- rbind(scoring_df, scoring_row)
  }

  # Clean up row names
  if (nrow(game_info_df) > 0) rownames(game_info_df) <- NULL
  if (nrow(team_info_df) > 0) rownames(team_info_df) <- NULL
  if (nrow(scoring_df) > 0) rownames(scoring_df) <- NULL

  return(list(
    game_info = game_info_df,
    team_info = team_info_df,
    scoring_plays = scoring_df
  ))
}

#' Fetch NFL game summary data using Site API
#'
#' Retrieves comprehensive game summary information from ESPN's Site API.
#' The function fetches detailed game information including team details,
#' venue information, and scoring plays.
#'
#' @param event_id Character or Numeric. ESPN event ID (required).
#'   The unique identifier for the game in ESPN's database.
#' @param return_type Character. Type of data to return (default: "all").
#'   Options are:
#'   \itemize{
#'     \item "game_info" - Basic game information (date, venue, attendance, etc.)
#'     \item "team_info" - Team details and final scores
#'     \item "scoring_plays" - Chronological scoring information
#'     \item "all" - All data types combined
#'   }
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'nfl_game_summary_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data. The main purpose is global
#'   environment assignment based on return_type:
#'   \itemize{
#'     \item \code{nfl_game_info} - Game information data frame
#'     \item \code{nfl_team_info} - Team information data frame
#'     \item \code{nfl_scoring_plays} - Scoring plays data frame
#'     \item \code{nfl_game_summary_raw} - Raw JSON response (if raw = TRUE)
#'   }
#'
#' @details
#' The function creates structured data frames with comprehensive game information:
#'
#' **Game Information** (\code{nfl_game_info}):
#' \itemize{
#'   \item Basic details: event_id, game_date, game_status, season_year, week
#'   \item Venue details: venue_name, venue_city, venue_state, venue_capacity
#'   \item Context: neutral_site, attendance, broadcast_network
#' }
#'
#' **Team Information** (\code{nfl_team_info}):
#' \itemize{
#'   \item Team details: team_name, team_abbreviation, team_colors, team_logo
#'   \item Game context: home_away, score, winner
#' }
#'
#' **Scoring Plays** (\code{nfl_scoring_plays}):
#' \itemize{
#'   \item Chronological scoring events with play details
#'   \item Period, clock, play_type, description
#'   \item Score progression: home_score, away_score
#' }
#'
#' This comprehensive data is valuable for game analysis, historical research,
#' and detailed performance evaluation.
#'
#' @examples
#' \dontrun{
#' # Get complete game summary for a specific game
#' fetch_nfl_game_summary(event_id = "401671617")
#'
#' # Check what data was created
#' head(nfl_game_info)
#' head(nfl_team_info)
#' head(nfl_scoring_plays)
#'
#' # Get only team info
#' fetch_nfl_game_summary(event_id = "401671617", return_type = "team_info")
#'
#' # View game basic information
#' game_details <- nfl_game_info[c("game_date", "venue_name", "attendance")]
#' print(game_details)
#'
#' # Check final scores
#' final_scores <- nfl_team_info[c("team_abbreviation", "score", "winner")]
#' print(final_scores)
#'
#' # Review scoring progression
#' scoring_summary <- nfl_scoring_plays[c("period", "clock", "team_abbreviation",
#'                                        "play_type", "home_score", "away_score")]
#' print(scoring_summary)
#'
#' # Get raw data for debugging
#' fetch_nfl_game_summary(event_id = "401671617", raw = TRUE)
#' str(nfl_game_summary_raw, max.level = 2)
#' }
#'
#' @seealso \code{\link{fetch_multiple_nfl_game_summaries}} for fetching
#'   multiple games' data
#'
#' @export
fetch_nfl_game_summary <- function(event_id, return_type = "all", raw = FALSE) {
  # Input validation
  if (missing(event_id)) {
    stop("'event_id' is a required parameter")
  }

  valid_types <- c("game_info", "team_info", "scoring_plays", "all")
  if (!return_type %in% valid_types) {
    stop(sprintf("'return_type' must be one of: %s", paste(valid_types, collapse = ", ")))
  }

  # Convert event_id to character for URL building
  event_id <- as.character(event_id)

  # Build API URL for Site API
  url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/football/nfl/summary?event=%s",
                 event_id)

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
      assign("nfl_game_summary_raw", data, envir = .GlobalEnv)
      message("Raw NFL game summary data assigned to: nfl_game_summary_raw")
      message("Data structure preview:")

      # Show available sections
      sections <- names(data)
      message("- Available sections: ", paste(sections, collapse = ", "))

      # Show game info if available
      header <- extract_nested_summary(data, c("header"), list())
      if (length(header) > 0) {
        competitions <- extract_nested_summary(header, c("competitions"), list())
        if (length(competitions) > 0) {
          competitors <- extract_nested_summary(competitions[[1]], c("competitors"), list())
          message("- Teams in game: ", length(competitors))
        }
      }

      # Show box score info
      # Note: Box score functionality removed - data structure varies

      # Show scoring plays
      scoring_plays <- extract_nested_summary(data, c("scoringPlays"), list())
      message("- Scoring plays: ", length(scoring_plays))

      return(invisible(data))
    }

    # Create datasets
    datasets <- create_game_summary_datasets(data, event_id)

    # Assign data based on return_type
    result_data <- list()

    if (return_type %in% c("game_info", "all")) {
      assign("nfl_game_info", datasets$game_info, envir = .GlobalEnv)
      result_data$game_info <- datasets$game_info
      message(sprintf("Game info data assigned to: nfl_game_info (%d records)", nrow(datasets$game_info)))
    }

    if (return_type %in% c("team_info", "all")) {
      assign("nfl_team_info", datasets$team_info, envir = .GlobalEnv)
      result_data$team_info <- datasets$team_info
      message(sprintf("Team info data assigned to: nfl_team_info (%d teams)", nrow(datasets$team_info)))
    }

    if (return_type %in% c("scoring_plays", "all")) {
      assign("nfl_scoring_plays", datasets$scoring_plays, envir = .GlobalEnv)
      result_data$scoring_plays <- datasets$scoring_plays
      message(sprintf("Scoring plays data assigned to: nfl_scoring_plays (%d plays)", nrow(datasets$scoring_plays)))
    }

    # Return appropriate data structure
    if (return_type == "all") {
      return(invisible(result_data))
    } else {
      return(invisible(result_data[[return_type]]))
    }

  }, error = function(e) {
    stop(sprintf("Failed to fetch NFL game summary for event %s: %s",
                 event_id, e$message))
  })
}

#' Fetch multiple NFL game summaries
#'
#' Retrieves game summary data for multiple NFL games with rate limiting to
#' be respectful to ESPN's Site API. This function calls \code{\link{fetch_nfl_game_summary}}
#' for each game and combines the results.
#'
#' @param event_ids Character or Numeric vector. ESPN event IDs.
#'   Vector of unique identifiers for games in ESPN's database.
#' @param return_type Character. Type of data to return (default: "all").
#'   Same options as \code{\link{fetch_nfl_game_summary}}.
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
#' This is particularly useful for analyzing multiple games from a week,
#' comparing team performance across games, or building historical datasets.
#'
#' @examples
#' \dontrun{
#' # Get summaries for multiple games from same week
#' week1_games <- c("401671617", "401671618", "401671619")
#' fetch_multiple_nfl_game_summaries(week1_games)
#'
#' # Get only box scores for multiple games
#' fetch_multiple_nfl_game_summaries(week1_games, return_type = "all")
#'
#' # Analyze combined results
#' unique_games <- unique(nfl_game_info$event_id)
#' cat("Retrieved summaries for", length(unique_games), "games\n")
#' }
#'
#' @seealso \code{\link{fetch_nfl_game_summary}} for single game data
#'
#' @export
fetch_multiple_nfl_game_summaries <- function(event_ids, return_type = "all",
                                              delay = 0.3, raw = FALSE) {
  # Input validation
  if (length(event_ids) == 0) {
    stop("'event_ids' must contain at least one event ID")
  }

  valid_types <- c("game_info", "team_info", "scoring_plays", "all")
  if (!return_type %in% valid_types) {
    stop(sprintf("'return_type' must be one of: %s", paste(valid_types, collapse = ", ")))
  }

  # Initialize combined data containers
  all_game_info <- data.frame()
  all_team_info <- data.frame()
  all_scoring_plays <- data.frame()

  message(sprintf("Starting to fetch game summary data for %d games...", length(event_ids)))

  # Process each game sequentially
  for (i in seq_along(event_ids)) {
    event_id <- event_ids[i]
    message(sprintf("Fetching game summary for event %s (%d/%d)...", event_id, i, length(event_ids)))

    tryCatch({
      # Fetch individual game data
      game_data <- fetch_nfl_game_summary(
        event_id = event_id,
        return_type = return_type,
        raw = raw
      )

      # If raw data requested, return after first game
      if (isTRUE(raw)) {
        return(invisible(game_data))
      }

      # Combine data based on return type
      if (return_type %in% c("game_info", "all")) {
        game_info_df <- get("nfl_game_info", envir = .GlobalEnv)
        all_game_info <- rbind(all_game_info, game_info_df)
      }

      if (return_type %in% c("team_info", "all")) {
        team_info_df <- get("nfl_team_info", envir = .GlobalEnv)
        all_team_info <- rbind(all_team_info, team_info_df)
      }

      if (return_type %in% c("scoring_plays", "all")) {
        scoring_plays_df <- get("nfl_scoring_plays", envir = .GlobalEnv)
        all_scoring_plays <- rbind(all_scoring_plays, scoring_plays_df)
      }

    }, error = function(e) {
      message(sprintf("Failed to fetch game summary for event %s: %s", event_id, e$message))
    })

    # Be respectful to the API with delay between requests
    if (i < length(event_ids)) {
      Sys.sleep(delay)
    }
  }

  # Remove duplicates and assign combined datasets to global environment
  result_data <- list()

  if (return_type %in% c("game_info", "all") && nrow(all_game_info) > 0) {
    all_game_info <- all_game_info[!duplicated(all_game_info$event_id), ]
    assign("nfl_game_info", all_game_info, envir = .GlobalEnv)
    result_data$game_info <- all_game_info

    message(sprintf("Combined game info data assigned to: nfl_game_info (%d games)", nrow(all_game_info)))
  }

  if (return_type %in% c("team_info", "all") && nrow(all_team_info) > 0) {
    all_team_info <- all_team_info[!duplicated(paste(all_team_info$event_id, all_team_info$team_id)), ]
    assign("nfl_team_info", all_team_info, envir = .GlobalEnv)
    result_data$team_info <- all_team_info

    unique_games <- length(unique(all_team_info$event_id))
    message(sprintf("Combined team info data assigned to: nfl_team_info (%d games, %d team records)",
                    unique_games, nrow(all_team_info)))
  }

  if (return_type %in% c("scoring_plays", "all") && nrow(all_scoring_plays) > 0) {
    all_scoring_plays <- all_scoring_plays[!duplicated(paste(all_scoring_plays$event_id,
                                                             all_scoring_plays$play_id)), ]
    assign("nfl_scoring_plays", all_scoring_plays, envir = .GlobalEnv)
    result_data$scoring_plays <- all_scoring_plays

    unique_games <- length(unique(all_scoring_plays$event_id))
    total_plays <- nrow(all_scoring_plays)
    message(sprintf("Combined scoring plays data assigned to: nfl_scoring_plays (%d games, %d plays)",
                    unique_games, total_plays))
  }

  # Return appropriate data structure
  if (return_type == "all") {
    return(invisible(result_data))
  } else {
    return(invisible(result_data[[return_type]]))
  }
}
