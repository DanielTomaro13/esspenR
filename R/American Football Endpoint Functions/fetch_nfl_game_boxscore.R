#' Safe nested data extraction helper function for boxscore
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_boxscore <- function(data, path, default = NA) {
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

#' Create boxscore data frames from API response
#'
#' Processes raw JSON response from ESPN CDN API into structured data frames
#' containing detailed boxscore information
#'
#' @param data Raw JSON response from ESPN CDN API boxscore endpoint
#' @param event_id Character. Event ID used in request
#' @return List containing multiple data frames with boxscore information
#' @keywords internal
create_boxscore_datasets <- function(data, event_id) {

  # Initialize team stats data frame
  team_stats_df <- data.frame(
    event_id = character(0),
    team_id = character(0),
    team_name = character(0),
    team_abbreviation = character(0),
    home_away = character(0),
    stat_category = character(0),
    stat_name = character(0),
    stat_display_name = character(0),
    stat_value = character(0),
    stat_display_value = character(0),
    stringsAsFactors = FALSE
  )

  # Initialize player stats data frame
  player_stats_df <- data.frame(
    event_id = character(0),
    team_id = character(0),
    team_abbreviation = character(0),
    player_id = character(0),
    player_name = character(0),
    player_display_name = character(0),
    player_position = character(0),
    player_jersey = character(0),
    stat_category = character(0),
    stat_name = character(0),
    stat_display_name = character(0),
    stat_value = character(0),
    stat_display_value = character(0),
    stringsAsFactors = FALSE
  )

  # Initialize game info data frame
  game_info_df <- data.frame(
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
    stringsAsFactors = FALSE
  )

  # Extract game header information from __gamepackage__
  home_team_id <- extract_nested_boxscore(data, c("__gamepackage__", "homeTeam", "id"), NA_character_)
  away_team_id <- extract_nested_boxscore(data, c("__gamepackage__", "awayTeam", "id"), NA_character_)
  home_team_name <- extract_nested_boxscore(data, c("__gamepackage__", "homeTeam", "team", "displayName"), NA_character_)
  away_team_name <- extract_nested_boxscore(data, c("__gamepackage__", "awayTeam", "team", "displayName"), NA_character_)
  home_team_score <- extract_nested_boxscore(data, c("__gamepackage__", "homeTeam", "score"), NA_character_)
  away_team_score <- extract_nested_boxscore(data, c("__gamepackage__", "awayTeam", "score"), NA_character_)

  # Extract additional game info from header if available
  game_info <- extract_nested_boxscore(data, c("gamepackageJSON", "header"), list())
  game_date <- extract_nested_boxscore(game_info, c("competitions", 1, "date"), NA_character_)
  season_info <- extract_nested_boxscore(game_info, c("season"), list())
  season_year <- extract_nested_boxscore(season_info, c("year"), "2024")
  season_type <- extract_nested_boxscore(season_info, c("type"), "2")
  week <- extract_nested_boxscore(game_info, c("week"), "1")

  # Add game info row
  game_row <- data.frame(
    event_id = as.character(event_id),
    game_date = as.character(game_date),
    game_status = "Final",
    season_year = as.character(season_year),
    season_type = as.character(season_type),
    week = as.character(week),
    home_team_id = as.character(home_team_id),
    home_team_name = as.character(home_team_name),
    home_team_score = as.character(home_team_score),
    away_team_id = as.character(away_team_id),
    away_team_name = as.character(away_team_name),
    away_team_score = as.character(away_team_score),
    stringsAsFactors = FALSE
  )

  game_info_df <- rbind(game_info_df, game_row)

  # Extract boxscore data - try multiple possible paths
  boxscore_data <- extract_nested_boxscore(data, c("gamepackageJSON", "boxscore"),
                                           extract_nested_boxscore(data, c("boxscore"), list()))

  if (length(boxscore_data) == 0) {
    return(list(
      game_info = game_info_df,
      team_stats = team_stats_df,
      player_stats = player_stats_df
    ))
  }

  # Extract team statistics
  teams <- extract_nested_boxscore(boxscore_data, c("teams"), list())

  for (team_data in teams) {
    # Extract team identification
    team_info <- extract_nested_boxscore(team_data, c("team"), list())
    team_id <- extract_nested_boxscore(team_info, c("id"), NA_character_)
    team_name <- extract_nested_boxscore(team_info, c("displayName"), NA_character_)
    team_abbreviation <- extract_nested_boxscore(team_info, c("abbreviation"), NA_character_)
    home_away <- extract_nested_boxscore(team_data, c("homeAway"), NA_character_)

    # Extract team statistics if available
    statistics <- extract_nested_boxscore(team_data, c("statistics"), list())

    for (stat in statistics) {
      stat_name <- extract_nested_boxscore(stat, c("name"), NA_character_)
      stat_label <- extract_nested_boxscore(stat, c("label"), stat_name)
      stat_value <- extract_nested_boxscore(stat, c("value"), NA_character_)
      stat_display_value <- extract_nested_boxscore(stat, c("displayValue"), stat_value)

      # Add team stats row
      team_stat_row <- data.frame(
        event_id = as.character(event_id),
        team_id = as.character(team_id),
        team_name = as.character(team_name),
        team_abbreviation = as.character(team_abbreviation),
        home_away = as.character(home_away),
        stat_category = "team_stats",
        stat_name = as.character(stat_name),
        stat_display_name = as.character(stat_label),
        stat_value = as.character(stat_value),
        stat_display_value = as.character(stat_display_value),
        stringsAsFactors = FALSE
      )

      team_stats_df <- rbind(team_stats_df, team_stat_row)
    }
  }

  # Extract player statistics
  players_sections <- extract_nested_boxscore(boxscore_data, c("players"), list())

  for (player_section in players_sections) {
    # Get team info for this section
    team_info <- extract_nested_boxscore(player_section, c("team"), list())
    team_id <- extract_nested_boxscore(team_info, c("id"), NA_character_)
    team_abbreviation <- extract_nested_boxscore(team_info, c("abbreviation"), NA_character_)

    # Process statistics categories
    stat_categories <- extract_nested_boxscore(player_section, c("statistics"), list())

    for (stat_category in stat_categories) {
      category_name <- extract_nested_boxscore(stat_category, c("name"), "Unknown")

      # Get stat definitions for this category
      labels <- extract_nested_boxscore(stat_category, c("keys"), list())
      descriptions <- extract_nested_boxscore(stat_category, c("descriptions"), list())

      # Extract individual players
      athletes <- extract_nested_boxscore(stat_category, c("athletes"), list())

      for (athlete_data in athletes) {
        athlete_info <- extract_nested_boxscore(athlete_data, c("athlete"), list())
        player_id <- extract_nested_boxscore(athlete_info, c("id"), NA_character_)
        player_name <- extract_nested_boxscore(athlete_info, c("displayName"), NA_character_)
        player_jersey <- extract_nested_boxscore(athlete_info, c("jersey"), NA_character_)

        # Extract player stats
        stats <- extract_nested_boxscore(athlete_data, c("stats"), list())

        if (length(stats) > 0 && length(labels) > 0) {
          # Create rows for each stat
          for (j in seq_along(stats)) {
            if (j <= length(labels)) {
              stat_label <- labels[[j]]
              stat_description <- if (j <= length(descriptions)) descriptions[[j]] else stat_label
              stat_value <- stats[[j]]

              # Add player stats row
              player_stat_row <- data.frame(
                event_id = as.character(event_id),
                team_id = as.character(team_id),
                team_abbreviation = as.character(team_abbreviation),
                player_id = as.character(player_id),
                player_name = as.character(player_name),
                player_display_name = as.character(player_name),
                player_position = NA_character_,
                player_jersey = as.character(player_jersey),
                stat_category = as.character(category_name),
                stat_name = as.character(stat_label),
                stat_display_name = as.character(stat_description),
                stat_value = as.character(stat_value),
                stat_display_value = as.character(stat_value),
                stringsAsFactors = FALSE
              )

              player_stats_df <- rbind(player_stats_df, player_stat_row)
            }
          }
        }
      }
    }
  }

  # Clean up row names
  if (nrow(game_info_df) > 0) rownames(game_info_df) <- NULL
  if (nrow(team_stats_df) > 0) rownames(team_stats_df) <- NULL
  if (nrow(player_stats_df) > 0) rownames(player_stats_df) <- NULL

  return(list(
    game_info = game_info_df,
    team_stats = team_stats_df,
    player_stats = player_stats_df
  ))
}

#' Fetch NFL game boxscore data using CDN API
#'
#' Retrieves detailed boxscore information from ESPN's CDN API.
#' The function fetches comprehensive team and player statistics
#' for a specific NFL game.
#'
#' @param event_id Character or Numeric. ESPN event ID (required).
#'   The unique identifier for the game in ESPN's database.
#' @param return_type Character. Type of data to return (default: "all").
#'   Options are:
#'   \itemize{
#'     \item "game_info" - Basic game information
#'     \item "team_stats" - Team-level statistics
#'     \item "player_stats" - Individual player statistics
#'     \item "all" - All data types combined
#'   }
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'nfl_boxscore_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data. The main purpose is global
#'   environment assignment based on return_type:
#'   \itemize{
#'     \item \code{nfl_boxscore_game_info} - Game information data frame
#'     \item \code{nfl_boxscore_team_stats} - Team statistics data frame
#'     \item \code{nfl_boxscore_player_stats} - Player statistics data frame
#'     \item \code{nfl_boxscore_raw} - Raw JSON response (if raw = TRUE)
#'   }
#'
#' @details
#' The function creates structured data frames with comprehensive boxscore information:
#'
#' **Game Information** (\code{nfl_boxscore_game_info}):
#' \itemize{
#'   \item Game details: event_id, game_date, game_status, season info
#'   \item Team matchup: home_team vs away_team with final scores
#' }
#'
#' **Team Statistics** (\code{nfl_boxscore_team_stats}):
#' \itemize{
#'   \item Team-level statistics by category (passing, rushing, defense, etc.)
#'   \item Both home and away team stats in same data frame
#'   \item Individual stat names, values, and display values
#' }
#'
#' **Player Statistics** (\code{nfl_boxscore_player_stats}):
#' \itemize{
#'   \item Individual player performance by statistical category
#'   \item Player identification: name, position, jersey number
#'   \item Detailed statistics for passing, rushing, receiving, defense, etc.
#' }
#'
#' This provides the most detailed statistical breakdown available,
#' including individual player performance data that's valuable for
#' fantasy sports, player analysis, and comprehensive game evaluation.
#'
#' @examples
#' \dontrun{
#' # Get complete boxscore for a specific game
#' fetch_nfl_game_boxscore(event_id = "401671617")
#'
#' # Check what data was created
#' head(nfl_boxscore_game_info)
#' head(nfl_boxscore_team_stats)
#' head(nfl_boxscore_player_stats)
#'
#' # Get only player statistics
#' fetch_nfl_game_boxscore(event_id = "401671617", return_type = "player_stats")
#'
#' # Analyze team statistics
#' team_passing <- nfl_boxscore_team_stats[
#'   nfl_boxscore_team_stats$stat_category == "team_stats",
#'   c("team_abbreviation", "stat_name", "stat_value")
#' ]
#' print(team_passing)
#'
#' # View quarterback performance
#' qb_stats <- nfl_boxscore_player_stats[
#'   nfl_boxscore_player_stats$stat_category == "passing",
#'   c("player_name", "stat_name", "stat_value")
#' ]
#' print(qb_stats)
#'
#' # Check available statistical categories
#' unique(nfl_boxscore_team_stats$stat_category)
#' unique(nfl_boxscore_player_stats$stat_category)
#'
#' # Get raw data for debugging
#' fetch_nfl_game_boxscore(event_id = "401671617", raw = TRUE)
#' str(nfl_boxscore_raw, max.level = 2)
#' }
#'
#' @seealso \code{\link{fetch_multiple_nfl_game_boxscores}} for fetching
#'   multiple games' data
#'
#' @export
fetch_nfl_game_boxscore <- function(event_id, return_type = "all", raw = FALSE) {

  # Input validation
  if (missing(event_id)) {
    stop("'event_id' is a required parameter")
  }

  valid_types <- c("game_info", "team_stats", "player_stats", "all")
  if (!return_type %in% valid_types) {
    stop(sprintf("'return_type' must be one of: %s", paste(valid_types, collapse = ", ")))
  }

  # Convert event_id to character for URL building
  event_id <- as.character(event_id)

  # Build API URL for CDN API
  url <- sprintf("https://cdn.espn.com/core/nfl/boxscore?xhr=1&gameId=%s", event_id)

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
      assign("nfl_boxscore_raw", data, envir = .GlobalEnv)
      message("Raw NFL boxscore data assigned to: nfl_boxscore_raw")
      message("Data structure preview:")

      # Show available sections
      sections <- names(data)
      message("- Available sections: ", paste(sections, collapse = ", "))

      # Show gamepackageJSON structure if available
      gamepackage <- extract_nested_boxscore(data, c("gamepackageJSON"), list())
      if (length(gamepackage) > 0) {
        gp_sections <- names(gamepackage)
        message("- GamepackageJSON sections: ", paste(gp_sections, collapse = ", "))

        # Show boxscore info
        boxscore <- extract_nested_boxscore(gamepackage, c("boxscore"), list())
        if (length(boxscore) > 0) {
          teams <- extract_nested_boxscore(boxscore, c("teams"), list())
          message("- Boxscore teams: ", length(teams))

          if (length(teams) > 0) {
            players <- extract_nested_boxscore(teams[[1]], c("players"), list())
            message("- Player categories for first team: ", length(players))
          }
        }
      }

      return(invisible(data))
    }

    # Create datasets
    datasets <- create_boxscore_datasets(data, event_id)

    # Assign data based on return_type
    result_data <- list()

    if (return_type %in% c("game_info", "all")) {
      assign("nfl_boxscore_game_info", datasets$game_info, envir = .GlobalEnv)
      result_data$game_info <- datasets$game_info
      message(sprintf("Boxscore game info assigned to: nfl_boxscore_game_info (%d records)", nrow(datasets$game_info)))
    }

    if (return_type %in% c("team_stats", "all")) {
      assign("nfl_boxscore_team_stats", datasets$team_stats, envir = .GlobalEnv)
      result_data$team_stats <- datasets$team_stats

      stat_categories <- length(unique(datasets$team_stats$stat_category[!is.na(datasets$team_stats$stat_category)]))
      message(sprintf("Boxscore team stats assigned to: nfl_boxscore_team_stats (%d statistics, %d categories)",
                      nrow(datasets$team_stats), stat_categories))
    }

    if (return_type %in% c("player_stats", "all")) {
      assign("nfl_boxscore_player_stats", datasets$player_stats, envir = .GlobalEnv)
      result_data$player_stats <- datasets$player_stats

      unique_players <- length(unique(datasets$player_stats$player_id[!is.na(datasets$player_stats$player_id)]))
      player_categories <- length(unique(datasets$player_stats$stat_category[!is.na(datasets$player_stats$stat_category)]))
      message(sprintf("Boxscore player stats assigned to: nfl_boxscore_player_stats (%d players, %d categories, %d total records)",
                      unique_players, player_categories, nrow(datasets$player_stats)))
    }

    # Return appropriate data structure
    if (return_type == "all") {
      return(invisible(result_data))
    } else {
      return(invisible(result_data[[return_type]]))
    }

  }, error = function(e) {
    stop(sprintf("Failed to fetch NFL boxscore for event %s: %s",
                 event_id, e$message))
  })
}

#' Fetch multiple NFL game boxscores
#'
#' Retrieves boxscore data for multiple NFL games with rate limiting to
#' be respectful to ESPN's CDN API. This function calls \code{\link{fetch_nfl_game_boxscore}}
#' for each game and combines the results.
#'
#' @param event_ids Character or Numeric vector. ESPN event IDs.
#'   Vector of unique identifiers for games in ESPN's database.
#' @param return_type Character. Type of data to return (default: "all").
#'   Same options as \code{\link{fetch_nfl_game_boxscore}}.
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
#' This is particularly useful for building comprehensive statistical databases,
#' comparing player performance across multiple games, or analyzing team
#' performance trends over time.
#'
#' @examples
#' \dontrun{
#' # Get boxscores for multiple games from same week
#' week1_games <- c("401671617", "401671618", "401671619")
#' fetch_multiple_nfl_game_boxscores(week1_games)
#'
#' # Get only player stats for multiple games
#' fetch_multiple_nfl_game_boxscores(week1_games, return_type = "player_stats")
#'
#' # Use longer delay for larger requests
#' fetch_multiple_nfl_game_boxscores(week1_games, delay = 0.5)
#'
#' # Analyze combined results
#' unique_games <- unique(nfl_boxscore_game_info$event_id)
#' cat("Retrieved boxscores for", length(unique_games), "games\n")
#'
#' # Compare quarterback performance across multiple games
#' qb_comparison <- nfl_boxscore_player_stats[
#'   nfl_boxscore_player_stats$stat_category == "passing" &
#'   nfl_boxscore_player_stats$stat_name == "passingYards",
#'   c("event_id", "player_name", "stat_value")
#' ]
#' }
#'
#' @seealso \code{\link{fetch_nfl_game_boxscore}} for single game data
#' @export
fetch_multiple_nfl_game_boxscores <- function(event_ids, return_type = "all",
                                              delay = 0.2, raw = FALSE) {
  # Input validation
  if (length(event_ids) == 0) {
    stop("'event_ids' must contain at least one event ID")
  }

  valid_types <- c("game_info", "team_stats", "player_stats", "all")
  if (!return_type %in% valid_types) {
    stop(sprintf("'return_type' must be one of: %s", paste(valid_types, collapse = ", ")))
  }

  # Initialize combined data containers
  all_game_info <- data.frame()
  all_team_stats <- data.frame()
  all_player_stats <- data.frame()

  message(sprintf("Starting to fetch boxscore data for %d games...", length(event_ids)))

  # Process each game sequentially
  for (i in seq_along(event_ids)) {
    event_id <- event_ids[i]
    message(sprintf("Fetching boxscore for event %s (%d/%d)...", event_id, i, length(event_ids)))

    tryCatch({
      # Fetch individual game data
      game_data <- fetch_nfl_game_boxscore(
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
        game_info_df <- get("nfl_boxscore_game_info", envir = .GlobalEnv)
        all_game_info <- rbind(all_game_info, game_info_df)
      }

      if (return_type %in% c("team_stats", "all")) {
        team_stats_df <- get("nfl_boxscore_team_stats", envir = .GlobalEnv)
        all_team_stats <- rbind(all_team_stats, team_stats_df)
      }

      if (return_type %in% c("player_stats", "all")) {
        player_stats_df <- get("nfl_boxscore_player_stats", envir = .GlobalEnv)
        all_player_stats <- rbind(all_player_stats, player_stats_df)
      }

    }, error = function(e) {
      message(sprintf("Failed to fetch boxscore for event %s: %s", event_id, e$message))
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
    assign("nfl_boxscore_game_info", all_game_info, envir = .GlobalEnv)
    result_data$game_info <- all_game_info

    message(sprintf("Combined boxscore game info assigned to: nfl_boxscore_game_info (%d games)", nrow(all_game_info)))
  }

  if (return_type %in% c("team_stats", "all") && nrow(all_team_stats) > 0) {
    all_team_stats <- all_team_stats[!duplicated(paste(all_team_stats$event_id,
                                                       all_team_stats$team_id,
                                                       all_team_stats$stat_name)), ]
    assign("nfl_boxscore_team_stats", all_team_stats, envir = .GlobalEnv)
    result_data$team_stats <- all_team_stats

    unique_games <- length(unique(all_team_stats$event_id))
    stat_categories <- length(unique(all_team_stats$stat_category[!is.na(all_team_stats$stat_category)]))
    message(sprintf("Combined boxscore team stats assigned to: nfl_boxscore_team_stats (%d games, %d statistics, %d categories)",
                    unique_games, nrow(all_team_stats), stat_categories))
  }

  if (return_type %in% c("player_stats", "all") && nrow(all_player_stats) > 0) {
    all_player_stats <- all_player_stats[!duplicated(paste(all_player_stats$event_id,
                                                           all_player_stats$player_id,
                                                           all_player_stats$stat_name)), ]
    assign("nfl_boxscore_player_stats", all_player_stats, envir = .GlobalEnv)
    result_data$player_stats <- all_player_stats

    unique_games <- length(unique(all_player_stats$event_id))
    unique_players <- length(unique(all_player_stats$player_id[!is.na(all_player_stats$player_id)]))
    player_categories <- length(unique(all_player_stats$stat_category[!is.na(all_player_stats$stat_category)]))
    message(sprintf("Combined boxscore player stats assigned to: nfl_boxscore_player_stats (%d games, %d players, %d categories, %d total records)",
                    unique_games, unique_players, player_categories, nrow(all_player_stats)))
  }

  # Return appropriate data structure
  if (return_type == "all") {
    return(invisible(result_data))
  } else {
    return(invisible(result_data[[return_type]]))
  }
}
