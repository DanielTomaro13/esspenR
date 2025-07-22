#' Safe nested data extraction helper function for NHL player statistics
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_nhl_player_stats <- function(data, path, default = NA) {
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

#' Get NHL player information from ESPN player profile API
#'
#' Since the gamelog API doesn't include athlete info, fetch it separately
#' @param player_id ESPN player ID
#' @return List with player information
#' @keywords internal
fetch_nhl_player_info <- function(player_id) {
  # Try the player profile endpoint
  url <- paste0("https://site.web.api.espn.com/apis/common/v3/sports/hockey/nhl/athletes/", player_id)

  tryCatch({
    resp <- httr::GET(url, httr::timeout(10))
    if (httr::status_code(resp) == 200) {
      player_data <- httr::content(resp, as = "parsed", simplifyVector = TRUE)

      # Extract player information
      athlete <- extract_nested_nhl_player_stats(player_data, c("athlete"), list())
      if (length(athlete) > 0) {
        return(list(
          name = extract_nested_nhl_player_stats(athlete, c("name"), NA_character_),
          display_name = extract_nested_nhl_player_stats(athlete, c("displayName"), NA_character_),
          jersey = extract_nested_nhl_player_stats(athlete, c("jersey"), NA_character_),
          position = extract_nested_nhl_player_stats(athlete, c("position", "abbreviation"), NA_character_),
          team_id = extract_nested_nhl_player_stats(athlete, c("team", "id"), NA_character_),
          team_abbreviation = extract_nested_nhl_player_stats(athlete, c("team", "abbreviation"), NA_character_),
          team_display_name = extract_nested_nhl_player_stats(athlete, c("team", "displayName"), NA_character_),
          shoots = extract_nested_nhl_player_stats(athlete, c("shoots", "abbreviation"), NA_character_),
          catches = extract_nested_nhl_player_stats(athlete, c("catches", "abbreviation"), NA_character_)
        ))
      }
    }
  }, error = function(e) {
    message("Could not fetch player info: ", e$message)
  })

  # Return default values if fetch failed
  return(list(
    name = paste("Player", player_id),
    display_name = paste("Player", player_id),
    jersey = NA_character_,
    position = NA_character_,
    team_id = NA_character_,
    team_abbreviation = NA_character_,
    team_display_name = NA_character_,
    shoots = NA_character_,
    catches = NA_character_
  ))
}

#' Create enhanced NHL player game log with detailed statistics from ESPN API
#'
#' Processes game-by-game data with complete statistics from the events structure
#' @param data Raw JSON response from ESPN API gamelog endpoint
#' @param player_id Player ID used in the request
#' @param player_info Player information from separate API call
#' @return Data frame with detailed game-by-game statistics
#' @keywords internal
create_enhanced_nhl_player_gamelog_dataset <- function(data, player_id, player_info) {
  # Initialize enhanced game log data frame with all possible statistics
  gamelog_df <- data.frame(
    player_id = character(0),
    player_name = character(0),
    player_display_name = character(0),
    player_jersey = character(0),
    player_position = character(0),
    player_shoots = character(0),
    player_catches = character(0),
    team_id = character(0),
    team_abbreviation = character(0),
    team_display_name = character(0),
    season_display_name = character(0),
    season_type = character(0),
    category_display_name = character(0),
    game_id = character(0),
    game_date = character(0),
    opponent_id = character(0),
    opponent_abbreviation = character(0),
    opponent_display_name = character(0),
    opponent_logo = character(0),
    at_vs = character(0),
    score = character(0),
    result = character(0),
    home_team_score = character(0),
    away_team_score = character(0),
    league_name = character(0),
    league_abbreviation = character(0),
    # Skater statistics (14+ fields from ESPN structure)
    goals = character(0),                    # G
    assists = character(0),                  # A
    points = character(0),                   # PTS
    plus_minus = character(0),               # +/-
    penalty_minutes = character(0),          # PIM
    shots_on_goal = character(0),            # SOG
    power_play_goals = character(0),         # PPG
    power_play_assists = character(0),       # PPA
    short_handed_goals = character(0),       # SHG
    short_handed_assists = character(0),     # SHA
    game_winning_goals = character(0),       # GWG
    overtime_goals = character(0),           # OTG
    shooting_percentage = character(0),      # S%
    faceoffs_won = character(0),             # FOW
    faceoffs_lost = character(0),            # FOL (if available)
    faceoff_percentage = character(0),       # FO% (if available)
    stringsAsFactors = FALSE
  )

  # Get stat labels and names from the data structure
  stat_labels <- extract_nested_nhl_player_stats(data, c("labels"), NULL)
  stat_names <- extract_nested_nhl_player_stats(data, c("names"), NULL)
  stat_display_names <- extract_nested_nhl_player_stats(data, c("displayNames"), NULL)

  # Create mapping of stats (ESPN provides: G, A, PTS, +/-, PIM, SOG, PPG, PPA, SHG, SHA, GWG, OTG, S%, FOW)
  if (!is.null(stat_labels) && length(stat_labels) >= 14) {
    message("Found ", length(stat_labels), " statistical categories: ", paste(stat_labels, collapse = ", "))
  }

  # Process season types
  season_types <- extract_nested_nhl_player_stats(data, c("seasonTypes"), NULL)
  if (is.null(season_types) || !is.data.frame(season_types)) {
    message("No season types found")
    return(gamelog_df)
  }

  # Process each season type
  for (season_idx in 1:nrow(season_types)) {
    season_display_name <- season_types$displayName[season_idx]
    season_team <- season_types$displayTeam[season_idx]

    # Determine season type
    season_type <- ifelse(grepl("Postseason|Playoff", season_display_name, ignore.case = TRUE), "playoffs",
                          ifelse(grepl("Preseason", season_display_name, ignore.case = TRUE), "preseason", "regular"))

    # Get categories for this season
    if ("categories" %in% names(season_types) && length(season_types$categories) >= season_idx) {
      categories <- season_types$categories[[season_idx]]

      if (!is.null(categories) && is.data.frame(categories) && nrow(categories) > 0) {
        # Process each category (Monthly breakdowns, etc.)
        for (cat_idx in 1:nrow(categories)) {
          category_display_name <- categories$displayName[cat_idx]
          category_type <- categories$type[cat_idx]

          # Get events for this category
          if ("events" %in% names(categories) && length(categories$events) >= cat_idx) {
            events_for_category <- categories$events[[cat_idx]]

            if (!is.null(events_for_category) && is.data.frame(events_for_category) && nrow(events_for_category) > 0) {
              # Process each game event
              for (event_idx in 1:nrow(events_for_category)) {
                event_id <- events_for_category$eventId[event_idx]

                # Get game statistics
                if ("stats" %in% names(events_for_category) && length(events_for_category$stats) >= event_idx) {
                  game_stats <- events_for_category$stats[[event_idx]]

                  # Get detailed event information from the main events structure
                  events_data <- extract_nested_nhl_player_stats(data, c("events"), NULL)
                  event_details <- NULL

                  if (!is.null(events_data) && event_id %in% names(events_data)) {
                    event_details <- events_data[[event_id]]
                  }

                  # Extract game details
                  game_date <- NA_character_
                  opponent_id <- NA_character_
                  opponent_abbreviation <- NA_character_
                  opponent_display_name <- NA_character_
                  opponent_logo <- NA_character_
                  at_vs <- NA_character_
                  score <- NA_character_
                  result <- NA_character_
                  home_team_score <- NA_character_
                  away_team_score <- NA_character_
                  league_name <- NA_character_
                  league_abbreviation <- NA_character_

                  if (!is.null(event_details)) {
                    game_date <- extract_nested_nhl_player_stats(event_details, c("gameDate"), NA_character_)
                    at_vs <- extract_nested_nhl_player_stats(event_details, c("atVs"), NA_character_)
                    score <- extract_nested_nhl_player_stats(event_details, c("score"), NA_character_)
                    result <- extract_nested_nhl_player_stats(event_details, c("gameResult"), NA_character_)
                    home_team_score <- extract_nested_nhl_player_stats(event_details, c("homeTeamScore"), NA_character_)
                    away_team_score <- extract_nested_nhl_player_stats(event_details, c("awayTeamScore"), NA_character_)
                    league_name <- extract_nested_nhl_player_stats(event_details, c("leagueName"), NA_character_)
                    league_abbreviation <- extract_nested_nhl_player_stats(event_details, c("leagueAbbreviation"), NA_character_)

                    # Extract opponent information
                    opponent <- extract_nested_nhl_player_stats(event_details, c("opponent"), list())
                    if (length(opponent) > 0) {
                      opponent_id <- extract_nested_nhl_player_stats(opponent, c("id"), NA_character_)
                      opponent_abbreviation <- extract_nested_nhl_player_stats(opponent, c("abbreviation"), NA_character_)
                      opponent_display_name <- extract_nested_nhl_player_stats(opponent, c("displayName"), NA_character_)
                      opponent_logo <- extract_nested_nhl_player_stats(opponent, c("logo"), NA_character_)
                    }
                  }

                  # Parse game statistics (ESPN provides 14 stats in order)
                  # Default all stats to NA
                  goals <- assists <- points <- plus_minus <- penalty_minutes <- shots_on_goal <- NA_character_
                  power_play_goals <- power_play_assists <- short_handed_goals <- short_handed_assists <- NA_character_
                  game_winning_goals <- overtime_goals <- shooting_percentage <- faceoffs_won <- NA_character_
                  faceoffs_lost <- faceoff_percentage <- NA_character_

                  if (!is.null(game_stats) && length(game_stats) >= 14) {
                    # ESPN order: G, A, PTS, +/-, PIM, SOG, PPG, PPA, SHG, SHA, GWG, OTG, S%, FOW
                    goals <- as.character(game_stats[1])                    # G
                    assists <- as.character(game_stats[2])                  # A
                    points <- as.character(game_stats[3])                   # PTS
                    plus_minus <- as.character(game_stats[4])               # +/-
                    penalty_minutes <- as.character(game_stats[5])          # PIM
                    shots_on_goal <- as.character(game_stats[6])            # SOG
                    power_play_goals <- as.character(game_stats[7])         # PPG
                    power_play_assists <- as.character(game_stats[8])       # PPA
                    short_handed_goals <- as.character(game_stats[9])       # SHG
                    short_handed_assists <- as.character(game_stats[10])    # SHA
                    game_winning_goals <- as.character(game_stats[11])      # GWG
                    overtime_goals <- as.character(game_stats[12])          # OTG
                    shooting_percentage <- as.character(game_stats[13])     # S%
                    faceoffs_won <- as.character(game_stats[14])            # FOW

                    # Additional stats if available (some players may have more)
                    if (length(game_stats) >= 15) {
                      faceoffs_lost <- as.character(game_stats[15])         # FOL
                    }
                    if (length(game_stats) >= 16) {
                      faceoff_percentage <- as.character(game_stats[16])    # FO%
                    }
                  }

                  # Create game row
                  game_row <- data.frame(
                    player_id = as.character(player_id),
                    player_name = as.character(player_info$name),
                    player_display_name = as.character(player_info$display_name),
                    player_jersey = as.character(player_info$jersey),
                    player_position = as.character(player_info$position),
                    player_shoots = as.character(player_info$shoots),
                    player_catches = as.character(player_info$catches),
                    team_id = as.character(player_info$team_id),
                    team_abbreviation = as.character(season_team),
                    team_display_name = as.character(player_info$team_display_name),
                    season_display_name = as.character(season_display_name),
                    season_type = as.character(season_type),
                    category_display_name = as.character(category_display_name),
                    game_id = as.character(event_id),
                    game_date = as.character(game_date),
                    opponent_id = as.character(opponent_id),
                    opponent_abbreviation = as.character(opponent_abbreviation),
                    opponent_display_name = as.character(opponent_display_name),
                    opponent_logo = as.character(opponent_logo),
                    at_vs = as.character(at_vs),
                    score = as.character(score),
                    result = as.character(result),
                    home_team_score = as.character(home_team_score),
                    away_team_score = as.character(away_team_score),
                    league_name = as.character(league_name),
                    league_abbreviation = as.character(league_abbreviation),
                    # Statistics
                    goals = goals,
                    assists = assists,
                    points = points,
                    plus_minus = plus_minus,
                    penalty_minutes = penalty_minutes,
                    shots_on_goal = shots_on_goal,
                    power_play_goals = power_play_goals,
                    power_play_assists = power_play_assists,
                    short_handed_goals = short_handed_goals,
                    short_handed_assists = short_handed_assists,
                    game_winning_goals = game_winning_goals,
                    overtime_goals = overtime_goals,
                    shooting_percentage = shooting_percentage,
                    faceoffs_won = faceoffs_won,
                    faceoffs_lost = faceoffs_lost,
                    faceoff_percentage = faceoff_percentage,
                    stringsAsFactors = FALSE
                  )

                  gamelog_df <- rbind(gamelog_df, game_row)
                }
              }
            }
          }
        }
      }
    }
  }

  # Clean up row names
  if (nrow(gamelog_df) > 0) {
    rownames(gamelog_df) <- NULL
  }

  return(gamelog_df)
}

#' NHL player statistics fetcher with complete game-by-game data
#'
#' Retrieves comprehensive game-by-game statistics for NHL players from ESPN's gamelog API.
#' This function extracts all 14+ statistical categories for each game.
#'
#' @param player_id Character or numeric. NHL player ID from ESPN
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#' @return Invisibly returns processed data frame with comprehensive statistics
#'
#' @details
#' This function processes the complete ESPN gamelog API response to extract:
#'
#' **Complete Game Statistics** (14+ fields per game):
#' - Goals (G), Assists (A), Points (PTS)
#' - Plus/Minus (+/-), Penalty Minutes (PIM), Shots on Goal (SOG)
#' - Power Play Goals (PPG), Power Play Assists (PPA)
#' - Short Handed Goals (SHG), Short Handed Assists (SHA)
#' - Game Winning Goals (GWG), Overtime Goals (OTG)
#' - Shooting Percentage (S%), Faceoffs Won (FOW)
#' - Additional stats when available (FOL, FO%)
#'
#' **Enhanced Game Context**:
#' - Complete opponent information with logos
#' - Home/away team scores
#' - Season type breakdown (Regular/Playoffs/Preseason)
#' - Monthly/category organization
#' - League information
#'
#' @examples
#' \dontrun{
#' # Get Connor McDavid's complete game log
#' fetch_nhl_player_stats("3114478")
#'
#' # View detailed game statistics
#' head(nhl_player_gamelog)
#'
#' # Analyze playoff performance
#' playoff_games <- nhl_player_gamelog[
#'   nhl_player_gamelog$season_type == "playoffs", ]
#'
#' # Calculate season totals
#' regular_season <- nhl_player_gamelog[
#'   nhl_player_gamelog$season_type == "regular", ]
#' total_goals <- sum(as.numeric(regular_season$goals), na.rm = TRUE)
#' total_assists <- sum(as.numeric(regular_season$assists), na.rm = TRUE)
#' total_points <- sum(as.numeric(regular_season$points), na.rm = TRUE)
#'
#' # View recent high-scoring games
#' high_scoring <- nhl_player_gamelog[
#'   as.numeric(nhl_player_gamelog$points) >= 3, ]
#' print(high_scoring[, c("game_date", "opponent_abbreviation", "goals", "assists", "points")])
#' }
#'
#' @export
fetch_nhl_player_stats <- function(player_id, raw = FALSE) {
  # Input validation
  if (missing(player_id)) {
    stop("'player_id' is a required parameter")
  }

  if (!is.logical(raw)) {
    stop("'raw' parameter must be logical (TRUE or FALSE)")
  }

  # First, get player information from the profile endpoint
  message("Fetching player information...")
  player_info <- fetch_nhl_player_info(player_id)
  message("Player found: ", player_info$display_name, " (", player_info$team_abbreviation, ") - ", player_info$position)

  # Get the gamelog data
  url <- paste0("https://site.web.api.espn.com/apis/common/v3/sports/hockey/nhl/athletes/",
                player_id, "/gamelog")

  message("Fetching NHL player game statistics...")
  resp <- httr::GET(url, httr::timeout(30))

  if (httr::status_code(resp) != 200) {
    stop("Failed to fetch player statistics data. Status code: ", httr::status_code(resp))
  }

  data <- httr::content(resp, as = "parsed", simplifyVector = TRUE)

  if (isTRUE(raw)) {
    assign("nhl_player_raw", data, envir = .GlobalEnv)
    message("Raw NHL player statistics data assigned to: nhl_player_raw")
    return(invisible(data))
  }

  # Create game log dataset with complete statistics
  message("Processing game log with complete statistics...")
  gamelog <- create_enhanced_nhl_player_gamelog_dataset(data, player_id, player_info)
  assign("nhl_player_gamelog", gamelog, envir = .GlobalEnv)

  message("Data processed for ", player_info$display_name)
  message("Game log: ", nrow(gamelog), " games with complete statistics")

  # Show sample data
  if (nrow(gamelog) > 0) {
    message("Game log sample (recent games):")
    recent_games <- head(gamelog, 3)
    for (i in 1:nrow(recent_games)) {
      game <- recent_games[i, ]
      message("  ", game$game_date, " vs ", game$opponent_abbreviation, ": ",
              game$goals, "G ", game$assists, "A ", game$points, "P ",
              "(+/- ", game$plus_minus, ") - ", game$result)
    }
  }

  invisible(gamelog)
}

#' Fetch multiple NHL player statistics
#'
#' Retrieves statistics for multiple NHL players with rate limiting to
#' be respectful to ESPN's API. This function calls \code{\link{fetch_nhl_player_stats}}
#' for each player and combines the results.
#'
#' @param player_ids Character or Numeric vector. ESPN NHL player IDs.
#'   Vector of unique identifiers for players in ESPN's database.
#' @param delay Numeric. Delay in seconds between API requests (default: 0.5).
#'   Used to be respectful to ESPN's servers and avoid rate limiting.
#' @param raw Logical. If TRUE, assigns raw JSON to global environment
#'   for the first player only (default: FALSE).
#'
#' @return Invisibly returns the combined data frame. The main purpose is global
#'   environment assignment of combined dataset from all players.
#'
#' @details
#' The function processes players sequentially with a configurable delay
#' between requests. Failed requests for individual players are logged but
#' do not stop the overall process. The final dataset contains data from
#' all successfully processed players with complete game-by-game statistics.
#'
#' This is particularly useful for team analysis, comparing multiple players,
#' or building comprehensive hockey databases with detailed performance metrics.
#'
#' @examples
#' \dontrun{
#' # Get stats for multiple players
#' player_ids <- c("3114478", "5493", "4024716")  # McDavid, Crosby, Pastrnak
#' fetch_multiple_nhl_player_stats(player_ids)
#'
#' # Use longer delay for larger requests
#' fetch_multiple_nhl_player_stats(player_ids, delay = 1.0)
#'
#' # Analyze combined results
#' unique_players <- unique(nhl_player_gamelog$player_display_name)
#' cat("Retrieved statistics for", length(unique_players), "players\n")
#'
#' # Compare power play production
#' pp_production <- aggregate(
#'   cbind(as.numeric(nhl_player_gamelog$power_play_goals),
#'         as.numeric(nhl_player_gamelog$power_play_assists)),
#'   by = list(nhl_player_gamelog$player_display_name),
#'   FUN = sum, na.rm = TRUE
#' )
#' colnames(pp_production) <- c("Player", "PP_Goals", "PP_Assists")
#' print(pp_production)
#'
#' # Analyze game-winning goal leaders
#' gwg_leaders <- aggregate(
#'   as.numeric(nhl_player_gamelog$game_winning_goals),
#'   by = list(nhl_player_gamelog$player_display_name),
#'   FUN = sum, na.rm = TRUE
#' )
#' colnames(gwg_leaders) <- c("Player", "Game_Winning_Goals")
#' gwg_leaders <- gwg_leaders[order(-gwg_leaders$Game_Winning_Goals), ]
#' print(head(gwg_leaders))
#' }
#'
#' @seealso \code{\link{fetch_nhl_player_stats}} for single player data
#' @importFrom httr GET status_code content timeout
#' @export
fetch_multiple_nhl_player_stats <- function(player_ids, delay = 0.5, raw = FALSE) {
  # Input validation
  if (length(player_ids) == 0) {
    stop("'player_ids' must contain at least one player ID")
  }

  if (!is.numeric(delay) || delay < 0) {
    stop("'delay' must be a non-negative number")
  }

  if (!is.logical(raw)) {
    stop("'raw' parameter must be logical (TRUE or FALSE)")
  }

  # Initialize combined data container
  all_gamelog <- data.frame()

  message(sprintf("Starting to fetch NHL player statistics for %d players...", length(player_ids)))

  # Process each player sequentially
  for (i in seq_along(player_ids)) {
    player_id <- player_ids[i]
    message(sprintf("Fetching NHL player stats for player %s (%d/%d)...", player_id, i, length(player_ids)))

    tryCatch({
      # Fetch individual player data
      player_data <- fetch_nhl_player_stats(
        player_id = player_id,
        raw = raw
      )

      # If raw data requested, return after first player
      if (isTRUE(raw)) {
        return(invisible(player_data))
      }

      # Combine data
      gamelog_df <- get("nhl_player_gamelog", envir = .GlobalEnv)
      all_gamelog <- rbind(all_gamelog, gamelog_df)

    }, error = function(e) {
      message(sprintf("Failed to fetch NHL player stats for player %s: %s", player_id, e$message))
    })

    # Be respectful to the API with delay between requests
    if (i < length(player_ids)) {
      Sys.sleep(delay)
    }
  }

  # Remove duplicates and assign combined dataset to global environment
  if (nrow(all_gamelog) > 0) {
    all_gamelog <- all_gamelog[!duplicated(paste(all_gamelog$player_id,
                                                 all_gamelog$game_id)), ]
    assign("nhl_player_gamelog", all_gamelog, envir = .GlobalEnv)

    unique_players <- length(unique(all_gamelog$player_id))
    total_games <- nrow(all_gamelog)
    message(sprintf("Combined NHL player game log assigned to: nhl_player_gamelog (%d players, %d games)",
                    unique_players, total_games))

    # Show combined analytics
    if (nrow(all_gamelog) > 0) {
      regular_season_games <- sum(all_gamelog$season_type == "regular", na.rm = TRUE)
      playoff_games <- sum(all_gamelog$season_type == "playoffs", na.rm = TRUE)
      preseason_games <- sum(all_gamelog$season_type == "preseason", na.rm = TRUE)
      message("Combined game log breakdown:")
      message(sprintf("  - Regular season games: %d", regular_season_games))
      message(sprintf("  - Playoff games: %d", playoff_games))
      message(sprintf("  - Preseason games: %d", preseason_games))

      # Statistical summary
      total_goals <- sum(as.numeric(all_gamelog$goals), na.rm = TRUE)
      total_assists <- sum(as.numeric(all_gamelog$assists), na.rm = TRUE)
      total_points <- sum(as.numeric(all_gamelog$points), na.rm = TRUE)
      total_pp_goals <- sum(as.numeric(all_gamelog$power_play_goals), na.rm = TRUE)
      total_gwg <- sum(as.numeric(all_gamelog$game_winning_goals), na.rm = TRUE)

      message("Combined statistics totals:")
      message(sprintf("  - Total goals: %d", total_goals))
      message(sprintf("  - Total assists: %d", total_assists))
      message(sprintf("  - Total points: %d", total_points))
      message(sprintf("  - Power play goals: %d", total_pp_goals))
      message(sprintf("  - Game winning goals: %d", total_gwg))
    }
  }

  return(invisible(all_gamelog))
}
