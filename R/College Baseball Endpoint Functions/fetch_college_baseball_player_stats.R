#' Safe nested data extraction helper function for college_baseball player statistics
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_college_baseball_player_stats <- function(data, path, default = NA) {
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

#' Get college_baseball player information from ESPN player profile API
#'
#' Since the gamelog API doesn't include athlete info, fetch it separately
#'
#' @param player_id ESPN player ID
#' @return List with player information
#' @keywords internal
fetch_college_baseball_player_info <- function(player_id) {
  # Try the player profile endpoint
  url <- paste0("https://site.web.api.espn.com/apis/common/v3/sports/baseball/college-baseball/athletes/", player_id)

  tryCatch({
    resp <- httr::GET(url, httr::timeout(10))
    if (httr::status_code(resp) == 200) {
      player_data <- httr::content(resp, as = "parsed", simplifyVector = TRUE)

      # Extract player information
      athlete <- extract_nested_college_baseball_player_stats(player_data, c("athlete"), list())
      if (length(athlete) > 0) {
        return(list(
          name = extract_nested_college_baseball_player_stats(athlete, c("name"), NA_character_),
          display_name = extract_nested_college_baseball_player_stats(athlete, c("displayName"), NA_character_),
          jersey = extract_nested_college_baseball_player_stats(athlete, c("jersey"), NA_character_),
          position = extract_nested_college_baseball_player_stats(athlete, c("position", "abbreviation"), NA_character_),
          team_id = extract_nested_college_baseball_player_stats(athlete, c("team", "id"), NA_character_),
          team_abbreviation = extract_nested_college_baseball_player_stats(athlete, c("team", "abbreviation"), NA_character_),
          team_display_name = extract_nested_college_baseball_player_stats(athlete, c("team", "displayName"), NA_character_),
          bats = extract_nested_college_baseball_player_stats(athlete, c("bats", "abbreviation"), NA_character_),
          throws = extract_nested_college_baseball_player_stats(athlete, c("throws", "abbreviation"), NA_character_)
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
    bats = NA_character_,
    throws = NA_character_
  ))
}

#' Parse ESPN college_baseball statistics format
#'
#' ESPN returns batting stats as: AVG, OBP, SLG, OPS, AB, R, H, 2B, 3B, HR, RBI, BB, SO, SB, CS, E
#' ESPN returns pitching stats as: ERA, WHIP, IP, H, R, ER, HR, BB, SO, W, L, SV, HLD, BS, BLSV
#'
#' @param stats_vector Character vector of statistics
#' @param stat_type Character. Type of stats: "batting" or "pitching"
#' @return Named list of parsed statistics
#' @keywords internal
parse_college_baseball_espn_stats <- function(stats_vector, stat_type = "batting") {
  if (length(stats_vector) == 0) {
    if (stat_type == "batting") {
      return(list(
        avg = NA_character_, obp = NA_character_, slg = NA_character_, ops = NA_character_,
        at_bats = NA_character_, runs = NA_character_, hits = NA_character_,
        doubles = NA_character_, triples = NA_character_, home_runs = NA_character_,
        rbi = NA_character_, walks = NA_character_, strikeouts = NA_character_,
        stolen_bases = NA_character_, caught_stealing = NA_character_, errors = NA_character_
      ))
    } else {
      return(list(
        era = NA_character_, whip = NA_character_, innings_pitched = NA_character_,
        hits_allowed = NA_character_, runs_allowed = NA_character_, earned_runs = NA_character_,
        home_runs_allowed = NA_character_, walks_allowed = NA_character_, strikeouts = NA_character_,
        wins = NA_character_, losses = NA_character_, saves = NA_character_,
        holds = NA_character_, blown_saves = NA_character_, blown_saves_losses = NA_character_
      ))
    }
  }

  safe_stat <- function(index, default = NA_character_) {
    if (length(stats_vector) >= index && !is.na(stats_vector[index]) && stats_vector[index] != "") {
      return(as.character(stats_vector[index]))
    }
    return(default)
  }

  if (stat_type == "batting") {
    # ESPN batting order: AVG, OBP, SLG, OPS, AB, R, H, 2B, 3B, HR, RBI, BB, SO, SB, CS, E
    return(list(
      avg = safe_stat(1),                # AVG
      obp = safe_stat(2),                # OBP
      slg = safe_stat(3),                # SLG
      ops = safe_stat(4),                # OPS
      at_bats = safe_stat(5),            # AB
      runs = safe_stat(6),               # R
      hits = safe_stat(7),               # H
      doubles = safe_stat(8),            # 2B
      triples = safe_stat(9),            # 3B
      home_runs = safe_stat(10),         # HR
      rbi = safe_stat(11),               # RBI
      walks = safe_stat(12),             # BB
      strikeouts = safe_stat(13),        # SO
      stolen_bases = safe_stat(14),      # SB
      caught_stealing = safe_stat(15),   # CS
      errors = safe_stat(16)             # E
    ))
  } else {
    # ESPN pitching order: ERA, WHIP, IP, H, R, ER, HR, BB, SO, W, L, SV, HLD, BS, BLSV
    return(list(
      era = safe_stat(1),                      # ERA
      whip = safe_stat(2),                     # WHIP
      innings_pitched = safe_stat(3),          # IP
      hits_allowed = safe_stat(4),             # H
      runs_allowed = safe_stat(5),             # R
      earned_runs = safe_stat(6),              # ER
      home_runs_allowed = safe_stat(7),        # HR
      walks_allowed = safe_stat(8),            # BB
      strikeouts = safe_stat(9),               # SO
      wins = safe_stat(10),                    # W
      losses = safe_stat(11),                  # L
      saves = safe_stat(12),                   # SV
      holds = safe_stat(13),                   # HLD
      blown_saves = safe_stat(14),             # BS
      blown_saves_losses = safe_stat(15)       # BLSV
    ))
  }
}

#' Create college_baseball season summary statistics data frame from ESPN API
#'
#' Processes season summary data from the seasonTypes$summary structure
#'
#' @param data Raw JSON response from ESPN API
#' @param player_id Player ID used in the request
#' @param player_info Player information from separate API call
#' @return Data frame with season summary statistics
#' @keywords internal
create_college_baseball_season_summary_dataset <- function(data, player_id, player_info) {
  # Initialize season summary data frame for both batting and pitching
  season_summary_df <- data.frame(
    player_id = character(0),
    player_name = character(0),
    player_display_name = character(0),
    player_jersey = character(0),
    player_position = character(0),
    player_bats = character(0),
    player_throws = character(0),
    team_id = character(0),
    team_abbreviation = character(0),
    team_display_name = character(0),
    season_display_name = character(0),
    season_team = character(0),
    stat_category = character(0),
    stat_type = character(0),
    stat_type_display = character(0),
    # Batting stats
    avg = character(0), obp = character(0), slg = character(0), ops = character(0),
    at_bats = character(0), runs = character(0), hits = character(0),
    doubles = character(0), triples = character(0), home_runs = character(0),
    rbi = character(0), walks = character(0), strikeouts_batting = character(0),
    stolen_bases = character(0), caught_stealing = character(0), errors = character(0),
    # Pitching stats
    era = character(0), whip = character(0), innings_pitched = character(0),
    hits_allowed = character(0), runs_allowed = character(0), earned_runs = character(0),
    home_runs_allowed = character(0), walks_allowed = character(0), strikeouts_pitching = character(0),
    wins = character(0), losses = character(0), saves = character(0),
    holds = character(0), blown_saves = character(0), blown_saves_losses = character(0),
    stringsAsFactors = FALSE
  )

  # Process season types
  season_types <- extract_nested_college_baseball_player_stats(data, c("seasonTypes"), NULL)
  if (is.null(season_types) || !is.data.frame(season_types)) {
    message("No season types found or not in expected format")
    return(season_summary_df)
  }

  # Process each season type
  for (i in 1:nrow(season_types)) {
    season_display_name <- season_types$displayName[i]
    season_team <- season_types$displayTeam[i]

    # Get summary stats for this season
    if ("summary" %in% names(season_types) && length(season_types$summary) >= i) {
      summary_for_season <- season_types$summary[[i]]
      if (!is.null(summary_for_season) && is.data.frame(summary_for_season) && nrow(summary_for_season) > 0) {
        for (j in 1:nrow(summary_for_season)) {
          stat_type_display <- summary_for_season$displayName[j]
          stat_category <- summary_for_season$name[j]

          # Skip if displayName is NA
          if (is.na(stat_type_display)) {
            next
          }

          # Get stats data from the nested structure
          if ("stats" %in% names(summary_for_season) && length(summary_for_season$stats) >= j) {
            stats_data <- summary_for_season$stats[[j]]
            if (!is.null(stats_data) && length(stats_data) > 0) {
              # Determine stat type and category
              stat_type <- ifelse(grepl("Average", stat_type_display, ignore.case = TRUE), "avg",
                                  ifelse(grepl("Total", stat_type_display, ignore.case = TRUE), "total", "other"))

              # Determine if this is batting or pitching based on category
              is_batting <- grepl("batting|hitting", stat_category, ignore.case = TRUE)
              is_pitching <- grepl("pitching", stat_category, ignore.case = TRUE)

              # Handle empty results by defaulting to FALSE
              if (length(is_batting) == 0) is_batting <- FALSE
              if (length(is_pitching) == 0) is_pitching <- FALSE

              # Skip if we can't determine the category
              if (!is_batting && !is_pitching) {
                next
              }

              # Parse the statistics
              if (is_batting) {
                parsed_stats <- parse_college_baseball_espn_stats(stats_data, "batting")
                pitching_stats <- list(era = NA_character_, whip = NA_character_, innings_pitched = NA_character_,
                                       hits_allowed = NA_character_, runs_allowed = NA_character_, earned_runs = NA_character_,
                                       home_runs_allowed = NA_character_, walks_allowed = NA_character_, strikeouts_pitching = NA_character_,
                                       wins = NA_character_, losses = NA_character_, saves = NA_character_,
                                       holds = NA_character_, blown_saves = NA_character_, blown_saves_losses = NA_character_)
                batting_stats <- parsed_stats
                batting_stats$strikeouts_batting <- batting_stats$strikeouts
                batting_stats$strikeouts <- NULL
              } else if (is_pitching) {
                parsed_stats <- parse_college_baseball_espn_stats(stats_data, "pitching")
                batting_stats <- list(avg = NA_character_, obp = NA_character_, slg = NA_character_, ops = NA_character_,
                                      at_bats = NA_character_, runs = NA_character_, hits = NA_character_,
                                      doubles = NA_character_, triples = NA_character_, home_runs = NA_character_,
                                      rbi = NA_character_, walks = NA_character_, strikeouts_batting = NA_character_,
                                      stolen_bases = NA_character_, caught_stealing = NA_character_, errors = NA_character_)
                pitching_stats <- parsed_stats
                pitching_stats$strikeouts_pitching <- pitching_stats$strikeouts
                pitching_stats$strikeouts <- NULL
              } else {
                next  # Skip unknown categories
              }

              summary_row <- data.frame(
                player_id = as.character(player_id),
                player_name = as.character(player_info$name),
                player_display_name = as.character(player_info$display_name),
                player_jersey = as.character(player_info$jersey),
                player_position = as.character(player_info$position),
                player_bats = as.character(player_info$bats),
                player_throws = as.character(player_info$throws),
                team_id = as.character(player_info$team_id),
                team_abbreviation = as.character(player_info$team_abbreviation),
                team_display_name = as.character(player_info$team_display_name),
                season_display_name = as.character(season_display_name),
                season_team = as.character(ifelse(is.na(season_team), "", season_team)),
                stat_category = as.character(stat_category),
                stat_type = as.character(stat_type),
                stat_type_display = as.character(stat_type_display),
                # Batting stats
                avg = as.character(batting_stats$avg),
                obp = as.character(batting_stats$obp),
                slg = as.character(batting_stats$slg),
                ops = as.character(batting_stats$ops),
                at_bats = as.character(batting_stats$at_bats),
                runs = as.character(batting_stats$runs),
                hits = as.character(batting_stats$hits),
                doubles = as.character(batting_stats$doubles),
                triples = as.character(batting_stats$triples),
                home_runs = as.character(batting_stats$home_runs),
                rbi = as.character(batting_stats$rbi),
                walks = as.character(batting_stats$walks),
                strikeouts_batting = as.character(batting_stats$strikeouts_batting),
                stolen_bases = as.character(batting_stats$stolen_bases),
                caught_stealing = as.character(batting_stats$caught_stealing),
                errors = as.character(batting_stats$errors),
                # Pitching stats
                era = as.character(pitching_stats$era),
                whip = as.character(pitching_stats$whip),
                innings_pitched = as.character(pitching_stats$innings_pitched),
                hits_allowed = as.character(pitching_stats$hits_allowed),
                runs_allowed = as.character(pitching_stats$runs_allowed),
                earned_runs = as.character(pitching_stats$earned_runs),
                home_runs_allowed = as.character(pitching_stats$home_runs_allowed),
                walks_allowed = as.character(pitching_stats$walks_allowed),
                strikeouts_pitching = as.character(pitching_stats$strikeouts_pitching),
                wins = as.character(pitching_stats$wins),
                losses = as.character(pitching_stats$losses),
                saves = as.character(pitching_stats$saves),
                holds = as.character(pitching_stats$holds),
                blown_saves = as.character(pitching_stats$blown_saves),
                blown_saves_losses = as.character(pitching_stats$blown_saves_losses),
                stringsAsFactors = FALSE
              )

              season_summary_df <- rbind(season_summary_df, summary_row)
            }
          }
        }
      }
    }
  }

  # Clean up row names
  if (nrow(season_summary_df) > 0) {
    rownames(season_summary_df) <- NULL
  }

  return(season_summary_df)
}

#' Create detailed college_baseball player game log data frame from ESPN API
#'
#' Processes game-by-game data from the seasonTypes$categories structure
#'
#' @param data Raw JSON response from ESPN API
#' @param player_id Player ID used in the request
#' @param player_info Player information from separate API call
#' @return Data frame with detailed game log
#' @keywords internal
create_college_baseball_player_gamelog_dataset <- function(data, player_id, player_info) {
  # Initialize game log data frame
  gamelog_df <- data.frame(
    player_id = character(0),
    player_name = character(0),
    player_display_name = character(0),
    player_jersey = character(0),
    player_position = character(0),
    player_bats = character(0),
    player_throws = character(0),
    team_id = character(0),
    team_abbreviation = character(0),
    team_display_name = character(0),
    season_display_name = character(0),
    game_id = character(0),
    game_date = character(0),
    opponent_id = character(0),
    opponent_abbreviation = character(0),
    at_vs = character(0),
    score = character(0),
    result = character(0),
    stringsAsFactors = FALSE
  )

  # The actual structure has events directly, not in seasonTypes$categories
  events_data <- extract_nested_college_baseball_player_stats(data, c("events"), NULL)

  if (is.null(events_data) || !is.list(events_data) || length(events_data) == 0) {
    message("No events data found")
    return(gamelog_df)
  }

  # Process each game event
  for (event_name in names(events_data)) {
    event_data <- events_data[[event_name]]

    if (!is.null(event_data) && is.list(event_data)) {
      # Extract game information
      game_id <- extract_nested_college_baseball_player_stats(event_data, c("id"), event_name)
      game_date <- extract_nested_college_baseball_player_stats(event_data, c("gameDate"), NA_character_)
      at_vs <- extract_nested_college_baseball_player_stats(event_data, c("atVs"), NA_character_)
      score <- extract_nested_college_baseball_player_stats(event_data, c("score"), NA_character_)
      result <- extract_nested_college_baseball_player_stats(event_data, c("gameResult"), NA_character_)

      # Extract opponent information
      opponent <- extract_nested_college_baseball_player_stats(event_data, c("opponent"), list())
      opponent_id <- extract_nested_college_baseball_player_stats(opponent, c("id"), NA_character_)
      opponent_abbreviation <- extract_nested_college_baseball_player_stats(opponent, c("abbreviation"), NA_character_)

      # Extract team information
      team <- extract_nested_college_baseball_player_stats(event_data, c("team"), list())
      team_abbreviation <- extract_nested_college_baseball_player_stats(team, c("abbreviation"), NA_character_)

      # Create row for this game
      game_row <- data.frame(
        player_id = as.character(player_id),
        player_name = as.character(player_info$name),
        player_display_name = as.character(player_info$display_name),
        player_jersey = as.character(player_info$jersey),
        player_position = as.character(player_info$position),
        player_bats = as.character(player_info$bats),
        player_throws = as.character(player_info$throws),
        team_id = as.character(player_info$team_id),
        team_abbreviation = as.character(team_abbreviation),
        team_display_name = as.character(player_info$team_display_name),
        season_display_name = "2025 Regular Season",  # Default since not in event data
        game_id = as.character(game_id),
        game_date = as.character(game_date),
        opponent_id = as.character(opponent_id),
        opponent_abbreviation = as.character(opponent_abbreviation),
        at_vs = as.character(at_vs),
        score = as.character(score),
        result = as.character(result),
        stringsAsFactors = FALSE
      )

      gamelog_df <- rbind(gamelog_df, game_row)
    }
  }

  # Clean up row names
  if (nrow(gamelog_df) > 0) {
    rownames(gamelog_df) <- NULL
  }

  return(gamelog_df)
}

#' Fetch detailed college_baseball player statistics using ESPN Web API
#'
#' Retrieves comprehensive statistics for a specific college_baseball player from ESPN's Web API.
#' Extracts data directly from the seasonTypes structure - fast and reliable!
#'
#' @param player_id Character or numeric. college_baseball player ID from ESPN
#'   (e.g., "30951" for Bryce Harper, "33712" for Kyle Schwarber).
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'college_baseball_player_stats_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns a list with both datasets. The main purpose is global
#'   environment assignment of:
#'   \itemize{
#'     \item \code{college_baseball_player_season_summary}: Season averages and totals by season type
#'     \item \code{college_baseball_player_gamelog}: Game-by-game statistics organized by category
#'   }
#'
#' @details
#' The function extracts data directly from ESPN's seasonTypes structure which contains:
#'
#' **Season Summary** (`seasonTypes$summary`):
#' - Averages and totals for each season type (Regular Season, Playoffs, Spring Training)
#' - Complete statistical categories for season-level analysis
#' - Separate batting and pitching statistics
#'
#' **Game Log** (`seasonTypes$categories$events`):
#' - Individual game statistics organized by categories (monthly, playoff rounds)
#' - Complete box score data for each game
#' - Season and category context for filtering and analysis
#'
#' **Batting Statistics** (16 total):
#' AVG, OBP, SLG, OPS, At-Bats, Runs, Hits, Doubles, Triples, Home Runs,
#' RBI, Walks, Strikeouts, Stolen Bases, Caught Stealing, Errors
#'
#' **Pitching Statistics** (15 total):
#' ERA, WHIP, Innings Pitched, Hits Allowed, Runs Allowed, Earned Runs,
#' Home Runs Allowed, Walks Allowed, Strikeouts, Wins, Losses, Saves,
#' Holds, Blown Saves, Blown Save Losses
#'
#' @examples
#' \dontrun{
#' # Get Bryce Harper statistics
#' fetch_college_baseball_player_stats("30951")
#'
#' # Check season summaries
#' print(college_baseball_player_season_summary)
#'
#' # Check game log
#' head(college_baseball_player_gamelog)
#'
#' # View regular season batting averages
#' reg_season_batting <- college_baseball_player_season_summary[
#'   college_baseball_player_season_summary$season_display_name == "2024 Regular Season" &
#'   college_baseball_player_season_summary$stat_category == "batting" &
#'   college_baseball_player_season_summary$stat_type == "avg", ]
#' print(reg_season_batting[, c("avg", "home_runs", "rbi", "ops")])
#'
#' # View recent games
#' recent_games <- head(college_baseball_player_gamelog[
#'   college_baseball_player_gamelog$stat_category == "batting" &
#'   !is.na(college_baseball_player_gamelog$at_bats), ], 10)
#' print(recent_games[, c("category_display_name", "at_bats", "hits", "home_runs", "rbi")])
#'
#' # View pitching statistics (for pitchers)
#' pitching_stats <- college_baseball_player_season_summary[
#'   college_baseball_player_season_summary$stat_category == "pitching" &
#'   college_baseball_player_season_summary$stat_type == "total", ]
#' print(pitching_stats[, c("era", "whip", "innings_pitched", "strikeouts_pitching", "wins")])
#'
#' # Calculate batting average from game log
#' batting_games <- college_baseball_player_gamelog[
#'   college_baseball_player_gamelog$stat_category == "batting" &
#'   !is.na(college_baseball_player_gamelog$at_bats) &
#'   as.numeric(college_baseball_player_gamelog$at_bats) > 0, ]
#'
#' total_hits <- sum(as.numeric(batting_games$hits), na.rm = TRUE)
#' total_at_bats <- sum(as.numeric(batting_games$at_bats), na.rm = TRUE)
#' calculated_avg <- round(total_hits / total_at_bats, 3)
#'
#' print(paste("Calculated batting average:", calculated_avg))
#' }
#'
#' @importFrom httr GET status_code content timeout
#' @export
fetch_college_baseball_player_stats <- function(player_id, raw = FALSE) {
  # Input validation
  if (missing(player_id)) {
    stop("'player_id' is a required parameter")
  }

  if (!is.logical(raw)) {
    stop("'raw' parameter must be logical (TRUE or FALSE)")
  }

  # First, get player information from the profile endpoint
  message("Fetching player information...")
  player_info <- fetch_college_baseball_player_info(player_id)
  message("Player found: ", player_info$display_name, " (", player_info$team_abbreviation, ") - ", player_info$position)

  # Get the gamelog data which contains all statistics in seasonTypes
  url <- paste0("https://site.web.api.espn.com/apis/common/v3/sports/baseball/college-baseball/athletes/",
                player_id, "/gamelog")

  message("Fetching college_baseball player statistics...")

  resp <- httr::GET(url, httr::timeout(30))

  if (httr::status_code(resp) != 200) {
    stop("Failed to fetch player statistics data. Status code: ", httr::status_code(resp))
  }

  data <- httr::content(resp, as = "parsed", simplifyVector = TRUE)

  if (isTRUE(raw)) {
    assign("college_baseball_player_stats_raw", data, envir = .GlobalEnv)
    message("Raw college_baseball player statistics data assigned to: college_baseball_player_stats_raw")
    return(invisible(data))
  }

  # Debug: Show the structure we're working with
  message("Processing ESPN data structure:")
  season_types <- data$seasonTypes
  if (!is.null(season_types) && is.data.frame(season_types)) {
    message("  Found ", nrow(season_types), " season types")
    for (i in 1:nrow(season_types)) {
      message("    ", i, ": ", season_types$displayName[i])
    }
  }

  # Create game log dataset only (no season summary)
  message("Processing game log...")
  gamelog <- create_college_baseball_player_gamelog_dataset(data, player_id, player_info)
  assign("college_baseball_player_gamelog", gamelog, envir = .GlobalEnv)

  message("Fetched data for ", player_info$display_name)
  message("Game log: ", nrow(gamelog), " games")

  # Show sample data
  if (nrow(gamelog) > 0) {
    message("Game log sample:")
    for (i in 1:min(3, nrow(gamelog))) {
      game <- gamelog[i, ]
      message("  Game ", game$game_id, " (", game$game_date, "): ",
              game$opponent_abbreviation, " ", game$score, " (", game$result, ")")
    }
  }

  result <- list(
    gamelog = gamelog
  )

  invisible(result)
}

#' Fetch multiple college_baseball player statistics
#'
#' Retrieves statistics for multiple college_baseball players with rate limiting to
#' be respectful to ESPN's API. This function calls \code{\link{fetch_college_baseball_player_stats}}
#' for each player and combines the results.
#'
#' @param player_ids Character or Numeric vector. ESPN college_baseball player IDs.
#'   Vector of unique identifiers for players in ESPN's database.
#' @param delay Numeric. Delay in seconds between API requests (default: 0.5).
#'   Used to be respectful to ESPN's servers and avoid rate limiting.
#' @param raw Logical. If TRUE, assigns raw JSON to global environment
#'   for the first player only (default: FALSE).
#'
#' @return Invisibly returns the combined data frames. The main purpose is global
#'   environment assignment of combined datasets from all players.
#'
#' @details
#' The function processes players sequentially with a configurable delay
#' between requests. Failed requests for individual players are logged but
#' do not stop the overall process. The final datasets contain data from
#' all successfully processed players.
#'
#' This is particularly useful for team analysis, comparing multiple players,
#' or building comprehensive baseball databases.
#'
#' @examples
#' \dontrun{
#' # Get stats for multiple players
#' player_ids <- c("30951", "33712", "41169")  # Harper, Schwarber, Bohm
#' fetch_multiple_college_baseball_player_stats(player_ids)
#'
#' # Use longer delay for larger requests
#' fetch_multiple_college_baseball_player_stats(player_ids, delay = 1.0)
#'
#' # Analyze combined results
#' unique_players <- unique(college_baseball_player_season_summary$player_display_name)
#' cat("Retrieved statistics for", length(unique_players), "players\n")
#'
#' # Compare batting averages
#' batting_comparison <- college_baseball_player_season_summary[
#'   college_baseball_player_season_summary$stat_category == "batting" &
#'   college_baseball_player_season_summary$stat_type == "avg",
#'   c("player_display_name", "avg", "home_runs", "rbi", "ops")
#' ]
#' print(batting_comparison)
#' }
#'
#' @seealso \code{\link{fetch_college_baseball_player_stats}} for single player data
#' @importFrom httr GET status_code content timeout
#' @export
fetch_multiple_college_baseball_player_stats <- function(player_ids, delay = 0.5, raw = FALSE) {
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

  # Initialize combined data containers
  all_season_summary <- data.frame()
  all_gamelog <- data.frame()

  message(sprintf("Starting to fetch college_baseball player statistics for %d players...", length(player_ids)))

  # Process each player sequentially
  for (i in seq_along(player_ids)) {
    player_id <- player_ids[i]
    message(sprintf("Fetching college_baseball player stats for player %s (%d/%d)...", player_id, i, length(player_ids)))

    tryCatch({
      # Fetch individual player data
      player_data <- fetch_college_baseball_player_stats(
        player_id = player_id,
        raw = raw
      )

      # If raw data requested, return after first player
      if (isTRUE(raw)) {
        return(invisible(player_data))
      }

      # Combine data
      summary_df <- get("college_baseball_player_season_summary", envir = .GlobalEnv)
      gamelog_df <- get("college_baseball_player_gamelog", envir = .GlobalEnv)

      all_season_summary <- rbind(all_season_summary, summary_df)
      all_gamelog <- rbind(all_gamelog, gamelog_df)

    }, error = function(e) {
      message(sprintf("Failed to fetch college_baseball player stats for player %s: %s", player_id, e$message))
    })

    # Be respectful to the API with delay between requests
    if (i < length(player_ids)) {
      Sys.sleep(delay)
    }
  }

  # Remove duplicates and assign combined datasets to global environment
  if (nrow(all_season_summary) > 0) {
    all_season_summary <- all_season_summary[!duplicated(paste(all_season_summary$player_id,
                                                               all_season_summary$season_display_name,
                                                               all_season_summary$stat_category,
                                                               all_season_summary$stat_type)), ]
    assign("college_baseball_player_season_summary", all_season_summary, envir = .GlobalEnv)

    unique_players <- length(unique(all_season_summary$player_id))
    message(sprintf("Combined college_baseball player season summary assigned to: college_baseball_player_season_summary (%d players, %d records)",
                    unique_players, nrow(all_season_summary)))
  }

  if (nrow(all_gamelog) > 0) {
    all_gamelog <- all_gamelog[!duplicated(paste(all_gamelog$player_id,
                                                 all_gamelog$game_id,
                                                 all_gamelog$stat_category)), ]
    assign("college_baseball_player_gamelog", all_gamelog, envir = .GlobalEnv)

    unique_players <- length(unique(all_gamelog$player_id))
    total_games <- nrow(all_gamelog)
    message(sprintf("Combined college_baseball player game log assigned to: college_baseball_player_gamelog (%d players, %d games)",
                    unique_players, total_games))

    # Show combined analytics
    if (nrow(all_gamelog) > 0) {
      batting_games <- sum(all_gamelog$stat_category == "batting", na.rm = TRUE)
      pitching_games <- sum(all_gamelog$stat_category == "pitching", na.rm = TRUE)

      message("Combined game log breakdown:")
      message(sprintf("  - Total batting games: %d", batting_games))
      message(sprintf("  - Total pitching games: %d", pitching_games))
    }
  }

  result_data <- list(
    season_summary = all_season_summary,
    gamelog = all_gamelog
  )

  return(invisible(result_data))
}
