#' Safe nested data extraction helper function for mens_college_basketball player statistics
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_player_stats <- function(data, path, default = NA) {
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

#' Get player information from ESPN player profile API
#'
#' Since the gamelog API doesn't include athlete info, fetch it separately
#'
#' @param player_id ESPN player ID
#' @return List with player information
#' @keywords internal
fetch_player_info <- function(player_id) {
  # Try the player profile endpoint
  url <- paste0("https://site.web.api.espn.com/apis/common/v3/sports/basketball/mens-college-basketball/athletes/", player_id)

  tryCatch({
    resp <- httr::GET(url, httr::timeout(10))

    if (httr::status_code(resp) == 200) {
      player_data <- httr::content(resp, as = "parsed", simplifyVector = TRUE)

      # Extract player information
      athlete <- extract_nested_player_stats(player_data, c("athlete"), list())

      if (length(athlete) > 0) {
        return(list(
          name = extract_nested_player_stats(athlete, c("name"), NA_character_),
          display_name = extract_nested_player_stats(athlete, c("displayName"), NA_character_),
          jersey = extract_nested_player_stats(athlete, c("jersey"), NA_character_),
          position = extract_nested_player_stats(athlete, c("position", "abbreviation"), NA_character_),
          team_id = extract_nested_player_stats(athlete, c("team", "id"), NA_character_),
          team_abbreviation = extract_nested_player_stats(athlete, c("team", "abbreviation"), NA_character_),
          team_display_name = extract_nested_player_stats(athlete, c("team", "displayName"), NA_character_)
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
    team_display_name = NA_character_
  ))
}

#' Parse ESPN statistics format
#'
#' ESPN returns stats as character vectors in specific order
#' Based on actual ESPN labels: MIN, FG, FG%, 3PT, 3P%, FT, FT%, REB, AST, BLK, STL, PF, TO, PTS
#'
#' @param stats_vector Character vector of statistics
#' @return Named list of parsed statistics
#' @keywords internal
parse_espn_stats <- function(stats_vector) {
  if (length(stats_vector) == 0) {
    return(list(
      minutes = NA_character_,
      field_goals = NA_character_,
      field_goal_pct = NA_character_,
      three_pointers = NA_character_,
      three_point_pct = NA_character_,
      free_throws = NA_character_,
      free_throw_pct = NA_character_,
      total_rebounds = NA_character_,
      assists = NA_character_,
      blocks = NA_character_,
      steals = NA_character_,
      personal_fouls = NA_character_,
      turnovers = NA_character_,
      points = NA_character_
    ))
  }

  safe_stat <- function(index, default = NA_character_) {
    if (length(stats_vector) >= index && !is.na(stats_vector[index]) && stats_vector[index] != "") {
      return(as.character(stats_vector[index]))
    }
    return(default)
  }

  # ESPN actual order: MIN, FG, FG%, 3PT, 3P%, FT, FT%, REB, AST, BLK, STL, PF, TO, PTS
  return(list(
    minutes = safe_stat(1),           # MIN
    field_goals = safe_stat(2),       # FG
    field_goal_pct = safe_stat(3),    # FG%
    three_pointers = safe_stat(4),    # 3PT
    three_point_pct = safe_stat(5),   # 3P%
    free_throws = safe_stat(6),       # FT
    free_throw_pct = safe_stat(7),    # FT%
    total_rebounds = safe_stat(8),    # REB
    assists = safe_stat(9),           # AST
    blocks = safe_stat(10),           # BLK
    steals = safe_stat(11),           # STL
    personal_fouls = safe_stat(12),   # PF
    turnovers = safe_stat(13),        # TO
    points = safe_stat(14)            # PTS
  ))
}

#' Create season summary statistics data frame from ESPN API
#'
#' Processes season summary data from the seasonTypes$summary structure
#'
#' @param data Raw JSON response from ESPN API
#' @param player_id Player ID used in the request
#' @param player_info Player information from separate API call
#' @return Data frame with season summary statistics
#' @keywords internal
create_mens_college_basketball_season_summary_dataset <- function(data, player_id, player_info) {
  # Initialize season summary data frame
  season_summary_df <- data.frame(
    player_id = character(0),
    player_name = character(0),
    player_display_name = character(0),
    player_jersey = character(0),
    player_position = character(0),
    team_id = character(0),
    team_abbreviation = character(0),
    team_display_name = character(0),
    season_display_name = character(0),
    season_team = character(0),
    stat_type = character(0),
    stat_type_display = character(0),
    minutes = character(0),
    field_goals = character(0),
    field_goal_pct = character(0),
    three_pointers = character(0),
    three_point_pct = character(0),
    free_throws = character(0),
    free_throw_pct = character(0),
    offensive_rebounds = character(0),
    defensive_rebounds = character(0),
    total_rebounds = character(0),
    assists = character(0),
    steals = character(0),
    blocks = character(0),
    turnovers = character(0),
    personal_fouls = character(0),
    points = character(0),
    stringsAsFactors = FALSE
  )

  # Process season types
  season_types <- extract_nested_player_stats(data, c("seasonTypes"), NULL)

  if (is.null(season_types) || !is.data.frame(season_types)) {
    message("No season types found or not in expected format")
    return(season_summary_df)
  }

  # Process each season type
  for (i in 1:nrow(season_types)) {
    season_display_name <- season_types$displayName[i]
    season_team <- season_types$displayTeam[i]

    # Get summary stats for this season using the correct path
    # Based on your structure: seasonTypes$summary[[i]]$stats[[j]]
    if ("summary" %in% names(season_types) && length(season_types$summary) >= i) {
      summary_for_season <- season_types$summary[[i]]

      if (!is.null(summary_for_season) && is.data.frame(summary_for_season) && nrow(summary_for_season) > 0) {
        for (j in 1:nrow(summary_for_season)) {
          stat_type_display <- summary_for_season$displayName[j]

          # Skip if displayName is NA
          if (is.na(stat_type_display)) {
            next
          }

          # Get stats data from the nested structure
          if ("stats" %in% names(summary_for_season) && length(summary_for_season$stats) >= j) {
            stats_data <- summary_for_season$stats[[j]]

            if (!is.null(stats_data) && length(stats_data) > 0) {
              # Determine stat type
              stat_type <- ifelse(grepl("Average", stat_type_display, ignore.case = TRUE), "avg",
                                  ifelse(grepl("Total", stat_type_display, ignore.case = TRUE), "total", "other"))

              # Parse the statistics
              parsed_stats <- parse_espn_stats(stats_data)

              summary_row <- data.frame(
                player_id = as.character(player_id),
                player_name = as.character(player_info$name),
                player_display_name = as.character(player_info$display_name),
                player_jersey = as.character(player_info$jersey),
                player_position = as.character(player_info$position),
                team_id = as.character(player_info$team_id),
                team_abbreviation = as.character(player_info$team_abbreviation),
                team_display_name = as.character(player_info$team_display_name),
                season_display_name = as.character(season_display_name),
                season_team = as.character(ifelse(is.na(season_team), "", season_team)),
                stat_type = as.character(stat_type),
                stat_type_display = as.character(stat_type_display),
                minutes = as.character(parsed_stats$minutes),
                field_goals = as.character(parsed_stats$field_goals),
                field_goal_pct = as.character(parsed_stats$field_goal_pct),
                three_pointers = as.character(parsed_stats$three_pointers),
                three_point_pct = as.character(parsed_stats$three_point_pct),
                free_throws = as.character(parsed_stats$free_throws),
                free_throw_pct = as.character(parsed_stats$free_throw_pct),
                offensive_rebounds = as.character(parsed_stats$offensive_rebounds),
                defensive_rebounds = as.character(parsed_stats$defensive_rebounds),
                total_rebounds = as.character(parsed_stats$total_rebounds),
                assists = as.character(parsed_stats$assists),
                steals = as.character(parsed_stats$steals),
                blocks = as.character(parsed_stats$blocks),
                turnovers = as.character(parsed_stats$turnovers),
                personal_fouls = as.character(parsed_stats$personal_fouls),
                points = as.character(parsed_stats$points),
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

#' Create detailed mens_college_basketball player game log data frame from ESPN API
#'
#' Processes game-by-game data from the seasonTypes$categories structure
#'
#' @param data Raw JSON response from ESPN API
#' @param player_id Player ID used in the request
#' @param player_info Player information from separate API call
#' @return Data frame with detailed game log
#' @keywords internal
create_mens_college_basketball_player_gamelog_dataset <- function(data, player_id, player_info) {
  # Initialize game log data frame
  gamelog_df <- data.frame(
    player_id = character(0),
    player_name = character(0),
    player_display_name = character(0),
    player_jersey = character(0),
    player_position = character(0),
    team_id = character(0),
    team_abbreviation = character(0),
    team_display_name = character(0),
    season_display_name = character(0),
    category_display_name = character(0),
    game_id = character(0),
    minutes = character(0),
    field_goals = character(0),
    field_goal_pct = character(0),
    three_pointers = character(0),
    three_point_pct = character(0),
    free_throws = character(0),
    free_throw_pct = character(0),
    total_rebounds = character(0),
    assists = character(0),
    blocks = character(0),
    steals = character(0),
    personal_fouls = character(0),
    turnovers = character(0),
    points = character(0),
    stringsAsFactors = FALSE
  )

  # Process season types
  season_types <- extract_nested_player_stats(data, c("seasonTypes"), NULL)

  if (is.null(season_types) || !is.data.frame(season_types)) {
    message("No season types found")
    return(gamelog_df)
  }

  # Process each season type
  for (i in 1:nrow(season_types)) {
    season_display_name <- season_types$displayName[i]

    # Get categories for this season using the correct path
    # Based on your structure: seasonTypes$categories[[i]]
    if ("categories" %in% names(season_types) && length(season_types$categories) >= i) {
      categories <- season_types$categories[[i]]

      if (!is.null(categories) && is.data.frame(categories) && nrow(categories) > 0) {
        # Process each category (monthly, playoff rounds, etc.)
        for (j in 1:nrow(categories)) {
          category_display_name <- categories$displayName[j]
          category_type <- categories$type[j]

          # Only process 'event' type categories (individual games)
          if (!is.na(category_type) && category_type == "event") {
            # Get events for this category using the correct path
            # Based on your structure: categories$events[[j]]
            if ("events" %in% names(categories) && length(categories$events) >= j) {
              events_data <- categories$events[[j]]

              if (!is.null(events_data) && is.data.frame(events_data) && nrow(events_data) > 0) {
                # Process each game in this category
                for (k in 1:nrow(events_data)) {
                  game_id <- events_data$eventId[k]

                  # Get stats for this game using the correct path
                  # Based on your structure: events_data$stats[[k]]
                  if ("stats" %in% names(events_data) && length(events_data$stats) >= k) {
                    stats_data <- events_data$stats[[k]]

                    if (!is.null(stats_data) && length(stats_data) > 0) {
                      # Parse the statistics
                      parsed_stats <- parse_espn_stats(stats_data)

                      # Create row for this game
                      game_row <- data.frame(
                        player_id = as.character(player_id),
                        player_name = as.character(player_info$name),
                        player_display_name = as.character(player_info$display_name),
                        player_jersey = as.character(player_info$jersey),
                        player_position = as.character(player_info$position),
                        team_id = as.character(player_info$team_id),
                        team_abbreviation = as.character(player_info$team_abbreviation),
                        team_display_name = as.character(player_info$team_display_name),
                        season_display_name = as.character(season_display_name),
                        category_display_name = as.character(category_display_name),
                        game_id = as.character(game_id),
                        minutes = as.character(parsed_stats$minutes),
                        field_goals = as.character(parsed_stats$field_goals),
                        field_goal_pct = as.character(parsed_stats$field_goal_pct),
                        three_pointers = as.character(parsed_stats$three_pointers),
                        three_point_pct = as.character(parsed_stats$three_point_pct),
                        free_throws = as.character(parsed_stats$free_throws),
                        free_throw_pct = as.character(parsed_stats$free_throw_pct),
                        total_rebounds = as.character(parsed_stats$total_rebounds),
                        assists = as.character(parsed_stats$assists),
                        blocks = as.character(parsed_stats$blocks),
                        steals = as.character(parsed_stats$steals),
                        personal_fouls = as.character(parsed_stats$personal_fouls),
                        turnovers = as.character(parsed_stats$turnovers),
                        points = as.character(parsed_stats$points),
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
    }
  }

  # Clean up row names
  if (nrow(gamelog_df) > 0) {
    rownames(gamelog_df) <- NULL
  }

  return(gamelog_df)
}

#' Fetch detailed mens_college_basketball player statistics using ESPN Web API
#'
#' Retrieves comprehensive statistics for a specific mens_college_basketball player from ESPN's Web API.
#' Extracts data directly from the seasonTypes structure - fast and reliable!
#'
#' @param player_id Character or numeric. mens_college_basketball player ID from ESPN
#'   (e.g., "3032977" for Giannis Antetokounmpo, "1966" for LeBron James).
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'mens_college_basketball_player_stats_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns a list with both datasets. The main purpose is global
#'   environment assignment of:
#'   \itemize{
#'     \item \code{mens_college_basketball_player_season_summary}: Season averages and totals by season type
#'     \item \code{mens_college_basketball_player_gamelog}: Game-by-game statistics organized by category
#'   }
#'
#' @details
#' The function extracts data directly from ESPN's seasonTypes structure which contains:
#'
#' **Season Summary** (`seasonTypes$summary`):
#' - Averages and totals for each season type (Regular Season, Playoffs, Preseason)
#' - Complete statistical categories for season-level analysis
#'
#' **Game Log** (`seasonTypes$categories$events`):
#' - Individual game statistics organized by categories (monthly, playoff rounds)
#' - Complete box score data for each game
#' - Season and category context for filtering and analysis
#'
#' **Statistical Categories** (16 total):
#' Minutes, Field Goals, FG%, 3-Pointers, 3P%, Free Throws, FT%,
#' Offensive Rebounds, Defensive Rebounds, Total Rebounds, Assists,
#' Steals, Blocks, Turnovers, Personal Fouls, Points
#'
#' @examples
#' \dontrun{
#' # Get Giannis statistics
#' fetch_mens_college_basketball_player_stats("3032977")
#'
#' # Check season summaries
#' print(mens_college_basketball_player_season_summary)
#'
#' # Check game log
#' head(mens_college_basketball_player_gamelog)
#'
#' # View regular season averages
#' reg_season_avg <- mens_college_basketball_player_season_summary[
#'   mens_college_basketball_player_season_summary$season_display_name == "2024-25 Regular Season" &
#'   mens_college_basketball_player_season_summary$stat_type == "avg", ]
#' print(reg_season_avg[, c("points", "total_rebounds", "assists", "field_goal_pct")])
#'
#' # View playoff games
#' playoff_games <- mens_college_basketball_player_gamelog[
#'   mens_college_basketball_player_gamelog$season_display_name == "2024-25 Postseason", ]
#' head(playoff_games[, c("category_display_name", "points", "total_rebounds", "assists")])
#'
#' # Calculate averages from game log
#' all_games <- mens_college_basketball_player_gamelog[!is.na(mens_college_basketball_player_gamelog$points), ]
#' avg_points <- mean(as.numeric(all_games$points), na.rm = TRUE)
#' avg_rebounds <- mean(as.numeric(all_games$total_rebounds), na.rm = TRUE)
#' avg_assists <- mean(as.numeric(all_games$assists), na.rm = TRUE)
#'
#' print(paste("Calculated averages:", round(avg_points, 1), "PPG,",
#'             round(avg_rebounds, 1), "RPG,", round(avg_assists, 1), "APG"))
#' }
#'
#' @export
fetch_mens_college_basketball_player_stats <- function(player_id, raw = FALSE) {
  # First, get player information from the profile endpoint
  message("Fetching player information...")
  player_info <- fetch_player_info(player_id)
  message("Player found: ", player_info$display_name, " (", player_info$team_abbreviation, ")")

  # Get the gamelog data which contains all statistics in seasonTypes
  url <- paste0("https://site.web.api.espn.com/apis/common/v3/sports/basketball/mens-college-basketball/athletes/",
                player_id, "/gamelog")

  resp <- httr::GET(url, httr::timeout(30))

  if (httr::status_code(resp) != 200) {
    stop("Failed to fetch player statistics data. Status code: ", httr::status_code(resp))
  }

  data <- httr::content(resp, as = "parsed", simplifyVector = TRUE)

  if (isTRUE(raw)) {
    assign("mens_college_basketball_player_stats_raw", data, envir = .GlobalEnv)
  }

  # Debug: Show the structure we're working with
  message("Processing ESPN data structure:")
  season_types <- data$seasonTypes
  message("  Found ", nrow(season_types), " season types")
  for (i in 1:nrow(season_types)) {
    message("    ", i, ": ", season_types$displayName[i])
  }

  # Create game log dataset only (no season summary)
  message("Processing game log...")
  gamelog <- create_mens_college_basketball_player_gamelog_dataset(data, player_id, player_info)
  assign("mens_college_basketball_player_gamelog", gamelog, envir = .GlobalEnv)

  message("Fetched data for ", player_info$display_name)
  message("Game log: ", nrow(gamelog), " games")

  # Show sample data
  if (nrow(gamelog) > 0) {
    recent_games <- head(gamelog[!is.na(gamelog$points), ], 3)
    message("Game log sample:")
    for (i in 1:min(3, nrow(recent_games))) {
      game <- recent_games[i, ]
      message("  Game ", game$game_id, " (", game$category_display_name, "): ",
              game$points, " pts, ", game$total_rebounds, " reb, ", game$assists, " ast")
    }
  }

  result <- list(
    gamelog = gamelog
  )

  invisible(result)
}
