#' Safe nested data extraction helper function for NHL boxscore data
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_nhl_boxscore <- function(data, path, default = NA) {
  tryCatch({
    result <- data
    for (key in path) {
      if (is.null(result) || (!is.list(result) && !is.data.frame(result)) ||
          !key %in% names(result)) {
        return(default)
      }
      result <- result[[key]]
    }
    if (is.null(result)) default else result
  }, error = function(e) {
    default
  })
}

#' Create NHL player statistics data frame from ESPN boxscore API
#'
#' Processes player statistics from ESPN boxscore players data
#' @param data Raw JSON response from ESPN boxscore API
#' @param event_id Event ID used in the request
#' @return Data frame with player statistics
#' @keywords internal
create_nhl_player_stats_dataset <- function(data, event_id) {
  # Initialize player stats data frame
  player_stats_df <- data.frame(
    event_id = character(0),
    team_id = character(0),
    team_abbreviation = character(0),
    team_display_name = character(0),
    team_home_away = character(0),
    player_id = character(0),
    player_display_name = character(0),
    player_short_name = character(0),
    player_jersey = character(0),
    player_position = character(0),
    player_active = character(0),
    player_scratched = character(0),
    stat_category = character(0),
    # All possible NHL stats based on the structure
    blocked_shots = character(0),
    hits = character(0),
    takeaways = character(0),
    plus_minus = character(0),
    goals = character(0),
    assists = character(0),
    points = character(0),
    penalty_minutes = character(0),
    shots_on_goal = character(0),
    time_on_ice = character(0),
    power_play_goals = character(0),
    power_play_assists = character(0),
    short_handed_goals = character(0),
    short_handed_assists = character(0),
    game_winning_goals = character(0),
    overtime_goals = character(0),
    shooting_percentage = character(0),
    faceoffs_won = character(0),
    faceoffs_lost = character(0),
    faceoff_percentage = character(0),
    giveaways = character(0),
    # Goalie stats
    goals_against = character(0),
    shots_against = character(0),
    shootout_saves = character(0),
    shootout_shots_against = character(0),
    saves = character(0),
    save_percentage = character(0),
    shutouts = character(0),
    wins = character(0),
    losses = character(0),
    overtime_losses = character(0),
    goals_against_average = character(0),
    minutes_played = character(0),
    stringsAsFactors = FALSE
  )

  # Extract boxscore data
  boxscore <- extract_nested_nhl_boxscore(data, c("boxscore"), list())
  if (length(boxscore) == 0) {
    warning("No boxscore data found")
    return(player_stats_df)
  }

  # Extract players data from the boxscore structure
  players_data <- extract_nested_nhl_boxscore(boxscore, c("players"), list())
  if (length(players_data) == 0 || !is.data.frame(players_data)) {
    warning("No players data found in boxscore")
    return(player_stats_df)
  }

  # Process each team's players
  for (team_idx in seq_len(nrow(players_data))) {
    # Extract team information
    if (is.data.frame(players_data$team) && nrow(players_data$team) >= team_idx) {
      team_id <- players_data$team$id[team_idx]
      team_abbreviation <- players_data$team$abbreviation[team_idx]
      team_display_name <- players_data$team$displayName[team_idx]
    } else {
      team_id <- NA_character_
      team_abbreviation <- NA_character_
      team_display_name <- NA_character_
    }

    # Determine home/away (first team is away, second is home)
    team_home_away <- if (team_idx == 1) "away" else "home"

    # Extract statistics for this team
    team_stats_list <- players_data$statistics[[team_idx]]

    # Check if we have statistics data
    if (is.data.frame(team_stats_list) && nrow(team_stats_list) > 0) {
      # Process each stat category (forwards, defenses, goalies)
      for (stat_category_idx in seq_len(nrow(team_stats_list))) {
        stat_category_row <- team_stats_list[stat_category_idx, ]
        stat_category <- stat_category_row$name

        # Get the stat keys, labels, and descriptions for this category
        stat_keys <- stat_category_row$keys[[1]]
        stat_labels <- stat_category_row$labels[[1]]
        stat_descriptions <- stat_category_row$descriptions[[1]]

        # Extract athletes data for this stat category
        athletes_list <- stat_category_row$athletes[[1]]
        if (is.data.frame(athletes_list) && nrow(athletes_list) > 0) {
          # Process each player
          for (player_idx in seq_len(nrow(athletes_list))) {
            athlete_row <- athletes_list[player_idx, ]

            # Extract player information - athlete is already a data frame row
            athlete_info <- athlete_row$athlete
            if (is.data.frame(athlete_info) && nrow(athlete_info) > 0) {
              # Get first row of athlete info
              player_id <- as.character(athlete_info$id[1])
              player_display_name <- as.character(athlete_info$displayName[1])
              player_short_name <- as.character(athlete_info$shortName[1])
              player_jersey <- as.character(athlete_info$jersey[1])

              # Position information - handle nested data frame
              position_data <- athlete_info$position
              if (is.data.frame(position_data) && nrow(position_data) > 0) {
                player_position <- as.character(position_data$abbreviation[1])
              } else {
                player_position <- NA_character_
              }
            } else {
              player_id <- NA_character_
              player_display_name <- NA_character_
              player_short_name <- NA_character_
              player_jersey <- NA_character_
              player_position <- NA_character_
            }

            # Player game status - safely extract from athlete_row
            player_active <- if ("active" %in% names(athlete_row)) {
              as.character(athlete_row$active)
            } else {
              "TRUE"
            }
            player_scratched <- if ("scratched" %in% names(athlete_row)) {
              as.character(athlete_row$scratched)
            } else {
              "FALSE"
            }

            # Extract player statistics - stats is a list element containing character vector
            stats_vector <- NULL
            if ("stats" %in% names(athlete_row) && !is.null(athlete_row$stats)) {
              if (is.list(athlete_row$stats) && length(athlete_row$stats) > 0) {
                stats_vector <- athlete_row$stats[[1]]
              } else if (is.character(athlete_row$stats)) {
                stats_vector <- athlete_row$stats
              }
            }

            # Initialize all stats with NA
            stat_values <- list(
              blocked_shots = NA_character_, hits = NA_character_, takeaways = NA_character_,
              plus_minus = NA_character_, goals = NA_character_, assists = NA_character_,
              points = NA_character_, penalty_minutes = NA_character_, shots_on_goal = NA_character_,
              time_on_ice = NA_character_, power_play_goals = NA_character_, power_play_assists = NA_character_,
              short_handed_goals = NA_character_, short_handed_assists = NA_character_,
              game_winning_goals = NA_character_, overtime_goals = NA_character_,
              shooting_percentage = NA_character_, faceoffs_won = NA_character_,
              faceoffs_lost = NA_character_, faceoff_percentage = NA_character_, giveaways = NA_character_,
              goals_against = NA_character_, shots_against = NA_character_, shootout_saves = NA_character_,
              shootout_shots_against = NA_character_, saves = NA_character_, save_percentage = NA_character_,
              shutouts = NA_character_, wins = NA_character_, losses = NA_character_,
              overtime_losses = NA_character_, goals_against_average = NA_character_, minutes_played = NA_character_
            )

            if (is.character(stats_vector) && length(stats_vector) > 0 && length(stat_keys) > 0) {
              # Safe stat extraction with key mapping
              for (i in seq_along(stat_keys)) {
                if (i <= length(stats_vector)) {
                  stat_key <- stat_keys[i]
                  stat_value <- as.character(stats_vector[i])

                  # Map common stat keys to our column names
                  column_name <- switch(stat_key,
                                        "blockedShots" = "blocked_shots",
                                        "hits" = "hits",
                                        "takeaways" = "takeaways",
                                        "plusMinus" = "plus_minus",
                                        "goals" = "goals",
                                        "assists" = "assists",
                                        "points" = "points",
                                        "penaltyMinutes" = "penalty_minutes",
                                        "shotsOnGoal" = "shots_on_goal",
                                        "timeOnIce" = "time_on_ice",
                                        "powerPlayGoals" = "power_play_goals",
                                        "powerPlayAssists" = "power_play_assists",
                                        "shortHandedGoals" = "short_handed_goals",
                                        "shortHandedAssists" = "short_handed_assists",
                                        "gameWinningGoals" = "game_winning_goals",
                                        "overtimeGoals" = "overtime_goals",
                                        "shootingPercentage" = "shooting_percentage",
                                        "faceoffsWon" = "faceoffs_won",
                                        "faceoffsLost" = "faceoffs_lost",
                                        "faceoffPercentage" = "faceoff_percentage",
                                        "giveaways" = "giveaways",
                                        "goalsAgainst" = "goals_against",
                                        "shotsAgainst" = "shots_against",
                                        "shootoutSaves" = "shootout_saves",
                                        "shootoutShotsAgainst" = "shootout_shots_against",
                                        "saves" = "saves",
                                        "savePercentage" = "save_percentage",
                                        "shutouts" = "shutouts",
                                        "wins" = "wins",
                                        "losses" = "losses",
                                        "overtimeLosses" = "overtime_losses",
                                        "goalsAgainstAverage" = "goals_against_average",
                                        "minutesPlayed" = "minutes_played",
                                        NA_character_
                  )

                  if (!is.na(column_name) && column_name %in% names(stat_values)) {
                    stat_values[[column_name]] <- stat_value
                  }
                }
              }
            }

            # Create player stats row
            player_row <- data.frame(
              event_id = as.character(event_id),
              team_id = as.character(team_id),
              team_abbreviation = as.character(team_abbreviation),
              team_display_name = as.character(team_display_name),
              team_home_away = as.character(team_home_away),
              player_id = as.character(player_id),
              player_display_name = as.character(player_display_name),
              player_short_name = as.character(player_short_name),
              player_jersey = as.character(player_jersey),
              player_position = as.character(player_position),
              player_active = as.character(player_active),
              player_scratched = as.character(player_scratched),
              stat_category = as.character(stat_category),
              # Map all stat values
              blocked_shots = as.character(stat_values$blocked_shots),
              hits = as.character(stat_values$hits),
              takeaways = as.character(stat_values$takeaways),
              plus_minus = as.character(stat_values$plus_minus),
              goals = as.character(stat_values$goals),
              assists = as.character(stat_values$assists),
              points = as.character(stat_values$points),
              penalty_minutes = as.character(stat_values$penalty_minutes),
              shots_on_goal = as.character(stat_values$shots_on_goal),
              time_on_ice = as.character(stat_values$time_on_ice),
              power_play_goals = as.character(stat_values$power_play_goals),
              power_play_assists = as.character(stat_values$power_play_assists),
              short_handed_goals = as.character(stat_values$short_handed_goals),
              short_handed_assists = as.character(stat_values$short_handed_assists),
              game_winning_goals = as.character(stat_values$game_winning_goals),
              overtime_goals = as.character(stat_values$overtime_goals),
              shooting_percentage = as.character(stat_values$shooting_percentage),
              faceoffs_won = as.character(stat_values$faceoffs_won),
              faceoffs_lost = as.character(stat_values$faceoffs_lost),
              faceoff_percentage = as.character(stat_values$faceoff_percentage),
              giveaways = as.character(stat_values$giveaways),
              goals_against = as.character(stat_values$goals_against),
              shots_against = as.character(stat_values$shots_against),
              shootout_saves = as.character(stat_values$shootout_saves),
              shootout_shots_against = as.character(stat_values$shootout_shots_against),
              saves = as.character(stat_values$saves),
              save_percentage = as.character(stat_values$save_percentage),
              shutouts = as.character(stat_values$shutouts),
              wins = as.character(stat_values$wins),
              losses = as.character(stat_values$losses),
              overtime_losses = as.character(stat_values$overtime_losses),
              goals_against_average = as.character(stat_values$goals_against_average),
              minutes_played = as.character(stat_values$minutes_played),
              stringsAsFactors = FALSE
            )

            player_stats_df <- rbind(player_stats_df, player_row)
          }
        }
      }
    }
  }

  # Clean up row names
  if (nrow(player_stats_df) > 0) {
    rownames(player_stats_df) <- NULL
  }

  return(player_stats_df)
}

#' Create NHL team statistics data frame from ESPN boxscore API
#'
#' Processes team-level statistics from ESPN boxscore teams data
#' @param data Raw JSON response from ESPN boxscore API
#' @param event_id Event ID used in the request
#' @return Data frame with team statistics
#' @keywords internal
create_nhl_team_stats_dataset <- function(data, event_id) {
  # Initialize team stats data frame
  team_stats_df <- data.frame(
    event_id = character(0),
    team_id = character(0),
    team_abbreviation = character(0),
    team_display_name = character(0),
    team_home_away = character(0),
    team_score = character(0),
    stat_name = character(0),
    stat_abbreviation = character(0),
    stat_value = character(0),
    stat_label = character(0),
    stringsAsFactors = FALSE
  )

  # Extract boxscore data
  boxscore <- extract_nested_nhl_boxscore(data, c("boxscore"), list())
  if (length(boxscore) == 0) {
    warning("No boxscore data found")
    return(team_stats_df)
  }

  # Extract teams data from the boxscore structure
  teams_data <- extract_nested_nhl_boxscore(boxscore, c("teams"), list())
  if (length(teams_data) == 0 || !is.data.frame(teams_data)) {
    warning("No teams data found in boxscore")
    return(team_stats_df)
  }

  # Process each team's statistics
  for (team_idx in seq_len(nrow(teams_data))) {
    # Extract team information
    if (is.data.frame(teams_data$team) && nrow(teams_data$team) >= team_idx) {
      team_id <- teams_data$team$id[team_idx]
      team_abbreviation <- teams_data$team$abbreviation[team_idx]
      team_display_name <- teams_data$team$displayName[team_idx]
    } else {
      team_id <- NA_character_
      team_abbreviation <- NA_character_
      team_display_name <- NA_character_
    }

    team_home_away <- teams_data$homeAway[team_idx]

    # Get team score from header
    team_score <- NA_character_
    header <- extract_nested_nhl_boxscore(data, c("header"), list())
    if (length(header) > 0) {
      competitions <- extract_nested_nhl_boxscore(header, c("competitions"), list())
      if (length(competitions) > 0) {
        competitors <- extract_nested_nhl_boxscore(competitions[[1]], c("competitors"), list())
        if (length(competitors) > 0) {
          for (comp in competitors) {
            comp_team <- extract_nested_nhl_boxscore(comp, c("team"), list())
            if (extract_nested_nhl_boxscore(comp_team, c("id"), "") == team_id) {
              team_score <- extract_nested_nhl_boxscore(comp, c("score"), NA_character_)
              break
            }
          }
        }
      }
    }

    # Extract team statistics from teams statistics
    if (length(teams_data$statistics) >= team_idx && is.data.frame(teams_data$statistics[[team_idx]])) {
      team_stats_raw <- teams_data$statistics[[team_idx]]
      for (stat_idx in seq_len(nrow(team_stats_raw))) {
        stat_row <- team_stats_raw[stat_idx, ]

        # Create team stats row for each statistic
        team_row <- data.frame(
          event_id = as.character(event_id),
          team_id = as.character(team_id),
          team_abbreviation = as.character(team_abbreviation),
          team_display_name = as.character(team_display_name),
          team_home_away = as.character(team_home_away),
          team_score = as.character(team_score),
          stat_name = as.character(stat_row$name),
          stat_abbreviation = as.character(stat_row$abbreviation),
          stat_value = as.character(stat_row$displayValue),
          stat_label = as.character(stat_row$label),
          stringsAsFactors = FALSE
        )

        team_stats_df <- rbind(team_stats_df, team_row)
      }
    }
  }

  # Clean up row names
  if (nrow(team_stats_df) > 0) {
    rownames(team_stats_df) <- NULL
  }

  return(team_stats_df)
}

#' Fetch detailed NHL boxscore using ESPN API
#'
#' Retrieves comprehensive boxscore information for a specific NHL game from ESPN's API.
#' Assigns player statistics and team statistics to global environment.
#'
#' @param event_id Character or numeric. ESPN event/game ID
#'   (e.g., "401589281" for a specific NHL game).
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'nhl_boxscore_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns a list containing the datasets. The main purpose is global
#'   environment assignment of:
#'   \itemize{
#'     \item \code{nhl_player_stats}: Individual player statistics for both teams
#'     \item \code{nhl_team_stats}: Team-level statistics and totals
#'   }
#'
#' @details
#' The function creates comprehensive datasets from ESPN's NHL boxscore API.
#' **Player Statistics** (\code{nhl_player_stats}):
#' \itemize{
#'   \item Skater stats: goals, assists, points, +/-, PIM, shots, PPG, PPA, SHG, SHA, GWG, OTG, S%, FOW, FOL, FO%, TOI, hits, blocked shots, takeaways, giveaways
#'   \item Goalie stats: wins, losses, OTL, shots against, goals against, GAA, saves, SV%, shutouts, minutes, shootout saves/shots against
#'   \item Player details: name, position, jersey number, active/scratched status
#' }
#'
#' **Team Statistics** (\code{nhl_team_stats}):
#' \itemize{
#'   \item Team totals for blocked shots, hits, takeaways, shots, power play stats, penalty kill stats, faceoffs, etc.
#'   \item Final scores and game outcome data
#' }
#'
#' **Statistical Categories**:
#' Players are organized by position groups (forwards, defenses, goalies) with appropriate stats for each category.
#'
#' @examples
#' \dontrun{
#' # Get boxscore for a specific game
#' fetch_nhl_boxscore("401589281")
#'
#' # Access player statistics
#' head(nhl_player_stats)
#'
#' # View forwards only
#' forwards <- nhl_player_stats[nhl_player_stats$stat_category == "forwards", ]
#' head(forwards[, c("player_display_name", "goals", "assists", "points", "plus_minus")])
#'
#' # View goalies only
#' goalies <- nhl_player_stats[nhl_player_stats$stat_category == "goalies", ]
#' head(goalies[, c("player_display_name", "wins", "saves", "save_percentage", "goals_against_average")])
#'
#' # Access team statistics
#' head(nhl_team_stats)
#'
#' # Find top scorers
#' top_scorers <- nhl_player_stats[
#'   nhl_player_stats$stat_category %in% c("forwards", "defenses") &
#'   !is.na(nhl_player_stats$points) &
#'   nhl_player_stats$points != "" &
#'   nhl_player_stats$points != "0",
#' ]
#' if(nrow(top_scorers) > 0) {
#'   top_scorers <- top_scorers[order(-as.numeric(top_scorers$points)), ]
#'   head(top_scorers[, c("player_display_name", "team_abbreviation", "goals", "assists", "points")])
#' }
#'
#' # Analyze hits and blocked shots
#' physical_stats <- nhl_player_stats[
#'   !is.na(nhl_player_stats$hits) &
#'   nhl_player_stats$hits != "" &
#'   as.numeric(nhl_player_stats$hits) > 0,
#' ]
#' }
#'
#' @importFrom httr GET status_code content timeout
#' @export
fetch_nhl_boxscore <- function(event_id, raw = FALSE) {
  # Input validation
  if (missing(event_id) || is.null(event_id) || event_id == "") {
    stop("event_id must be provided and cannot be empty")
  }

  if (!is.logical(raw)) {
    stop("raw parameter must be logical (TRUE or FALSE)")
  }

  # Construct boxscore API URL for NHL
  url <- paste0("https://site.api.espn.com/apis/site/v2/sports/hockey/nhl/summary?event=", event_id)

  message("Fetching NHL boxscore for game ", event_id, "...")

  # Make HTTP request with error handling
  resp <- tryCatch({
    httr::GET(url, httr::timeout(30))
  }, error = function(e) {
    stop("Failed to connect to ESPN API: ", e$message)
  })

  if (httr::status_code(resp) != 200) {
    stop("Failed to fetch boxscore data. Status code: ", httr::status_code(resp))
  }

  # Parse JSON response
  data <- tryCatch({
    httr::content(resp, as = "parsed", simplifyVector = TRUE)
  }, error = function(e) {
    stop("Failed to parse JSON response: ", e$message)
  })

  if (isTRUE(raw)) {
    assign("nhl_boxscore_raw", data, envir = .GlobalEnv)
    message("Raw NHL boxscore data assigned to: nhl_boxscore_raw")

    # Show structure for debugging
    if ("boxscore" %in% names(data)) {
      boxscore_sections <- names(data$boxscore)
      message("Boxscore sections: ", paste(boxscore_sections, collapse = ", "))
      if ("players" %in% names(data$boxscore)) {
        message("Players data type: ", class(data$boxscore$players))
        if (is.data.frame(data$boxscore$players)) {
          message("Players data frame rows: ", nrow(data$boxscore$players))
          message("Players data frame cols: ", paste(names(data$boxscore$players), collapse = ", "))
        }
      }
    }
    return(invisible(data))
  }

  # Create player statistics dataset
  message("Processing player statistics...")
  player_stats <- tryCatch({
    create_nhl_player_stats_dataset(data, event_id)
  }, error = function(e) {
    warning("Error processing player statistics: ", e$message)
    data.frame()
  })

  # Assign to global environment
  assign("nhl_player_stats", player_stats, envir = .GlobalEnv)

  # Create team statistics dataset
  message("Processing team statistics...")
  team_stats <- tryCatch({
    create_nhl_team_stats_dataset(data, event_id)
  }, error = function(e) {
    warning("Error processing team statistics: ", e$message)
    data.frame()
  })

  # Assign to global environment
  assign("nhl_team_stats", team_stats, envir = .GlobalEnv)

  message("Players processed: ", nrow(player_stats))
  message("Teams processed: ", nrow(team_stats))

  # Show sample stats if available
  if (nrow(player_stats) > 0) {
    # Show breakdown by category
    forwards_count <- sum(player_stats$stat_category == "forwards", na.rm = TRUE)
    defenses_count <- sum(player_stats$stat_category == "defenses", na.rm = TRUE)
    goalies_count <- sum(player_stats$stat_category == "goalies", na.rm = TRUE)
    message("  - Forwards: ", forwards_count)
    message("  - Defenses: ", defenses_count)
    message("  - Goalies: ", goalies_count)

    # Show top performers
    skater_stats <- player_stats[player_stats$stat_category %in% c("forwards", "defenses") &
                                   !is.na(player_stats$points) & player_stats$points != "" & player_stats$points != "0", ]
    if (nrow(skater_stats) > 0) {
      skater_stats$points_numeric <- suppressWarnings(as.numeric(skater_stats$points))
      skater_stats <- skater_stats[!is.na(skater_stats$points_numeric), ]
      if (nrow(skater_stats) > 0) {
        top_scorer <- skater_stats[which.max(skater_stats$points_numeric), ]
        message("  Top scorer: ", top_scorer$player_display_name, " (", top_scorer$team_abbreviation,
                ") - ", top_scorer$points, " points (", top_scorer$goals, "G, ", top_scorer$assists, "A)")
      }
    }

    # Show goalie performance
    goalie_stats <- player_stats[player_stats$stat_category == "goalies" &
                                   !is.na(player_stats$saves) & player_stats$saves != "", ]
    if (nrow(goalie_stats) > 0) {
      for (g in seq_len(nrow(goalie_stats))) {
        goalie <- goalie_stats[g, ]
        message("  Goalie: ", goalie$player_display_name, " (", goalie$team_abbreviation,
                ") - ", goalie$saves, " saves")
        if (!is.na(goalie$save_percentage) && goalie$save_percentage != "") {
          message("    SV%: ", goalie$save_percentage)
        }
      }
    }
  }

  result <- list(
    player_stats = player_stats,
    team_stats = team_stats
  )

  invisible(result)
}
