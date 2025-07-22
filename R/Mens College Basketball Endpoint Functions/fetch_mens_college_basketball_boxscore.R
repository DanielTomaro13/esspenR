#' Safe nested data extraction helper function for mens_college_basketball boxscore data
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

#' Create player statistics data frame from ESPN boxscore API
#'
#' Processes player statistics from ESPN boxscore players data
#' @param data Raw JSON response from ESPN boxscore API
#' @param event_id Event ID used in the request
#' @return Data frame with player statistics
#' @keywords internal
create_player_stats_dataset <- function(data, event_id) {
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
    player_starter = character(0),
    player_played = character(0),
    player_reason = character(0),
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
    plus_minus = character(0),
    stringsAsFactors = FALSE
  )

  # Extract boxscore data
  boxscore <- extract_nested_boxscore(data, c("boxscore"), list())

  if (length(boxscore) == 0) {
    warning("No boxscore data found")
    return(player_stats_df)
  }

  # Extract players data from the boxscore structure
  players_data <- extract_nested_boxscore(boxscore, c("players"), list())

  if (length(players_data) == 0 || !is.data.frame(players_data)) {
    warning("No players data found in boxscore")
    return(player_stats_df)
  }

  # Process each team's players
  for (i in seq_len(nrow(players_data))) {
    # Extract team information - fix the team data extraction
    if (is.data.frame(players_data$team) && nrow(players_data$team) >= i) {
      team_id <- players_data$team$id[i]
      team_abbreviation <- players_data$team$abbreviation[i]
      team_display_name <- players_data$team$displayName[i]
    } else {
      team_id <- NA_character_
      team_abbreviation <- NA_character_
      team_display_name <- NA_character_
    }

    # Determine home/away from the teams order (first team is away, second is home)
    team_home_away <- if (i == 1) "away" else "home"

    # Extract statistics for this team
    team_stats_list <- players_data$statistics[[i]]

    # Check if we have statistics data
    if (is.data.frame(team_stats_list) && nrow(team_stats_list) > 0) {
      # Get the first (and typically only) statistics group
      stat_group <- team_stats_list[1, ]

      # Extract athletes data
      athletes_list <- extract_nested_boxscore(stat_group, c("athletes"), list())

      if (length(athletes_list) > 0 && is.data.frame(athletes_list[[1]])) {
        athletes_df <- athletes_list[[1]]

        # Process each player
        for (j in seq_len(nrow(athletes_df))) {
          athlete_row <- athletes_df[j, ]

          # Extract player information - fix the athlete data extraction
          if (is.data.frame(athlete_row$athlete) && nrow(athlete_row$athlete) > 0) {
            athlete_info <- athlete_row$athlete[1, ]
            player_id <- athlete_info$id
            player_display_name <- athlete_info$displayName
            player_short_name <- athlete_info$shortName
            player_jersey <- athlete_info$jersey

            # Position information
            if (is.data.frame(athlete_info$position) && nrow(athlete_info$position) > 0) {
              player_position <- athlete_info$position$abbreviation[1]
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

          # Player game status - handle NULL values properly
          player_starter <- if (is.null(athlete_row$starter)) "FALSE" else as.character(athlete_row$starter)
          player_played <- if (is.null(athlete_row$didNotPlay) || !athlete_row$didNotPlay) "TRUE" else "FALSE"
          player_reason <- if (is.null(athlete_row$reason)) NA_character_ else as.character(athlete_row$reason)

          # Extract player statistics
          stats_vector <- athlete_row$stats[[1]]

          # Initialize stats variables with NA
          minutes <- NA_character_
          field_goals <- NA_character_
          field_goal_pct <- NA_character_
          three_pointers <- NA_character_
          three_point_pct <- NA_character_
          free_throws <- NA_character_
          free_throw_pct <- NA_character_
          total_rebounds <- NA_character_
          assists <- NA_character_
          blocks <- NA_character_
          steals <- NA_character_
          personal_fouls <- NA_character_
          turnovers <- NA_character_
          points <- NA_character_
          plus_minus <- NA_character_

          if (length(stats_vector) >= 14) {
            # Safe stat extraction
            safe_stat <- function(index, default = NA_character_) {
              if (length(stats_vector) >= index && !is.na(stats_vector[index]) &&
                  stats_vector[index] != "") {
                return(as.character(stats_vector[index]))
              }
              return(default)
            }

            # Map ESPN statistics based on order from structure
            minutes <- safe_stat(1)
            field_goals <- safe_stat(2)
            three_pointers <- safe_stat(3)
            free_throws <- safe_stat(4)
            total_rebounds <- safe_stat(5)
            assists <- safe_stat(6)
            blocks <- safe_stat(7)
            steals <- safe_stat(8)
            personal_fouls <- safe_stat(9)
            turnovers <- safe_stat(10)
            points <- safe_stat(11)
            plus_minus <- safe_stat(12)

            # Calculate percentages from made/attempted if available
            if (!is.na(field_goals) && field_goals != "") {
              fg_parts <- strsplit(field_goals, "-")[[1]]
              if (length(fg_parts) == 2) {
                made <- suppressWarnings(as.numeric(fg_parts[1]))
                attempted <- suppressWarnings(as.numeric(fg_parts[2]))
                if (!is.na(made) && !is.na(attempted) && attempted > 0) {
                  field_goal_pct <- as.character(round((made / attempted) * 100, 1))
                }
              }
            }

            if (!is.na(three_pointers) && three_pointers != "") {
              tp_parts <- strsplit(three_pointers, "-")[[1]]
              if (length(tp_parts) == 2) {
                made <- suppressWarnings(as.numeric(tp_parts[1]))
                attempted <- suppressWarnings(as.numeric(tp_parts[2]))
                if (!is.na(made) && !is.na(attempted) && attempted > 0) {
                  three_point_pct <- as.character(round((made / attempted) * 100, 1))
                }
              }
            }

            if (!is.na(free_throws) && free_throws != "") {
              ft_parts <- strsplit(free_throws, "-")[[1]]
              if (length(ft_parts) == 2) {
                made <- suppressWarnings(as.numeric(ft_parts[1]))
                attempted <- suppressWarnings(as.numeric(ft_parts[2]))
                if (!is.na(made) && !is.na(attempted) && attempted > 0) {
                  free_throw_pct <- as.character(round((made / attempted) * 100, 1))
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
            player_starter = as.character(player_starter),
            player_played = as.character(player_played),
            player_reason = as.character(player_reason),
            minutes = as.character(minutes),
            field_goals = as.character(field_goals),
            field_goal_pct = as.character(field_goal_pct),
            three_pointers = as.character(three_pointers),
            three_point_pct = as.character(three_point_pct),
            free_throws = as.character(free_throws),
            free_throw_pct = as.character(free_throw_pct),
            total_rebounds = as.character(total_rebounds),
            assists = as.character(assists),
            blocks = as.character(blocks),
            steals = as.character(steals),
            personal_fouls = as.character(personal_fouls),
            turnovers = as.character(turnovers),
            points = as.character(points),
            plus_minus = as.character(plus_minus),
            stringsAsFactors = FALSE
          )

          player_stats_df <- rbind(player_stats_df, player_row)
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

#' Create team statistics data frame from ESPN boxscore API
#'
#' Processes team-level statistics from ESPN boxscore teams data
#' @param data Raw JSON response from ESPN boxscore API
#' @param event_id Event ID used in the request
#' @return Data frame with team statistics
#' @keywords internal
create_team_stats_dataset <- function(data, event_id) {
  # Initialize team stats data frame
  team_stats_df <- data.frame(
    event_id = character(0),
    team_id = character(0),
    team_abbreviation = character(0),
    team_display_name = character(0),
    team_home_away = character(0),
    team_score = character(0),
    stat_group = character(0),
    stat_group_display_name = character(0),
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

  # Extract boxscore data
  boxscore <- extract_nested_boxscore(data, c("boxscore"), list())

  if (length(boxscore) == 0) {
    warning("No boxscore data found")
    return(team_stats_df)
  }

  # Extract teams data from the boxscore structure
  teams_data <- extract_nested_boxscore(boxscore, c("teams"), list())

  if (length(teams_data) == 0 || !is.data.frame(teams_data)) {
    warning("No teams data found in boxscore")
    return(team_stats_df)
  }

  # Process each team's statistics
  for (i in seq_len(nrow(teams_data))) {
    # Extract team information - fix the team data extraction
    if (is.data.frame(teams_data$team) && nrow(teams_data$team) >= i) {
      team_id <- teams_data$team$id[i]
      team_abbreviation <- teams_data$team$abbreviation[i]
      team_display_name <- teams_data$team$displayName[i]
    } else {
      team_id <- NA_character_
      team_abbreviation <- NA_character_
      team_display_name <- NA_character_
    }

    team_home_away <- teams_data$homeAway[i]

    # Get team score from header
    team_score <- NA_character_
    header <- extract_nested_boxscore(data, c("header"), list())
    if (length(header) > 0) {
      competitions <- extract_nested_boxscore(header, c("competitions"), list())
      if (length(competitions) > 0) {
        competitors <- extract_nested_boxscore(competitions[[1]], c("competitors"), list())
        if (length(competitors) > 0) {
          for (comp in competitors) {
            comp_team <- extract_nested_boxscore(comp, c("team"), list())
            if (extract_nested_boxscore(comp_team, c("id"), "") == team_id) {
              team_score <- extract_nested_boxscore(comp, c("score"), NA_character_)
              break
            }
          }
        }
      }
    }

    # Extract team statistics - fix the statistics extraction
    # The statistics field contains a data frame for this team
    if (length(teams_data$statistics) >= i && is.data.frame(teams_data$statistics[[i]])) {
      team_stats_df_raw <- teams_data$statistics[[i]]

      for (j in seq_len(nrow(team_stats_df_raw))) {
        # Access the row directly from the data frame
        stat_name <- team_stats_df_raw$name[j]
        stat_value <- team_stats_df_raw$displayValue[j]
        stat_label <- team_stats_df_raw$label[j]

        # Initialize team stats for this specific stat
        field_goals <- NA_character_
        field_goal_pct <- NA_character_
        three_pointers <- NA_character_
        three_point_pct <- NA_character_
        free_throws <- NA_character_
        free_throw_pct <- NA_character_
        total_rebounds <- NA_character_
        assists <- NA_character_
        blocks <- NA_character_
        steals <- NA_character_
        personal_fouls <- NA_character_
        turnovers <- NA_character_
        points <- NA_character_

        # Map the statistics based on the name field and assign to correct variable
        if (stat_name == "fieldGoalsMade-fieldGoalsAttempted") {
          field_goals <- stat_value
        } else if (stat_name == "fieldGoalPct") {
          field_goal_pct <- stat_value
        } else if (stat_name == "threePointFieldGoalsMade-threePointFieldGoalsAttempted") {
          three_pointers <- stat_value
        } else if (stat_name == "threePointFieldGoalPct") {
          three_point_pct <- stat_value
        } else if (stat_name == "freeThrowsMade-freeThrowsAttempted") {
          free_throws <- stat_value
        } else if (stat_name == "freeThrowPct") {
          free_throw_pct <- stat_value
        } else if (stat_name == "totalRebounds") {
          total_rebounds <- stat_value
        } else if (stat_name == "assists") {
          assists <- stat_value
        } else if (stat_name == "blocks") {
          blocks <- stat_value
        } else if (stat_name == "steals") {
          steals <- stat_value
        } else if (stat_name == "fouls") {
          personal_fouls <- stat_value
        } else if (stat_name == "turnovers") {
          turnovers <- stat_value
        } else if (stat_name == "points") {
          points <- stat_value
        }

        # Create team stats row for each statistic
        team_row <- data.frame(
          event_id = as.character(event_id),
          team_id = as.character(team_id),
          team_abbreviation = as.character(team_abbreviation),
          team_display_name = as.character(team_display_name),
          team_home_away = as.character(team_home_away),
          team_score = as.character(team_score),
          stat_group = as.character(stat_name),
          stat_group_display_name = as.character(stat_label),
          field_goals = as.character(field_goals),
          field_goal_pct = as.character(field_goal_pct),
          three_pointers = as.character(three_pointers),
          three_point_pct = as.character(three_point_pct),
          free_throws = as.character(free_throws),
          free_throw_pct = as.character(free_throw_pct),
          total_rebounds = as.character(total_rebounds),
          assists = as.character(assists),
          blocks = as.character(blocks),
          steals = as.character(steals),
          personal_fouls = as.character(personal_fouls),
          turnovers = as.character(turnovers),
          points = as.character(points),
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

#' Fetch detailed mens_college_basketball boxscore using ESPN API
#'
#' Retrieves comprehensive boxscore information for a specific mens_college_basketball game from ESPN's API.
#' Assigns player statistics and team statistics to global environment.
#'
#' @param event_id Character or numeric. ESPN event/game ID
#'   (e.g., "401768050" for a specific mens_college_basketball game).
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'mens_college_basketball_boxscore_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns a list containing the datasets. The main purpose is global
#'   environment assignment of:
#'   \itemize{
#'     \item \code{mens_college_basketball_player_stats}: Individual player statistics for both teams
#'     \item \code{mens_college_basketball_team_stats}: Team-level statistics and totals
#'   }
#'
#' @details
#' Statistical categories include minutes, field goals, 3-pointers, free throws,
#' rebounds, assists, steals, blocks, turnovers, personal fouls, points, and plus/minus.
#'
#' @examples
#' \dontrun{
#' # Get boxscore for a specific game
#' fetch_mens_college_basketball_boxscore("401768050")
#'
#' # Access player statistics
#' head(mens_college_basketball_player_stats)
#'
#' # Access team statistics
#' head(mens_college_basketball_team_stats)
#' }
#'
#' @importFrom httr GET status_code content timeout
#' @export
fetch_mens_college_basketball_boxscore <- function(event_id, raw = FALSE) {
  # Input validation
  if (missing(event_id) || is.null(event_id) || event_id == "") {
    stop("event_id must be provided and cannot be empty")
  }

  if (!is.logical(raw)) {
    stop("raw parameter must be logical (TRUE or FALSE)")
  }

  # Construct boxscore API URL
  url <- paste0("https://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/summary?event=", event_id)

  message("Fetching boxscore for game ", event_id, "...")

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
    assign("mens_college_basketball_boxscore_raw", data, envir = .GlobalEnv)
  }

  # Create player statistics dataset
  message("Processing player statistics...")
  player_stats <- tryCatch({
    create_player_stats_dataset(data, event_id)
  }, error = function(e) {
    warning("Error processing player statistics: ", e$message)
    data.frame()
  })

  # Assign to global environment
  assign("mens_college_basketball_player_stats", player_stats, envir = .GlobalEnv)

  # Create team statistics dataset
  message("Processing team statistics...")
  team_stats <- tryCatch({
    create_team_stats_dataset(data, event_id)
  }, error = function(e) {
    warning("Error processing team statistics: ", e$message)
    data.frame()
  })

  # Assign to global environment
  assign("mens_college_basketball_team_stats", team_stats, envir = .GlobalEnv)

  message("Players processed: ", nrow(player_stats))
  message("Teams processed: ", nrow(team_stats))

  result <- list(
    player_stats = player_stats,
    team_stats = team_stats
  )

  invisible(result)
}
