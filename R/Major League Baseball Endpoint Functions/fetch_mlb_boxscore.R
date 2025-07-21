#' Safe nested data extraction helper function for MLB boxscore data
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_mlb_boxscore <- function(data, path, default = NA) {
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

#' Create MLB player statistics data frame from ESPN boxscore API
#'
#' Processes player statistics from ESPN boxscore players data
#' @param data Raw JSON response from ESPN boxscore API
#' @param event_id Event ID used in the request
#' @return Data frame with player statistics
#' @keywords internal
create_mlb_player_stats_dataset <- function(data, event_id) {
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
    stat_type = character(0),
    # Batting stats
    at_bats = character(0),
    runs = character(0),
    hits = character(0),
    rbi = character(0),
    walks = character(0),
    strikeouts = character(0),
    avg = character(0),
    obp = character(0),
    slg = character(0),
    ops = character(0),
    # Pitching stats
    innings_pitched = character(0),
    hits_allowed = character(0),
    runs_allowed = character(0),
    earned_runs = character(0),
    walks_allowed = character(0),
    strikeouts_pitched = character(0),
    home_runs_allowed = character(0),
    era = character(0),
    whip = character(0),
    pitches = character(0),
    strikes = character(0),
    stringsAsFactors = FALSE
  )

  # Extract boxscore data
  boxscore <- extract_nested_mlb_boxscore(data, c("boxscore"), list())

  if (length(boxscore) == 0) {
    warning("No boxscore data found")
    return(player_stats_df)
  }

  # Extract players data from the boxscore structure
  players_data <- extract_nested_mlb_boxscore(boxscore, c("players"), list())

  if (length(players_data) == 0 || !is.data.frame(players_data)) {
    warning("No players data found in boxscore")
    return(player_stats_df)
  }

  # Process each team's players
  for (i in seq_len(nrow(players_data))) {
    # Extract team information
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
      # Process each stat type (batting, pitching)
      for (stat_type_idx in seq_len(nrow(team_stats_list))) {
        stat_row <- team_stats_list[stat_type_idx, ]
        stat_type <- stat_row$type

        # Extract athletes data for this stat type
        athletes_list <- stat_row$athletes[[1]]

        if (is.data.frame(athletes_list) && nrow(athletes_list) > 0) {
          # Process each player
          for (j in seq_len(nrow(athletes_list))) {
            athlete_row <- athletes_list[j, ]

            # Extract player information
            if (is.data.frame(athlete_row$athlete) && nrow(athlete_row$athlete) > 0) {
              athlete_info <- athlete_row$athlete[1, ]
              player_id <- athlete_info$id
              player_display_name <- athlete_info$displayName
              player_short_name <- athlete_info$shortName
              player_jersey <- NA_character_  # Not in headshot data

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

            # Player game status
            player_starter <- if (is.null(athlete_row$starter)) "FALSE" else as.character(athlete_row$starter)
            player_played <- if (is.null(athlete_row$active) || athlete_row$active) "TRUE" else "FALSE"

            # Extract player statistics
            stats_vector <- athlete_row$stats[[1]]

            # Initialize all stats variables with NA
            at_bats <- runs <- hits <- rbi <- walks <- strikeouts <- avg <- obp <- slg <- ops <- NA_character_
            innings_pitched <- hits_allowed <- runs_allowed <- earned_runs <- walks_allowed <- NA_character_
            strikeouts_pitched <- home_runs_allowed <- era <- whip <- pitches <- strikes <- NA_character_

            if (length(stats_vector) > 0) {
              # Safe stat extraction
              safe_stat <- function(index, default = NA_character_) {
                if (length(stats_vector) >= index && !is.na(stats_vector[index]) &&
                    stats_vector[index] != "") {
                  return(as.character(stats_vector[index]))
                }
                return(default)
              }

              # Map statistics based on stat type
              if (stat_type == "batting") {
                # Batting statistics order from structure: H-AB, AB, R, H, RBI, BB, SO, AVG, OBP, SLG, OPS, HR
                at_bats <- safe_stat(2)  # AB
                runs <- safe_stat(3)     # R
                hits <- safe_stat(4)     # H
                rbi <- safe_stat(5)      # RBI
                walks <- safe_stat(6)    # BB
                strikeouts <- safe_stat(7) # SO
                avg <- safe_stat(8)      # AVG
                obp <- safe_stat(9)      # OBP
                slg <- safe_stat(10)     # SLG
                ops <- safe_stat(11)     # OPS
              } else if (stat_type == "pitching") {
                # Pitching statistics order: IP, H, R, ER, BB, SO, HR, ERA, WHIP, P
                innings_pitched <- safe_stat(1)    # IP
                hits_allowed <- safe_stat(2)       # H
                runs_allowed <- safe_stat(3)       # R
                earned_runs <- safe_stat(4)        # ER
                walks_allowed <- safe_stat(5)      # BB
                strikeouts_pitched <- safe_stat(6) # SO
                home_runs_allowed <- safe_stat(7)  # HR
                era <- safe_stat(8)                 # ERA
                whip <- safe_stat(9)                # WHIP
                pitches <- safe_stat(10)            # P
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
              stat_type = as.character(stat_type),
              at_bats = as.character(at_bats),
              runs = as.character(runs),
              hits = as.character(hits),
              rbi = as.character(rbi),
              walks = as.character(walks),
              strikeouts = as.character(strikeouts),
              avg = as.character(avg),
              obp = as.character(obp),
              slg = as.character(slg),
              ops = as.character(ops),
              innings_pitched = as.character(innings_pitched),
              hits_allowed = as.character(hits_allowed),
              runs_allowed = as.character(runs_allowed),
              earned_runs = as.character(earned_runs),
              walks_allowed = as.character(walks_allowed),
              strikeouts_pitched = as.character(strikeouts_pitched),
              home_runs_allowed = as.character(home_runs_allowed),
              era = as.character(era),
              whip = as.character(whip),
              pitches = as.character(pitches),
              strikes = as.character(strikes),
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

#' Create MLB team statistics data frame from ESPN boxscore API
#'
#' Processes team-level statistics from ESPN boxscore teams data
#' @param data Raw JSON response from ESPN boxscore API
#' @param event_id Event ID used in the request
#' @return Data frame with team statistics
#' @keywords internal
create_mlb_team_stats_dataset <- function(data, event_id) {
  # Initialize team stats data frame
  team_stats_df <- data.frame(
    event_id = character(0),
    team_id = character(0),
    team_abbreviation = character(0),
    team_display_name = character(0),
    team_home_away = character(0),
    team_score = character(0),
    stat_type = character(0),
    stat_name = character(0),
    stat_value = character(0),
    stringsAsFactors = FALSE
  )

  # Extract boxscore data
  boxscore <- extract_nested_mlb_boxscore(data, c("boxscore"), list())

  if (length(boxscore) == 0) {
    warning("No boxscore data found")
    return(team_stats_df)
  }

  # Extract teams data from the boxscore structure
  teams_data <- extract_nested_mlb_boxscore(boxscore, c("teams"), list())

  if (length(teams_data) == 0 || !is.data.frame(teams_data)) {
    warning("No teams data found in boxscore")
    return(team_stats_df)
  }

  # Process each team's statistics
  for (i in seq_len(nrow(teams_data))) {
    # Extract team information
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
    header <- extract_nested_mlb_boxscore(data, c("header"), list())
    if (length(header) > 0) {
      competitions <- extract_nested_mlb_boxscore(header, c("competitions"), list())
      if (length(competitions) > 0) {
        competitors <- extract_nested_mlb_boxscore(competitions[[1]], c("competitors"), list())
        if (length(competitors) > 0) {
          for (comp in competitors) {
            comp_team <- extract_nested_mlb_boxscore(comp, c("team"), list())
            if (extract_nested_mlb_boxscore(comp_team, c("id"), "") == team_id) {
              team_score <- extract_nested_mlb_boxscore(comp, c("score"), NA_character_)
              break
            }
          }
        }
      }
    }

    # Extract team statistics from teams statistics
    if (length(teams_data$statistics) >= i && is.data.frame(teams_data$statistics[[i]])) {
      team_stats_df_raw <- teams_data$statistics[[i]]

      for (j in seq_len(nrow(team_stats_df_raw))) {
        stat_group <- team_stats_df_raw[j, ]
        stat_type <- stat_group$name

        # Get the stats data frame for this stat type
        stats_data <- stat_group$stats[[1]]

        if (is.data.frame(stats_data) && nrow(stats_data) > 0) {
          for (k in seq_len(nrow(stats_data))) {
            stat_row <- stats_data[k, ]

            # Create team stats row for each statistic
            team_row <- data.frame(
              event_id = as.character(event_id),
              team_id = as.character(team_id),
              team_abbreviation = as.character(team_abbreviation),
              team_display_name = as.character(team_display_name),
              team_home_away = as.character(team_home_away),
              team_score = as.character(team_score),
              stat_type = as.character(stat_type),
              stat_name = as.character(stat_row$name),
              stat_value = as.character(stat_row$displayValue),
              stringsAsFactors = FALSE
            )

            team_stats_df <- rbind(team_stats_df, team_row)
          }
        }
      }
    }
  }

  # Clean up row names
  if (nrow(team_stats_df) > 0) {
    rownames(team_stats_df) <- NULL
  }

  return(team_stats_df)
}

#' Fetch detailed MLB boxscore using ESPN API
#'
#' Retrieves comprehensive boxscore information for a specific MLB game from ESPN's API.
#' Assigns player statistics and team statistics to global environment.
#'
#' @param event_id Character or numeric. ESPN event/game ID
#'   (e.g., "401581132" for a specific MLB game).
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'mlb_boxscore_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns a list containing the datasets. The main purpose is global
#'   environment assignment of:
#'   \itemize{
#'     \item \code{mlb_player_stats}: Individual player statistics for both teams
#'     \item \code{mlb_team_stats}: Team-level statistics and totals
#'   }
#'
#' @details
#' The function creates comprehensive datasets from ESPN's MLB boxscore API.
#'
#' **Player Statistics** (\code{mlb_player_stats}):
#' \itemize{
#'   \item Batting stats: at-bats, runs, hits, RBI, walks, strikeouts, AVG, OBP, SLG, OPS
#'   \item Pitching stats: innings pitched, hits allowed, runs allowed, ERA, WHIP, strikeouts
#'   \item Player details: name, position, starter status
#' }
#'
#' **Team Statistics** (\code{mlb_team_stats}):
#' \itemize{
#'   \item Team batting totals and averages
#'   \item Team pitching totals and averages
#'   \item Final scores and game outcome data
#' }
#'
#' **Statistical Categories**:
#' Batting: AB, R, H, RBI, BB, SO, AVG, OBP, SLG, OPS
#' Pitching: IP, H, R, ER, BB, SO, HR, ERA, WHIP, Pitches
#'
#' @examples
#' \dontrun{
#' # Get boxscore for a specific game
#' fetch_mlb_boxscore("401581132")
#'
#' # Access player statistics
#' head(mlb_player_stats)
#'
#' # View batting statistics only
#' batting_stats <- mlb_player_stats[mlb_player_stats$stat_type == "batting", ]
#' head(batting_stats[, c("player_display_name", "at_bats", "hits", "rbi", "avg")])
#'
#' # View pitching statistics only
#' pitching_stats <- mlb_player_stats[mlb_player_stats$stat_type == "pitching", ]
#' head(pitching_stats[, c("player_display_name", "innings_pitched", "era", "whip")])
#'
#' # Access team statistics
#' head(mlb_team_stats)
#'
#' # Find top hitters
#' top_hitters <- mlb_player_stats[
#'   mlb_player_stats$stat_type == "batting" &
#'   !is.na(mlb_player_stats$hits) &
#'   mlb_player_stats$hits != "",
#' ]
#' top_hitters <- top_hitters[order(-as.numeric(top_hitters$hits)), ]
#' head(top_hitters[, c("player_display_name", "team_abbreviation", "hits", "rbi")])
#' }
#'
#' @importFrom httr GET status_code content timeout
#' @export
fetch_mlb_boxscore <- function(event_id, raw = FALSE) {
  # Input validation
  if (missing(event_id) || is.null(event_id) || event_id == "") {
    stop("event_id must be provided and cannot be empty")
  }

  if (!is.logical(raw)) {
    stop("raw parameter must be logical (TRUE or FALSE)")
  }

  # Construct boxscore API URL for MLB
  url <- paste0("https://site.api.espn.com/apis/site/v2/sports/baseball/mlb/summary?event=", event_id)

  message("Fetching MLB boxscore for game ", event_id, "...")

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
    assign("mlb_boxscore_raw", data, envir = .GlobalEnv)
    message("Raw MLB boxscore data assigned to: mlb_boxscore_raw")

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
    create_mlb_player_stats_dataset(data, event_id)
  }, error = function(e) {
    warning("Error processing player statistics: ", e$message)
    data.frame()
  })

  # Assign to global environment
  assign("mlb_player_stats", player_stats, envir = .GlobalEnv)

  # Create team statistics dataset
  message("Processing team statistics...")
  team_stats <- tryCatch({
    create_mlb_team_stats_dataset(data, event_id)
  }, error = function(e) {
    warning("Error processing team statistics: ", e$message)
    data.frame()
  })

  # Assign to global environment
  assign("mlb_team_stats", team_stats, envir = .GlobalEnv)

  message("Players processed: ", nrow(player_stats))
  message("Teams processed: ", nrow(team_stats))

  # Show sample stats if available
  if (nrow(player_stats) > 0) {
    # Show batting and pitching breakdown
    batting_count <- sum(player_stats$stat_type == "batting", na.rm = TRUE)
    pitching_count <- sum(player_stats$stat_type == "pitching", na.rm = TRUE)

    message("  - Batting records: ", batting_count)
    message("  - Pitching records: ", pitching_count)

    # Show top performers
    batting_stats <- player_stats[player_stats$stat_type == "batting" &
                                    !is.na(player_stats$hits) & player_stats$hits != "", ]
    if (nrow(batting_stats) > 0) {
      batting_stats$hits_numeric <- suppressWarnings(as.numeric(batting_stats$hits))
      batting_stats <- batting_stats[!is.na(batting_stats$hits_numeric), ]
      if (nrow(batting_stats) > 0) {
        top_hitter <- batting_stats[which.max(batting_stats$hits_numeric), ]
        message("  Top hitter: ", top_hitter$player_display_name, " (", top_hitter$team_abbreviation,
                ") - ", top_hitter$hits, " hits")
      }
    }
  }

  result <- list(
    player_stats = player_stats,
    team_stats = team_stats
  )

  invisible(result)
}
