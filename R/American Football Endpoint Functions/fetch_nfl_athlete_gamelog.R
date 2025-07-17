#' Safe nested data extraction helper function
#'
#' Safely extracts values from nested list structures with error handling
#'
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested <- function(data, path, default = NA) {
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

#' Create athlete game log data frame from gamelog response
#'
#' Processes raw JSON response from ESPN Web API into a structured data frame
#' containing game-by-game statistics for an NFL athlete
#'
#' @param data Raw JSON response from ESPN Web API athlete gamelog endpoint
#' @param athlete_id Character. Athlete ID used in request
#' @return data.frame with athlete game log information
#' @keywords internal
create_athlete_gamelog_dataset <- function(data, athlete_id) {
  # Initialize result data frame
  result_df <- data.frame(
    athlete_id = character(0),
    season_year = character(0),
    season_type = character(0),
    event_id = character(0),
    game_date = character(0),
    week = character(0),
    opponent_id = character(0),
    opponent_name = character(0),
    opponent_abbreviation = character(0),
    opponent_logo = character(0),
    home_away = character(0),
    game_result = character(0),
    team_score = character(0),
    opponent_score = character(0),
    final_score = character(0),
    stat_category = character(0),
    stat_label = character(0),
    stat_name = character(0),
    stat_display_name = character(0),
    stat_value = character(0),
    stat_display_value = character(0),
    stringsAsFactors = FALSE
  )

  # Get the main stat definitions
  labels <- extract_nested(data, c("labels"), list())
  names_list <- extract_nested(data, c("names"), list())
  display_names <- extract_nested(data, c("displayNames"), list())
  events_data <- extract_nested(data, c("events"), list())

  if (length(events_data) == 0 || length(labels) == 0) {
    return(result_df)
  }

  # Get categories to understand stat groupings
  categories <- extract_nested(data, c("categories"), list())

  # Determine stat category boundaries
  passing_count <- 11  # Default based on typical structure
  rushing_start <- 12  # Rushing stats start after passing

  if (length(categories) >= 2) {
    passing_count <- extract_nested(categories[[1]], c("count"), 11)
    rushing_start <- passing_count + 1
  }

  # Process season types to get game statistics
  season_types <- extract_nested(data, c("seasonTypes"), list())

  for (season_type in season_types) {
    season_display <- extract_nested(season_type, c("displayName"), "")

    # Extract season year and type
    season_year <- "2024"  # Default
    if (grepl("\\d{4}", season_display)) {
      season_year <- regmatches(season_display, regexpr("\\d{4}", season_display))
    }

    season_type_name <- if (grepl("Postseason", season_display)) "Postseason" else "Regular Season"

    # Get categories within this season type
    season_categories <- extract_nested(season_type, c("categories"), list())

    for (category in season_categories) {
      category_events <- extract_nested(category, c("events"), list())

      # Process each event (game)
      for (event_entry in category_events) {
        event_id <- extract_nested(event_entry, c("eventId"), NA_character_)
        event_stats <- extract_nested(event_entry, c("stats"), list())

        if (is.null(event_id) || is.na(event_id) || length(event_stats) == 0) {
          next
        }

        # Get event details from main events object
        event_details <- extract_nested(events_data, c(event_id), list())

        if (length(event_details) == 0) {
          next
        }

        # Extract game information
        game_date <- extract_nested(event_details, c("gameDate"), NA_character_)
        week <- extract_nested(event_details, c("week"), NA_character_)
        home_away <- extract_nested(event_details, c("atVs"), NA_character_)
        game_result <- extract_nested(event_details, c("gameResult"), NA_character_)
        score <- extract_nested(event_details, c("score"), NA_character_)

        # Extract opponent information
        opponent <- extract_nested(event_details, c("opponent"), list())
        opponent_id <- extract_nested(opponent, c("id"), NA_character_)
        opponent_name <- extract_nested(opponent, c("displayName"), NA_character_)
        opponent_abbreviation <- extract_nested(opponent, c("abbreviation"), NA_character_)
        opponent_logo <- extract_nested(opponent, c("logo"), NA_character_)

        # Extract team scores
        home_score <- extract_nested(event_details, c("homeTeamScore"), NA_character_)
        away_score <- extract_nested(event_details, c("awayTeamScore"), NA_character_)

        # Determine team vs opponent scores based on home/away
        if (home_away == "vs") {  # Home game
          team_score <- home_score
          opponent_score <- away_score
        } else if (home_away == "@") {  # Away game
          team_score <- away_score
          opponent_score <- home_score
        } else {
          team_score <- NA_character_
          opponent_score <- NA_character_
        }

        # Process each statistic for this game
        for (stat_idx in seq_along(event_stats)) {
          if (stat_idx <= length(labels) && stat_idx <= length(names_list) && stat_idx <= length(display_names)) {

            # Determine stat category based on position
            if (stat_idx <= passing_count) {
              stat_category <- "Passing"
            } else {
              stat_category <- "Rushing"
            }

            stat_value <- event_stats[[stat_idx]]

            # Create row for this statistic
            game_row <- data.frame(
              athlete_id = as.character(athlete_id),
              season_year = as.character(season_year),
              season_type = season_type_name,
              event_id = as.character(event_id),
              game_date = as.character(game_date),
              week = as.character(week),
              opponent_id = as.character(opponent_id),
              opponent_name = as.character(opponent_name),
              opponent_abbreviation = as.character(opponent_abbreviation),
              opponent_logo = as.character(opponent_logo),
              home_away = as.character(home_away),
              game_result = as.character(game_result),
              team_score = as.character(team_score),
              opponent_score = as.character(opponent_score),
              final_score = as.character(score),
              stat_category = stat_category,
              stat_label = as.character(labels[[stat_idx]]),
              stat_name = as.character(names_list[[stat_idx]]),
              stat_display_name = as.character(display_names[[stat_idx]]),
              stat_value = as.character(stat_value),
              stat_display_value = as.character(stat_value),
              stringsAsFactors = FALSE
            )

            result_df <- rbind(result_df, game_row)
          }
        }
      }
    }
  }

  # Clean up row names
  if (nrow(result_df) > 0) {
    rownames(result_df) <- NULL
  }

  return(result_df)
}

#' Create athlete season summary data frame from gamelog response
#'
#' Processes raw JSON response to create season summary statistics
#'
#' @param data Raw JSON response from ESPN Web API athlete gamelog endpoint
#' @param athlete_id Character. Athlete ID used in request
#' @return data.frame with athlete season summary information
#' @keywords internal
create_athlete_season_summary_dataset <- function(data, athlete_id) {
  # Initialize result data frame
  result_df <- data.frame(
    athlete_id = character(0),
    season_year = character(0),
    season_type = character(0),
    stat_category = character(0),
    stat_label = character(0),
    stat_name = character(0),
    stat_display_name = character(0),
    stat_value = character(0),
    stat_display_value = character(0),
    stringsAsFactors = FALSE
  )

  # Get stat definitions
  labels <- extract_nested(data, c("labels"), list())
  names_list <- extract_nested(data, c("names"), list())
  display_names <- extract_nested(data, c("displayNames"), list())

  if (length(labels) == 0) {
    return(result_df)
  }

  # Get categories for stat groupings
  categories <- extract_nested(data, c("categories"), list())
  passing_count <- 11
  if (length(categories) >= 1) {
    passing_count <- extract_nested(categories[[1]], c("count"), 11)
  }

  # Process season types
  season_types <- extract_nested(data, c("seasonTypes"), list())

  for (season_type in season_types) {
    season_display <- extract_nested(season_type, c("displayName"), "")

    # Extract season year and type
    season_year <- "2024"
    if (grepl("\\d{4}", season_display)) {
      season_year <- regmatches(season_display, regexpr("\\d{4}", season_display))
    }

    season_type_name <- if (grepl("Postseason", season_display)) "Postseason" else "Regular Season"

    # Get summary stats
    summary_data <- extract_nested(season_type, c("summary"), list())
    if (length(summary_data) > 0) {
      stats_list <- extract_nested(summary_data, c("stats"), list())

      if (length(stats_list) > 0) {
        for (stats_entry in stats_list) {
          stats_values <- extract_nested(stats_entry, c("stats"), list())

          if (length(stats_values) > 0) {
            # Process each summary statistic
            for (stat_idx in seq_along(stats_values)) {
              if (stat_idx <= length(labels) && stat_idx <= length(names_list) && stat_idx <= length(display_names)) {

                # Determine stat category
                stat_category <- if (stat_idx <= passing_count) "Passing" else "Rushing"

                stat_value <- stats_values[[stat_idx]]

                summary_row <- data.frame(
                  athlete_id = as.character(athlete_id),
                  season_year = as.character(season_year),
                  season_type = season_type_name,
                  stat_category = stat_category,
                  stat_label = as.character(labels[[stat_idx]]),
                  stat_name = as.character(names_list[[stat_idx]]),
                  stat_display_name = as.character(display_names[[stat_idx]]),
                  stat_value = as.character(stat_value),
                  stat_display_value = as.character(stat_value),
                  stringsAsFactors = FALSE
                )

                result_df <- rbind(result_df, summary_row)
              }
            }
          }
        }
      }
    }
  }

  if (nrow(result_df) > 0) {
    rownames(result_df) <- NULL
  }

  return(result_df)
}

#' Fetch NFL athlete game log data using Web API
#'
#' Retrieves detailed game-by-game statistics for NFL players from ESPN's API.
#' The function fetches data from ESPN's Web API and processes it into a
#' structured data frame containing game details, opponent information, and
#' statistical performance for each game.
#'
#' @param athlete_id Character or Numeric. ESPN athlete ID (required).
#'   The unique identifier for the athlete in ESPN's database.
#' @param season Character or Numeric. Season year (optional).
#'   If not provided, fetches current/latest season. Format: "2024" or 2024.
#' @param season_type Character. Season type to fetch (default: "all").
#'   Options are:
#'   \itemize{
#'     \item "regular" - Regular season games only
#'     \item "postseason" - Playoff games only
#'     \item "all" - Both regular season and postseason games
#'   }
#' @param return_type Character. Type of data to return (default: "games").
#'   Options are:
#'   \itemize{
#'     \item "games" - Game-by-game statistics
#'     \item "summary" - Season summary statistics
#'     \item "both" - Both game-by-game and summary data
#'   }
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'nfl_athlete_gamelog_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data. The main purpose is global
#'   environment assignment:
#'   \itemize{
#'     \item \code{nfl_athlete_gamelog} - Game-by-game data frame (if return_type includes "games")
#'     \item \code{nfl_athlete_season_summary} - Season summary data frame (if return_type includes "summary")
#'     \item \code{nfl_athlete_gamelog_raw} - Raw JSON response (if raw = TRUE)
#'   }
#'
#' @details
#' The function creates structured data frames with the following columns:
#'
#' Game log data frame columns:
#' \itemize{
#'   \item athlete_id, season_year, season_type, event_id
#'   \item game_date, week, opponent information
#'   \item home_away, game_result, scores
#'   \item stat_category, stat_name, stat_value, etc.
#' }
#'
#' The function handles both passing and rushing statistics, automatically
#' categorizing them based on the ESPN API response structure.
#'
#' @examples
#' \dontrun{
#' # Get game-by-game stats for Josh Allen
#' fetch_nfl_athlete_gamelog(athlete_id = "3918298")
#' head(nfl_athlete_gamelog)
#'
#' # Get specific season data
#' fetch_nfl_athlete_gamelog(athlete_id = "3918298", season = "2024")
#'
#' # Get only regular season games
#' fetch_nfl_athlete_gamelog(athlete_id = "3918298", season_type = "regular")
#'
#' # Get season summary statistics
#' fetch_nfl_athlete_gamelog(athlete_id = "3918298", return_type = "summary")
#' head(nfl_athlete_season_summary)
#'
#' # Get both games and summary data
#' fetch_nfl_athlete_gamelog(athlete_id = "3918298", return_type = "both")
#'
#' # Get raw data for debugging
#' fetch_nfl_athlete_gamelog(athlete_id = "3918298", raw = TRUE)
#' str(nfl_athlete_gamelog_raw, max.level = 2)
#' }
#'
#' @seealso \code{\link{fetch_multiple_nfl_athlete_gamelogs}} for fetching
#'   multiple athletes' data
#'
#' @export
fetch_nfl_athlete_gamelog <- function(athlete_id, season = NULL, season_type = "all",
                                      return_type = "games", raw = FALSE) {
  # Input validation
  if (missing(athlete_id)) {
    stop("'athlete_id' is a required parameter")
  }

  if (!season_type %in% c("regular", "postseason", "all")) {
    stop("'season_type' must be one of: 'regular', 'postseason', or 'all'")
  }

  if (!return_type %in% c("games", "summary", "both")) {
    stop("'return_type' must be one of: 'games', 'summary', or 'both'")
  }

  # Convert athlete_id to character for URL building
  athlete_id <- as.character(athlete_id)

  # Build API URL
  url <- sprintf("https://site.web.api.espn.com/apis/common/v3/sports/football/nfl/athletes/%s/gamelog",
                 athlete_id)

  # Add query parameters if specified
  query_params <- list()
  if (!is.null(season)) {
    query_params$season <- as.character(season)
  }
  if (season_type != "all") {
    query_params$seasontype <- switch(season_type,
                                      "regular" = "2",
                                      "postseason" = "3")
  }

  # Fetch and parse data
  tryCatch({
    resp <- httr::GET(url, query = query_params, httr::timeout(60))

    if (httr::status_code(resp) != 200) {
      stop(sprintf("API request failed: HTTP %d - %s",
                   httr::status_code(resp),
                   httr::http_status(resp)$message))
    }

    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content_text, simplifyVector = FALSE, simplifyDataFrame = FALSE)

    # Handle raw data assignment for debugging
    if (isTRUE(raw)) {
      assign("nfl_athlete_gamelog_raw", data, envir = .GlobalEnv)
      message("Raw NFL athlete gamelog data assigned to: nfl_athlete_gamelog_raw")
      message("Data structure preview:")
      message("- Events count: ", length(extract_nested(data, c("events"), list())))
      message("- Season types count: ", length(extract_nested(data, c("seasonTypes"), list())))
      message("- Labels count: ", length(extract_nested(data, c("labels"), list())))
      return(invisible(data))
    }

    # Create datasets based on return_type parameter
    result_data <- list()

    if (return_type %in% c("games", "both")) {
      gamelog_df <- create_athlete_gamelog_dataset(data, athlete_id)
      assign("nfl_athlete_gamelog", gamelog_df, envir = .GlobalEnv)
      result_data$games <- gamelog_df

      # Calculate summary statistics for user feedback
      unique_games <- length(unique(gamelog_df$event_id[!is.na(gamelog_df$event_id)]))
      message(sprintf("NFL athlete game log data assigned to: nfl_athlete_gamelog (%d games, %d stat records)",
                      unique_games, nrow(gamelog_df)))
    }

    if (return_type %in% c("summary", "both")) {
      summary_df <- create_athlete_season_summary_dataset(data, athlete_id)
      assign("nfl_athlete_season_summary", summary_df, envir = .GlobalEnv)
      result_data$summary <- summary_df

      message(sprintf("NFL athlete season summary data assigned to: nfl_athlete_season_summary (%d stat records)",
                      nrow(summary_df)))
    }

    # Return appropriate data structure
    if (return_type == "both") {
      return(invisible(result_data))
    } else if (return_type == "games") {
      return(invisible(result_data$games))
    } else if (return_type == "summary") {
      return(invisible(result_data$summary))
    }

  }, error = function(e) {
    stop(sprintf("Failed to fetch NFL athlete gamelog for athlete %s: %s",
                 athlete_id, e$message))
  })
}

#' Fetch multiple NFL athletes' game log data
#'
#' Retrieves game log data for multiple NFL athletes with rate limiting to
#' be respectful to ESPN's API. This function calls \code{\link{fetch_nfl_athlete_gamelog}}
#' for each athlete and combines the results.
#'
#' @param athlete_ids Character or Numeric vector. ESPN athlete IDs.
#'   Vector of unique identifiers for athletes in ESPN's database.
#' @param season Character or Numeric. Season year (optional).
#'   If not provided, fetches current/latest season for all athletes.
#' @param season_type Character. Season type to fetch (default: "all").
#'   Same options as \code{\link{fetch_nfl_athlete_gamelog}}.
#' @param return_type Character. Type of data to return (default: "games").
#'   Same options as \code{\link{fetch_nfl_athlete_gamelog}}.
#' @param delay Numeric. Delay in seconds between API requests (default: 0.1).
#'   Used to be respectful to ESPN's servers and avoid rate limiting.
#' @param raw Logical. If TRUE, assigns raw JSON to global environment
#'   for the first athlete only (default: FALSE).
#'
#' @return Invisibly returns the combined data. The main purpose is global
#'   environment assignment of combined datasets from all athletes:
#'   \itemize{
#'     \item \code{nfl_athlete_gamelog} - Combined game-by-game data
#'     \item \code{nfl_athlete_season_summary} - Combined season summary data
#'   }
#'
#' @details
#' The function processes athletes sequentially with a configurable delay
#' between requests. Failed requests for individual athletes are logged but
#' do not stop the overall process. Duplicate records are automatically
#' removed from the final combined dataset.
#'
#' @examples
#' \dontrun{
#' # Get game logs for multiple quarterbacks
#' qb_ids <- c("3918298", "3139477", "4035671")  # Allen, Mahomes, Burrow
#' fetch_multiple_nfl_athlete_gamelogs(qb_ids)
#'
#' # Get specific season for multiple players
#' fetch_multiple_nfl_athlete_gamelogs(qb_ids, season = "2024")
#'
#' # Get season summaries for multiple players
#' fetch_multiple_nfl_athlete_gamelogs(qb_ids, return_type = "summary")
#'
#' # Use longer delay for larger requests
#' fetch_multiple_nfl_athlete_gamelogs(qb_ids, delay = 0.5)
#' }
#'
#' @seealso \code{\link{fetch_nfl_athlete_gamelog}} for single athlete data
#'
#' @export
fetch_multiple_nfl_athlete_gamelogs <- function(athlete_ids, season = NULL, season_type = "all",
                                                return_type = "games", delay = 0.1, raw = FALSE) {
  # Input validation
  if (length(athlete_ids) == 0) {
    stop("'athlete_ids' must contain at least one athlete ID")
  }

  # Initialize combined data containers
  all_gamelog_data <- data.frame()
  all_summary_data <- data.frame()

  message(sprintf("Starting to fetch gamelog data for %d athletes...", length(athlete_ids)))

  # Process each athlete sequentially
  for (i in seq_along(athlete_ids)) {
    athlete_id <- athlete_ids[i]
    message(sprintf("Fetching gamelog data for athlete %s (%d/%d)...", athlete_id, i, length(athlete_ids)))

    tryCatch({
      # Fetch individual athlete data
      athlete_data <- fetch_nfl_athlete_gamelog(
        athlete_id = athlete_id,
        season = season,
        season_type = season_type,
        return_type = return_type,
        raw = raw
      )

      # If raw data requested, return after first athlete
      if (isTRUE(raw)) {
        return(invisible(athlete_data))
      }

      # Combine data based on return type
      if (return_type %in% c("games", "both")) {
        gamelog_df <- get("nfl_athlete_gamelog", envir = .GlobalEnv)
        all_gamelog_data <- rbind(all_gamelog_data, gamelog_df)
      }

      if (return_type %in% c("summary", "both")) {
        summary_df <- get("nfl_athlete_season_summary", envir = .GlobalEnv)
        all_summary_data <- rbind(all_summary_data, summary_df)
      }

    }, error = function(e) {
      message(sprintf("Failed to fetch gamelog data for athlete %s: %s", athlete_id, e$message))
    })

    # Be respectful to the API with delay between requests
    if (i < length(athlete_ids)) {
      Sys.sleep(delay)
    }
  }

  # Remove duplicates and assign combined datasets to global environment
  result_data <- list()

  if (return_type %in% c("games", "both") && nrow(all_gamelog_data) > 0) {
    # Remove duplicate records based on athlete, event, and stat
    all_gamelog_data <- all_gamelog_data[!duplicated(paste(all_gamelog_data$athlete_id,
                                                           all_gamelog_data$event_id,
                                                           all_gamelog_data$stat_name)), ]
    assign("nfl_athlete_gamelog", all_gamelog_data, envir = .GlobalEnv)
    result_data$games <- all_gamelog_data

    # Calculate summary statistics for user feedback
    unique_games <- length(unique(all_gamelog_data$event_id[!is.na(all_gamelog_data$event_id)]))
    unique_athletes <- length(unique(all_gamelog_data$athlete_id))

    message(sprintf("Completed! NFL athlete game log data assigned to: nfl_athlete_gamelog (%d athletes, %d games, %d stat records)",
                    unique_athletes, unique_games, nrow(all_gamelog_data)))
  }

  if (return_type %in% c("summary", "both") && nrow(all_summary_data) > 0) {
    # Remove duplicate records based on athlete, season, and stat
    all_summary_data <- all_summary_data[!duplicated(paste(all_summary_data$athlete_id,
                                                           all_summary_data$season_year,
                                                           all_summary_data$stat_name)), ]
    assign("nfl_athlete_season_summary", all_summary_data, envir = .GlobalEnv)
    result_data$summary <- all_summary_data

    unique_athletes <- length(unique(all_summary_data$athlete_id))

    message(sprintf("Completed! NFL athlete season summary data assigned to: nfl_athlete_season_summary (%d athletes, %d stat records)",
                    unique_athletes, nrow(all_summary_data)))
  }

  # Return appropriate data structure
  if (return_type == "both") {
    return(invisible(result_data))
  } else if (return_type == "games") {
    return(invisible(result_data$games))
  } else if (return_type == "summary") {
    return(invisible(result_data$summary))
  }
}
