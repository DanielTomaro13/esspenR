#' Safe nested data extraction helper function for soccer standings
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_soccer_standings <- function(data, path, default = NA) {
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

#' Create soccer standings data frames from ESPN API response
#'
#' Processes raw JSON response from ESPN soccer standings API into structured data frames
#' containing detailed league standings information
#'
#' @param data Raw JSON response from ESPN soccer standings API
#' @param league_slug Character. League slug used in request
#' @return List containing multiple data frames with standings information
#' @keywords internal
create_soccer_standings_datasets <- function(data, league_slug) {
  # Initialize data frames
  league_info_df <- data.frame(
    league_slug = character(0),
    league_name = character(0),
    league_abbreviation = character(0),
    season_year = character(0),
    season_type = character(0),
    season_display_name = character(0),
    total_teams = character(0),
    total_groups = character(0),
    stringsAsFactors = FALSE
  )

  standings_df <- data.frame(
    league_slug = character(0),
    season_year = character(0),
    group_name = character(0),
    group_id = character(0),
    position = character(0),
    team_id = character(0),
    team_name = character(0),
    team_display_name = character(0),
    team_abbreviation = character(0),
    team_logo = character(0),
    games_played = character(0),
    wins = character(0),
    losses = character(0),
    draws = character(0),
    points = character(0),
    goals_for = character(0),
    goals_against = character(0),
    goal_difference = character(0),
    form = character(0),
    note = character(0),
    note_color = character(0),
    stringsAsFactors = FALSE
  )

  # Extract league information
  league_name <- league_abbreviation <- season_year <- season_type <- season_display_name <- NA_character_

  # Check for league info in different possible locations
  if ("league" %in% names(data)) {
    league_info <- data$league
    league_name <- extract_nested_soccer_standings(league_info, c("name"), NA_character_)
    league_abbreviation <- extract_nested_soccer_standings(league_info, c("abbreviation"), NA_character_)

    # Season information
    season_info <- extract_nested_soccer_standings(league_info, c("season"), list())
    if (length(season_info) > 0) {
      season_year <- extract_nested_soccer_standings(season_info, c("year"), NA_character_)
      season_display_name <- extract_nested_soccer_standings(season_info, c("displayName"), NA_character_)
      season_type <- extract_nested_soccer_standings(season_info, c("type", "name"), NA_character_)
    }
  }

  # Extract standings data
  children <- extract_nested_soccer_standings(data, c("children"), list())
  total_teams <- 0
  total_groups <- 0

  if (is.list(children) && length(children) > 0) {
    total_groups <- length(children)

    for (group in children) {
      group_name <- extract_nested_soccer_standings(group, c("name"), "Main")
      group_id <- extract_nested_soccer_standings(group, c("id"), NA_character_)

      # Extract standings from this group
      standings <- extract_nested_soccer_standings(group, c("standings"), list())

      if (is.list(standings) && length(standings) > 0) {
        # Standings can be a list of entries
        entries <- if ("entries" %in% names(standings)) standings$entries else standings

        if (is.list(entries)) {
          for (i in seq_along(entries)) {
            entry <- entries[[i]]

            if (is.list(entry)) {
              # Basic team information
              team_info <- extract_nested_soccer_standings(entry, c("team"), list())
              team_id <- extract_nested_soccer_standings(team_info, c("id"), NA_character_)
              team_name <- extract_nested_soccer_standings(team_info, c("name"), NA_character_)
              team_display_name <- extract_nested_soccer_standings(team_info, c("displayName"), NA_character_)
              team_abbreviation <- extract_nested_soccer_standings(team_info, c("abbreviation"), NA_character_)
              team_logo <- extract_nested_soccer_standings(team_info, c("logo"), NA_character_)

              # Position and statistics
              position <- extract_nested_soccer_standings(entry, c("position"), i)

              # Extract stats
              stats <- extract_nested_soccer_standings(entry, c("stats"), list())
              games_played <- wins <- losses <- draws <- points <- NA_character_
              goals_for <- goals_against <- goal_difference <- NA_character_

              if (is.list(stats)) {
                for (stat in stats) {
                  if (is.list(stat) && "name" %in% names(stat)) {
                    stat_name <- stat$name
                    stat_value <- extract_nested_soccer_standings(stat, c("value"), NA_character_)

                    # Map stat names to our columns
                    if (stat_name == "gamesPlayed" || stat_name == "GP") {
                      games_played <- stat_value
                    } else if (stat_name == "wins" || stat_name == "W") {
                      wins <- stat_value
                    } else if (stat_name == "losses" || stat_name == "L") {
                      losses <- stat_value
                    } else if (stat_name == "ties" || stat_name == "D" || stat_name == "draws") {
                      draws <- stat_value
                    } else if (stat_name == "points" || stat_name == "PTS") {
                      points <- stat_value
                    } else if (stat_name == "pointsFor" || stat_name == "GF") {
                      goals_for <- stat_value
                    } else if (stat_name == "pointsAgainst" || stat_name == "GA") {
                      goals_against <- stat_value
                    } else if (stat_name == "pointDifferential" || stat_name == "GD") {
                      goal_difference <- stat_value
                    }
                  }
                }
              }

              # Form (recent results)
              form <- extract_nested_soccer_standings(entry, c("form"), NA_character_)

              # Note (qualification/relegation info)
              note <- extract_nested_soccer_standings(entry, c("note"), NA_character_)
              note_color <- extract_nested_soccer_standings(entry, c("note", "color"), NA_character_)

              # Create standings row
              standings_row <- data.frame(
                league_slug = as.character(league_slug),
                season_year = as.character(season_year),
                group_name = as.character(group_name),
                group_id = as.character(group_id),
                position = as.character(position),
                team_id = as.character(team_id),
                team_name = as.character(team_name),
                team_display_name = as.character(team_display_name),
                team_abbreviation = as.character(team_abbreviation),
                team_logo = as.character(team_logo),
                games_played = as.character(games_played),
                wins = as.character(wins),
                losses = as.character(losses),
                draws = as.character(draws),
                points = as.character(points),
                goals_for = as.character(goals_for),
                goals_against = as.character(goals_against),
                goal_difference = as.character(goal_difference),
                form = as.character(form),
                note = as.character(note),
                note_color = as.character(note_color),
                stringsAsFactors = FALSE
              )
              standings_df <- rbind(standings_df, standings_row)
              total_teams <- total_teams + 1
            }
          }
        }
      }
    }
  }

  # Create league info row
  league_row <- data.frame(
    league_slug = as.character(league_slug),
    league_name = as.character(league_name),
    league_abbreviation = as.character(league_abbreviation),
    season_year = as.character(season_year),
    season_type = as.character(season_type),
    season_display_name = as.character(season_display_name),
    total_teams = as.character(total_teams),
    total_groups = as.character(total_groups),
    stringsAsFactors = FALSE
  )
  league_info_df <- rbind(league_info_df, league_row)

  # Clean up row names
  if (nrow(league_info_df) > 0) rownames(league_info_df) <- NULL
  if (nrow(standings_df) > 0) rownames(standings_df) <- NULL

  return(list(
    league_info = league_info_df,
    standings = standings_df
  ))
}

#' Fetch soccer league standings data using ESPN API
#'
#' Retrieves soccer league standings and team positions from ESPN's API for a specific league.
#' The function fetches comprehensive standings data including team statistics, positions, and form.
#'
#' @param league_slug Character. ESPN league slug (required).
#'   Examples: "eng.1" (Premier League), "esp.1" (La Liga), "ger.1" (Bundesliga).
#' @param season Character. Season year (optional).
#'   Format: "YYYY" (e.g., "2024"). If not specified, returns current season.
#' @param return_type Character. Type of data to return (default: "all").
#'   Options: "league_info", "standings", "all".
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'soccer_standings_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data.
#'
#' @examples
#' \dontrun{
#' # Get Premier League current standings
#' fetch_soccer_standings("eng.1")
#'
#' # Get La Liga standings for specific season
#' fetch_soccer_standings("esp.1", season = "2023")
#'
#' # Get raw data for debugging
#' fetch_soccer_standings("eng.1", raw = TRUE)
#' }
#'
#' @importFrom httr GET status_code content timeout
#' @importFrom jsonlite fromJSON
#' @export
fetch_soccer_standings <- function(league_slug, season = NULL, return_type = "all", raw = FALSE) {
  # Input validation
  if (missing(league_slug) || is.null(league_slug) || league_slug == "") {
    stop("'league_slug' is required")
  }

  valid_types <- c("league_info", "standings", "all")
  if (!return_type %in% valid_types) {
    stop(sprintf("'return_type' must be one of: %s", paste(valid_types, collapse = ", ")))
  }

  if (!is.logical(raw)) {
    stop("'raw' parameter must be logical (TRUE or FALSE)")
  }

  # Build API URL
  base_url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/soccer/%s/standings", league_slug)

  # Add season parameter if provided
  if (!is.null(season) && nchar(as.character(season)) > 0) {
    api_url <- paste0(base_url, "?season=", season)
    message(sprintf("Fetching soccer standings for %s (season: %s)...", league_slug, season))
  } else {
    api_url <- base_url
    message(sprintf("Fetching soccer standings for %s...", league_slug))
  }

  # Fetch and parse data
  tryCatch({
    resp <- httr::GET(api_url, httr::timeout(30))

    if (httr::status_code(resp) != 200) {
      stop(sprintf("API request failed: HTTP %d", httr::status_code(resp)))
    }

    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content_text, simplifyVector = TRUE, simplifyDataFrame = TRUE)

    # Handle raw data assignment for debugging
    if (isTRUE(raw)) {
      assign("soccer_standings_raw", data, envir = .GlobalEnv)
      message("Raw soccer standings data assigned to: soccer_standings_raw")
      message("Available sections: ", paste(names(data), collapse = ", "))
      return(invisible(data))
    }

    # Create datasets
    datasets <- create_soccer_standings_datasets(data, league_slug)

    # Assign data based on return_type
    result_data <- list()

    if (return_type %in% c("league_info", "all")) {
      assign("soccer_standings_league_info", datasets$league_info, envir = .GlobalEnv)
      result_data$league_info <- datasets$league_info
      message(sprintf("League info assigned to: soccer_standings_league_info (%d records)",
                      nrow(datasets$league_info)))
    }

    if (return_type %in% c("standings", "all")) {
      assign("soccer_standings", datasets$standings, envir = .GlobalEnv)
      result_data$standings <- datasets$standings
      message(sprintf("Standings assigned to: soccer_standings (%d teams)",
                      nrow(datasets$standings)))

      # Show sample information
      if (nrow(datasets$standings) > 0) {
        # Show top 3 teams
        top_teams <- head(datasets$standings[order(as.numeric(datasets$standings$position)), ], 3)
        message("Top 3 teams:")
        for (i in seq_len(nrow(top_teams))) {
          team <- top_teams[i, ]
          message(sprintf("  %s. %s - %s points",
                          team$position, team$team_display_name, team$points))
        }
      }
    }

    # Return appropriate data structure
    if (return_type == "all") {
      return(invisible(result_data))
    } else {
      return(invisible(result_data[[return_type]]))
    }

  }, error = function(e) {
    stop(sprintf("Failed to fetch soccer standings for %s: %s", league_slug, e$message))
  })
}
