#' Safe nested data extraction helper function for soccer teams
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_soccer_teams <- function(data, path, default = NA) {
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

#' Create soccer teams data frames from ESPN API response
#'
#' Processes raw JSON response from ESPN soccer teams API into structured data frames
#' containing detailed team information
#'
#' @param data Raw JSON response from ESPN soccer teams API
#' @param league_slug Character. League slug used in request
#' @return List containing multiple data frames with team information
#' @keywords internal
create_soccer_teams_datasets <- function(data, league_slug) {
  # Initialize data frames
  league_info_df <- data.frame(
    league_slug = character(0),
    league_name = character(0),
    league_abbreviation = character(0),
    season_year = character(0),
    season_type = character(0),
    total_teams = character(0),
    stringsAsFactors = FALSE
  )

  teams_df <- data.frame(
    league_slug = character(0),
    team_id = character(0),
    team_name = character(0),
    team_display_name = character(0),
    team_short_display_name = character(0),
    team_abbreviation = character(0),
    team_location = character(0),
    team_nickname = character(0),
    team_color = character(0),
    team_alternate_color = character(0),
    team_logo = character(0),
    team_is_active = character(0),
    stringsAsFactors = FALSE
  )

  # Extract league information
  league_name <- league_abbreviation <- season_year <- season_type <- NA_character_

  # Extract teams data - based on the actual structure shown in debug
  sports <- extract_nested_soccer_teams(data, c("sports"), list())
  total_teams <- 0

  if (is.data.frame(sports) && nrow(sports) > 0) {
    # Get leagues from sports (first row should be soccer)
    leagues_data <- sports$leagues[[1]]  # First sport should be soccer

    if (is.data.frame(leagues_data) && nrow(leagues_data) > 0) {
      first_league <- leagues_data[1, ]

      # Extract league info
      league_name <- as.character(first_league$name)
      league_abbreviation <- as.character(first_league$abbreviation)

      # Season information
      if ("season" %in% names(first_league) && is.data.frame(first_league$season)) {
        season_year <- as.character(first_league$season$year[1])
        if ("type" %in% names(first_league$season) && is.data.frame(first_league$season$type)) {
          season_type <- as.character(first_league$season$type$name[1])
        }
      }

      # Extract teams - the teams data is in first_league$teams[[1]]
      if ("teams" %in% names(first_league) && is.list(first_league$teams) && length(first_league$teams) > 0) {
        teams_list <- first_league$teams[[1]]

        if (is.data.frame(teams_list) && nrow(teams_list) > 0 && "team" %in% names(teams_list)) {
          # The team data is directly in teams_list$team (which is a data frame with all teams)
          team_data <- teams_list$team

          if (is.data.frame(team_data) && nrow(team_data) > 0) {
            total_teams <- nrow(team_data)

            for (i in seq_len(nrow(team_data))) {
              team_info <- team_data[i, ]

              team_id <- as.character(team_info$id)
              team_name <- as.character(team_info$name)
              team_display_name <- as.character(team_info$displayName)
              team_short_display_name <- as.character(team_info$shortDisplayName)
              team_abbreviation <- as.character(team_info$abbreviation)
              team_location <- as.character(team_info$location)
              team_nickname <- as.character(team_info$nickname)
              team_color <- as.character(team_info$color)
              team_alternate_color <- as.character(team_info$alternateColor)
              team_is_active <- as.character(team_info$isActive)

              # Extract logo (first logo URL from the list)
              team_logo <- NA_character_
              if ("logos" %in% names(team_info) && is.list(team_info$logos) && length(team_info$logos) > 0) {
                logo_df <- team_info$logos[[1]]
                if (is.data.frame(logo_df) && nrow(logo_df) > 0 && "href" %in% names(logo_df)) {
                  team_logo <- as.character(logo_df$href[1])
                }
              }

              # Create team row
              team_df_row <- data.frame(
                league_slug = as.character(league_slug),
                team_id = team_id,
                team_name = team_name,
                team_display_name = team_display_name,
                team_short_display_name = team_short_display_name,
                team_abbreviation = team_abbreviation,
                team_location = team_location,
                team_nickname = team_nickname,
                team_color = team_color,
                team_alternate_color = team_alternate_color,
                team_logo = team_logo,
                team_is_active = team_is_active,
                stringsAsFactors = FALSE
              )
              teams_df <- rbind(teams_df, team_df_row)
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
    total_teams = as.character(total_teams),
    stringsAsFactors = FALSE
  )
  league_info_df <- rbind(league_info_df, league_row)

  # Clean up row names
  if (nrow(league_info_df) > 0) rownames(league_info_df) <- NULL
  if (nrow(teams_df) > 0) rownames(teams_df) <- NULL

  return(list(
    league_info = league_info_df,
    teams = teams_df
  ))
}

#' Fetch soccer teams data using ESPN API
#'
#' Retrieves soccer team information from ESPN's API for a specific league.
#' The function fetches comprehensive team data including details, venues, and records.
#'
#' @param league_slug Character. ESPN league slug (required).
#'   Examples: "eng.1" (Premier League), "esp.1" (La Liga), "ger.1" (Bundesliga).
#'   Use \code{fetch_soccer_leagues()} to get available league slugs.
#' @param return_type Character. Type of data to return (default: "all").
#'   Options are:
#'   \itemize{
#'     \item "league_info" - Basic league and season information
#'     \item "teams" - Complete team list with details
#'     \item "all" - Both league info and teams
#'   }
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'soccer_teams_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data. The main purpose is global
#'   environment assignment based on return_type:
#'   \itemize{
#'     \item \code{soccer_teams_league_info} - League summary data frame
#'     \item \code{soccer_teams} - Complete teams list data frame
#'     \item \code{soccer_teams_raw} - Raw JSON response (if raw = TRUE)
#'   }
#'
#' @details
#' The function creates structured data frames with comprehensive team information:
#'
#' **League Info** (\code{soccer_teams_league_info}):
#' \itemize{
#'   \item League details: slug, name, abbreviation
#'   \item Season information: year, type
#'   \item Total teams count
#' }
#'
#' **Teams** (\code{soccer_teams}):
#' \itemize{
#'   \item Team identity: ID, name, display name, abbreviation
#'   \item Team details: location, nickname, colors, logo
#'   \item Status: active/inactive status
#'   \item Venue: stadium ID, name, city, capacity
#'   \item Record: season record summary (wins-losses-draws)
#' }
#'
#' **Team Colors**:
#' Colors are provided as hex codes (e.g., "ff0000" for red) for use in visualizations
#'
#' **Venue Information**:
#' Includes stadium details like capacity and location for match analysis
#'
#' @examples
#' \dontrun{
#' # Get Premier League teams
#' fetch_soccer_teams("eng.1")
#'
#' # Get La Liga teams
#' fetch_soccer_teams("esp.1")
#'
#' # Get Bundesliga teams
#' fetch_soccer_teams("ger.1")
#'
#' # Get Champions League teams (tournament format)
#' fetch_soccer_teams("uefa.champions")
#'
#' # Check what data was created
#' head(soccer_teams_league_info)
#' head(soccer_teams)
#'
#' # Get only teams data
#' fetch_soccer_teams("eng.1", return_type = "teams")
#'
#' # Find specific team
#' liverpool <- soccer_teams[
#'   grepl("Liverpool", soccer_teams$team_display_name),
#' ]
#'
#' # Find teams by city
#' london_teams <- soccer_teams[
#'   grepl("London", soccer_teams$venue_city),
#' ]
#'
#' # Find teams by nickname
#' united_teams <- soccer_teams[
#'   grepl("United", soccer_teams$team_name),
#' ]
#'
#' # Analyze venue capacities
#' venues_with_capacity <- soccer_teams[
#'   !is.na(soccer_teams$venue_capacity) &
#'   soccer_teams$venue_capacity != "",
#' ]
#' venues_with_capacity$capacity_num <- as.numeric(venues_with_capacity$venue_capacity)
#' largest_stadiums <- venues_with_capacity[
#'   order(-venues_with_capacity$capacity_num),
#' ]
#'
#' # Find teams with good records
#' teams_with_records <- soccer_teams[
#'   !is.na(soccer_teams$record_wins) &
#'   soccer_teams$record_wins != "",
#' ]
#' teams_with_records$wins_num <- as.numeric(teams_with_records$record_wins)
#' most_wins <- teams_with_records[
#'   order(-teams_with_records$wins_num),
#' ]
#'
#' # Color analysis for visualizations
#' team_colors <- soccer_teams[
#'   !is.na(soccer_teams$team_color) &
#'   soccer_teams$team_color != "",
#'   c("team_display_name", "team_color", "team_alternate_color")
#' ]
#'
#' # Find active teams
#' active_teams <- soccer_teams[
#'   soccer_teams$team_is_active == "TRUE",
#' ]
#'
#' # Stadium analysis
#' unique_venues <- soccer_teams[
#'   !is.na(soccer_teams$venue_name) &
#'   soccer_teams$venue_name != "",
#'   c("venue_name", "venue_city", "venue_capacity")
#' ]
#' unique_venues <- unique_venues[!duplicated(unique_venues$venue_name), ]
#'
#' # Team abbreviations for quick reference
#' team_abbrevs <- soccer_teams[
#'   c("team_display_name", "team_abbreviation", "team_id")
#' ]
#'
#' # Get raw data for debugging
#' fetch_soccer_teams("eng.1", raw = TRUE)
#' }
#'
#' @seealso
#' \code{\link{fetch_soccer_leagues}} to get available league slugs
#' \code{\link{fetch_soccer_standings}} for current team positions
#' \code{\link{fetch_soccer_team_schedule}} for individual team schedules
#'
#' Common league slugs:
#' \itemize{
#'   \item \code{eng.1} - English Premier League
#'   \item \code{esp.1} - Spanish La Liga
#'   \item \code{ger.1} - German Bundesliga
#'   \item \code{ita.1} - Italian Serie A
#'   \item \code{fra.1} - French Ligue 1
#'   \item \code{usa.1} - Major League Soccer (MLS)
#'   \item \code{uefa.champions} - UEFA Champions League
#'   \item \code{uefa.europa} - UEFA Europa League
#' }
#'
#' @importFrom httr GET status_code content timeout
#' @importFrom jsonlite fromJSON
#' @export
fetch_soccer_teams <- function(league_slug, return_type = "all", raw = FALSE) {
  # Input validation
  if (missing(league_slug) || is.null(league_slug) || league_slug == "") {
    stop("'league_slug' is required. Use fetch_soccer_leagues() to get available league slugs.")
  }

  valid_types <- c("league_info", "teams", "all")
  if (!return_type %in% valid_types) {
    stop(sprintf("'return_type' must be one of: %s", paste(valid_types, collapse = ", ")))
  }

  if (!is.logical(raw)) {
    stop("'raw' parameter must be logical (TRUE or FALSE)")
  }

  # Build API URL
  api_url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/soccer/%s/teams", league_slug)

  message(sprintf("Fetching soccer teams for %s...", league_slug))

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
      assign("soccer_teams_raw", data, envir = .GlobalEnv)
      message("Raw soccer teams data assigned to: soccer_teams_raw")
      message("Available sections: ", paste(names(data), collapse = ", "))

      # Show structure info
      if ("sports" %in% names(data)) {
        sports <- data$sports
        if (is.data.frame(sports) && nrow(sports) > 0) {
          message("- Sports found: ", nrow(sports))
          if ("leagues" %in% names(sports) && length(sports$leagues) > 0) {
            leagues <- sports$leagues[[1]]
            if (is.data.frame(leagues) && nrow(leagues) > 0) {
              message("- Leagues found: ", nrow(leagues))
              if ("teams" %in% names(leagues) && length(leagues$teams) > 0) {
                teams_list <- leagues$teams[[1]]
                if (is.data.frame(teams_list)) {
                  message("- Teams found: ", nrow(teams_list))
                }
              }
            }
          }
        }
      }

      return(invisible(data))
    }

    # Create datasets
    datasets <- create_soccer_teams_datasets(data, league_slug)

    # Assign data based on return_type
    result_data <- list()

    if (return_type %in% c("league_info", "all")) {
      assign("soccer_teams_league_info", datasets$league_info, envir = .GlobalEnv)
      result_data$league_info <- datasets$league_info
      message(sprintf("Soccer teams league info assigned to: soccer_teams_league_info (%d records)",
                      nrow(datasets$league_info)))
    }

    if (return_type %in% c("teams", "all")) {
      assign("soccer_teams", datasets$teams, envir = .GlobalEnv)
      result_data$teams <- datasets$teams
      message(sprintf("Soccer teams assigned to: soccer_teams (%d teams)",
                      nrow(datasets$teams)))

      # Show sample team information
      if (nrow(datasets$teams) > 0) {
        # Show sample teams
        sample_teams <- head(datasets$teams, 5)
        message("Sample teams:")
        for (i in seq_len(nrow(sample_teams))) {
          team <- sample_teams[i, ]
          message(sprintf("  - %s (%s) - ID: %s",
                          team$team_display_name, team$team_abbreviation, team$team_id))
        }

        # Show venue information
        venues_count <- sum(!is.na(datasets$teams$venue_name) & datasets$teams$venue_name != "")
        message(sprintf("Teams with venue info: %d", venues_count))

        # Show record information
        records_count <- sum(!is.na(datasets$teams$record_summary) & datasets$teams$record_summary != "")
        message(sprintf("Teams with record info: %d", records_count))

        # Show active teams
        active_count <- sum(datasets$teams$team_is_active == "TRUE", na.rm = TRUE)
        message(sprintf("Active teams: %d", active_count))

        # Show cities represented
        unique_cities <- unique(datasets$teams$venue_city[!is.na(datasets$teams$venue_city)])
        unique_cities <- unique_cities[unique_cities != ""]
        if (length(unique_cities) > 0) {
          sample_cities <- head(unique_cities, 5)
          message("Sample cities: ", paste(sample_cities, collapse = ", "))
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
    stop(sprintf("Failed to fetch soccer teams for %s: %s", league_slug, e$message))
  })
}

#' Fetch multiple soccer league teams
#'
#' Retrieves team information for multiple soccer leagues with rate limiting to
#' be respectful to ESPN's API. This function calls \code{\link{fetch_soccer_teams}}
#' for each league and combines the results.
#'
#' @param league_slugs Character vector. ESPN league slugs.
#'   Examples: c("eng.1", "esp.1", "ger.1") for Premier League, La Liga, Bundesliga.
#' @param return_type Character. Type of data to return (default: "all").
#'   Same options as \code{\link{fetch_soccer_teams}}.
#' @param delay Numeric. Delay in seconds between API requests (default: 0.5).
#'   Used to be respectful to ESPN's servers and avoid rate limiting.
#' @param raw Logical. If TRUE, assigns raw JSON to global environment
#'   for the first league only (default: FALSE).
#'
#' @return Invisibly returns the combined data frames. The main purpose is global
#'   environment assignment of combined datasets from all leagues.
#'
#' @details
#' The function processes leagues sequentially with a configurable delay
#' between requests. Failed requests for individual leagues are logged but
#' do not stop the overall process. The final datasets contain data from
#' all successfully processed leagues.
#'
#' @examples
#' \dontrun{
#' # Get teams for major European leagues
#' major_leagues <- c("eng.1", "esp.1", "ger.1", "ita.1", "fra.1")
#' fetch_multiple_soccer_teams(major_leagues)
#'
#' # Get teams only
#' fetch_multiple_soccer_teams(major_leagues, return_type = "teams")
#'
#' # Use longer delay for larger requests
#' fetch_multiple_soccer_teams(major_leagues, delay = 1.0)
#'
#' # Analyze combined results
#' unique_leagues <- unique(soccer_teams$league_slug)
#' cat("Retrieved teams for", length(unique_leagues), "leagues\n")
#'
#' # Find all teams named "United"
#' united_teams <- soccer_teams[
#'   grepl("United", soccer_teams$team_name),
#' ]
#'
#' # Compare stadium capacities across leagues
#' stadiums <- soccer_teams[
#'   !is.na(soccer_teams$venue_capacity) &
#'   soccer_teams$venue_capacity != "",
#' ]
#' stadiums$capacity_num <- as.numeric(stadiums$venue_capacity)
#' largest_stadiums <- stadiums[order(-stadiums$capacity_num), ]
#'
#' # Analyze team colors across leagues
#' team_colors <- soccer_teams[
#'   !is.na(soccer_teams$team_color),
#'   c("league_slug", "team_display_name", "team_color")
#' ]
#' }
#'
#' @seealso \code{\link{fetch_soccer_teams}} for single league data
#' @importFrom httr GET status_code content timeout
#' @export
fetch_multiple_soccer_teams <- function(league_slugs, return_type = "all",
                                        delay = 0.5, raw = FALSE) {
  # Input validation
  if (missing(league_slugs) || length(league_slugs) == 0) {
    stop("'league_slugs' must contain at least one league slug")
  }

  valid_types <- c("league_info", "teams", "all")
  if (!return_type %in% valid_types) {
    stop(sprintf("'return_type' must be one of: %s", paste(valid_types, collapse = ", ")))
  }

  if (!is.numeric(delay) || delay < 0) {
    stop("'delay' must be a non-negative number")
  }

  if (!is.logical(raw)) {
    stop("'raw' parameter must be logical (TRUE or FALSE)")
  }

  # Initialize combined data containers
  all_league_info <- data.frame()
  all_teams <- data.frame()

  message(sprintf("Starting to fetch soccer teams for %d leagues...", length(league_slugs)))

  # Process each league sequentially
  for (i in seq_along(league_slugs)) {
    league_slug <- league_slugs[i]
    message(sprintf("Fetching teams for %s (%d/%d)...", league_slug, i, length(league_slugs)))

    tryCatch({
      # Fetch individual league data
      teams_data <- fetch_soccer_teams(
        league_slug = league_slug,
        return_type = return_type,
        raw = raw
      )

      # If raw data requested, return after first league
      if (isTRUE(raw)) {
        return(invisible(teams_data))
      }

      # Combine data based on return type
      if (return_type %in% c("league_info", "all")) {
        league_info_df <- get("soccer_teams_league_info", envir = .GlobalEnv)
        all_league_info <- rbind(all_league_info, league_info_df)
      }

      if (return_type %in% c("teams", "all")) {
        teams_df <- get("soccer_teams", envir = .GlobalEnv)
        all_teams <- rbind(all_teams, teams_df)
      }

    }, error = function(e) {
      message(sprintf("Failed to fetch teams for %s: %s", league_slug, e$message))
    })

    # Be respectful to the API with delay between requests
    if (i < length(league_slugs)) {
      Sys.sleep(delay)
    }
  }

  # Remove duplicates and assign combined datasets to global environment
  result_data <- list()

  if (return_type %in% c("league_info", "all") && nrow(all_league_info) > 0) {
    all_league_info <- all_league_info[!duplicated(all_league_info$league_slug), ]
    assign("soccer_teams_league_info", all_league_info, envir = .GlobalEnv)
    result_data$league_info <- all_league_info
    message(sprintf("Combined soccer teams league info assigned to: soccer_teams_league_info (%d leagues)",
                    nrow(all_league_info)))
  }

  if (return_type %in% c("teams", "all") && nrow(all_teams) > 0) {
    all_teams <- all_teams[!duplicated(paste(all_teams$league_slug, all_teams$team_id)), ]
    assign("soccer_teams", all_teams, envir = .GlobalEnv)
    result_data$teams <- all_teams
    unique_leagues <- length(unique(all_teams$league_slug))
    total_teams <- nrow(all_teams)
    message(sprintf("Combined soccer teams assigned to: soccer_teams (%d leagues, %d teams)",
                    unique_leagues, total_teams))

    # Show combined analytics
    if (nrow(all_teams) > 0) {
      # Show league breakdown
      league_counts <- table(all_teams$league_slug)
      message("Teams per league:")
      for (league in names(league_counts)) {
        message(sprintf("  - %s: %d", league, league_counts[league]))
      }

      # Show total active teams
      active_count <- sum(all_teams$team_is_active == "TRUE", na.rm = TRUE)
      message(sprintf("Total active teams: %d", active_count))

      # Show total unique venues
      unique_venues <- length(unique(all_teams$venue_name[!is.na(all_teams$venue_name) & all_teams$venue_name != ""]))
      message(sprintf("Total unique venues: %d", unique_venues))

      # Show total unique cities
      unique_cities <- length(unique(all_teams$venue_city[!is.na(all_teams$venue_city) & all_teams$venue_city != ""]))
      message(sprintf("Total unique cities: %d", unique_cities))
    }
  }

  # Return appropriate data structure
  if (return_type == "all") {
    return(invisible(result_data))
  } else {
    return(invisible(result_data[[return_type]]))
  }
}
