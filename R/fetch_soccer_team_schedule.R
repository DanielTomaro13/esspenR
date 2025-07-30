#' Safe nested data extraction helper function for soccer team schedule
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_soccer_schedule <- function(data, path, default = NA) {
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

#' Create soccer team schedule data frames from ESPN API response
#'
#' Processes raw JSON response from ESPN soccer team schedule API into structured data frames
#' containing detailed match schedule information
#'
#' @param data Raw JSON response from ESPN soccer team schedule API
#' @param league_slug Character. League slug used in request
#' @param team_id Character. Team ID used in request
#' @return List containing multiple data frames with schedule information
#' @keywords internal
create_soccer_team_schedule_datasets <- function(data, league_slug, team_id) {
  # Initialize data frames
  team_info_df <- data.frame(
    league_slug = character(0),
    team_id = character(0),
    team_name = character(0),
    team_display_name = character(0),
    team_abbreviation = character(0),
    team_location = character(0),
    team_color = character(0),
    team_logo = character(0),
    season_year = character(0),
    season_type = character(0),
    total_events = character(0),
    stringsAsFactors = FALSE
  )

  schedule_df <- data.frame(
    league_slug = character(0),
    team_id = character(0),
    event_id = character(0),
    event_name = character(0),
    event_short_name = character(0),
    event_date = character(0),
    event_status = character(0),
    event_status_detail = character(0),
    is_home_game = character(0),
    opponent_team_id = character(0),
    opponent_team_name = character(0),
    opponent_team_abbreviation = character(0),
    team_score = character(0),
    opponent_score = character(0),
    result = character(0),
    venue_name = character(0),
    venue_city = character(0),
    attendance = character(0),
    clock = character(0),
    competition_name = character(0),
    season_type = character(0),
    stringsAsFactors = FALSE
  )

  # Extract team information
  team_info <- extract_nested_soccer_schedule(data, c("team"), list())
  team_name <- team_display_name <- team_abbreviation <- team_location <- NA_character_
  team_color <- team_logo <- season_year <- season_type <- NA_character_

  if (is.list(team_info) && length(team_info) > 0) {
    team_name <- extract_nested_soccer_schedule(team_info, c("name"), NA_character_)
    team_display_name <- extract_nested_soccer_schedule(team_info, c("displayName"), NA_character_)
    team_abbreviation <- extract_nested_soccer_schedule(team_info, c("abbreviation"), NA_character_)
    team_location <- extract_nested_soccer_schedule(team_info, c("location"), NA_character_)
    team_color <- extract_nested_soccer_schedule(team_info, c("color"), NA_character_)
    team_logo <- extract_nested_soccer_schedule(team_info, c("logo"), NA_character_)
  }

  # Extract season information
  season_info <- extract_nested_soccer_schedule(data, c("season"), list())
  if (is.list(season_info) && length(season_info) > 0) {
    season_year <- extract_nested_soccer_schedule(season_info, c("year"), NA_character_)
    season_type <- extract_nested_soccer_schedule(season_info, c("name"), NA_character_)
  }

  # Extract events (matches)
  events <- extract_nested_soccer_schedule(data, c("events"), list())
  total_events <- if (is.data.frame(events)) nrow(events) else 0

  # Create team info row
  team_row <- data.frame(
    league_slug = as.character(league_slug),
    team_id = as.character(team_id),
    team_name = as.character(team_name),
    team_display_name = as.character(team_display_name),
    team_abbreviation = as.character(team_abbreviation),
    team_location = as.character(team_location),
    team_color = as.character(team_color),
    team_logo = as.character(team_logo),
    season_year = as.character(season_year),
    season_type = as.character(season_type),
    total_events = as.character(total_events),
    stringsAsFactors = FALSE
  )
  team_info_df <- rbind(team_info_df, team_row)

  # Process each match/event
  if (is.data.frame(events) && nrow(events) > 0) {
    for (i in seq_len(nrow(events))) {
      event <- events[i, ]

      # Basic event information
      event_id <- as.character(event$id)
      event_name <- as.character(event$name)
      event_short_name <- as.character(event$shortName)
      event_date <- as.character(event$date)

      # Event status
      event_status <- event_status_detail <- clock <- NA_character_
      if ("status" %in% names(event) && is.data.frame(event$status)) {
        if ("type" %in% names(event$status) && is.data.frame(event$status$type)) {
          event_status <- as.character(event$status$type$name[1])
          event_status_detail <- as.character(event$status$type$detail[1])
        }
        clock <- as.character(event$status$displayClock[1])
      }

      # Initialize variables
      venue_name <- venue_city <- attendance <- competition_name <- NA_character_
      is_home_game <- opponent_team_id <- opponent_team_name <- opponent_team_abbreviation <- NA_character_
      team_score <- opponent_score <- result <- NA_character_

      # Extract competition data
      if ("competitions" %in% names(event) && is.list(event$competitions) && length(event$competitions) > 0) {
        competition <- event$competitions[[1]]

        # Competition name
        if ("type" %in% names(competition) && is.data.frame(competition$type)) {
          competition_name <- as.character(competition$type$name[1])
        }

        # Venue information
        if ("venue" %in% names(competition) && is.data.frame(competition$venue)) {
          venue_name <- as.character(competition$venue$fullName[1])
          if ("address" %in% names(competition$venue) && is.data.frame(competition$venue$address)) {
            venue_city <- as.character(competition$venue$address$city[1])
          }
        }

        # Attendance
        if ("attendance" %in% names(competition)) {
          attendance <- as.character(competition$attendance)
        }

        # Team information and scores
        if ("competitors" %in% names(competition) && is.data.frame(competition$competitors)) {
          competitors <- competition$competitors

          for (j in seq_len(nrow(competitors))) {
            competitor <- competitors[j, ]

            # Team details
            competitor_team_id <- as.character(competitor$id[1])
            home_away <- as.character(competitor$homeAway[1])
            score <- as.character(competitor$score[1])

            # Check if this is our team or the opponent
            if (competitor_team_id == team_id) {
              # This is our team
              team_score <- score
              is_home_game <- if (home_away == "home") "TRUE" else "FALSE"
            } else {
              # This is the opponent
              opponent_team_id <- competitor_team_id
              opponent_score <- score

              # Get opponent team details
              if ("team" %in% names(competitor) && is.data.frame(competitor$team)) {
                opponent_team_name <- as.character(competitor$team$displayName[1])
                opponent_team_abbreviation <- as.character(competitor$team$abbreviation[1])
              }
            }
          }

          # Determine result (W/L/D) for completed matches
          if (event_status == "STATUS_FINAL" && !is.na(team_score) && !is.na(opponent_score)) {
            team_score_num <- as.numeric(team_score)
            opponent_score_num <- as.numeric(opponent_score)

            if (!is.na(team_score_num) && !is.na(opponent_score_num)) {
              if (team_score_num > opponent_score_num) {
                result <- "W"
              } else if (team_score_num < opponent_score_num) {
                result <- "L"
              } else {
                result <- "D"
              }
            }
          }
        }
      }

      # Create schedule row
      schedule_row <- data.frame(
        league_slug = as.character(league_slug),
        team_id = as.character(team_id),
        event_id = event_id,
        event_name = event_name,
        event_short_name = event_short_name,
        event_date = event_date,
        event_status = event_status,
        event_status_detail = event_status_detail,
        is_home_game = is_home_game,
        opponent_team_id = opponent_team_id,
        opponent_team_name = opponent_team_name,
        opponent_team_abbreviation = opponent_team_abbreviation,
        team_score = team_score,
        opponent_score = opponent_score,
        result = result,
        venue_name = venue_name,
        venue_city = venue_city,
        attendance = attendance,
        clock = clock,
        competition_name = competition_name,
        season_type = as.character(season_type),
        stringsAsFactors = FALSE
      )
      schedule_df <- rbind(schedule_df, schedule_row)
    }
  }

  # Clean up row names
  if (nrow(team_info_df) > 0) rownames(team_info_df) <- NULL
  if (nrow(schedule_df) > 0) rownames(schedule_df) <- NULL

  return(list(
    team_info = team_info_df,
    schedule = schedule_df
  ))
}

#' Fetch soccer team schedule data using ESPN API
#'
#' Retrieves soccer team schedule and match history from ESPN's API for a specific team.
#' The function fetches comprehensive schedule data including past results and upcoming fixtures.
#'
#' @param league_slug Character. ESPN league slug (required).
#'   Examples: "eng.1" (Premier League), "esp.1" (La Liga), "uefa.champions" (Champions League).
#'   Use \code{fetch_soccer_leagues()} to get available league slugs.
#' @param team_id Character or Numeric. ESPN team ID (required).
#'   The unique identifier for the team in ESPN's database.
#'   Use scoreboard data or team endpoints to find team IDs.
#' @param season Character. Season year (optional).
#'   Format: "YYYY" (e.g., "2024"). If not specified, returns current season.
#' @param return_type Character. Type of data to return (default: "all").
#'   Options are:
#'   \itemize{
#'     \item "team_info" - Basic team information and season details
#'     \item "schedule" - Complete schedule with match details
#'     \item "all" - Both team info and schedule
#'   }
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'soccer_team_schedule_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data. The main purpose is global
#'   environment assignment based on return_type:
#'   \itemize{
#'     \item \code{soccer_team_schedule_info} - Team information data frame
#'     \item \code{soccer_team_schedule} - Complete schedule data frame
#'     \item \code{soccer_team_schedule_raw} - Raw JSON response (if raw = TRUE)
#'   }
#'
#' @details
#' The function creates structured data frames with comprehensive team schedule information:
#'
#' **Team Info** (\code{soccer_team_schedule_info}):
#' \itemize{
#'   \item Team details: ID, name, display name, abbreviation
#'   \item Team branding: location, color, logo URL
#'   \item Season information: year, type, total events
#' }
#'
#' **Schedule** (\code{soccer_team_schedule}):
#' \itemize{
#'   \item Match details: event_id, name, short_name, date, status
#'   \item Game context: home/away indicator, opponent details
#'   \item Results: team score, opponent score, result (W/L/D)
#'   \item Venue: stadium name, city, attendance
#'   \item Competition: tournament/league name
#'   \item Timing: clock, status details
#' }
#'
#' **Match Status Examples**:
#' \itemize{
#'   \item "STATUS_SCHEDULED" - Upcoming match
#'   \item "STATUS_IN_PROGRESS" - Live match
#'   \item "STATUS_FINAL" - Completed match
#'   \item "STATUS_POSTPONED" - Postponed match
#' }
#'
#' @examples
#' \dontrun{
#' # Get Liverpool's Premier League schedule (team ID: 364)
#' fetch_soccer_team_schedule("eng.1", "364")
#'
#' # Get Manchester City's schedule for specific season
#' fetch_soccer_team_schedule("eng.1", "382", season = "2024")
#'
#' # Get Barcelona's La Liga schedule
#' fetch_soccer_team_schedule("esp.1", "83")
#'
#' # Get Real Madrid's Champions League schedule
#' fetch_soccer_team_schedule("uefa.champions", "86")
#'
#' # Check what data was created
#' head(soccer_team_schedule_info)
#' head(soccer_team_schedule)
#'
#' # Get only schedule data
#' fetch_soccer_team_schedule("eng.1", "364", return_type = "schedule")
#'
#' # Analyze team performance
#' completed_matches <- soccer_team_schedule[
#'   soccer_team_schedule$event_status == "STATUS_FINAL",
#' ]
#'
#' # Count wins, losses, draws
#' results_summary <- table(completed_matches$result)
#' print(results_summary)
#'
#' # Find home vs away performance
#' home_matches <- soccer_team_schedule[
#'   soccer_team_schedule$is_home_game == "TRUE" &
#'   soccer_team_schedule$event_status == "STATUS_FINAL",
#' ]
#' away_matches <- soccer_team_schedule[
#'   soccer_team_schedule$is_home_game == "FALSE" &
#'   soccer_team_schedule$event_status == "STATUS_FINAL",
#' ]
#'
#' # Find upcoming fixtures
#' upcoming_matches <- soccer_team_schedule[
#'   soccer_team_schedule$event_status == "STATUS_SCHEDULED",
#' ]
#'
#' # Find matches against specific opponent
#' rival_matches <- soccer_team_schedule[
#'   grepl("Manchester United", soccer_team_schedule$opponent_team_name),
#' ]
#'
#' # Find high-scoring matches
#' high_scoring <- soccer_team_schedule[
#'   !is.na(soccer_team_schedule$team_score) &
#'   !is.na(soccer_team_schedule$opponent_score) &
#'   (as.numeric(soccer_team_schedule$team_score) +
#'    as.numeric(soccer_team_schedule$opponent_score)) >= 4,
#' ]
#'
#' # Find matches by venue
#' anfield_matches <- soccer_team_schedule[
#'   grepl("Anfield", soccer_team_schedule$venue_name),
#' ]
#'
#' # Calculate goal statistics
#' completed <- soccer_team_schedule[
#'   soccer_team_schedule$event_status == "STATUS_FINAL" &
#'   !is.na(soccer_team_schedule$team_score),
#' ]
#' goals_scored <- sum(as.numeric(completed$team_score), na.rm = TRUE)
#' goals_conceded <- sum(as.numeric(completed$opponent_score), na.rm = TRUE)
#'
#' # Get raw data for debugging
#' fetch_soccer_team_schedule("eng.1", "364", raw = TRUE)
#' }
#'
#' @seealso
#' \code{\link{fetch_soccer_leagues}} to get available league slugs
#' \code{\link{fetch_soccer_scoreboard}} to find team IDs from current matches
#'
#' Common team IDs for major clubs:
#' \itemize{
#'   \item \code{364} - Liverpool FC
#'   \item \code{382} - Manchester City
#'   \item \code{360} - Manchester United
#'   \item \code{359} - Chelsea FC
#'   \item \code{361} - Arsenal FC
#'   \item \code{83} - FC Barcelona
#'   \item \code{86} - Real Madrid
#'   \item \code{80} - Bayern Munich
#' }
#'
#' @importFrom httr GET status_code content timeout
#' @importFrom jsonlite fromJSON
#' @export
fetch_soccer_team_schedule <- function(league_slug, team_id, season = NULL, return_type = "all", raw = FALSE) {
  # Input validation
  if (missing(league_slug) || is.null(league_slug) || league_slug == "") {
    stop("'league_slug' is required. Use fetch_soccer_leagues() to get available league slugs.")
  }

  if (missing(team_id) || is.null(team_id) || team_id == "") {
    stop("'team_id' is required. Use scoreboard data to find team IDs.")
  }

  valid_types <- c("team_info", "schedule", "all")
  if (!return_type %in% valid_types) {
    stop(sprintf("'return_type' must be one of: %s", paste(valid_types, collapse = ", ")))
  }

  if (!is.logical(raw)) {
    stop("'raw' parameter must be logical (TRUE or FALSE)")
  }

  # Convert team_id to character for URL building
  team_id <- as.character(team_id)

  # Build API URL
  base_url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/soccer/%s/teams/%s/schedule",
                      league_slug, team_id)

  # Add season parameter if provided
  if (!is.null(season) && season != "") {
    url <- sprintf("%s?season=%s", base_url, season)
    message(sprintf("Fetching soccer team schedule for %s in %s (season: %s)...", team_id, league_slug, season))
  } else {
    url <- base_url
    message(sprintf("Fetching soccer team schedule for %s in %s...", team_id, league_slug))
  }

  # Fetch and parse data
  tryCatch({
    resp <- httr::GET(url, httr::timeout(30))
    if (httr::status_code(resp) != 200) {
      stop(sprintf("API request failed: HTTP %d - %s",
                   httr::status_code(resp),
                   httr::http_status(resp)$message))
    }

    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content_text, simplifyVector = TRUE, simplifyDataFrame = TRUE)

    # Handle raw data assignment for debugging
    if (isTRUE(raw)) {
      assign("soccer_team_schedule_raw", data, envir = .GlobalEnv)
      message("Raw soccer team schedule data assigned to: soccer_team_schedule_raw")
      message("Data structure preview:")

      # Show available sections
      sections <- names(data)
      message("- Available sections: ", paste(sections, collapse = ", "))

      # Show team info if available
      if ("team" %in% names(data)) {
        team_info <- data$team
        if (is.list(team_info)) {
          team_name <- extract_nested_soccer_schedule(team_info, c("displayName"), "Unknown")
          message("- Team: ", team_name)
        }
      }

      # Show events info if available
      if ("events" %in% names(data)) {
        events <- data$events
        if (is.data.frame(events)) {
          message("- Total matches found: ", nrow(events))
        }
      }

      return(invisible(data))
    }

    # Create datasets
    datasets <- create_soccer_team_schedule_datasets(data, league_slug, team_id)

    # Assign data based on return_type
    result_data <- list()

    if (return_type %in% c("team_info", "all")) {
      assign("soccer_team_schedule_info", datasets$team_info, envir = .GlobalEnv)
      result_data$team_info <- datasets$team_info
      message(sprintf("Soccer team schedule info assigned to: soccer_team_schedule_info (%d records)",
                      nrow(datasets$team_info)))
    }

    if (return_type %in% c("schedule", "all")) {
      assign("soccer_team_schedule", datasets$schedule, envir = .GlobalEnv)
      result_data$schedule <- datasets$schedule
      message(sprintf("Soccer team schedule assigned to: soccer_team_schedule (%d matches)",
                      nrow(datasets$schedule)))

      # Show sample schedule information
      if (nrow(datasets$schedule) > 0) {
        # Show status breakdown
        status_counts <- table(datasets$schedule$event_status)
        if (length(status_counts) > 0) {
          message("Match status breakdown:")
          for (status in names(status_counts)) {
            message(sprintf("  - %s: %d", status, status_counts[status]))
          }
        }

        # Show results breakdown for completed matches
        completed_matches <- datasets$schedule[datasets$schedule$event_status == "STATUS_FINAL", ]
        if (nrow(completed_matches) > 0) {
          results_counts <- table(completed_matches$result)
          if (length(results_counts) > 0) {
            message("Results breakdown (completed matches):")
            for (result in names(results_counts)) {
              message(sprintf("  - %s: %d", result, results_counts[result]))
            }
          }
        }

        # Show home vs away breakdown
        home_count <- sum(datasets$schedule$is_home_game == "TRUE", na.rm = TRUE)
        away_count <- sum(datasets$schedule$is_home_game == "FALSE", na.rm = TRUE)
        message(sprintf("Home matches: %d, Away matches: %d", home_count, away_count))

        # Show sample opponents
        opponents <- unique(datasets$schedule$opponent_team_name)
        opponents <- opponents[!is.na(opponents) & opponents != ""]
        if (length(opponents) > 0) {
          sample_opponents <- head(opponents, 5)
          message("Sample opponents: ", paste(sample_opponents, collapse = ", "))
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
    stop(sprintf("Failed to fetch soccer team schedule for team %s in %s: %s", team_id, league_slug, e$message))
  })
}

#' Fetch multiple soccer team schedules
#'
#' Retrieves schedule data for multiple soccer teams with rate limiting to
#' be respectful to ESPN's API. This function calls \code{\link{fetch_soccer_team_schedule}}
#' for each team and combines the results.
#'
#' @param teams_data Data frame or list with team information.
#'   Must contain 'league_slug' and 'team_id' columns/elements.
#'   Can also contain optional 'season' column/element.
#' @param return_type Character. Type of data to return (default: "all").
#'   Same options as \code{\link{fetch_soccer_team_schedule}}.
#' @param delay Numeric. Delay in seconds between API requests (default: 0.5).
#'   Used to be respectful to ESPN's servers and avoid rate limiting.
#' @param raw Logical. If TRUE, assigns raw JSON to global environment
#'   for the first team only (default: FALSE).
#'
#' @return Invisibly returns the combined data frames. The main purpose is global
#'   environment assignment of combined datasets from all teams.
#'
#' @details
#' The function processes teams sequentially with a configurable delay
#' between requests. Failed requests for individual teams are logged but
#' do not stop the overall process. The final datasets contain data from
#' all successfully processed teams.
#'
#' @examples
#' \dontrun{
#' # Create team data frame for multiple Premier League teams
#' teams_df <- data.frame(
#'   league_slug = c("eng.1", "eng.1", "eng.1"),
#'   team_id = c("364", "382", "360"),
#'   season = c("2024", "2024", "2024")
#' )
#' fetch_multiple_soccer_team_schedules(teams_df)
#'
#' # Get schedules for teams across different leagues
#' teams_df <- data.frame(
#'   league_slug = c("eng.1", "esp.1", "ger.1"),
#'   team_id = c("364", "83", "80")
#' )
#' fetch_multiple_soccer_team_schedules(teams_df)
#'
#' # Get schedules only
#' fetch_multiple_soccer_team_schedules(teams_df, return_type = "schedule")
#'
#' # Use longer delay for larger requests
#' fetch_multiple_soccer_team_schedules(teams_df, delay = 1.0)
#'
#' # Analyze combined results
#' unique_teams <- unique(soccer_team_schedule$team_id)
#' cat("Retrieved schedules for", length(unique_teams), "teams\n")
#'
#' # Compare team performance
#' completed_matches <- soccer_team_schedule[
#'   soccer_team_schedule$event_status == "STATUS_FINAL",
#' ]
#'
#' team_performance <- aggregate(
#'   completed_matches$result,
#'   by = list(team = completed_matches$team_id),
#'   FUN = table
#' )
#' }
#'
#' @seealso \code{\link{fetch_soccer_team_schedule}} for single team data
#' @importFrom httr GET status_code content timeout
#' @export
fetch_multiple_soccer_team_schedules <- function(teams_data, return_type = "all",
                                                 delay = 0.5, raw = FALSE) {
  # Input validation
  if (missing(teams_data) || length(teams_data) == 0) {
    stop("'teams_data' must contain team information")
  }

  # Convert to data frame if it's a list
  if (is.list(teams_data) && !is.data.frame(teams_data)) {
    teams_data <- do.call(rbind, teams_data)
  }

  # Check required columns
  if (!is.data.frame(teams_data) ||
      !"league_slug" %in% names(teams_data) ||
      !"team_id" %in% names(teams_data)) {
    stop("'teams_data' must be a data frame with 'league_slug' and 'team_id' columns")
  }

  valid_types <- c("team_info", "schedule", "all")
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
  all_team_info <- data.frame()
  all_schedules <- data.frame()

  message(sprintf("Starting to fetch soccer team schedules for %d teams...", nrow(teams_data)))

  # Process each team sequentially
  for (i in seq_len(nrow(teams_data))) {
    team_row <- teams_data[i, ]
    league_slug <- team_row$league_slug
    team_id <- team_row$team_id
    season <- if ("season" %in% names(team_row)) team_row$season else NULL

    message(sprintf("Fetching schedule for team %s in %s (%d/%d)...",
                    team_id, league_slug, i, nrow(teams_data)))

    tryCatch({
      # Fetch individual team data
      schedule_data <- fetch_soccer_team_schedule(
        league_slug = league_slug,
        team_id = team_id,
        season = season,
        return_type = return_type,
        raw = raw
      )

      # If raw data requested, return after first team
      if (isTRUE(raw)) {
        return(invisible(schedule_data))
      }

      # Combine data based on return type
      if (return_type %in% c("team_info", "all")) {
        team_info_df <- get("soccer_team_schedule_info", envir = .GlobalEnv)
        all_team_info <- rbind(all_team_info, team_info_df)
      }

      if (return_type %in% c("schedule", "all")) {
        schedule_df <- get("soccer_team_schedule", envir = .GlobalEnv)
        all_schedules <- rbind(all_schedules, schedule_df)
      }

    }, error = function(e) {
      message(sprintf("Failed to fetch schedule for team %s in %s: %s", team_id, league_slug, e$message))
    })

    # Be respectful to the API with delay between requests
    if (i < nrow(teams_data)) {
      Sys.sleep(delay)
    }
  }

  # Remove duplicates and assign combined datasets to global environment
  result_data <- list()

  if (return_type %in% c("team_info", "all") && nrow(all_team_info) > 0) {
    all_team_info <- all_team_info[!duplicated(paste(all_team_info$league_slug, all_team_info$team_id)), ]
    assign("soccer_team_schedule_info", all_team_info, envir = .GlobalEnv)
    result_data$team_info <- all_team_info
    message(sprintf("Combined soccer team schedule info assigned to: soccer_team_schedule_info (%d teams)",
                    nrow(all_team_info)))
  }

  if (return_type %in% c("schedule", "all") && nrow(all_schedules) > 0) {
    all_schedules <- all_schedules[!duplicated(paste(all_schedules$league_slug, all_schedules$team_id, all_schedules$event_id)), ]
    assign("soccer_team_schedule", all_schedules, envir = .GlobalEnv)
    result_data$schedule <- all_schedules
    unique_teams <- length(unique(paste(all_schedules$league_slug, all_schedules$team_id)))
    total_matches <- nrow(all_schedules)
    message(sprintf("Combined soccer team schedules assigned to: soccer_team_schedule (%d teams, %d matches)",
                    unique_teams, total_matches))

    # Show combined analytics
    if (nrow(all_schedules) > 0) {
      # Show status breakdown
      status_counts <- table(all_schedules$event_status)
      message("Combined match status breakdown:")
      for (status in names(status_counts)) {
        message(sprintf("  - %s: %d", status, status_counts[status]))
      }

      # Show results breakdown for completed matches
      completed_matches <- all_schedules[all_schedules$event_status == "STATUS_FINAL", ]
      if (nrow(completed_matches) > 0) {
        results_counts <- table(completed_matches$result)
        message("Combined results breakdown (completed matches):")
        for (result in names(results_counts)) {
          message(sprintf("  - %s: %d", result, results_counts[result]))
        }
      }

      # Show league breakdown
      league_counts <- table(all_schedules$league_slug)
      message("Matches per league:")
      for (league in names(league_counts)) {
        message(sprintf("  - %s: %d", league, league_counts[league]))
      }

      # Show total unique opponents
      unique_opponents <- length(unique(all_schedules$opponent_team_name[!is.na(all_schedules$opponent_team_name)]))
      message(sprintf("Total unique opponents: %d", unique_opponents))
    }
  }

  # Return appropriate data structure
  if (return_type == "all") {
    return(invisible(result_data))
  } else {
    return(invisible(result_data[[return_type]]))
  }
}
