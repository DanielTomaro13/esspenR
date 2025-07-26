#' Safe nested data extraction helper function for soccer scoreboard
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_soccer_scoreboard <- function(data, path, default = NA) {
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

#' Create soccer scoreboard data frames from ESPN API response
#'
#' Processes raw JSON response from ESPN soccer scoreboard API into structured data frames
#' containing detailed match information
#'
#' @param data Raw JSON response from ESPN soccer scoreboard API
#' @param league_slug Character. League slug used in request
#' @return List containing multiple data frames with scoreboard information
#' @keywords internal
create_soccer_scoreboard_datasets <- function(data, league_slug) {
  # Initialize data frames
  league_info_df <- data.frame(
    league_slug = character(0),
    league_name = character(0),
    league_abbreviation = character(0),
    season_year = character(0),
    season_type = character(0),
    date_range = character(0),
    total_events = character(0),
    stringsAsFactors = FALSE
  )

  matches_df <- data.frame(
    league_slug = character(0),
    event_id = character(0),
    event_name = character(0),
    event_short_name = character(0),
    event_date = character(0),
    event_status = character(0),
    event_status_detail = character(0),
    home_team_id = character(0),
    home_team_name = character(0),
    home_team_abbreviation = character(0),
    home_score = character(0),
    away_team_id = character(0),
    away_team_name = character(0),
    away_team_abbreviation = character(0),
    away_score = character(0),
    venue_name = character(0),
    venue_city = character(0),
    attendance = character(0),
    clock = character(0),
    home_odds = character(0),
    away_odds = character(0),
    draw_odds = character(0),
    stringsAsFactors = FALSE
  )

  # Extract league information
  leagues_data <- extract_nested_soccer_scoreboard(data, c("leagues"), list())
  league_name <- league_abbreviation <- season_year <- season_type <- NA_character_

  if (is.data.frame(leagues_data) && nrow(leagues_data) > 0) {
    first_league <- leagues_data[1, ]
    league_name <- first_league$name
    league_abbreviation <- first_league$abbreviation

    # Season information
    if ("season" %in% names(first_league) && is.data.frame(first_league$season)) {
      season_year <- as.character(first_league$season$year[1])
      season_type <- first_league$season$type$name[1]
    }
  }

  # Extract date range
  date_range <- extract_nested_soccer_scoreboard(data, c("day", "date"), "")

  # Extract events (matches)
  events <- extract_nested_soccer_scoreboard(data, c("events"), list())
  total_events <- if (is.data.frame(events)) nrow(events) else 0

  # Create league info row
  league_row <- data.frame(
    league_slug = as.character(league_slug),
    league_name = as.character(league_name),
    league_abbreviation = as.character(league_abbreviation),
    season_year = as.character(season_year),
    season_type = as.character(season_type),
    date_range = as.character(date_range),
    total_events = as.character(total_events),
    stringsAsFactors = FALSE
  )
  league_info_df <- rbind(league_info_df, league_row)

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
      venue_name <- venue_city <- attendance <- NA_character_
      home_team_id <- away_team_id <- NA_character_
      home_team_name <- away_team_name <- NA_character_
      home_team_abbreviation <- away_team_abbreviation <- NA_character_
      home_score <- away_score <- NA_character_
      home_odds <- away_odds <- draw_odds <- NA_character_

      # Extract competition data
      if ("competitions" %in% names(event) && is.list(event$competitions) && length(event$competitions) > 0) {
        competition <- event$competitions[[1]]

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

        # Team information
        if ("competitors" %in% names(competition) && is.data.frame(competition$competitors)) {
          competitors <- competition$competitors

          for (j in seq_len(nrow(competitors))) {
            competitor <- competitors[j, ]

            # Team details
            if ("team" %in% names(competitor) && is.data.frame(competitor$team)) {
              team_id <- as.character(competitor$team$id[1])
              team_name <- as.character(competitor$team$displayName[1])
              team_abbreviation <- as.character(competitor$team$abbreviation[1])
            } else {
              team_id <- team_name <- team_abbreviation <- NA_character_
            }

            team_score <- as.character(competitor$score[1])
            home_away <- as.character(competitor$homeAway[1])

            if (home_away == "home") {
              home_team_id <- team_id
              home_team_name <- team_name
              home_team_abbreviation <- team_abbreviation
              home_score <- team_score
            } else if (home_away == "away") {
              away_team_id <- team_id
              away_team_name <- team_name
              away_team_abbreviation <- team_abbreviation
              away_score <- team_score
            }
          }
        }

        # Odds information
        if ("odds" %in% names(competition) && is.data.frame(competition$odds)) {
          odds_data <- competition$odds

          for (k in seq_len(nrow(odds_data))) {
            odds_row <- odds_data[k, ]

            # Extract different types of odds
            if ("homeTeamOdds" %in% names(odds_row) && is.data.frame(odds_row$homeTeamOdds)) {
              home_odds <- as.character(odds_row$homeTeamOdds$summary[1])
            }

            if ("awayTeamOdds" %in% names(odds_row) && is.data.frame(odds_row$awayTeamOdds)) {
              away_odds <- as.character(odds_row$awayTeamOdds$summary[1])
            }

            if ("drawOdds" %in% names(odds_row) && is.data.frame(odds_row$drawOdds)) {
              draw_odds <- as.character(odds_row$drawOdds$summary[1])
            }

            # If we found odds, break (use first provider)
            if (!is.na(home_odds) || !is.na(away_odds) || !is.na(draw_odds)) {
              break
            }
          }
        }
      }

      # Create match row
      match_row <- data.frame(
        league_slug = as.character(league_slug),
        event_id = event_id,
        event_name = event_name,
        event_short_name = event_short_name,
        event_date = event_date,
        event_status = event_status,
        event_status_detail = event_status_detail,
        home_team_id = home_team_id,
        home_team_name = home_team_name,
        home_team_abbreviation = home_team_abbreviation,
        home_score = home_score,
        away_team_id = away_team_id,
        away_team_name = away_team_name,
        away_team_abbreviation = away_team_abbreviation,
        away_score = away_score,
        venue_name = venue_name,
        venue_city = venue_city,
        attendance = attendance,
        clock = clock,
        home_odds = home_odds,
        away_odds = away_odds,
        draw_odds = draw_odds,
        stringsAsFactors = FALSE
      )
      matches_df <- rbind(matches_df, match_row)
    }
  }

  # Clean up row names
  if (nrow(league_info_df) > 0) rownames(league_info_df) <- NULL
  if (nrow(matches_df) > 0) rownames(matches_df) <- NULL

  return(list(
    league_info = league_info_df,
    matches = matches_df
  ))
}

#' Fetch soccer scoreboard/schedule data using ESPN API
#'
#' Retrieves soccer match schedules and scores from ESPN's API for a specific league.
#' The function fetches comprehensive match data including scores, status, teams, and venues.
#'
#' @param league_slug Character. ESPN league slug (required).
#'   Examples: "eng.1" (Premier League), "esp.1" (La Liga), "usa.1" (MLS).
#'   Use \code{fetch_soccer_leagues()} to get available league slugs.
#' @param dates Character. Date range for matches (optional).
#'   Format: "YYYYMMDD" for single date or "YYYYMMDD-YYYYMMDD" for range.
#'   If not specified, returns current/recent matches.
#' @param return_type Character. Type of data to return (default: "all").
#'   Options are:
#'   \itemize{
#'     \item "league_info" - Basic league and season information
#'     \item "matches" - Match details, scores, and status
#'     \item "all" - Both league info and matches
#'   }
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'soccer_scoreboard_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data. The main purpose is global
#'   environment assignment based on return_type:
#'   \itemize{
#'     \item \code{soccer_scoreboard_league_info} - League summary data frame
#'     \item \code{soccer_scoreboard_matches} - Match details data frame
#'     \item \code{soccer_scoreboard_raw} - Raw JSON response (if raw = TRUE)
#'   }
#'
#' @details
#' The function creates structured data frames with comprehensive match information:
#'
#' **League Info** (\code{soccer_scoreboard_league_info}):
#' \itemize{
#'   \item League details: slug, name, abbreviation
#'   \item Season information: year, type
#'   \item Date range and total events count
#' }
#'
#' **Matches** (\code{soccer_scoreboard_matches}):
#' \itemize{
#'   \item Match details: event_id, name, date, status
#'   \item Team information: home/away teams with IDs, names, abbreviations
#'   \item Scores: current match scores
#'   \item Venue: stadium name and city
#'   \item Competition: tournament/league round information
#'   \item Broadcast: TV/streaming information
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
#' # Get Premier League current matches
#' fetch_soccer_scoreboard("eng.1")
#'
#' # Get La Liga matches for specific date
#' fetch_soccer_scoreboard("esp.1", dates = "20250125")
#'
#' # Get MLS matches for date range
#' fetch_soccer_scoreboard("usa.1", dates = "20250101-20250131")
#'
#' # Get Bundesliga matches (Germany)
#' fetch_soccer_scoreboard("ger.1")
#'
#' # Get Champions League matches
#' fetch_soccer_scoreboard("uefa.champions")
#'
#' # Check what data was created
#' head(soccer_scoreboard_league_info)
#' head(soccer_scoreboard_matches)
#'
#' # Get only match data
#' fetch_soccer_scoreboard("eng.1", return_type = "matches")
#'
#' # Find live matches
#' live_matches <- soccer_scoreboard_matches[
#'   soccer_scoreboard_matches$event_status == "STATUS_IN_PROGRESS",
#' ]
#'
#' # Find completed matches with scores
#' completed_matches <- soccer_scoreboard_matches[
#'   soccer_scoreboard_matches$event_status == "STATUS_FINAL" &
#'   !is.na(soccer_scoreboard_matches$home_score),
#' ]
#'
#' # Find upcoming matches
#' upcoming_matches <- soccer_scoreboard_matches[
#'   soccer_scoreboard_matches$event_status == "STATUS_SCHEDULED",
#' ]
#'
#' # Get matches by team (example: Arsenal)
#' arsenal_matches <- soccer_scoreboard_matches[
#'   grepl("Arsenal", soccer_scoreboard_matches$home_team_name) |
#'   grepl("Arsenal", soccer_scoreboard_matches$away_team_name),
#' ]
#'
#' # Get matches by venue
#' wembley_matches <- soccer_scoreboard_matches[
#'   grepl("Wembley", soccer_scoreboard_matches$venue_name),
#' ]
#'
#' # View broadcast information
#' matches_with_tv <- soccer_scoreboard_matches[
#'   !is.na(soccer_scoreboard_matches$broadcast_info) &
#'   soccer_scoreboard_matches$broadcast_info != "",
#' ]
#'
#' # Get raw data for debugging
#' fetch_soccer_scoreboard("eng.1", raw = TRUE)
#' }
#'
#' @seealso
#' \code{\link{fetch_soccer_leagues}} to get available league slugs
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
fetch_soccer_scoreboard <- function(league_slug, dates = NULL, return_type = "all", raw = FALSE) {
  # Input validation
  if (missing(league_slug) || is.null(league_slug) || league_slug == "") {
    stop("'league_slug' is required. Use fetch_soccer_leagues() to get available league slugs.")
  }

  valid_types <- c("league_info", "matches", "all")
  if (!return_type %in% valid_types) {
    stop(sprintf("'return_type' must be one of: %s", paste(valid_types, collapse = ", ")))
  }

  if (!is.logical(raw)) {
    stop("'raw' parameter must be logical (TRUE or FALSE)")
  }

  # Build API URL
  base_url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/soccer/%s/scoreboard", league_slug)

  # Add dates parameter if provided
  if (!is.null(dates) && dates != "") {
    url <- sprintf("%s?dates=%s", base_url, dates)
    message(sprintf("Fetching soccer scoreboard for %s (dates: %s)...", league_slug, dates))
  } else {
    url <- base_url
    message(sprintf("Fetching soccer scoreboard for %s...", league_slug))
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
      assign("soccer_scoreboard_raw", data, envir = .GlobalEnv)
      message("Raw soccer scoreboard data assigned to: soccer_scoreboard_raw")
      message("Data structure preview:")

      # Show available sections
      sections <- names(data)
      message("- Available sections: ", paste(sections, collapse = ", "))

      # Show events info if available
      if ("events" %in% names(data)) {
        events <- data$events
        if (is.list(events)) {
          message("- Total matches found: ", length(events))
        }
      }

      # Show league info
      if ("leagues" %in% names(data)) {
        leagues <- data$leagues
        if (is.list(leagues) && length(leagues) > 0) {
          league_name <- extract_nested_soccer_scoreboard(leagues[[1]], c("name"), "Unknown")
          message("- League: ", league_name)
        }
      }

      return(invisible(data))
    }

    # Create datasets
    datasets <- create_soccer_scoreboard_datasets(data, league_slug)

    # Assign data based on return_type
    result_data <- list()

    if (return_type %in% c("league_info", "all")) {
      assign("soccer_scoreboard_league_info", datasets$league_info, envir = .GlobalEnv)
      result_data$league_info <- datasets$league_info
      message(sprintf("Soccer scoreboard league info assigned to: soccer_scoreboard_league_info (%d records)",
                      nrow(datasets$league_info)))
    }

    if (return_type %in% c("matches", "all")) {
      assign("soccer_scoreboard_matches", datasets$matches, envir = .GlobalEnv)
      result_data$matches <- datasets$matches
      message(sprintf("Soccer scoreboard matches assigned to: soccer_scoreboard_matches (%d matches)",
                      nrow(datasets$matches)))

      # Show sample match information
      if (nrow(datasets$matches) > 0) {
        # Show status breakdown
        status_counts <- table(datasets$matches$event_status)
        if (length(status_counts) > 0) {
          message("Match status breakdown:")
          for (status in names(status_counts)) {
            message(sprintf("  - %s: %d", status, status_counts[status]))
          }
        }

        # Show sample teams
        unique_teams <- unique(c(datasets$matches$home_team_name, datasets$matches$away_team_name))
        unique_teams <- unique_teams[!is.na(unique_teams) & unique_teams != ""]
        if (length(unique_teams) > 0) {
          sample_teams <- head(unique_teams, 5)
          message("Sample teams: ", paste(sample_teams, collapse = ", "))
        }

        # Show venues
        unique_venues <- unique(datasets$matches$venue_name)
        unique_venues <- unique_venues[!is.na(unique_venues) & unique_venues != ""]
        if (length(unique_venues) > 0) {
          message(sprintf("Venues found: %d", length(unique_venues)))
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
    stop(sprintf("Failed to fetch soccer scoreboard for %s: %s", league_slug, e$message))
  })
}

#' Fetch multiple soccer league scoreboards
#'
#' Retrieves scoreboard data for multiple soccer leagues with rate limiting to
#' be respectful to ESPN's API. This function calls \code{\link{fetch_soccer_scoreboard}}
#' for each league and combines the results.
#'
#' @param league_slugs Character vector. ESPN league slugs.
#'   Examples: c("eng.1", "esp.1", "ger.1") for Premier League, La Liga, Bundesliga.
#' @param dates Character. Date range for matches (optional, applied to all leagues).
#'   Format: "YYYYMMDD" for single date or "YYYYMMDD-YYYYMMDD" for range.
#' @param return_type Character. Type of data to return (default: "all").
#'   Same options as \code{\link{fetch_soccer_scoreboard}}.
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
#' # Get scoreboards for major European leagues
#' major_leagues <- c("eng.1", "esp.1", "ger.1", "ita.1", "fra.1")
#' fetch_multiple_soccer_scoreboards(major_leagues)
#'
#' # Get scoreboards for specific date
#' fetch_multiple_soccer_scoreboards(major_leagues, dates = "20250125")
#'
#' # Get matches only for multiple leagues
#' fetch_multiple_soccer_scoreboards(major_leagues, return_type = "matches")
#'
#' # Use longer delay for larger requests
#' fetch_multiple_soccer_scoreboards(major_leagues, delay = 1.0)
#'
#' # Analyze combined results
#' unique_leagues <- unique(soccer_scoreboard_matches$league_slug)
#' cat("Retrieved scoreboards for", length(unique_leagues), "leagues\n")
#'
#' # Find all live matches across leagues
#' live_matches <- soccer_scoreboard_matches[
#'   soccer_scoreboard_matches$event_status == "STATUS_IN_PROGRESS",
#' ]
#'
#' # Compare scores across leagues
#' completed_matches <- soccer_scoreboard_matches[
#'   soccer_scoreboard_matches$event_status == "STATUS_FINAL" &
#'   !is.na(soccer_scoreboard_matches$home_score),
#' ]
#'
#' # Get all matches by date
#' todays_matches <- soccer_scoreboard_matches[
#'   as.Date(soccer_scoreboard_matches$event_date) == Sys.Date(),
#' ]
#' }
#'
#' @seealso \code{\link{fetch_soccer_scoreboard}} for single league data
#' @importFrom httr GET status_code content timeout
#' @export
fetch_multiple_soccer_scoreboards <- function(league_slugs, dates = NULL, return_type = "all",
                                              delay = 0.5, raw = FALSE) {
  # Input validation
  if (length(league_slugs) == 0) {
    stop("'league_slugs' must contain at least one league slug")
  }

  valid_types <- c("league_info", "matches", "all")
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
  all_matches <- data.frame()

  message(sprintf("Starting to fetch soccer scoreboards for %d leagues...", length(league_slugs)))

  # Process each league sequentially
  for (i in seq_along(league_slugs)) {
    league_slug <- league_slugs[i]
    message(sprintf("Fetching soccer scoreboard for %s (%d/%d)...", league_slug, i, length(league_slugs)))

    tryCatch({
      # Fetch individual league data
      scoreboard_data <- fetch_soccer_scoreboard(
        league_slug = league_slug,
        dates = dates,
        return_type = return_type,
        raw = raw
      )

      # If raw data requested, return after first league
      if (isTRUE(raw)) {
        return(invisible(scoreboard_data))
      }

      # Combine data based on return type
      if (return_type %in% c("league_info", "all")) {
        league_info_df <- get("soccer_scoreboard_league_info", envir = .GlobalEnv)
        all_league_info <- rbind(all_league_info, league_info_df)
      }

      if (return_type %in% c("matches", "all")) {
        matches_df <- get("soccer_scoreboard_matches", envir = .GlobalEnv)
        all_matches <- rbind(all_matches, matches_df)
      }

    }, error = function(e) {
      message(sprintf("Failed to fetch soccer scoreboard for %s: %s", league_slug, e$message))
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
    assign("soccer_scoreboard_league_info", all_league_info, envir = .GlobalEnv)
    result_data$league_info <- all_league_info
    message(sprintf("Combined soccer scoreboard league info assigned to: soccer_scoreboard_league_info (%d leagues)",
                    nrow(all_league_info)))
  }

  if (return_type %in% c("matches", "all") && nrow(all_matches) > 0) {
    all_matches <- all_matches[!duplicated(paste(all_matches$league_slug, all_matches$event_id)), ]
    assign("soccer_scoreboard_matches", all_matches, envir = .GlobalEnv)
    result_data$matches <- all_matches
    unique_leagues <- length(unique(all_matches$league_slug))
    total_matches <- nrow(all_matches)
    message(sprintf("Combined soccer scoreboard matches assigned to: soccer_scoreboard_matches (%d leagues, %d matches)",
                    unique_leagues, total_matches))

    # Show combined analytics
    if (nrow(all_matches) > 0) {
      # Show status breakdown
      status_counts <- table(all_matches$event_status)
      message("Combined match status breakdown:")
      for (status in names(status_counts)) {
        message(sprintf("  - %s: %d", status, status_counts[status]))
      }

      # Show league breakdown
      league_counts <- table(all_matches$league_slug)
      message("Matches per league:")
      for (league in names(league_counts)) {
        message(sprintf("  - %s: %d", league, league_counts[league]))
      }

      # Show total venues
      unique_venues <- length(unique(all_matches$venue_name[!is.na(all_matches$venue_name)]))
      message(sprintf("Total unique venues: %d", unique_venues))
    }
  }

  # Return appropriate data structure
  if (return_type == "all") {
    return(invisible(result_data))
  } else {
    return(invisible(result_data[[return_type]]))
  }
}
