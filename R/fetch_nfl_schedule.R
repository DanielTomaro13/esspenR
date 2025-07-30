#' Fetch NFL schedule data from ESPN Site API
#'
#' @param year Numeric. Season year (e.g., 2023, 2024).
#' @param week Numeric. Week number (1-18 regular season, 19-22 playoffs). Optional.
#' @param seasontype Numeric. Season type (1=preseason, 2=regular, 3=postseason). Default 2.
#' @param raw Logical. If TRUE, assigns raw JSON to global environment. Default FALSE.
#' @return Invisibly returns the data, but main purpose is global environment assignment.
#' @export
#'
#' @examples
#' # Get NFL schedule for specific week
#' fetch_nfl_schedule(year = 2024, week = 1)
#' head(nfl_schedule)
#'
#' # Get entire season schedule
#' fetch_nfl_schedule(year = 2024)
#'
#' # Get raw data
#' fetch_nfl_schedule(year = 2024, week = 1, raw = TRUE)
#'
fetch_nfl_schedule <- function(year, week = NULL, seasontype = 2, raw = FALSE) {
  # Validate inputs
  if (missing(year)) {
    stop("'year' is a required parameter")
  }
  if (!is.numeric(year) || year < 1990 || year > as.numeric(format(Sys.Date(), "%Y")) + 2) {
    stop("year must be a valid numeric year between 1990 and two years in the future")
  }
  if (!is.null(week) && (!is.numeric(week) || week < 1 || week > 22)) {
    stop("week must be numeric between 1-22")
  }
  if (!seasontype %in% c(1, 2, 3)) {
    stop("seasontype must be 1 (preseason), 2 (regular season), or 3 (postseason)")
  }

  # Build URL
  base_url <- "https://site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard"

  # Build query parameters
  query_params <- list(
    dates = year,
    seasontype = seasontype
  )

  if (!is.null(week)) {
    query_params$week <- week
  }

  url <- httr::modify_url(base_url, query = query_params)

  # Fetch and parse
  tryCatch({
    resp <- httr::GET(url, httr::timeout(60))
    if (httr::status_code(resp) != 200) {
      stop(sprintf("API request failed: HTTP %d - %s",
                   httr::status_code(resp),
                   httr::http_status(resp)$message))
    }

    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content_text, simplifyVector = FALSE, simplifyDataFrame = FALSE)

    # Handle raw data assignment
    if (isTRUE(raw)) {
      assign("nfl_schedule_raw", data, envir = .GlobalEnv)
      message("Raw NFL schedule data assigned to: nfl_schedule_raw")
      return(invisible(data))
    }

    # Create clean schedule dataset
    schedule_df <- create_clean_schedule_dataset(data, year, week, seasontype)

    # Assign to global environment
    assign("nfl_schedule", schedule_df, envir = .GlobalEnv)

    message(sprintf("NFL schedule data assigned to: nfl_schedule (%d games)", nrow(schedule_df)))

    return(invisible(schedule_df))

  }, error = function(e) {
    stop(sprintf("Failed to fetch NFL schedule data: %s", e$message))
  })
}

#' Create clean schedule dataset from ESPN Site API response
#'
#' @param data Raw JSON response from ESPN Site API
#' @param year Year parameter used in request
#' @param week Week parameter used in request (can be NULL)
#' @param seasontype Season type parameter used in request
#' @return Clean data frame with one row per game
create_clean_schedule_dataset <- function(data, year, week, seasontype) {
  # Initialize result data frame
  result_df <- data.frame(
    game_id = character(0),
    game_uid = character(0),
    game_date = character(0),
    game_name = character(0),
    week_number = integer(0),
    week_text = character(0),
    season_type = integer(0),
    season_year = integer(0),
    status_type = character(0),
    status_detail = character(0),
    is_completed = logical(0),
    # Home team info
    home_team_id = character(0),
    home_team_abbr = character(0),
    home_team_name = character(0),
    home_team_display_name = character(0),
    home_team_logo = character(0),
    home_score = character(0),
    home_record = character(0),
    # Away team info
    away_team_id = character(0),
    away_team_abbr = character(0),
    away_team_name = character(0),
    away_team_display_name = character(0),
    away_team_logo = character(0),
    away_score = character(0),
    away_record = character(0),
    # Venue info
    venue_name = character(0),
    venue_city = character(0),
    venue_state = character(0),
    # Broadcast info
    broadcast = character(0),
    stringsAsFactors = FALSE
  )

  # Navigate to events (games)
  events <- NULL
  if ("events" %in% names(data)) {
    events <- data[["events"]]
  } else if ("content" %in% names(data) && "schedule" %in% names(data[["content"]])) {
    # Handle alternative structure like yours
    schedule_data <- data[["content"]][["schedule"]]
    if ("events" %in% names(schedule_data)) {
      events <- schedule_data[["events"]]
    }
  }

  if (is.null(events) || length(events) == 0) {
    return(result_df)
  }

  # Process each game
  for (i in seq_along(events)) {
    event <- events[[i]]

    # Basic game info
    game_id <- if ("id" %in% names(event)) event[["id"]] else NA_character_
    game_uid <- if ("uid" %in% names(event)) event[["uid"]] else NA_character_
    game_date <- if ("date" %in% names(event)) event[["date"]] else NA_character_
    game_name <- if ("name" %in% names(event)) event[["name"]] else NA_character_

    # Week info
    week_info <- if ("week" %in% names(event)) event[["week"]] else list()
    week_number <- if ("number" %in% names(week_info)) week_info[["number"]] else NA_integer_
    week_text <- if ("text" %in% names(week_info)) week_info[["text"]] else NA_character_

    # Season info
    season_info <- if ("season" %in% names(event)) event[["season"]] else list()
    season_type <- if ("type" %in% names(season_info)) season_info[["type"]] else seasontype
    season_year <- if ("year" %in% names(season_info)) season_info[["year"]] else year

    # Process competition (should be just one)
    if ("competitions" %in% names(event) && length(event[["competitions"]]) > 0) {
      competition <- event[["competitions"]][[1]]

      # Status info
      status_info <- if ("status" %in% names(competition)) competition[["status"]] else list()
      status_type_info <- if ("type" %in% names(status_info)) status_info[["type"]] else list()
      status_type <- if ("name" %in% names(status_type_info)) status_type_info[["name"]] else NA_character_
      status_detail <- if ("detail" %in% names(status_type_info)) status_type_info[["detail"]] else NA_character_
      is_completed <- if ("completed" %in% names(status_type_info)) status_type_info[["completed"]] else FALSE

      # Venue info
      venue_info <- if ("venue" %in% names(competition)) competition[["venue"]] else list()
      venue_name <- if ("fullName" %in% names(venue_info)) venue_info[["fullName"]] else NA_character_
      venue_address <- if ("address" %in% names(venue_info)) venue_info[["address"]] else list()
      venue_city <- if ("city" %in% names(venue_address)) venue_address[["city"]] else NA_character_
      venue_state <- if ("state" %in% names(venue_address)) venue_address[["state"]] else NA_character_

      # Broadcast info
      broadcast <- if ("broadcast" %in% names(competition)) competition[["broadcast"]] else NA_character_

      # Initialize team variables
      home_team_id <- away_team_id <- NA_character_
      home_team_abbr <- away_team_abbr <- NA_character_
      home_team_name <- away_team_name <- NA_character_
      home_team_display_name <- away_team_display_name <- NA_character_
      home_team_logo <- away_team_logo <- NA_character_
      home_score <- away_score <- NA_character_
      home_record <- away_record <- NA_character_

      # Process competitors (teams)
      if ("competitors" %in% names(competition) && length(competition[["competitors"]]) > 0) {
        for (j in seq_along(competition[["competitors"]])) {
          competitor <- competition[["competitors"]][[j]]
          home_away <- if ("homeAway" %in% names(competitor)) competitor[["homeAway"]] else NA_character_

          # Team info
          team_info <- if ("team" %in% names(competitor)) competitor[["team"]] else list()
          team_id <- if ("id" %in% names(team_info)) team_info[["id"]] else NA_character_
          team_abbr <- if ("abbreviation" %in% names(team_info)) team_info[["abbreviation"]] else NA_character_
          team_name <- if ("name" %in% names(team_info)) team_info[["name"]] else NA_character_
          team_display_name <- if ("displayName" %in% names(team_info)) team_info[["displayName"]] else NA_character_

          # Team logo
          team_logo <- NA_character_
          if ("logos" %in% names(team_info) && length(team_info[["logos"]]) > 0) {
            team_logo <- if ("href" %in% names(team_info[["logos"]][[1]])) team_info[["logos"]][[1]][["href"]] else NA_character_
          }

          # Score
          score <- if ("score" %in% names(competitor)) competitor[["score"]] else NA_character_

          # Record
          record <- NA_character_
          if ("records" %in% names(competitor) && length(competitor[["records"]]) > 0) {
            for (rec in competitor[["records"]]) {
              if ("name" %in% names(rec) && rec[["name"]] == "overall" && "summary" %in% names(rec)) {
                record <- rec[["summary"]]
                break
              }
            }
          }

          # Assign to home/away variables
          if (home_away == "home") {
            home_team_id <- team_id
            home_team_abbr <- team_abbr
            home_team_name <- team_name
            home_team_display_name <- team_display_name
            home_team_logo <- team_logo
            home_score <- score
            home_record <- record
          } else if (home_away == "away") {
            away_team_id <- team_id
            away_team_abbr <- team_abbr
            away_team_name <- team_name
            away_team_display_name <- team_display_name
            away_team_logo <- team_logo
            away_score <- score
            away_record <- record
          }
        }
      }

      # Create game row
      game_row <- data.frame(
        game_id = game_id,
        game_uid = game_uid,
        game_date = game_date,
        game_name = game_name,
        week_number = week_number,
        week_text = week_text,
        season_type = season_type,
        season_year = season_year,
        status_type = status_type,
        status_detail = status_detail,
        is_completed = is_completed,
        home_team_id = home_team_id,
        home_team_abbr = home_team_abbr,
        home_team_name = home_team_name,
        home_team_display_name = home_team_display_name,
        home_team_logo = home_team_logo,
        home_score = home_score,
        home_record = home_record,
        away_team_id = away_team_id,
        away_team_abbr = away_team_abbr,
        away_team_name = away_team_name,
        away_team_display_name = away_team_display_name,
        away_team_logo = away_team_logo,
        away_score = away_score,
        away_record = away_record,
        venue_name = venue_name,
        venue_city = venue_city,
        venue_state = venue_state,
        broadcast = broadcast,
        stringsAsFactors = FALSE
      )

      # Add to result
      result_df <- rbind(result_df, game_row)
    }
  }

  # Clean up row names
  if (nrow(result_df) > 0) {
    rownames(result_df) <- NULL
  }

  return(result_df)
}
