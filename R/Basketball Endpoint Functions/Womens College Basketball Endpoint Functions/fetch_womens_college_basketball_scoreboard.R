#' Fetch womens_college_basketball scoreboard data using Site API
#'
#' @param year Numeric. Season year (e.g., 2023, 2024).
#' @param week Numeric. Week number. Optional.
#' @param seasontype Numeric. Season type (1=preseason, 2=regular, 3=postseason). Default 2.
#' @param dates Character. Date in YYYYMMDD format. Optional.
#' @param raw Logical. If TRUE, assigns raw JSON to global environment. Default FALSE.
#' @return Invisibly returns the data, but main purpose is global environment assignment.
#' @export
#'
#' @examples
#' # Get womens_college_basketball scoreboard for current date
#' fetch_womens_college_basketball_scoreboard()
#' head(womens_college_basketball_scoreboard)
#'
#' # Get scoreboard for specific date
#' fetch_womens_college_basketball_scoreboard(dates = "20241225")
#'
#' # Get raw data
#' fetch_womens_college_basketball_scoreboard(raw = TRUE)
#'
fetch_womens_college_basketball_scoreboard <- function(year = NULL, week = NULL, seasontype = 2, dates = NULL, raw = FALSE) {

  # Validate inputs
  if (!is.null(year) && (!is.numeric(year) || year < 1990 || year > as.numeric(format(Sys.Date(), "%Y")) + 2)) {
    stop("year must be a valid numeric year between 1990 and two years in the future")
  }

  if (!is.null(week) && (!is.numeric(week) || week < 1 || week > 30)) {
    stop("week must be numeric between 1-30")
  }

  if (!seasontype %in% c(1, 2, 3)) {
    stop("seasontype must be 1 (preseason), 2 (regular season), or 3 (postseason)")
  }

  if (!is.null(dates) && (!is.character(dates) || nchar(dates) != 8 || !grepl("^\\d{8}$", dates))) {
    stop("dates must be in YYYYMMDD format (e.g., '20241225')")
  }

  # Build URL
  base_url <- "https://site.api.espn.com/apis/site/v2/sports/basketball/womens-college-basketball/scoreboard"

  # Build query parameters
  query_params <- list()

  if (!is.null(year)) {
    query_params$dates <- year
  }

  if (!is.null(week)) {
    query_params$week <- week
  }

  if (!is.null(dates)) {
    query_params$dates <- dates
  }

  if (seasontype != 2) {
    query_params$seasontype <- seasontype
  }

  url <- httr::modify_url(base_url, query = query_params)

  # Fetch and parse
  tryCatch({
    message("Fetching womens_college_basketball scoreboard data...")

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
      assign("womens_college_basketball_scoreboard_raw", data, envir = .GlobalEnv)
      message("Raw womens_college_basketball scoreboard data assigned to: womens_college_basketball_scoreboard_raw")
      return(invisible(data))
    }

    # Create clean scoreboard dataset
    scoreboard_df <- create_clean_womens_college_basketball_scoreboard_dataset(data, year, week, seasontype, dates)

    # Assign to global environment
    assign("womens_college_basketball_scoreboard", scoreboard_df, envir = .GlobalEnv)
    message(sprintf("womens_college_basketball scoreboard data assigned to: womens_college_basketball_scoreboard (%d games)", nrow(scoreboard_df)))

    return(invisible(scoreboard_df))

  }, error = function(e) {
    stop(sprintf("Failed to fetch womens_college_basketball scoreboard data: %s", e$message))
  })
}

#' Create clean womens_college_basketball scoreboard dataset from ESPN Site API response
#'
#' @param data Raw JSON response from ESPN Site API
#' @param year Year parameter used in request (can be NULL)
#' @param week Week parameter used in request (can be NULL)
#' @param seasontype Season type parameter used in request
#' @param dates Dates parameter used in request (can be NULL)
#' @return Clean data frame with one row per game
create_clean_womens_college_basketball_scoreboard_dataset <- function(data, year, week, seasontype, dates) {

  # Initialize result data frame
  result_df <- data.frame(
    game_id = character(0),
    game_uid = character(0),
    game_date = character(0),
    game_name = character(0),
    game_short_name = character(0),
    week_number = integer(0),
    season_type = integer(0),
    season_year = integer(0),
    status_type = character(0),
    status_detail = character(0),
    status_short_detail = character(0),
    is_completed = logical(0),
    is_live = logical(0),
    period = integer(0),
    clock = character(0),
    # Home team info
    home_team_id = character(0),
    home_team_abbr = character(0),
    home_team_name = character(0),
    home_team_display_name = character(0),
    home_team_location = character(0),
    home_team_logo = character(0),
    home_score = character(0),
    home_record = character(0),
    home_winner = logical(0),
    # Away team info
    away_team_id = character(0),
    away_team_abbr = character(0),
    away_team_name = character(0),
    away_team_display_name = character(0),
    away_team_location = character(0),
    away_team_logo = character(0),
    away_score = character(0),
    away_record = character(0),
    away_winner = logical(0),
    # Venue info
    venue_name = character(0),
    venue_city = character(0),
    venue_state = character(0),
    venue_capacity = character(0),
    # Broadcast info
    broadcast = character(0),
    # Betting info (if available)
    spread = character(0),
    over_under = character(0),
    stringsAsFactors = FALSE
  )

  # Navigate to events (games) - handle the actual ESPN Site API structure
  events <- NULL

  if ("content" %in% names(data) && "sbData" %in% names(data[["content"]])) {
    # Handle the actual structure: data$content$sbData$events
    sb_data <- data[["content"]][["sbData"]]
    if ("events" %in% names(sb_data)) {
      events <- sb_data[["events"]]
    }
  } else if ("events" %in% names(data)) {
    # Fallback to direct events structure
    events <- data[["events"]]
  }

  if (is.null(events) || length(events) == 0) {
    return(result_df)
  }

  # Get season info from data
  season_info <- if ("season" %in% names(data)) data[["season"]] else list()
  data_season_year <- if ("year" %in% names(season_info)) season_info[["year"]] else year
  data_season_type <- if ("type" %in% names(season_info)) season_info[["type"]] else seasontype

  # Process each game
  for (i in seq_along(events)) {
    event <- events[[i]]

    # Basic game info
    game_id <- if ("id" %in% names(event)) event[["id"]] else NA_character_
    game_uid <- if ("uid" %in% names(event)) event[["uid"]] else NA_character_
    game_date <- if ("date" %in% names(event)) event[["date"]] else NA_character_
    game_name <- if ("name" %in% names(event)) event[["name"]] else NA_character_
    game_short_name <- if ("shortName" %in% names(event)) event[["shortName"]] else NA_character_

    # Week info
    week_info <- if ("week" %in% names(event)) event[["week"]] else list()
    week_number <- if ("number" %in% names(week_info)) week_info[["number"]] else NA_integer_

    # Season info (prefer event-level, fallback to data-level)
    event_season_info <- if ("season" %in% names(event)) event[["season"]] else list()
    season_type <- if ("type" %in% names(event_season_info)) event_season_info[["type"]] else data_season_type
    season_year <- if ("year" %in% names(event_season_info)) event_season_info[["year"]] else data_season_year

    # Process competition (should be just one)
    if ("competitions" %in% names(event) && length(event[["competitions"]]) > 0) {
      competition <- event[["competitions"]][[1]]

      # Status info
      status_info <- if ("status" %in% names(competition)) competition[["status"]] else list()
      status_type_info <- if ("type" %in% names(status_info)) status_info[["type"]] else list()
      status_type <- if ("name" %in% names(status_type_info)) status_type_info[["name"]] else NA_character_
      status_detail <- if ("detail" %in% names(status_type_info)) status_type_info[["detail"]] else NA_character_
      status_short_detail <- if ("shortDetail" %in% names(status_type_info)) status_type_info[["shortDetail"]] else NA_character_
      is_completed <- if ("completed" %in% names(status_type_info)) status_type_info[["completed"]] else FALSE
      status_state <- if ("state" %in% names(status_type_info)) status_type_info[["state"]] else NA_character_
      is_live <- !is.na(status_state) && status_state == "in"

      # Clock and period info
      period <- if ("period" %in% names(status_info)) status_info[["period"]] else NA_integer_
      clock <- if ("displayClock" %in% names(status_info)) status_info[["displayClock"]] else NA_character_

      # Venue info
      venue_info <- if ("venue" %in% names(competition)) competition[["venue"]] else list()
      venue_name <- if ("fullName" %in% names(venue_info)) venue_info[["fullName"]] else NA_character_
      venue_capacity <- if ("capacity" %in% names(venue_info)) as.character(venue_info[["capacity"]]) else NA_character_
      venue_address <- if ("address" %in% names(venue_info)) venue_info[["address"]] else list()
      venue_city <- if ("city" %in% names(venue_address)) venue_address[["city"]] else NA_character_
      venue_state <- if ("state" %in% names(venue_address)) venue_address[["state"]] else NA_character_

      # Broadcast info
      broadcast <- NA_character_
      if ("broadcasts" %in% names(competition) && length(competition[["broadcasts"]]) > 0) {
        first_broadcast <- competition[["broadcasts"]][[1]]
        if ("names" %in% names(first_broadcast) && length(first_broadcast[["names"]]) > 0) {
          broadcast <- first_broadcast[["names"]][[1]]
        }
      }

      # Betting info
      spread <- NA_character_
      over_under <- NA_character_
      if ("odds" %in% names(competition) && length(competition[["odds"]]) > 0) {
        odds_info <- competition[["odds"]][[1]]
        if ("spread" %in% names(odds_info)) {
          spread <- as.character(odds_info[["spread"]])
        }
        if ("overUnder" %in% names(odds_info)) {
          over_under <- as.character(odds_info[["overUnder"]])
        }
      }

      # Initialize team variables
      home_team_id <- away_team_id <- NA_character_
      home_team_abbr <- away_team_abbr <- NA_character_
      home_team_name <- away_team_name <- NA_character_
      home_team_display_name <- away_team_display_name <- NA_character_
      home_team_location <- away_team_location <- NA_character_
      home_team_logo <- away_team_logo <- NA_character_
      home_score <- away_score <- NA_character_
      home_record <- away_record <- NA_character_
      home_winner <- away_winner <- FALSE

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
          team_location <- if ("location" %in% names(team_info)) team_info[["location"]] else NA_character_

          # Team logo
          team_logo <- NA_character_
          if ("logo" %in% names(team_info)) {
            team_logo <- team_info[["logo"]]
          } else if ("logos" %in% names(team_info) && length(team_info[["logos"]]) > 0) {
            team_logo <- if ("href" %in% names(team_info[["logos"]][[1]])) team_info[["logos"]][[1]][["href"]] else NA_character_
          }

          # Score and winner
          score <- if ("score" %in% names(competitor)) as.character(competitor[["score"]]) else NA_character_
          winner <- if ("winner" %in% names(competitor)) competitor[["winner"]] else FALSE

          # Record
          record <- NA_character_
          if ("records" %in% names(competitor) && length(competitor[["records"]]) > 0) {
            for (rec in competitor[["records"]]) {
              if ("name" %in% names(rec) && rec[["name"]] == "overall" && "summary" %in% names(rec)) {
                record <- rec[["summary"]]
                break
              } else if ("type" %in% names(rec) && rec[["type"]] == "total" && "summary" %in% names(rec)) {
                record <- rec[["summary"]]
                break
              }
            }
            # If no overall/total record found, use first available
            if (is.na(record) && length(competitor[["records"]]) > 0) {
              first_rec <- competitor[["records"]][[1]]
              if ("summary" %in% names(first_rec)) {
                record <- first_rec[["summary"]]
              }
            }
          }

          # Assign to home/away variables
          if (home_away == "home") {
            home_team_id <- team_id
            home_team_abbr <- team_abbr
            home_team_name <- team_name
            home_team_display_name <- team_display_name
            home_team_location <- team_location
            home_team_logo <- team_logo
            home_score <- score
            home_record <- record
            home_winner <- winner
          } else if (home_away == "away") {
            away_team_id <- team_id
            away_team_abbr <- team_abbr
            away_team_name <- team_name
            away_team_display_name <- team_display_name
            away_team_location <- team_location
            away_team_logo <- team_logo
            away_score <- score
            away_record <- record
            away_winner <- winner
          }
        }
      }

      # Create game row
      game_row <- data.frame(
        game_id = game_id,
        game_uid = game_uid,
        game_date = game_date,
        game_name = game_name,
        game_short_name = game_short_name,
        week_number = week_number,
        season_type = season_type,
        season_year = season_year,
        status_type = status_type,
        status_detail = status_detail,
        status_short_detail = status_short_detail,
        is_completed = is_completed,
        is_live = is_live,
        period = period,
        clock = clock,
        home_team_id = home_team_id,
        home_team_abbr = home_team_abbr,
        home_team_name = home_team_name,
        home_team_display_name = home_team_display_name,
        home_team_location = home_team_location,
        home_team_logo = home_team_logo,
        home_score = home_score,
        home_record = home_record,
        home_winner = home_winner,
        away_team_id = away_team_id,
        away_team_abbr = away_team_abbr,
        away_team_name = away_team_name,
        away_team_display_name = away_team_display_name,
        away_team_location = away_team_location,
        away_team_logo = away_team_logo,
        away_score = away_score,
        away_record = away_record,
        away_winner = away_winner,
        venue_name = venue_name,
        venue_city = venue_city,
        venue_state = venue_state,
        venue_capacity = venue_capacity,
        broadcast = broadcast,
        spread = spread,
        over_under = over_under,
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

