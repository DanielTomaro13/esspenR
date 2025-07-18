#' Safe nested data extraction helper function for college football scoreboard
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_cfb <- function(data, path, default = NA) {
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

#' Extract team information from college football competitor data
#'
#' Processes team/competitor information from game data
#' @param competitor List containing competitor information
#' @param prefix Character prefix for column names (e.g., "home_", "away_")
#' @return Named list with team information
#' @keywords internal
extract_cfb_team_info <- function(competitor, prefix = "") {
  if (is.null(competitor) || length(competitor) == 0) {
    return(setNames(rep(NA_character_, 14), paste0(prefix, c(
      "team_id", "team_uid", "team_abbreviation", "team_display_name",
      "team_short_display_name", "team_name", "team_location", "team_color",
      "team_alternate_color", "team_logo", "team_record", "team_conference_id",
      "team_conference_name", "team_rank"
    ))))
  }

  team <- extract_nested_cfb(competitor, c("team"), list())

  team_id <- extract_nested_cfb(team, c("id"), NA_character_)
  team_uid <- extract_nested_cfb(team, c("uid"), NA_character_)
  team_abbreviation <- extract_nested_cfb(team, c("abbreviation"), NA_character_)
  team_display_name <- extract_nested_cfb(team, c("displayName"), NA_character_)
  team_short_display_name <- extract_nested_cfb(team, c("shortDisplayName"), NA_character_)
  team_name <- extract_nested_cfb(team, c("name"), NA_character_)
  team_location <- extract_nested_cfb(team, c("location"), NA_character_)
  team_color <- extract_nested_cfb(team, c("color"), NA_character_)
  team_alternate_color <- extract_nested_cfb(team, c("alternateColor"), NA_character_)

  # Logo
  team_logo <- NA_character_
  logos <- extract_nested_cfb(team, c("logos"), list())
  if (length(logos) > 0) {
    team_logo <- extract_nested_cfb(logos[[1]], c("href"), NA_character_)
  }

  # Record
  team_record <- NA_character_
  records <- extract_nested_cfb(competitor, c("records"), list())
  if (length(records) > 0) {
    # Find overall record
    for (record in records) {
      record_type <- extract_nested_cfb(record, c("type"), NA_character_)
      if (!is.na(record_type) && record_type == "total") {
        summary <- extract_nested_cfb(record, c("summary"), NA_character_)
        if (!is.na(summary)) {
          team_record <- summary
          break
        }
      }
    }
    # If no total record found, use first available
    if (is.na(team_record) && length(records) > 0) {
      team_record <- extract_nested_cfb(records[[1]], c("summary"), NA_character_)
    }
  }

  # Conference information
  team_conference_id <- NA_character_
  team_conference_name <- NA_character_
  conference_info <- extract_nested_cfb(team, c("conferenceId"), NA_character_)
  if (!is.na(conference_info)) {
    team_conference_id <- conference_info
  }

  # Try alternate path for conference
  conferences <- extract_nested_cfb(team, c("conferences"), list())
  if (length(conferences) > 0) {
    first_conf <- conferences[[1]]
    if (!is.na(team_conference_id)) {
      team_conference_id <- extract_nested_cfb(first_conf, c("id"), team_conference_id)
    }
    team_conference_name <- extract_nested_cfb(first_conf, c("name"), NA_character_)
  }

  # Ranking
  team_rank <- NA_character_
  curRank <- extract_nested_cfb(competitor, c("curRank"), NA_character_)
  if (!is.na(curRank)) {
    team_rank <- curRank
  }

  return(setNames(list(
    team_id, team_uid, team_abbreviation, team_display_name,
    team_short_display_name, team_name, team_location, team_color,
    team_alternate_color, team_logo, team_record, team_conference_id,
    team_conference_name, team_rank
  ), paste0(prefix, c(
    "team_id", "team_uid", "team_abbreviation", "team_display_name",
    "team_short_display_name", "team_name", "team_location", "team_color",
    "team_alternate_color", "team_logo", "team_record", "team_conference_id",
    "team_conference_name", "team_rank"
  ))))
}

#' Get college football conference group mappings
#'
#' Returns a mapping of conference names to group IDs for ESPN API
#' @return Named vector with conference names as names and group IDs as values
#' @export
get_cfb_conference_groups <- function() {
  conference_groups <- c(
    "ACC" = "1", "Big 12" = "4", "Big Ten" = "5", "Pac-12" = "9",
    "SEC" = "8", "American" = "151", "C-USA" = "12", "MAC" = "15",
    "Mountain West" = "17", "Sun Belt" = "37", "FBS Independents" = "18",
    "SWAC" = "40", "MEAC" = "16", "Big Sky" = "3", "CAA" = "48",
    "Ivy League" = "14", "MVFC" = "21", "OVC" = "20", "Patriot League" = "22",
    "Pioneer League" = "23", "Southern Conference" = "24", "Southland" = "25"
  )
  return(conference_groups)
}

#' Create college football scoreboard data frame from Site API response
#'
#' Processes raw JSON response from ESPN Site API into structured data frame
#' containing college football scoreboard information
#'
#' @param data Raw JSON response from ESPN Site API scoreboard endpoint
#' @param groups Character. Group IDs used in request (if any)
#' @param week Character. Week number used in request (if any)
#' @param dates Character. Date used in request (if any)
#' @return Data frame with scoreboard information
#' @keywords internal
create_cfb_scoreboard_dataset <- function(data, groups = NA, week = NA, dates = NA) {

  # Initialize scoreboard data frame
  scoreboard_df <- data.frame(
    query_groups = character(0),
    query_week = character(0),
    query_dates = character(0),
    season_year = character(0),
    season_type = character(0),
    week_number = character(0),
    game_id = character(0),
    game_uid = character(0),
    game_date = character(0),
    game_name = character(0),
    game_short_name = character(0),
    game_status = character(0),
    game_status_detail = character(0),
    game_clock = character(0),
    game_period = character(0),
    game_attendance = character(0),
    venue_id = character(0),
    venue_name = character(0),
    venue_full_name = character(0),
    venue_address_city = character(0),
    venue_address_state = character(0),
    venue_capacity = character(0),
    venue_indoor = character(0),
    broadcast_network = character(0),
    broadcast_lang = character(0),
    odds_provider = character(0),
    odds_details = character(0),
    home_team_id = character(0),
    home_team_uid = character(0),
    home_team_abbreviation = character(0),
    home_team_display_name = character(0),
    home_team_short_display_name = character(0),
    home_team_name = character(0),
    home_team_location = character(0),
    home_team_color = character(0),
    home_team_alternate_color = character(0),
    home_team_logo = character(0),
    home_team_record = character(0),
    home_team_conference_id = character(0),
    home_team_conference_name = character(0),
    home_team_rank = character(0),
    home_team_score = character(0),
    home_team_winner = character(0),
    away_team_id = character(0),
    away_team_uid = character(0),
    away_team_abbreviation = character(0),
    away_team_display_name = character(0),
    away_team_short_display_name = character(0),
    away_team_name = character(0),
    away_team_location = character(0),
    away_team_color = character(0),
    away_team_alternate_color = character(0),
    away_team_logo = character(0),
    away_team_record = character(0),
    away_team_conference_id = character(0),
    away_team_conference_name = character(0),
    away_team_rank = character(0),
    away_team_score = character(0),
    away_team_winner = character(0),
    stringsAsFactors = FALSE
  )

  # Extract season information
  season_info <- extract_nested_cfb(data, c("season"), list())
  season_year <- extract_nested_cfb(season_info, c("year"), NA_character_)
  season_type <- extract_nested_cfb(season_info, c("type"), NA_character_)

  # Extract week information
  week_info <- extract_nested_cfb(data, c("week"), list())
  week_number <- extract_nested_cfb(week_info, c("number"), NA_character_)

  # Extract events (games)
  events <- extract_nested_cfb(data, c("events"), list())

  for (i in seq_along(events)) {
    event <- events[[i]]

    # Basic game information
    game_id <- extract_nested_cfb(event, c("id"), NA_character_)
    game_uid <- extract_nested_cfb(event, c("uid"), NA_character_)
    game_date <- extract_nested_cfb(event, c("date"), NA_character_)
    game_name <- extract_nested_cfb(event, c("name"), NA_character_)
    game_short_name <- extract_nested_cfb(event, c("shortName"), NA_character_)

    # Status information
    status_info <- extract_nested_cfb(event, c("status"), list())
    game_status <- extract_nested_cfb(status_info, c("type", "name"), NA_character_)
    game_status_detail <- extract_nested_cfb(status_info, c("type", "detail"), NA_character_)
    game_clock <- extract_nested_cfb(status_info, c("displayClock"), NA_character_)
    game_period <- extract_nested_cfb(status_info, c("period"), NA_character_)

    # Initialize variables
    game_attendance <- NA_character_
    venue_id <- NA_character_
    venue_name <- NA_character_
    venue_full_name <- NA_character_
    venue_address_city <- NA_character_
    venue_address_state <- NA_character_
    venue_capacity <- NA_character_
    venue_indoor <- NA_character_
    broadcast_network <- NA_character_
    broadcast_lang <- NA_character_
    odds_provider <- NA_character_
    odds_details <- NA_character_

    competitions <- extract_nested_cfb(event, c("competitions"), list())
    if (length(competitions) > 0) {
      competition <- competitions[[1]]

      # Attendance
      game_attendance <- extract_nested_cfb(competition, c("attendance"), NA_character_)

      # Venue information
      venue_info <- extract_nested_cfb(competition, c("venue"), list())
      if (length(venue_info) > 0) {
        venue_id <- extract_nested_cfb(venue_info, c("id"), NA_character_)
        venue_name <- extract_nested_cfb(venue_info, c("shortName"), NA_character_)
        venue_full_name <- extract_nested_cfb(venue_info, c("fullName"), NA_character_)
        venue_address_city <- extract_nested_cfb(venue_info, c("address", "city"), NA_character_)
        venue_address_state <- extract_nested_cfb(venue_info, c("address", "state"), NA_character_)
        venue_capacity <- extract_nested_cfb(venue_info, c("capacity"), NA_character_)
        venue_indoor <- extract_nested_cfb(venue_info, c("indoor"), "false")
      }

      # Broadcast information
      broadcasts <- extract_nested_cfb(competition, c("broadcasts"), list())
      if (length(broadcasts) > 0) {
        first_broadcast <- broadcasts[[1]]
        broadcast_names <- extract_nested_cfb(first_broadcast, c("names"), list())
        if (length(broadcast_names) > 0) {
          broadcast_network <- broadcast_names[[1]]
        }
        broadcast_lang <- extract_nested_cfb(first_broadcast, c("lang"), NA_character_)
      }

      # Odds information
      odds <- extract_nested_cfb(competition, c("odds"), list())
      if (length(odds) > 0) {
        first_odds <- odds[[1]]
        odds_provider <- extract_nested_cfb(first_odds, c("provider", "name"), NA_character_)
        odds_details <- extract_nested_cfb(first_odds, c("details"), NA_character_)
      }

      # Team information
      competitors <- extract_nested_cfb(competition, c("competitors"), list())

      home_team_info <- list()
      away_team_info <- list()
      home_team_score <- NA_character_
      home_team_winner <- "false"
      away_team_score <- NA_character_
      away_team_winner <- "false"

      if (length(competitors) >= 2) {
        for (comp in competitors) {
          home_away <- extract_nested_cfb(comp, c("homeAway"), NA_character_)

          if (!is.na(home_away) && home_away == "home") {
            home_team_info <- extract_cfb_team_info(comp, "home_")
            home_team_score <- extract_nested_cfb(comp, c("score"), NA_character_)
            home_team_winner <- extract_nested_cfb(comp, c("winner"), "false")
          } else if (!is.na(home_away) && home_away == "away") {
            away_team_info <- extract_cfb_team_info(comp, "away_")
            away_team_score <- extract_nested_cfb(comp, c("score"), NA_character_)
            away_team_winner <- extract_nested_cfb(comp, c("winner"), "false")
          }
        }
      }

      # If we don't have home/away designation, assign first two competitors
      if (length(home_team_info) == 0 && length(competitors) >= 1) {
        home_team_info <- extract_cfb_team_info(competitors[[1]], "home_")
        home_team_score <- extract_nested_cfb(competitors[[1]], c("score"), NA_character_)
        home_team_winner <- extract_nested_cfb(competitors[[1]], c("winner"), "false")
      }

      if (length(away_team_info) == 0 && length(competitors) >= 2) {
        away_team_info <- extract_cfb_team_info(competitors[[2]], "away_")
        away_team_score <- extract_nested_cfb(competitors[[2]], c("score"), NA_character_)
        away_team_winner <- extract_nested_cfb(competitors[[2]], c("winner"), "false")
      }
    }

    # Ensure team info lists have all required fields
    if (length(home_team_info) == 0) {
      home_team_info <- setNames(rep(NA_character_, 14), paste0("home_", c(
        "team_id", "team_uid", "team_abbreviation", "team_display_name",
        "team_short_display_name", "team_name", "team_location", "team_color",
        "team_alternate_color", "team_logo", "team_record", "team_conference_id",
        "team_conference_name", "team_rank"
      )))
    }

    if (length(away_team_info) == 0) {
      away_team_info <- setNames(rep(NA_character_, 14), paste0("away_", c(
        "team_id", "team_uid", "team_abbreviation", "team_display_name",
        "team_short_display_name", "team_name", "team_location", "team_color",
        "team_alternate_color", "team_logo", "team_record", "team_conference_id",
        "team_conference_name", "team_rank"
      )))
    }

    # Create row
    game_row <- data.frame(
      query_groups = as.character(ifelse(is.na(groups), "", groups)),
      query_week = as.character(ifelse(is.na(week), "", week)),
      query_dates = as.character(ifelse(is.na(dates), "", dates)),
      season_year = as.character(season_year),
      season_type = as.character(season_type),
      week_number = as.character(week_number),
      game_id = as.character(game_id),
      game_uid = as.character(game_uid),
      game_date = as.character(game_date),
      game_name = as.character(game_name),
      game_short_name = as.character(game_short_name),
      game_status = as.character(game_status),
      game_status_detail = as.character(game_status_detail),
      game_clock = as.character(game_clock),
      game_period = as.character(game_period),
      game_attendance = as.character(game_attendance),
      venue_id = as.character(venue_id),
      venue_name = as.character(venue_name),
      venue_full_name = as.character(venue_full_name),
      venue_address_city = as.character(venue_address_city),
      venue_address_state = as.character(venue_address_state),
      venue_capacity = as.character(venue_capacity),
      venue_indoor = as.character(venue_indoor),
      broadcast_network = as.character(broadcast_network),
      broadcast_lang = as.character(broadcast_lang),
      odds_provider = as.character(odds_provider),
      odds_details = as.character(odds_details),
      home_team_id = as.character(home_team_info$home_team_id),
      home_team_uid = as.character(home_team_info$home_team_uid),
      home_team_abbreviation = as.character(home_team_info$home_team_abbreviation),
      home_team_display_name = as.character(home_team_info$home_team_display_name),
      home_team_short_display_name = as.character(home_team_info$home_team_short_display_name),
      home_team_name = as.character(home_team_info$home_team_name),
      home_team_location = as.character(home_team_info$home_team_location),
      home_team_color = as.character(home_team_info$home_team_color),
      home_team_alternate_color = as.character(home_team_info$home_team_alternate_color),
      home_team_logo = as.character(home_team_info$home_team_logo),
      home_team_record = as.character(home_team_info$home_team_record),
      home_team_conference_id = as.character(home_team_info$home_team_conference_id),
      home_team_conference_name = as.character(home_team_info$home_team_conference_name),
      home_team_rank = as.character(home_team_info$home_team_rank),
      home_team_score = as.character(home_team_score),
      home_team_winner = as.character(home_team_winner),
      away_team_id = as.character(away_team_info$away_team_id),
      away_team_uid = as.character(away_team_info$away_team_uid),
      away_team_abbreviation = as.character(away_team_info$away_team_abbreviation),
      away_team_display_name = as.character(away_team_info$away_team_display_name),
      away_team_short_display_name = as.character(away_team_info$away_team_short_display_name),
      away_team_name = as.character(away_team_info$away_team_name),
      away_team_location = as.character(away_team_info$away_team_location),
      away_team_color = as.character(away_team_info$away_team_color),
      away_team_alternate_color = as.character(away_team_info$away_team_alternate_color),
      away_team_logo = as.character(away_team_info$away_team_logo),
      away_team_record = as.character(away_team_info$away_team_record),
      away_team_conference_id = as.character(away_team_info$away_team_conference_id),
      away_team_conference_name = as.character(away_team_info$away_team_conference_name),
      away_team_rank = as.character(away_team_info$away_team_rank),
      away_team_score = as.character(away_team_score),
      away_team_winner = as.character(away_team_winner),
      stringsAsFactors = FALSE
    )

    scoreboard_df <- rbind(scoreboard_df, game_row)
  }

  # Clean up row names
  if (nrow(scoreboard_df) > 0) rownames(scoreboard_df) <- NULL

  return(scoreboard_df)
}

#' Fetch college football scoreboard using Site API
#'
#' Retrieves college football scoreboard data from ESPN's Site API.
#' The function fetches comprehensive game information including scores,
#' team details, venues, broadcasts, and rankings.
#'
#' @param dates Character. Date to fetch games for in YYYYMMDD format.
#'   If NULL, fetches current/recent games (default: NULL).
#' @param groups Character or numeric. Conference group ID(s) to filter by.
#'   Can be single value or vector. Use \code{get_cfb_conference_groups()} to see mappings.
#'   Examples: "8" (SEC), "5" (Big Ten), c("8", "5") (SEC and Big Ten).
#' @param week Integer. Week number to fetch (1-15+ depending on season).
#'   If NULL, fetches current week (default: NULL).
#' @param year Integer. Season year to fetch (default: current year).
#' @param season_type Integer. Season type: 1=Preseason, 2=Regular, 3=Postseason (default: 2).
#' @param limit Integer. Maximum number of games to retrieve (default: 300).
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'cfb_scoreboard_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data frame. The main purpose is global
#'   environment assignment of \code{cfb_scoreboard} containing:
#'   \itemize{
#'     \item Game information: ID, date, name, status, scores, attendance
#'     \item Season details: year, type, week number
#'     \item Team details: names, abbreviations, colors, logos, records, rankings, conferences
#'     \item Venue information: name, location, capacity, indoor/outdoor
#'     \item Broadcast details: network, language
#'     \item Betting information: odds provider and details
#'   }
#'
#' @details
#' The function creates a structured data frame with comprehensive college football game information.
#' Each row represents a game with detailed information about teams, venue, broadcast, and betting.
#'
#' **Game Information**:
#' \itemize{
#'   \item Basic details: game ID, date, name, status, scores
#'   \item Timing: season year, week number, game clock, period
#'   \item Attendance: actual attendance figures
#' }
#'
#' **Team Details**:
#' \itemize{
#'   \item Identity: team names, abbreviations, locations
#'   \item Conference: conference ID and name
#'   \item Branding: team colors, logos
#'   \item Performance: records, scores, rankings, winner status
#' }
#'
#' **Conference Groups**: Major conferences and their group IDs:
#' \itemize{
#'   \item Power 5: ACC(1), Big 12(4), Big Ten(5), Pac-12(9), SEC(8)
#'   \item Group of 5: American(151), C-USA(12), MAC(15), Mountain West(17), Sun Belt(37)
#'   \item FCS: SWAC(40), MEAC(16), Big Sky(3), CAA(48), etc.
#' }
#'
#' @examples
#' \dontrun{
#' # Get all current games
#' fetch_college_football_scoreboard()
#'
#' # Get SEC games for specific week
#' fetch_college_football_scoreboard(groups = "8", week = 5)
#'
#' # Get games for specific date
#' fetch_college_football_scoreboard(dates = "20241109")
#'
#' # Get multiple conferences
#' fetch_college_football_scoreboard(groups = c("8", "5"), week = 10)  # SEC + Big Ten
#'
#' # Get games for specific year and week
#' fetch_college_football_scoreboard(week = 12, year = 2023)
#'
#' # Check the data
#' head(cfb_scoreboard)
#'
#' # View conference mappings
#' conference_groups <- get_cfb_conference_groups()
#' print(conference_groups)
#'
#' # Analyze ranked matchups
#' ranked_games <- cfb_scoreboard[
#'   !is.na(cfb_scoreboard$home_team_rank) | !is.na(cfb_scoreboard$away_team_rank),
#'   c("game_short_name", "home_team_rank", "home_team_display_name",
#'     "away_team_rank", "away_team_display_name", "home_team_score", "away_team_score")
#' ]
#' print("Ranked team games:")
#' print(ranked_games)
#'
#' # Games by conference
#' conf_games <- table(cfb_scoreboard$home_team_conference_name)
#' print("Games by conference:")
#' print(sort(conf_games, decreasing = TRUE))
#'
#' # Attendance analysis
#' attendance_data <- cfb_scoreboard[!is.na(cfb_scoreboard$game_attendance), ]
#' if(nrow(attendance_data) > 0) {
#'   attendance_data$attendance_num <- as.numeric(attendance_data$game_attendance)
#'   avg_attendance <- mean(attendance_data$attendance_num, na.rm = TRUE)
#'   cat(sprintf("Average attendance: %.0f\n", avg_attendance))
#' }
#' }
#'
#' @seealso \code{\link{get_cfb_conference_groups}} for conference group mappings
#'
#' @export
fetch_college_football_scoreboard <- function(dates = NULL, groups = NULL, week = NULL,
                                              year = as.integer(format(Sys.Date(), "%Y")),
                                              season_type = 2, limit = 300, raw = FALSE) {

  # Input validation
  if (!is.null(dates)) {
    if (!is.character(dates) || nchar(dates) != 8) {
      stop("'dates' must be in YYYYMMDD format (e.g., '20241109')")
    }
    # Validate date format
    if (!grepl("^\\d{8}$", dates)) {
      stop("'dates' must contain only digits in YYYYMMDD format")
    }
  }

  if (!is.null(groups)) {
    groups <- as.character(groups)
    # Validate group IDs are numeric
    if (!all(grepl("^\\d+$", groups))) {
      stop("'groups' must be numeric conference group IDs")
    }
  }

  if (!is.null(week)) {
    if (!is.numeric(week) || week < 1 || week > 20) {
      stop("'week' must be a number between 1 and 20")
    }
  }

  if (!is.numeric(year) || year < 1990 || year > 2030) {
    stop("'year' must be a valid year between 1990 and 2030")
  }

  if (!season_type %in% c(1, 2, 3)) {
    stop("'season_type' must be 1 (preseason), 2 (regular), or 3 (postseason)")
  }

  if (!is.numeric(limit) || limit < 1 || limit > 1000) {
    stop("'limit' must be a number between 1 and 1000")
  }

  # Build API URL
  url <- "https://site.api.espn.com/apis/site/v2/sports/football/college-football/scoreboard"

  # Build query parameters
  params <- list()

  if (!is.null(dates)) {
    params$dates <- dates
    message(sprintf("Fetching college football games for date: %s", dates))
  }

  if (!is.null(groups)) {
    groups_str <- paste(groups, collapse = "&groups=")
    params$groups <- groups_str

    # Map group IDs to conference names for display
    conf_mapping <- get_cfb_conference_groups()
    conf_names <- names(conf_mapping)[conf_mapping %in% groups]
    if (length(conf_names) > 0) {
      message(sprintf("Fetching games for group ID(s): %s", paste(groups, collapse = ", ")))
    }
  }

  if (!is.null(week)) {
    params$week <- week
    message(sprintf("Fetching games for week: %d", week))
  }

  params$seasontype <- season_type
  params$limit <- limit

  # Add query parameters to URL
  if (length(params) > 0) {
    param_strings <- character(0)
    for (name in names(params)) {
      if (name == "groups") {
        # Handle multiple groups specially
        param_strings <- c(param_strings, paste0("groups=", strsplit(params[[name]], "&groups=")[[1]]))
      } else {
        param_strings <- c(param_strings, paste0(name, "=", params[[name]]))
      }
    }
    url <- paste0(url, "?", paste(param_strings, collapse = "&"))
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
    data <- jsonlite::fromJSON(content_text, simplifyVector = FALSE, simplifyDataFrame = FALSE)

    # Handle raw data assignment for debugging
    if (isTRUE(raw)) {
      assign("cfb_scoreboard_raw", data, envir = .GlobalEnv)
      message("Raw college football scoreboard data assigned to: cfb_scoreboard_raw")
      message("Data structure preview:")

      # Show available sections
      sections <- names(data)
      message("- Available sections: ", paste(sections, collapse = ", "))

      # Show events count
      events <- extract_nested_cfb(data, c("events"), list())
      message("- Total games found: ", length(events))

      # Show season info
      season_info <- extract_nested_cfb(data, c("season"), list())
      season_year <- extract_nested_cfb(season_info, c("year"), "Unknown")
      season_type_name <- extract_nested_cfb(season_info, c("type"), "Unknown")
      message(sprintf("- Season: %s (%s)", season_year, season_type_name))

      # Show week info
      week_info <- extract_nested_cfb(data, c("week"), list())
      week_number <- extract_nested_cfb(week_info, c("number"), "Unknown")
      message(sprintf("- Week: %s", week_number))

      if (length(events) > 0) {
        first_event <- events[[1]]
        event_sections <- names(first_event)
        message("- First game sections: ", paste(event_sections, collapse = ", "))
      }

      return(invisible(data))
    }

    # Create scoreboard dataset
    scoreboard_df <- create_cfb_scoreboard_dataset(data,
                                                   ifelse(is.null(groups), NA, paste(groups, collapse = ",")),
                                                   ifelse(is.null(week), NA, week),
                                                   ifelse(is.null(dates), NA, dates))

    # Assign to global environment
    assign("cfb_scoreboard", scoreboard_df, envir = .GlobalEnv)

    # Summary message
    total_games <- nrow(scoreboard_df)

    message(sprintf("College football scoreboard assigned to: cfb_scoreboard (%d games)", total_games))

    if (total_games > 0) {
      # Count game statuses
      completed_games <- sum(scoreboard_df$game_status %in% c("STATUS_FINAL", "Final"), na.rm = TRUE)
      in_progress_games <- sum(scoreboard_df$game_status %in% c("STATUS_IN_PROGRESS", "In Progress"), na.rm = TRUE)
      upcoming_games <- total_games - completed_games - in_progress_games

      message(sprintf("  - Completed games: %d", completed_games))
      message(sprintf("  - In-progress games: %d", in_progress_games))
      message(sprintf("  - Upcoming games: %d", upcoming_games))

      # Show ranked teams
      ranked_home <- sum(!is.na(scoreboard_df$home_team_rank) & scoreboard_df$home_team_rank != "", na.rm = TRUE)
      ranked_away <- sum(!is.na(scoreboard_df$away_team_rank) & scoreboard_df$away_team_rank != "", na.rm = TRUE)
      total_ranked_teams <- ranked_home + ranked_away

      if (total_ranked_teams > 0) {
        message(sprintf("  - Games with ranked teams: %d", sum(ranked_home > 0 | ranked_away > 0)))
      }

      # Show conferences represented
      home_conferences <- unique(scoreboard_df$home_team_conference_name[!is.na(scoreboard_df$home_team_conference_name)])
      away_conferences <- unique(scoreboard_df$away_team_conference_name[!is.na(scoreboard_df$away_team_conference_name)])
      all_conferences <- unique(c(home_conferences, away_conferences))
      all_conferences <- all_conferences[all_conferences != ""]

      if (length(all_conferences) > 0) {
        message(sprintf("  - Conferences represented: %d", length(all_conferences)))
        if (length(all_conferences) <= 5) {
          message(sprintf("    %s", paste(all_conferences, collapse = ", ")))
        }
      }

      # Show recent games
      if (total_games > 0) {
        recent_games <- head(scoreboard_df, 3)
        message("\nRecent games:")
        for (i in 1:nrow(recent_games)) {
          game <- recent_games[i, ]
          home_rank <- ifelse(is.na(game$home_team_rank) || game$home_team_rank == "", "", paste0("#", game$home_team_rank, " "))
          away_rank <- ifelse(is.na(game$away_team_rank) || game$away_team_rank == "", "", paste0("#", game$away_team_rank, " "))

          message(sprintf("  %s: %s%s vs %s%s",
                          substr(game$game_date, 1, 10),
                          away_rank, game$away_team_abbreviation,
                          home_rank, game$home_team_abbreviation))
        }
      }
    }

    return(invisible(scoreboard_df))

  }, error = function(e) {
    stop(sprintf("Failed to fetch college football scoreboard: %s", e$message))
  })
}

#' Fetch college football scoreboard for multiple conferences
#'
#' Retrieves scoreboard data for multiple conferences with rate limiting.
#' This function calls \code{\link{fetch_college_football_scoreboard}} for each conference
#' and combines the results.
#'
#' @param conferences Character vector. Conference names or group IDs to fetch.
#'   Can use conference names from \code{get_cfb_conference_groups()} or group IDs.
#'   Examples: c("SEC", "Big Ten"), c("8", "5"), c("SEC", "5").
#' @param week Integer. Week number to fetch (default: NULL for current week).
#' @param year Integer. Season year to fetch (default: current year).
#' @param season_type Integer. Season type (default: 2 for regular season).
#' @param dates Character. Date in YYYYMMDD format (default: NULL).
#' @param delay Numeric. Delay in seconds between API requests (default: 0.5).
#' @param raw Logical. If TRUE, assigns raw JSON to global environment
#'   for the first conference only (default: FALSE).
#'
#' @return Invisibly returns the combined data frame. The main purpose is global
#'   environment assignment of combined \code{cfb_scoreboard} from all conferences.
#'
#' @examples
#' \dontrun{
#' # Get Power 5 conferences
#' fetch_multiple_cfb_scoreboard(c("SEC", "Big Ten", "ACC", "Big 12", "Pac-12"))
#'
#' # Get specific conferences by ID
#' fetch_multiple_cfb_scoreboard(c("8", "5"), week = 10)
#'
#' # Mix names and IDs
#' fetch_multiple_cfb_scoreboard(c("SEC", "5", "ACC"), week = 12)
#' }
#'
#' @seealso \code{\link{fetch_college_football_scoreboard}} for single conference data
#' @export
fetch_multiple_cfb_scoreboard <- function(conferences, week = NULL,
                                          year = as.integer(format(Sys.Date(), "%Y")),
                                          season_type = 2, dates = NULL, delay = 0.5, raw = FALSE) {
  # Input validation
  if (length(conferences) == 0) {
    stop("'conferences' must contain at least one conference")
  }

  # Convert conference names to group IDs if necessary
  conf_mapping <- get_cfb_conference_groups()
  group_ids <- character(0)

  for (conf in conferences) {
    if (conf %in% names(conf_mapping)) {
      # It's a conference name
      group_ids <- c(group_ids, conf_mapping[conf])
    } else if (conf %in% conf_mapping) {
      # It's already a group ID
      group_ids <- c(group_ids, conf)
    } else {
      warning(sprintf("Unknown conference '%s' - skipping", conf))
    }
  }

  if (length(group_ids) == 0) {
    stop("No valid conferences found")
  }

  # Initialize combined data container
  all_games <- data.frame()

  message(sprintf("Starting to fetch college football scoreboard for %d conferences...", length(group_ids)))

  # Process each conference sequentially
  for (i in seq_along(group_ids)) {
    group_id <- group_ids[i]

    # Find conference name for display
    conf_name <- names(conf_mapping)[conf_mapping == group_id]
    if (length(conf_name) == 0) conf_name <- paste("Group", group_id)

    message(sprintf("Fetching %s games (%d/%d)...", conf_name, i, length(group_ids)))

    tryCatch({
      # Fetch individual conference data
      conf_data <- fetch_college_football_scoreboard(
        dates = dates,
        groups = group_id,
        week = week,
        year = year,
        season_type = season_type,
        raw = raw
      )

      # If raw data requested, return after first conference
      if (isTRUE(raw)) {
        return(invisible(conf_data))
      }

      # Combine data
      games_df <- get("cfb_scoreboard", envir = .GlobalEnv)
      all_games <- rbind(all_games, games_df)

    }, error = function(e) {
      message(sprintf("Failed to fetch %s games: %s", conf_name, e$message))
    })

    # Be respectful to the API with delay between requests
    if (i < length(group_ids)) {
      Sys.sleep(delay)
    }
  }

  # Remove duplicates and assign combined dataset to global environment
  if (nrow(all_games) > 0) {
    all_games <- all_games[!duplicated(all_games$game_id), ]
    assign("cfb_scoreboard", all_games, envir = .GlobalEnv)

    total_games <- nrow(all_games)
    unique_conferences <- length(unique(c(all_games$home_team_conference_name, all_games$away_team_conference_name)))

    message(sprintf("Combined college football scoreboard assigned to: cfb_scoreboard (%d games, %d conferences)",
                    total_games, unique_conferences))

    # Show conference breakdown
    conf_counts <- table(all_games$query_groups)
    message("Games per conference group:")
    for (group_id in names(conf_counts)) {
      conf_name <- names(conf_mapping)[conf_mapping == group_id]
      if (length(conf_name) == 0) conf_name <- paste("Group", group_id)
      message(sprintf("  %s: %d games", conf_name, conf_counts[group_id]))
    }
  }

  return(invisible(all_games))
}
