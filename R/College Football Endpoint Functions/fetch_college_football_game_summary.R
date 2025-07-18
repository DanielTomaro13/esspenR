#' Safe nested data extraction helper function for college football game summary
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_game <- function(data, path, default = NA) {
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

#' Create college football game summary data frame from Site API response
#'
#' Processes raw JSON response from ESPN Site API into structured data frame
#' containing comprehensive game summary information
#'
#' @param data Raw JSON response from ESPN Site API game summary endpoint
#' @param game_id Character. Game ID used in request
#' @return Data frame with game summary information
#' @keywords internal
create_cfb_game_summary_dataset <- function(data, game_id) {

  # Initialize game summary data frame with game-focused fields only
  game_summary_df <- data.frame(
    query_game_id = character(0),
    game_id = character(0),
    game_uid = character(0),
    game_date = character(0),
    game_name = character(0),
    game_short_name = character(0),
    season_year = character(0),
    season_type = character(0),
    week_number = character(0),
    game_status = character(0),
    game_status_detail = character(0),
    game_clock = character(0),
    game_period = character(0),
    game_completed = character(0),
    attendance = character(0),
    venue_id = character(0),
    venue_name = character(0),
    venue_full_name = character(0),
    venue_city = character(0),
    venue_state = character(0),
    venue_capacity = character(0),
    venue_indoor = character(0),
    broadcast_network = character(0),
    broadcast_type = character(0),
    officials_count = character(0),
    weather_conditions = character(0),
    weather_temperature = character(0),
    weather_humidity = character(0),
    weather_wind_speed = character(0),
    weather_wind_direction = character(0),
    home_team_id = character(0),
    home_team_uid = character(0),
    home_team_abbreviation = character(0),
    home_team_display_name = character(0),
    home_team_name = character(0),
    home_team_logo = character(0),
    home_team_color = character(0),
    home_team_score = character(0),
    home_team_line_score = character(0),
    home_team_record = character(0),
    home_team_rank = character(0),
    home_team_winner = character(0),
    away_team_id = character(0),
    away_team_uid = character(0),
    away_team_abbreviation = character(0),
    away_team_display_name = character(0),
    away_team_name = character(0),
    away_team_logo = character(0),
    away_team_color = character(0),
    away_team_score = character(0),
    away_team_line_score = character(0),
    away_team_record = character(0),
    away_team_rank = character(0),
    away_team_winner = character(0),
    game_notes = character(0),
    game_situation = character(0),
    last_play = character(0),
    game_note_special = character(0),
    # Game data availability flags
    drives_count = character(0),
    scoring_plays_count = character(0),
    total_plays = character(0),
    videos_count = character(0),
    has_boxscore = character(0),
    has_drives = character(0),
    has_scoring_plays = character(0),
    has_win_probability = character(0),
    stringsAsFactors = FALSE
  )

  # Extract header information (basic game data)
  header <- extract_nested_game(data, c("header"), list())

  game_uid <- extract_nested_game(header, c("uid"), NA_character_)

  # Competition information
  competitions <- extract_nested_game(header, c("competitions"), list())
  if (length(competitions) == 0) {
    # Try alternative path
    competitions <- extract_nested_game(data, c("competitions"), list())
  }

  if (length(competitions) == 0) {
    warning("No competition data found in game summary")
    return(game_summary_df)
  }

  competition <- competitions[[1]]

  # Basic game information
  game_date <- extract_nested_game(competition, c("date"), NA_character_)
  game_name <- extract_nested_game(competition, c("name"), NA_character_)
  game_short_name <- extract_nested_game(competition, c("shortName"), NA_character_)
  game_completed <- extract_nested_game(competition, c("completed"), "false")
  attendance <- extract_nested_game(competition, c("attendance"), NA_character_)

  # Season information
  season_year <- extract_nested_game(competition, c("season", "year"), NA_character_)
  season_type <- extract_nested_game(competition, c("season", "type"), NA_character_)
  week_number <- extract_nested_game(competition, c("week", "number"), NA_character_)

  # Status information
  status <- extract_nested_game(competition, c("status"), list())
  game_status <- extract_nested_game(status, c("type", "name"), NA_character_)
  game_status_detail <- extract_nested_game(status, c("type", "detail"), NA_character_)
  game_clock <- extract_nested_game(status, c("displayClock"), NA_character_)
  game_period <- extract_nested_game(status, c("period"), NA_character_)

  # Venue information
  venue <- extract_nested_game(competition, c("venue"), list())
  venue_id <- extract_nested_game(venue, c("id"), NA_character_)
  venue_name <- extract_nested_game(venue, c("shortName"), NA_character_)
  venue_full_name <- extract_nested_game(venue, c("fullName"), NA_character_)
  venue_capacity <- extract_nested_game(venue, c("capacity"), NA_character_)
  venue_indoor <- extract_nested_game(venue, c("indoor"), "false")

  venue_address <- extract_nested_game(venue, c("address"), list())
  venue_city <- extract_nested_game(venue_address, c("city"), NA_character_)
  venue_state <- extract_nested_game(venue_address, c("state"), NA_character_)

  # Broadcast information
  broadcasts <- extract_nested_game(competition, c("broadcasts"), list())
  broadcast_network <- NA_character_
  broadcast_type <- NA_character_
  if (length(broadcasts) > 0) {
    first_broadcast <- broadcasts[[1]]
    broadcast_names <- extract_nested_game(first_broadcast, c("names"), list())
    if (length(broadcast_names) > 0) {
      broadcast_network <- broadcast_names[[1]]
    }
    broadcast_type <- extract_nested_game(first_broadcast, c("type", "shortName"), NA_character_)
  }

  # Officials information
  officials <- extract_nested_game(competition, c("officials"), list())
  officials_count <- as.character(length(officials))

  # Weather information (if available)
  weather <- extract_nested_game(competition, c("weather"), list())
  weather_conditions <- extract_nested_game(weather, c("conditionId"), NA_character_)
  weather_temperature <- extract_nested_game(weather, c("temperature"), NA_character_)
  weather_humidity <- extract_nested_game(weather, c("humidity"), NA_character_)
  weather_wind_speed <- extract_nested_game(weather, c("windSpeed"), NA_character_)
  weather_wind_direction <- extract_nested_game(weather, c("windDirection"), NA_character_)

  # Team information
  competitors <- extract_nested_game(competition, c("competitors"), list())

  home_team_id <- NA_character_
  home_team_uid <- NA_character_
  home_team_abbreviation <- NA_character_
  home_team_display_name <- NA_character_
  home_team_name <- NA_character_
  home_team_logo <- NA_character_
  home_team_color <- NA_character_
  home_team_score <- NA_character_
  home_team_line_score <- NA_character_
  home_team_record <- NA_character_
  home_team_rank <- NA_character_
  home_team_winner <- "false"

  away_team_id <- NA_character_
  away_team_uid <- NA_character_
  away_team_abbreviation <- NA_character_
  away_team_display_name <- NA_character_
  away_team_name <- NA_character_
  away_team_logo <- NA_character_
  away_team_color <- NA_character_
  away_team_score <- NA_character_
  away_team_line_score <- NA_character_
  away_team_record <- NA_character_
  away_team_rank <- NA_character_
  away_team_winner <- "false"

  for (competitor in competitors) {
    home_away <- extract_nested_game(competitor, c("homeAway"), NA_character_)

    team <- extract_nested_game(competitor, c("team"), list())
    team_id <- extract_nested_game(team, c("id"), NA_character_)
    team_uid <- extract_nested_game(team, c("uid"), NA_character_)
    team_abbreviation <- extract_nested_game(team, c("abbreviation"), NA_character_)
    team_display_name <- extract_nested_game(team, c("displayName"), NA_character_)
    team_name <- extract_nested_game(team, c("name"), NA_character_)
    team_color <- extract_nested_game(team, c("color"), NA_character_)

    # Logo
    team_logo <- NA_character_
    logos <- extract_nested_game(team, c("logos"), list())
    if (length(logos) > 0) {
      team_logo <- extract_nested_game(logos[[1]], c("href"), NA_character_)
    }

    # Score and line score
    team_score <- extract_nested_game(competitor, c("score"), NA_character_)
    line_scores <- extract_nested_game(competitor, c("linescores"), list())
    line_score_values <- sapply(line_scores, function(ls) extract_nested_game(ls, c("value"), "0"))
    team_line_score <- paste(line_score_values, collapse = "-")

    # Record
    records <- extract_nested_game(competitor, c("records"), list())
    team_record <- NA_character_
    if (length(records) > 0) {
      for (record in records) {
        if (extract_nested_game(record, c("type"), "") == "total") {
          team_record <- extract_nested_game(record, c("summary"), NA_character_)
          break
        }
      }
      if (is.na(team_record) && length(records) > 0) {
        team_record <- extract_nested_game(records[[1]], c("summary"), NA_character_)
      }
    }

    # Ranking
    team_rank <- extract_nested_game(competitor, c("curRank"), NA_character_)

    # Winner status
    team_winner <- extract_nested_game(competitor, c("winner"), "false")

    if (!is.na(home_away) && home_away == "home") {
      home_team_id <- team_id
      home_team_uid <- team_uid
      home_team_abbreviation <- team_abbreviation
      home_team_display_name <- team_display_name
      home_team_name <- team_name
      home_team_logo <- team_logo
      home_team_color <- team_color
      home_team_score <- team_score
      home_team_line_score <- team_line_score
      home_team_record <- team_record
      home_team_rank <- team_rank
      home_team_winner <- team_winner
    } else if (!is.na(home_away) && home_away == "away") {
      away_team_id <- team_id
      away_team_uid <- team_uid
      away_team_abbreviation <- team_abbreviation
      away_team_display_name <- team_display_name
      away_team_name <- team_name
      away_team_logo <- team_logo
      away_team_color <- team_color
      away_team_score <- team_score
      away_team_line_score <- team_line_score
      away_team_record <- team_record
      away_team_rank <- team_rank
      away_team_winner <- team_winner
    }
  }

  # Game notes and situation
  game_notes <- NA_character_
  notes <- extract_nested_game(competition, c("notes"), list())
  if (length(notes) > 0) {
    note_texts <- sapply(notes, function(note) extract_nested_game(note, c("headline"), ""))
    game_notes <- paste(note_texts[note_texts != ""], collapse = "; ")
  }

  # Current game situation
  game_situation <- extract_nested_game(competition, c("situation", "shortDownDistanceText"), NA_character_)

  # Last play (if available)
  last_play <- extract_nested_game(competition, c("situation", "lastPlay", "text"), NA_character_)

  # Additional rich data extraction
  drives_count <- "0"
  drives_data <- extract_nested_game(data, c("drives"), list())
  if (length(drives_data) > 0) {
    drives_previous <- extract_nested_game(drives_data, c("previous"), list())
    drives_count <- as.character(length(drives_previous))
  }

  # Scoring plays
  scoring_plays_count <- "0"
  scoring_plays <- extract_nested_game(data, c("scoringPlays"), list())
  if (length(scoring_plays) > 0) {
    scoring_plays_count <- as.character(length(scoring_plays))
  }

  # Videos
  videos_count <- "0"
  videos <- extract_nested_game(data, c("videos"), list())
  if (length(videos) > 0) {
    videos_count <- as.character(length(videos))
  }

  # Data availability flags
  has_boxscore <- ifelse("boxscore" %in% names(data), "true", "false")
  has_drives <- ifelse("drives" %in% names(data), "true", "false")
  has_scoring_plays <- ifelse("scoringPlays" %in% names(data), "true", "false")
  has_win_probability <- ifelse("winprobability" %in% names(data), "true", "false")

  # Special game note (like championship game designation)
  game_note_special <- extract_nested_game(header, c("gameNote"), NA_character_)
  if (is.na(game_note_special)) {
    game_note_special <- extract_nested_game(data, c("header", "gameNote"), NA_character_)
  }

  # Calculate total plays estimate
  total_plays <- "0"
  if (has_drives == "true" && length(drives_previous) > 0) {
    play_count <- 0
    for (drive in drives_previous) {
      plays <- extract_nested_game(drive, c("plays"), list())
      play_count <- play_count + length(plays)
    }
    total_plays <- as.character(play_count)
  }

  # Create row
  game_row <- data.frame(
    query_game_id = as.character(game_id),
    game_id = as.character(game_id),
    game_uid = as.character(game_uid),
    game_date = as.character(game_date),
    game_name = as.character(game_name),
    game_short_name = as.character(game_short_name),
    season_year = as.character(season_year),
    season_type = as.character(season_type),
    week_number = as.character(week_number),
    game_status = as.character(game_status),
    game_status_detail = as.character(game_status_detail),
    game_clock = as.character(game_clock),
    game_period = as.character(game_period),
    game_completed = as.character(game_completed),
    attendance = as.character(attendance),
    venue_id = as.character(venue_id),
    venue_name = as.character(venue_name),
    venue_full_name = as.character(venue_full_name),
    venue_city = as.character(venue_city),
    venue_state = as.character(venue_state),
    venue_capacity = as.character(venue_capacity),
    venue_indoor = as.character(venue_indoor),
    broadcast_network = as.character(broadcast_network),
    broadcast_type = as.character(broadcast_type),
    officials_count = as.character(officials_count),
    weather_conditions = as.character(weather_conditions),
    weather_temperature = as.character(weather_temperature),
    weather_humidity = as.character(weather_humidity),
    weather_wind_speed = as.character(weather_wind_speed),
    weather_wind_direction = as.character(weather_wind_direction),
    home_team_id = as.character(home_team_id),
    home_team_uid = as.character(home_team_uid),
    home_team_abbreviation = as.character(home_team_abbreviation),
    home_team_display_name = as.character(home_team_display_name),
    home_team_name = as.character(home_team_name),
    home_team_logo = as.character(home_team_logo),
    home_team_color = as.character(home_team_color),
    home_team_score = as.character(home_team_score),
    home_team_line_score = as.character(home_team_line_score),
    home_team_record = as.character(home_team_record),
    home_team_rank = as.character(home_team_rank),
    home_team_winner = as.character(home_team_winner),
    away_team_id = as.character(away_team_id),
    away_team_uid = as.character(away_team_uid),
    away_team_abbreviation = as.character(away_team_abbreviation),
    away_team_display_name = as.character(away_team_display_name),
    away_team_name = as.character(away_team_name),
    away_team_logo = as.character(away_team_logo),
    away_team_color = as.character(away_team_color),
    away_team_score = as.character(away_team_score),
    away_team_line_score = as.character(away_team_line_score),
    away_team_record = as.character(away_team_record),
    away_team_rank = as.character(away_team_rank),
    away_team_winner = as.character(away_team_winner),
    game_notes = as.character(game_notes),
    game_situation = as.character(game_situation),
    last_play = as.character(last_play),
    game_note_special = as.character(game_note_special),
    # Game data availability flags
    drives_count = as.character(drives_count),
    scoring_plays_count = as.character(scoring_plays_count),
    total_plays = as.character(total_plays),
    videos_count = as.character(videos_count),
    has_boxscore = as.character(has_boxscore),
    has_drives = as.character(has_drives),
    has_scoring_plays = as.character(has_scoring_plays),
    has_win_probability = as.character(has_win_probability),
    stringsAsFactors = FALSE
  )

  game_summary_df <- rbind(game_summary_df, game_row)

  # Clean up row names
  if (nrow(game_summary_df) > 0) rownames(game_summary_df) <- NULL

  return(game_summary_df)
}

#' Fetch college football game summary using Site API
#'
#' Retrieves comprehensive game summary information for a specific college football game from ESPN's Site API.
#' The function fetches detailed game data including teams, scores, venue, weather, officials,
#' and broadcast information.
#'
#' @param game_id Character. ESPN game ID for the specific game.
#'   Examples: "400934572" (2017 Army-Navy), "401012678", etc.
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'cfb_game_summary_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data frame. The main purpose is global
#'   environment assignment of \code{cfb_game_summary} containing:
#'   \itemize{
#'     \item Game information: ID, date, name, status, scores, attendance
#'     \item Season details: year, type, week number
#'     \item Team details: names, logos, colors, records, rankings, line scores
#'     \item Venue information: name, location, capacity, indoor status
#'     \item Broadcast details: network, type
#'     \item Game conditions: weather, officials count
#'     \item Game state: current situation, last play, notes
#'   }
#'
#' @details
#' The function creates a comprehensive data frame with detailed college football game information.
#' This provides much more detail than basic scoreboards, including real-time game state,
#' weather conditions, and complete team performance data.
#'
#' **Game Information**:
#' \itemize{
#'   \item Identity: game ID, date, name, season context
#'   \item Status: current status, period, clock, completion status
#'   \item Performance: scores, line scores, winner designation
#' }
#'
#' **Team Details**:
#' \itemize{
#'   \item Identity: team names, abbreviations, logos, colors
#'   \item Performance: current scores, quarterly/period line scores
#'   \item Context: season records, current rankings
#' }
#'
#' **Game Environment**:
#' \itemize{
#'   \item Venue: stadium details, location, capacity, indoor status
#'   \item Weather: temperature, humidity, wind conditions (when available)
#'   \item Broadcast: network, broadcast type
#'   \item Officials: number of officials assigned
#' }
#'
#' **Game State** (for live/recent games):
#' \itemize{
#'   \item Situation: current down and distance, field position
#'   \item Last play: description of most recent play
#'   \item Notes: special game notes or circumstances
#' }
#'
#' @examples
#' \dontrun{
#' # Get 2017 Army-Navy game summary
#' fetch_college_football_game_summary("400934572")
#'
#' # Get recent game
#' fetch_college_football_game_summary("401520281")
#'
#' # Check the detailed data
#' head(cfb_game_summary)
#'
#' # View game basics
#' game_basics <- cfb_game_summary[, c("game_name", "game_date", "game_status",
#'                                    "home_team_display_name", "home_team_score",
#'                                    "away_team_display_name", "away_team_score")]
#' print("Game basics:")
#' print(game_basics)
#'
#' # View team details
#' team_details <- cfb_game_summary[, c("home_team_abbreviation", "home_team_record", "home_team_rank",
#'                                     "away_team_abbreviation", "away_team_record", "away_team_rank")]
#' print("Team details:")
#' print(team_details)
#'
#' # View venue and conditions
#' venue_info <- cfb_game_summary[, c("venue_full_name", "venue_city", "venue_state",
#'                                   "venue_capacity", "attendance", "weather_temperature")]
#' print("Venue and conditions:")
#' print(venue_info)
#'
#' # View scoring by period
#' scoring_info <- cfb_game_summary[, c("home_team_display_name", "home_team_line_score", "home_team_score",
#'                                     "away_team_display_name", "away_team_line_score", "away_team_score")]
#' print("Scoring breakdown:")
#' print(scoring_info)
#'
#' # Analyze attendance vs capacity
#' if(!is.na(cfb_game_summary$attendance) && !is.na(cfb_game_summary$venue_capacity)) {
#'   attendance_num <- as.numeric(cfb_game_summary$attendance)
#'   capacity_num <- as.numeric(cfb_game_summary$venue_capacity)
#'
#'   if(!is.na(attendance_num) && !is.na(capacity_num) && capacity_num > 0) {
#'     attendance_pct <- (attendance_num / capacity_num) * 100
#'     cat(sprintf("Attendance: %s / %s (%.1f%% capacity)\n",
#'                 format(attendance_num, big.mark = ","),
#'                 format(capacity_num, big.mark = ","),
#'                 attendance_pct))
#'   }
#' }
#'
#' # Weather analysis
#' if(!is.na(cfb_game_summary$weather_temperature)) {
#'   cat(sprintf("Weather: %s°F", cfb_game_summary$weather_temperature))
#'   if(!is.na(cfb_game_summary$weather_wind_speed)) {
#'     cat(sprintf(", Wind: %s mph", cfb_game_summary$weather_wind_speed))
#'   }
#'   if(!is.na(cfb_game_summary$weather_humidity)) {
#'     cat(sprintf(", Humidity: %s%%", cfb_game_summary$weather_humidity))
#'   }
#'   cat("\n")
#' }
#'
#' # Live game information
#' if(cfb_game_summary$game_status == "In Progress") {
#'   cat("LIVE GAME STATUS:\n")
#'   cat(sprintf("Period: %s, Clock: %s\n", cfb_game_summary$game_period, cfb_game_summary$game_clock))
#'
#'   if(!is.na(cfb_game_summary$game_situation)) {
#'     cat(sprintf("Situation: %s\n", cfb_game_summary$game_situation))
#'   }
#'
#'   if(!is.na(cfb_game_summary$last_play)) {
#'     cat(sprintf("Last play: %s\n", cfb_game_summary$last_play))
#'   }
#' }
#' }
#'
#' @export
fetch_college_football_game_summary <- function(game_id, raw = FALSE) {

  # Input validation
  if (missing(game_id) || is.null(game_id) || game_id == "") {
    stop("'game_id' parameter is required. Provide ESPN game ID (e.g., '400934572').")
  }

  game_id <- as.character(game_id)

  # Validate game ID format (should be numeric)
  if (!grepl("^\\d+$", game_id)) {
    stop("'game_id' should be a numeric ESPN game ID (e.g., '400934572')")
  }

  # Build API URL
  url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/football/college-football/summary?event=%s", game_id)

  # Fetch and parse data
  tryCatch({
    message(sprintf("Fetching game summary for game ID: %s", game_id))

    resp <- httr::GET(url, httr::timeout(30))

    if (httr::status_code(resp) != 200) {
      stop(sprintf("API request failed: HTTP %d - %s. Game ID '%s' may not exist or be invalid.",
                   httr::status_code(resp),
                   httr::http_status(resp)$message,
                   game_id))
    }

    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content_text, simplifyVector = FALSE, simplifyDataFrame = FALSE)

    # Handle raw data assignment for debugging
    if (isTRUE(raw)) {
      assign("cfb_game_summary_raw", data, envir = .GlobalEnv)
      message("Raw game summary data assigned to: cfb_game_summary_raw")
      message("Data structure preview:")

      # Show available sections
      sections <- names(data)
      message("- Available sections: ", paste(sections, collapse = ", "))

      # Show key game information
      header <- extract_nested_game(data, c("header"), list())
      if (length(header) > 0) {
        competitions <- extract_nested_game(header, c("competitions"), list())
        if (length(competitions) > 0) {
          comp <- competitions[[1]]
          game_name <- extract_nested_game(comp, c("name"), "Unknown Game")
          game_date <- extract_nested_game(comp, c("date"), "Unknown Date")
          status <- extract_nested_game(comp, c("status", "type", "name"), "Unknown Status")

          message(sprintf("- Game: %s", game_name))
          message(sprintf("- Date: %s", substr(game_date, 1, 10)))
          message(sprintf("- Status: %s", status))
        }
      }

      return(invisible(data))
    }

    # Create game summary dataset
    game_summary_df <- create_cfb_game_summary_dataset(data, game_id)

    # Assign to global environment
    assign("cfb_game_summary", game_summary_df, envir = .GlobalEnv)

    # Summary message
    if (nrow(game_summary_df) > 0) {
      game_info <- game_summary_df[1, ]

      message(sprintf("Game summary assigned to: cfb_game_summary"))
      message(sprintf("Game: %s", game_info$game_name))
      message(sprintf("Date: %s", substr(game_info$game_date, 1, 10)))
      message(sprintf("Status: %s", game_info$game_status))

      # Show team matchup and score
      if (!is.na(game_info$away_team_display_name) && !is.na(game_info$home_team_display_name)) {
        matchup <- sprintf("%s vs %s", game_info$away_team_display_name, game_info$home_team_display_name)
        if (!is.na(game_info$away_team_score) && !is.na(game_info$home_team_score)) {
          matchup <- sprintf("%s %s-%s", matchup, game_info$away_team_score, game_info$home_team_score)
        }
        message(sprintf("Matchup: %s", matchup))
      }

      # Show venue information
      if (!is.na(game_info$venue_full_name)) {
        venue_info <- game_info$venue_full_name
        if (!is.na(game_info$venue_city) && !is.na(game_info$venue_state)) {
          venue_info <- sprintf("%s (%s, %s)", venue_info, game_info$venue_city, game_info$venue_state)
        }
        message(sprintf("Venue: %s", venue_info))
      }

      # Show attendance information
      if (!is.na(game_info$attendance) && game_info$attendance != "") {
        attendance_msg <- sprintf("Attendance: %s", format(as.numeric(game_info$attendance), big.mark = ","))
        if (!is.na(game_info$venue_capacity) && game_info$venue_capacity != "") {
          capacity <- as.numeric(game_info$venue_capacity)
          attendance_num <- as.numeric(game_info$attendance)
          if (!is.na(capacity) && !is.na(attendance_num) && capacity > 0) {
            pct <- round((attendance_num / capacity) * 100, 1)
            attendance_msg <- sprintf("%s (%.1f%% capacity)", attendance_msg, pct)
          }
        }
        message(attendance_msg)
      }

      # Show broadcast information
      if (!is.na(game_info$broadcast_network) && game_info$broadcast_network != "") {
        message(sprintf("Broadcast: %s", game_info$broadcast_network))
      }

      # Show current game state for live games
      if (game_info$game_status %in% c("In Progress", "Halftime", "End Period")) {
        if (!is.na(game_info$game_period) && !is.na(game_info$game_clock)) {
          message(sprintf("Current: Period %s, %s", game_info$game_period, game_info$game_clock))
        }
        if (!is.na(game_info$game_situation) && game_info$game_situation != "") {
          message(sprintf("Situation: %s", game_info$game_situation))
        }
      }

      # Show weather if available
      if (!is.na(game_info$weather_temperature) && game_info$weather_temperature != "") {
        weather_msg <- sprintf("Weather: %s°F", game_info$weather_temperature)
        if (!is.na(game_info$weather_wind_speed) && game_info$weather_wind_speed != "") {
          weather_msg <- sprintf("%s, Wind: %s mph", weather_msg, game_info$weather_wind_speed)
        }
        message(weather_msg)
      }

      # Show data completeness
      non_na_fields <- sum(!is.na(game_info) & game_info != "")
      total_fields <- ncol(game_info)
      completeness <- round((non_na_fields / total_fields) * 100, 1)
      message(sprintf("Data completeness: %d/%d fields (%.1f%%)", non_na_fields, total_fields, completeness))

    } else {
      message("No game data could be extracted from API response")
    }

    return(invisible(game_summary_df))

  }, error = function(e) {
    stop(sprintf("Failed to fetch game summary for game ID '%s': %s", game_id, e$message))
  })
}

#' Fetch game summaries for multiple college football games
#'
#' Retrieves comprehensive game summary information for multiple college football games with rate limiting.
#' This function calls \code{\link{fetch_college_football_game_summary}} for each game
#' and combines the results.
#'
#' @param game_ids Character vector. ESPN game IDs for the games to fetch.
#' @param delay Numeric. Delay in seconds between API requests (default: 0.5).
#'   Used to be respectful to ESPN's servers.
#' @param raw Logical. If TRUE, assigns raw JSON to global environment
#'   for the first game only (default: FALSE).
#'
#' @return Invisibly returns the combined data frame. The main purpose is global
#'   environment assignment of combined \code{cfb_game_summary} from all games.
#'
#' @examples
#' \dontrun{
#' # Get summaries for multiple games
#' game_ids <- c("400934572", "401012678", "401520281")
#' fetch_multiple_cfb_game_summaries(game_ids)
#'
#' # Check combined results
#' head(cfb_game_summary)
#'
#' # Compare attendance across games
#' attendance_comparison <- cfb_game_summary[, c("game_name", "venue_full_name",
#'                                              "attendance", "venue_capacity")]
#' attendance_comparison$attendance_num <- as.numeric(attendance_comparison$attendance)
#' attendance_comparison$capacity_num <- as.numeric(attendance_comparison$venue_capacity)
#' attendance_comparison$capacity_pct <- round((attendance_comparison$attendance_num /
#'                                            attendance_comparison$capacity_num) * 100, 1)
#'
#' print("Attendance comparison:")
#' print(attendance_comparison[order(-attendance_comparison$attendance_num), ])
#'
#' # Analyze game outcomes
#' game_outcomes <- cfb_game_summary[, c("game_name", "home_team_display_name", "home_team_score",
#'                                      "away_team_display_name", "away_team_score",
#'                                      "home_team_winner", "away_team_winner")]
#' print("Game outcomes:")
#' print(game_outcomes)
#' }
#'
#' @seealso \code{\link{fetch_college_football_game_summary}} for single game data
#' @export
fetch_multiple_cfb_game_summaries <- function(game_ids, delay = 0.5, raw = FALSE) {
  # Input validation
  if (length(game_ids) == 0) {
    stop("'game_ids' must contain at least one game ID")
  }

  if (!is.numeric(delay) || delay < 0) {
    stop("'delay' must be a non-negative numeric value")
  }

  # Initialize combined data container
  all_games <- data.frame()

  message(sprintf("Starting to fetch game summaries for %d games...", length(game_ids)))

  # Process each game sequentially
  for (i in seq_along(game_ids)) {
    game_id <- game_ids[i]
    message(sprintf("Fetching game %s (%d/%d)...", game_id, i, length(game_ids)))

    tryCatch({
      # Fetch individual game data
      game_data <- fetch_college_football_game_summary(
        game_id = game_id,
        raw = raw
      )

      # If raw data requested, return after first game
      if (isTRUE(raw)) {
        return(invisible(game_data))
      }

      # Combine data
      if (exists("cfb_game_summary", envir = .GlobalEnv)) {
        game_df <- get("cfb_game_summary", envir = .GlobalEnv)
        all_games <- rbind(all_games, game_df)
      }

    }, error = function(e) {
      message(sprintf("Failed to fetch game %s: %s", game_id, e$message))
    })

    # Be respectful to the API with delay between requests
    if (i < length(game_ids)) {
      Sys.sleep(delay)
    }
  }

  # Assign combined dataset to global environment
  if (nrow(all_games) > 0) {
    # Remove duplicates based on game ID
    all_games <- all_games[!duplicated(all_games$game_id), ]
    assign("cfb_game_summary", all_games, envir = .GlobalEnv)

    total_games <- nrow(all_games)

    message(sprintf("Combined game summaries assigned to: cfb_game_summary (%d games)", total_games))

    # Show summary statistics
    completed_games <- sum(all_games$game_completed == "true", na.rm = TRUE)
    games_with_attendance <- sum(!is.na(all_games$attendance) & all_games$attendance != "")
    games_with_weather <- sum(!is.na(all_games$weather_temperature) & all_games$weather_temperature != "")

    message(sprintf("  - Completed games: %d", completed_games))
    message(sprintf("  - Games with attendance data: %d", games_with_attendance))
    message(sprintf("  - Games with weather data: %d", games_with_weather))

    # Show venue breakdown
    unique_venues <- length(unique(all_games$venue_full_name[!is.na(all_games$venue_full_name) & all_games$venue_full_name != ""]))
    message(sprintf("  - Unique venues: %d", unique_venues))

    # Show broadcast networks
    networks <- unique(all_games$broadcast_network[!is.na(all_games$broadcast_network) & all_games$broadcast_network != ""])
    if (length(networks) > 0) {
      message(sprintf("  - Broadcast networks: %s", paste(networks, collapse = ", ")))
    }

    # Show average data completeness
    completeness_scores <- apply(all_games, 1, function(row) {
      sum(!is.na(row) & row != "") / length(row)
    })
    avg_completeness <- round(mean(completeness_scores) * 100, 1)
    message(sprintf("  - Average data completeness: %.1f%%", avg_completeness))
  }

  return(invisible(all_games))
}
