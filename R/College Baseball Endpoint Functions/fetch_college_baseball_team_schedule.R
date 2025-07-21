#' Fetch college_baseball team schedule data using ESPN Site API
#'
#' @param team_id Character or Numeric. ESPN team ID (e.g., "10" for New York Yankees).
#' @param season Numeric. Season year (e.g., 2023, 2024). Optional - defaults to current season.
#' @param raw Logical. If TRUE, assigns raw JSON to global environment as 'college_baseball_schedule_raw'. Default FALSE.
#' @return Invisibly returns the data, but main purpose is global environment assignment.
#' @export
#'
#' @examples
#' # Get current season schedule for New York Yankees
#' fetch_college_baseball_team_schedule(team_id = "10")
#' head(college_baseball_team_schedule)
#'
#' # Get 2023 season schedule
#' fetch_college_baseball_team_schedule(team_id = 10, season = 2023)
#'
#' # Get raw data for custom processing
#' fetch_college_baseball_team_schedule(team_id = "10", season = 2024, raw = TRUE)
#' str(college_baseball_schedule_raw, max.level = 2)
#'
fetch_college_baseball_team_schedule <- function(team_id, season = NULL, raw = FALSE) {
  # Validate inputs
  if (missing(team_id)) {
    stop("'team_id' is a required parameter")
  }

  # Convert team_id to character for URL building
  team_id <- as.character(team_id)

  # Validate season if provided
  if (!is.null(season)) {
    if (!is.numeric(season) || season < 1900 || season > as.numeric(format(Sys.Date(), "%Y")) + 2) {
      stop("season must be a valid numeric year between 1900 and two years in the future")
    }
  }

  # Build URL
  base_url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/baseball/college-baseball/teams/%s/schedule", team_id)

  # Add season parameter if specified
  if (!is.null(season)) {
    url <- httr::modify_url(base_url, query = list(season = season))
  } else {
    url <- base_url
  }

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
      assign("college_baseball_schedule_raw", data, envir = .GlobalEnv)
      message("Raw college_baseball schedule data assigned to: college_baseball_schedule_raw")
      return(invisible(data))
    }

    # Create clean schedule dataset
    schedule_df <- create_clean_college_baseball_schedule_dataset(data, team_id, season)

    # Assign to global environment
    assign("college_baseball_team_schedule", schedule_df, envir = .GlobalEnv)
    season_text <- if (!is.null(season)) season else "current"
    message(sprintf("college_baseball team schedule assigned to: college_baseball_team_schedule (Team ID: %s, %s season, %d games)",
                    team_id, season_text, nrow(schedule_df)))

    return(invisible(schedule_df))

  }, error = function(e) {
    stop(sprintf("Failed to fetch college_baseball schedule for team %s: %s", team_id, e$message))
  })
}

#' Create clean college_baseball team schedule dataset from ESPN Site API response
#'
#' @param data Raw JSON response from ESPN Site API schedule endpoint
#' @param team_id Team ID used in request
#' @param season Season used in request (can be NULL)
#' @return Clean data frame with schedule information
create_clean_college_baseball_schedule_dataset <- function(data, team_id, season) {
  # Initialize result data frame
  result_df <- data.frame(
    request_team_id = character(0),
    request_season = character(0),
    # Team info
    team_id = character(0),
    team_abbreviation = character(0),
    team_display_name = character(0),
    # Game info
    game_id = character(0),
    game_uid = character(0),
    game_date = character(0),
    game_name = character(0),
    game_short_name = character(0),
    # Season info
    season_type = integer(0),
    season_year = integer(0),
    # Game status
    status_type = character(0),
    status_detail = character(0),
    is_completed = logical(0),
    # Opponent info
    opponent_id = character(0),
    opponent_abbreviation = character(0),
    opponent_display_name = character(0),
    opponent_location = character(0),
    opponent_logo = character(0),
    # Game details
    home_away = character(0),
    venue_name = character(0),
    venue_city = character(0),
    venue_state = character(0),
    # Scores
    team_score = character(0),
    opponent_score = character(0),
    team_winner = logical(0),
    opponent_winner = logical(0),
    # Broadcast
    broadcast = character(0),
    stringsAsFactors = FALSE
  )

  # Extract team information
  team_info <- if ("team" %in% names(data)) data[["team"]] else list()
  team_id_api <- if ("id" %in% names(team_info)) team_info[["id"]] else team_id
  team_abbreviation <- if ("abbreviation" %in% names(team_info)) team_info[["abbreviation"]] else NA_character_
  team_display_name <- if ("displayName" %in% names(team_info)) team_info[["displayName"]] else NA_character_

  # Process events (games)
  events <- NULL
  if ("events" %in% names(data) && length(data[["events"]]) > 0) {
    events <- data[["events"]]
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
    game_short_name <- if ("shortName" %in% names(event)) event[["shortName"]] else NA_character_

    # Season info
    season_info <- if ("season" %in% names(event)) event[["season"]] else list()
    season_type <- if ("type" %in% names(season_info)) as.integer(season_info[["type"]]) else NA_integer_
    season_year <- if ("year" %in% names(season_info)) as.integer(season_info[["year"]]) else
      if (!is.null(season)) as.integer(season) else NA_integer_

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
      broadcast_list <- if ("broadcasts" %in% names(competition)) competition[["broadcasts"]] else list()
      broadcast <- if (length(broadcast_list) > 0 && "names" %in% names(broadcast_list[[1]])) {
        paste(broadcast_list[[1]][["names"]], collapse = ", ")
      } else NA_character_

      # Initialize opponent and game variables
      opponent_id <- opponent_abbreviation <- opponent_display_name <- opponent_location <- opponent_logo <- NA_character_
      home_away <- NA_character_
      team_score <- opponent_score <- NA_character_
      team_winner <- opponent_winner <- FALSE

      # Process competitors (teams)
      if ("competitors" %in% names(competition) && length(competition[["competitors"]]) > 0) {
        for (j in seq_along(competition[["competitors"]])) {
          competitor <- competition[["competitors"]][[j]]

          # Team info
          team_info_comp <- if ("team" %in% names(competitor)) competitor[["team"]] else list()
          comp_team_id <- if ("id" %in% names(team_info_comp)) team_info_comp[["id"]] else NA_character_

          # Check if this is our team or the opponent
          if (comp_team_id == team_id_api) {
            # This is our team
            home_away <- if ("homeAway" %in% names(competitor)) competitor[["homeAway"]] else NA_character_
            team_score <- if ("score" %in% names(competitor)) competitor[["score"]] else NA_character_
            team_winner <- if ("winner" %in% names(competitor)) competitor[["winner"]] else FALSE
          } else {
            # This is the opponent
            opponent_id <- comp_team_id
            opponent_abbreviation <- if ("abbreviation" %in% names(team_info_comp)) team_info_comp[["abbreviation"]] else NA_character_
            opponent_display_name <- if ("displayName" %in% names(team_info_comp)) team_info_comp[["displayName"]] else NA_character_
            opponent_location <- if ("location" %in% names(team_info_comp)) team_info_comp[["location"]] else NA_character_

            # Opponent logo
            if ("logo" %in% names(team_info_comp)) {
              opponent_logo <- team_info_comp[["logo"]]
            } else if ("logos" %in% names(team_info_comp) && length(team_info_comp[["logos"]]) > 0) {
              opponent_logo <- if ("href" %in% names(team_info_comp[["logos"]][[1]]))
                team_info_comp[["logos"]][[1]][["href"]] else NA_character_
            }

            # Opponent score and result
            opponent_score <- if ("score" %in% names(competitor)) competitor[["score"]] else NA_character_
            opponent_winner <- if ("winner" %in% names(competitor)) competitor[["winner"]] else FALSE
          }
        }
      }

      # Create game row
      game_row <- data.frame(
        request_team_id = team_id,
        request_season = if (!is.null(season)) as.character(season) else "current",
        team_id = team_id_api,
        team_abbreviation = team_abbreviation,
        team_display_name = team_display_name,
        game_id = game_id,
        game_uid = game_uid,
        game_date = game_date,
        game_name = game_name,
        game_short_name = game_short_name,
        season_type = season_type,
        season_year = season_year,
        status_type = status_type,
        status_detail = status_detail,
        is_completed = is_completed,
        opponent_id = opponent_id,
        opponent_abbreviation = opponent_abbreviation,
        opponent_display_name = opponent_display_name,
        opponent_location = opponent_location,
        opponent_logo = opponent_logo,
        home_away = home_away,
        venue_name = venue_name,
        venue_city = venue_city,
        venue_state = venue_state,
        team_score = team_score,
        opponent_score = opponent_score,
        team_winner = team_winner,
        opponent_winner = opponent_winner,
        broadcast = broadcast,
        stringsAsFactors = FALSE
      )

      # Add to result
      result_df <- rbind(result_df, game_row)
    }
  }

  # Clean up row names and sort by date
  if (nrow(result_df) > 0) {
    rownames(result_df) <- NULL
    # Sort by season type, then game date
    result_df$game_date_parsed <- as.Date(substr(result_df$game_date, 1, 10))
    result_df <- result_df[order(result_df$season_type, result_df$game_date_parsed, na.last = TRUE), ]
    result_df$game_date_parsed <- NULL
    rownames(result_df) <- NULL
  }

  return(result_df)
}
