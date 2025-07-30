#' Safe nested data extraction helper function for NFL night games
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_night <- function(data, path, default = NA) {
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

#' Extract team information from competitor data
#'
#' Processes team/competitor information from game data
#' @param competitor List containing competitor information
#' @param prefix Character prefix for column names (e.g., "home_", "away_")
#' @return Named list with team information
#' @keywords internal
extract_team_info <- function(competitor, prefix = "") {
  if (is.null(competitor) || length(competitor) == 0) {
    return(setNames(rep(NA_character_, 11), paste0(prefix, c(
      "team_id", "team_uid", "team_abbreviation", "team_display_name",
      "team_short_display_name", "team_name", "team_location", "team_color",
      "team_alternate_color", "team_logo", "team_record"
    ))))
  }

  team <- extract_nested_night(competitor, c("team"), list())

  team_id <- extract_nested_night(team, c("id"), NA_character_)
  team_uid <- extract_nested_night(team, c("uid"), NA_character_)
  team_abbreviation <- extract_nested_night(team, c("abbreviation"), NA_character_)
  team_display_name <- extract_nested_night(team, c("displayName"), NA_character_)
  team_short_display_name <- extract_nested_night(team, c("shortDisplayName"), NA_character_)
  team_name <- extract_nested_night(team, c("name"), NA_character_)
  team_location <- extract_nested_night(team, c("location"), NA_character_)
  team_color <- extract_nested_night(team, c("color"), NA_character_)
  team_alternate_color <- extract_nested_night(team, c("alternateColor"), NA_character_)

  # Logo
  team_logo <- NA_character_
  logos <- extract_nested_night(team, c("logos"), list())
  if (length(logos) > 0) {
    team_logo <- extract_nested_night(logos[[1]], c("href"), NA_character_)
  }

  # Record
  team_record <- NA_character_
  records <- extract_nested_night(competitor, c("records"), list())
  if (length(records) > 0) {
    # Find overall record
    for (record in records) {
      record_type <- extract_nested_night(record, c("type"), NA_character_)
      if (!is.na(record_type) && record_type == "total") {
        summary <- extract_nested_night(record, c("summary"), NA_character_)
        if (!is.na(summary)) {
          team_record <- summary
          break
        }
      }
    }
    # If no total record found, use first available
    if (is.na(team_record) && length(records) > 0) {
      team_record <- extract_nested_night(records[[1]], c("summary"), NA_character_)
    }
  }

  return(setNames(list(
    team_id, team_uid, team_abbreviation, team_display_name,
    team_short_display_name, team_name, team_location, team_color,
    team_alternate_color, team_logo, team_record
  ), paste0(prefix, c(
    "team_id", "team_uid", "team_abbreviation", "team_display_name",
    "team_short_display_name", "team_name", "team_location", "team_color",
    "team_alternate_color", "team_logo", "team_record"
  ))))
}

#' Create NFL night games data frame from Site API response
#'
#' Processes raw JSON response from ESPN Site API into structured data frame
#' containing night game information
#'
#' @param data Raw JSON response from ESPN Site API night games endpoint
#' @param night Character. Night type used in request (monday, thursday, sunday)
#' @return Data frame with night games information
#' @keywords internal
create_nfl_night_games_dataset <- function(data, night) {

  # Initialize night games data frame
  night_games_df <- data.frame(
    night_type = character(0),
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
    venue_id = character(0),
    venue_name = character(0),
    venue_full_name = character(0),
    venue_address_city = character(0),
    venue_address_state = character(0),
    venue_capacity = character(0),
    venue_indoor = character(0),
    broadcast_network = character(0),
    broadcast_lang = character(0),
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
    away_team_score = character(0),
    away_team_winner = character(0),
    stringsAsFactors = FALSE
  )

  # Extract events (games)
  events <- extract_nested_night(data, c("events"), list())

  for (i in seq_along(events)) {
    event <- events[[i]]

    # Basic game information
    game_id <- extract_nested_night(event, c("id"), NA_character_)
    game_uid <- extract_nested_night(event, c("uid"), NA_character_)
    game_date <- extract_nested_night(event, c("date"), NA_character_)
    game_name <- extract_nested_night(event, c("name"), NA_character_)
    game_short_name <- extract_nested_night(event, c("shortName"), NA_character_)

    # Season information
    season_info <- extract_nested_night(event, c("season"), list())
    season_year <- extract_nested_night(season_info, c("year"), NA_character_)
    season_type <- extract_nested_night(season_info, c("type"), NA_character_)

    # Week information
    week_info <- extract_nested_night(event, c("week"), list())
    week_number <- extract_nested_night(week_info, c("number"), NA_character_)

    # Status information
    status_info <- extract_nested_night(event, c("status"), list())
    game_status <- extract_nested_night(status_info, c("type", "name"), NA_character_)
    game_status_detail <- extract_nested_night(status_info, c("type", "detail"), NA_character_)
    game_clock <- extract_nested_night(status_info, c("displayClock"), NA_character_)
    game_period <- extract_nested_night(status_info, c("period"), NA_character_)

    # Venue information
    venue_id <- NA_character_
    venue_name <- NA_character_
    venue_full_name <- NA_character_
    venue_address_city <- NA_character_
    venue_address_state <- NA_character_
    venue_capacity <- NA_character_
    venue_indoor <- NA_character_

    competitions <- extract_nested_night(event, c("competitions"), list())
    if (length(competitions) > 0) {
      competition <- competitions[[1]]
      venue_info <- extract_nested_night(competition, c("venue"), list())

      if (length(venue_info) > 0) {
        venue_id <- extract_nested_night(venue_info, c("id"), NA_character_)
        venue_name <- extract_nested_night(venue_info, c("shortName"), NA_character_)
        venue_full_name <- extract_nested_night(venue_info, c("fullName"), NA_character_)
        venue_address_city <- extract_nested_night(venue_info, c("address", "city"), NA_character_)
        venue_address_state <- extract_nested_night(venue_info, c("address", "state"), NA_character_)
        venue_capacity <- extract_nested_night(venue_info, c("capacity"), NA_character_)
        venue_indoor <- extract_nested_night(venue_info, c("indoor"), "false")
      }

      # Broadcast information
      broadcast_network <- NA_character_
      broadcast_lang <- NA_character_
      broadcasts <- extract_nested_night(competition, c("broadcasts"), list())
      if (length(broadcasts) > 0) {
        # Take the first broadcast
        first_broadcast <- broadcasts[[1]]
        broadcast_network <- extract_nested_night(first_broadcast, c("names", 1), NA_character_)
        broadcast_lang <- extract_nested_night(first_broadcast, c("lang"), NA_character_)
      }

      # Team information
      competitors <- extract_nested_night(competition, c("competitors"), list())

      home_team_info <- list()
      away_team_info <- list()
      home_team_score <- NA_character_
      home_team_winner <- "false"
      away_team_score <- NA_character_
      away_team_winner <- "false"

      if (length(competitors) >= 2) {
        for (comp in competitors) {
          home_away <- extract_nested_night(comp, c("homeAway"), NA_character_)

          if (!is.na(home_away) && home_away == "home") {
            home_team_info <- extract_team_info(comp, "home_")
            home_team_score <- extract_nested_night(comp, c("score"), NA_character_)
            home_team_winner <- extract_nested_night(comp, c("winner"), "false")
          } else if (!is.na(home_away) && home_away == "away") {
            away_team_info <- extract_team_info(comp, "away_")
            away_team_score <- extract_nested_night(comp, c("score"), NA_character_)
            away_team_winner <- extract_nested_night(comp, c("winner"), "false")
          }
        }
      }

      # If we don't have home/away designation, assign first two competitors
      if (length(home_team_info) == 0 && length(competitors) >= 1) {
        home_team_info <- extract_team_info(competitors[[1]], "home_")
        home_team_score <- extract_nested_night(competitors[[1]], c("score"), NA_character_)
        home_team_winner <- extract_nested_night(competitors[[1]], c("winner"), "false")
      }

      if (length(away_team_info) == 0 && length(competitors) >= 2) {
        away_team_info <- extract_team_info(competitors[[2]], "away_")
        away_team_score <- extract_nested_night(competitors[[2]], c("score"), NA_character_)
        away_team_winner <- extract_nested_night(competitors[[2]], c("winner"), "false")
      }
    }

    # Ensure team info lists have all required fields
    if (length(home_team_info) == 0) {
      home_team_info <- setNames(rep(NA_character_, 11), paste0("home_", c(
        "team_id", "team_uid", "team_abbreviation", "team_display_name",
        "team_short_display_name", "team_name", "team_location", "team_color",
        "team_alternate_color", "team_logo", "team_record"
      )))
    }

    if (length(away_team_info) == 0) {
      away_team_info <- setNames(rep(NA_character_, 11), paste0("away_", c(
        "team_id", "team_uid", "team_abbreviation", "team_display_name",
        "team_short_display_name", "team_name", "team_location", "team_color",
        "team_alternate_color", "team_logo", "team_record"
      )))
    }

    # Create row
    game_row <- data.frame(
      night_type = as.character(night),
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
      venue_id = as.character(venue_id),
      venue_name = as.character(venue_name),
      venue_full_name = as.character(venue_full_name),
      venue_address_city = as.character(venue_address_city),
      venue_address_state = as.character(venue_address_state),
      venue_capacity = as.character(venue_capacity),
      venue_indoor = as.character(venue_indoor),
      broadcast_network = as.character(broadcast_network),
      broadcast_lang = as.character(broadcast_lang),
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
      away_team_score = as.character(away_team_score),
      away_team_winner = as.character(away_team_winner),
      stringsAsFactors = FALSE
    )

    night_games_df <- rbind(night_games_df, game_row)
  }

  # Clean up row names
  if (nrow(night_games_df) > 0) rownames(night_games_df) <- NULL

  return(night_games_df)
}

#' Fetch NFL night games using Site API
#'
#' Retrieves games for specific NFL night broadcasts from ESPN's Site API.
#' The function fetches comprehensive game information for Monday Night Football,
#' Thursday Night Football, or Sunday Night Football.
#'
#' @param night Character. Night type to fetch games for.
#'   Must be one of: "monday", "thursday", "sunday" (default: "monday").
#' @param year Integer. Season year to fetch (default: current year).
#'   The function will fetch games from the specified season.
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'nfl_night_games_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data frame. The main purpose is global
#'   environment assignment of \code{nfl_night_games} containing:
#'   \itemize{
#'     \item Game information: ID, date, name, status, scores
#'     \item Season details: year, type, week number
#'     \item Venue information: name, location, capacity, indoor/outdoor
#'     \item Broadcast details: network, language
#'     \item Team details: names, abbreviations, colors, logos, records
#'     \item Game results: scores, winners
#'   }
#'
#' @details
#' The function creates a structured data frame with comprehensive night game information.
#' Each row represents a game with detailed information about teams, venue, and broadcast.
#'
#' **Game Information**:
#' \itemize{
#'   \item Basic details: game ID, date, name, status
#'   \item Timing: season year, week number, game clock, period
#'   \item Results: scores, winners, game status
#' }
#'
#' **Venue Details**:
#' \itemize{
#'   \item Location: venue name, city, state
#'   \item Specifications: capacity, indoor/outdoor designation
#' }
#'
#' **Team Information**:
#' \itemize{
#'   \item Identity: team names, abbreviations, locations
#'   \item Branding: team colors, logos
#'   \item Performance: records, scores, winner status
#' }
#'
#' **Night Types**:
#' \itemize{
#'   \item "monday" - Monday Night Football (typically ESPN)
#'   \item "thursday" - Thursday Night Football (typically Prime Video/NFL Network)
#'   \item "sunday" - Sunday Night Football (typically NBC)
#' }
#'
#' @examples
#' \dontrun{
#' # Get Monday Night Football games
#' fetch_nfl_night_games("monday")
#'
#' # Get Thursday Night Football games
#' fetch_nfl_night_games("thursday")
#'
#' # Get Sunday Night Football games
#' fetch_nfl_night_games("sunday")
#'
#' # Get games from specific year
#' fetch_nfl_night_games("monday", year = 2023)
#'
#' # Check the data
#' head(nfl_night_games)
#'
#' # View recent games
#' recent_games <- nfl_night_games[1:5, c("game_date", "game_short_name",
#'                                        "home_team_abbreviation", "home_team_score",
#'                                        "away_team_abbreviation", "away_team_score")]
#' print(recent_games)
#'
#' # Games by venue
#' venue_games <- table(nfl_night_games$venue_name)
#' print("Games by venue:")
#' print(sort(venue_games, decreasing = TRUE))
#'
#' # Broadcast networks
#' network_games <- table(nfl_night_games$broadcast_network)
#' print("Games by network:")
#' print(network_games)
#'
#' # Team performance in night games
#' home_wins <- sum(nfl_night_games$home_team_winner == "true", na.rm = TRUE)
#' away_wins <- sum(nfl_night_games$away_team_winner == "true", na.rm = TRUE)
#' total_games <- nrow(nfl_night_games[!is.na(nfl_night_games$home_team_winner), ])
#'
#' cat(sprintf("Home team wins: %d/%d (%.1f%%)\n",
#'             home_wins, total_games, (home_wins/total_games)*100))
#' cat(sprintf("Away team wins: %d/%d (%.1f%%)\n",
#'             away_wins, total_games, (away_wins/total_games)*100))
#' }
#'
#' @export
fetch_nfl_night_games <- function(night = "monday", year = as.integer(format(Sys.Date(), "%Y")), raw = FALSE) {

  # Input validation
  valid_nights <- c("monday", "thursday", "sunday")
  if (!tolower(night) %in% valid_nights) {
    stop(sprintf("'night' must be one of: %s", paste(valid_nights, collapse = ", ")))
  }

  night <- tolower(night)

  if (!is.numeric(year) || year < 1990 || year > 2030) {
    stop("'year' must be a valid year between 1990 and 2030")
  }

  # Build API URL
  # Note: The API endpoint might need season parameter or default to current season
  url <- sprintf("https://site.api.espn.com/apis/site/v2/%snightfootball", night)

  # Add year parameter if API supports it
  url <- sprintf("%s?season=%d", url, year)

  # Fetch and parse data
  tryCatch({
    message(sprintf("Fetching %s Night Football games for %d season...",
                    tools::toTitleCase(night), year))

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
      assign("nfl_night_games_raw", data, envir = .GlobalEnv)
      message("Raw NFL night games data assigned to: nfl_night_games_raw")
      message("Data structure preview:")

      # Show available sections
      sections <- names(data)
      message("- Available sections: ", paste(sections, collapse = ", "))

      # Show events count
      events <- extract_nested_night(data, c("events"), list())
      message("- Total games found: ", length(events))

      if (length(events) > 0) {
        first_event <- events[[1]]
        event_sections <- names(first_event)
        message("- First game sections: ", paste(event_sections, collapse = ", "))
      }

      return(invisible(data))
    }

    # Create night games dataset
    night_games_df <- create_nfl_night_games_dataset(data, night)

    # Assign to global environment
    assign("nfl_night_games", night_games_df, envir = .GlobalEnv)

    # Summary message
    total_games <- nrow(night_games_df)

    message(sprintf("NFL %s night games assigned to: nfl_night_games (%d games)",
                    tools::toTitleCase(night), total_games))

    if (total_games > 0) {
      # Count completed vs upcoming games
      completed_games <- sum(night_games_df$game_status %in% c("STATUS_FINAL", "Final"), na.rm = TRUE)
      upcoming_games <- sum(!night_games_df$game_status %in% c("STATUS_FINAL", "Final"), na.rm = TRUE)

      message(sprintf("  - Completed games: %d", completed_games))
      message(sprintf("  - Upcoming/In-progress games: %d", upcoming_games))

      # Show unique venues
      unique_venues <- length(unique(night_games_df$venue_name[!is.na(night_games_df$venue_name)]))
      message(sprintf("  - Unique venues: %d", unique_venues))

      # Show broadcast networks
      unique_networks <- unique(night_games_df$broadcast_network[!is.na(night_games_df$broadcast_network)])
      if (length(unique_networks) > 0) {
        message(sprintf("  - Broadcast networks: %s", paste(unique_networks, collapse = ", ")))
      }

      # Show recent games
      if (total_games > 0) {
        recent_games <- head(night_games_df, 3)
        message("\nRecent games:")
        for (i in 1:nrow(recent_games)) {
          game <- recent_games[i, ]
          message(sprintf("  %s: %s vs %s",
                          substr(game$game_date, 1, 10),
                          game$away_team_abbreviation,
                          game$home_team_abbreviation))
        }
      }
    }

    return(invisible(night_games_df))

  }, error = function(e) {
    stop(sprintf("Failed to fetch NFL %s night games: %s", night, e$message))
  })
}

#' Fetch NFL night games for multiple nights
#'
#' Retrieves night games for multiple nights (Monday, Thursday, Sunday) with rate limiting.
#' This function calls \code{\link{fetch_nfl_night_games}} for each night
#' and combines the results.
#'
#' @param nights Character vector. Night types to fetch games for.
#'   Must be subset of: c("monday", "thursday", "sunday").
#'   Default: c("monday", "thursday", "sunday") (all nights).
#' @param year Integer. Season year to fetch (default: current year).
#' @param delay Numeric. Delay in seconds between API requests (default: 0.5).
#'   Used to be respectful to ESPN's servers.
#' @param raw Logical. If TRUE, assigns raw JSON to global environment
#'   for the first night only (default: FALSE).
#'
#' @return Invisibly returns the combined data frame. The main purpose is global
#'   environment assignment of combined \code{nfl_night_games} from all nights.
#'
#' @examples
#' \dontrun{
#' # Get all night games
#' fetch_multiple_nfl_night_games()
#'
#' # Get specific nights
#' fetch_multiple_nfl_night_games(c("monday", "thursday"))
#'
#' # Get games from specific year
#' fetch_multiple_nfl_night_games(year = 2023)
#'
#' # Check combined results
#' table(nfl_night_games$night_type)
#' }
#'
#' @seealso \code{\link{fetch_nfl_night_games}} for single night data
#' @export
fetch_multiple_nfl_night_games <- function(nights = c("monday", "thursday", "sunday"),
                                           year = as.integer(format(Sys.Date(), "%Y")),
                                           delay = 0.5, raw = FALSE) {
  # Input validation
  valid_nights <- c("monday", "thursday", "sunday")
  nights <- tolower(nights)

  if (!all(nights %in% valid_nights)) {
    invalid_nights <- nights[!nights %in% valid_nights]
    stop(sprintf("Invalid night(s): %s. Must be subset of: %s",
                 paste(invalid_nights, collapse = ", "),
                 paste(valid_nights, collapse = ", ")))
  }

  if (length(nights) == 0) {
    stop("'nights' must contain at least one night")
  }

  # Initialize combined data container
  all_games <- data.frame()

  message(sprintf("Starting to fetch NFL night games for %d nights (%s season)...",
                  length(nights), year))

  # Process each night sequentially
  for (i in seq_along(nights)) {
    night <- nights[i]
    message(sprintf("Fetching %s night games (%d/%d)...",
                    tools::toTitleCase(night), i, length(nights)))

    tryCatch({
      # Fetch individual night data
      night_data <- fetch_nfl_night_games(
        night = night,
        year = year,
        raw = raw
      )

      # If raw data requested, return after first night
      if (isTRUE(raw)) {
        return(invisible(night_data))
      }

      # Combine data
      games_df <- get("nfl_night_games", envir = .GlobalEnv)
      all_games <- rbind(all_games, games_df)

    }, error = function(e) {
      message(sprintf("Failed to fetch %s night games: %s", night, e$message))
    })

    # Be respectful to the API with delay between requests
    if (i < length(nights)) {
      Sys.sleep(delay)
    }
  }

  # Remove duplicates and assign combined dataset to global environment
  if (nrow(all_games) > 0) {
    all_games <- all_games[!duplicated(all_games$game_id), ]
    assign("nfl_night_games", all_games, envir = .GlobalEnv)

    unique_nights <- length(unique(all_games$night_type))
    total_games <- nrow(all_games)

    message(sprintf("Combined NFL night games assigned to: nfl_night_games (%d nights, %d games)",
                    unique_nights, total_games))

    # Show night breakdown
    night_counts <- table(all_games$night_type)
    message("Games per night:")
    for (night_type in names(night_counts)) {
      message(sprintf("  %s: %d games", tools::toTitleCase(night_type), night_counts[night_type]))
    }

    # Show season breakdown
    season_counts <- table(all_games$season_year)
    if (length(season_counts) > 1) {
      message("Games per season:")
      for (season in names(season_counts)) {
        message(sprintf("  %s: %d games", season, season_counts[season]))
      }
    }
  }

  return(invisible(all_games))
}
