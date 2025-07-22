#' Safe nested data extraction helper function for game plays
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_plays <- function(data, path, default = NA) {
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

#' Create game plays data frames from Core API response
#'
#' Processes raw JSON response from ESPN Core API into structured data frames
#' containing detailed play information
#'
#' @param data Raw JSON response from ESPN Core API plays endpoint
#' @param event_id Character. Event ID used in request
#' @return List containing multiple data frames with play information
#' @keywords internal
create_gameplays_datasets <- function(data, event_id) {

  # Initialize plays data frame
  plays_df <- data.frame(
    event_id = character(0),
    play_id = character(0),
    sequence_number = character(0),
    type_id = character(0),
    type_text = character(0),
    type_abbreviation = character(0),
    period_number = character(0),
    clock_value = character(0),
    clock_display_value = character(0),
    scoring_play = character(0),
    score_value = character(0),
    away_score = character(0),
    home_score = character(0),
    play_text = character(0),
    short_text = character(0),
    start_down = character(0),
    start_distance = character(0),
    start_yard_line = character(0),
    start_yards_to_endzone = character(0),
    end_down = character(0),
    end_distance = character(0),
    end_yard_line = character(0),
    end_yards_to_endzone = character(0),
    stat_yardage = character(0),
    yards_after_catch = character(0),
    modified_date = character(0),
    wallclock = character(0),
    stringsAsFactors = FALSE
  )

  # Initialize participants data frame
  participants_df <- data.frame(
    event_id = character(0),
    play_id = character(0),
    athlete_ref = character(0),
    participant_order = character(0),
    participant_type = character(0),
    stringsAsFactors = FALSE
  )

  # Initialize summary data frame
  summary_df <- data.frame(
    event_id = character(0),
    total_plays = character(0),
    total_pages = character(0),
    page_index = character(0),
    page_size = character(0),
    request_url = character(0),
    stringsAsFactors = FALSE
  )

  # Extract pagination info
  page_info <- extract_nested_plays(data, c("pageInfo"), list())
  page_index <- extract_nested_plays(page_info, c("pageIndex"), NA_character_)
  page_size <- extract_nested_plays(page_info, c("pageSize"), NA_character_)
  total_pages <- extract_nested_plays(page_info, c("totalPages"), NA_character_)

  # Extract items (plays)
  items <- extract_nested_plays(data, c("items"), list())
  total_plays <- length(items)

  # Add summary row
  summary_row <- data.frame(
    event_id = as.character(event_id),
    total_plays = as.character(total_plays),
    total_pages = as.character(total_pages),
    page_index = as.character(page_index),
    page_size = as.character(page_size),
    request_url = NA_character_,
    stringsAsFactors = FALSE
  )

  summary_df <- rbind(summary_df, summary_row)

  if (length(items) > 0) {
    for (i in seq_along(items)) {
      play_data <- items[[i]]

      # Extract basic play information
      play_id <- extract_nested_plays(play_data, c("id"), as.character(i))
      sequence_number <- extract_nested_plays(play_data, c("sequenceNumber"), as.character(i))

      # Play type information
      type_info <- extract_nested_plays(play_data, c("type"), list())
      type_id <- extract_nested_plays(type_info, c("id"), NA_character_)
      type_text <- extract_nested_plays(type_info, c("text"), NA_character_)
      type_abbreviation <- extract_nested_plays(type_info, c("abbreviation"), NA_character_)

      # Period information
      period_info <- extract_nested_plays(play_data, c("period"), list())
      period_number <- extract_nested_plays(period_info, c("number"), NA_character_)

      # Clock information
      clock_info <- extract_nested_plays(play_data, c("clock"), list())
      clock_value <- extract_nested_plays(clock_info, c("value"), NA_character_)
      clock_display_value <- extract_nested_plays(clock_info, c("displayValue"), NA_character_)

      # Scoring information
      scoring_play <- extract_nested_plays(play_data, c("scoringPlay"), "false")
      score_value <- extract_nested_plays(play_data, c("scoreValue"), "0")
      away_score <- extract_nested_plays(play_data, c("awayScore"), NA_character_)
      home_score <- extract_nested_plays(play_data, c("homeScore"), NA_character_)

      # Play text
      play_text <- extract_nested_plays(play_data, c("text"), NA_character_)
      short_text <- extract_nested_plays(play_data, c("shortText"), NA_character_)

      # Start situation
      start_info <- extract_nested_plays(play_data, c("start"), list())
      start_down <- extract_nested_plays(start_info, c("down"), NA_character_)
      start_distance <- extract_nested_plays(start_info, c("distance"), NA_character_)
      start_yard_line <- extract_nested_plays(start_info, c("yardLine"), NA_character_)
      start_yards_to_endzone <- extract_nested_plays(start_info, c("yardsToEndzone"), NA_character_)

      # End situation
      end_info <- extract_nested_plays(play_data, c("end"), list())
      end_down <- extract_nested_plays(end_info, c("down"), NA_character_)
      end_distance <- extract_nested_plays(end_info, c("distance"), NA_character_)
      end_yard_line <- extract_nested_plays(end_info, c("yardLine"), NA_character_)
      end_yards_to_endzone <- extract_nested_plays(end_info, c("yardsToEndzone"), NA_character_)

      # Statistics
      stat_yardage <- extract_nested_plays(play_data, c("statYardage"), NA_character_)
      yards_after_catch <- extract_nested_plays(play_data, c("yardsAfterCatch"), NA_character_)

      # Metadata
      modified_date <- extract_nested_plays(play_data, c("modified"), NA_character_)
      wallclock <- extract_nested_plays(play_data, c("wallclock"), NA_character_)

      # Add play row
      play_row <- data.frame(
        event_id = as.character(event_id),
        play_id = as.character(play_id),
        sequence_number = as.character(sequence_number),
        type_id = as.character(type_id),
        type_text = as.character(type_text),
        type_abbreviation = as.character(type_abbreviation),
        period_number = as.character(period_number),
        clock_value = as.character(clock_value),
        clock_display_value = as.character(clock_display_value),
        scoring_play = as.character(scoring_play),
        score_value = as.character(score_value),
        away_score = as.character(away_score),
        home_score = as.character(home_score),
        play_text = as.character(play_text),
        short_text = as.character(short_text),
        start_down = as.character(start_down),
        start_distance = as.character(start_distance),
        start_yard_line = as.character(start_yard_line),
        start_yards_to_endzone = as.character(start_yards_to_endzone),
        end_down = as.character(end_down),
        end_distance = as.character(end_distance),
        end_yard_line = as.character(end_yard_line),
        end_yards_to_endzone = as.character(end_yards_to_endzone),
        stat_yardage = as.character(stat_yardage),
        yards_after_catch = as.character(yards_after_catch),
        modified_date = as.character(modified_date),
        wallclock = as.character(wallclock),
        stringsAsFactors = FALSE
      )

      plays_df <- rbind(plays_df, play_row)

      # Extract participants
      participants <- extract_nested_plays(play_data, c("participants"), list())

      for (participant in participants) {
        athlete_ref <- extract_nested_plays(participant, c("athlete", "$ref"), NA_character_)
        participant_order <- extract_nested_plays(participant, c("order"), NA_character_)
        participant_type <- extract_nested_plays(participant, c("type"), NA_character_)

        # Add participant row
        participant_row <- data.frame(
          event_id = as.character(event_id),
          play_id = as.character(play_id),
          athlete_ref = as.character(athlete_ref),
          participant_order = as.character(participant_order),
          participant_type = as.character(participant_type),
          stringsAsFactors = FALSE
        )

        participants_df <- rbind(participants_df, participant_row)
      }
    }
  }

  # Clean up row names
  if (nrow(summary_df) > 0) rownames(summary_df) <- NULL
  if (nrow(plays_df) > 0) rownames(plays_df) <- NULL
  if (nrow(participants_df) > 0) rownames(participants_df) <- NULL

  return(list(
    summary = summary_df,
    plays = plays_df,
    participants = participants_df
  ))
}

#' Fetch NFL game plays data using Core API
#'
#' Retrieves detailed play information from ESPN's Core API.
#' The function fetches comprehensive play data with participant details
#' for a specific NFL game.
#'
#' @param event_id Character or Numeric. ESPN event ID (required).
#'   The unique identifier for the game in ESPN's database.
#' @param limit Numeric. Maximum number of plays to retrieve (default: 300).
#'   ESPN Core API supports pagination with this parameter.
#' @param return_type Character. Type of data to return (default: "all").
#'   Options are:
#'   \itemize{
#'     \item "summary" - Basic summary information
#'     \item "plays" - Individual play data
#'     \item "participants" - Player participation data
#'     \item "all" - All data types combined
#'   }
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'nfl_plays_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data. The main purpose is global
#'   environment assignment based on return_type:
#'   \itemize{
#'     \item \code{nfl_plays_summary} - Summary data frame
#'     \item \code{nfl_plays_plays} - Plays data frame
#'     \item \code{nfl_plays_participants} - Participants data frame
#'     \item \code{nfl_plays_raw} - Raw JSON response (if raw = TRUE)
#'   }
#'
#' @details
#' The function creates structured data frames with comprehensive play information:
#'
#' **Summary** (\code{nfl_plays_summary}):
#' \itemize{
#'   \item Request metadata: total_plays, total_pages, page_index, page_size
#'   \item Event identification and pagination information
#' }
#'
#' **Plays** (\code{nfl_plays_plays}):
#' \itemize{
#'   \item Play identification: play_id, sequence_number, type information
#'   \item Game situation: period, clock, down, distance, yard_line
#'   \item Field position: start and end situations with yards_to_endzone
#'   \item Scoring: scoring_play flag, score_value, running scores
#'   \item Statistics: stat_yardage, yards_after_catch
#' }
#'
#' **Participants** (\code{nfl_plays_participants}):
#' \itemize{
#'   \item Player references: athlete_ref (API reference URLs)
#'   \item Participation details: order, type (rusher, passer, receiver, etc.)
#'   \item Play context linking
#' }
#'
#' This provides detailed individual play information from ESPN's Core API,
#' including specific player participation data with API references for
#' further player detail retrieval.
#'
#' @examples
#' \dontrun{
#' # Get complete plays data for a specific game
#' fetch_nfl_gameplays(event_id = "401671617")
#'
#' # Check what data was created
#' head(nfl_plays_summary)
#' head(nfl_plays_plays)
#' head(nfl_plays_participants)
#'
#' # Get only plays data with higher limit
#' fetch_nfl_gameplays(event_id = "401671617", limit = 500, return_type = "plays")
#'
#' # Analyze scoring plays
#' scoring_plays <- nfl_plays_plays[
#'   nfl_plays_plays$scoring_play == "TRUE",
#'   c("sequence_number", "period_number", "clock_display_value",
#'     "score_value", "short_text")
#' ]
#' print(scoring_plays)
#'
#' # View play types
#' play_type_summary <- table(nfl_plays_plays$type_text)
#' print(play_type_summary)
#'
#' # Third down situations
#' third_downs <- nfl_plays_plays[
#'   nfl_plays_plays$start_down == "3",
#'   c("sequence_number", "start_distance", "type_text", "short_text")
#' ]
#' head(third_downs)
#'
#' # Participant analysis
#' participant_types <- table(nfl_plays_participants$participant_type)
#' print(participant_types)
#'
#' # Get raw data for debugging
#' fetch_nfl_gameplays(event_id = "401671617", raw = TRUE)
#' str(nfl_plays_raw, max.level = 2)
#' }
#'
#' @seealso \code{\link{fetch_multiple_nfl_gameplays}} for fetching
#'   multiple games' data
#'
#' @export
fetch_nfl_gameplays <- function(event_id, limit = 300, return_type = "all", raw = FALSE) {

  # Input validation
  if (missing(event_id)) {
    stop("'event_id' is a required parameter")
  }

  valid_types <- c("summary", "plays", "participants", "all")
  if (!return_type %in% valid_types) {
    stop(sprintf("'return_type' must be one of: %s", paste(valid_types, collapse = ", ")))
  }

  if (!is.numeric(limit) || limit <= 0) {
    stop("'limit' must be a positive numeric value")
  }

  # Convert event_id to character for URL building
  event_id <- as.character(event_id)

  # Build API URL for Core API
  url <- sprintf("https://sports.core.api.espn.com/v2/sports/football/leagues/nfl/events/%s/competitions/%s/plays?limit=%d",
                 event_id, event_id, limit)

  # Fetch and parse data
  tryCatch({
    resp <- httr::GET(url, httr::timeout(60))

    if (httr::status_code(resp) != 200) {
      stop(sprintf("API request failed: HTTP %d - %s",
                   httr::status_code(resp),
                   httr::http_status(resp)$message))
    }

    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content_text, simplifyVector = FALSE, simplifyDataFrame = FALSE)

    # Handle raw data assignment for debugging
    if (isTRUE(raw)) {
      assign("nfl_plays_raw", data, envir = .GlobalEnv)
      message("Raw NFL plays data assigned to: nfl_plays_raw")
      message("Data structure preview:")

      # Show available sections
      sections <- names(data)
      message("- Available sections: ", paste(sections, collapse = ", "))

      # Show pagination info
      page_info <- extract_nested_plays(data, c("pageInfo"), list())
      if (length(page_info) > 0) {
        page_sections <- names(page_info)
        message("- Page info sections: ", paste(page_sections, collapse = ", "))

        total_pages <- extract_nested_plays(page_info, c("totalPages"), "unknown")
        page_size <- extract_nested_plays(page_info, c("pageSize"), "unknown")
        message("- Total pages: ", total_pages, ", Page size: ", page_size)
      }

      # Show items info
      items <- extract_nested_plays(data, c("items"), list())
      message("- Total plays retrieved: ", length(items))

      return(invisible(data))
    }

    # Create datasets
    datasets <- create_gameplays_datasets(data, event_id)

    # Update summary with request URL
    datasets$summary$request_url <- url

    # Assign data based on return_type
    result_data <- list()

    if (return_type %in% c("summary", "all")) {
      assign("nfl_plays_summary", datasets$summary, envir = .GlobalEnv)
      result_data$summary <- datasets$summary
      message(sprintf("Game plays summary assigned to: nfl_plays_summary (%d records)", nrow(datasets$summary)))
    }

    if (return_type %in% c("plays", "all")) {
      assign("nfl_plays_plays", datasets$plays, envir = .GlobalEnv)
      result_data$plays <- datasets$plays
      message(sprintf("Game plays assigned to: nfl_plays_plays (%d plays)", nrow(datasets$plays)))
    }

    if (return_type %in% c("participants", "all")) {
      assign("nfl_plays_participants", datasets$participants, envir = .GlobalEnv)
      result_data$participants <- datasets$participants
      message(sprintf("Game plays participants assigned to: nfl_plays_participants (%d participations)", nrow(datasets$participants)))
    }

    # Return appropriate data structure
    if (return_type == "all") {
      return(invisible(result_data))
    } else {
      return(invisible(result_data[[return_type]]))
    }

  }, error = function(e) {
    stop(sprintf("Failed to fetch NFL game plays for event %s: %s",
                 event_id, e$message))
  })
}

#' Fetch multiple NFL game plays data
#'
#' Retrieves plays data for multiple NFL games with rate limiting to
#' be respectful to ESPN's Core API. This function calls \code{\link{fetch_nfl_gameplays}}
#' for each game and combines the results.
#'
#' @param event_ids Character or Numeric vector. ESPN event IDs.
#'   Vector of unique identifiers for games in ESPN's database.
#' @param limit Numeric. Maximum number of plays per game to retrieve (default: 300).
#' @param return_type Character. Type of data to return (default: "all").
#'   Same options as \code{\link{fetch_nfl_gameplays}}.
#' @param delay Numeric. Delay in seconds between API requests (default: 0.2).
#'   Used to be respectful to ESPN's servers and avoid rate limiting.
#' @param raw Logical. If TRUE, assigns raw JSON to global environment
#'   for the first game only (default: FALSE).
#'
#' @return Invisibly returns the combined data frames. The main purpose is global
#'   environment assignment of combined datasets from all games.
#'
#' @details
#' The function processes games sequentially with a configurable delay
#' between requests. Failed requests for individual games are logged but
#' do not stop the overall process. The final datasets contain data from
#' all successfully processed games.
#'
#' This is particularly useful for building comprehensive play databases
#' with detailed participant information across multiple games, enabling
#' advanced player performance tracking and team analysis.
#'
#' @examples
#' \dontrun{
#' # Get plays for multiple games from same week
#' week1_games <- c("401671617", "401671618", "401671619")
#' fetch_multiple_nfl_gameplays(week1_games)
#'
#' # Get only participant data for multiple games
#' fetch_multiple_nfl_gameplays(week1_games, return_type = "participants")
#'
#' # Use higher limit and longer delay for larger requests
#' fetch_multiple_nfl_gameplays(week1_games, limit = 500, delay = 0.5)
#'
#' # Analyze combined results
#' unique_games <- unique(nfl_plays_summary$event_id)
#' cat("Retrieved plays for", length(unique_games), "games\n")
#'
#' # Play type analysis across multiple games
#' play_types <- table(nfl_plays_plays$type_text)
#' print(play_types)
#' }
#'
#' @seealso \code{\link{fetch_nfl_gameplays}} for single game data
#' @export
fetch_multiple_nfl_gameplays <- function(event_ids, limit = 300, return_type = "all",
                                         delay = 0.2, raw = FALSE) {
  # Input validation
  if (length(event_ids) == 0) {
    stop("'event_ids' must contain at least one event ID")
  }

  valid_types <- c("summary", "plays", "participants", "all")
  if (!return_type %in% valid_types) {
    stop(sprintf("'return_type' must be one of: %s", paste(valid_types, collapse = ", ")))
  }

  if (!is.numeric(limit) || limit <= 0) {
    stop("'limit' must be a positive numeric value")
  }

  # Initialize combined data containers
  all_summary <- data.frame()
  all_plays <- data.frame()
  all_participants <- data.frame()

  message(sprintf("Starting to fetch game plays data for %d games...", length(event_ids)))

  # Process each game sequentially
  for (i in seq_along(event_ids)) {
    event_id <- event_ids[i]
    message(sprintf("Fetching game plays for event %s (%d/%d)...", event_id, i, length(event_ids)))

    tryCatch({
      # Fetch individual game data
      game_data <- fetch_nfl_gameplays(
        event_id = event_id,
        limit = limit,
        return_type = return_type,
        raw = raw
      )

      # If raw data requested, return after first game
      if (isTRUE(raw)) {
        return(invisible(game_data))
      }

      # Combine data based on return type
      if (return_type %in% c("summary", "all")) {
        summary_df <- get("nfl_plays_summary", envir = .GlobalEnv)
        all_summary <- rbind(all_summary, summary_df)
      }

      if (return_type %in% c("plays", "all")) {
        plays_df <- get("nfl_plays_plays", envir = .GlobalEnv)
        all_plays <- rbind(all_plays, plays_df)
      }

      if (return_type %in% c("participants", "all")) {
        participants_df <- get("nfl_plays_participants", envir = .GlobalEnv)
        all_participants <- rbind(all_participants, participants_df)
      }

    }, error = function(e) {
      message(sprintf("Failed to fetch game plays for event %s: %s", event_id, e$message))
    })

    # Be respectful to the API with delay between requests
    if (i < length(event_ids)) {
      Sys.sleep(delay)
    }
  }

  # Remove duplicates and assign combined datasets to global environment
  result_data <- list()

  if (return_type %in% c("summary", "all") && nrow(all_summary) > 0) {
    all_summary <- all_summary[!duplicated(all_summary$event_id), ]
    assign("nfl_plays_summary", all_summary, envir = .GlobalEnv)
    result_data$summary <- all_summary

    message(sprintf("Combined game plays summary assigned to: nfl_plays_summary (%d games)", nrow(all_summary)))
  }

  if (return_type %in% c("plays", "all") && nrow(all_plays) > 0) {
    all_plays <- all_plays[!duplicated(paste(all_plays$event_id, all_plays$play_id)), ]
    assign("nfl_plays_plays", all_plays, envir = .GlobalEnv)
    result_data$plays <- all_plays

    unique_games <- length(unique(all_plays$event_id))
    total_plays <- nrow(all_plays)
    message(sprintf("Combined game plays assigned to: nfl_plays_plays (%d games, %d plays)",
                    unique_games, total_plays))
  }

  if (return_type %in% c("participants", "all") && nrow(all_participants) > 0) {
    all_participants <- all_participants[!duplicated(paste(all_participants$event_id,
                                                           all_participants$play_id,
                                                           all_participants$athlete_ref)), ]
    assign("nfl_plays_participants", all_participants, envir = .GlobalEnv)
    result_data$participants <- all_participants

    unique_games <- length(unique(all_participants$event_id))
    total_participations <- nrow(all_participants)
    message(sprintf("Combined game plays participants assigned to: nfl_plays_participants (%d games, %d participations)",
                    unique_games, total_participations))
  }

  # Return appropriate data structure
  if (return_type == "all") {
    return(invisible(result_data))
  } else {
    return(invisible(result_data[[return_type]]))
  }
}
