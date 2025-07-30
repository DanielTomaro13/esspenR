#' Safe nested data extraction helper function for college football play-by-play
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_pbp <- function(data, path, default = NA) {
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

#' Create college football play-by-play data frame from Site API response
#'
#' Processes raw JSON response from ESPN Site API into structured data frame
#' containing detailed play-by-play information from drives
#'
#' @param data Raw JSON response from ESPN Site API game summary endpoint
#' @param game_id Character. Game ID used in request
#' @return Data frame with play-by-play information
#' @keywords internal
create_cfb_playbyplay_dataset <- function(data, game_id) {

  # Initialize play-by-play data frame
  playbyplay_df <- data.frame(
    query_game_id = character(0),
    game_id = character(0),
    drive_id = character(0),
    drive_number = character(0),
    drive_description = character(0),
    drive_team_id = character(0),
    drive_team_abbreviation = character(0),
    drive_team_display_name = character(0),
    drive_start_period = character(0),
    drive_start_clock = character(0),
    drive_start_yard_line = character(0),
    drive_start_yards_to_goal = character(0),
    drive_end_period = character(0),
    drive_end_clock = character(0),
    drive_end_yard_line = character(0),
    drive_end_yards_to_goal = character(0),
    drive_time_elapsed = character(0),
    drive_yards = character(0),
    drive_offensive_plays = character(0),
    drive_is_score = character(0),
    drive_result = character(0),
    drive_short_result = character(0),
    drive_display_result = character(0),
    play_id = character(0),
    play_sequence_number = character(0),
    play_type_id = character(0),
    play_type_text = character(0),
    play_type_abbreviation = character(0),
    play_text = character(0),
    play_short_text = character(0),
    play_away_score = character(0),
    play_home_score = character(0),
    play_period = character(0),
    play_clock = character(0),
    play_yard_line = character(0),
    play_yards_to_goal = character(0),
    play_down = character(0),
    play_distance = character(0),
    play_yards_gained = character(0),
    play_scoring_play = character(0),
    play_scoring_type = character(0),
    play_team_id = character(0),
    play_team_abbreviation = character(0),
    stringsAsFactors = FALSE
  )

  # Extract drives data
  drives_data <- extract_nested_pbp(data, c("drives"), list())
  if (length(drives_data) == 0) {
    warning("No drives data found in API response")
    return(playbyplay_df)
  }

  # Extract previous drives
  drives_previous <- extract_nested_pbp(drives_data, c("previous"), list())
  if (length(drives_previous) == 0) {
    warning("No previous drives found in drives data")
    return(playbyplay_df)
  }

  drive_number <- 1

  for (drive in drives_previous) {
    # Drive information
    drive_id <- extract_nested_pbp(drive, c("id"), NA_character_)
    drive_description <- extract_nested_pbp(drive, c("description"), NA_character_)
    drive_yards <- extract_nested_pbp(drive, c("yards"), NA_character_)
    drive_offensive_plays <- extract_nested_pbp(drive, c("offensivePlays"), NA_character_)
    drive_is_score <- extract_nested_pbp(drive, c("isScore"), "false")
    drive_result <- extract_nested_pbp(drive, c("result"), NA_character_)
    drive_short_result <- extract_nested_pbp(drive, c("shortDisplayResult"), NA_character_)
    drive_display_result <- extract_nested_pbp(drive, c("displayResult"), NA_character_)

    # Drive team information
    drive_team <- extract_nested_pbp(drive, c("team"), list())
    drive_team_id <- extract_nested_pbp(drive_team, c("id"), NA_character_)
    drive_team_abbreviation <- extract_nested_pbp(drive_team, c("abbreviation"), NA_character_)
    drive_team_display_name <- extract_nested_pbp(drive_team, c("displayName"), NA_character_)

    # Drive start information
    drive_start <- extract_nested_pbp(drive, c("start"), list())
    drive_start_period <- extract_nested_pbp(drive_start, c("period", "number"), NA_character_)
    drive_start_clock <- extract_nested_pbp(drive_start, c("clock", "displayValue"), NA_character_)
    drive_start_yard_line <- extract_nested_pbp(drive_start, c("yardLine"), NA_character_)
    drive_start_yards_to_goal <- extract_nested_pbp(drive_start, c("yardsToGoal"), NA_character_)

    # Drive end information
    drive_end <- extract_nested_pbp(drive, c("end"), list())
    drive_end_period <- extract_nested_pbp(drive_end, c("period", "number"), NA_character_)
    drive_end_clock <- extract_nested_pbp(drive_end, c("clock", "displayValue"), NA_character_)
    drive_end_yard_line <- extract_nested_pbp(drive_end, c("yardLine"), NA_character_)
    drive_end_yards_to_goal <- extract_nested_pbp(drive_end, c("yardsToGoal"), NA_character_)

    # Drive time elapsed
    time_elapsed <- extract_nested_pbp(drive, c("timeElapsed"), list())
    drive_time_elapsed <- extract_nested_pbp(time_elapsed, c("displayValue"), NA_character_)

    # Extract plays in this drive
    plays <- extract_nested_pbp(drive, c("plays"), list())

    if (length(plays) > 0) {
      for (i in seq_along(plays)) {
        play <- plays[[i]]

        # Play basic information
        play_id <- extract_nested_pbp(play, c("id"), NA_character_)
        play_sequence_number <- extract_nested_pbp(play, c("sequenceNumber"), as.character(i))
        play_text <- extract_nested_pbp(play, c("text"), NA_character_)
        play_short_text <- extract_nested_pbp(play, c("shortText"), NA_character_)
        play_away_score <- extract_nested_pbp(play, c("awayScore"), NA_character_)
        play_home_score <- extract_nested_pbp(play, c("homeScore"), NA_character_)
        play_scoring_play <- extract_nested_pbp(play, c("scoringPlay"), "false")

        # Play type information
        play_type <- extract_nested_pbp(play, c("type"), list())
        play_type_id <- extract_nested_pbp(play_type, c("id"), NA_character_)
        play_type_text <- extract_nested_pbp(play_type, c("text"), NA_character_)
        play_type_abbreviation <- extract_nested_pbp(play_type, c("abbreviation"), NA_character_)

        # Play situation
        play_period <- extract_nested_pbp(play, c("period", "number"), NA_character_)
        play_clock <- extract_nested_pbp(play, c("clock", "displayValue"), NA_character_)
        play_yard_line <- extract_nested_pbp(play, c("start", "yardLine"), NA_character_)
        play_yards_to_goal <- extract_nested_pbp(play, c("start", "yardsToGoal"), NA_character_)
        play_down <- extract_nested_pbp(play, c("start", "down"), NA_character_)
        play_distance <- extract_nested_pbp(play, c("start", "distance"), NA_character_)

        # Play result
        play_yards_gained <- extract_nested_pbp(play, c("statYardage"), NA_character_)

        # Scoring information
        play_scoring_type <- NA_character_
        if (play_scoring_play == "true") {
          scoring_type <- extract_nested_pbp(play, c("scoringType"), list())
          play_scoring_type <- extract_nested_pbp(scoring_type, c("name"), NA_character_)
        }

        # Play team (who executed the play)
        play_team <- extract_nested_pbp(play, c("team"), list())
        play_team_id <- extract_nested_pbp(play_team, c("id"), NA_character_)
        play_team_abbreviation <- extract_nested_pbp(play_team, c("abbreviation"), NA_character_)

        # Create play row
        play_row <- data.frame(
          query_game_id = as.character(game_id),
          game_id = as.character(game_id),
          drive_id = as.character(drive_id),
          drive_number = as.character(drive_number),
          drive_description = as.character(drive_description),
          drive_team_id = as.character(drive_team_id),
          drive_team_abbreviation = as.character(drive_team_abbreviation),
          drive_team_display_name = as.character(drive_team_display_name),
          drive_start_period = as.character(drive_start_period),
          drive_start_clock = as.character(drive_start_clock),
          drive_start_yard_line = as.character(drive_start_yard_line),
          drive_start_yards_to_goal = as.character(drive_start_yards_to_goal),
          drive_end_period = as.character(drive_end_period),
          drive_end_clock = as.character(drive_end_clock),
          drive_end_yard_line = as.character(drive_end_yard_line),
          drive_end_yards_to_goal = as.character(drive_end_yards_to_goal),
          drive_time_elapsed = as.character(drive_time_elapsed),
          drive_yards = as.character(drive_yards),
          drive_offensive_plays = as.character(drive_offensive_plays),
          drive_is_score = as.character(drive_is_score),
          drive_result = as.character(drive_result),
          drive_short_result = as.character(drive_short_result),
          drive_display_result = as.character(drive_display_result),
          play_id = as.character(play_id),
          play_sequence_number = as.character(play_sequence_number),
          play_type_id = as.character(play_type_id),
          play_type_text = as.character(play_type_text),
          play_type_abbreviation = as.character(play_type_abbreviation),
          play_text = as.character(play_text),
          play_short_text = as.character(play_short_text),
          play_away_score = as.character(play_away_score),
          play_home_score = as.character(play_home_score),
          play_period = as.character(play_period),
          play_clock = as.character(play_clock),
          play_yard_line = as.character(play_yard_line),
          play_yards_to_goal = as.character(play_yards_to_goal),
          play_down = as.character(play_down),
          play_distance = as.character(play_distance),
          play_yards_gained = as.character(play_yards_gained),
          play_scoring_play = as.character(play_scoring_play),
          play_scoring_type = as.character(play_scoring_type),
          play_team_id = as.character(play_team_id),
          play_team_abbreviation = as.character(play_team_abbreviation),
          stringsAsFactors = FALSE
        )

        playbyplay_df <- rbind(playbyplay_df, play_row)
      }
    } else {
      # Create a drive-only row if no plays found
      drive_row <- data.frame(
        query_game_id = as.character(game_id),
        game_id = as.character(game_id),
        drive_id = as.character(drive_id),
        drive_number = as.character(drive_number),
        drive_description = as.character(drive_description),
        drive_team_id = as.character(drive_team_id),
        drive_team_abbreviation = as.character(drive_team_abbreviation),
        drive_team_display_name = as.character(drive_team_display_name),
        drive_start_period = as.character(drive_start_period),
        drive_start_clock = as.character(drive_start_clock),
        drive_start_yard_line = as.character(drive_start_yard_line),
        drive_start_yards_to_goal = as.character(drive_start_yards_to_goal),
        drive_end_period = as.character(drive_end_period),
        drive_end_clock = as.character(drive_end_clock),
        drive_end_yard_line = as.character(drive_end_yard_line),
        drive_end_yards_to_goal = as.character(drive_end_yards_to_goal),
        drive_time_elapsed = as.character(drive_time_elapsed),
        drive_yards = as.character(drive_yards),
        drive_offensive_plays = as.character(drive_offensive_plays),
        drive_is_score = as.character(drive_is_score),
        drive_result = as.character(drive_result),
        drive_short_result = as.character(drive_short_result),
        drive_display_result = as.character(drive_display_result),
        play_id = NA_character_,
        play_sequence_number = NA_character_,
        play_type_id = NA_character_,
        play_type_text = NA_character_,
        play_type_abbreviation = NA_character_,
        play_text = NA_character_,
        play_short_text = NA_character_,
        play_away_score = NA_character_,
        play_home_score = NA_character_,
        play_period = NA_character_,
        play_clock = NA_character_,
        play_yard_line = NA_character_,
        play_yards_to_goal = NA_character_,
        play_down = NA_character_,
        play_distance = NA_character_,
        play_yards_gained = NA_character_,
        play_scoring_play = NA_character_,
        play_scoring_type = NA_character_,
        play_team_id = NA_character_,
        play_team_abbreviation = NA_character_,
        stringsAsFactors = FALSE
      )

      playbyplay_df <- rbind(playbyplay_df, drive_row)
    }

    drive_number <- drive_number + 1
  }

  # Clean up row names
  if (nrow(playbyplay_df) > 0) rownames(playbyplay_df) <- NULL

  return(playbyplay_df)
}

#' Fetch college football play-by-play data using Site API
#'
#' Retrieves detailed play-by-play information for a specific college football game from ESPN's Site API.
#' The function fetches comprehensive drive and play data including down and distance,
#' field position, play descriptions, and scoring information.
#'
#' @param game_id Character. ESPN game ID for the specific game.
#'   Examples: "401677192" (CFP Championship), "400934572", etc.
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'cfb_playbyplay_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data frame. The main purpose is global
#'   environment assignment of \code{cfb_playbyplay} containing:
#'   \itemize{
#'     \item Drive information: ID, description, team, start/end position, time, result
#'     \item Play details: type, text description, down/distance, field position
#'     \item Game state: period, clock, score after each play
#'     \item Results: yards gained, scoring plays, drive outcomes
#'   }
#'
#' @details
#' The function creates a detailed data frame with play-by-play information for the entire game.
#' Each row represents either a play within a drive, with comprehensive information about
#' game situation, play execution, and results.
#'
#' **Drive Information**:
#' \itemize{
#'   \item Identity: drive ID, number, team executing
#'   \item Summary: description, total yards, number of plays, time elapsed
#'   \item Position: starting and ending field position and yards to goal
#'   \item Result: outcome (TD, FG, punt, turnover, etc.)
#' }
#'
#' **Play Details**:
#' \itemize{
#'   \item Situation: down, distance, field position, period, clock
#'   \item Execution: play type, detailed description, team
#'   \item Result: yards gained, new field position, scoring
#'   \item Score: game score after the play
#' }
#'
#' **Game Flow**:
#' \itemize{
#'   \item Sequential: plays are ordered chronologically
#'   \item Contextual: each play includes full game state
#'   \item Comprehensive: all drives and plays included
#' }
#'
#' @examples
#' \dontrun{
#' # Get play-by-play for CFP Championship
#' fetch_college_football_playbyplay("401677192")
#'
#' # Check the data
#' head(cfb_playbyplay)
#' nrow(cfb_playbyplay)
#'
#' # View drive summaries
#' drive_summary <- cfb_playbyplay[!is.na(cfb_playbyplay$play_id), ] %>%
#'   group_by(drive_number, drive_team_abbreviation) %>%
#'   summarise(
#'     drive_description = first(drive_description),
#'     drive_result = first(drive_result),
#'     plays = n(),
#'     .groups = 'drop'
#'   )
#'
#' print("Drive summaries:")
#' print(drive_summary)
#'
#' # View scoring plays
#' scoring_plays <- cfb_playbyplay[cfb_playbyplay$play_scoring_play == "true",
#'                                c("drive_number", "play_team_abbreviation",
#'                                  "play_text", "play_scoring_type",
#'                                  "play_home_score", "play_away_score")]
#'
#' if(nrow(scoring_plays) > 0) {
#'   print("Scoring plays:")
#'   print(scoring_plays)
#' }
#'
#' # Analyze third down conversions
#' third_downs <- cfb_playbyplay[cfb_playbyplay$play_down == "3" & !is.na(cfb_playbyplay$play_down), ]
#'
#' if(nrow(third_downs) > 0) {
#'   third_downs$yards_needed <- as.numeric(third_downs$play_distance)
#'   third_downs$yards_gained_num <- as.numeric(third_downs$play_yards_gained)
#'   third_downs$converted <- third_downs$yards_gained_num >= third_downs$yards_needed
#'
#'   conversion_rate <- mean(third_downs$converted, na.rm = TRUE)
#'   cat(sprintf("Third down conversion rate: %.1f%% (%d/%d)\n",
#'               conversion_rate * 100,
#'               sum(third_downs$converted, na.rm = TRUE),
#'               nrow(third_downs)))
#' }
#'
#' # Play type analysis
#' play_types <- table(cfb_playbyplay$play_type_text[!is.na(cfb_playbyplay$play_type_text)])
#' print("Most common play types:")
#' print(head(sort(play_types, decreasing = TRUE), 10))
#'
#' # Red zone analysis
#' red_zone_plays <- cfb_playbyplay[
#'   !is.na(cfb_playbyplay$play_yards_to_goal) &
#'   as.numeric(cfb_playbyplay$play_yards_to_goal) <= 20 &
#'   as.numeric(cfb_playbyplay$play_yards_to_goal) > 0, ]
#'
#' if(nrow(red_zone_plays) > 0) {
#'   red_zone_scores <- sum(red_zone_plays$play_scoring_play == "true", na.rm = TRUE)
#'   red_zone_drives <- length(unique(red_zone_plays$drive_id))
#'
#'   cat(sprintf("Red zone efficiency: %d scoring plays in %d red zone drives\n",
#'               red_zone_scores, red_zone_drives))
#' }
#'
#' # Game momentum - score progression
#' scoring_progression <- cfb_playbyplay[
#'   !is.na(cfb_playbyplay$play_home_score) & !is.na(cfb_playbyplay$play_away_score),
#'   c("play_sequence_number", "play_period", "play_clock",
#'     "play_home_score", "play_away_score", "play_scoring_play")]
#'
#' if(nrow(scoring_progression) > 0) {
#'   scoring_progression$score_diff <- as.numeric(scoring_progression$play_home_score) -
#'                                    as.numeric(scoring_progression$play_away_score)
#'
#'   print("Score progression (when scoring occurred):")
#'   print(head(scoring_progression[scoring_progression$play_scoring_play == "true", ], 10))
#' }
#'
#' # Time of possession by team
#' if(require(dplyr, quietly = TRUE)) {
#'   drive_times <- cfb_playbyplay %>%
#'     filter(!is.na(drive_time_elapsed) & drive_time_elapsed != "") %>%
#'     select(drive_team_abbreviation, drive_time_elapsed) %>%
#'     distinct()
#'
#'   # Convert time elapsed to seconds (simplified - assumes MM:SS format)
#'   if(nrow(drive_times) > 0) {
#'     print("Drive times by team:")
#'     print(head(drive_times, 10))
#'   }
#' }
#' }
#'
#' @export
fetch_college_football_playbyplay <- function(game_id, raw = FALSE) {

  # Input validation
  if (missing(game_id) || is.null(game_id) || game_id == "") {
    stop("'game_id' parameter is required. Provide ESPN game ID (e.g., '401677192').")
  }

  game_id <- as.character(game_id)

  # Validate game ID format (should be numeric)
  if (!grepl("^\\d+$", game_id)) {
    stop("'game_id' should be a numeric ESPN game ID (e.g., '401677192')")
  }

  # Build API URL (same as game summary since drives are included)
  url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/football/college-football/summary?event=%s", game_id)

  # Fetch and parse data
  tryCatch({
    message(sprintf("Fetching play-by-play data for game ID: %s", game_id))

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
      assign("cfb_playbyplay_raw", data, envir = .GlobalEnv)
      message("Raw play-by-play data assigned to: cfb_playbyplay_raw")
      message("Data structure preview:")

      # Show available sections
      sections <- names(data)
      message("- Available sections: ", paste(sections, collapse = ", "))

      # Show drives information
      drives_data <- extract_nested_pbp(data, c("drives"), list())
      if (length(drives_data) > 0) {
        drives_previous <- extract_nested_pbp(drives_data, c("previous"), list())
        message(sprintf("- Total drives found: %d", length(drives_previous)))

        if (length(drives_previous) > 0) {
          first_drive <- drives_previous[[1]]
          drive_desc <- extract_nested_pbp(first_drive, c("description"), "Unknown")
          plays <- extract_nested_pbp(first_drive, c("plays"), list())
          message(sprintf("- First drive: %s (%d plays)", drive_desc, length(plays)))
        }
      } else {
        message("- No drives data found")
      }

      return(invisible(data))
    }

    # Create play-by-play dataset
    playbyplay_df <- create_cfb_playbyplay_dataset(data, game_id)

    # Assign to global environment
    assign("cfb_playbyplay", playbyplay_df, envir = .GlobalEnv)

    # Summary message
    total_plays <- nrow(playbyplay_df)

    message(sprintf("Play-by-play data assigned to: cfb_playbyplay (%d plays)", total_plays))

    if (total_plays > 0) {
      # Count drives and teams
      unique_drives <- length(unique(playbyplay_df$drive_id[!is.na(playbyplay_df$drive_id)]))
      plays_with_data <- sum(!is.na(playbyplay_df$play_id))
      scoring_plays <- sum(playbyplay_df$play_scoring_play == "true", na.rm = TRUE)

      message(sprintf("  - Total drives: %d", unique_drives))
      message(sprintf("  - Individual plays: %d", plays_with_data))
      message(sprintf("  - Scoring plays: %d", scoring_plays))

      # Show teams involved
      teams <- unique(c(playbyplay_df$drive_team_abbreviation[!is.na(playbyplay_df$drive_team_abbreviation)]))
      teams <- teams[teams != ""]
      if (length(teams) > 0) {
        message(sprintf("  - Teams: %s", paste(teams, collapse = " vs ")))
      }

      # Show play types
      play_types <- table(playbyplay_df$play_type_text[!is.na(playbyplay_df$play_type_text)])
      if (length(play_types) > 0) {
        top_types <- head(sort(play_types, decreasing = TRUE), 3)
        message(sprintf("  - Top play types: %s", paste(names(top_types), collapse = ", ")))
      }

      # Show drive results
      drive_results <- table(playbyplay_df$drive_result[!is.na(playbyplay_df$drive_result)])
      if (length(drive_results) > 0) {
        message(sprintf("  - Drive outcomes: %s", paste(names(drive_results), collapse = ", ")))
      }

      # Show sample scoring plays
      if (scoring_plays > 0) {
        scoring_sample <- playbyplay_df[playbyplay_df$play_scoring_play == "true",
                                        c("play_team_abbreviation", "play_scoring_type", "play_text")]
        scoring_sample <- scoring_sample[!is.na(scoring_sample$play_team_abbreviation), ]

        if (nrow(scoring_sample) > 0) {
          message("\nSample scoring plays:")
          for (i in 1:min(3, nrow(scoring_sample))) {
            play <- scoring_sample[i, ]
            score_text <- play$play_text
            if (nchar(score_text) > 60) {
              score_text <- paste0(substr(score_text, 1, 57), "...")
            }
            message(sprintf("  %s %s: %s", play$play_team_abbreviation, play$play_scoring_type, score_text))
          }
        }
      }
    }

    return(invisible(playbyplay_df))

  }, error = function(e) {
    stop(sprintf("Failed to fetch play-by-play data for game ID '%s': %s", game_id, e$message))
  })
}

#' Safe nested data extraction helper function for college football scoring plays
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_scoring <- function(data, path, default = NA) {
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

#' Create college football scoring plays data frame from Site API response
#'
#' Processes raw JSON response from ESPN Site API into structured data frame
#' containing detailed scoring plays information
#'
#' @param data Raw JSON response from ESPN Site API game summary endpoint
#' @param game_id Character. Game ID used in request
#' @return Data frame with scoring plays information
#' @keywords internal
create_cfb_scoring_plays_dataset <- function(data, game_id) {

  # Initialize scoring plays data frame
  scoring_plays_df <- data.frame(
    query_game_id = character(0),
    game_id = character(0),
    scoring_play_id = character(0),
    scoring_play_number = character(0),
    play_type_id = character(0),
    play_type_text = character(0),
    play_type_abbreviation = character(0),
    play_text = character(0),
    away_score = character(0),
    home_score = character(0),
    period_number = character(0),
    clock_value = character(0),
    clock_display = character(0),
    scoring_team_id = character(0),
    scoring_team_uid = character(0),
    scoring_team_display_name = character(0),
    scoring_team_abbreviation = character(0),
    scoring_team_logo = character(0),
    scoring_type_name = character(0),
    scoring_type_display_name = character(0),
    scoring_type_abbreviation = character(0),
    stringsAsFactors = FALSE
  )

  # Extract scoring plays data
  scoring_plays <- extract_nested_scoring(data, c("scoringPlays"), list())

  if (length(scoring_plays) == 0) {
    warning("No scoring plays found in API response")
    return(scoring_plays_df)
  }

  for (i in seq_along(scoring_plays)) {
    scoring_play <- scoring_plays[[i]]

    # Basic scoring play information
    scoring_play_id <- extract_nested_scoring(scoring_play, c("id"), NA_character_)
    play_text <- extract_nested_scoring(scoring_play, c("text"), NA_character_)
    away_score <- extract_nested_scoring(scoring_play, c("awayScore"), NA_character_)
    home_score <- extract_nested_scoring(scoring_play, c("homeScore"), NA_character_)

    # Play type information
    play_type <- extract_nested_scoring(scoring_play, c("type"), list())
    play_type_id <- extract_nested_scoring(play_type, c("id"), NA_character_)
    play_type_text <- extract_nested_scoring(play_type, c("text"), NA_character_)
    play_type_abbreviation <- extract_nested_scoring(play_type, c("abbreviation"), NA_character_)

    # Period information
    period <- extract_nested_scoring(scoring_play, c("period"), list())
    period_number <- extract_nested_scoring(period, c("number"), NA_character_)

    # Clock information
    clock <- extract_nested_scoring(scoring_play, c("clock"), list())
    clock_value <- extract_nested_scoring(clock, c("value"), NA_character_)
    clock_display <- extract_nested_scoring(clock, c("displayValue"), NA_character_)

    # Team information
    team <- extract_nested_scoring(scoring_play, c("team"), list())
    scoring_team_id <- extract_nested_scoring(team, c("id"), NA_character_)
    scoring_team_uid <- extract_nested_scoring(team, c("uid"), NA_character_)
    scoring_team_display_name <- extract_nested_scoring(team, c("displayName"), NA_character_)
    scoring_team_abbreviation <- extract_nested_scoring(team, c("abbreviation"), NA_character_)
    scoring_team_logo <- extract_nested_scoring(team, c("logo"), NA_character_)

    # Alternative logo path
    if (is.na(scoring_team_logo)) {
      logos <- extract_nested_scoring(team, c("logos"), list())
      if (length(logos) > 0) {
        scoring_team_logo <- extract_nested_scoring(logos[[1]], c("href"), NA_character_)
      }
    }

    # Scoring type information
    scoring_type <- extract_nested_scoring(scoring_play, c("scoringType"), list())
    scoring_type_name <- extract_nested_scoring(scoring_type, c("name"), NA_character_)
    scoring_type_display_name <- extract_nested_scoring(scoring_type, c("displayName"), NA_character_)
    scoring_type_abbreviation <- extract_nested_scoring(scoring_type, c("abbreviation"), NA_character_)

    # Create scoring play row
    scoring_play_row <- data.frame(
      query_game_id = as.character(game_id),
      game_id = as.character(game_id),
      scoring_play_id = as.character(scoring_play_id),
      scoring_play_number = as.character(i),
      play_type_id = as.character(play_type_id),
      play_type_text = as.character(play_type_text),
      play_type_abbreviation = as.character(play_type_abbreviation),
      play_text = as.character(play_text),
      away_score = as.character(away_score),
      home_score = as.character(home_score),
      period_number = as.character(period_number),
      clock_value = as.character(clock_value),
      clock_display = as.character(clock_display),
      scoring_team_id = as.character(scoring_team_id),
      scoring_team_uid = as.character(scoring_team_uid),
      scoring_team_display_name = as.character(scoring_team_display_name),
      scoring_team_abbreviation = as.character(scoring_team_abbreviation),
      scoring_team_logo = as.character(scoring_team_logo),
      scoring_type_name = as.character(scoring_type_name),
      scoring_type_display_name = as.character(scoring_type_display_name),
      scoring_type_abbreviation = as.character(scoring_type_abbreviation),
      stringsAsFactors = FALSE
    )

    scoring_plays_df <- rbind(scoring_plays_df, scoring_play_row)
  }

  # Clean up row names
  if (nrow(scoring_plays_df) > 0) rownames(scoring_plays_df) <- NULL

  return(scoring_plays_df)
}

#' Fetch college football scoring plays using Site API
#'
#' Retrieves detailed scoring plays information for a specific college football game from ESPN's Site API.
#' The function fetches comprehensive scoring data including touchdowns, field goals,
#' safeties, and other scoring plays with timing and team information.
#'
#' @param game_id Character. ESPN game ID for the specific game.
#'   Examples: "401677192" (CFP Championship), "400934572", etc.
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'cfb_scoring_plays_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data frame. The main purpose is global
#'   environment assignment of \code{cfb_scoring_plays} containing:
#'   \itemize{
#'     \item Scoring details: play description, type, points scored
#'     \item Game state: period, clock, score after play
#'     \item Team information: scoring team details and logo
#'     \item Sequence: chronological order of all scoring plays
#'   }
#'
#' @details
#' The function creates a structured data frame with all scoring plays from the game.
#' Each row represents a single scoring play with complete context about when
#' and how the points were scored.
#'
#' **Scoring Information**:
#' \itemize{
#'   \item Play details: description of the scoring play
#'   \item Type: touchdown, field goal, safety, etc.
#'   \item Method: rushing, passing, kicking, defensive, etc.
#' }
#'
#' **Game Context**:
#' \itemize{
#'   \item Timing: period and game clock when score occurred
#'   \item Score: game score after the scoring play
#'   \item Sequence: chronological order of scoring
#' }
#'
#' **Team Details**:
#' \itemize{
#'   \item Identity: team that scored with full name and abbreviation
#'   \item Branding: team logo for visualization
#' }
#'
#' @examples
#' \dontrun{
#' # Get scoring plays for CFP Championship
#' fetch_college_football_scoring_plays("401677192")
#'
#' # Check the data
#' head(cfb_scoring_plays)
#' nrow(cfb_scoring_plays)
#'
#' # View all scoring plays in order
#' scoring_summary <- cfb_scoring_plays[, c("scoring_play_number", "period_number", "clock_display",
#'                                         "scoring_team_abbreviation", "scoring_type_display_name",
#'                                         "play_text", "away_score", "home_score")]
#'
#' print("Game scoring summary:")
#' print(scoring_summary)
#'
#' # Analyze scoring by team
#' if(require(dplyr, quietly = TRUE)) {
#'   team_scoring <- cfb_scoring_plays %>%
#'     group_by(scoring_team_abbreviation, scoring_type_display_name) %>%
#'     summarise(
#'       count = n(),
#'       .groups = 'drop'
#'     ) %>%
#'     arrange(scoring_team_abbreviation, desc(count))
#'
#'   print("Scoring by team and type:")
#'   print(team_scoring)
#' }
#'
#' # Analyze scoring by period
#' period_scoring <- table(cfb_scoring_plays$period_number, cfb_scoring_plays$scoring_team_abbreviation)
#' print("Scoring plays by period:")
#' print(period_scoring)
#'
#' # Find touchdowns vs field goals
#' touchdowns <- cfb_scoring_plays[cfb_scoring_plays$scoring_type_abbreviation == "TD", ]
#' field_goals <- cfb_scoring_plays[cfb_scoring_plays$scoring_type_abbreviation == "FG", ]
#'
#' cat(sprintf("Touchdowns: %d\n", nrow(touchdowns)))
#' cat(sprintf("Field Goals: %d\n", nrow(field_goals)))
#'
#' if(nrow(touchdowns) > 0) {
#'   td_types <- table(touchdowns$play_type_text)
#'   print("Touchdown types:")
#'   print(td_types)
#' }
#'
#' # Score progression analysis
#' cfb_scoring_plays$score_diff <- abs(as.numeric(cfb_scoring_plays$home_score) -
#'                                    as.numeric(cfb_scoring_plays$away_score))
#'
#' print("Score differential after each scoring play:")
#' score_progression <- cfb_scoring_plays[, c("scoring_play_number", "period_number",
#'                                           "clock_display", "scoring_team_abbreviation",
#'                                           "away_score", "home_score", "score_diff")]
#' print(score_progression)
#'
#' # Late game scoring (4th quarter)
#' fourth_quarter_scoring <- cfb_scoring_plays[cfb_scoring_plays$period_number == "4", ]
#' if(nrow(fourth_quarter_scoring) > 0) {
#'   print("Fourth quarter scoring:")
#'   print(fourth_quarter_scoring[, c("clock_display", "scoring_team_abbreviation",
#'                                   "scoring_type_display_name", "play_text")])
#' }
#'
#' # Longest scoring plays (for touchdowns)
#' if(nrow(touchdowns) > 0) {
#'   # Extract yardage from play text (simplified pattern matching)
#'   touchdowns$yards <- as.numeric(gsub(".*?(\\d+) Yd.*", "\\1", touchdowns$play_text))
#'
#'   longest_tds <- touchdowns[order(-touchdowns$yards, na.last = TRUE), ]
#'   print("Longest touchdown plays:")
#'   print(head(longest_tds[, c("scoring_team_abbreviation", "yards", "play_text")], 5))
#' }
#'
#' # Game momentum shifts
#' cfb_scoring_plays$cumulative_diff <- NA
#' for(i in 1:nrow(cfb_scoring_plays)) {
#'   home_score <- as.numeric(cfb_scoring_plays$home_score[i])
#'   away_score <- as.numeric(cfb_scoring_plays$away_score[i])
#'   cfb_scoring_plays$cumulative_diff[i] <- home_score - away_score
#' }
#'
#' print("Score momentum (positive = home team leading):")
#' momentum <- cfb_scoring_plays[, c("scoring_play_number", "period_number",
#'                                  "scoring_team_abbreviation", "cumulative_diff")]
#' print(momentum)
#' }
#'
#' @export
fetch_college_football_scoring_plays <- function(game_id, raw = FALSE) {

  # Input validation
  if (missing(game_id) || is.null(game_id) || game_id == "") {
    stop("'game_id' parameter is required. Provide ESPN game ID (e.g., '401677192').")
  }

  game_id <- as.character(game_id)

  # Validate game ID format (should be numeric)
  if (!grepl("^\\d+$", game_id)) {
    stop("'game_id' should be a numeric ESPN game ID (e.g., '401677192')")
  }

  # Build API URL (same as game summary since scoring plays are included)
  url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/football/college-football/summary?event=%s", game_id)

  # Fetch and parse data
  tryCatch({
    message(sprintf("Fetching scoring plays for game ID: %s", game_id))

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
      assign("cfb_scoring_plays_raw", data, envir = .GlobalEnv)
      message("Raw scoring plays data assigned to: cfb_scoring_plays_raw")
      message("Data structure preview:")

      # Show available sections
      sections <- names(data)
      message("- Available sections: ", paste(sections, collapse = ", "))

      # Show scoring plays information
      scoring_plays <- extract_nested_scoring(data, c("scoringPlays"), list())
      message(sprintf("- Total scoring plays found: %d", length(scoring_plays)))

      if (length(scoring_plays) > 0) {
        first_score <- scoring_plays[[1]]
        play_text <- extract_nested_scoring(first_score, c("text"), "Unknown")
        team <- extract_nested_scoring(first_score, c("team", "abbreviation"), "Unknown")
        score_type <- extract_nested_scoring(first_score, c("scoringType", "displayName"), "Unknown")

        message(sprintf("- First scoring play: %s %s - %s", team, score_type, play_text))
      }

      return(invisible(data))
    }

    # Create scoring plays dataset
    scoring_plays_df <- create_cfb_scoring_plays_dataset(data, game_id)

    # Assign to global environment
    assign("cfb_scoring_plays", scoring_plays_df, envir = .GlobalEnv)

    # Summary message
    total_scores <- nrow(scoring_plays_df)

    message(sprintf("Scoring plays assigned to: cfb_scoring_plays (%d scoring plays)", total_scores))

    if (total_scores > 0) {
      # Count by team
      teams <- unique(scoring_plays_df$scoring_team_abbreviation[!is.na(scoring_plays_df$scoring_team_abbreviation)])
      teams <- teams[teams != ""]

      if (length(teams) > 0) {
        message(sprintf("  - Teams that scored: %s", paste(teams, collapse = ", ")))

        # Count scores by team
        for (team in teams) {
          team_scores <- sum(scoring_plays_df$scoring_team_abbreviation == team, na.rm = TRUE)
          message(sprintf("    %s: %d scoring plays", team, team_scores))
        }
      }

      # Count by scoring type
      scoring_types <- table(scoring_plays_df$scoring_type_display_name[!is.na(scoring_plays_df$scoring_type_display_name)])
      if (length(scoring_types) > 0) {
        message(sprintf("  - Scoring types: %s", paste(names(scoring_types), collapse = ", ")))
      }

      # Show final score
      if (total_scores > 0) {
        final_away <- scoring_plays_df$away_score[total_scores]
        final_home <- scoring_plays_df$home_score[total_scores]
        message(sprintf("  - Final score: Away %s, Home %s", final_away, final_home))
      }

      # Show sample scoring plays
      if (total_scores > 0) {
        sample_scores <- min(3, total_scores)
        message("\nSample scoring plays:")
        for (i in 1:sample_scores) {
          play <- scoring_plays_df[i, ]
          play_text <- play$play_text
          if (nchar(play_text) > 60) {
            play_text <- paste0(substr(play_text, 1, 57), "...")
          }
          message(sprintf("  %d. Q%s %s - %s %s: %s",
                          i, play$period_number, play$clock_display,
                          play$scoring_team_abbreviation, play$scoring_type_display_name,
                          play_text))
        }
      }
    }

    return(invisible(scoring_plays_df))

  }, error = function(e) {
    stop(sprintf("Failed to fetch scoring plays for game ID '%s': %s", game_id, e$message))
  })
}
