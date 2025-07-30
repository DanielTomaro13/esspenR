#' Safe nested data extraction helper function for NFL draft
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_draft <- function(data, path, default = NA) {
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

#' Fetch detailed information from Core API reference URL
#'
#' Retrieves detailed information from ESPN's Core API reference URL
#' @param ref_url Character. Full reference URL
#' @param timeout Numeric. Request timeout in seconds (default: 30)
#' @return List with detailed information or NULL if failed
#' @keywords internal
fetch_reference_data <- function(ref_url, timeout = 30) {
  if (is.na(ref_url) || ref_url == "" || is.null(ref_url)) {
    return(NULL)
  }

  tryCatch({
    resp <- httr::GET(ref_url, httr::timeout(timeout))

    if (httr::status_code(resp) != 200) {
      return(NULL)
    }

    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content_text, simplifyVector = FALSE, simplifyDataFrame = FALSE)
    return(data)

  }, error = function(e) {
    return(NULL)
  })
}

#' Create NFL draft data frames from Core API response - FIXED VERSION
#'
#' Processes raw JSON response from ESPN Core API into structured data frames
#' containing detailed draft information including complete draft order
#'
#' @param data Raw JSON response from ESPN Core API draft endpoint
#' @param year Character. Season year used in request
#' @param fetch_details Logical. Whether to fetch detailed information from reference URLs
#' @param detail_delay Numeric. Delay between detail API requests in seconds
#' @return List containing multiple data frames with draft information
#' @keywords internal
create_nfldraft_datasets <- function(data, year, fetch_details = TRUE, detail_delay = 0.1) {

  # Initialize draft summary data frame
  draft_summary_df <- data.frame(
    season_year = character(0),
    draft_uid = character(0),
    draft_display_name = character(0),
    draft_short_display_name = character(0),
    number_of_rounds = character(0),
    status_ref = character(0),
    athletes_ref = character(0),
    rounds_ref = character(0),
    total_positions = character(0),
    total_teams_with_needs = character(0),
    stringsAsFactors = FALSE
  )

  # Initialize positions data frame
  positions_df <- data.frame(
    season_year = character(0),
    position_ref = character(0),
    position_id = character(0),
    position_name = character(0),
    position_display_name = character(0),
    position_abbreviation = character(0),
    position_leaf = character(0),
    parent_position_ref = character(0),
    stringsAsFactors = FALSE
  )

  # Initialize team needs data frame
  team_needs_df <- data.frame(
    season_year = character(0),
    team_ref = character(0),
    team_id = character(0),
    position_id = character(0),
    position_name = character(0),
    position_display_name = character(0),
    position_abbreviation = character(0),
    need_priority = character(0),
    stringsAsFactors = FALSE
  )

  # Initialize athletes data frame - NOW BASED ON DRAFT ORDER
  athletes_df <- data.frame(
    season_year = character(0),
    round_number = character(0),
    pick_number = character(0),
    overall_pick = character(0),
    pick_status = character(0),
    traded = character(0),
    trade_note = character(0),
    drafting_team_ref = character(0),
    drafting_team_id = character(0),
    athlete_ref = character(0),
    athlete_id = character(0),
    athlete_guid = character(0),
    athlete_first_name = character(0),
    athlete_last_name = character(0),
    athlete_full_name = character(0),
    athlete_display_name = character(0),
    athlete_short_name = character(0),
    athlete_weight = character(0),
    athlete_display_weight = character(0),
    athlete_height = character(0),
    athlete_display_height = character(0),
    athlete_position_ref = character(0),
    athlete_position_id = character(0),
    athlete_position_name = character(0),
    athlete_position_abbreviation = character(0),
    college_ref = character(0),
    college_id = character(0),
    draft_grade = character(0),
    position_rank = character(0),
    overall_rank = character(0),
    speed_score = character(0),
    stringsAsFactors = FALSE
  )

  # Initialize rounds data frame (from rounds reference)
  rounds_df <- data.frame(
    season_year = character(0),
    round_number = character(0),
    round_name = character(0),
    round_display_name = character(0),
    total_picks = character(0),
    stringsAsFactors = FALSE
  )

  # Initialize draft order data frame
  draft_order_df <- data.frame(
    season_year = character(0),
    round_number = character(0),
    pick_number = character(0),
    overall_pick = character(0),
    pick_status = character(0),
    traded = character(0),
    trade_note = character(0),
    drafting_team_ref = character(0),
    drafting_team_id = character(0),
    athlete_ref = character(0),
    athlete_id = character(0),
    athlete_name = character(0),
    athlete_display_name = character(0),
    athlete_position_abbreviation = character(0),
    athlete_college = character(0),
    stringsAsFactors = FALSE
  )

  # Extract basic draft information
  draft_uid <- extract_nested_draft(data, c("uid"), NA_character_)
  draft_display_name <- extract_nested_draft(data, c("displayName"), NA_character_)
  draft_short_display_name <- extract_nested_draft(data, c("shortDisplayName"), NA_character_)
  number_of_rounds <- extract_nested_draft(data, c("numberOfRounds"), NA_character_)

  # Extract reference URLs
  status_ref <- extract_nested_draft(data, c("status", "$ref"), NA_character_)
  athletes_ref <- extract_nested_draft(data, c("athletes", "$ref"), NA_character_)
  rounds_ref <- extract_nested_draft(data, c("rounds", "$ref"), NA_character_)

  # Extract positions information
  positions <- extract_nested_draft(data, c("positions"), list())
  total_positions <- length(positions)

  for (i in seq_along(positions)) {
    position_data <- positions[[i]]

    position_ref <- extract_nested_draft(position_data, c("$ref"), NA_character_)
    position_id <- extract_nested_draft(position_data, c("id"), NA_character_)
    position_name <- extract_nested_draft(position_data, c("name"), NA_character_)
    position_display_name <- extract_nested_draft(position_data, c("displayName"), NA_character_)
    position_abbreviation <- extract_nested_draft(position_data, c("abbreviation"), NA_character_)
    position_leaf <- extract_nested_draft(position_data, c("leaf"), NA_character_)
    parent_position_ref <- extract_nested_draft(position_data, c("parent", "$ref"), NA_character_)

    position_row <- data.frame(
      season_year = as.character(year),
      position_ref = as.character(position_ref),
      position_id = as.character(position_id),
      position_name = as.character(position_name),
      position_display_name = as.character(position_display_name),
      position_abbreviation = as.character(position_abbreviation),
      position_leaf = as.character(position_leaf),
      parent_position_ref = as.character(parent_position_ref),
      stringsAsFactors = FALSE
    )

    positions_df <- rbind(positions_df, position_row)
  }

  # Extract team needs information
  needs <- extract_nested_draft(data, c("needs"), list())
  total_teams_with_needs <- length(needs)

  for (i in seq_along(needs)) {
    need_data <- needs[[i]]

    team_info <- extract_nested_draft(need_data, c("team"), list())
    team_ref <- extract_nested_draft(team_info, c("$ref"), NA_character_)

    # Extract team ID from reference URL
    team_id <- NA_character_
    if (!is.na(team_ref)) {
      team_id_match <- regmatches(team_ref, regexpr("teams/([0-9]+)", team_ref))
      if (length(team_id_match) > 0) {
        team_id <- gsub("teams/", "", team_id_match)
      }
    }

    need_positions <- extract_nested_draft(need_data, c("positions"), list())

    for (j in seq_along(need_positions)) {
      position_data <- need_positions[[j]]

      position_id <- extract_nested_draft(position_data, c("id"), NA_character_)
      position_name <- extract_nested_draft(position_data, c("name"), NA_character_)
      position_display_name <- extract_nested_draft(position_data, c("displayName"), NA_character_)
      position_abbreviation <- extract_nested_draft(position_data, c("abbreviation"), NA_character_)

      need_row <- data.frame(
        season_year = as.character(year),
        team_ref = as.character(team_ref),
        team_id = as.character(team_id),
        position_id = as.character(position_id),
        position_name = as.character(position_name),
        position_display_name = as.character(position_display_name),
        position_abbreviation = as.character(position_abbreviation),
        need_priority = as.character(j),
        stringsAsFactors = FALSE
      )

      team_needs_df <- rbind(team_needs_df, need_row)
    }
  }

  # Add draft summary row
  summary_row <- data.frame(
    season_year = as.character(year),
    draft_uid = as.character(draft_uid),
    draft_display_name = as.character(draft_display_name),
    draft_short_display_name = as.character(draft_short_display_name),
    number_of_rounds = as.character(number_of_rounds),
    status_ref = as.character(status_ref),
    athletes_ref = as.character(athletes_ref),
    rounds_ref = as.character(rounds_ref),
    total_positions = as.character(total_positions),
    total_teams_with_needs = as.character(total_teams_with_needs),
    stringsAsFactors = FALSE
  )

  draft_summary_df <- rbind(draft_summary_df, summary_row)

  # Fetch detailed information if requested
  if (fetch_details) {
    message("Fetching detailed draft information...")

    # Fetch complete draft order and athlete details from rounds data
    if (!is.na(rounds_ref) && rounds_ref != "") {
      message("Fetching complete draft order with athlete details...")
      rounds_data <- fetch_reference_data(rounds_ref)

      if (!is.null(rounds_data)) {
        rounds_items <- extract_nested_draft(rounds_data, c("items"), list())

        for (round_num in seq_along(rounds_items)) {
          round_item <- rounds_items[[round_num]]

          round_number <- extract_nested_draft(round_item, c("number"), as.character(round_num))
          round_name <- extract_nested_draft(round_item, c("displayName"), paste("Round", round_number))
          round_display_name <- round_name

          # Get picks for this round - picks are directly embedded
          picks_items <- extract_nested_draft(round_item, c("picks"), list())
          total_picks <- length(picks_items)

          if (total_picks > 0) {
            message(sprintf("Processing round %s: %d picks...", round_number, total_picks))

            for (pick_num in seq_along(picks_items)) {
              pick_data <- picks_items[[pick_num]]

              # Extract pick information directly from embedded data
              pick_number <- extract_nested_draft(pick_data, c("pick"), NA_character_)
              overall_pick <- extract_nested_draft(pick_data, c("overall"), NA_character_)
              traded <- extract_nested_draft(pick_data, c("traded"), "false")
              trade_note <- extract_nested_draft(pick_data, c("tradeNote"), NA_character_)

              # Pick status
              status_info <- extract_nested_draft(pick_data, c("status"), list())
              pick_status <- extract_nested_draft(status_info, c("name"), NA_character_)

              # Drafting team
              team_info <- extract_nested_draft(pick_data, c("team"), list())
              drafting_team_ref <- extract_nested_draft(team_info, c("$ref"), NA_character_)

              # Extract team ID from reference URL
              drafting_team_id <- NA_character_
              if (!is.na(drafting_team_ref)) {
                team_id_match <- regmatches(drafting_team_ref, regexpr("teams/([0-9]+)", drafting_team_ref))
                if (length(team_id_match) > 0) {
                  drafting_team_id <- gsub("teams/", "", team_id_match)
                }
              }

              # Athlete information
              athlete_info <- extract_nested_draft(pick_data, c("athlete"), list())
              athlete_ref <- extract_nested_draft(athlete_info, c("$ref"), NA_character_)

              # Initialize athlete details
              athlete_id <- NA_character_
              athlete_guid <- NA_character_
              athlete_first_name <- NA_character_
              athlete_last_name <- NA_character_
              athlete_full_name <- NA_character_
              athlete_display_name <- NA_character_
              athlete_short_name <- NA_character_
              athlete_weight <- NA_character_
              athlete_display_weight <- NA_character_
              athlete_height <- NA_character_
              athlete_display_height <- NA_character_
              athlete_position_ref <- NA_character_
              athlete_position_id <- NA_character_
              athlete_position_name <- NA_character_
              athlete_position_abbreviation <- NA_character_
              college_ref <- NA_character_
              college_id <- NA_character_
              draft_grade <- NA_character_
              position_rank <- NA_character_
              overall_rank <- NA_character_
              speed_score <- NA_character_

              # Fetch comprehensive athlete details if available
              if (!is.na(athlete_ref) && athlete_ref != "") {
                if (pick_num %% 25 == 0) {
                  message(sprintf("  Fetching athlete details for pick %d/%d...", pick_num, total_picks))
                }

                athlete_details <- fetch_reference_data(athlete_ref)

                if (!is.null(athlete_details)) {
                  # Basic athlete information
                  athlete_id <- extract_nested_draft(athlete_details, c("id"), NA_character_)
                  athlete_guid <- extract_nested_draft(athlete_details, c("guid"), NA_character_)
                  athlete_first_name <- extract_nested_draft(athlete_details, c("firstName"), NA_character_)
                  athlete_last_name <- extract_nested_draft(athlete_details, c("lastName"), NA_character_)
                  athlete_full_name <- extract_nested_draft(athlete_details, c("fullName"), NA_character_)
                  athlete_display_name <- extract_nested_draft(athlete_details, c("displayName"), athlete_full_name)
                  athlete_short_name <- extract_nested_draft(athlete_details, c("shortName"), NA_character_)

                  # Physical measurements
                  athlete_weight <- extract_nested_draft(athlete_details, c("weight"), NA_character_)
                  athlete_display_weight <- extract_nested_draft(athlete_details, c("displayWeight"), NA_character_)
                  athlete_height <- extract_nested_draft(athlete_details, c("height"), NA_character_)
                  athlete_display_height <- extract_nested_draft(athlete_details, c("displayHeight"), NA_character_)

                  # Position information
                  position_info <- extract_nested_draft(athlete_details, c("position"), list())
                  athlete_position_ref <- extract_nested_draft(position_info, c("$ref"), NA_character_)
                  athlete_position_id <- extract_nested_draft(position_info, c("id"), NA_character_)
                  athlete_position_name <- extract_nested_draft(position_info, c("name"), NA_character_)
                  athlete_position_abbreviation <- extract_nested_draft(position_info, c("abbreviation"), NA_character_)

                  # College information
                  college_info <- extract_nested_draft(athlete_details, c("college"), list())
                  college_ref <- extract_nested_draft(college_info, c("$ref"), NA_character_)
                  if (!is.na(college_ref)) {
                    college_id_match <- regmatches(college_ref, regexpr("colleges/([0-9]+)", college_ref))
                    if (length(college_id_match) > 0) {
                      college_id <- gsub("colleges/", "", college_id_match)
                    }
                  }

                  # Draft attributes/grades
                  attributes <- extract_nested_draft(athlete_details, c("attributes"), list())
                  for (attr in attributes) {
                    attr_name <- extract_nested_draft(attr, c("name"), NA_character_)
                    attr_value <- extract_nested_draft(attr, c("displayValue"), NA_character_)

                    if (!is.na(attr_name)) {
                      if (attr_name == "grade") {
                        draft_grade <- attr_value
                      } else if (attr_name == "rank") {
                        position_rank <- attr_value
                      } else if (attr_name == "overall") {
                        overall_rank <- attr_value
                      } else if (attr_name == "speed") {
                        speed_score <- attr_value
                      }
                    }
                  }
                }

                # Add delay to be respectful to the API
                Sys.sleep(detail_delay)
              }

              # Add comprehensive athlete row (this becomes the main athletes dataset)
              athlete_row <- data.frame(
                season_year = as.character(year),
                round_number = as.character(round_number),
                pick_number = as.character(pick_number),
                overall_pick = as.character(overall_pick),
                pick_status = as.character(pick_status),
                traded = as.character(traded),
                trade_note = as.character(trade_note),
                drafting_team_ref = as.character(drafting_team_ref),
                drafting_team_id = as.character(drafting_team_id),
                athlete_ref = as.character(athlete_ref),
                athlete_id = as.character(athlete_id),
                athlete_guid = as.character(athlete_guid),
                athlete_first_name = as.character(athlete_first_name),
                athlete_last_name = as.character(athlete_last_name),
                athlete_full_name = as.character(athlete_full_name),
                athlete_display_name = as.character(athlete_display_name),
                athlete_short_name = as.character(athlete_short_name),
                athlete_weight = as.character(athlete_weight),
                athlete_display_weight = as.character(athlete_display_weight),
                athlete_height = as.character(athlete_height),
                athlete_display_height = as.character(athlete_display_height),
                athlete_position_ref = as.character(athlete_position_ref),
                athlete_position_id = as.character(athlete_position_id),
                athlete_position_name = as.character(athlete_position_name),
                athlete_position_abbreviation = as.character(athlete_position_abbreviation),
                college_ref = as.character(college_ref),
                college_id = as.character(college_id),
                draft_grade = as.character(draft_grade),
                position_rank = as.character(position_rank),
                overall_rank = as.character(overall_rank),
                speed_score = as.character(speed_score),
                stringsAsFactors = FALSE
              )

              athletes_df <- rbind(athletes_df, athlete_row)

              # Also add simplified draft order row
              draft_order_row <- data.frame(
                season_year = as.character(year),
                round_number = as.character(round_number),
                pick_number = as.character(pick_number),
                overall_pick = as.character(overall_pick),
                pick_status = as.character(pick_status),
                traded = as.character(traded),
                trade_note = as.character(trade_note),
                drafting_team_ref = as.character(drafting_team_ref),
                drafting_team_id = as.character(drafting_team_id),
                athlete_ref = as.character(athlete_ref),
                athlete_id = as.character(athlete_id),
                athlete_name = as.character(athlete_full_name),
                athlete_display_name = as.character(athlete_display_name),
                athlete_position_abbreviation = as.character(athlete_position_abbreviation),
                athlete_college = as.character(college_id),
                stringsAsFactors = FALSE
              )

              draft_order_df <- rbind(draft_order_df, draft_order_row)
            }

            # Add round row
            round_row <- data.frame(
              season_year = as.character(year),
              round_number = as.character(round_number),
              round_name = as.character(round_name),
              round_display_name = as.character(round_display_name),
              total_picks = as.character(total_picks),
              stringsAsFactors = FALSE
            )

            rounds_df <- rbind(rounds_df, round_row)
          }
        }
      }
    }
  }

  # Clean up row names
  if (nrow(draft_summary_df) > 0) rownames(draft_summary_df) <- NULL
  if (nrow(positions_df) > 0) rownames(positions_df) <- NULL
  if (nrow(team_needs_df) > 0) rownames(team_needs_df) <- NULL
  if (nrow(athletes_df) > 0) rownames(athletes_df) <- NULL
  if (nrow(rounds_df) > 0) rownames(rounds_df) <- NULL
  if (nrow(draft_order_df) > 0) rownames(draft_order_df) <- NULL

  return(list(
    summary = draft_summary_df,
    positions = positions_df,
    team_needs = team_needs_df,
    athletes = athletes_df,
    rounds = rounds_df,
    draft_order = draft_order_df
  ))
}

#' Fetch NFL draft data using Core API - COMPLETE FIXED VERSION
#'
#' Retrieves detailed NFL draft information from ESPN's Core API.
#' The function fetches comprehensive draft data including athletes,
#' rounds, positions, team needs, and complete draft order for a specific draft year.
#'
#' @param year Character or Numeric. Draft year (default: 2024).
#'   The year of the NFL draft to retrieve information for.
#' @param return_type Character. Type of data to return (default: "all").
#'   Options are:
#'   \itemize{
#'     \item "summary" - Basic draft summary information
#'     \item "positions" - Available positions information
#'     \item "team_needs" - Team draft needs by position
#'     \item "athletes" - Comprehensive drafted athletes information (in draft order)
#'     \item "rounds" - Draft rounds information
#'     \item "draft_order" - Simple draft order with pick-by-pick details
#'     \item "all" - All data types combined
#'   }
#' @param fetch_details Logical. Whether to fetch detailed information
#'   from reference URLs (default: TRUE). This makes additional API calls.
#' @param detail_delay Numeric. Delay between detail API requests in seconds
#'   (default: 0.1). Used to be respectful to ESPN's servers.
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'nfl_draft_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data. The main purpose is global
#'   environment assignment based on return_type:
#'   \itemize{
#'     \item \code{nfl_draft_summary} - Draft summary data frame
#'     \item \code{nfl_draft_positions} - Positions data frame
#'     \item \code{nfl_draft_team_needs} - Team needs data frame
#'     \item \code{nfl_draft_athletes} - Comprehensive athletes data frame (in draft order)
#'     \item \code{nfl_draft_rounds} - Rounds data frame
#'     \item \code{nfl_draft_order} - Simple draft order data frame
#'     \item \code{nfl_draft_raw} - Raw JSON response (if raw = TRUE)
#'   }
#'
#' @export
fetch_nfl_draft <- function(year = 2024, return_type = "all", fetch_details = TRUE,
                            detail_delay = 0.1, raw = FALSE) {

  # Input validation
  valid_types <- c("summary", "positions", "team_needs", "athletes", "rounds", "draft_order", "all")
  if (!return_type %in% valid_types) {
    stop(sprintf("'return_type' must be one of: %s", paste(valid_types, collapse = ", ")))
  }

  if (!is.logical(fetch_details)) {
    stop("'fetch_details' must be TRUE or FALSE")
  }

  if (!is.numeric(detail_delay) || detail_delay < 0) {
    stop("'detail_delay' must be a non-negative numeric value")
  }

  # Convert year to character for URL building
  year <- as.character(year)

  # Build API URL for Core API
  url <- sprintf("https://sports.core.api.espn.com/v2/sports/football/leagues/nfl/seasons/%s/draft", year)

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
      assign("nfl_draft_raw", data, envir = .GlobalEnv)
      message("Raw NFL draft data assigned to: nfl_draft_raw")
      message("Data structure preview:")

      # Show available sections
      sections <- names(data)
      message("- Available sections: ", paste(sections, collapse = ", "))

      # Show reference URLs
      athletes_ref <- extract_nested_draft(data, c("athletes", "$ref"), "none")
      rounds_ref <- extract_nested_draft(data, c("rounds", "$ref"), "none")
      message("- Athletes reference: ", athletes_ref)
      message("- Rounds reference: ", rounds_ref)

      # Show positions and needs counts
      positions <- extract_nested_draft(data, c("positions"), list())
      needs <- extract_nested_draft(data, c("needs"), list())
      message("- Total positions: ", length(positions))
      message("- Teams with needs: ", length(needs))

      return(invisible(data))
    }

    # Warn user about detail fetching
    if (fetch_details) {
      rounds_ref <- extract_nested_draft(data, c("rounds", "$ref"), NA_character_)
      if (!is.na(rounds_ref)) {
        message("Fetching complete draft order and athlete details will make many additional API requests...")
        message("This may take several minutes. Set fetch_details=FALSE for faster results.")
      }
    }

    # Create datasets
    datasets <- create_nfldraft_datasets(data, year, fetch_details, detail_delay)

    # Assign data based on return_type
    result_data <- list()

    if (return_type %in% c("summary", "all")) {
      assign("nfl_draft_summary", datasets$summary, envir = .GlobalEnv)
      result_data$summary <- datasets$summary
      message(sprintf("NFL draft summary assigned to: nfl_draft_summary (%d records)", nrow(datasets$summary)))
    }

    if (return_type %in% c("positions", "all")) {
      assign("nfl_draft_positions", datasets$positions, envir = .GlobalEnv)
      result_data$positions <- datasets$positions
      message(sprintf("NFL draft positions assigned to: nfl_draft_positions (%d positions)", nrow(datasets$positions)))
    }

    if (return_type %in% c("team_needs", "all")) {
      assign("nfl_draft_team_needs", datasets$team_needs, envir = .GlobalEnv)
      result_data$team_needs <- datasets$team_needs
      message(sprintf("NFL draft team needs assigned to: nfl_draft_team_needs (%d needs)", nrow(datasets$team_needs)))
    }

    if (return_type %in% c("athletes", "all")) {
      assign("nfl_draft_athletes", datasets$athletes, envir = .GlobalEnv)
      result_data$athletes <- datasets$athletes

      unique_athletes <- length(unique(datasets$athletes$athlete_id[!is.na(datasets$athletes$athlete_id)]))
      message(sprintf("NFL draft athletes assigned to: nfl_draft_athletes (%d athletes with full details)", unique_athletes))
    }

    if (return_type %in% c("rounds", "all")) {
      assign("nfl_draft_rounds", datasets$rounds, envir = .GlobalEnv)
      result_data$rounds <- datasets$rounds
      message(sprintf("NFL draft rounds assigned to: nfl_draft_rounds (%d rounds)", nrow(datasets$rounds)))
    }

    if (return_type %in% c("draft_order", "all")) {
      assign("nfl_draft_order", datasets$draft_order, envir = .GlobalEnv)
      result_data$draft_order <- datasets$draft_order
      message(sprintf("NFL draft order assigned to: nfl_draft_order (%d picks)", nrow(datasets$draft_order)))
    }

    # Return appropriate data structure
    if (return_type == "all") {
      return(invisible(result_data))
    } else {
      return(invisible(result_data[[return_type]]))
    }

  }, error = function(e) {
    stop(sprintf("Failed to fetch NFL draft for year %s: %s", year, e$message))
  })
}

#' Fetch multiple NFL draft data across years
#'
#' Retrieves draft data for multiple years with rate limiting to
#' be respectful to ESPN's Core API. This function calls \code{\link{fetch_nfl_draft}}
#' for each year and combines the results.
#'
#' @param years Character or Numeric vector. Draft years to fetch (default: 2024).
#'   Vector of years for the NFL drafts to retrieve information for.
#' @param return_type Character. Type of data to return (default: "all").
#'   Same options as \code{\link{fetch_nfl_draft}}.
#' @param fetch_details Logical. Whether to fetch detailed information
#'   from reference URLs (default: TRUE). This makes many additional API calls.
#' @param detail_delay Numeric. Delay between detail API requests in seconds
#'   (default: 0.1). Used to be respectful to ESPN's servers.
#' @param delay Numeric. Delay in seconds between main API requests (default: 0.2).
#'   Used to be respectful to ESPN's servers and avoid rate limiting.
#' @param raw Logical. If TRUE, assigns raw JSON to global environment
#'   for the first year only (default: FALSE).
#'
#' @return Invisibly returns the combined data frames. The main purpose is global
#'   environment assignment of combined datasets from all years.
#'
#' @export
fetch_multiple_nfl_draft <- function(years = 2024, return_type = "all", fetch_details = TRUE,
                                     detail_delay = 0.1, delay = 0.2, raw = FALSE) {
  # Input validation
  if (length(years) == 0) {
    stop("'years' must contain at least one year")
  }

  valid_types <- c("summary", "positions", "team_needs", "athletes", "rounds", "draft_order", "all")
  if (!return_type %in% valid_types) {
    stop(sprintf("'return_type' must be one of: %s", paste(valid_types, collapse = ", ")))
  }

  # Initialize combined data containers
  all_summary <- data.frame()
  all_positions <- data.frame()
  all_team_needs <- data.frame()
  all_athletes <- data.frame()
  all_rounds <- data.frame()
  all_draft_order <- data.frame()

  message(sprintf("Starting to fetch NFL draft data for %d years...", length(years)))

  # Process each year sequentially
  for (i in seq_along(years)) {
    year <- years[i]
    message(sprintf("Fetching NFL draft for %s (%d/%d)...", year, i, length(years)))

    tryCatch({
      # Fetch individual year data
      year_data <- fetch_nfl_draft(
        year = year,
        return_type = return_type,
        fetch_details = fetch_details,
        detail_delay = detail_delay,
        raw = raw
      )

      # If raw data requested, return after first year
      if (isTRUE(raw)) {
        return(invisible(year_data))
      }

      # Combine data based on return type
      if (return_type %in% c("summary", "all")) {
        summary_df <- get("nfl_draft_summary", envir = .GlobalEnv)
        all_summary <- rbind(all_summary, summary_df)
      }

      if (return_type %in% c("positions", "all")) {
        positions_df <- get("nfl_draft_positions", envir = .GlobalEnv)
        all_positions <- rbind(all_positions, positions_df)
      }

      if (return_type %in% c("team_needs", "all")) {
        team_needs_df <- get("nfl_draft_team_needs", envir = .GlobalEnv)
        all_team_needs <- rbind(all_team_needs, team_needs_df)
      }

      if (return_type %in% c("athletes", "all")) {
        athletes_df <- get("nfl_draft_athletes", envir = .GlobalEnv)
        all_athletes <- rbind(all_athletes, athletes_df)
      }

      if (return_type %in% c("rounds", "all")) {
        rounds_df <- get("nfl_draft_rounds", envir = .GlobalEnv)
        all_rounds <- rbind(all_rounds, rounds_df)
      }

      if (return_type %in% c("draft_order", "all")) {
        draft_order_df <- get("nfl_draft_order", envir = .GlobalEnv)
        all_draft_order <- rbind(all_draft_order, draft_order_df)
      }

    }, error = function(e) {
      message(sprintf("Failed to fetch NFL draft for year %s: %s", year, e$message))
    })

    # Be respectful to the API with delay between requests
    if (i < length(years)) {
      Sys.sleep(delay)
    }
  }

  # Remove duplicates and assign combined datasets to global environment
  result_data <- list()

  if (return_type %in% c("summary", "all") && nrow(all_summary) > 0) {
    all_summary <- all_summary[!duplicated(all_summary$season_year), ]
    assign("nfl_draft_summary", all_summary, envir = .GlobalEnv)
    result_data$summary <- all_summary

    message(sprintf("Combined NFL draft summary assigned to: nfl_draft_summary (%d years)", nrow(all_summary)))
  }

  if (return_type %in% c("positions", "all") && nrow(all_positions) > 0) {
    all_positions <- all_positions[!duplicated(paste(all_positions$season_year, all_positions$position_id)), ]
    assign("nfl_draft_positions", all_positions, envir = .GlobalEnv)
    result_data$positions <- all_positions

    unique_years <- length(unique(all_positions$season_year))
    message(sprintf("Combined NFL draft positions assigned to: nfl_draft_positions (%d years, %d positions)",
                    unique_years, nrow(all_positions)))
  }

  if (return_type %in% c("team_needs", "all") && nrow(all_team_needs) > 0) {
    all_team_needs <- all_team_needs[!duplicated(paste(all_team_needs$season_year,
                                                       all_team_needs$team_id,
                                                       all_team_needs$position_id)), ]
    assign("nfl_draft_team_needs", all_team_needs, envir = .GlobalEnv)
    result_data$team_needs <- all_team_needs

    unique_years <- length(unique(all_team_needs$season_year))
    message(sprintf("Combined NFL draft team needs assigned to: nfl_draft_team_needs (%d years, %d needs)",
                    unique_years, nrow(all_team_needs)))
  }

  if (return_type %in% c("athletes", "all") && nrow(all_athletes) > 0) {
    all_athletes <- all_athletes[!duplicated(paste(all_athletes$season_year, all_athletes$athlete_id)), ]
    assign("nfl_draft_athletes", all_athletes, envir = .GlobalEnv)
    result_data$athletes <- all_athletes

    unique_years <- length(unique(all_athletes$season_year))
    unique_athletes <- length(unique(all_athletes$athlete_id[!is.na(all_athletes$athlete_id)]))
    message(sprintf("Combined NFL draft athletes assigned to: nfl_draft_athletes (%d years, %d athletes)",
                    unique_years, unique_athletes))
  }

  if (return_type %in% c("rounds", "all") && nrow(all_rounds) > 0) {
    all_rounds <- all_rounds[!duplicated(paste(all_rounds$season_year, all_rounds$round_number)), ]
    assign("nfl_draft_rounds", all_rounds, envir = .GlobalEnv)
    result_data$rounds <- all_rounds

    unique_years <- length(unique(all_rounds$season_year))
    message(sprintf("Combined NFL draft rounds assigned to: nfl_draft_rounds (%d years, %d rounds)",
                    unique_years, nrow(all_rounds)))
  }

  if (return_type %in% c("draft_order", "all") && nrow(all_draft_order) > 0) {
    all_draft_order <- all_draft_order[!duplicated(paste(all_draft_order$season_year, all_draft_order$overall_pick)), ]
    assign("nfl_draft_order", all_draft_order, envir = .GlobalEnv)
    result_data$draft_order <- all_draft_order

    unique_years <- length(unique(all_draft_order$season_year))
    message(sprintf("Combined NFL draft order assigned to: nfl_draft_order (%d years, %d picks)",
                    unique_years, nrow(all_draft_order)))
  }

  # Return appropriate data structure
  if (return_type == "all") {
    return(invisible(result_data))
  } else {
    return(invisible(result_data[[return_type]]))
  }
}
