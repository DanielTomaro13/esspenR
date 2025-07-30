#' Safe nested data extraction helper function for college football rankings
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_rankings <- function(data, path, default = NA) {
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

#' Get college football ranking poll types
#'
#' Returns information about available ranking polls
#' @return Named list with poll information
#' @export
get_cfb_ranking_polls <- function() {
  polls <- list(
    "AP Top 25" = list(
      "name" = "AP Top 25",
      "short_name" = "AP",
      "description" = "Associated Press Poll",
      "typical_size" = 25
    ),
    "Coaches Poll" = list(
      "name" = "Coaches Poll",
      "short_name" = "Coaches",
      "description" = "USA Today Coaches Poll",
      "typical_size" = 25
    ),
    "College Football Playoff" = list(
      "name" = "College Football Playoff",
      "short_name" = "CFP",
      "description" = "College Football Playoff Rankings",
      "typical_size" = 25
    ),
    "BCS" = list(
      "name" = "BCS",
      "short_name" = "BCS",
      "description" = "Bowl Championship Series (Historical)",
      "typical_size" = 25
    )
  )
  return(polls)
}

#' Create college football rankings data frame from Site API response
#'
#' Processes raw JSON response from ESPN Site API into structured data frame
#' containing college football rankings information
#'
#' @param data Raw JSON response from ESPN Site API rankings endpoint
#' @return Data frame with rankings information
#' @keywords internal
create_cfb_rankings_dataset <- function(data) {

  # Initialize rankings data frame
  rankings_df <- data.frame(
    poll_name = character(0),
    poll_short_name = character(0),
    poll_type = character(0),
    poll_published_date = character(0),
    season_year = character(0),
    season_type = character(0),
    week_number = character(0),
    rank_position = character(0),
    previous_rank = character(0),
    points = character(0),
    first_place_votes = character(0),
    trend = character(0),
    team_id = character(0),
    team_uid = character(0),
    team_abbreviation = character(0),
    team_display_name = character(0),
    team_short_display_name = character(0),
    team_name = character(0),
    team_location = character(0),
    team_color = character(0),
    team_alternate_color = character(0),
    team_logo = character(0),
    team_record_summary = character(0),
    team_record_wins = character(0),
    team_record_losses = character(0),
    team_record_ties = character(0),
    team_conference_id = character(0),
    team_conference_name = character(0),
    team_conference_short_name = character(0),
    stringsAsFactors = FALSE
  )

  # Extract rankings
  rankings <- extract_nested_rankings(data, c("rankings"), list())

  for (i in seq_along(rankings)) {
    ranking <- rankings[[i]]

    # Poll information
    poll_name <- extract_nested_rankings(ranking, c("name"), NA_character_)
    poll_short_name <- extract_nested_rankings(ranking, c("shortName"), NA_character_)
    poll_type <- extract_nested_rankings(ranking, c("type"), NA_character_)
    poll_published_date <- extract_nested_rankings(ranking, c("publishedDate"), NA_character_)

    # Season information
    season_info <- extract_nested_rankings(ranking, c("season"), list())
    season_year <- extract_nested_rankings(season_info, c("year"), NA_character_)
    season_type <- extract_nested_rankings(season_info, c("type"), NA_character_)

    # Week information
    week_info <- extract_nested_rankings(ranking, c("week"), list())
    week_number <- extract_nested_rankings(week_info, c("number"), NA_character_)

    # Extract ranks (teams)
    ranks <- extract_nested_rankings(ranking, c("ranks"), list())

    for (j in seq_along(ranks)) {
      rank_entry <- ranks[[j]]

      # Ranking position information
      rank_position <- extract_nested_rankings(rank_entry, c("current"), NA_character_)
      previous_rank <- extract_nested_rankings(rank_entry, c("previous"), NA_character_)
      points <- extract_nested_rankings(rank_entry, c("points"), NA_character_)
      first_place_votes <- extract_nested_rankings(rank_entry, c("firstPlaceVotes"), NA_character_)
      trend <- extract_nested_rankings(rank_entry, c("trend"), NA_character_)

      # Team information
      team_info <- extract_nested_rankings(rank_entry, c("team"), list())

      team_id <- extract_nested_rankings(team_info, c("id"), NA_character_)
      team_uid <- extract_nested_rankings(team_info, c("uid"), NA_character_)
      team_abbreviation <- extract_nested_rankings(team_info, c("abbreviation"), NA_character_)
      team_display_name <- extract_nested_rankings(team_info, c("displayName"), NA_character_)
      team_short_display_name <- extract_nested_rankings(team_info, c("shortDisplayName"), NA_character_)
      team_name <- extract_nested_rankings(team_info, c("name"), NA_character_)
      team_location <- extract_nested_rankings(team_info, c("location"), NA_character_)
      team_color <- extract_nested_rankings(team_info, c("color"), NA_character_)
      team_alternate_color <- extract_nested_rankings(team_info, c("alternateColor"), NA_character_)

      # Team logo
      team_logo <- NA_character_
      logos <- extract_nested_rankings(team_info, c("logos"), list())
      if (length(logos) > 0) {
        team_logo <- extract_nested_rankings(logos[[1]], c("href"), NA_character_)
      }

      # Team record
      team_record_summary <- NA_character_
      team_record_wins <- NA_character_
      team_record_losses <- NA_character_
      team_record_ties <- NA_character_

      record_info <- extract_nested_rankings(rank_entry, c("recordSummary"), NA_character_)
      if (!is.na(record_info)) {
        team_record_summary <- record_info

        # Try to parse wins-losses-ties
        if (grepl("^\\d+-\\d+", record_info)) {
          record_parts <- strsplit(record_info, "-")[[1]]
          if (length(record_parts) >= 2) {
            team_record_wins <- record_parts[1]
            team_record_losses <- record_parts[2]
            if (length(record_parts) >= 3) {
              team_record_ties <- record_parts[3]
            } else {
              team_record_ties <- "0"
            }
          }
        }
      }

      # Conference information
      team_conference_id <- NA_character_
      team_conference_name <- NA_character_
      team_conference_short_name <- NA_character_

      # Try multiple paths for conference info
      conference_info <- extract_nested_rankings(team_info, c("conferenceId"), NA_character_)
      if (!is.na(conference_info)) {
        team_conference_id <- conference_info
      }

      conferences <- extract_nested_rankings(team_info, c("conferences"), list())
      if (length(conferences) > 0) {
        first_conf <- conferences[[1]]
        if (is.na(team_conference_id)) {
          team_conference_id <- extract_nested_rankings(first_conf, c("id"), NA_character_)
        }
        team_conference_name <- extract_nested_rankings(first_conf, c("name"), NA_character_)
        team_conference_short_name <- extract_nested_rankings(first_conf, c("shortName"), NA_character_)
      }

      # Create row
      ranking_row <- data.frame(
        poll_name = as.character(poll_name),
        poll_short_name = as.character(poll_short_name),
        poll_type = as.character(poll_type),
        poll_published_date = as.character(poll_published_date),
        season_year = as.character(season_year),
        season_type = as.character(season_type),
        week_number = as.character(week_number),
        rank_position = as.character(rank_position),
        previous_rank = as.character(previous_rank),
        points = as.character(points),
        first_place_votes = as.character(first_place_votes),
        trend = as.character(trend),
        team_id = as.character(team_id),
        team_uid = as.character(team_uid),
        team_abbreviation = as.character(team_abbreviation),
        team_display_name = as.character(team_display_name),
        team_short_display_name = as.character(team_short_display_name),
        team_name = as.character(team_name),
        team_location = as.character(team_location),
        team_color = as.character(team_color),
        team_alternate_color = as.character(team_alternate_color),
        team_logo = as.character(team_logo),
        team_record_summary = as.character(team_record_summary),
        team_record_wins = as.character(team_record_wins),
        team_record_losses = as.character(team_record_losses),
        team_record_ties = as.character(team_record_ties),
        team_conference_id = as.character(team_conference_id),
        team_conference_name = as.character(team_conference_name),
        team_conference_short_name = as.character(team_conference_short_name),
        stringsAsFactors = FALSE
      )

      rankings_df <- rbind(rankings_df, ranking_row)
    }
  }

  # Clean up row names
  if (nrow(rankings_df) > 0) rownames(rankings_df) <- NULL

  return(rankings_df)
}

#' Fetch college football rankings using Site API
#'
#' Retrieves college football rankings from ESPN's Site API.
#' The function fetches comprehensive ranking information including
#' AP Top 25, Coaches Poll, and College Football Playoff rankings.
#'
#' @param year Integer. Season year to fetch rankings for (default: current year).
#' @param week Integer. Week number to fetch rankings for (default: NULL for latest).
#' @param season_type Integer. Season type: 2=Regular, 3=Postseason (default: 2).
#' @param poll Character. Specific poll to fetch. Options: "all", "ap", "coaches", "cfp" (default: "all").
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'cfb_rankings_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data frame. The main purpose is global
#'   environment assignment of \code{cfb_rankings} containing:
#'   \itemize{
#'     \item Poll information: name, type, published date
#'     \item Season details: year, type, week number
#'     \item Ranking data: position, previous rank, points, first-place votes, trend
#'     \item Team details: names, abbreviations, colors, logos, records
#'     \item Conference information: conference ID, name, short name
#'   }
#'
#' @details
#' The function creates a structured data frame with comprehensive college football ranking information.
#' Each row represents a team's ranking in a specific poll with detailed information about
#' the team, their ranking position, and poll-specific data.
#'
#' **Poll Information**:
#' \itemize{
#'   \item Poll details: name, short name, type, published date
#'   \item Timing: season year, week number, season type
#' }
#'
#' **Ranking Data**:
#' \itemize{
#'   \item Position: current rank, previous rank
#'   \item Voting: points received, first-place votes
#'   \item Movement: trend (up, down, new, etc.)
#' }
#'
#' **Team Information**:
#' \itemize{
#'   \item Identity: team names, abbreviations, locations
#'   \item Branding: team colors, logos
#'   \item Performance: win-loss records
#'   \item Conference: conference affiliation and details
#' }
#'
#' **Available Polls**:
#' \itemize{
#'   \item AP Top 25: Associated Press Poll
#'   \item Coaches Poll: USA Today Coaches Poll
#'   \item CFP Rankings: College Football Playoff Rankings
#'   \item Historical: BCS Rankings (when available)
#' }
#'
#' @examples
#' \dontrun{
#' # Get all current rankings
#' fetch_college_football_rankings()
#'
#' # Get rankings for specific week
#' fetch_college_football_rankings(week = 10)
#'
#' # Get rankings for specific year
#' fetch_college_football_rankings(year = 2023, week = 15)
#'
#' # Get only AP Poll rankings
#' fetch_college_football_rankings(poll = "ap")
#'
#' # Get CFP Rankings
#' fetch_college_football_rankings(poll = "cfp")
#'
#' # Check the data
#' head(cfb_rankings)
#'
#' # View AP Top 10
#' ap_top10 <- cfb_rankings[
#'   cfb_rankings$poll_short_name == "AP" &
#'   as.numeric(cfb_rankings$rank_position) <= 10,
#'   c("rank_position", "team_display_name", "team_record_summary", "points")
#' ]
#' print("AP Top 10:")
#' print(ap_top10)
#'
#' # Compare polls for same teams
#' if(require(dplyr, quietly = TRUE)) {
#'   poll_comparison <- cfb_rankings %>%
#'     filter(rank_position <= 25) %>%
#'     select(poll_short_name, rank_position, team_display_name) %>%
#'     pivot_wider(names_from = poll_short_name, values_from = rank_position) %>%
#'     arrange(as.numeric(AP))
#'
#'   print("Poll comparison:")
#'   print(head(poll_comparison, 15))
#' }
#'
#' # First-place votes analysis
#' first_place <- cfb_rankings[
#'   !is.na(cfb_rankings$first_place_votes) &
#'   cfb_rankings$first_place_votes != "" &
#'   cfb_rankings$first_place_votes != "0",
#'   c("poll_short_name", "rank_position", "team_display_name", "first_place_votes")
#' ]
#' if(nrow(first_place) > 0) {
#'   print("Teams receiving first-place votes:")
#'   print(first_place)
#' }
#'
#' # Movement analysis
#' movement <- cfb_rankings[
#'   !is.na(cfb_rankings$trend) & cfb_rankings$trend != "",
#'   c("poll_short_name", "rank_position", "previous_rank", "team_display_name", "trend")
#' ]
#' if(nrow(movement) > 0) {
#'   print("Ranking movement:")
#'   print(head(movement, 20))
#' }
#'
#' # Conference representation in rankings
#' conf_representation <- table(cfb_rankings$team_conference_name[cfb_rankings$team_conference_name != ""])
#' print("Conference representation in rankings:")
#' print(sort(conf_representation, decreasing = TRUE))
#' }
#'
#' @seealso \code{\link{get_cfb_ranking_polls}} for poll information
#'
#' @export
fetch_college_football_rankings <- function(year = as.integer(format(Sys.Date(), "%Y")),
                                            week = NULL, season_type = 2, poll = "all", raw = FALSE) {

  # Input validation
  if (!is.numeric(year) || year < 1990 || year > 2030) {
    stop("'year' must be a valid year between 1990 and 2030")
  }

  if (!is.null(week)) {
    if (!is.numeric(week) || week < 1 || week > 20) {
      stop("'week' must be a number between 1 and 20")
    }
  }

  if (!season_type %in% c(2, 3)) {
    stop("'season_type' must be 2 (regular season) or 3 (postseason)")
  }

  valid_polls <- c("all", "ap", "coaches", "cfp")
  if (!tolower(poll) %in% valid_polls) {
    stop(sprintf("'poll' must be one of: %s", paste(valid_polls, collapse = ", ")))
  }

  poll <- tolower(poll)

  # Build API URL
  url <- "https://site.api.espn.com/apis/site/v2/sports/football/college-football/rankings"

  # Build query parameters
  params <- list()

  if (!is.null(week)) {
    params$week <- week
    message(sprintf("Fetching college football rankings for week %d, %d season...", week, year))
  } else {
    message(sprintf("Fetching latest college football rankings for %d season...", year))
  }

  params$season <- year
  params$seasontype <- season_type

  # Add query parameters to URL
  if (length(params) > 0) {
    param_strings <- paste0(names(params), "=", params)
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
      assign("cfb_rankings_raw", data, envir = .GlobalEnv)
      message("Raw college football rankings data assigned to: cfb_rankings_raw")
      message("Data structure preview:")

      # Show available sections
      sections <- names(data)
      message("- Available sections: ", paste(sections, collapse = ", "))

      # Show rankings count
      rankings <- extract_nested_rankings(data, c("rankings"), list())
      message("- Total polls found: ", length(rankings))

      if (length(rankings) > 0) {
        poll_names <- character(0)
        for (ranking in rankings) {
          poll_name <- extract_nested_rankings(ranking, c("shortName"), "Unknown")
          poll_names <- c(poll_names, poll_name)
        }
        message("- Available polls: ", paste(poll_names, collapse = ", "))

        first_ranking <- rankings[[1]]
        ranking_sections <- names(first_ranking)
        message("- First poll sections: ", paste(ranking_sections, collapse = ", "))

        # Show number of teams in first poll
        ranks <- extract_nested_rankings(first_ranking, c("ranks"), list())
        message("- Teams in first poll: ", length(ranks))
      }

      return(invisible(data))
    }

    # Create rankings dataset
    rankings_df <- create_cfb_rankings_dataset(data)

    # Filter by poll if specified
    if (poll != "all" && nrow(rankings_df) > 0) {
      poll_filter <- switch(poll,
                            "ap" = "AP",
                            "coaches" = "Coaches",
                            "cfp" = "CFP",
                            "all" = NULL
      )

      if (!is.null(poll_filter)) {
        rankings_df <- rankings_df[rankings_df$poll_short_name == poll_filter, ]
        if (nrow(rankings_df) == 0) {
          warning(sprintf("No %s poll data found for specified parameters", poll_filter))
        }
      }
    }

    # Assign to global environment
    assign("cfb_rankings", rankings_df, envir = .GlobalEnv)

    # Summary message
    total_rankings <- nrow(rankings_df)

    if (poll == "all") {
      message(sprintf("College football rankings assigned to: cfb_rankings (%d team rankings)", total_rankings))
    } else {
      message(sprintf("College football %s rankings assigned to: cfb_rankings (%d team rankings)",
                      toupper(poll), total_rankings))
    }

    if (total_rankings > 0) {
      # Show available polls
      unique_polls <- unique(rankings_df$poll_short_name[!is.na(rankings_df$poll_short_name)])
      if (length(unique_polls) > 0) {
        message(sprintf("  - Available polls: %s", paste(unique_polls, collapse = ", ")))

        # Show teams per poll
        for (poll_name in unique_polls) {
          poll_teams <- sum(rankings_df$poll_short_name == poll_name, na.rm = TRUE)
          message(sprintf("    %s: %d teams", poll_name, poll_teams))
        }
      }

      # Show season and week info
      season_years <- unique(rankings_df$season_year[!is.na(rankings_df$season_year)])
      week_numbers <- unique(rankings_df$week_number[!is.na(rankings_df$week_number)])

      if (length(season_years) > 0) {
        message(sprintf("  - Season(s): %s", paste(season_years, collapse = ", ")))
      }
      if (length(week_numbers) > 0) {
        message(sprintf("  - Week(s): %s", paste(week_numbers, collapse = ", ")))
      }

      # Show published dates
      pub_dates <- unique(rankings_df$poll_published_date[!is.na(rankings_df$poll_published_date)])
      if (length(pub_dates) > 0) {
        message(sprintf("  - Published: %s", paste(substr(pub_dates, 1, 10), collapse = ", ")))
      }

      # Show top 5 from first poll
      if (total_rankings >= 5) {
        first_poll <- unique(rankings_df$poll_short_name)[1]
        top5 <- rankings_df[
          rankings_df$poll_short_name == first_poll &
            as.numeric(rankings_df$rank_position) <= 5,
          c("rank_position", "team_display_name", "team_record_summary")
        ]

        if (nrow(top5) > 0) {
          message(sprintf("\n%s Top 5:", first_poll))
          for (i in 1:nrow(top5)) {
            team <- top5[i, ]
            record_info <- ifelse(is.na(team$team_record_summary) || team$team_record_summary == "",
                                  "", paste0(" (", team$team_record_summary, ")"))
            message(sprintf("  %s. %s%s", team$rank_position, team$team_display_name, record_info))
          }
        }
      }
    }

    return(invisible(rankings_df))

  }, error = function(e) {
    stop(sprintf("Failed to fetch college football rankings: %s", e$message))
  })
}

#' Fetch college football rankings for multiple weeks
#'
#' Retrieves rankings data for multiple weeks with rate limiting.
#' This function calls \code{\link{fetch_college_football_rankings}} for each week
#' and combines the results.
#'
#' @param weeks Integer vector. Week numbers to fetch rankings for.
#' @param year Integer. Season year to fetch (default: current year).
#' @param season_type Integer. Season type (default: 2 for regular season).
#' @param poll Character. Specific poll to fetch (default: "all").
#' @param delay Numeric. Delay in seconds between API requests (default: 0.5).
#' @param raw Logical. If TRUE, assigns raw JSON to global environment
#'   for the first week only (default: FALSE).
#'
#' @return Invisibly returns the combined data frame. The main purpose is global
#'   environment assignment of combined \code{cfb_rankings} from all weeks.
#'
#' @examples
#' \dontrun{
#' # Get rankings for multiple weeks
#' fetch_multiple_cfb_rankings(weeks = c(8, 9, 10, 11, 12))
#'
#' # Get AP Poll for specific weeks
#' fetch_multiple_cfb_rankings(weeks = c(10, 15), poll = "ap")
#'
#' # Analyze ranking changes over time
#' if(require(dplyr, quietly = TRUE)) {
#'   ranking_trends <- cfb_rankings %>%
#'     filter(poll_short_name == "AP") %>%
#'     select(week_number, rank_position, team_display_name) %>%
#'     arrange(team_display_name, week_number)
#'
#'   print("Ranking trends:")
#'   print(head(ranking_trends, 20))
#' }
#' }
#'
#' @seealso \code{\link{fetch_college_football_rankings}} for single week data
#' @export
fetch_multiple_cfb_rankings <- function(weeks, year = as.integer(format(Sys.Date(), "%Y")),
                                        season_type = 2, poll = "all", delay = 0.5, raw = FALSE) {
  # Input validation
  if (length(weeks) == 0) {
    stop("'weeks' must contain at least one week")
  }

  if (!all(is.numeric(weeks)) || any(weeks < 1) || any(weeks > 20)) {
    stop("All 'weeks' must be numbers between 1 and 20")
  }

  # Initialize combined data container
  all_rankings <- data.frame()

  message(sprintf("Starting to fetch college football rankings for %d weeks (%s season)...",
                  length(weeks), year))

  # Process each week sequentially
  for (i in seq_along(weeks)) {
    week <- weeks[i]
    message(sprintf("Fetching rankings for week %d (%d/%d)...", week, i, length(weeks)))

    tryCatch({
      # Fetch individual week data
      week_data <- fetch_college_football_rankings(
        year = year,
        week = week,
        season_type = season_type,
        poll = poll,
        raw = raw
      )

      # If raw data requested, return after first week
      if (isTRUE(raw)) {
        return(invisible(week_data))
      }

      # Combine data
      rankings_df <- get("cfb_rankings", envir = .GlobalEnv)
      all_rankings <- rbind(all_rankings, rankings_df)

    }, error = function(e) {
      message(sprintf("Failed to fetch rankings for week %d: %s", week, e$message))
    })

    # Be respectful to the API with delay between requests
    if (i < length(weeks)) {
      Sys.sleep(delay)
    }
  }

  # Remove duplicates and assign combined dataset to global environment
  if (nrow(all_rankings) > 0) {
    # Remove duplicates based on poll, week, and team
    all_rankings <- all_rankings[!duplicated(paste(all_rankings$poll_short_name,
                                                   all_rankings$week_number,
                                                   all_rankings$team_id)), ]
    assign("cfb_rankings", all_rankings, envir = .GlobalEnv)

    total_rankings <- nrow(all_rankings)
    unique_weeks <- length(unique(all_rankings$week_number))
    unique_polls <- length(unique(all_rankings$poll_short_name))

    message(sprintf("Combined college football rankings assigned to: cfb_rankings (%d weeks, %d polls, %d rankings)",
                    unique_weeks, unique_polls, total_rankings))

    # Show week breakdown
    week_counts <- table(all_rankings$week_number)
    message("Rankings per week:")
    for (week_num in names(week_counts)) {
      message(sprintf("  Week %s: %d rankings", week_num, week_counts[week_num]))
    }

    # Show poll breakdown
    poll_counts <- table(all_rankings$poll_short_name)
    message("Rankings per poll:")
    for (poll_name in names(poll_counts)) {
      message(sprintf("  %s: %d rankings", poll_name, poll_counts[poll_name]))
    }
  }

  return(invisible(all_rankings))
}
