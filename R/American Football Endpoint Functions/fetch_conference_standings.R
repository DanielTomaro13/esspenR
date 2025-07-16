#' Fetch NFL conference standings data using Core API
#'
#' @param year Numeric. Season year (e.g., 2023, 2024).
#' @param seasontype Numeric. Season type (1=preseason, 2=regular, 3=postseason). Default 2.
#' @param conference Character or Numeric. Conference - "AFC"/"NFC" or 8/7. Default "AFC".
#' @param raw Logical. If TRUE, assigns raw JSON and links to global environment. Default FALSE.
#' @return Invisibly returns the data, but main purpose is global environment assignment.
#' @export
#'
#' @examples
#' # Get AFC standings for 2024 regular season
#' fetch_nfl_conference_standings(year = 2024, conference = "AFC")
#' head(nfl_conference_overall)
#'
#' # Get NFC standings
#' fetch_nfl_conference_standings(year = 2024, conference = "NFC")
#'
#' # Get raw data and links
#' fetch_nfl_conference_standings(year = 2024, conference = "AFC", raw = TRUE)
#' head(nfl_conference_standings_links)
#'
fetch_nfl_conference_standings <- function(year, seasontype = 2, conference = "AFC", raw = FALSE) {
  # Validate inputs
  if (missing(year)) {
    stop("'year' is a required parameter")
  }
  if (!is.numeric(year) || year < 1990 || year > as.numeric(format(Sys.Date(), "%Y")) + 2) {
    stop("year must be a valid numeric year between 1990 and two years in the future")
  }
  if (!seasontype %in% c(1, 2, 3)) {
    stop("seasontype must be 1 (preseason), 2 (regular season), or 3 (postseason)")
  }

  # Convert conference to numeric ID
  conference_id <- NULL
  conference_name <- NULL
  if (is.character(conference)) {
    if (toupper(conference) == "AFC") {
      conference_id <- 8
      conference_name <- "AFC"
    } else if (toupper(conference) == "NFC") {
      conference_id <- 7
      conference_name <- "NFC"
    } else {
      stop("conference must be 'AFC' or 'NFC' (or numeric 8/7)")
    }
  } else if (is.numeric(conference)) {
    if (conference == 8) {
      conference_id <- 8
      conference_name <- "AFC"
    } else if (conference == 7) {
      conference_id <- 7
      conference_name <- "NFC"
    } else {
      stop("conference ID must be 8 (AFC) or 7 (NFC)")
    }
  } else {
    stop("conference must be 'AFC'/'NFC' or numeric 8/7")
  }

  # Build main URL to get standings types
  base_url <- sprintf("https://sports.core.api.espn.com/v2/sports/football/leagues/nfl/seasons/%d/types/%d/groups/%d/standings",
                      year, seasontype, conference_id)

  # Fetch main standings types
  tryCatch({
    resp <- httr::GET(base_url, httr::timeout(60))
    if (httr::status_code(resp) != 200) {
      stop(sprintf("API request failed: HTTP %d - %s",
                   httr::status_code(resp),
                   httr::http_status(resp)$message))
    }

    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    main_data <- jsonlite::fromJSON(content_text, simplifyVector = FALSE, simplifyDataFrame = FALSE)

    # Extract standings links
    standings_links <- data.frame(
      standings_type = character(0),
      id = character(0),
      name = character(0),
      display_name = character(0),
      ref_url = character(0),
      stringsAsFactors = FALSE
    )

    if ("items" %in% names(main_data) && length(main_data[["items"]]) > 0) {
      for (i in seq_along(main_data[["items"]])) {
        item <- main_data[["items"]][[i]]
        standings_links <- rbind(standings_links, data.frame(
          standings_type = if ("name" %in% names(item)) item[["name"]] else NA_character_,
          id = if ("id" %in% names(item)) item[["id"]] else NA_character_,
          name = if ("name" %in% names(item)) item[["name"]] else NA_character_,
          display_name = if ("displayName" %in% names(item)) item[["displayName"]] else NA_character_,
          ref_url = if ("$ref" %in% names(item)) item[["$ref"]] else NA_character_,
          stringsAsFactors = FALSE
        ))
      }
    }

    # If raw = TRUE, assign links and fetch raw data for each
    if (isTRUE(raw)) {
      # Assign links dataset
      assign("nfl_conference_standings_links", standings_links, envir = .GlobalEnv)

      # Fetch raw data for each standings type
      assigned_raw_vars <- c()
      for (i in 1:nrow(standings_links)) {
        if (!is.na(standings_links$ref_url[i])) {
          standings_type <- standings_links$standings_type[i]

          # Fetch raw data
          raw_resp <- httr::GET(standings_links$ref_url[i], httr::timeout(60))
          if (httr::status_code(raw_resp) == 200) {
            raw_content <- httr::content(raw_resp, as = "text", encoding = "UTF-8")
            raw_data <- jsonlite::fromJSON(raw_content, simplifyVector = FALSE, simplifyDataFrame = FALSE)

            # Assign to global environment
            raw_var_name <- paste0("nfl_conference_standings_raw_", standings_type)
            assign(raw_var_name, raw_data, envir = .GlobalEnv)
            assigned_raw_vars <- c(assigned_raw_vars, raw_var_name)
          }
        }
      }

      message(sprintf("Raw NFL %s conference standings data assigned to global environment:", conference_name))
      message(sprintf("  Links: nfl_conference_standings_links"))
      message(sprintf("  Raw data: %s", paste(assigned_raw_vars, collapse = ", ")))

      return(invisible(list(links = standings_links, raw_data = assigned_raw_vars)))
    }

    # Find and fetch only the "overall" standings
    overall_url <- NULL
    for (i in 1:nrow(standings_links)) {
      if (!is.na(standings_links$standings_type[i]) && standings_links$standings_type[i] == "overall") {
        overall_url <- standings_links$ref_url[i]
        break
      }
    }

    if (!is.null(overall_url)) {
      # Fetch overall standings data
      standings_resp <- httr::GET(overall_url, httr::timeout(60))
      if (httr::status_code(standings_resp) == 200) {
        standings_content <- httr::content(standings_resp, as = "text", encoding = "UTF-8")
        standings_data <- jsonlite::fromJSON(standings_content, simplifyVector = FALSE, simplifyDataFrame = FALSE)

        # Create clean dataset
        clean_dataset <- create_clean_conference_standings(standings_data, "overall", conference_id, conference_name)

        # Assign to global environment
        assign("nfl_conference_overall", clean_dataset, envir = .GlobalEnv)

        message(sprintf("NFL %s conference overall standings assigned to: nfl_conference_overall", conference_name))
      } else {
        stop("Failed to fetch overall standings data")
      }
    } else {
      stop("Could not find overall standings in the response")
    }

    return(invisible(clean_dataset))

  }, error = function(e) {
    stop(sprintf("Failed to fetch NFL %s conference standings data: %s", conference_name, e$message))
  })
}

#' Create clean conference standings dataset from ESPN Core API response
#'
#' @param standings_data Raw JSON response from ESPN Core API standings endpoint
#' @param standings_type Type of standings (overall, playoff, expanded, division)
#' @param conference_id Conference ID (7=NFC, 8=AFC)
#' @param conference_name Conference name (AFC/NFC)
#' @return Clean data frame with one row per team
create_clean_conference_standings <- function(standings_data, standings_type, conference_id, conference_name) {
  # Initialize result data frame
  result_df <- data.frame(
    conference_id = numeric(0),
    conference_name = character(0),
    standings_type = character(0),
    team_id = character(0),
    team_ref = character(0),
    # Key stats - will be extracted from the "overall" record
    wins = numeric(0),
    losses = numeric(0),
    ties = numeric(0),
    win_percentage = numeric(0),
    points_for = numeric(0),
    points_against = numeric(0),
    point_differential = numeric(0),
    home_record = character(0),
    away_record = character(0),
    division_record = character(0),
    conference_record = character(0),
    playoff_seed = character(0),
    clincher = character(0),
    streak = character(0),
    stringsAsFactors = FALSE
  )

  # Process standings entries if they exist
  if ("standings" %in% names(standings_data) && length(standings_data[["standings"]]) > 0) {
    for (standing_idx in seq_along(standings_data[["standings"]])) {
      standing_entry <- standings_data[["standings"]][[standing_idx]]

      # Extract team information
      team_ref <- if ("team" %in% names(standing_entry) && "$ref" %in% names(standing_entry[["team"]]))
        standing_entry[["team"]][["$ref"]] else NA_character_

      # Extract team ID from reference URL
      team_id <- if (!is.na(team_ref)) {
        team_id_match <- regmatches(team_ref, regexpr("[0-9]+(?=\\?|$)", team_ref, perl = TRUE))
        if (length(team_id_match) > 0) team_id_match else NA_character_
      } else NA_character_

      # Initialize team row with basic info
      team_row <- data.frame(
        conference_id = conference_id,
        conference_name = conference_name,
        standings_type = standings_type,
        team_id = team_id,
        team_ref = team_ref,
        wins = NA_real_,
        losses = NA_real_,
        ties = NA_real_,
        win_percentage = NA_real_,
        points_for = NA_real_,
        points_against = NA_real_,
        point_differential = NA_real_,
        home_record = NA_character_,
        away_record = NA_character_,
        division_record = NA_character_,
        conference_record = NA_character_,
        playoff_seed = NA_character_,
        clincher = NA_character_,
        streak = NA_character_,
        stringsAsFactors = FALSE
      )

      # Process records for this team to extract key stats
      if ("records" %in% names(standing_entry) && length(standing_entry[["records"]]) > 0) {
        for (record_idx in seq_along(standing_entry[["records"]])) {
          record <- standing_entry[["records"]][[record_idx]]
          record_name <- if ("name" %in% names(record)) record[["name"]] else NA_character_

          # Extract stats from this record
          if ("stats" %in% names(record) && length(record[["stats"]]) > 0) {
            for (stat in record[["stats"]]) {
              stat_name <- if ("name" %in% names(stat)) stat[["name"]] else NA_character_
              stat_value <- if ("value" %in% names(stat)) stat[["value"]] else NA
              stat_display <- if ("displayValue" %in% names(stat)) stat[["displayValue"]] else NA_character_

              # Map stats based on record type and stat name
              if (record_name == "overall") {
                if (stat_name == "wins") team_row$wins <- as.numeric(stat_value)
                else if (stat_name == "losses") team_row$losses <- as.numeric(stat_value)
                else if (stat_name == "ties") team_row$ties <- as.numeric(stat_value)
                else if (stat_name == "winPercent") team_row$win_percentage <- as.numeric(stat_value)
                else if (stat_name == "pointsFor") team_row$points_for <- as.numeric(stat_value)
                else if (stat_name == "pointsAgainst") team_row$points_against <- as.numeric(stat_value)
                else if (stat_name == "differential" || stat_name == "pointDifferential") team_row$point_differential <- as.numeric(stat_value)
                else if (stat_name == "playoffSeed") team_row$playoff_seed <- stat_display
                else if (stat_name == "clincher") team_row$clincher <- stat_display
                else if (stat_name == "streak") team_row$streak <- stat_display
              } else if (record_name == "Home") {
                team_row$home_record <- if ("summary" %in% names(record)) record[["summary"]] else stat_display
              } else if (record_name == "Road") {
                team_row$away_record <- if ("summary" %in% names(record)) record[["summary"]] else stat_display
              } else if (record_name == "vs. Div.") {
                team_row$division_record <- if ("summary" %in% names(record)) record[["summary"]] else stat_display
              } else if (record_name == "vs. Conf.") {
                team_row$conference_record <- if ("summary" %in% names(record)) record[["summary"]] else stat_display
              }
            }
          }

          # Also grab summary records
          if (record_name == "Home" && "summary" %in% names(record)) {
            team_row$home_record <- record[["summary"]]
          } else if (record_name == "Road" && "summary" %in% names(record)) {
            team_row$away_record <- record[["summary"]]
          } else if (record_name == "vs. Div." && "summary" %in% names(record)) {
            team_row$division_record <- record[["summary"]]
          } else if (record_name == "vs. Conf." && "summary" %in% names(record)) {
            team_row$conference_record <- record[["summary"]]
          }
        }
      }

      # Add this team to results
      result_df <- rbind(result_df, team_row)
    }
  }

  # Clean up row names
  if (nrow(result_df) > 0) {
    rownames(result_df) <- NULL
  }

  return(result_df)
}
