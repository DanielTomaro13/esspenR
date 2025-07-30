#' Fetch specific NFL team data using Core API
#'
#' @param team_id Character or Numeric. ESPN team ID (e.g., "12" for Kansas City Chiefs).
#' @param year Numeric. Season year (e.g., 2023, 2024).
#' @param raw Logical. If TRUE, assigns raw JSON to global environment as 'nfl_team_raw'. Default FALSE.
#' @return Invisibly returns the data, but main purpose is global environment assignment.
#' @export
#'
#' @examples
#' # Get Kansas City Chiefs data for 2024
#' fetch_nfl_specific_team(team_id = "12", year = 2024)
#' head(nfl_team_info)
#'
#' # Get Buffalo Bills data
#' fetch_nfl_specific_team(team_id = 2, year = 2024)
#'
#' # Get raw data for custom processing
#' fetch_nfl_specific_team(team_id = "12", year = 2024, raw = TRUE)
#' str(nfl_team_raw, max.level = 2)
#'
fetch_nfl_specific_team <- function(team_id, year, raw = FALSE) {
  # Validate inputs
  if (missing(team_id)) {
    stop("'team_id' is a required parameter")
  }
  if (missing(year)) {
    stop("'year' is a required parameter")
  }
  if (!is.numeric(year) || year < 1990 || year > as.numeric(format(Sys.Date(), "%Y")) + 2) {
    stop("year must be a valid numeric year between 1990 and two years in the future")
  }

  # Convert team_id to character for URL building
  team_id <- as.character(team_id)

  # Build URL
  url <- sprintf("https://sports.core.api.espn.com/v2/sports/football/leagues/nfl/seasons/%d/teams/%s",
                 year, team_id)

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
      assign("nfl_team_raw", data, envir = .GlobalEnv)
      message("Raw NFL team data assigned to: nfl_team_raw")
      return(invisible(data))
    }

    # Create clean team dataset
    team_df <- create_clean_specific_team_dataset(data, team_id, year)

    # Assign to global environment
    assign("nfl_team_info", team_df, envir = .GlobalEnv)

    message(sprintf("NFL team data assigned to: nfl_team_info (Team ID: %s, Year: %d)",
                    team_id, year))

    return(invisible(team_df))

  }, error = function(e) {
    stop(sprintf("Failed to fetch NFL team data for team %s in %d: %s", team_id, year, e$message))
  })
}

#' Create clean specific team dataset from ESPN Core API response
#'
#' @param data Raw JSON response from ESPN Core API team endpoint
#' @param team_id Team ID used in request
#' @param year Year used in request
#' @return Clean data frame with team information
create_clean_specific_team_dataset <- function(data, team_id, year) {
  # Initialize result data frame
  result_df <- data.frame(
    request_team_id = character(0),
    request_year = integer(0),
    # Basic team info
    team_id = character(0),
    team_uid = character(0),
    team_slug = character(0),
    team_abbreviation = character(0),
    team_display_name = character(0),
    team_short_display_name = character(0),
    team_name = character(0),
    team_nickname = character(0),
    team_location = character(0),
    team_color = character(0),
    team_alternate_color = character(0),
    is_active = logical(0),
    is_all_star = logical(0),
    # Season-specific info
    season_year = integer(0),
    season_type = integer(0),
    season_slug = character(0),
    # Logos
    team_logo = character(0),
    team_logo_dark = character(0),
    # Links/References
    team_ref = character(0),
    venue_ref = character(0),
    groups_ref = character(0),
    record_ref = character(0),
    athletes_ref = character(0),
    events_ref = character(0),
    stringsAsFactors = FALSE
  )

  # Extract basic team information
  team_id_api <- if ("id" %in% names(data)) data[["id"]] else NA_character_
  team_uid <- if ("uid" %in% names(data)) data[["uid"]] else NA_character_
  team_slug <- if ("slug" %in% names(data)) data[["slug"]] else NA_character_
  team_abbreviation <- if ("abbreviation" %in% names(data)) data[["abbreviation"]] else NA_character_
  team_display_name <- if ("displayName" %in% names(data)) data[["displayName"]] else NA_character_
  team_short_display_name <- if ("shortDisplayName" %in% names(data)) data[["shortDisplayName"]] else NA_character_
  team_name <- if ("name" %in% names(data)) data[["name"]] else NA_character_
  team_nickname <- if ("nickname" %in% names(data)) data[["nickname"]] else NA_character_
  team_location <- if ("location" %in% names(data)) data[["location"]] else NA_character_
  team_color <- if ("color" %in% names(data)) data[["color"]] else NA_character_
  team_alternate_color <- if ("alternateColor" %in% names(data)) data[["alternateColor"]] else NA_character_
  is_active <- if ("isActive" %in% names(data)) data[["isActive"]] else TRUE
  is_all_star <- if ("isAllStar" %in% names(data)) data[["isAllStar"]] else FALSE

  # Season information
  season_year_api <- year  # Default to request year
  season_type <- NA_integer_
  season_slug <- NA_character_

  if ("season" %in% names(data)) {
    season_info <- data[["season"]]
    season_year_api <- if ("year" %in% names(season_info)) season_info[["year"]] else year
    season_type <- if ("type" %in% names(season_info)) season_info[["type"]] else NA_integer_
    season_slug <- if ("slug" %in% names(season_info)) season_info[["slug"]] else NA_character_
  }

  # Logos
  team_logo <- team_logo_dark <- NA_character_

  if ("logos" %in% names(data) && length(data[["logos"]]) > 0) {
    for (logo in data[["logos"]]) {
      if ("href" %in% names(logo)) {
        if ("rel" %in% names(logo) && length(logo[["rel"]]) > 0) {
          rel_types <- logo[["rel"]]
          if ("dark" %in% rel_types) {
            team_logo_dark <- logo[["href"]]
          } else {
            # Default logo (non-dark)
            if (is.na(team_logo)) {
              team_logo <- logo[["href"]]
            }
          }
        } else {
          # If no rel specified, use as default
          if (is.na(team_logo)) {
            team_logo <- logo[["href"]]
          }
        }
      }
    }
  }

  # Reference links for additional data
  team_ref <- if ("$ref" %in% names(data)) data[["$ref"]] else NA_character_
  venue_ref <- if ("venue" %in% names(data) && "$ref" %in% names(data[["venue"]]))
    data[["venue"]][["$ref"]] else NA_character_
  groups_ref <- if ("groups" %in% names(data) && "$ref" %in% names(data[["groups"]]))
    data[["groups"]][["$ref"]] else NA_character_
  record_ref <- if ("record" %in% names(data) && "$ref" %in% names(data[["record"]]))
    data[["record"]][["$ref"]] else NA_character_
  athletes_ref <- if ("athletes" %in% names(data) && "$ref" %in% names(data[["athletes"]]))
    data[["athletes"]][["$ref"]] else NA_character_
  events_ref <- if ("events" %in% names(data) && "$ref" %in% names(data[["events"]]))
    data[["events"]][["$ref"]] else NA_character_

  # Create result row
  result_row <- data.frame(
    request_team_id = team_id,
    request_year = year,
    team_id = team_id_api,
    team_uid = team_uid,
    team_slug = team_slug,
    team_abbreviation = team_abbreviation,
    team_display_name = team_display_name,
    team_short_display_name = team_short_display_name,
    team_name = team_name,
    team_nickname = team_nickname,
    team_location = team_location,
    team_color = team_color,
    team_alternate_color = team_alternate_color,
    is_active = is_active,
    is_all_star = is_all_star,
    season_year = season_year_api,
    season_type = season_type,
    season_slug = season_slug,
    team_logo = team_logo,
    team_logo_dark = team_logo_dark,
    team_ref = team_ref,
    venue_ref = venue_ref,
    groups_ref = groups_ref,
    record_ref = record_ref,
    athletes_ref = athletes_ref,
    events_ref = events_ref,
    stringsAsFactors = FALSE
  )

  # Add to result
  result_df <- rbind(result_df, result_row)

  # Clean up row names
  if (nrow(result_df) > 0) {
    rownames(result_df) <- NULL
  }

  return(result_df)
}
