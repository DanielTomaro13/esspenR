#' Safe nested data extraction helper function for detailed team data
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_detailed <- function(data, path, default = NA) {
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

#' Create detailed college football team data frame from Site API response
#'
#' Processes raw JSON response from ESPN Site API into structured data frame
#' containing comprehensive team information
#'
#' @param data Raw JSON response from ESPN Site API team endpoint
#' @param team_identifier Character. Team identifier used in request
#' @return Data frame with detailed team information
#' @keywords internal
create_cfb_detailed_team_dataset <- function(data, team_identifier) {

  # Initialize detailed team data frame
  detailed_team_df <- data.frame(
    query_team_identifier = character(0),
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
    team_is_active = character(0),
    team_is_all_star = character(0),
    team_logo_href = character(0),
    team_logo_alt = character(0),
    team_logo_width = character(0),
    team_logo_height = character(0),
    team_logo_dark_href = character(0),
    venue_id = character(0),
    venue_name = character(0),
    venue_full_name = character(0),
    venue_capacity = character(0),
    venue_grass = character(0),
    venue_city = character(0),
    venue_state = character(0),
    venue_zip_code = character(0),
    venue_country_code = character(0),
    venue_indoor = character(0),
    venue_dome = character(0),
    conference_id = character(0),
    conference_name = character(0),
    conference_short_name = character(0),
    conference_uid = character(0),
    conference_abbreviation = character(0),
    group_id = character(0),
    group_name = character(0),
    group_short_name = character(0),
    group_parent_group_id = character(0),
    current_season_year = character(0),
    current_season_type = character(0),
    current_season_name = character(0),
    record_overall_wins = character(0),
    record_overall_losses = character(0),
    record_overall_ties = character(0),
    record_overall_percentage = character(0),
    record_overall_summary = character(0),
    record_home_wins = character(0),
    record_home_losses = character(0),
    record_home_summary = character(0),
    record_away_wins = character(0),
    record_away_losses = character(0),
    record_away_summary = character(0),
    standing_conference_rank = character(0),
    standing_conference_games_played = character(0),
    standing_points_for = character(0),
    standing_points_against = character(0),
    standing_point_differential = character(0),
    stringsAsFactors = FALSE
  )

  # Navigate to team data - handle different possible structures
  team_data <- NULL

  # Try multiple paths to find team data
  if ("team" %in% names(data)) {
    team_data <- data$team
  } else if ("teams" %in% names(data) && length(data$teams) > 0) {
    team_data <- data$teams[[1]]
    if ("team" %in% names(team_data)) {
      team_data <- team_data$team
    }
  } else {
    # Try sports navigation
    if ("sports" %in% names(data)) {
      sports <- data$sports
      if (length(sports) > 0) {
        sport <- sports[[1]]
        if ("leagues" %in% names(sport)) {
          leagues <- sport$leagues
          if (length(leagues) > 0) {
            league <- leagues[[1]]
            if ("teams" %in% names(league) && length(league$teams) > 0) {
              team_data <- league$teams[[1]]
              if ("team" %in% names(team_data)) {
                team_data <- team_data$team
              }
            }
          }
        }
      }
    }
  }

  if (is.null(team_data)) {
    warning("No team data found in API response")
    return(detailed_team_df)
  }

  # Basic team information
  team_id <- extract_nested_detailed(team_data, c("id"), NA_character_)
  team_uid <- extract_nested_detailed(team_data, c("uid"), NA_character_)
  team_slug <- extract_nested_detailed(team_data, c("slug"), NA_character_)
  team_abbreviation <- extract_nested_detailed(team_data, c("abbreviation"), NA_character_)
  team_display_name <- extract_nested_detailed(team_data, c("displayName"), NA_character_)
  team_short_display_name <- extract_nested_detailed(team_data, c("shortDisplayName"), NA_character_)
  team_name <- extract_nested_detailed(team_data, c("name"), NA_character_)
  team_nickname <- extract_nested_detailed(team_data, c("nickname"), NA_character_)
  team_location <- extract_nested_detailed(team_data, c("location"), NA_character_)
  team_color <- extract_nested_detailed(team_data, c("color"), NA_character_)
  team_alternate_color <- extract_nested_detailed(team_data, c("alternateColor"), NA_character_)
  team_is_active <- extract_nested_detailed(team_data, c("isActive"), "true")
  team_is_all_star <- extract_nested_detailed(team_data, c("isAllStar"), "false")

  # Logo information - get both regular and dark logos
  team_logo_href <- NA_character_
  team_logo_alt <- NA_character_
  team_logo_width <- NA_character_
  team_logo_height <- NA_character_
  team_logo_dark_href <- NA_character_

  logos <- extract_nested_detailed(team_data, c("logos"), list())
  if (length(logos) > 0) {
    for (logo in logos) {
      logo_rel <- extract_nested_detailed(logo, c("rel"), list())
      if (length(logo_rel) > 0) {
        if ("default" %in% logo_rel || "full" %in% logo_rel) {
          team_logo_href <- extract_nested_detailed(logo, c("href"), team_logo_href)
          team_logo_alt <- extract_nested_detailed(logo, c("alt"), team_logo_alt)
          team_logo_width <- extract_nested_detailed(logo, c("width"), team_logo_width)
          team_logo_height <- extract_nested_detailed(logo, c("height"), team_logo_height)
        } else if ("dark" %in% logo_rel) {
          team_logo_dark_href <- extract_nested_detailed(logo, c("href"), team_logo_dark_href)
        }
      }
    }
  }

  # Venue information
  venue_id <- NA_character_
  venue_name <- NA_character_
  venue_full_name <- NA_character_
  venue_capacity <- NA_character_
  venue_grass <- NA_character_
  venue_city <- NA_character_
  venue_state <- NA_character_
  venue_zip_code <- NA_character_
  venue_country_code <- NA_character_
  venue_indoor <- NA_character_
  venue_dome <- NA_character_

  venue_info <- extract_nested_detailed(team_data, c("venue"), list())
  if (length(venue_info) > 0) {
    venue_id <- extract_nested_detailed(venue_info, c("id"), NA_character_)
    venue_name <- extract_nested_detailed(venue_info, c("shortName"), NA_character_)
    venue_full_name <- extract_nested_detailed(venue_info, c("fullName"), NA_character_)
    if (is.na(venue_name)) {
      venue_name <- extract_nested_detailed(venue_info, c("name"), NA_character_)
    }
    venue_capacity <- extract_nested_detailed(venue_info, c("capacity"), NA_character_)
    venue_grass <- extract_nested_detailed(venue_info, c("grass"), NA_character_)
    venue_indoor <- extract_nested_detailed(venue_info, c("indoor"), "false")
    venue_dome <- extract_nested_detailed(venue_info, c("dome"), "false")

    # Address information
    address_info <- extract_nested_detailed(venue_info, c("address"), list())
    if (length(address_info) > 0) {
      venue_city <- extract_nested_detailed(address_info, c("city"), NA_character_)
      venue_state <- extract_nested_detailed(address_info, c("state"), NA_character_)
      venue_zip_code <- extract_nested_detailed(address_info, c("zipCode"), NA_character_)
      venue_country_code <- extract_nested_detailed(address_info, c("countryCode"), NA_character_)
    }
  }

  # Conference information
  conference_id <- NA_character_
  conference_name <- NA_character_
  conference_short_name <- NA_character_
  conference_uid <- NA_character_
  conference_abbreviation <- NA_character_

  # Try different paths for conference
  conference_info <- extract_nested_detailed(team_data, c("conference"), list())
  if (length(conference_info) > 0) {
    conference_id <- extract_nested_detailed(conference_info, c("id"), NA_character_)
    conference_name <- extract_nested_detailed(conference_info, c("name"), NA_character_)
    conference_short_name <- extract_nested_detailed(conference_info, c("shortName"), NA_character_)
    conference_uid <- extract_nested_detailed(conference_info, c("uid"), NA_character_)
    conference_abbreviation <- extract_nested_detailed(conference_info, c("abbreviation"), NA_character_)
  } else {
    # Try conferences array
    conferences <- extract_nested_detailed(team_data, c("conferences"), list())
    if (length(conferences) > 0) {
      first_conf <- conferences[[1]]
      conference_id <- extract_nested_detailed(first_conf, c("id"), NA_character_)
      conference_name <- extract_nested_detailed(first_conf, c("name"), NA_character_)
      conference_short_name <- extract_nested_detailed(first_conf, c("shortName"), NA_character_)
      conference_uid <- extract_nested_detailed(first_conf, c("uid"), NA_character_)
      conference_abbreviation <- extract_nested_detailed(first_conf, c("abbreviation"), NA_character_)
    }
  }

  # Group information
  group_id <- NA_character_
  group_name <- NA_character_
  group_short_name <- NA_character_
  group_parent_group_id <- NA_character_

  groups <- extract_nested_detailed(team_data, c("groups"), list())
  if (length(groups) > 0) {
    # Find the main conference group
    for (group in groups) {
      is_conf_group <- extract_nested_detailed(group, c("isConferenceGroup"), "false")
      if (is_conf_group == "true" || is.na(group_id)) {
        group_id <- extract_nested_detailed(group, c("id"), NA_character_)
        group_name <- extract_nested_detailed(group, c("name"), NA_character_)
        group_short_name <- extract_nested_detailed(group, c("shortName"), NA_character_)
        group_parent_group_id <- extract_nested_detailed(group, c("parentGroupId"), NA_character_)

        if (is_conf_group == "true") break
      }
    }
  }

  # Season information
  current_season_year <- NA_character_
  current_season_type <- NA_character_
  current_season_name <- NA_character_

  season_info <- extract_nested_detailed(team_data, c("season"), list())
  if (length(season_info) > 0) {
    current_season_year <- extract_nested_detailed(season_info, c("year"), NA_character_)
    current_season_type <- extract_nested_detailed(season_info, c("type"), NA_character_)
    current_season_name <- extract_nested_detailed(season_info, c("name"), NA_character_)
  }

  # Record information
  record_overall_wins <- NA_character_
  record_overall_losses <- NA_character_
  record_overall_ties <- NA_character_
  record_overall_percentage <- NA_character_
  record_overall_summary <- NA_character_
  record_home_wins <- NA_character_
  record_home_losses <- NA_character_
  record_home_summary <- NA_character_
  record_away_wins <- NA_character_
  record_away_losses <- NA_character_
  record_away_summary <- NA_character_

  # Try different paths for records
  records <- extract_nested_detailed(team_data, c("record"), list())
  if (length(records) == 0) {
    records <- extract_nested_detailed(team_data, c("records"), list())
  }

  if (length(records) > 0) {
    # Handle both single record object and array of records
    if ("items" %in% names(records)) {
      records <- records$items
    }

    if (is.list(records) && !is.null(names(records)) && "wins" %in% names(records)) {
      # Single record object
      record_overall_wins <- extract_nested_detailed(records, c("wins"), NA_character_)
      record_overall_losses <- extract_nested_detailed(records, c("losses"), NA_character_)
      record_overall_ties <- extract_nested_detailed(records, c("ties"), NA_character_)
      record_overall_percentage <- extract_nested_detailed(records, c("winPercentage"), NA_character_)
      record_overall_summary <- extract_nested_detailed(records, c("summary"), NA_character_)
    } else if (is.list(records)) {
      # Array of records
      for (record in records) {
        record_type <- extract_nested_detailed(record, c("type"), NA_character_)
        if (!is.na(record_type)) {
          if (record_type == "total" || record_type == "overall") {
            record_overall_wins <- extract_nested_detailed(record, c("wins"), NA_character_)
            record_overall_losses <- extract_nested_detailed(record, c("losses"), NA_character_)
            record_overall_ties <- extract_nested_detailed(record, c("ties"), NA_character_)
            record_overall_percentage <- extract_nested_detailed(record, c("winPercentage"), NA_character_)
            record_overall_summary <- extract_nested_detailed(record, c("summary"), NA_character_)
          } else if (record_type == "home") {
            record_home_wins <- extract_nested_detailed(record, c("wins"), NA_character_)
            record_home_losses <- extract_nested_detailed(record, c("losses"), NA_character_)
            record_home_summary <- extract_nested_detailed(record, c("summary"), NA_character_)
          } else if (record_type == "road" || record_type == "away") {
            record_away_wins <- extract_nested_detailed(record, c("wins"), NA_character_)
            record_away_losses <- extract_nested_detailed(record, c("losses"), NA_character_)
            record_away_summary <- extract_nested_detailed(record, c("summary"), NA_character_)
          }
        }
      }
    }
  }

  # Standing information
  standing_conference_rank <- NA_character_
  standing_conference_games_played <- NA_character_
  standing_points_for <- NA_character_
  standing_points_against <- NA_character_
  standing_point_differential <- NA_character_

  standings <- extract_nested_detailed(team_data, c("standings"), list())
  if (length(standings) > 0) {
    # Handle different standings structures
    if ("entries" %in% names(standings)) {
      standings_entries <- standings$entries
      if (length(standings_entries) > 0) {
        entry <- standings_entries[[1]]
        standing_conference_rank <- extract_nested_detailed(entry, c("stats", "rank"), NA_character_)
        standing_conference_games_played <- extract_nested_detailed(entry, c("stats", "gamesPlayed"), NA_character_)
        standing_points_for <- extract_nested_detailed(entry, c("stats", "pointsFor"), NA_character_)
        standing_points_against <- extract_nested_detailed(entry, c("stats", "pointsAgainst"), NA_character_)
        standing_point_differential <- extract_nested_detailed(entry, c("stats", "pointDifferential"), NA_character_)
      }
    }
  }

  # Create row
  team_row <- data.frame(
    query_team_identifier = as.character(team_identifier),
    team_id = as.character(team_id),
    team_uid = as.character(team_uid),
    team_slug = as.character(team_slug),
    team_abbreviation = as.character(team_abbreviation),
    team_display_name = as.character(team_display_name),
    team_short_display_name = as.character(team_short_display_name),
    team_name = as.character(team_name),
    team_nickname = as.character(team_nickname),
    team_location = as.character(team_location),
    team_color = as.character(team_color),
    team_alternate_color = as.character(team_alternate_color),
    team_is_active = as.character(team_is_active),
    team_is_all_star = as.character(team_is_all_star),
    team_logo_href = as.character(team_logo_href),
    team_logo_alt = as.character(team_logo_alt),
    team_logo_width = as.character(team_logo_width),
    team_logo_height = as.character(team_logo_height),
    team_logo_dark_href = as.character(team_logo_dark_href),
    venue_id = as.character(venue_id),
    venue_name = as.character(venue_name),
    venue_full_name = as.character(venue_full_name),
    venue_capacity = as.character(venue_capacity),
    venue_grass = as.character(venue_grass),
    venue_city = as.character(venue_city),
    venue_state = as.character(venue_state),
    venue_zip_code = as.character(venue_zip_code),
    venue_country_code = as.character(venue_country_code),
    venue_indoor = as.character(venue_indoor),
    venue_dome = as.character(venue_dome),
    conference_id = as.character(conference_id),
    conference_name = as.character(conference_name),
    conference_short_name = as.character(conference_short_name),
    conference_uid = as.character(conference_uid),
    conference_abbreviation = as.character(conference_abbreviation),
    group_id = as.character(group_id),
    group_name = as.character(group_name),
    group_short_name = as.character(group_short_name),
    group_parent_group_id = as.character(group_parent_group_id),
    current_season_year = as.character(current_season_year),
    current_season_type = as.character(current_season_type),
    current_season_name = as.character(current_season_name),
    record_overall_wins = as.character(record_overall_wins),
    record_overall_losses = as.character(record_overall_losses),
    record_overall_ties = as.character(record_overall_ties),
    record_overall_percentage = as.character(record_overall_percentage),
    record_overall_summary = as.character(record_overall_summary),
    record_home_wins = as.character(record_home_wins),
    record_home_losses = as.character(record_home_losses),
    record_home_summary = as.character(record_home_summary),
    record_away_wins = as.character(record_away_wins),
    record_away_losses = as.character(record_away_losses),
    record_away_summary = as.character(record_away_summary),
    standing_conference_rank = as.character(standing_conference_rank),
    standing_conference_games_played = as.character(standing_conference_games_played),
    standing_points_for = as.character(standing_points_for),
    standing_points_against = as.character(standing_points_against),
    standing_point_differential = as.character(standing_point_differential),
    stringsAsFactors = FALSE
  )

  detailed_team_df <- rbind(detailed_team_df, team_row)

  # Clean up row names
  if (nrow(detailed_team_df) > 0) rownames(detailed_team_df) <- NULL

  return(detailed_team_df)
}

#' Fetch detailed college football team information using Site API
#'
#' Retrieves comprehensive information for a specific college football team from ESPN's Site API.
#' The function fetches detailed team data including venue, conference, records, standings,
#' and complete team identity information.
#'
#' @param team Character. Team identifier - can be team abbreviation (e.g., "GT", "UGA")
#'   or team ID (e.g., "61", "52"). Team abbreviations are preferred.
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'cfb_team_detailed_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data frame. The main purpose is global
#'   environment assignment of \code{cfb_team_detailed} containing:
#'   \itemize{
#'     \item Team identity: ID, UID, slug, abbreviation, names, nickname, location
#'     \item Team branding: colors, logos (regular and dark versions)
#'     \item Team status: active status, all-star designation
#'     \item Venue details: stadium name, capacity, surface, location, indoor/dome status
#'     \item Conference info: conference ID, name, abbreviation, group classification
#'     \item Season data: current season year, type, name
#'     \item Records: overall, home, away win-loss records with percentages
#'     \item Standings: conference rank, games played, points for/against
#'   }
#'
#' @details
#' The function creates a comprehensive data frame with detailed college football team information.
#' This provides much more detail than the basic teams list, including current season
#' performance data, complete venue information, and conference standings.
#'
#' **Team Identity**:
#' \itemize{
#'   \item Complete naming: display name, short name, nickname, location
#'   \item Identifiers: team ID (for API calls), UID, slug, abbreviation
#'   \item Status: active status, special designations
#' }
#'
#' **Team Branding**:
#' \itemize{
#'   \item Colors: primary and alternate team colors (hex codes)
#'   \item Logos: URLs for regular and dark theme logos with dimensions
#' }
#'
#' **Venue Information**:
#' \itemize{
#'   \item Stadium: name, full name, capacity, playing surface
#'   \item Location: city, state, zip code, country code
#'   \item Features: indoor/outdoor, dome designation
#' }
#'
#' **Conference & Groups**:
#' \itemize{
#'   \item Conference: official conference affiliation with full details
#'   \item Groups: ESPN's conference grouping system with parent relationships
#' }
#'
#' **Performance Data**:
#' \itemize{
#'   \item Records: overall, home, away records with win percentages
#'   \item Standings: conference rank, points scored/allowed
#'   \item Season: current season information
#' }
#'
#' @examples
#' \dontrun{
#' # Get Georgia Tech detailed information
#' fetch_college_football_team_detailed("GT")
#'
#' # Get team by ID
#' fetch_college_football_team_detailed("61")
#'
#' # Get multiple teams (call individually)
#' teams_to_fetch <- c("GT", "UGA", "FSU", "MIA")
#' for(team in teams_to_fetch) {
#'   fetch_college_football_team_detailed(team)
#'   # Process data here or combine later
#' }
#'
#' # Check the detailed data
#' head(cfb_team_detailed)
#'
#' # View team identity and branding
#' team_identity <- cfb_team_detailed[, c("team_abbreviation", "team_display_name",
#'                                       "team_location", "team_color", "team_alternate_color")]
#' print("Team identity:")
#' print(team_identity)
#'
#' # View venue information
#' venue_info <- cfb_team_detailed[, c("venue_name", "venue_capacity", "venue_city",
#'                                    "venue_state", "venue_indoor", "venue_grass")]
#' print("Venue information:")
#' print(venue_info)
#'
#' # View conference and group information
#' conf_info <- cfb_team_detailed[, c("conference_name", "conference_short_name",
#'                                   "group_name", "group_short_name")]
#' print("Conference information:")
#' print(conf_info)
#'
#' # View current season records
#' record_info <- cfb_team_detailed[, c("current_season_year", "record_overall_summary",
#'                                     "record_home_summary", "record_away_summary",
#'                                     "record_overall_percentage")]
#' print("Season records:")
#' print(record_info)
#'
#' # View standings information
#' standing_info <- cfb_team_detailed[, c("standing_conference_rank", "standing_points_for",
#'                                       "standing_points_against", "standing_point_differential")]
#' print("Conference standings:")
#' print(standing_info)
#'
#' # Check venue capacity and features
#' if(!is.na(cfb_team_detailed$venue_capacity)) {
#'   capacity <- as.numeric(cfb_team_detailed$venue_capacity)
#'   indoor_status <- ifelse(cfb_team_detailed$venue_indoor == "true", "Indoor", "Outdoor")
#'   dome_status <- ifelse(cfb_team_detailed$venue_dome == "true", "Dome", "Open")
#'
#'   cat(sprintf("Stadium: %s\n", cfb_team_detailed$venue_full_name))
#'   cat(sprintf("Capacity: %s\n", format(capacity, big.mark = ",")))
#'   cat(sprintf("Type: %s %s\n", indoor_status, dome_status))
#' }
#'
#' # Analyze team colors
#' if(!is.na(cfb_team_detailed$team_color)) {
#'   cat(sprintf("Primary color: #%s\n", cfb_team_detailed$team_color))
#'   if(!is.na(cfb_team_detailed$team_alternate_color)) {
#'     cat(sprintf("Secondary color: #%s\n", cfb_team_detailed$team_alternate_color))
#'   }
#' }
#' }
#'
#' @seealso \code{\link{fetch_college_football_teams}} for basic team list
#'
#' @export
fetch_college_football_team_detailed <- function(team, raw = FALSE) {

  # Input validation
  if (missing(team) || is.null(team) || team == "") {
    stop("'team' parameter is required. Provide team abbreviation (e.g., 'GT') or team ID.")
  }

  team <- as.character(team)

  # Build API URL
  url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/football/college-football/teams/%s", team)

  # Fetch and parse data
  tryCatch({
    message(sprintf("Fetching detailed information for team: %s", team))

    resp <- httr::GET(url, httr::timeout(30))

    if (httr::status_code(resp) != 200) {
      stop(sprintf("API request failed: HTTP %d - %s. Team '%s' may not exist or be invalid.",
                   httr::status_code(resp),
                   httr::http_status(resp)$message,
                   team))
    }

    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content_text, simplifyVector = FALSE, simplifyDataFrame = FALSE)

    # Handle raw data assignment for debugging
    if (isTRUE(raw)) {
      assign("cfb_team_detailed_raw", data, envir = .GlobalEnv)
      message("Raw detailed team data assigned to: cfb_team_detailed_raw")
      message("Data structure preview:")

      # Show available sections
      sections <- names(data)
      message("- Available sections: ", paste(sections, collapse = ", "))

      # Try to find team data and show its structure
      team_data <- NULL
      if ("team" %in% names(data)) {
        team_data <- data$team
        message("- Found team data directly")
      } else if ("teams" %in% names(data)) {
        message("- Found teams array")
      }

      if (!is.null(team_data)) {
        team_sections <- names(team_data)
        message("- Team data sections: ", paste(team_sections, collapse = ", "))

        # Show key information
        team_name <- extract_nested_detailed(team_data, c("displayName"), "Unknown")
        team_conf <- extract_nested_detailed(team_data, c("conference", "name"), "Unknown")
        venue_name <- extract_nested_detailed(team_data, c("venue", "fullName"), "Unknown")

        message(sprintf("- Team: %s", team_name))
        message(sprintf("- Conference: %s", team_conf))
        message(sprintf("- Venue: %s", venue_name))
      }

      return(invisible(data))
    }

    # Create detailed team dataset
    team_detailed_df <- create_cfb_detailed_team_dataset(data, team)

    # Assign to global environment
    assign("cfb_team_detailed", team_detailed_df, envir = .GlobalEnv)

    # Summary message
    if (nrow(team_detailed_df) > 0) {
      team_info <- team_detailed_df[1, ]

      message(sprintf("Detailed team information assigned to: cfb_team_detailed"))
      message(sprintf("Team: %s (%s)", team_info$team_display_name, team_info$team_abbreviation))

      # Show conference information
      if (!is.na(team_info$conference_name) && team_info$conference_name != "") {
        message(sprintf("Conference: %s", team_info$conference_name))
      }

      # Show venue information
      if (!is.na(team_info$venue_full_name) && team_info$venue_full_name != "") {
        venue_details <- team_info$venue_full_name
        if (!is.na(team_info$venue_capacity) && team_info$venue_capacity != "") {
          venue_details <- paste0(venue_details, " (", format(as.numeric(team_info$venue_capacity), big.mark = ","), ")")
        }
        message(sprintf("Venue: %s", venue_details))
      }

      # Show current season record
      if (!is.na(team_info$record_overall_summary) && team_info$record_overall_summary != "") {
        message(sprintf("Record: %s", team_info$record_overall_summary))

        if (!is.na(team_info$record_overall_percentage) && team_info$record_overall_percentage != "") {
          win_pct <- round(as.numeric(team_info$record_overall_percentage) * 100, 1)
          message(sprintf("Win percentage: %.1f%%", win_pct))
        }
      }

      # Show conference standing
      if (!is.na(team_info$standing_conference_rank) && team_info$standing_conference_rank != "") {
        rank_info <- paste("Conference rank:", team_info$standing_conference_rank)

        if (!is.na(team_info$standing_points_for) && team_info$standing_points_for != "" &&
            !is.na(team_info$standing_points_against) && team_info$standing_points_against != "") {
          rank_info <- paste0(rank_info, sprintf(" (PF: %s, PA: %s)",
                                                 team_info$standing_points_for,
                                                 team_info$standing_points_against))
        }
        message(rank_info)
      }

      # Show data completeness
      non_na_fields <- sum(!is.na(team_info) & team_info != "")
      total_fields <- ncol(team_info)
      completeness <- round((non_na_fields / total_fields) * 100, 1)
      message(sprintf("Data completeness: %d/%d fields (%.1f%%)", non_na_fields, total_fields, completeness))

    } else {
      message("No team data could be extracted from API response")
    }

    return(invisible(team_detailed_df))

  }, error = function(e) {
    stop(sprintf("Failed to fetch detailed team information for '%s': %s", team, e$message))
  })
}

#' Fetch detailed information for multiple college football teams
#'
#' Retrieves comprehensive information for multiple college football teams with rate limiting.
#' This function calls \code{\link{fetch_college_football_team_detailed}} for each team
#' and combines the results.
#'
#' @param teams Character vector. Team identifiers - can be team abbreviations or IDs.
#' @param delay Numeric. Delay in seconds between API requests (default: 0.5).
#'   Used to be respectful to ESPN's servers.
#' @param raw Logical. If TRUE, assigns raw JSON to global environment
#'   for the first team only (default: FALSE).
#'
#' @return Invisibly returns the combined data frame. The main purpose is global
#'   environment assignment of combined \code{cfb_team_detailed} from all teams.
#'
#' @examples
#' \dontrun{
#' # Get detailed info for ACC Coastal teams
#' fetch_multiple_cfb_teams_detailed(c("GT", "UVA", "VT", "UNC", "DUKE", "PITT", "MIA"))
#'
#' # Get info for specific rivalry teams
#' fetch_multiple_cfb_teams_detailed(c("GT", "UGA", "CLEM", "FSU"))
#'
#' # Check combined results
#' head(cfb_team_detailed)
#'
#' # Compare venue capacities
#' venues <- cfb_team_detailed[, c("team_abbreviation", "venue_full_name", "venue_capacity")]
#' venues$capacity_num <- as.numeric(venues$venue_capacity)
#' venues <- venues[order(-venues$capacity_num, na.last = TRUE), ]
#' print("Venues by capacity:")
#' print(venues)
#'
#' # Compare records
#' records <- cfb_team_detailed[, c("team_abbreviation", "team_display_name",
#'                                 "record_overall_summary", "record_overall_percentage")]
#' records$win_pct <- as.numeric(records$record_overall_percentage) * 100
#' records <- records[order(-records$win_pct, na.last = TRUE), ]
#' print("Teams by win percentage:")
#' print(records)
#' }
#'
#' @seealso \code{\link{fetch_college_football_team_detailed}} for single team data
#' @export
fetch_multiple_cfb_teams_detailed <- function(teams, delay = 0.5, raw = FALSE) {
  # Input validation
  if (length(teams) == 0) {
    stop("'teams' must contain at least one team")
  }

  if (!is.numeric(delay) || delay < 0) {
    stop("'delay' must be a non-negative numeric value")
  }

  # Initialize combined data container
  all_teams <- data.frame()

  message(sprintf("Starting to fetch detailed information for %d teams...", length(teams)))

  # Process each team sequentially
  for (i in seq_along(teams)) {
    team <- teams[i]
    message(sprintf("Fetching team %s (%d/%d)...", team, i, length(teams)))

    tryCatch({
      # Fetch individual team data
      team_data <- fetch_college_football_team_detailed(
        team = team,
        raw = raw
      )

      # If raw data requested, return after first team
      if (isTRUE(raw)) {
        return(invisible(team_data))
      }

      # Combine data
      if (exists("cfb_team_detailed", envir = .GlobalEnv)) {
        team_df <- get("cfb_team_detailed", envir = .GlobalEnv)
        all_teams <- rbind(all_teams, team_df)
      }

    }, error = function(e) {
      message(sprintf("Failed to fetch team %s: %s", team, e$message))
    })

    # Be respectful to the API with delay between requests
    if (i < length(teams)) {
      Sys.sleep(delay)
    }
  }

  # Assign combined dataset to global environment
  if (nrow(all_teams) > 0) {
    # Remove duplicates based on team ID
    all_teams <- all_teams[!duplicated(all_teams$team_id), ]
    assign("cfb_team_detailed", all_teams, envir = .GlobalEnv)

    total_teams <- nrow(all_teams)

    message(sprintf("Combined detailed team information assigned to: cfb_team_detailed (%d teams)", total_teams))

    # Show summary statistics
    teams_with_venue <- sum(!is.na(all_teams$venue_full_name) & all_teams$venue_full_name != "")
    teams_with_conference <- sum(!is.na(all_teams$conference_name) & all_teams$conference_name != "")
    teams_with_records <- sum(!is.na(all_teams$record_overall_summary) & all_teams$record_overall_summary != "")

    message(sprintf("  - Teams with venue data: %d", teams_with_venue))
    message(sprintf("  - Teams with conference data: %d", teams_with_conference))
    message(sprintf("  - Teams with current records: %d", teams_with_records))

    # Show conferences represented
    if (teams_with_conference > 0) {
      conferences <- unique(all_teams$conference_name[!is.na(all_teams$conference_name) & all_teams$conference_name != ""])
      message(sprintf("  - Conferences represented: %s", paste(conferences, collapse = ", ")))
    }

    # Show average data completeness
    completeness_scores <- apply(all_teams, 1, function(row) {
      sum(!is.na(row) & row != "") / length(row)
    })
    avg_completeness <- round(mean(completeness_scores) * 100, 1)
    message(sprintf("  - Average data completeness: %.1f%%", avg_completeness))
  }

  return(invisible(all_teams))
}
