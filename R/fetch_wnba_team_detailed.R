#' Safe nested data extraction helper function for detailed wnba team data
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_wnba_detailed <- function(data, path, default = NA) {
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

#' Create detailed wnba team data frame from Site API response
#'
#' Processes raw JSON response from ESPN Site API into structured data frame
#' containing comprehensive wnba team information
#'
#' @param data Raw JSON response from ESPN Site API team endpoint
#' @param team_identifier Character. Team identifier used in request
#' @return Data frame with detailed team information
#' @keywords internal
create_wnba_detailed_team_dataset <- function(data, team_identifier) {

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
    division_id = character(0),
    division_name = character(0),
    division_short_name = character(0),
    division_uid = character(0),
    division_abbreviation = character(0),
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
    record_conference_wins = character(0),
    record_conference_losses = character(0),
    record_conference_summary = character(0),
    standing_conference_rank = character(0),
    standing_division_rank = character(0),
    standing_league_rank = character(0),
    standing_games_played = character(0),
    standing_games_behind = character(0),
    standing_wins = character(0),
    standing_losses = character(0),
    standing_win_percentage = character(0),
    standing_playoff_seed = character(0),
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
  team_id <- extract_nested_wnba_detailed(team_data, c("id"), NA_character_)
  team_uid <- extract_nested_wnba_detailed(team_data, c("uid"), NA_character_)
  team_slug <- extract_nested_wnba_detailed(team_data, c("slug"), NA_character_)
  team_abbreviation <- extract_nested_wnba_detailed(team_data, c("abbreviation"), NA_character_)
  team_display_name <- extract_nested_wnba_detailed(team_data, c("displayName"), NA_character_)
  team_short_display_name <- extract_nested_wnba_detailed(team_data, c("shortDisplayName"), NA_character_)
  team_name <- extract_nested_wnba_detailed(team_data, c("name"), NA_character_)
  team_nickname <- extract_nested_wnba_detailed(team_data, c("nickname"), NA_character_)
  team_location <- extract_nested_wnba_detailed(team_data, c("location"), NA_character_)
  team_color <- extract_nested_wnba_detailed(team_data, c("color"), NA_character_)
  team_alternate_color <- extract_nested_wnba_detailed(team_data, c("alternateColor"), NA_character_)
  team_is_active <- extract_nested_wnba_detailed(team_data, c("isActive"), "true")
  team_is_all_star <- extract_nested_wnba_detailed(team_data, c("isAllStar"), "false")

  # Logo information - get both regular and dark logos
  team_logo_href <- NA_character_
  team_logo_alt <- NA_character_
  team_logo_width <- NA_character_
  team_logo_height <- NA_character_
  team_logo_dark_href <- NA_character_

  logos <- extract_nested_wnba_detailed(team_data, c("logos"), list())
  if (length(logos) > 0) {
    for (logo in logos) {
      logo_rel <- extract_nested_wnba_detailed(logo, c("rel"), list())
      if (length(logo_rel) > 0) {
        if ("default" %in% logo_rel || "full" %in% logo_rel) {
          team_logo_href <- extract_nested_wnba_detailed(logo, c("href"), team_logo_href)
          team_logo_alt <- extract_nested_wnba_detailed(logo, c("alt"), team_logo_alt)
          team_logo_width <- extract_nested_wnba_detailed(logo, c("width"), team_logo_width)
          team_logo_height <- extract_nested_wnba_detailed(logo, c("height"), team_logo_height)
        } else if ("dark" %in% logo_rel) {
          team_logo_dark_href <- extract_nested_wnba_detailed(logo, c("href"), team_logo_dark_href)
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

  venue_info <- extract_nested_wnba_detailed(team_data, c("venue"), list())
  if (length(venue_info) > 0) {
    venue_id <- extract_nested_wnba_detailed(venue_info, c("id"), NA_character_)
    venue_name <- extract_nested_wnba_detailed(venue_info, c("shortName"), NA_character_)
    venue_full_name <- extract_nested_wnba_detailed(venue_info, c("fullName"), NA_character_)
    if (is.na(venue_name)) {
      venue_name <- extract_nested_wnba_detailed(venue_info, c("name"), NA_character_)
    }
    venue_capacity <- extract_nested_wnba_detailed(venue_info, c("capacity"), NA_character_)
    venue_grass <- extract_nested_wnba_detailed(venue_info, c("grass"), NA_character_)
    venue_indoor <- extract_nested_wnba_detailed(venue_info, c("indoor"), "true")
    venue_dome <- extract_nested_wnba_detailed(venue_info, c("dome"), "false")

    # Address information
    address_info <- extract_nested_wnba_detailed(venue_info, c("address"), list())
    if (length(address_info) > 0) {
      venue_city <- extract_nested_wnba_detailed(address_info, c("city"), NA_character_)
      venue_state <- extract_nested_wnba_detailed(address_info, c("state"), NA_character_)
      venue_zip_code <- extract_nested_wnba_detailed(address_info, c("zipCode"), NA_character_)
      venue_country_code <- extract_nested_wnba_detailed(address_info, c("countryCode"), NA_character_)
    }
  }

  # Conference information
  conference_id <- NA_character_
  conference_name <- NA_character_
  conference_short_name <- NA_character_
  conference_uid <- NA_character_
  conference_abbreviation <- NA_character_

  # Try different paths for conference
  conference_info <- extract_nested_wnba_detailed(team_data, c("conference"), list())
  if (length(conference_info) > 0) {
    conference_id <- extract_nested_wnba_detailed(conference_info, c("id"), NA_character_)
    conference_name <- extract_nested_wnba_detailed(conference_info, c("name"), NA_character_)
    conference_short_name <- extract_nested_wnba_detailed(conference_info, c("shortName"), NA_character_)
    conference_uid <- extract_nested_wnba_detailed(conference_info, c("uid"), NA_character_)
    conference_abbreviation <- extract_nested_wnba_detailed(conference_info, c("abbreviation"), NA_character_)
  } else {
    # Try groups array for conference
    groups <- extract_nested_wnba_detailed(team_data, c("groups"), list())
    if (length(groups) > 0) {
      for (group in groups) {
        is_conf_group <- extract_nested_wnba_detailed(group, c("isConferenceGroup"), "false")
        if (is_conf_group == "true") {
          conference_id <- extract_nested_wnba_detailed(group, c("id"), NA_character_)
          conference_name <- extract_nested_wnba_detailed(group, c("name"), NA_character_)
          conference_short_name <- extract_nested_wnba_detailed(group, c("shortName"), NA_character_)
          conference_uid <- extract_nested_wnba_detailed(group, c("uid"), NA_character_)
          conference_abbreviation <- extract_nested_wnba_detailed(group, c("abbreviation"), NA_character_)
          break
        }
      }
    }
  }

  # Division information
  division_id <- NA_character_
  division_name <- NA_character_
  division_short_name <- NA_character_
  division_uid <- NA_character_
  division_abbreviation <- NA_character_

  # Try different paths for division
  division_info <- extract_nested_wnba_detailed(team_data, c("division"), list())
  if (length(division_info) > 0) {
    division_id <- extract_nested_wnba_detailed(division_info, c("id"), NA_character_)
    division_name <- extract_nested_wnba_detailed(division_info, c("name"), NA_character_)
    division_short_name <- extract_nested_wnba_detailed(division_info, c("shortName"), NA_character_)
    division_uid <- extract_nested_wnba_detailed(division_info, c("uid"), NA_character_)
    division_abbreviation <- extract_nested_wnba_detailed(division_info, c("abbreviation"), NA_character_)
  } else {
    # Try groups array for division
    groups <- extract_nested_wnba_detailed(team_data, c("groups"), list())
    if (length(groups) > 0) {
      for (group in groups) {
        is_conf_group <- extract_nested_wnba_detailed(group, c("isConferenceGroup"), "false")
        if (is_conf_group == "false") {
          division_id <- extract_nested_wnba_detailed(group, c("id"), NA_character_)
          division_name <- extract_nested_wnba_detailed(group, c("name"), NA_character_)
          division_short_name <- extract_nested_wnba_detailed(group, c("shortName"), NA_character_)
          division_uid <- extract_nested_wnba_detailed(group, c("uid"), NA_character_)
          division_abbreviation <- extract_nested_wnba_detailed(group, c("abbreviation"), NA_character_)
          break
        }
      }
    }
  }

  # Season information
  current_season_year <- NA_character_
  current_season_type <- NA_character_
  current_season_name <- NA_character_

  season_info <- extract_nested_wnba_detailed(team_data, c("season"), list())
  if (length(season_info) > 0) {
    current_season_year <- extract_nested_wnba_detailed(season_info, c("year"), NA_character_)
    current_season_type <- extract_nested_wnba_detailed(season_info, c("type"), NA_character_)
    current_season_name <- extract_nested_wnba_detailed(season_info, c("name"), NA_character_)
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
  record_conference_wins <- NA_character_
  record_conference_losses <- NA_character_
  record_conference_summary <- NA_character_

  # Try different paths for records
  records <- extract_nested_wnba_detailed(team_data, c("record"), list())
  if (length(records) == 0) {
    records <- extract_nested_wnba_detailed(team_data, c("records"), list())
  }

  if (length(records) > 0) {
    # Handle both single record object and array of records
    if ("items" %in% names(records)) {
      records <- records$items
    }

    if (is.list(records) && !is.null(names(records)) && "wins" %in% names(records)) {
      # Single record object
      record_overall_wins <- extract_nested_wnba_detailed(records, c("wins"), NA_character_)
      record_overall_losses <- extract_nested_wnba_detailed(records, c("losses"), NA_character_)
      record_overall_ties <- extract_nested_wnba_detailed(records, c("ties"), NA_character_)
      record_overall_percentage <- extract_nested_wnba_detailed(records, c("winPercentage"), NA_character_)
      record_overall_summary <- extract_nested_wnba_detailed(records, c("summary"), NA_character_)
    } else if (is.list(records)) {
      # Array of records
      for (record in records) {
        record_type <- extract_nested_wnba_detailed(record, c("type"), NA_character_)
        record_name <- extract_nested_wnba_detailed(record, c("name"), NA_character_)

        if (!is.na(record_type) || !is.na(record_name)) {
          if (record_type == "total" || record_name == "overall") {
            record_overall_wins <- extract_nested_wnba_detailed(record, c("wins"), NA_character_)
            record_overall_losses <- extract_nested_wnba_detailed(record, c("losses"), NA_character_)
            record_overall_ties <- extract_nested_wnba_detailed(record, c("ties"), NA_character_)
            record_overall_percentage <- extract_nested_wnba_detailed(record, c("winPercentage"), NA_character_)
            record_overall_summary <- extract_nested_wnba_detailed(record, c("summary"), NA_character_)
          } else if (record_type == "home" || record_name == "home") {
            record_home_wins <- extract_nested_wnba_detailed(record, c("wins"), NA_character_)
            record_home_losses <- extract_nested_wnba_detailed(record, c("losses"), NA_character_)
            record_home_summary <- extract_nested_wnba_detailed(record, c("summary"), NA_character_)
          } else if (record_type == "road" || record_name == "road" || record_name == "away") {
            record_away_wins <- extract_nested_wnba_detailed(record, c("wins"), NA_character_)
            record_away_losses <- extract_nested_wnba_detailed(record, c("losses"), NA_character_)
            record_away_summary <- extract_nested_wnba_detailed(record, c("summary"), NA_character_)
          } else if (record_type == "conference" || record_name == "conference") {
            record_conference_wins <- extract_nested_wnba_detailed(record, c("wins"), NA_character_)
            record_conference_losses <- extract_nested_wnba_detailed(record, c("losses"), NA_character_)
            record_conference_summary <- extract_nested_wnba_detailed(record, c("summary"), NA_character_)
          }
        }
      }
    }
  }

  # Standing information
  standing_conference_rank <- NA_character_
  standing_division_rank <- NA_character_
  standing_league_rank <- NA_character_
  standing_games_played <- NA_character_
  standing_games_behind <- NA_character_
  standing_wins <- NA_character_
  standing_losses <- NA_character_
  standing_win_percentage <- NA_character_
  standing_playoff_seed <- NA_character_

  standings <- extract_nested_wnba_detailed(team_data, c("standings"), list())
  if (length(standings) > 0) {
    # Handle different standings structures
    if ("entries" %in% names(standings)) {
      standings_entries <- standings$entries
      if (length(standings_entries) > 0) {
        entry <- standings_entries[[1]]
        standing_conference_rank <- extract_nested_wnba_detailed(entry, c("stats", "conferenceRank"), NA_character_)
        standing_division_rank <- extract_nested_wnba_detailed(entry, c("stats", "divisionRank"), NA_character_)
        standing_league_rank <- extract_nested_wnba_detailed(entry, c("stats", "leagueRank"), NA_character_)
        standing_games_played <- extract_nested_wnba_detailed(entry, c("stats", "gamesPlayed"), NA_character_)
        standing_games_behind <- extract_nested_wnba_detailed(entry, c("stats", "gamesBehind"), NA_character_)
        standing_wins <- extract_nested_wnba_detailed(entry, c("stats", "wins"), NA_character_)
        standing_losses <- extract_nested_wnba_detailed(entry, c("stats", "losses"), NA_character_)
        standing_win_percentage <- extract_nested_wnba_detailed(entry, c("stats", "winPercent"), NA_character_)
        standing_playoff_seed <- extract_nested_wnba_detailed(entry, c("stats", "playoffSeed"), NA_character_)
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
    division_id = as.character(division_id),
    division_name = as.character(division_name),
    division_short_name = as.character(division_short_name),
    division_uid = as.character(division_uid),
    division_abbreviation = as.character(division_abbreviation),
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
    record_conference_wins = as.character(record_conference_wins),
    record_conference_losses = as.character(record_conference_losses),
    record_conference_summary = as.character(record_conference_summary),
    standing_conference_rank = as.character(standing_conference_rank),
    standing_division_rank = as.character(standing_division_rank),
    standing_league_rank = as.character(standing_league_rank),
    standing_games_played = as.character(standing_games_played),
    standing_games_behind = as.character(standing_games_behind),
    standing_wins = as.character(standing_wins),
    standing_losses = as.character(standing_losses),
    standing_win_percentage = as.character(standing_win_percentage),
    standing_playoff_seed = as.character(standing_playoff_seed),
    stringsAsFactors = FALSE
  )

  detailed_team_df <- rbind(detailed_team_df, team_row)

  # Clean up row names
  if (nrow(detailed_team_df) > 0) rownames(detailed_team_df) <- NULL

  return(detailed_team_df)
}

#' Fetch detailed wnba team information using Site API
#'
#' Retrieves comprehensive information for a specific wnba team from ESPN's Site API.
#' The function fetches detailed team data including venue, conference, records, standings,
#' and complete team identity information.
#'
#' @param team Character. Team identifier - can be team abbreviation (e.g., "LAL", "BOS")
#'   or team ID (e.g., "13", "2"). Team abbreviations are preferred.
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'wnba_team_detailed_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data frame. The main purpose is global
#'   environment assignment of \code{wnba_team_detailed} containing:
#'   \itemize{
#'     \item Team identity: ID, UID, slug, abbreviation, names, nickname, location
#'     \item Team branding: colors, logos (regular and dark versions)
#'     \item Team status: active status, all-star designation
#'     \item Venue details: arena name, capacity, surface, location, indoor/dome status
#'     \item Conference info: conference ID, name, abbreviation
#'     \item Division info: division ID, name, abbreviation
#'     \item Season data: current season year, type, name
#'     \item Records: overall, home, away, conference win-loss records with percentages
#'     \item Standings: conference/division/league rank, games played, games behind, playoff seed
#'   }
#'
#' @details
#' The function creates a comprehensive data frame with detailed wnba team information.
#' This provides much more detail than the basic teams list, including current season
#' performance data, complete venue information, and standings.
#'
#' **Team Identity**:
#' \itemize{
#'   \item Complete naming: display name, short name, nickname, location
#'   \item Identifiers: team ID (for API calls), UID, slug, abbreviation
#'   \item Status: active status, special designations
#' }
#'
#' **Team Organization**:
#' \itemize{
#'   \item Conference: Eastern or Western Conference affiliation
#'   \item Division: Atlantic, Central, Southeast, Northwest, Pacific, Southwest
#'   \item Structure: hierarchical organization within wnba
#' }
#'
#' **Venue Information**:
#' \itemize{
#'   \item Arena: name, full name, capacity
#'   \item Location: city, state, zip code, country code
#'   \item Features: indoor/outdoor status, dome designation
#' }
#'
#' **Performance Data**:
#' \itemize{
#'   \item Records: overall, home, away, conference records with win percentages
#'   \item Standings: conference, division, and league rankings
#'   \item Season: current season information and playoff seeding
#' }
#'
#' @examples
#' \dontrun{
#' # Get Lakers detailed information
#' fetch_wnba_team_detailed("LAL")
#'
#' # Get team by ID
#' fetch_wnba_team_detailed("13")
#'
#' # Get multiple teams (call individually)
#' teams_to_fetch <- c("LAL", "BOS", "GSW", "MIA")
#' for(team in teams_to_fetch) {
#'   fetch_wnba_team_detailed(team)
#'   # Process data here or combine later
#' }
#'
#' # Check the detailed data
#' head(wnba_team_detailed)
#'
#' # View team identity and branding
#' team_identity <- wnba_team_detailed[, c("team_abbreviation", "team_display_name",
#'                                       "team_location", "team_color", "team_alternate_color")]
#' print("Team identity:")
#' print(team_identity)
#'
#' # View venue information
#' venue_info <- wnba_team_detailed[, c("venue_name", "venue_full_name", "venue_capacity",
#'                                    "venue_city", "venue_state", "venue_indoor")]
#' print("Venue information:")
#' print(venue_info)
#'
#' # View conference and division information
#' org_info <- wnba_team_detailed[, c("conference_name", "conference_short_name",
#'                                  "division_name", "division_short_name")]
#' print("Organizational information:")
#' print(org_info)
#'
#' # View current season records
#' record_info <- wnba_team_detailed[, c("current_season_year", "record_overall_summary",
#'                                     "record_home_summary", "record_away_summary",
#'                                     "record_overall_percentage")]
#' print("Season records:")
#' print(record_info)
#'
#' # View standings information
#' standing_info <- wnba_team_detailed[, c("standing_conference_rank", "standing_division_rank",
#'                                       "standing_league_rank", "standing_games_played",
#'                                       "standing_games_behind", "standing_win_percentage",
#'                                       "standing_playoff_seed")]
#' print("Standings information:")
#' print(standing_info)
#' }
#'
#' @export
fetch_wnba_team_detailed <- function(team, raw = FALSE) {
  url <- paste0("https://site.api.espn.com/apis/site/v2/sports/basketball/wnba/teams/", team)
  resp <- httr::GET(url, httr::timeout(10))

  if (httr::status_code(resp) != 200) {
    stop("Failed to fetch data for team: ", team)
  }

  data <- httr::content(resp, as = "parsed", simplifyVector = TRUE)

  if (isTRUE(raw)) {
    assign("wnba_team_detailed_raw", data, envir = .GlobalEnv)
  }

  result <- create_wnba_detailed_team_dataset(data, team)
  assign("wnba_team_detailed", result, envir = .GlobalEnv)

  invisible(result)
}
