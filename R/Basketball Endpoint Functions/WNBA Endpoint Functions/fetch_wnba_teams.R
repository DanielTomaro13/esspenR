#' Safe nested data extraction helper function for wnba teams
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_wnba_teams <- function(data, path, default = NA) {
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

#' Create wnba teams data frame from Site API response
#'
#' Processes raw JSON response from ESPN Site API into structured data frame
#' containing wnba teams information
#'
#' @param data Raw JSON response from ESPN Site API teams endpoint
#' @return Data frame with teams information
#' @keywords internal
create_wnba_teams_dataset <- function(data) {

  # Initialize teams data frame
  teams_df <- data.frame(
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
    venue_id = character(0),
    venue_name = character(0),
    venue_full_name = character(0),
    venue_capacity = character(0),
    venue_city = character(0),
    venue_state = character(0),
    venue_zip_code = character(0),
    venue_country_code = character(0),
    venue_indoor = character(0),
    stringsAsFactors = FALSE
  )

  # Extract teams data - try multiple possible structures
  teams_data <- list()

  # Try multiple possible paths for teams data
  if ("sports" %in% names(data)) {
    sports <- data$sports
    if (length(sports) > 0) {
      sport <- sports[[1]]
      if ("leagues" %in% names(sport)) {
        leagues <- sport$leagues
        if (length(leagues) > 0) {
          league <- leagues[[1]]
          if ("teams" %in% names(league)) {
            teams_data <- league$teams
          }
        }
      }
    }
  }

  # Alternative path - direct teams array
  if (length(teams_data) == 0 && "teams" %in% names(data)) {
    teams_data <- data$teams
  }

  # Another alternative - items array
  if (length(teams_data) == 0 && "items" %in% names(data)) {
    teams_data <- data$items
  }

  if (length(teams_data) == 0) {
    warning("No teams found in API response")
    return(teams_df)
  }

  for (i in seq_along(teams_data)) {
    team_wrapper <- teams_data[[i]]

    # Extract the actual team object from the wrapper
    team <- team_wrapper
    if ("team" %in% names(team_wrapper)) {
      team <- team_wrapper$team
    }

    # Basic team information
    team_id <- extract_nested_wnba_teams(team, c("id"), NA_character_)
    team_uid <- extract_nested_wnba_teams(team, c("uid"), NA_character_)
    team_slug <- extract_nested_wnba_teams(team, c("slug"), NA_character_)
    team_abbreviation <- extract_nested_wnba_teams(team, c("abbreviation"), NA_character_)
    team_display_name <- extract_nested_wnba_teams(team, c("displayName"), NA_character_)
    team_short_display_name <- extract_nested_wnba_teams(team, c("shortDisplayName"), NA_character_)
    team_name <- extract_nested_wnba_teams(team, c("name"), NA_character_)
    team_nickname <- extract_nested_wnba_teams(team, c("nickname"), NA_character_)
    team_location <- extract_nested_wnba_teams(team, c("location"), NA_character_)
    team_color <- extract_nested_wnba_teams(team, c("color"), NA_character_)
    team_alternate_color <- extract_nested_wnba_teams(team, c("alternateColor"), NA_character_)
    team_is_active <- extract_nested_wnba_teams(team, c("isActive"), "true")
    team_is_all_star <- extract_nested_wnba_teams(team, c("isAllStar"), "false")

    # Logo information - get both regular and dark logos
    team_logo_href <- NA_character_
    team_logo_alt <- NA_character_
    team_logo_width <- NA_character_
    team_logo_height <- NA_character_
    team_logo_dark_href <- NA_character_

    logos <- extract_nested_wnba_teams(team, c("logos"), list())
    if (length(logos) > 0) {
      for (logo in logos) {
        logo_rel <- extract_nested_wnba_teams(logo, c("rel"), list())
        if (length(logo_rel) > 0) {
          if ("default" %in% logo_rel || "full" %in% logo_rel) {
            team_logo_href <- extract_nested_wnba_teams(logo, c("href"), team_logo_href)
            team_logo_alt <- extract_nested_wnba_teams(logo, c("alt"), team_logo_alt)
            team_logo_width <- extract_nested_wnba_teams(logo, c("width"), team_logo_width)
            team_logo_height <- extract_nested_wnba_teams(logo, c("height"), team_logo_height)
          } else if ("dark" %in% logo_rel) {
            team_logo_dark_href <- extract_nested_wnba_teams(logo, c("href"), team_logo_dark_href)
          }
        }
      }
    }

    # Conference information
    conference_id <- NA_character_
    conference_name <- NA_character_
    conference_short_name <- NA_character_
    conference_uid <- NA_character_
    conference_abbreviation <- NA_character_

    # Try different paths for conference
    conference_info <- extract_nested_wnba_teams(team, c("conference"), list())
    if (length(conference_info) > 0) {
      conference_id <- extract_nested_wnba_teams(conference_info, c("id"), NA_character_)
      conference_name <- extract_nested_wnba_teams(conference_info, c("name"), NA_character_)
      conference_short_name <- extract_nested_wnba_teams(conference_info, c("shortName"), NA_character_)
      conference_uid <- extract_nested_wnba_teams(conference_info, c("uid"), NA_character_)
      conference_abbreviation <- extract_nested_wnba_teams(conference_info, c("abbreviation"), NA_character_)
    } else {
      # Try groups array for conference information
      groups <- extract_nested_wnba_teams(team, c("groups"), list())
      if (length(groups) > 0) {
        # Find conference group
        for (group in groups) {
          is_conf_group <- extract_nested_wnba_teams(group, c("isConferenceGroup"), "false")
          if (is_conf_group == "true") {
            conference_id <- extract_nested_wnba_teams(group, c("id"), NA_character_)
            conference_name <- extract_nested_wnba_teams(group, c("name"), NA_character_)
            conference_short_name <- extract_nested_wnba_teams(group, c("shortName"), NA_character_)
            conference_uid <- extract_nested_wnba_teams(group, c("uid"), NA_character_)
            conference_abbreviation <- extract_nested_wnba_teams(group, c("abbreviation"), NA_character_)
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
    division_info <- extract_nested_wnba_teams(team, c("division"), list())
    if (length(division_info) > 0) {
      division_id <- extract_nested_wnba_teams(division_info, c("id"), NA_character_)
      division_name <- extract_nested_wnba_teams(division_info, c("name"), NA_character_)
      division_short_name <- extract_nested_wnba_teams(division_info, c("shortName"), NA_character_)
      division_uid <- extract_nested_wnba_teams(division_info, c("uid"), NA_character_)
      division_abbreviation <- extract_nested_wnba_teams(division_info, c("abbreviation"), NA_character_)
    } else {
      # Try groups array for division information
      groups <- extract_nested_wnba_teams(team, c("groups"), list())
      if (length(groups) > 0) {
        # Find division group (not conference group)
        for (group in groups) {
          is_conf_group <- extract_nested_wnba_teams(group, c("isConferenceGroup"), "false")
          if (is_conf_group == "false") {
            division_id <- extract_nested_wnba_teams(group, c("id"), NA_character_)
            division_name <- extract_nested_wnba_teams(group, c("name"), NA_character_)
            division_short_name <- extract_nested_wnba_teams(group, c("shortName"), NA_character_)
            division_uid <- extract_nested_wnba_teams(group, c("uid"), NA_character_)
            division_abbreviation <- extract_nested_wnba_teams(group, c("abbreviation"), NA_character_)
            break
          }
        }
      }
    }

    # Venue information
    venue_id <- NA_character_
    venue_name <- NA_character_
    venue_full_name <- NA_character_
    venue_capacity <- NA_character_
    venue_city <- NA_character_
    venue_state <- NA_character_
    venue_zip_code <- NA_character_
    venue_country_code <- NA_character_
    venue_indoor <- NA_character_

    venue_info <- extract_nested_wnba_teams(team, c("venue"), list())
    if (length(venue_info) > 0) {
      venue_id <- extract_nested_wnba_teams(venue_info, c("id"), NA_character_)
      venue_name <- extract_nested_wnba_teams(venue_info, c("shortName"), NA_character_)
      venue_full_name <- extract_nested_wnba_teams(venue_info, c("fullName"), NA_character_)
      if (is.na(venue_name)) {
        venue_name <- extract_nested_wnba_teams(venue_info, c("name"), NA_character_)
      }
      venue_capacity <- extract_nested_wnba_teams(venue_info, c("capacity"), NA_character_)
      venue_indoor <- extract_nested_wnba_teams(venue_info, c("indoor"), "true")  # wnba is typically indoor

      # Address information
      address_info <- extract_nested_wnba_teams(venue_info, c("address"), list())
      if (length(address_info) > 0) {
        venue_city <- extract_nested_wnba_teams(address_info, c("city"), NA_character_)
        venue_state <- extract_nested_wnba_teams(address_info, c("state"), NA_character_)
        venue_zip_code <- extract_nested_wnba_teams(address_info, c("zipCode"), NA_character_)
        venue_country_code <- extract_nested_wnba_teams(address_info, c("countryCode"), NA_character_)
      }
    }

    # Create row
    team_row <- data.frame(
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
      venue_id = as.character(venue_id),
      venue_name = as.character(venue_name),
      venue_full_name = as.character(venue_full_name),
      venue_capacity = as.character(venue_capacity),
      venue_city = as.character(venue_city),
      venue_state = as.character(venue_state),
      venue_zip_code = as.character(venue_zip_code),
      venue_country_code = as.character(venue_country_code),
      venue_indoor = as.character(venue_indoor),
      stringsAsFactors = FALSE
    )

    teams_df <- rbind(teams_df, team_row)
  }

  # Clean up row names
  if (nrow(teams_df) > 0) rownames(teams_df) <- NULL

  return(teams_df)
}

#' Fetch wnba teams using Site API
#'
#' Retrieves comprehensive wnba teams information from ESPN's Site API.
#' The function fetches detailed team data including identities, conferences,
#' divisions, venues, and branding information.
#'
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'wnba_teams_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data frame. The main purpose is global
#'   environment assignment of \code{wnba_teams} containing:
#'   \itemize{
#'     \item Team identity: ID, UID, slug, abbreviation, names, nickname, location
#'     \item Team branding: colors, logos (regular and dark versions)
#'     \item Team status: active status, all-star designation
#'     \item Conference info: conference ID, name, abbreviation
#'     \item Division info: division ID, name, abbreviation
#'     \item Venue details: arena name, capacity, location, indoor status
#'   }
#'
#' @details
#' The function creates a comprehensive data frame with all wnba teams information.
#' This provides complete team data for analysis, visualization, and API reference.
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
#'   \item Conference: Eastern or Western Conference details
#'   \item Division: Atlantic, Central, Southeast, Northwest, Pacific, Southwest
#'   \item Structure: hierarchical organization within wnba
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
#'   \item Arena: name, full name, capacity
#'   \item Location: city, state, zip code, country code
#'   \item Features: indoor status (typically true for wnba)
#' }
#'
#' @examples
#' \dontrun{
#' # Get all wnba teams
#' fetch_wnba_teams()
#'
#' # Check the data
#' head(wnba_teams)
#' str(wnba_teams)
#'
#' # View team identity and branding
#' team_identity <- wnba_teams[, c("team_abbreviation", "team_display_name",
#'                               "team_location", "team_color", "team_alternate_color")]
#' print("Team identity:")
#' print(team_identity)
#'
#' # View conference and division breakdown
#' conf_division <- wnba_teams[, c("team_abbreviation", "team_display_name",
#'                               "conference_name", "division_name")]
#' print("Conference and division breakdown:")
#' print(conf_division)
#'
#' # Analyze conferences
#' if(require(dplyr, quietly = TRUE)) {
#'   conference_breakdown <- wnba_teams %>%
#'     filter(!is.na(conference_name) & conference_name != "") %>%
#'     count(conference_name, sort = TRUE)
#'
#'   print("Teams by conference:")
#'   print(conference_breakdown)
#'
#'   # Division breakdown
#'   division_breakdown <- wnba_teams %>%
#'     filter(!is.na(division_name) & division_name != "") %>%
#'     count(conference_name, division_name) %>%
#'     arrange(conference_name, division_name)
#'
#'   print("Teams by division:")
#'   print(division_breakdown)
#' }
#'
#' # Venue analysis
#' venue_info <- wnba_teams[!is.na(wnba_teams$venue_full_name),
#'                        c("team_abbreviation", "venue_full_name", "venue_capacity",
#'                          "venue_city", "venue_state")]
#'
#' if(nrow(venue_info) > 0) {
#'   venue_info$capacity_num <- as.numeric(venue_info$venue_capacity)
#'   venue_info <- venue_info[order(-venue_info$capacity_num, na.last = TRUE), ]
#'
#'   print("Venues by capacity:")
#'   print(head(venue_info, 10))
#' }
#'
#' # Eastern Conference teams
#' eastern_teams <- wnba_teams[wnba_teams$conference_name == "Eastern Conference" &
#'                           !is.na(wnba_teams$conference_name), ]
#'
#' if(nrow(eastern_teams) > 0) {
#'   print("Eastern Conference teams:")
#'   print(eastern_teams[, c("team_abbreviation", "team_display_name", "division_name")])
#' }
#'
#' # Western Conference teams
#' western_teams <- wnba_teams[wnba_teams$conference_name == "Western Conference" &
#'                           !is.na(wnba_teams$conference_name), ]
#'
#' if(nrow(western_teams) > 0) {
#'   print("Western Conference teams:")
#'   print(western_teams[, c("team_abbreviation", "team_display_name", "division_name")])
#' }
#'
#' # Team colors analysis
#' colors_data <- wnba_teams[!is.na(wnba_teams$team_color), ]
#' if(nrow(colors_data) > 0) {
#'   print("Team colors:")
#'   for(i in 1:min(10, nrow(colors_data))) {
#'     team <- colors_data[i, ]
#'     cat(sprintf("%s: Primary #%s", team$team_abbreviation, team$team_color))
#'     if(!is.na(team$team_alternate_color) && team$team_alternate_color != "") {
#'       cat(sprintf(", Secondary #%s", team$team_alternate_color))
#'     }
#'     cat("\n")
#'   }
#' }
#'
#' # Find teams with largest arenas
#' large_venues <- wnba_teams[!is.na(wnba_teams$venue_capacity) & wnba_teams$venue_capacity != "", ]
#' if(nrow(large_venues) > 0) {
#'   large_venues$capacity_num <- as.numeric(large_venues$venue_capacity)
#'   large_venues <- large_venues[order(-large_venues$capacity_num), ]
#'
#'   print("Largest wnba arenas:")
#'   print(head(large_venues[, c("team_abbreviation", "venue_full_name", "capacity_num")], 5))
#' }
#'
#' # Geographic distribution
#' if(require(dplyr, quietly = TRUE)) {
#'   geo_distribution <- wnba_teams %>%
#'     filter(!is.na(venue_state) & venue_state != "") %>%
#'     count(venue_state, sort = TRUE)
#'
#'   print("Teams by state:")
#'   print(head(geo_distribution, 10))
#' }
#'
#' # Get team ID for API calls
#' lakers_info <- wnba_teams[wnba_teams$team_abbreviation == "LAL", ]
#' if(nrow(lakers_info) > 0) {
#'   cat(sprintf("Lakers team ID: %s\n", lakers_info$team_id))
#'   cat(sprintf("Lakers venue: %s\n", lakers_info$venue_full_name))
#' }
#' }
#'
#' @export
fetch_wnba_teams <- function(raw = FALSE) {

  # Build API URL
  url <- "https://site.api.espn.com/apis/site/v2/sports/basketball/wnba/teams"

  # Fetch and parse data
  tryCatch({
    message("Fetching all wnba teams...")

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
      assign("wnba_teams_raw", data, envir = .GlobalEnv)
      message("Raw wnba teams data assigned to: wnba_teams_raw")
      message("Data structure preview:")

      # Show available sections
      sections <- names(data)
      message("- Available sections: ", paste(sections, collapse = ", "))

      # Try to find teams data and show its structure
      teams_data <- list()
      if ("sports" %in% names(data)) {
        teams_data <- extract_nested_wnba_teams(data, c("sports", 1, "leagues", 1, "teams"), list())
        if (length(teams_data) > 0) {
          message("- Found teams data via sports path")
        }
      }

      if (length(teams_data) == 0 && "teams" %in% names(data)) {
        teams_data <- data$teams
        message("- Found teams data directly")
      }

      if (length(teams_data) > 0) {
        message(sprintf("- Total teams found: %d", length(teams_data)))

        if (length(teams_data) > 0) {
          first_team <- teams_data[[1]]
          if ("team" %in% names(first_team)) {
            first_team <- first_team$team
          }
          team_name <- extract_nested_wnba_teams(first_team, c("displayName"), "Unknown")
          team_conf <- extract_nested_wnba_teams(first_team, c("conference", "name"), "Unknown")
          venue_name <- extract_nested_wnba_teams(first_team, c("venue", "fullName"), "Unknown")

          message(sprintf("- Sample team: %s", team_name))
          message(sprintf("- Sample conference: %s", team_conf))
          message(sprintf("- Sample venue: %s", venue_name))
        }
      } else {
        message("- No teams data found")
      }

      return(invisible(data))
    }

    # Create teams dataset
    teams_df <- create_wnba_teams_dataset(data)

    # Assign to global environment
    assign("wnba_teams", teams_df, envir = .GlobalEnv)

    # Summary message
    total_teams <- nrow(teams_df)

    message(sprintf("wnba teams assigned to: wnba_teams (%d teams)", total_teams))

    if (total_teams > 0) {
      # Count active vs inactive teams
      active_teams <- sum(teams_df$team_is_active == "true", na.rm = TRUE)
      inactive_teams <- sum(teams_df$team_is_active == "false", na.rm = TRUE)

      message(sprintf("  - Active teams: %d", active_teams))
      if (inactive_teams > 0) {
        message(sprintf("  - Inactive teams: %d", inactive_teams))
      }

      # Count teams with valid IDs
      teams_with_ids <- sum(!is.na(teams_df$team_id) & teams_df$team_id != "", na.rm = TRUE)
      message(sprintf("  - Teams with IDs: %d", teams_with_ids))

      # Conference breakdown
      conf_counts <- table(teams_df$conference_name[!is.na(teams_df$conference_name) & teams_df$conference_name != ""])
      if (length(conf_counts) > 0) {
        message("  - Conference breakdown:")
        for (conf_name in names(conf_counts)) {
          message(sprintf("    %s: %d teams", conf_name, conf_counts[conf_name]))
        }
      }

      # Division breakdown
      div_counts <- table(teams_df$division_name[!is.na(teams_df$division_name) & teams_df$division_name != ""])
      if (length(div_counts) > 0) {
        message(sprintf("  - Divisions represented: %d", length(div_counts)))
      }

      # Venue information
      venues_with_capacity <- sum(!is.na(teams_df$venue_capacity) & teams_df$venue_capacity != "", na.rm = TRUE)
      if (venues_with_capacity > 0) {
        message(sprintf("  - Teams with venue capacity data: %d", venues_with_capacity))
      }

      # Show sample teams
      if (total_teams >= 5) {
        sample_teams <- head(teams_df, 5)
        message("\nSample teams:")
        for (i in 1:nrow(sample_teams)) {
          team <- sample_teams[i, ]
          conf_info <- ifelse(is.na(team$conference_name) || team$conference_name == "",
                              "", paste0(" (", team$conference_name, ")"))
          message(sprintf("  %s - %s%s", team$team_abbreviation, team$team_display_name, conf_info))
        }
      }

      # Show team ID range for API usage
      if (teams_with_ids > 0) {
        valid_ids <- teams_df$team_id[!is.na(teams_df$team_id) & teams_df$team_id != ""]
        numeric_ids <- as.numeric(valid_ids[grepl("^\\d+$", valid_ids)])
        if (length(numeric_ids) > 0) {
          message(sprintf("\nTeam ID range: %d to %d", min(numeric_ids), max(numeric_ids)))
          message("Use these IDs for team-specific API calls")
        }
      }
    }

    return(invisible(teams_df))

  }, error = function(e) {
    stop(sprintf("Failed to fetch wnba teams: %s", e$message))
  })
}
