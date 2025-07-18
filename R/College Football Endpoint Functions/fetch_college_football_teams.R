#' Safe nested data extraction helper function for college football teams
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_teams <- function(data, path, default = NA) {
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

#' Create college football teams data frame from Site API response
#'
#' Processes raw JSON response from ESPN Site API into structured data frame
#' containing basic college football team information
#'
#' @param data Raw JSON response from ESPN Site API teams endpoint
#' @return Data frame with teams information
#' @keywords internal
create_cfb_teams_dataset <- function(data) {

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
    venue_id = character(0),
    venue_name = character(0),
    venue_capacity = character(0),
    venue_grass = character(0),
    venue_city = character(0),
    venue_state = character(0),
    venue_zip_code = character(0),
    venue_country_code = character(0),
    venue_indoor = character(0),
    conference_id = character(0),
    conference_name = character(0),
    conference_short_name = character(0),
    conference_uid = character(0),
    conference_abbreviation = character(0),
    group_id = character(0),
    group_name = character(0),
    group_short_name = character(0),
    group_is_conference_group = character(0),
    stringsAsFactors = FALSE
  )

  # Extract teams from different possible locations in the response
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
    team <- extract_nested_teams(team_wrapper, c("team"), team_wrapper)

    # Basic team information
    team_id <- extract_nested_teams(team, c("id"), NA_character_)
    team_uid <- extract_nested_teams(team, c("uid"), NA_character_)
    team_slug <- extract_nested_teams(team, c("slug"), NA_character_)
    team_abbreviation <- extract_nested_teams(team, c("abbreviation"), NA_character_)
    team_display_name <- extract_nested_teams(team, c("displayName"), NA_character_)
    team_short_display_name <- extract_nested_teams(team, c("shortDisplayName"), NA_character_)
    team_name <- extract_nested_teams(team, c("name"), NA_character_)
    team_nickname <- extract_nested_teams(team, c("nickname"), NA_character_)
    team_location <- extract_nested_teams(team, c("location"), NA_character_)
    team_color <- extract_nested_teams(team, c("color"), NA_character_)
    team_alternate_color <- extract_nested_teams(team, c("alternateColor"), NA_character_)
    team_is_active <- extract_nested_teams(team, c("isActive"), "true")
    team_is_all_star <- extract_nested_teams(team, c("isAllStar"), "false")

    # Logo information
    team_logo_href <- NA_character_
    team_logo_alt <- NA_character_
    team_logo_width <- NA_character_
    team_logo_height <- NA_character_

    logos <- extract_nested_teams(team, c("logos"), list())
    if (length(logos) > 0) {
      # Take the first logo
      first_logo <- logos[[1]]
      team_logo_href <- extract_nested_teams(first_logo, c("href"), NA_character_)
      team_logo_alt <- extract_nested_teams(first_logo, c("alt"), NA_character_)
      team_logo_width <- extract_nested_teams(first_logo, c("width"), NA_character_)
      team_logo_height <- extract_nested_teams(first_logo, c("height"), NA_character_)
    }

    # Venue information
    venue_id <- NA_character_
    venue_name <- NA_character_
    venue_capacity <- NA_character_
    venue_grass <- NA_character_
    venue_city <- NA_character_
    venue_state <- NA_character_
    venue_zip_code <- NA_character_
    venue_country_code <- NA_character_
    venue_indoor <- NA_character_

    venue_info <- extract_nested_teams(team, c("venue"), list())
    if (length(venue_info) > 0) {
      venue_id <- extract_nested_teams(venue_info, c("id"), NA_character_)
      venue_name <- extract_nested_teams(venue_info, c("fullName"), NA_character_)
      if (is.na(venue_name)) {
        venue_name <- extract_nested_teams(venue_info, c("name"), NA_character_)
      }
      venue_capacity <- extract_nested_teams(venue_info, c("capacity"), NA_character_)
      venue_grass <- extract_nested_teams(venue_info, c("grass"), NA_character_)
      venue_indoor <- extract_nested_teams(venue_info, c("indoor"), "false")

      # Address information
      address_info <- extract_nested_teams(venue_info, c("address"), list())
      if (length(address_info) > 0) {
        venue_city <- extract_nested_teams(address_info, c("city"), NA_character_)
        venue_state <- extract_nested_teams(address_info, c("state"), NA_character_)
        venue_zip_code <- extract_nested_teams(address_info, c("zipCode"), NA_character_)
        venue_country_code <- extract_nested_teams(address_info, c("countryCode"), NA_character_)
      }
    }

    # Venue information - not available in basic teams endpoint
    venue_id <- NA_character_
    venue_name <- NA_character_
    venue_capacity <- NA_character_
    venue_grass <- NA_character_
    venue_city <- NA_character_
    venue_state <- NA_character_
    venue_zip_code <- NA_character_
    venue_country_code <- NA_character_
    venue_indoor <- NA_character_

    # Check if venue info exists (unlikely in basic endpoint)
    venue_info <- extract_nested_teams(team, c("venue"), list())
    if (length(venue_info) > 0) {
      venue_id <- extract_nested_teams(venue_info, c("id"), NA_character_)
      venue_name <- extract_nested_teams(venue_info, c("fullName"), NA_character_)
      if (is.na(venue_name)) {
        venue_name <- extract_nested_teams(venue_info, c("name"), NA_character_)
      }
      venue_capacity <- extract_nested_teams(venue_info, c("capacity"), NA_character_)
      venue_grass <- extract_nested_teams(venue_info, c("grass"), NA_character_)
      venue_indoor <- extract_nested_teams(venue_info, c("indoor"), "false")

      # Address information
      address_info <- extract_nested_teams(venue_info, c("address"), list())
      if (length(address_info) > 0) {
        venue_city <- extract_nested_teams(address_info, c("city"), NA_character_)
        venue_state <- extract_nested_teams(address_info, c("state"), NA_character_)
        venue_zip_code <- extract_nested_teams(address_info, c("zipCode"), NA_character_)
        venue_country_code <- extract_nested_teams(address_info, c("countryCode"), NA_character_)
      }
    }

    # Conference information - not available in basic teams endpoint
    conference_id <- NA_character_
    conference_name <- NA_character_
    conference_short_name <- NA_character_
    conference_uid <- NA_character_
    conference_abbreviation <- NA_character_

    # Check if conference info exists (unlikely in basic endpoint)
    conference_info <- extract_nested_teams(team, c("conference"), list())
    if (length(conference_info) > 0) {
      conference_id <- extract_nested_teams(conference_info, c("id"), NA_character_)
      conference_name <- extract_nested_teams(conference_info, c("name"), NA_character_)
      conference_short_name <- extract_nested_teams(conference_info, c("shortName"), NA_character_)
      conference_uid <- extract_nested_teams(conference_info, c("uid"), NA_character_)
      conference_abbreviation <- extract_nested_teams(conference_info, c("abbreviation"), NA_character_)
    } else {
      # Try conferences array
      conferences <- extract_nested_teams(team, c("conferences"), list())
      if (length(conferences) > 0) {
        first_conf <- conferences[[1]]
        conference_id <- extract_nested_teams(first_conf, c("id"), NA_character_)
        conference_name <- extract_nested_teams(first_conf, c("name"), NA_character_)
        conference_short_name <- extract_nested_teams(first_conf, c("shortName"), NA_character_)
        conference_uid <- extract_nested_teams(first_conf, c("uid"), NA_character_)
        conference_abbreviation <- extract_nested_teams(first_conf, c("abbreviation"), NA_character_)
      } else {
        # Try direct conferenceId field
        conference_id <- extract_nested_teams(team, c("conferenceId"), NA_character_)
      }
    }

    # Group information (conference grouping)
    group_id <- NA_character_
    group_name <- NA_character_
    group_short_name <- NA_character_
    group_is_conference_group <- NA_character_

    groups <- extract_nested_teams(team, c("groups"), list())
    if (length(groups) > 0) {
      # Find the conference group (usually the first one or one marked as conference)
      for (group in groups) {
        is_conf_group <- extract_nested_teams(group, c("isConferenceGroup"), "false")
        if (is_conf_group == "true" || is.na(group_id)) {
          group_id <- extract_nested_teams(group, c("id"), NA_character_)
          group_name <- extract_nested_teams(group, c("name"), NA_character_)
          group_short_name <- extract_nested_teams(group, c("shortName"), NA_character_)
          group_is_conference_group <- is_conf_group

          if (is_conf_group == "true") break  # Prefer conference groups
        }
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
      venue_id = as.character(venue_id),
      venue_name = as.character(venue_name),
      venue_capacity = as.character(venue_capacity),
      venue_grass = as.character(venue_grass),
      venue_city = as.character(venue_city),
      venue_state = as.character(venue_state),
      venue_zip_code = as.character(venue_zip_code),
      venue_country_code = as.character(venue_country_code),
      venue_indoor = as.character(venue_indoor),
      conference_id = as.character(conference_id),
      conference_name = as.character(conference_name),
      conference_short_name = as.character(conference_short_name),
      conference_uid = as.character(conference_uid),
      conference_abbreviation = as.character(conference_abbreviation),
      group_id = as.character(group_id),
      group_name = as.character(group_name),
      group_short_name = as.character(group_short_name),
      group_is_conference_group = as.character(group_is_conference_group),
      stringsAsFactors = FALSE
    )

    teams_df <- rbind(teams_df, team_row)
  }

  # Clean up row names
  if (nrow(teams_df) > 0) rownames(teams_df) <- NULL

  return(teams_df)
}

#' Fetch college football teams list using Site API
#'
#' Retrieves a comprehensive list of college football teams from ESPN's Site API.
#' The function fetches basic team information that serves as a foundation
#' for other team-specific API calls.
#'
#' @param limit Integer. Maximum number of teams to retrieve (default: 1000).
#'   ESPN typically has 130+ FBS teams plus FCS teams.
#' @param group Character or numeric. Conference group ID to filter by (default: NULL for all teams).
#'   Use group IDs from \code{get_cfb_conference_groups()} to filter by conference.
#' @param division Character. Division to filter by: "fbs", "fcs", or NULL for all (default: NULL).
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'cfb_teams_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data frame. The main purpose is global
#'   environment assignment of \code{cfb_teams} containing:
#'   \itemize{
#'     \item Team identity: ID, UID, slug, abbreviation, names, nickname, location
#'     \item Team branding: colors, logos with dimensions
#'     \item Team status: active status, all-star designation
#'     \item Venue information: stadium name, capacity, surface, location, indoor/outdoor
#'     \item Conference details: conference ID, name, abbreviation
#'     \item Group classification: conference group information
#'   }
#'
#' @details
#' The function creates a structured data frame with basic college football team information.
#' Each row represents a team with essential details needed for further API calls and analysis.
#' This serves as a master list for team identification and basic classification.
#'
#' **Team Information**:
#' \itemize{
#'   \item Identity: team ID (for API calls), abbreviation, full names
#'   \item Location: team location/city, nickname/mascot
#'   \item Status: active status, special designations
#' }
#'
#' **Team Branding**:
#' \itemize{
#'   \item Colors: primary and alternate team colors (hex codes)
#'   \item Logos: logo URLs with dimensions and alt text
#' }
#'
#' **Venue Details**:
#' \itemize{
#'   \item Stadium: name, capacity, playing surface
#'   \item Location: city, state, zip code, country
#'   \item Features: indoor/outdoor designation
#' }
#'
#' **Conference Classification**:
#' \itemize{
#'   \item Conference: official conference affiliation
#'   \item Group: ESPN's conference grouping system
#'   \item Division: FBS/FCS classification (when available)
#' }
#'
#' @examples
#' \dontrun{
#' # Get all college football teams
#' fetch_college_football_teams()
#'
#' # Get teams from specific conference group (SEC)
#' fetch_college_football_teams(group = "8")
#'
#' # Check the data
#' head(cfb_teams)
#'
#' # View team counts by conference
#' conf_counts <- table(cfb_teams$conference_name)
#' print("Teams per conference:")
#' print(sort(conf_counts[conf_counts > 0], decreasing = TRUE))
#'
#' # Find teams by state
#' texas_teams <- cfb_teams[cfb_teams$venue_state == "TX" & !is.na(cfb_teams$venue_state), ]
#' print("Texas teams:")
#' print(texas_teams[, c("team_display_name", "conference_name", "venue_city")])
#'
#' # Largest stadiums
#' capacity_data <- cfb_teams[!is.na(cfb_teams$venue_capacity) & cfb_teams$venue_capacity != "", ]
#' if(nrow(capacity_data) > 0) {
#'   capacity_data$capacity_num <- as.numeric(capacity_data$venue_capacity)
#'   largest_venues <- capacity_data[order(-capacity_data$capacity_num), ]
#'
#'   print("Largest venues:")
#'   print(head(largest_venues[, c("team_display_name", "venue_name", "venue_capacity")], 10))
#' }
#'
#' # Indoor vs outdoor stadiums
#' venue_types <- table(cfb_teams$venue_indoor[cfb_teams$venue_indoor != ""])
#' print("Venue types:")
#' print(venue_types)
#'
#' # Teams by colors (find teams with specific colors)
#' red_teams <- cfb_teams[grepl("red|Red|ff0000|FF0000|cc0000|CC0000",
#'                             cfb_teams$team_color, ignore.case = TRUE), ]
#' if(nrow(red_teams) > 0) {
#'   print("Teams with red colors:")
#'   print(head(red_teams[, c("team_display_name", "team_color")], 10))
#' }
#'
#' # Active vs inactive teams
#' status_counts <- table(cfb_teams$team_is_active)
#' print("Team status:")
#' print(status_counts)
#'
#' # Conference group analysis
#' if(require(dplyr, quietly = TRUE)) {
#'   group_summary <- cfb_teams %>%
#'     filter(!is.na(group_name) & group_name != "") %>%
#'     group_by(group_name) %>%
#'     summarise(
#'       team_count = n(),
#'       conferences = n_distinct(conference_name, na.rm = TRUE),
#'       .groups = 'drop'
#'     ) %>%
#'     arrange(desc(team_count))
#'
#'   print("Teams by conference group:")
#'   print(group_summary)
#' }
#'
#' # Search for specific teams
#' team_search <- function(search_term) {
#'   matches <- cfb_teams[
#'     grepl(search_term, cfb_teams$team_display_name, ignore.case = TRUE) |
#'     grepl(search_term, cfb_teams$team_location, ignore.case = TRUE) |
#'     grepl(search_term, cfb_teams$team_nickname, ignore.case = TRUE),
#'     c("team_id", "team_display_name", "conference_name", "venue_city", "venue_state")
#'   ]
#'   return(matches)
#' }
#'
#' # Find all "State" universities
#' state_universities <- team_search("State")
#' print("State universities:")
#' print(head(state_universities, 10))
#'
#' # Find specific team by name
#' alabama_teams <- team_search("Alabama")
#' print("Alabama teams:")
#' print(alabama_teams)
#' }
#'
#' @seealso \code{\link{get_cfb_conference_groups}} for conference group mappings
#'
#' @export
fetch_college_football_teams <- function(limit = 1000, group = NULL, division = NULL, raw = FALSE) {

  # Input validation
  if (!is.numeric(limit) || limit < 1 || limit > 2000) {
    stop("'limit' must be a number between 1 and 2000")
  }

  if (!is.null(group)) {
    group <- as.character(group)
    if (!grepl("^\\d+$", group)) {
      stop("'group' must be a numeric conference group ID")
    }
  }

  if (!is.null(division)) {
    valid_divisions <- c("fbs", "fcs")
    if (!tolower(division) %in% valid_divisions) {
      stop(sprintf("'division' must be one of: %s", paste(valid_divisions, collapse = ", ")))
    }
    division <- tolower(division)
  }

  # Build API URL
  url <- "https://site.api.espn.com/apis/site/v2/sports/football/college-football/teams"

  # Build query parameters
  params <- list()
  params$limit <- limit

  if (!is.null(group)) {
    params$groups <- group
    message(sprintf("Fetching college football teams for group %s...", group))
  } else {
    message("Fetching all college football teams...")
  }

  if (!is.null(division)) {
    params$division <- division
    message(sprintf("Filtering for %s teams...", toupper(division)))
  }

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
      assign("cfb_teams_raw", data, envir = .GlobalEnv)
      message("Raw college football teams data assigned to: cfb_teams_raw")
      message("Data structure preview:")

      # Show available sections
      sections <- names(data)
      message("- Available sections: ", paste(sections, collapse = ", "))

      # Try to find teams count
      teams_count <- 0
      if ("sports" %in% names(data)) {
        sports <- data$sports
        if (length(sports) > 0) {
          sport <- sports[[1]]
          if ("leagues" %in% names(sport)) {
            leagues <- sport$leagues
            if (length(leagues) > 0) {
              league <- leagues[[1]]
              if ("teams" %in% names(league)) {
                teams_count <- length(league$teams)
              }
            }
          }
        }
      }

      if (teams_count == 0 && "teams" %in% names(data)) {
        teams_count <- length(data$teams)
      }

      if (teams_count == 0 && "items" %in% names(data)) {
        teams_count <- length(data$items)
      }

      message("- Total teams found: ", teams_count)

      return(invisible(data))
    }

    # Create teams dataset
    teams_df <- create_cfb_teams_dataset(data)

    # Assign to global environment
    assign("cfb_teams", teams_df, envir = .GlobalEnv)

    # Summary message
    total_teams <- nrow(teams_df)

    message(sprintf("College football teams assigned to: cfb_teams (%d teams)", total_teams))

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

      # Note about limited data in basic endpoint
      venue_count <- sum(!is.na(teams_df$venue_name) & teams_df$venue_name != "", na.rm = TRUE)
      conf_count <- sum(!is.na(teams_df$conference_name) & teams_df$conference_name != "", na.rm = TRUE)

      if (venue_count == 0 && conf_count == 0) {
        message("  - Note: Basic teams endpoint provides team identity only")
        message("  - Conference and venue data not available in this endpoint")
        message("  - Use team-specific endpoints for detailed information")
      } else {
        if (conf_count > 0) {
          message(sprintf("  - Teams with conference data: %d", conf_count))
        }
        if (venue_count > 0) {
          message(sprintf("  - Teams with venue data: %d", venue_count))
        }
      }

      # Show sample teams with available data
      if (teams_with_ids >= 5) {
        sample_teams <- head(teams_df[!is.na(teams_df$team_id), ], 5)
        message("\nSample teams:")
        for (i in 1:nrow(sample_teams)) {
          team <- sample_teams[i, ]
          message(sprintf("  %s - %s (ID: %s)",
                          team$team_abbreviation,
                          team$team_display_name,
                          team$team_id))
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
    stop(sprintf("Failed to fetch college football teams: %s", e$message))
  })
}
