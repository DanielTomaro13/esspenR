#' Fetch MLB team roster data using ESPN Site API
#'
#' @param team_id Character or Numeric. ESPN team ID (e.g., "10" for New York Yankees).
#'   Can also accept team name or abbreviation which will be automatically resolved.
#' @param raw Logical. If TRUE, assigns raw JSON to global environment as 'mlb_roster_raw'. Default FALSE.
#' @return Invisibly returns the data, but main purpose is global environment assignment.
#' @export
#'
#' @examples
#' # Get roster using team ID
#' fetch_mlb_team_roster(team_id = "10")
#'
#' # Get roster using team name
#' fetch_mlb_team_roster(team_id = "New York Yankees")
#'
#' # Get roster using team abbreviation
#' fetch_mlb_team_roster(team_id = "NYY")
#'
#' # Get raw data for custom processing
#' fetch_mlb_team_roster(team_id = "10", raw = TRUE)
#' str(mlb_roster_raw, max.level = 2)
#'
fetch_mlb_team_roster <- function(team_id, raw = FALSE) {
  # Validate inputs
  if (missing(team_id)) {
    stop("'team_id' is a required parameter")
  }

  # Convert team_id to character
  team_input <- as.character(team_id)

  # Check if team_id is numeric (actual ID) or needs to be resolved
  if (!grepl("^\\d+$", team_input)) {
    # It's a team name or abbreviation, resolve to ID
    resolved_ids <- find_mlb_team_id(team_input)
    if (length(resolved_ids) == 0) {
      stop(sprintf("Could not find team matching '%s'", team_input))
    } else if (length(resolved_ids) > 1) {
      stop(sprintf("Multiple teams found matching '%s'. Please be more specific or use team ID directly.", team_input))
    }
    team_id_final <- resolved_ids[1]
  } else {
    team_id_final <- team_input
  }

  # Build URL for MLB
  url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/baseball/mlb/teams/%s/roster", team_id_final)

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
      assign("mlb_roster_raw", data, envir = .GlobalEnv)
      message("Raw MLB roster data assigned to: mlb_roster_raw")
      return(invisible(data))
    }

    # Create clean roster dataset
    roster_df <- create_clean_mlb_roster_dataset(data, team_id_final)

    # Assign to global environment
    assign("mlb_roster", roster_df, envir = .GlobalEnv)
    message(sprintf("MLB roster data assigned to: mlb_roster (Team ID: %s, %d players)",
                    team_id_final, nrow(roster_df)))

    return(invisible(roster_df))

  }, error = function(e) {
    stop(sprintf("Failed to fetch MLB roster data for team %s: %s", team_input, e$message))
  })
}

#' Create clean MLB roster dataset from ESPN Site API response
#'
#' @param data Raw JSON response from ESPN Site API roster endpoint
#' @param team_id Team ID used in request
#' @return Clean data frame with roster information
create_clean_mlb_roster_dataset <- function(data, team_id) {
  # Initialize result data frame
  result_df <- data.frame(
    team_id = character(0),
    athlete_id = character(0),
    first_name = character(0),
    last_name = character(0),
    full_name = character(0),
    display_name = character(0),
    jersey = character(0),
    position_abbreviation = character(0),
    position_name = character(0),
    height = numeric(0),
    display_height = character(0),
    weight = numeric(0),
    display_weight = character(0),
    age = integer(0),
    date_of_birth = character(0),
    debut_year = integer(0),
    experience_years = integer(0),
    college_name = character(0),
    birth_city = character(0),
    birth_state = character(0),
    birth_country = character(0),
    headshot_href = character(0),
    bats = character(0),
    throws = character(0),
    status = character(0),
    stringsAsFactors = FALSE
  )

  # Extract team information
  team_info <- if ("team" %in% names(data)) data[["team"]] else list()
  team_id_api <- if ("id" %in% names(team_info)) team_info[["id"]] else team_id

  # Process athletes from data$athletes - organized by position groups (Pitchers, Catchers, Infielders, Outfielders, etc.)
  athletes_groups <- NULL
  if ("athletes" %in% names(data) && length(data[["athletes"]]) > 0) {
    athletes_groups <- data[["athletes"]]
  }

  if (is.null(athletes_groups) || length(athletes_groups) == 0) {
    return(result_df)
  }

  # Process each athlete group (pitchers, catchers, infielders, outfielders, designated hitters)
  for (group_idx in seq_along(athletes_groups)) {
    group <- athletes_groups[[group_idx]]

    # Get group position (Pitchers, Catchers, Infielders, Outfielders, Designated Hitter)
    group_position <- if ("position" %in% names(group)) group[["position"]] else "unknown"

    # Get athletes in this group
    if ("items" %in% names(group) && length(group[["items"]]) > 0) {
      athletes <- group[["items"]]

      # Process each athlete in this group
      for (i in seq_along(athletes)) {
        athlete_data <- athletes[[i]]

        # Basic athlete info
        athlete_id <- if ("id" %in% names(athlete_data)) athlete_data[["id"]] else NA_character_
        first_name <- if ("firstName" %in% names(athlete_data)) athlete_data[["firstName"]] else NA_character_
        last_name <- if ("lastName" %in% names(athlete_data)) athlete_data[["lastName"]] else NA_character_
        full_name <- if ("fullName" %in% names(athlete_data)) athlete_data[["fullName"]] else NA_character_
        display_name <- if ("displayName" %in% names(athlete_data)) athlete_data[["displayName"]] else NA_character_
        jersey <- if ("jersey" %in% names(athlete_data)) athlete_data[["jersey"]] else NA_character_

        # Physical attributes
        height <- if ("height" %in% names(athlete_data)) as.numeric(athlete_data[["height"]]) else NA_real_
        display_height <- if ("displayHeight" %in% names(athlete_data)) athlete_data[["displayHeight"]] else NA_character_
        weight <- if ("weight" %in% names(athlete_data)) as.numeric(athlete_data[["weight"]]) else NA_real_
        display_weight <- if ("displayWeight" %in% names(athlete_data)) athlete_data[["displayWeight"]] else NA_character_
        age <- if ("age" %in% names(athlete_data)) as.integer(athlete_data[["age"]]) else NA_integer_
        date_of_birth <- if ("dateOfBirth" %in% names(athlete_data)) athlete_data[["dateOfBirth"]] else NA_character_
        debut_year <- if ("debutYear" %in% names(athlete_data)) as.integer(athlete_data[["debutYear"]]) else NA_integer_

        # Experience
        experience_years <- NA_integer_
        if ("experience" %in% names(athlete_data)) {
          experience <- athlete_data[["experience"]]
          experience_years <- if ("years" %in% names(experience)) as.integer(experience[["years"]]) else NA_integer_
        }

        # Position info
        position_abbreviation <- position_name <- NA_character_
        if ("position" %in% names(athlete_data)) {
          position <- athlete_data[["position"]]
          position_abbreviation <- if ("abbreviation" %in% names(position)) position[["abbreviation"]] else NA_character_
          position_name <- if ("name" %in% names(position)) position[["name"]] else NA_character_
        }

        # Birth place
        birth_city <- birth_state <- birth_country <- NA_character_
        if ("birthPlace" %in% names(athlete_data)) {
          birth_place <- athlete_data[["birthPlace"]]
          birth_city <- if ("city" %in% names(birth_place)) birth_place[["city"]] else NA_character_
          birth_state <- if ("state" %in% names(birth_place)) birth_place[["state"]] else NA_character_
          birth_country <- if ("country" %in% names(birth_place)) birth_place[["country"]] else NA_character_
        }

        # College
        college_name <- NA_character_
        if ("college" %in% names(athlete_data)) {
          college <- athlete_data[["college"]]
          college_name <- if ("name" %in% names(college)) college[["name"]] else NA_character_
        }

        # Headshot
        headshot_href <- NA_character_
        if ("headshot" %in% names(athlete_data)) {
          headshot <- athlete_data[["headshot"]]
          headshot_href <- if ("href" %in% names(headshot)) headshot[["href"]] else NA_character_
        }

        # Baseball-specific: Batting and throwing hand
        bats <- throws <- NA_character_
        if ("bats" %in% names(athlete_data)) {
          bats_info <- athlete_data[["bats"]]
          bats <- if ("abbreviation" %in% names(bats_info)) bats_info[["abbreviation"]] else NA_character_
        }
        if ("throws" %in% names(athlete_data)) {
          throws_info <- athlete_data[["throws"]]
          throws <- if ("abbreviation" %in% names(throws_info)) throws_info[["abbreviation"]] else NA_character_
        }

        # Player status
        status <- NA_character_
        if ("status" %in% names(athlete_data)) {
          status_info <- athlete_data[["status"]]
          status <- if ("name" %in% names(status_info)) status_info[["name"]] else NA_character_
        }

        # Create athlete row
        athlete_row <- data.frame(
          team_id = team_id_api,
          athlete_id = athlete_id,
          first_name = first_name,
          last_name = last_name,
          full_name = full_name,
          display_name = display_name,
          jersey = jersey,
          position_abbreviation = position_abbreviation,
          position_name = position_name,
          height = height,
          display_height = display_height,
          weight = weight,
          display_weight = display_weight,
          age = age,
          date_of_birth = date_of_birth,
          debut_year = debut_year,
          experience_years = experience_years,
          college_name = college_name,
          birth_city = birth_city,
          birth_state = birth_state,
          birth_country = birth_country,
          headshot_href = headshot_href,
          bats = bats,
          throws = throws,
          status = status,
          stringsAsFactors = FALSE
        )

        # Add to result
        result_df <- rbind(result_df, athlete_row)
      }
    }
  }

  # Clean up row names and sort by position group and jersey number
  if (nrow(result_df) > 0) {
    rownames(result_df) <- NULL

    # Sort by position, then jersey number
    result_df$jersey_numeric <- suppressWarnings(as.numeric(result_df$jersey))

    # Define position order for MLB
    position_order <- c("P", "SP", "RP", "C", "1B", "2B", "3B", "SS", "LF", "CF", "RF", "OF", "DH", "IF")
    result_df$position_factor <- factor(result_df$position_abbreviation,
                                        levels = position_order,
                                        ordered = TRUE)

    result_df <- result_df[order(result_df$position_factor, result_df$jersey_numeric, na.last = TRUE), ]
    result_df$jersey_numeric <- NULL
    result_df$position_factor <- NULL
    rownames(result_df) <- NULL
  }

  return(result_df)
}

#' Get MLB team IDs dynamically from ESPN API
#'
#' @param cache Logical. If TRUE (default), caches results for the session to avoid repeated API calls.
#' @return Data frame with team names, IDs, abbreviations, and other team information
#' @export
#'
#' @examples
#' # Get all MLB team IDs
#' team_ids <- get_mlb_team_ids()
#' print(team_ids)
#'
#' # Force fresh API call without caching
#' team_ids <- get_mlb_team_ids(cache = FALSE)
#'
get_mlb_team_ids <- function(cache = TRUE) {
  # Check if cached data exists and caching is enabled
  if (cache && exists(".mlb_teams_cache", envir = .GlobalEnv)) {
    return(get(".mlb_teams_cache", envir = .GlobalEnv))
  }

  # ESPN API endpoint for MLB teams
  url <- "https://site.api.espn.com/apis/site/v2/sports/baseball/mlb/teams"

  tryCatch({
    resp <- httr::GET(url, httr::timeout(60))
    if (httr::status_code(resp) != 200) {
      stop(sprintf("API request failed: HTTP %d - %s",
                   httr::status_code(resp),
                   httr::http_status(resp)$message))
    }

    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content_text, simplifyVector = FALSE, simplifyDataFrame = FALSE)

    # Extract teams from sports[1]$leagues[1]$teams
    teams_list <- NULL
    if ("sports" %in% names(data) && length(data[["sports"]]) > 0) {
      sport <- data[["sports"]][[1]]
      if ("leagues" %in% names(sport) && length(sport[["leagues"]]) > 0) {
        league <- sport[["leagues"]][[1]]
        if ("teams" %in% names(league)) {
          teams_list <- league[["teams"]]
        }
      }
    }

    if (is.null(teams_list) || length(teams_list) == 0) {
      stop("No teams found in API response")
    }

    # Initialize result data frame
    result_df <- data.frame(
      team_id = character(0),
      team_name = character(0),
      display_name = character(0),
      abbreviation = character(0),
      short_display_name = character(0),
      color = character(0),
      alternate_color = character(0),
      is_active = logical(0),
      logo_href = character(0),
      location = character(0),
      nickname = character(0),
      stringsAsFactors = FALSE
    )

    # Process each team
    for (i in seq_along(teams_list)) {
      team <- teams_list[[i]]

      # Extract team info
      team_id <- if ("id" %in% names(team)) team[["id"]] else NA_character_
      team_name <- if ("name" %in% names(team)) team[["name"]] else NA_character_
      display_name <- if ("displayName" %in% names(team)) team[["displayName"]] else NA_character_
      abbreviation <- if ("abbreviation" %in% names(team)) team[["abbreviation"]] else NA_character_
      short_display_name <- if ("shortDisplayName" %in% names(team)) team[["shortDisplayName"]] else NA_character_
      color <- if ("color" %in% names(team)) team[["color"]] else NA_character_
      alternate_color <- if ("alternateColor" %in% names(team)) team[["alternateColor"]] else NA_character_
      is_active <- if ("isActive" %in% names(team)) as.logical(team[["isActive"]]) else NA
      location <- if ("location" %in% names(team)) team[["location"]] else NA_character_
      nickname <- if ("nickname" %in% names(team)) team[["nickname"]] else NA_character_

      # Extract logo
      logo_href <- NA_character_
      if ("logos" %in% names(team) && length(team[["logos"]]) > 0) {
        logo_href <- if ("href" %in% names(team[["logos"]][[1]])) team[["logos"]][[1]][["href"]] else NA_character_
      }

      # Create team row
      team_row <- data.frame(
        team_id = team_id,
        team_name = team_name,
        display_name = display_name,
        abbreviation = abbreviation,
        short_display_name = short_display_name,
        color = color,
        alternate_color = alternate_color,
        is_active = is_active,
        logo_href = logo_href,
        location = location,
        nickname = nickname,
        stringsAsFactors = FALSE
      )

      result_df <- rbind(result_df, team_row)
    }

    # Sort by team name and clean up
    if (nrow(result_df) > 0) {
      result_df <- result_df[order(result_df$team_name), ]
      rownames(result_df) <- NULL
    }

    # Cache the result if caching is enabled
    if (cache) {
      assign(".mlb_teams_cache", result_df, envir = .GlobalEnv)
    }

    return(result_df)

  }, error = function(e) {
    stop(sprintf("Failed to fetch MLB teams data: %s", e$message))
  })
}

#' Find MLB team ID by name or abbreviation
#'
#' @param team Character. Team name, abbreviation, or partial match.
#' @param cache Logical. If TRUE (default), uses cached team data if available.
#' @return Character vector of team ID(s) matching the search term
#' @export
#'
#' @examples
#' # Find team by full name
#' find_mlb_team_id("New York Yankees")
#'
#' # Find team by abbreviation
#' find_mlb_team_id("NYY")
#'
#' # Find teams with partial match
#' find_mlb_team_id("New York")
#'
find_mlb_team_id <- function(team, cache = TRUE) {
  if (missing(team) || is.null(team) || team == "") {
    stop("'team' parameter is required and cannot be empty")
  }

  # Get all teams
  teams_df <- get_mlb_team_ids(cache = cache)

  # Convert search term to lowercase for case-insensitive matching
  team_lower <- tolower(as.character(team))

  # Search in multiple columns
  matches <- teams_df[
    grepl(team_lower, tolower(teams_df$team_name), fixed = TRUE) |
      grepl(team_lower, tolower(teams_df$display_name), fixed = TRUE) |
      grepl(team_lower, tolower(teams_df$abbreviation), fixed = TRUE) |
      grepl(team_lower, tolower(teams_df$short_display_name), fixed = TRUE) |
      grepl(team_lower, tolower(teams_df$location), fixed = TRUE) |
      grepl(team_lower, tolower(teams_df$nickname), fixed = TRUE),
  ]

  if (nrow(matches) == 0) {
    warning(sprintf("No teams found matching '%s'", team))
    return(character(0))
  }

  if (nrow(matches) > 1) {
    message(sprintf("Multiple teams found matching '%s':", team))
    print(matches[, c("team_id", "display_name", "abbreviation")])
  }

  return(matches$team_id)
}
