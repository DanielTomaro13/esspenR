#' Fetch soccer team squad/roster data using ESPN API
#'
#' @param league Character. League identifier (e.g., "eng.1", "usa.1", "esp.1", "ger.1", "ita.1", "fra.1").
#' @param team_id Character or Numeric. Team ID (e.g., "359" for Arsenal).
#' @param raw Logical. If TRUE, assigns raw JSON to global environment as 'soccer_squad_raw'. Default FALSE.
#' @return Invisibly returns the data, but main purpose is global environment assignment.
#' @export
#'
#' @examples
#' \dontrun{
#' # Get Arsenal squad
#' fetch_soccer_squad("eng.1", "359")
#' head(soccer_squad)
#'
#' # Get Inter Miami squad
#' fetch_soccer_squad("usa.1", "20232")
#'
#' # Get Barcelona squad
#' fetch_soccer_squad("esp.1", "83")
#'
#' # Get raw data for custom processing
#' fetch_soccer_squad("eng.1", "359", raw = TRUE)
#' str(soccer_squad_raw, max.level = 2)
#' }
#'
fetch_soccer_squad <- function(league, team_id, raw = FALSE) {

  # Validate inputs
  if (missing(league) || !is.character(league) || length(league) != 1) {
    stop("'league' must be a single character string (e.g., 'eng.1', 'usa.1', 'esp.1')")
  }

  if (missing(team_id)) {
    stop("'team_id' is required (e.g., '359' for Arsenal)")
  }

  if (!is.logical(raw)) {
    stop("'raw' must be TRUE or FALSE")
  }

  # Convert team_id to character
  team_id <- as.character(team_id)

  # Build URL using the ESPN team roster endpoint
  base_url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/soccer/%s/teams/%s/roster", league, team_id)

  # Fetch and parse
  tryCatch({
    resp <- httr::GET(base_url, httr::timeout(60))

    if (httr::status_code(resp) != 200) {
      stop(sprintf("API request failed: HTTP %d - %s. Check if team_id '%s' is valid for league '%s'",
                   httr::status_code(resp),
                   httr::http_status(resp)$message,
                   team_id, league))
    }

    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content_text, simplifyVector = FALSE, simplifyDataFrame = FALSE)

    # Handle raw data assignment
    if (isTRUE(raw)) {
      assign("soccer_squad_raw", data, envir = .GlobalEnv)
      message("Raw soccer squad data assigned to: soccer_squad_raw")
      return(invisible(data))
    }

    # Create clean squad dataset
    squad_df <- create_clean_soccer_squad_dataset(data, league, team_id)

    # Assign to global environment
    assign("soccer_squad", squad_df, envir = .GlobalEnv)

    # Create informative message
    filter_info <- create_soccer_squad_filter_message(league, team_id, data)
    message(sprintf("Soccer squad data assigned to: soccer_squad (%d players%s)",
                    nrow(squad_df), filter_info))

    return(invisible(squad_df))

  }, error = function(e) {
    stop(sprintf("Failed to fetch soccer squad data: %s", e$message))
  })
}

#' Create clean squad dataset from ESPN team roster API response
#'
#' @param data Raw API response data
#' @param league League identifier
#' @param team_id Team ID
#' @return Clean data frame with squad information
#' @keywords internal
create_clean_soccer_squad_dataset <- function(data, league, team_id) {

  # Initialize result data frame
  result_df <- data.frame(
    player_id = character(0),
    player_uid = character(0),
    player_guid = character(0),
    player_first_name = character(0),
    player_last_name = character(0),
    player_full_name = character(0),
    player_display_name = character(0),
    player_short_name = character(0),
    jersey_number = character(0),
    position_id = character(0),
    position_name = character(0),
    position_display_name = character(0),
    position_abbreviation = character(0),
    age = integer(0),
    date_of_birth = character(0),
    height = numeric(0),
    display_height = character(0),
    weight = numeric(0),
    display_weight = character(0),
    gender = character(0),
    citizenship = character(0),
    citizenship_country_abbreviation = character(0),
    flag_url = character(0),
    status_id = character(0),
    status_name = character(0),
    status_type = character(0),
    slug = character(0),
    player_url = character(0),
    profiled = logical(0),
    team_id = character(0),
    team_name = character(0),
    team_abbreviation = character(0),
    league = character(0),
    stringsAsFactors = FALSE
  )

  # Extract team info first
  team_name <- team_abbreviation <- NA_character_
  if ("team" %in% names(data)) {
    team_info <- data[["team"]]
    team_name <- if ("displayName" %in% names(team_info)) team_info[["displayName"]] else NA_character_
    team_abbreviation <- if ("abbreviation" %in% names(team_info)) team_info[["abbreviation"]] else NA_character_
  }

  # Check if we have athletes data
  athletes <- NULL
  if ("athletes" %in% names(data) && length(data[["athletes"]]) > 0) {
    athletes <- data[["athletes"]]
  }

  if (is.null(athletes) || length(athletes) == 0) {
    return(result_df)
  }

  # Process each athlete
  for (i in seq_along(athletes)) {
    athlete_data <- athletes[[i]]

    # Extract basic athlete information
    player_id <- if ("id" %in% names(athlete_data)) athlete_data[["id"]] else NA_character_
    player_uid <- if ("uid" %in% names(athlete_data)) athlete_data[["uid"]] else NA_character_
    player_guid <- if ("guid" %in% names(athlete_data)) athlete_data[["guid"]] else NA_character_
    player_first_name <- if ("firstName" %in% names(athlete_data)) athlete_data[["firstName"]] else NA_character_
    player_last_name <- if ("lastName" %in% names(athlete_data)) athlete_data[["lastName"]] else NA_character_
    player_full_name <- if ("fullName" %in% names(athlete_data)) athlete_data[["fullName"]] else NA_character_
    player_display_name <- if ("displayName" %in% names(athlete_data)) athlete_data[["displayName"]] else NA_character_
    player_short_name <- if ("shortName" %in% names(athlete_data)) athlete_data[["shortName"]] else NA_character_
    jersey_number <- if ("jersey" %in% names(athlete_data)) athlete_data[["jersey"]] else NA_character_
    slug <- if ("slug" %in% names(athlete_data)) athlete_data[["slug"]] else NA_character_
    profiled <- if ("profiled" %in% names(athlete_data)) as.logical(athlete_data[["profiled"]]) else NA

    # Extract position information
    position_id <- position_name <- position_display_name <- position_abbreviation <- NA_character_
    if ("position" %in% names(athlete_data)) {
      position <- athlete_data[["position"]]
      position_id <- if ("id" %in% names(position)) position[["id"]] else NA_character_
      position_name <- if ("name" %in% names(position)) position[["name"]] else NA_character_
      position_display_name <- if ("displayName" %in% names(position)) position[["displayName"]] else NA_character_
      position_abbreviation <- if ("abbreviation" %in% names(position)) position[["abbreviation"]] else NA_character_
    }

    # Extract physical attributes
    age <- if ("age" %in% names(athlete_data)) as.integer(athlete_data[["age"]]) else NA_integer_
    date_of_birth <- if ("dateOfBirth" %in% names(athlete_data)) athlete_data[["dateOfBirth"]] else NA_character_
    height <- if ("height" %in% names(athlete_data)) as.numeric(athlete_data[["height"]]) else NA_real_
    display_height <- if ("displayHeight" %in% names(athlete_data)) athlete_data[["displayHeight"]] else NA_character_
    weight <- if ("weight" %in% names(athlete_data)) as.numeric(athlete_data[["weight"]]) else NA_real_
    display_weight <- if ("displayWeight" %in% names(athlete_data)) athlete_data[["displayWeight"]] else NA_character_
    gender <- if ("gender" %in% names(athlete_data)) athlete_data[["gender"]] else NA_character_

    # Extract citizenship information
    citizenship <- citizenship_country_abbreviation <- NA_character_
    if ("citizenship" %in% names(athlete_data)) {
      citizenship <- athlete_data[["citizenship"]]
    }
    if ("citizenshipCountry" %in% names(athlete_data)) {
      citizenship_country <- athlete_data[["citizenshipCountry"]]
      citizenship_country_abbreviation <- if ("abbreviation" %in% names(citizenship_country)) citizenship_country[["abbreviation"]] else NA_character_
    }

    # Extract flag URL
    flag_url <- NA_character_
    if ("flag" %in% names(athlete_data)) {
      flag <- athlete_data[["flag"]]
      flag_url <- if ("href" %in% names(flag)) flag[["href"]] else NA_character_
    }

    # Extract status information
    status_id <- status_name <- status_type <- NA_character_
    if ("status" %in% names(athlete_data)) {
      status <- athlete_data[["status"]]
      status_id <- if ("id" %in% names(status)) status[["id"]] else NA_character_
      status_name <- if ("name" %in% names(status)) status[["name"]] else NA_character_
      status_type <- if ("type" %in% names(status)) status[["type"]] else NA_character_
    }

    # Extract player URL from links
    player_url <- NA_character_
    if ("links" %in% names(athlete_data) && length(athlete_data[["links"]]) > 0) {
      # Get the first link (usually player overview/card)
      link <- athlete_data[["links"]][[1]]
      player_url <- if ("href" %in% names(link)) link[["href"]] else NA_character_
    }

    # Create athlete row
    athlete_row <- data.frame(
      player_id = player_id,
      player_uid = player_uid,
      player_guid = player_guid,
      player_first_name = player_first_name,
      player_last_name = player_last_name,
      player_full_name = player_full_name,
      player_display_name = player_display_name,
      player_short_name = player_short_name,
      jersey_number = jersey_number,
      position_id = position_id,
      position_name = position_name,
      position_display_name = position_display_name,
      position_abbreviation = position_abbreviation,
      age = age,
      date_of_birth = date_of_birth,
      height = height,
      display_height = display_height,
      weight = weight,
      display_weight = display_weight,
      gender = gender,
      citizenship = citizenship,
      citizenship_country_abbreviation = citizenship_country_abbreviation,
      flag_url = flag_url,
      status_id = status_id,
      status_name = status_name,
      status_type = status_type,
      slug = slug,
      player_url = player_url,
      profiled = profiled,
      team_id = team_id,
      team_name = team_name,
      team_abbreviation = team_abbreviation,
      league = league,
      stringsAsFactors = FALSE
    )

    # Add to result
    result_df <- rbind(result_df, athlete_row)
  }

  # Clean up and sort
  if (nrow(result_df) > 0) {
    rownames(result_df) <- NULL
    # Sort by position, then jersey number
    result_df$jersey_numeric <- suppressWarnings(as.numeric(result_df$jersey_number))
    result_df <- result_df[order(result_df$position_abbreviation, result_df$jersey_numeric, na.last = TRUE), ]
    result_df$jersey_numeric <- NULL
    rownames(result_df) <- NULL
  }

  return(result_df)
}

#' Create informative filter message for squad
#'
#' @param league League identifier
#' @param team_id Team ID
#' @param data Raw API response (to extract team name)
#' @return Character string describing applied filters
#' @keywords internal
create_soccer_squad_filter_message <- function(league, team_id, data) {
  filters <- character(0)

  # League names
  league_names <- list(
    "eng.1" = "Premier League",
    "esp.1" = "La Liga",
    "ger.1" = "Bundesliga",
    "ita.1" = "Serie A",
    "fra.1" = "Ligue 1",
    "usa.1" = "MLS",
    "mex.1" = "Liga MX",
    "ned.1" = "Eredivisie",
    "por.1" = "Primeira Liga",
    "bra.1" = "Brasileirão"
  )

  league_display <- if (league %in% names(league_names)) league_names[[league]] else league

  # Try to get team name from data
  team_name <- team_id
  if ("team" %in% names(data) && "displayName" %in% names(data[["team"]])) {
    team_name <- data[["team"]][["displayName"]]
  }

  filters <- c(filters, sprintf("league: %s", league_display))
  filters <- c(filters, sprintf("team: %s", team_name))

  if (length(filters) > 0) {
    return(sprintf(", filters: %s", paste(filters, collapse = ", ")))
  }

  return("")
}



#' Fetch squads for all teams in a soccer league
#'
#' @param league Character. League identifier (e.g., "eng.1", "usa.1", "esp.1").
#' @param raw Logical. If TRUE, assigns raw JSON to global environment as 'soccer_league_squads_raw'. Default FALSE.
#' @param delay Numeric. Delay in seconds between API calls to be respectful (default: 0.2).
#' @return Invisibly returns the data, but main purpose is global environment assignment.
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all Premier League squads
#' fetch_soccer_league_squads("eng.1")
#' head(soccer_league_squads)
#'
#' # Get all MLS squads with longer delay
#' fetch_soccer_league_squads("usa.1", delay = 0.5)
#'
#' # Get all La Liga squads
#' fetch_soccer_league_squads("esp.1")
#'
#' # View squad summary by team
#' library(dplyr)
#' soccer_league_squads %>%
#'   group_by(team_name) %>%
#'   summarise(players = n(), .groups = 'drop')
#' }
#'
fetch_soccer_league_squads <- function(league, raw = FALSE, delay = 0.2) {

  # Validate inputs
  if (missing(league) || !is.character(league) || length(league) != 1) {
    stop("'league' must be a single character string (e.g., 'eng.1', 'usa.1', 'esp.1')")
  }

  if (!is.logical(raw)) {
    stop("'raw' must be TRUE or FALSE")
  }

  if (!is.numeric(delay) || delay < 0) {
    stop("'delay' must be a non-negative number")
  }

  # First, get all teams in the league (using existing comprehensive function)
  message(sprintf("Step 1: Fetching teams list for league: %s...", league))
  teams_data <- fetch_soccer_teams(league, return_type = "teams")

  if (nrow(teams_data) == 0) {
    stop("No teams found for league: ", league)
  }

  message(sprintf("Found %d teams. Now fetching squads...", nrow(teams_data)))

  # Initialize combined data frame
  all_squads <- data.frame()
  failed_teams <- character(0)

  # Fetch squad for each team
  for (i in seq_len(nrow(teams_data))) {
    team_id <- teams_data$team_id[i]
    team_name <- teams_data$team_display_name[i]

    message(sprintf("Fetching squad %d/%d: %s (ID: %s)",
                    i, nrow(teams_data), team_name, team_id))

    tryCatch({
      # Fetch team squad
      squad_data <- fetch_soccer_squad(league, team_id, raw = FALSE)

      # Add to combined dataset
      if (nrow(squad_data) > 0) {
        all_squads <- rbind(all_squads, squad_data)
      }

    }, error = function(e) {
      warning(sprintf("Failed to fetch squad for %s (ID: %s): %s",
                      team_name, team_id, e$message))
      failed_teams <<- c(failed_teams, team_name)
    })

    # Add delay between requests to be respectful to the API
    if (i < nrow(teams_data)) {
      Sys.sleep(delay)
    }
  }

  # Handle raw data assignment
  if (isTRUE(raw)) {
    assign("soccer_league_squads_raw", all_squads, envir = .GlobalEnv)
    message("Raw soccer league squads data assigned to: soccer_league_squads_raw")
    return(invisible(all_squads))
  }

  # Assign to global environment
  assign("soccer_league_squads", all_squads, envir = .GlobalEnv)

  # Create informative message
  league_names <- list(
    "eng.1" = "Premier League", "esp.1" = "La Liga", "ger.1" = "Bundesliga",
    "ita.1" = "Serie A", "fra.1" = "Ligue 1", "usa.1" = "MLS",
    "mex.1" = "Liga MX", "ned.1" = "Eredivisie", "por.1" = "Primeira Liga",
    "bra.1" = "Brasileirão"
  )
  league_display <- if (league %in% names(league_names)) league_names[[league]] else league

  success_teams <- nrow(teams_data) - length(failed_teams)
  total_players <- nrow(all_squads)

  message(sprintf("Completed! Soccer league squads data assigned to: soccer_league_squads"))
  message(sprintf("Successfully fetched %d/%d teams, %d total players (%s)",
                  success_teams, nrow(teams_data), total_players, league_display))

  if (length(failed_teams) > 0) {
    message(sprintf("Failed teams: %s", paste(failed_teams, collapse = ", ")))
  }

  return(invisible(all_squads))
}
