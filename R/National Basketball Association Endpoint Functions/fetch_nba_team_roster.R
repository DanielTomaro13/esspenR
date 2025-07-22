#' Fetch nba team roster data using Site API
#'
#' @param team_id Character or Numeric. ESPN team ID (e.g., "12" for Los Angeles Clipperss).
#' @param raw Logical. If TRUE, assigns raw JSON to global environment as 'nba_roster_raw'. Default FALSE.
#' @return Invisibly returns the data, but main purpose is global environment assignment.
#' @export
#'
#' @examples
#' # Get roster for Los Angeles Clipperss
#' fetch_nba_team_roster(team_id = "12")
#' head(nba_roster)
#'
#'
#' # Get raw data for custom processing
#' fetch_nba_team_roster(team_id = "12", raw = TRUE)
#' str(nba_roster_raw, max.level = 2)
#'
fetch_nba_team_roster <- function(team_id, raw = FALSE) {
  # Validate inputs
  if (missing(team_id)) {
    stop("'team_id' is a required parameter")
  }

  # Convert team_id to character for URL building
  team_id <- as.character(team_id)

  # Build URL
  url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/basketball/nba/teams/%s/roster", team_id)

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
      assign("nba_roster_raw", data, envir = .GlobalEnv)
      message("Raw nba roster data assigned to: nba_roster_raw")
      return(invisible(data))
    }

    # Create clean roster dataset
    roster_df <- create_clean_roster_dataset(data, team_id)

    # Assign to global environment
    assign("nba_roster", roster_df, envir = .GlobalEnv)

    message(sprintf("nba roster data assigned to: nba_roster (Team ID: %s, %d players)",
                    team_id, nrow(roster_df)))

    return(invisible(roster_df))

  }, error = function(e) {
    stop(sprintf("Failed to fetch nba roster data for team %s: %s", team_id, e$message))
  })
}

#' Create clean roster dataset from ESPN Site API response
#'
#' @param data Raw JSON response from ESPN Site API roster endpoint
#' @param team_id Team ID used in request
#' @return Clean data frame with roster information
create_clean_roster_dataset <- function(data, team_id) {
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
    experience_years = integer(0),
    college_name = character(0),
    birth_city = character(0),
    birth_state = character(0),
    headshot_href = character(0),
    stringsAsFactors = FALSE
  )

  team_info <- if ("team" %in% names(data)) data[["team"]] else list()
  team_id_api <- if ("id" %in% names(team_info)) team_info[["id"]] else team_id

  athletes <- data[["athletes"]]
  if (is.null(athletes) || length(athletes) == 0) return(result_df)

  for (athlete_data in athletes) {
    athlete_id <- if (!is.null(athlete_data[["id"]])) athlete_data[["id"]] else NA_character_
    first_name <- if (!is.null(athlete_data[["firstName"]])) athlete_data[["firstName"]] else NA_character_
    last_name <- if (!is.null(athlete_data[["lastName"]])) athlete_data[["lastName"]] else NA_character_
    full_name <- if (!is.null(athlete_data[["fullName"]])) athlete_data[["fullName"]] else NA_character_
    display_name <- if (!is.null(athlete_data[["displayName"]])) athlete_data[["displayName"]] else NA_character_
    jersey <- if (!is.null(athlete_data[["jersey"]])) athlete_data[["jersey"]] else NA_character_

    height <- if (!is.null(athlete_data[["height"]])) as.numeric(athlete_data[["height"]]) else NA_real_
    display_height <- if (!is.null(athlete_data[["displayHeight"]])) athlete_data[["displayHeight"]] else NA_character_
    weight <- if (!is.null(athlete_data[["weight"]])) as.numeric(athlete_data[["weight"]]) else NA_real_
    display_weight <- if (!is.null(athlete_data[["displayWeight"]])) athlete_data[["displayWeight"]] else NA_character_
    age <- if (!is.null(athlete_data[["age"]])) as.integer(athlete_data[["age"]]) else NA_integer_

    experience_years <- NA_integer_
    if (!is.null(athlete_data[["experience"]])) {
      exp <- athlete_data[["experience"]]
      if (!is.null(exp[["years"]])) experience_years <- as.integer(exp[["years"]])
    }

    position_abbreviation <- NA_character_
    position_name <- NA_character_
    if (!is.null(athlete_data[["position"]])) {
      pos <- athlete_data[["position"]]
      if (!is.null(pos[["abbreviation"]])) position_abbreviation <- pos[["abbreviation"]]
      if (!is.null(pos[["name"]])) position_name <- pos[["name"]]
    }

    birth_city <- birth_state <- NA_character_
    if (!is.null(athlete_data[["birthPlace"]])) {
      birth <- athlete_data[["birthPlace"]]
      if (!is.null(birth[["city"]])) birth_city <- birth[["city"]]
      if (!is.null(birth[["state"]])) birth_state <- birth[["state"]]
    }

    college_name <- NA_character_
    if (!is.null(athlete_data[["college"]])) {
      col <- athlete_data[["college"]]
      if (!is.null(col[["name"]])) college_name <- col[["name"]]
    }

    headshot_href <- NA_character_
    if (!is.null(athlete_data[["headshot"]])) {
      head <- athlete_data[["headshot"]]
      if (!is.null(head[["href"]])) headshot_href <- head[["href"]]
    }

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
      experience_years = experience_years,
      college_name = college_name,
      birth_city = birth_city,
      birth_state = birth_state,
      headshot_href = headshot_href,
      stringsAsFactors = FALSE
    )

    result_df <- rbind(result_df, athlete_row)
  }

  if (nrow(result_df) > 0) {
    rownames(result_df) <- NULL
    result_df$jersey_numeric <- suppressWarnings(as.numeric(result_df$jersey))
    result_df <- result_df[order(result_df$position_abbreviation, result_df$jersey_numeric, na.last = TRUE), ]
    result_df$jersey_numeric <- NULL
    rownames(result_df) <- NULL
  }

  return(result_df)
}

