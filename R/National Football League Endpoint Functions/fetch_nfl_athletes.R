#' Fetch NFL athletes data using Core API
#'
#' @param limit Integer. Number of athletes to fetch (default: 100, max: 1000).
#' @param offset Integer. Starting position for pagination (default: 0).
#' @param active Logical. Filter for active players only (default: TRUE).
#' @param team_id Character or Numeric. Filter by specific team ID (optional).
#' @param position Character. Filter by position abbreviation (e.g., "QB", "RB", "WR").
#' @param raw Logical. If TRUE, assigns raw JSON to global environment as 'nfl_athletes_raw'. Default FALSE.
#' @return Invisibly returns the data, but main purpose is global environment assignment.
#' @export
#'
#' @examples
#' # Get first 100 active NFL athletes
#' fetch_nfl_athletes()
#' head(nfl_athletes)
#'
#' # Get specific number of athletes with offset
#' fetch_nfl_athletes(limit = 50, offset = 100)
#'
#' # Get all quarterbacks
#' fetch_nfl_athletes(position = "QB", limit = 500)
#'
#' # Get athletes from specific team
#' fetch_nfl_athletes(team_id = "12", limit = 200)
#'
#' # Get raw data for custom processing
#' fetch_nfl_athletes(limit = 50, raw = TRUE)
#' str(nfl_athletes_raw, max.level = 2)
#'
fetch_nfl_athletes <- function(limit = 100, offset = 0, active = TRUE,
                               team_id = NULL, position = NULL, raw = FALSE) {

  # Validate inputs
  if (!is.numeric(limit) || limit < 1 || limit > 1000) {
    stop("'limit' must be a number between 1 and 1000")
  }

  if (!is.numeric(offset) || offset < 0) {
    stop("'offset' must be a non-negative number")
  }

  if (!is.logical(active)) {
    stop("'active' must be TRUE or FALSE")
  }

  # Build base URL
  base_url <- "https://sports.core.api.espn.com/v3/sports/football/nfl/athletes"

  # Build query parameters
  params <- list(
    limit = limit,
    offset = offset
  )

  # Add optional filters
  if (isTRUE(active)) {
    params$active <- "true"
  }

  if (!is.null(team_id)) {
    params$team <- as.character(team_id)
  }

  if (!is.null(position)) {
    params$position <- toupper(position)
  }

  # Build URL with parameters
  url <- httr::modify_url(base_url, query = params)

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
      assign("nfl_athletes_raw", data, envir = .GlobalEnv)
      message("Raw NFL athletes data assigned to: nfl_athletes_raw")
      return(invisible(data))
    }

    # Create clean athletes dataset
    athletes_df <- create_clean_athletes_dataset(data, params)

    # Assign to global environment
    assign("nfl_athletes", athletes_df, envir = .GlobalEnv)

    # Create informative message
    filter_info <- create_filter_message(params)
    message(sprintf("NFL athletes data assigned to: nfl_athletes (%d athletes%s)",
                    nrow(athletes_df), filter_info))

    return(invisible(athletes_df))

  }, error = function(e) {
    stop(sprintf("Failed to fetch NFL athletes data: %s", e$message))
  })
}

#' Check if an athlete entry is a system placeholder
#'
#' @param athlete_data List containing athlete information
#' @return Logical indicating if this is a system placeholder
is_system_placeholder <- function(athlete_data) {
  # Check for common system placeholder patterns
  if ("lastName" %in% names(athlete_data)) {
    last_name <- athlete_data[["lastName"]]

    # Check for bracket patterns like [35], [Downed], [Touchback]
    if (grepl("^\\[.*\\]$", last_name, perl = TRUE)) {
      return(TRUE)
    }
  }

  if ("displayName" %in% names(athlete_data)) {
    display_name <- athlete_data[["displayName"]]

    # Check for bracket patterns in display name
    if (grepl("^\\s*\\[.*\\]$", display_name, perl = TRUE)) {
      return(TRUE)
    }
  }

  # Check for missing or empty first name (real players should have first names)
  if ("firstName" %in% names(athlete_data)) {
    first_name <- athlete_data[["firstName"]]
    if (is.null(first_name) || nchar(trimws(first_name)) == 0) {
      # Additional check - if lastName is also bracketed, it's likely a placeholder
      if ("lastName" %in% names(athlete_data)) {
        last_name <- athlete_data[["lastName"]]
        if (grepl("^\\[.*\\]$", last_name, perl = TRUE)) {
          return(TRUE)
        }
      }
    }
  } else {
    # No firstName field at all - check if this looks like a system entry
    if ("lastName" %in% names(athlete_data)) {
      last_name <- athlete_data[["lastName"]]
      if (grepl("^\\[.*\\]$", last_name, perl = TRUE)) {
        return(TRUE)
      }
    }
  }

  return(FALSE)
}

#' Create clean athletes dataset from ESPN Core API response
#'
#' @param data Raw JSON response from ESPN Core API athletes endpoint
#' @param params Query parameters used in request
#' @return Clean data frame with athlete information
create_clean_athletes_dataset <- function(data, params) {

  # Initialize result data frame
  result_df <- data.frame(
    athlete_id = character(0),
    uid = character(0),
    guid = character(0),
    first_name = character(0),
    last_name = character(0),
    full_name = character(0),
    display_name = character(0),
    short_name = character(0),
    jersey = character(0),
    position_abbreviation = character(0),
    position_name = character(0),
    height = numeric(0),
    display_height = character(0),
    weight = numeric(0),
    display_weight = character(0),
    age = integer(0),
    birth_date = character(0),
    birth_city = character(0),
    birth_state = character(0),
    birth_country = character(0),
    experience_years = integer(0),
    college_name = character(0),
    college_id = character(0),
    team_id = character(0),
    team_name = character(0),
    team_abbreviation = character(0),
    active = logical(0),
    headshot_href = character(0),
    pro_debut_year = integer(0),
    stringsAsFactors = FALSE
  )

  # Extract athletes from response - check for both "athletes" and "items"
  athletes <- NULL
  if ("athletes" %in% names(data) && length(data[["athletes"]]) > 0) {
    athletes <- data[["athletes"]]
  } else if ("items" %in% names(data) && length(data[["items"]]) > 0) {
    athletes <- data[["items"]]
  }

  if (is.null(athletes) || length(athletes) == 0) {
    return(result_df)
  }

  # Process each athlete
  for (i in seq_along(athletes)) {
    athlete_data <- athletes[[i]]

    # Skip non-player entries (system placeholders)
    if (is_system_placeholder(athlete_data)) {
      next
    }

    # Basic athlete info
    athlete_id <- if ("id" %in% names(athlete_data)) athlete_data[["id"]] else NA_character_
    first_name <- if ("firstName" %in% names(athlete_data)) athlete_data[["firstName"]] else NA_character_
    last_name <- if ("lastName" %in% names(athlete_data)) athlete_data[["lastName"]] else NA_character_
    full_name <- if ("fullName" %in% names(athlete_data)) athlete_data[["fullName"]] else NA_character_
    display_name <- if ("displayName" %in% names(athlete_data)) athlete_data[["displayName"]] else NA_character_
    short_name <- if ("shortName" %in% names(athlete_data)) athlete_data[["shortName"]] else NA_character_
    jersey <- if ("jersey" %in% names(athlete_data)) athlete_data[["jersey"]] else NA_character_
    active <- if ("active" %in% names(athlete_data)) as.logical(athlete_data[["active"]]) else NA
    uid <- if ("uid" %in% names(athlete_data)) athlete_data[["uid"]] else NA_character_
    guid <- if ("guid" %in% names(athlete_data)) athlete_data[["guid"]] else NA_character_

    # Physical attributes
    height <- if ("height" %in% names(athlete_data)) as.numeric(athlete_data[["height"]]) else NA_real_
    display_height <- if ("displayHeight" %in% names(athlete_data)) athlete_data[["displayHeight"]] else NA_character_
    weight <- if ("weight" %in% names(athlete_data)) as.numeric(athlete_data[["weight"]]) else NA_real_
    display_weight <- if ("displayWeight" %in% names(athlete_data)) athlete_data[["displayWeight"]] else NA_character_
    age <- if ("age" %in% names(athlete_data)) as.integer(athlete_data[["age"]]) else NA_integer_

    # Birth information
    birth_date <- birth_city <- birth_state <- birth_country <- NA_character_
    if ("birthDate" %in% names(athlete_data)) {
      birth_date <- athlete_data[["birthDate"]]
    }
    if ("birthPlace" %in% names(athlete_data)) {
      birth_place <- athlete_data[["birthPlace"]]
      birth_city <- if ("city" %in% names(birth_place)) birth_place[["city"]] else NA_character_
      birth_state <- if ("state" %in% names(birth_place)) birth_place[["state"]] else NA_character_
      birth_country <- if ("country" %in% names(birth_place)) birth_place[["country"]] else NA_character_
    }

    # Position info
    position_abbreviation <- position_name <- NA_character_
    if ("position" %in% names(athlete_data)) {
      position <- athlete_data[["position"]]
      position_abbreviation <- if ("abbreviation" %in% names(position)) position[["abbreviation"]] else NA_character_
      position_name <- if ("name" %in% names(position)) position[["name"]] else NA_character_
    }

    # Experience and debut
    experience_years <- NA_integer_
    pro_debut_year <- NA_integer_
    if ("experience" %in% names(athlete_data)) {
      experience <- athlete_data[["experience"]]
      experience_years <- if ("years" %in% names(experience)) as.integer(experience[["years"]]) else NA_integer_
    }
    if ("debutYear" %in% names(athlete_data)) {
      pro_debut_year <- as.integer(athlete_data[["debutYear"]])
    }

    # College information
    college_name <- college_id <- NA_character_
    if ("college" %in% names(athlete_data)) {
      college <- athlete_data[["college"]]
      college_name <- if ("name" %in% names(college)) college[["name"]] else NA_character_
      college_id <- if ("id" %in% names(college)) college[["id"]] else NA_character_
    }

    # Team information
    team_id <- team_name <- team_abbreviation <- NA_character_
    if ("team" %in% names(athlete_data)) {
      team <- athlete_data[["team"]]
      team_id <- if ("id" %in% names(team)) team[["id"]] else NA_character_
      team_name <- if ("name" %in% names(team)) team[["name"]] else NA_character_
      team_abbreviation <- if ("abbreviation" %in% names(team)) team[["abbreviation"]] else NA_character_
    }

    # Headshot
    headshot_href <- NA_character_
    if ("headshot" %in% names(athlete_data)) {
      headshot <- athlete_data[["headshot"]]
      headshot_href <- if ("href" %in% names(headshot)) headshot[["href"]] else NA_character_
    }

    # Create athlete row
    athlete_row <- data.frame(
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
      birth_date = birth_date,
      birth_city = birth_city,
      birth_state = birth_state,
      birth_country = birth_country,
      experience_years = experience_years,
      college_name = college_name,
      college_id = college_id,
      team_id = team_id,
      team_name = team_name,
      team_abbreviation = team_abbreviation,
      active = active,
      headshot_href = headshot_href,
      pro_debut_year = pro_debut_year,
      stringsAsFactors = FALSE
    )

    # Add to result
    result_df <- rbind(result_df, athlete_row)
  }

  # Clean up row names and sort
  if (nrow(result_df) > 0) {
    rownames(result_df) <- NULL

    # Sort by team, then position, then jersey number
    result_df$jersey_numeric <- suppressWarnings(as.numeric(result_df$jersey))
    result_df <- result_df[order(result_df$team_abbreviation,
                                 result_df$position_abbreviation,
                                 result_df$jersey_numeric,
                                 na.last = TRUE), ]
    result_df$jersey_numeric <- NULL
    rownames(result_df) <- NULL
  }

  return(result_df)
}

#' Create informative filter message for console output
#'
#' @param params Query parameters used in request
#' @return Character string describing applied filters
create_filter_message <- function(params) {
  filters <- character(0)

  if ("team" %in% names(params)) {
    filters <- c(filters, sprintf("team: %s", params$team))
  }

  if ("position" %in% names(params)) {
    filters <- c(filters, sprintf("position: %s", params$position))
  }

  if ("active" %in% names(params) && params$active == "true") {
    filters <- c(filters, "active players only")
  }

  if (length(filters) > 0) {
    return(sprintf(", filters: %s", paste(filters, collapse = ", ")))
  }

  return("")
}

#' Fetch all NFL athletes with pagination
#'
#' @param batch_size Integer. Number of athletes to fetch per request (default: 1000).
#' @param max_athletes Integer. Maximum total athletes to fetch (default: 10000).
#' @param active Logical. Filter for active players only (default: TRUE).
#' @param team_id Character or Numeric. Filter by specific team ID (optional).
#' @param position Character. Filter by position abbreviation (optional).
#' @param raw Logical. If TRUE, assigns raw JSON to global environment. Default FALSE.
#' @return Invisibly returns the data, but main purpose is global environment assignment.
#' @export
#'
#' @examples
#' # Get all active NFL athletes
#' fetch_all_nfl_athletes()
#'
#' # Get all quarterbacks
#' fetch_all_nfl_athletes(position = "QB")
#'
fetch_all_nfl_athletes <- function(batch_size = 1000, max_athletes = 10000,
                                   active = TRUE, team_id = NULL, position = NULL,
                                   raw = FALSE) {

  all_athletes <- data.frame()
  offset <- 0
  total_fetched <- 0

  message("Starting to fetch NFL athletes data...")

  while (total_fetched < max_athletes) {
    # Calculate how many to fetch this round
    current_limit <- min(batch_size, max_athletes - total_fetched)

    # Fetch batch
    batch_data <- fetch_nfl_athletes(
      limit = current_limit,
      offset = offset,
      active = active,
      team_id = team_id,
      position = position,
      raw = raw
    )

    # If raw requested, return after first batch
    if (isTRUE(raw)) {
      return(invisible(batch_data))
    }

    # Get the data that was assigned to global environment
    batch_df <- get("nfl_athletes", envir = .GlobalEnv)

    # If no more data, break
    if (nrow(batch_df) == 0) {
      break
    }

    # Add to combined dataset
    all_athletes <- rbind(all_athletes, batch_df)
    total_fetched <- nrow(all_athletes)
    offset <- offset + current_limit

    message(sprintf("Fetched %d athletes so far...", total_fetched))

    # If we got less than requested, we've reached the end
    if (nrow(batch_df) < current_limit) {
      break
    }

    # Small delay to be respectful to the API
    Sys.sleep(0.1)
  }

  # Remove duplicates (just in case)
  all_athletes <- all_athletes[!duplicated(all_athletes$athlete_id), ]

  # Assign final dataset to global environment
  assign("nfl_athletes", all_athletes, envir = .GlobalEnv)

  # Create informative message
  params <- list(active = active, team = team_id, position = position)
  filter_info <- create_filter_message(params)
  message(sprintf("Completed! NFL athletes data assigned to: nfl_athletes (%d total athletes%s)",
                  nrow(all_athletes), filter_info))

  return(invisible(all_athletes))
}
