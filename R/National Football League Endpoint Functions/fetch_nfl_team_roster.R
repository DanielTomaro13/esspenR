#' Fetch NFL team roster data using Site API
#'
#' @param team_id Character or Numeric. ESPN team ID (e.g., "12" for Kansas City Chiefs).
#' @param raw Logical. If TRUE, assigns raw JSON to global environment as 'nfl_roster_raw'. Default FALSE.
#' @return Invisibly returns the data, but main purpose is global environment assignment.
#' @export
#'
#' @examples
#' # Get roster for Kansas City Chiefs
#' fetch_nfl_team_roster(team_id = "12")
#' head(nfl_roster)
#'
#' # Get Buffalo Bills roster
#' fetch_nfl_team_roster(team_id = 2)
#'
#' # Get raw data for custom processing
#' fetch_nfl_team_roster(team_id = "12", raw = TRUE)
#' str(nfl_roster_raw, max.level = 2)
#'
fetch_nfl_team_roster <- function(team_id, raw = FALSE) {
  # Validate inputs
  if (missing(team_id)) {
    stop("'team_id' is a required parameter")
  }

  # Convert team_id to character for URL building
  team_id <- as.character(team_id)

  # Build URL
  url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/football/nfl/teams/%s/roster", team_id)

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
      assign("nfl_roster_raw", data, envir = .GlobalEnv)
      message("Raw NFL roster data assigned to: nfl_roster_raw")
      return(invisible(data))
    }

    # Create clean roster dataset
    roster_df <- create_clean_roster_dataset(data, team_id)

    # Assign to global environment
    assign("nfl_roster", roster_df, envir = .GlobalEnv)

    message(sprintf("NFL roster data assigned to: nfl_roster (Team ID: %s, %d players)",
                    team_id, nrow(roster_df)))

    return(invisible(roster_df))

  }, error = function(e) {
    stop(sprintf("Failed to fetch NFL roster data for team %s: %s", team_id, e$message))
  })
}

#' Create clean roster dataset from ESPN Site API response
#'
#' @param data Raw JSON response from ESPN Site API roster endpoint
#' @param team_id Team ID used in request
#' @return Clean data frame with roster information
create_clean_roster_dataset <- function(data, team_id) {
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
    experience_years = integer(0),
    college_name = character(0),
    birth_city = character(0),
    birth_state = character(0),
    headshot_href = character(0),
    stringsAsFactors = FALSE
  )

  # Extract team information
  team_info <- if ("team" %in% names(data)) data[["team"]] else list()
  team_id_api <- if ("id" %in% names(team_info)) team_info[["id"]] else team_id

  # Process athletes from data$athletes - organized by position groups
  athletes_groups <- NULL
  if ("athletes" %in% names(data) && length(data[["athletes"]]) > 0) {
    athletes_groups <- data[["athletes"]]
  }

  if (is.null(athletes_groups) || length(athletes_groups) == 0) {
    return(result_df)
  }

  # Process each athlete group (offense, defense, special teams, etc.)
  for (group_idx in seq_along(athletes_groups)) {
    group <- athletes_groups[[group_idx]]

    # Get group position (offense, defense, specialTeam, etc.)
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
        birth_city <- birth_state <- NA_character_
        if ("birthPlace" %in% names(athlete_data)) {
          birth_place <- athlete_data[["birthPlace"]]
          birth_city <- if ("city" %in% names(birth_place)) birth_place[["city"]] else NA_character_
          birth_state <- if ("state" %in% names(birth_place)) birth_place[["state"]] else NA_character_
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
          experience_years = experience_years,
          college_name = college_name,
          birth_city = birth_city,
          birth_state = birth_state,
          headshot_href = headshot_href,
          stringsAsFactors = FALSE
        )

        # Add to result
        result_df <- rbind(result_df, athlete_row)
      }
    }
  }

  # Clean up row names and sort by jersey number
  if (nrow(result_df) > 0) {
    rownames(result_df) <- NULL
    # Sort by position, then jersey number
    result_df$jersey_numeric <- suppressWarnings(as.numeric(result_df$jersey))
    result_df <- result_df[order(result_df$position_abbreviation, result_df$jersey_numeric, na.last = TRUE), ]
    result_df$jersey_numeric <- NULL
    rownames(result_df) <- NULL
  }

  return(result_df)
}
