#' Safe nested data extraction helper function for college football season
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_season <- function(data, path, default = NA) {
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

#' Create college football season data frame from API response
#'
#' Processes raw JSON response from ESPN Common API into structured data frame
#' containing college football season information
#'
#' @param data Raw JSON response from ESPN Common API season endpoint
#' @return Data frame with season information
#' @keywords internal
create_cfb_season_dataset <- function(data) {

  # Initialize season data frame
  season_df <- data.frame(
    season_year = character(0),
    season_start_date = character(0),
    season_end_date = character(0),
    season_display_name = character(0),
    season_type_id = character(0),
    season_type_name = character(0),
    season_type_abbreviation = character(0),
    season_type_start_date = character(0),
    season_type_end_date = character(0),
    season_type_has_standings = character(0),
    stringsAsFactors = FALSE
  )

  # Extract season information directly from root level
  season_year <- extract_nested_season(data, c("year"), NA_character_)
  season_start_date <- extract_nested_season(data, c("startDate"), NA_character_)
  season_end_date <- extract_nested_season(data, c("endDate"), NA_character_)
  season_display_name <- extract_nested_season(data, c("displayName"), NA_character_)

  # Extract season types
  season_types <- extract_nested_season(data, c("types"), list())

  if (length(season_types) > 0) {
    for (season_type in season_types) {
      # Season type information
      season_type_id <- extract_nested_season(season_type, c("id"), NA_character_)
      season_type_name <- extract_nested_season(season_type, c("name"), NA_character_)
      season_type_abbreviation <- extract_nested_season(season_type, c("abbreviation"), NA_character_)
      season_type_start_date <- extract_nested_season(season_type, c("startDate"), NA_character_)
      season_type_end_date <- extract_nested_season(season_type, c("endDate"), NA_character_)
      season_type_has_standings <- extract_nested_season(season_type, c("hasStandings"), "false")

      # Create season type row
      season_row <- data.frame(
        season_year = as.character(season_year),
        season_start_date = as.character(season_start_date),
        season_end_date = as.character(season_end_date),
        season_display_name = as.character(season_display_name),
        season_type_id = as.character(season_type_id),
        season_type_name = as.character(season_type_name),
        season_type_abbreviation = as.character(season_type_abbreviation),
        season_type_start_date = as.character(season_type_start_date),
        season_type_end_date = as.character(season_type_end_date),
        season_type_has_standings = as.character(season_type_has_standings),
        stringsAsFactors = FALSE
      )

      season_df <- rbind(season_df, season_row)
    }
  } else {
    # Create season-only row if no types found
    season_row <- data.frame(
      season_year = as.character(season_year),
      season_start_date = as.character(season_start_date),
      season_end_date = as.character(season_end_date),
      season_display_name = as.character(season_display_name),
      season_type_id = NA_character_,
      season_type_name = NA_character_,
      season_type_abbreviation = NA_character_,
      season_type_start_date = NA_character_,
      season_type_end_date = NA_character_,
      season_type_has_standings = NA_character_,
      stringsAsFactors = FALSE
    )

    season_df <- rbind(season_df, season_row)
  }

  # Clean up row names
  if (nrow(season_df) > 0) rownames(season_df) <- NULL

  return(season_df)
}

#' Fetch college football season information using Common API
#'
#' Retrieves comprehensive college football season information from ESPN's Common API.
#' The function fetches season dates, types (preseason, regular season, postseason),
#' and standings availability for college football seasons.
#'
#' @param year Integer. Season year to fetch (default: current year).
#'   Examples: 2024, 2023, 2025, etc.
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'cfb_season_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data frame. The main purpose is global
#'   environment assignment of \code{cfb_season} containing:
#'   \itemize{
#'     \item Season information: year, start/end dates, display name
#'     \item Season types: preseason, regular season, postseason details
#'     \item Type details: start/end dates, standings availability
#'     \item Administrative info: IDs, abbreviations, names
#'   }
#'
#' @details
#' The function creates a structured data frame with college football season information.
#' Each row represents a season type (preseason, regular, postseason) within a season,
#' providing complete timing and administrative details.
#'
#' **Season Information**:
#' \itemize{
#'   \item Identity: season year and display name
#'   \item Duration: overall season start and end dates
#'   \item Structure: breakdown into season types
#' }
#'
#' **Season Types**:
#' \itemize{
#'   \item Preseason: exhibition games and preparation
#'   \item Regular Season: conference and non-conference games
#'   \item Postseason: bowl games, playoffs, championship
#' }
#'
#' **Administrative Details**:
#' \itemize{
#'   \item Identifiers: type IDs for API calls
#'   \item Names: full names and abbreviations
#'   \item Features: standings availability, special characteristics
#' }
#'
#' @examples
#' \dontrun{
#' # Get current season information
#' fetch_college_football_season()
#'
#' # Get specific season
#' fetch_college_football_season(2024)
#'
#' # Check the data
#' head(cfb_season)
#'
#' # View season structure
#' season_overview <- cfb_season[, c("season_year", "season_type_name",
#'                                  "season_type_start_date", "season_type_end_date")]
#' print("Season structure:")
#' print(season_overview)
#'
#' # Check current season phase
#' current_date <- Sys.Date()
#' cfb_season$type_start <- as.Date(cfb_season$season_type_start_date)
#' cfb_season$type_end <- as.Date(cfb_season$season_type_end_date)
#'
#' current_phase <- cfb_season[
#'   !is.na(cfb_season$type_start) & !is.na(cfb_season$type_end) &
#'   cfb_season$type_start <= current_date & cfb_season$type_end >= current_date,
#'   c("season_year", "season_type_name", "season_type_start_date", "season_type_end_date")
#' ]
#'
#' if(nrow(current_phase) > 0) {
#'   print("Current season phase:")
#'   print(current_phase)
#' } else {
#'   print("No active season phase found for current date")
#' }
#'
#' # Analyze season dates
#' if(require(dplyr, quietly = TRUE)) {
#'   season_summary <- cfb_season %>%
#'     filter(!is.na(season_type_name)) %>%
#'     mutate(
#'       start_date = as.Date(season_type_start_date),
#'       end_date = as.Date(season_type_end_date),
#'       duration_days = as.numeric(end_date - start_date)
#'     ) %>%
#'     select(season_year, season_type_name, start_date, end_date, duration_days) %>%
#'     arrange(start_date)
#'
#'   print("Season phase durations:")
#'   print(season_summary)
#' }
#'
#' # Check standings availability
#' standings_info <- cfb_season[cfb_season$season_type_has_standings == "true",
#'                             c("season_year", "season_type_name", "season_type_has_standings")]
#'
#' if(nrow(standings_info) > 0) {
#'   print("Season types with standings:")
#'   print(standings_info)
#' }
#'
#' # Season type breakdown
#' type_counts <- table(cfb_season$season_type_name[!is.na(cfb_season$season_type_name)])
#' print("Season types available:")
#' print(type_counts)
#'
#' # Calculate days until next season phase
#' upcoming_phases <- cfb_season[
#'   !is.na(cfb_season$season_type_start_date) &
#'   as.Date(cfb_season$season_type_start_date) > current_date,
#'   c("season_year", "season_type_name", "season_type_start_date")
#' ]
#'
#' if(nrow(upcoming_phases) > 0) {
#'   upcoming_phases$days_until <- as.numeric(as.Date(upcoming_phases$season_type_start_date) - current_date)
#'   upcoming_phases <- upcoming_phases[order(upcoming_phases$days_until), ]
#'
#'   print("Upcoming season phases:")
#'   print(head(upcoming_phases))
#' }
#'
#' # Season comparison across years
#' if(length(unique(cfb_season$season_year)) > 1) {
#'   regular_seasons <- cfb_season[cfb_season$season_type_name == "Regular Season" &
#'                                !is.na(cfb_season$season_type_name), ]
#'
#'   if(nrow(regular_seasons) > 0) {
#'     regular_seasons$start_date <- as.Date(regular_seasons$season_type_start_date)
#'     regular_seasons$end_date <- as.Date(regular_seasons$season_type_end_date)
#'     regular_seasons$duration <- as.numeric(regular_seasons$end_date - regular_seasons$start_date)
#'
#'     print("Regular season comparison:")
#'     print(regular_seasons[, c("season_year", "start_date", "end_date", "duration")])
#'   }
#' }
#' }
#'
#' @export
fetch_college_football_season <- function(year = as.integer(format(Sys.Date(), "%Y")), raw = FALSE) {

  # Input validation
  if (!is.numeric(year) || year < 1990 || year > 2030) {
    stop("'year' must be a valid year between 1990 and 2030")
  }

  year <- as.integer(year)

  # Build API URL
  url <- sprintf("https://site.api.espn.com/apis/common/v3/sports/football/college-football/seasons/%d", year)

  # Fetch and parse data
  tryCatch({
    message(sprintf("Fetching college football season information for %d...", year))

    resp <- httr::GET(url, httr::timeout(30))

    if (httr::status_code(resp) != 200) {
      stop(sprintf("API request failed: HTTP %d - %s. Year '%d' may not be available.",
                   httr::status_code(resp),
                   httr::http_status(resp)$message,
                   year))
    }

    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content_text, simplifyVector = FALSE, simplifyDataFrame = FALSE)

    # Handle raw data assignment for debugging
    if (isTRUE(raw)) {
      assign("cfb_season_raw", data, envir = .GlobalEnv)
      message("Raw season data assigned to: cfb_season_raw")
      message("Data structure preview:")

      # Show available sections
      sections <- names(data)
      message("- Available sections: ", paste(sections, collapse = ", "))

      # Show seasons information
      season_year <- extract_nested_season(data, c("year"), "Unknown")
      season_name <- extract_nested_season(data, c("displayName"), "Unknown")
      season_types <- extract_nested_season(data, c("types"), list())

      message(sprintf("- Season: %s (%s)", season_year, season_name))
      message(sprintf("- Season types available: %d", length(season_types)))

      if (length(season_types) > 0) {
        type_names <- sapply(season_types, function(type) {
          extract_nested_season(type, c("name"), "Unknown")
        })
        message(sprintf("- Types: %s", paste(type_names, collapse = ", ")))
      }

      return(invisible(data))
    }

    # Create season dataset
    season_df <- create_cfb_season_dataset(data)

    # Assign to global environment
    assign("cfb_season", season_df, envir = .GlobalEnv)

    # Summary message
    total_entries <- nrow(season_df)

    message(sprintf("Season information assigned to: cfb_season (%d entries)", total_entries))

    if (total_entries > 0) {
      # Show unique seasons
      unique_years <- unique(season_df$season_year[!is.na(season_df$season_year)])
      if (length(unique_years) > 0) {
        message(sprintf("  - Season years: %s", paste(unique_years, collapse = ", ")))
      }

      # Show season types
      unique_types <- unique(season_df$season_type_name[!is.na(season_df$season_type_name)])
      if (length(unique_types) > 0) {
        message(sprintf("  - Season types: %s", paste(unique_types, collapse = ", ")))
      }

      # Show current season phase (if applicable)
      current_date <- Sys.Date()
      current_phase <- season_df[
        !is.na(season_df$season_type_start_date) & !is.na(season_df$season_type_end_date) &
          as.Date(season_df$season_type_start_date) <= current_date &
          as.Date(season_df$season_type_end_date) >= current_date,
      ]

      if (nrow(current_phase) > 0) {
        phase_name <- current_phase$season_type_name[1]
        phase_year <- current_phase$season_year[1]
        message(sprintf("  - Current phase: %s %s", phase_year, phase_name))
      }

      # Show season dates
      season_info <- season_df[!is.na(season_df$season_start_date), ]
      if (nrow(season_info) > 0) {
        season_start <- season_info$season_start_date[1]
        season_end <- season_info$season_end_date[1]
        message(sprintf("  - Season dates: %s to %s",
                        substr(season_start, 1, 10), substr(season_end, 1, 10)))
      }

      # Show standings availability
      with_standings <- sum(season_df$season_type_has_standings == "true", na.rm = TRUE)
      if (with_standings > 0) {
        message(sprintf("  - Season types with standings: %d", with_standings))
      }

      # Show sample season types with dates
      if (total_entries > 0) {
        sample_types <- min(3, total_entries)
        message("\nSeason type details:")
        for (i in 1:sample_types) {
          entry <- season_df[i, ]
          if (!is.na(entry$season_type_name)) {
            start_date <- substr(entry$season_type_start_date, 1, 10)
            end_date <- substr(entry$season_type_end_date, 1, 10)
            message(sprintf("  %s: %s to %s",
                            entry$season_type_name, start_date, end_date))
          }
        }
      }
    }

    return(invisible(season_df))

  }, error = function(e) {
    stop(sprintf("Failed to fetch season information for year %d: %s", year, e$message))
  })
}

#' Fetch season information for multiple college football years
#'
#' Retrieves season information for multiple years with rate limiting.
#' This function calls \code{\link{fetch_college_football_season}} for each year
#' and combines the results.
#'
#' @param years Integer vector. Years to fetch season information for.
#'   Examples: c(2022, 2023, 2024), 2020:2024, etc.
#' @param delay Numeric. Delay in seconds between API requests (default: 0.3).
#'   Used to be respectful to ESPN's servers.
#' @param raw Logical. If TRUE, assigns raw JSON to global environment
#'   for the first year only (default: FALSE).
#'
#' @return Invisibly returns the combined data frame. The main purpose is global
#'   environment assignment of combined \code{cfb_season} from all years.
#'
#' @examples
#' \dontrun{
#' # Get last 3 seasons
#' fetch_multiple_cfb_seasons(2022:2024)
#'
#' # Get specific years
#' fetch_multiple_cfb_seasons(c(2020, 2021, 2023, 2024))
#'
#' # Analyze season patterns across years
#' if(require(dplyr, quietly = TRUE)) {
#'   season_patterns <- cfb_season %>%
#'     filter(!is.na(season_type_name)) %>%
#'     mutate(
#'       start_date = as.Date(season_type_start_date),
#'       end_date = as.Date(season_type_end_date),
#'       duration = as.numeric(end_date - start_date),
#'       start_month = format(start_date, "%B"),
#'       end_month = format(end_date, "%B")
#'     ) %>%
#'     group_by(season_type_name) %>%
#'     summarise(
#'       avg_duration = round(mean(duration, na.rm = TRUE)),
#'       typical_start = names(sort(table(start_month), decreasing = TRUE))[1],
#'       typical_end = names(sort(table(end_month), decreasing = TRUE))[1],
#'       years_available = n_distinct(season_year),
#'       .groups = 'drop'
#'     )
#'
#'   print("Season patterns across years:")
#'   print(season_patterns)
#' }
#' }
#'
#' @seealso \code{\link{fetch_college_football_season}} for single year data
#' @export
fetch_multiple_cfb_seasons <- function(years, delay = 0.3, raw = FALSE) {
  # Input validation
  if (length(years) == 0) {
    stop("'years' must contain at least one year")
  }

  if (!all(is.numeric(years)) || any(years < 1990) || any(years > 2030)) {
    stop("All 'years' must be between 1990 and 2030")
  }

  if (!is.numeric(delay) || delay < 0) {
    stop("'delay' must be a non-negative numeric value")
  }

  # Initialize combined data container
  all_seasons <- data.frame()

  message(sprintf("Starting to fetch season information for %d years...", length(years)))

  # Process each year sequentially
  for (i in seq_along(years)) {
    year <- years[i]
    message(sprintf("Fetching season %d (%d/%d)...", year, i, length(years)))

    tryCatch({
      # Fetch individual year data
      season_data <- fetch_college_football_season(
        year = year,
        raw = raw
      )

      # If raw data requested, return after first year
      if (isTRUE(raw)) {
        return(invisible(season_data))
      }

      # Combine data
      if (exists("cfb_season", envir = .GlobalEnv)) {
        season_df <- get("cfb_season", envir = .GlobalEnv)
        all_seasons <- rbind(all_seasons, season_df)
      }

    }, error = function(e) {
      message(sprintf("Failed to fetch season %d: %s", year, e$message))
    })

    # Be respectful to the API with delay between requests
    if (i < length(years)) {
      Sys.sleep(delay)
    }
  }

  # Assign combined dataset to global environment
  if (nrow(all_seasons) > 0) {
    # Remove duplicates based on year and season type
    all_seasons <- all_seasons[!duplicated(paste(all_seasons$season_year, all_seasons$season_type_id)), ]
    assign("cfb_season", all_seasons, envir = .GlobalEnv)

    total_entries <- nrow(all_seasons)
    unique_years <- length(unique(all_seasons$season_year))
    unique_types <- length(unique(all_seasons$season_type_name[!is.na(all_seasons$season_type_name)]))

    message(sprintf("Combined season information assigned to: cfb_season (%d entries, %d years, %d types)",
                    total_entries, unique_years, unique_types))

    # Show year range
    year_range <- range(as.numeric(all_seasons$season_year), na.rm = TRUE)
    message(sprintf("  - Year range: %d to %d", year_range[1], year_range[2]))

    # Show season types
    type_counts <- table(all_seasons$season_type_name[!is.na(all_seasons$season_type_name)])
    if (length(type_counts) > 0) {
      message("  - Season types:")
      for (type_name in names(type_counts)) {
        message(sprintf("    %s: %d seasons", type_name, type_counts[type_name]))
      }
    }
  }

  return(invisible(all_seasons))
}
