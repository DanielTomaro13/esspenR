#' Safe nested data extraction helper function for NFL leaders
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_leaders <- function(data, path, default = NA) {
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

#' Fetch athlete details from Core API reference URL
#'
#' Retrieves detailed athlete information from ESPN's Core API reference URL
#' @param athlete_ref Character. Full athlete reference URL
#' @param timeout Numeric. Request timeout in seconds (default: 30)
#' @return List with athlete details or NAs if failed
#' @keywords internal
fetch_athlete_details <- function(athlete_ref, timeout = 30) {
  if (is.na(athlete_ref) || athlete_ref == "" || is.null(athlete_ref)) {
    return(list(
      athlete_id = NA_character_,
      athlete_name = NA_character_,
      athlete_display_name = NA_character_,
      athlete_short_name = NA_character_,
      athlete_jersey = NA_character_,
      athlete_position_ref = NA_character_,
      athlete_position_name = NA_character_,
      athlete_position_abbreviation = NA_character_,
      team_ref = NA_character_,
      team_id = NA_character_,
      team_name = NA_character_,
      team_display_name = NA_character_,
      team_abbreviation = NA_character_
    ))
  }

  tryCatch({
    resp <- httr::GET(athlete_ref, httr::timeout(timeout))

    if (httr::status_code(resp) != 200) {
      return(list(
        athlete_id = NA_character_,
        athlete_name = NA_character_,
        athlete_display_name = NA_character_,
        athlete_short_name = NA_character_,
        athlete_jersey = NA_character_,
        athlete_position_ref = NA_character_,
        athlete_position_name = NA_character_,
        athlete_position_abbreviation = NA_character_,
        team_ref = NA_character_,
        team_id = NA_character_,
        team_name = NA_character_,
        team_display_name = NA_character_,
        team_abbreviation = NA_character_
      ))
    }

    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    athlete_data <- jsonlite::fromJSON(content_text, simplifyVector = FALSE, simplifyDataFrame = FALSE)

    # Extract athlete information
    athlete_id <- extract_nested_leaders(athlete_data, c("id"), NA_character_)
    athlete_name <- extract_nested_leaders(athlete_data, c("fullName"), NA_character_)
    athlete_display_name <- extract_nested_leaders(athlete_data, c("displayName"), athlete_name)
    athlete_short_name <- extract_nested_leaders(athlete_data, c("shortName"), NA_character_)
    athlete_jersey <- extract_nested_leaders(athlete_data, c("jersey"), NA_character_)

    # Extract position information
    position_info <- extract_nested_leaders(athlete_data, c("position"), list())
    athlete_position_ref <- extract_nested_leaders(position_info, c("$ref"), NA_character_)
    athlete_position_name <- extract_nested_leaders(position_info, c("name"), NA_character_)
    athlete_position_abbreviation <- extract_nested_leaders(position_info, c("abbreviation"), NA_character_)

    # Extract team information
    team_info <- extract_nested_leaders(athlete_data, c("team"), list())
    team_ref <- extract_nested_leaders(team_info, c("$ref"), NA_character_)
    team_id <- extract_nested_leaders(team_info, c("id"), NA_character_)
    team_name <- extract_nested_leaders(team_info, c("name"), NA_character_)
    team_display_name <- extract_nested_leaders(team_info, c("displayName"), team_name)
    team_abbreviation <- extract_nested_leaders(team_info, c("abbreviation"), NA_character_)

    return(list(
      athlete_id = as.character(athlete_id),
      athlete_name = as.character(athlete_name),
      athlete_display_name = as.character(athlete_display_name),
      athlete_short_name = as.character(athlete_short_name),
      athlete_jersey = as.character(athlete_jersey),
      athlete_position_ref = as.character(athlete_position_ref),
      athlete_position_name = as.character(athlete_position_name),
      athlete_position_abbreviation = as.character(athlete_position_abbreviation),
      team_ref = as.character(team_ref),
      team_id = as.character(team_id),
      team_name = as.character(team_name),
      team_display_name = as.character(team_display_name),
      team_abbreviation = as.character(team_abbreviation)
    ))

  }, error = function(e) {
    return(list(
      athlete_id = NA_character_,
      athlete_name = NA_character_,
      athlete_display_name = NA_character_,
      athlete_short_name = NA_character_,
      athlete_jersey = NA_character_,
      athlete_position_ref = NA_character_,
      athlete_position_name = NA_character_,
      athlete_position_abbreviation = NA_character_,
      team_ref = NA_character_,
      team_id = NA_character_,
      team_name = NA_character_,
      team_display_name = NA_character_,
      team_abbreviation = NA_character_
    ))
  })
}

#' Create NFL leaders data frames from Core API response
#'
#' Processes raw JSON response from ESPN Core API into structured data frames
#' containing detailed league leader information
#'
#' @param data Raw JSON response from ESPN Core API leaders endpoint
#' @param year Character. Season year used in request
#' @param season_type Character. Season type used in request
#' @param fetch_athlete_info Logical. Whether to fetch detailed athlete info from reference URLs (default: TRUE)
#' @param athlete_delay Numeric. Delay between athlete API requests in seconds (default: 0.1)
#' @return List containing multiple data frames with leader information
#' @keywords internal
create_nflleaders_datasets <- function(data, year, season_type, fetch_athlete_info = TRUE, athlete_delay = 0.1) {

  # Initialize leaders data frame
  leaders_df <- data.frame(
    season_year = character(0),
    season_type = character(0),
    category_name = character(0),
    category_display_name = character(0),
    category_short_display_name = character(0),
    category_abbreviation = character(0),
    athlete_ref = character(0),
    athlete_id = character(0),
    athlete_name = character(0),
    athlete_display_name = character(0),
    athlete_short_name = character(0),
    athlete_jersey = character(0),
    athlete_position_ref = character(0),
    athlete_position_name = character(0),
    athlete_position_abbreviation = character(0),
    team_ref = character(0),
    team_id = character(0),
    team_name = character(0),
    team_display_name = character(0),
    team_abbreviation = character(0),
    rank = character(0),
    stat_value = character(0),
    stat_display_value = character(0),
    stringsAsFactors = FALSE
  )

  # Initialize categories summary data frame
  categories_df <- data.frame(
    season_year = character(0),
    season_type = character(0),
    category_name = character(0),
    category_display_name = character(0),
    category_short_display_name = character(0),
    category_abbreviation = character(0),
    total_leaders = character(0),
    stringsAsFactors = FALSE
  )

  # Initialize summary data frame
  summary_df <- data.frame(
    season_year = character(0),
    season_type = character(0),
    total_categories = character(0),
    total_leaders = character(0),
    request_url = character(0),
    stringsAsFactors = FALSE
  )

  # Extract categories directly (no items wrapper based on your structure)
  categories <- extract_nested_leaders(data, c("categories"), list())
  total_categories <- length(categories)
  total_leaders <- 0

  if (length(categories) > 0) {
    for (i in seq_along(categories)) {
      category_data <- categories[[i]]

      # Extract category information
      category_name <- extract_nested_leaders(category_data, c("name"), paste("category", i, sep = "_"))
      category_display_name <- extract_nested_leaders(category_data, c("displayName"), category_name)
      category_short_display_name <- extract_nested_leaders(category_data, c("shortDisplayName"), category_display_name)
      category_abbreviation <- extract_nested_leaders(category_data, c("abbreviation"), NA_character_)

      # Extract leaders for this category
      leaders <- extract_nested_leaders(category_data, c("leaders"), list())
      category_total_leaders <- length(leaders)

      # Add category summary row
      category_row <- data.frame(
        season_year = as.character(year),
        season_type = as.character(season_type),
        category_name = as.character(category_name),
        category_display_name = as.character(category_display_name),
        category_short_display_name = as.character(category_short_display_name),
        category_abbreviation = as.character(category_abbreviation),
        total_leaders = as.character(category_total_leaders),
        stringsAsFactors = FALSE
      )

      categories_df <- rbind(categories_df, category_row)

      message(sprintf("Processing category %d/%d: %s (%d leaders)...",
                      i, length(categories), category_display_name, category_total_leaders))

      # Process each leader in the category
      for (j in seq_along(leaders)) {
        leader_data <- leaders[[j]]

        # Extract rank and stat value
        rank <- extract_nested_leaders(leader_data, c("rank"), as.character(j))
        stat_value <- extract_nested_leaders(leader_data, c("value"), NA_character_)
        stat_display_value <- extract_nested_leaders(leader_data, c("displayValue"), stat_value)

        # Extract athlete reference
        athlete_info <- extract_nested_leaders(leader_data, c("athlete"), list())
        athlete_ref <- extract_nested_leaders(athlete_info, c("$ref"), NA_character_)

        # Initialize athlete details with NAs
        athlete_details <- list(
          athlete_id = NA_character_,
          athlete_name = NA_character_,
          athlete_display_name = NA_character_,
          athlete_short_name = NA_character_,
          athlete_jersey = NA_character_,
          athlete_position_ref = NA_character_,
          athlete_position_name = NA_character_,
          athlete_position_abbreviation = NA_character_,
          team_ref = NA_character_,
          team_id = NA_character_,
          team_name = NA_character_,
          team_display_name = NA_character_,
          team_abbreviation = NA_character_
        )

        # Fetch detailed athlete information if requested and athlete_ref is available
        if (fetch_athlete_info && !is.na(athlete_ref) && athlete_ref != "") {
          if (j %% 10 == 0) {
            message(sprintf("  Fetching athlete details %d/%d...", j, length(leaders)))
          }

          athlete_details <- fetch_athlete_details(athlete_ref)

          # Add delay to be respectful to the API
          if (j < length(leaders)) {
            Sys.sleep(athlete_delay)
          }
        }

        # Add leader row
        leader_row <- data.frame(
          season_year = as.character(year),
          season_type = as.character(season_type),
          category_name = as.character(category_name),
          category_display_name = as.character(category_display_name),
          category_short_display_name = as.character(category_short_display_name),
          category_abbreviation = as.character(category_abbreviation),
          athlete_ref = as.character(athlete_ref),
          athlete_id = as.character(athlete_details$athlete_id),
          athlete_name = as.character(athlete_details$athlete_name),
          athlete_display_name = as.character(athlete_details$athlete_display_name),
          athlete_short_name = as.character(athlete_details$athlete_short_name),
          athlete_jersey = as.character(athlete_details$athlete_jersey),
          athlete_position_ref = as.character(athlete_details$athlete_position_ref),
          athlete_position_name = as.character(athlete_details$athlete_position_name),
          athlete_position_abbreviation = as.character(athlete_details$athlete_position_abbreviation),
          team_ref = as.character(athlete_details$team_ref),
          team_id = as.character(athlete_details$team_id),
          team_name = as.character(athlete_details$team_name),
          team_display_name = as.character(athlete_details$team_display_name),
          team_abbreviation = as.character(athlete_details$team_abbreviation),
          rank = as.character(rank),
          stat_value = as.character(stat_value),
          stat_display_value = as.character(stat_display_value),
          stringsAsFactors = FALSE
        )

        leaders_df <- rbind(leaders_df, leader_row)
        total_leaders <- total_leaders + 1
      }
    }
  }

  # Add summary row
  summary_row <- data.frame(
    season_year = as.character(year),
    season_type = as.character(season_type),
    total_categories = as.character(total_categories),
    total_leaders = as.character(total_leaders),
    request_url = NA_character_,
    stringsAsFactors = FALSE
  )

  summary_df <- rbind(summary_df, summary_row)

  # Clean up row names
  if (nrow(summary_df) > 0) rownames(summary_df) <- NULL
  if (nrow(categories_df) > 0) rownames(categories_df) <- NULL
  if (nrow(leaders_df) > 0) rownames(leaders_df) <- NULL

  return(list(
    summary = summary_df,
    categories = categories_df,
    leaders = leaders_df
  ))
}

#' Fetch NFL league leaders data using Core API
#'
#' Retrieves detailed league leader information from ESPN's Core API.
#' The function fetches comprehensive statistical leaders across multiple
#' categories for a specific season year and type.
#'
#' @param year Character or Numeric. Season year (default: 2024).
#'   The year of the NFL season to retrieve leaders for.
#' @param season_type Character or Numeric. Season type (default: 2).
#'   Options are:
#'   \itemize{
#'     \item 1 - Preseason
#'     \item 2 - Regular Season
#'     \item 3 - Postseason
#'   }
#' @param return_type Character. Type of data to return (default: "all").
#'   Options are:
#'   \itemize{
#'     \item "summary" - Basic summary information
#'     \item "categories" - Statistical categories information
#'     \item "leaders" - Individual leader data
#'     \item "all" - All data types combined
#'   }
#' @param fetch_athlete_info Logical. Whether to fetch detailed athlete info
#'   from reference URLs (default: TRUE). This makes additional API calls.
#' @param athlete_delay Numeric. Delay between athlete API requests in seconds
#'   (default: 0.1). Used to be respectful to ESPN's servers.
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'nfl_leaders_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data. The main purpose is global
#'   environment assignment based on return_type:
#'   \itemize{
#'     \item \code{nfl_leaders_summary} - Summary data frame
#'     \item \code{nfl_leaders_categories} - Categories data frame
#'     \item \code{nfl_leaders_leaders} - Leaders data frame
#'     \item \code{nfl_leaders_raw} - Raw JSON response (if raw = TRUE)
#'   }
#'
#' @details
#' The function creates structured data frames with comprehensive league leader information.
#' When \code{fetch_athlete_info = TRUE}, the function will make additional API calls
#' to retrieve detailed athlete information including names, positions, teams, and jersey numbers.
#' This significantly increases the data quality but also increases processing time and API load.
#'
#' **Summary** (\code{nfl_leaders_summary}):
#' \itemize{
#'   \item Request metadata: season_year, season_type
#'   \item Overall statistics: total_categories, total_leaders
#' }
#'
#' **Categories** (\code{nfl_leaders_categories}):
#' \itemize{
#'   \item Category details: name, display_name, abbreviation
#'   \item Category statistics: total_leaders per category
#' }
#'
#' **Leaders** (\code{nfl_leaders_leaders}):
#' \itemize{
#'   \item Player details: athlete_id, name, position, jersey, team info
#'   \item Statistical data: category, rank, value, display_value
#'   \item API references: athlete_ref, team_ref for further queries
#' }
#'
#' @examples
#' \dontrun{
#' # Get complete leaders data for 2024 regular season with athlete details
#' fetch_nfl_leaders()
#'
#' # Get leaders without fetching athlete details (faster)
#' fetch_nfl_leaders(fetch_athlete_info = FALSE)
#'
#' # Get leaders with slower, more respectful API calls
#' fetch_nfl_leaders(athlete_delay = 0.2)
#'
#' # Check what data was created
#' head(nfl_leaders_summary)
#' head(nfl_leaders_categories)
#' head(nfl_leaders_leaders)
#'
#' # Analyze passing leaders with full details
#' passing_leaders <- nfl_leaders_leaders[
#'   nfl_leaders_leaders$category_name == "passingYards",
#'   c("rank", "athlete_display_name", "team_abbreviation",
#'     "athlete_position_abbreviation", "stat_display_value")
#' ]
#' head(passing_leaders, 10)
#'
#' # Team representation in leadership
#' team_leaders <- table(nfl_leaders_leaders$team_abbreviation)
#' team_leaders_sorted <- sort(team_leaders, decreasing = TRUE)
#' print("Teams with most statistical leaders:")
#' head(team_leaders_sorted, 10)
#' }
#'
#' @seealso \code{\link{fetch_multiple_nfl_leaders}} for fetching
#'   multiple seasons/types
#'
#' @export
fetch_nfl_leaders <- function(year = 2024, season_type = 2, return_type = "all",
                              fetch_athlete_info = TRUE, athlete_delay = 0.1, raw = FALSE) {

  # Input validation
  valid_season_types <- c(1, 2, 3)
  season_type <- as.numeric(season_type)
  if (!season_type %in% valid_season_types) {
    stop(sprintf("'season_type' must be one of: %s", paste(valid_season_types, collapse = ", ")))
  }

  valid_types <- c("summary", "categories", "leaders", "all")
  if (!return_type %in% valid_types) {
    stop(sprintf("'return_type' must be one of: %s", paste(valid_types, collapse = ", ")))
  }

  if (!is.logical(fetch_athlete_info)) {
    stop("'fetch_athlete_info' must be TRUE or FALSE")
  }

  if (!is.numeric(athlete_delay) || athlete_delay < 0) {
    stop("'athlete_delay' must be a non-negative numeric value")
  }

  # Convert year to character for URL building
  year <- as.character(year)

  # Build API URL for Core API
  url <- sprintf("https://sports.core.api.espn.com/v2/sports/football/leagues/nfl/seasons/%s/types/%d/leaders",
                 year, season_type)

  # Fetch and parse data
  tryCatch({
    resp <- httr::GET(url, httr::timeout(60))

    if (httr::status_code(resp) != 200) {
      stop(sprintf("API request failed: HTTP %d - %s",
                   httr::status_code(resp),
                   httr::http_status(resp)$message))
    }

    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content_text, simplifyVector = FALSE, simplifyDataFrame = FALSE)

    # Handle raw data assignment for debugging
    if (isTRUE(raw)) {
      assign("nfl_leaders_raw", data, envir = .GlobalEnv)
      message("Raw NFL leaders data assigned to: nfl_leaders_raw")
      message("Data structure preview:")

      # Show available sections
      sections <- names(data)
      message("- Available sections: ", paste(sections, collapse = ", "))

      # Show categories info
      categories <- extract_nested_leaders(data, c("categories"), list())
      if (length(categories) > 0) {
        message("- Total categories: ", length(categories))

        # Show first few category names
        category_names <- character(0)
        for (i in seq_len(min(5, length(categories)))) {
          cat_name <- extract_nested_leaders(categories[[i]], c("name"), paste("category", i))
          category_names <- c(category_names, cat_name)
        }
        message("- Sample categories: ", paste(category_names, collapse = ", "))
      }

      return(invisible(data))
    }

    # Warn user about athlete info fetching
    if (fetch_athlete_info) {
      categories <- extract_nested_leaders(data, c("categories"), list())
      estimated_requests <- sum(sapply(categories, function(cat) {
        leaders <- extract_nested_leaders(cat, c("leaders"), list())
        length(leaders)
      }))

      message(sprintf("Fetching athlete details will make approximately %d additional API requests...",
                      estimated_requests))
      message("This may take several minutes. Set fetch_athlete_info=FALSE for faster results.")
    }

    # Create datasets
    datasets <- create_nflleaders_datasets(data, year, season_type, fetch_athlete_info, athlete_delay)

    # Update summary with request URL
    datasets$summary$request_url <- url

    # Assign data based on return_type
    result_data <- list()

    if (return_type %in% c("summary", "all")) {
      assign("nfl_leaders_summary", datasets$summary, envir = .GlobalEnv)
      result_data$summary <- datasets$summary
      message(sprintf("NFL leaders summary assigned to: nfl_leaders_summary (%d records)", nrow(datasets$summary)))
    }

    if (return_type %in% c("categories", "all")) {
      assign("nfl_leaders_categories", datasets$categories, envir = .GlobalEnv)
      result_data$categories <- datasets$categories
      message(sprintf("NFL leaders categories assigned to: nfl_leaders_categories (%d categories)", nrow(datasets$categories)))
    }

    if (return_type %in% c("leaders", "all")) {
      assign("nfl_leaders_leaders", datasets$leaders, envir = .GlobalEnv)
      result_data$leaders <- datasets$leaders

      unique_athletes <- length(unique(datasets$leaders$athlete_id[!is.na(datasets$leaders$athlete_id)]))
      message(sprintf("NFL leaders assigned to: nfl_leaders_leaders (%d athletes, %d leader entries)",
                      unique_athletes, nrow(datasets$leaders)))
    }

    # Return appropriate data structure
    if (return_type == "all") {
      return(invisible(result_data))
    } else {
      return(invisible(result_data[[return_type]]))
    }

  }, error = function(e) {
    stop(sprintf("Failed to fetch NFL leaders for year %s, season type %s: %s",
                 year, season_type, e$message))
  })
}

#' Fetch multiple NFL leaders data across years and season types
#'
#' Retrieves leaders data for multiple years and season types with rate limiting to
#' be respectful to ESPN's Core API. This function calls \code{\link{fetch_nfl_leaders}}
#' for each combination and combines the results.
#'
#' @param years Character or Numeric vector. Season years to fetch (default: 2024).
#'   Vector of years for the NFL seasons to retrieve leaders for.
#' @param season_types Numeric vector. Season types to fetch (default: c(2)).
#'   Vector of season type identifiers (1=Preseason, 2=Regular, 3=Postseason).
#' @param return_type Character. Type of data to return (default: "all").
#'   Same options as \code{\link{fetch_nfl_leaders}}.
#' @param fetch_athlete_info Logical. Whether to fetch detailed athlete info
#'   from reference URLs (default: TRUE). This makes many additional API calls.
#' @param athlete_delay Numeric. Delay between athlete API requests in seconds
#'   (default: 0.1). Used to be respectful to ESPN's servers.
#' @param delay Numeric. Delay in seconds between main API requests (default: 0.2).
#'   Used to be respectful to ESPN's servers and avoid rate limiting.
#' @param raw Logical. If TRUE, assigns raw JSON to global environment
#'   for the first combination only (default: FALSE).
#'
#' @return Invisibly returns the combined data frames. The main purpose is global
#'   environment assignment of combined datasets from all combinations.
#'
#' @examples
#' \dontrun{
#' # Get leaders for last 2 regular seasons with athlete details
#' fetch_multiple_nfl_leaders(years = c(2023, 2024), season_types = c(2))
#'
#' # Get leaders without athlete details (much faster)
#' fetch_multiple_nfl_leaders(years = c(2022, 2023, 2024),
#'                           fetch_athlete_info = FALSE)
#'
#' # Get leaders with more conservative API usage
#' fetch_multiple_nfl_leaders(years = c(2023, 2024),
#'                           athlete_delay = 0.2, delay = 0.5)
#' }
#'
#' @seealso \code{\link{fetch_nfl_leaders}} for single season data
#' @export
fetch_multiple_nfl_leaders <- function(years = 2024, season_types = c(2),
                                       return_type = "all", fetch_athlete_info = TRUE,
                                       athlete_delay = 0.1, delay = 0.2, raw = FALSE) {
  # Input validation
  if (length(years) == 0) {
    stop("'years' must contain at least one year")
  }

  if (length(season_types) == 0) {
    stop("'season_types' must contain at least one season type")
  }

  valid_season_types <- c(1, 2, 3)
  season_types <- as.numeric(season_types)
  if (!all(season_types %in% valid_season_types)) {
    stop(sprintf("'season_types' must contain only: %s", paste(valid_season_types, collapse = ", ")))
  }

  valid_types <- c("summary", "categories", "leaders", "all")
  if (!return_type %in% valid_types) {
    stop(sprintf("'return_type' must be one of: %s", paste(valid_types, collapse = ", ")))
  }

  # Create combinations of years and season types
  combinations <- expand.grid(year = years, season_type = season_types, stringsAsFactors = FALSE)

  # Initialize combined data containers
  all_summary <- data.frame()
  all_categories <- data.frame()
  all_leaders <- data.frame()

  message(sprintf("Starting to fetch NFL leaders data for %d combinations...", nrow(combinations)))

  # Process each combination sequentially
  for (i in seq_len(nrow(combinations))) {
    year <- combinations$year[i]
    season_type <- combinations$season_type[i]

    season_name <- switch(as.character(season_type),
                          "1" = "Preseason",
                          "2" = "Regular Season",
                          "3" = "Postseason",
                          paste("Season Type", season_type))

    message(sprintf("Fetching NFL leaders for %s %s (%d/%d)...",
                    year, season_name, i, nrow(combinations)))

    tryCatch({
      # Fetch individual combination data
      combination_data <- fetch_nfl_leaders(
        year = year,
        season_type = season_type,
        return_type = return_type,
        fetch_athlete_info = fetch_athlete_info,
        athlete_delay = athlete_delay,
        raw = raw
      )

      # If raw data requested, return after first combination
      if (isTRUE(raw)) {
        return(invisible(combination_data))
      }

      # Combine data based on return type
      if (return_type %in% c("summary", "all")) {
        summary_df <- get("nfl_leaders_summary", envir = .GlobalEnv)
        all_summary <- rbind(all_summary, summary_df)
      }

      if (return_type %in% c("categories", "all")) {
        categories_df <- get("nfl_leaders_categories", envir = .GlobalEnv)
        all_categories <- rbind(all_categories, categories_df)
      }

      if (return_type %in% c("leaders", "all")) {
        leaders_df <- get("nfl_leaders_leaders", envir = .GlobalEnv)
        all_leaders <- rbind(all_leaders, leaders_df)
      }

    }, error = function(e) {
      message(sprintf("Failed to fetch NFL leaders for %s %s: %s", year, season_name, e$message))
    })

    # Be respectful to the API with delay between requests
    if (i < nrow(combinations)) {
      Sys.sleep(delay)
    }
  }

  # Remove duplicates and assign combined datasets to global environment
  result_data <- list()

  if (return_type %in% c("summary", "all") && nrow(all_summary) > 0) {
    all_summary <- all_summary[!duplicated(paste(all_summary$season_year, all_summary$season_type)), ]
    assign("nfl_leaders_summary", all_summary, envir = .GlobalEnv)
    result_data$summary <- all_summary

    message(sprintf("Combined NFL leaders summary assigned to: nfl_leaders_summary (%d combinations)", nrow(all_summary)))
  }

  if (return_type %in% c("categories", "all") && nrow(all_categories) > 0) {
    all_categories <- all_categories[!duplicated(paste(all_categories$season_year,
                                                       all_categories$season_type,
                                                       all_categories$category_name)), ]
    assign("nfl_leaders_categories", all_categories, envir = .GlobalEnv)
    result_data$categories <- all_categories

    unique_combinations <- nrow(unique(all_categories[c("season_year", "season_type")]))
    message(sprintf("Combined NFL leaders categories assigned to: nfl_leaders_categories (%d combinations, %d categories)",
                    unique_combinations, nrow(all_categories)))
  }

  if (return_type %in% c("leaders", "all") && nrow(all_leaders) > 0) {
    all_leaders <- all_leaders[!duplicated(paste(all_leaders$season_year,
                                                 all_leaders$season_type,
                                                 all_leaders$athlete_id,
                                                 all_leaders$category_name)), ]
    assign("nfl_leaders_leaders", all_leaders, envir = .GlobalEnv)
    result_data$leaders <- all_leaders

    unique_combinations <- nrow(unique(all_leaders[c("season_year", "season_type")]))
    unique_athletes <- length(unique(all_leaders$athlete_id[!is.na(all_leaders$athlete_id)]))
    total_entries <- nrow(all_leaders)
    message(sprintf("Combined NFL leaders assigned to: nfl_leaders_leaders (%d combinations, %d athletes, %d entries)",
                    unique_combinations, unique_athletes, total_entries))
  }

  # Return appropriate data structure
  if (return_type == "all") {
    return(invisible(result_data))
  } else {
    return(invisible(result_data[[return_type]]))
  }
}
