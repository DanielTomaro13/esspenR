#' Safe nested data extraction helper function for splits
#'
#' Safely extracts values from nested list structures with error handling
#'
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_splits <- function(data, path, default = NA) {
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

#' Create athlete splits data frame from API response
#'
#' Processes raw JSON response from ESPN Web API into a structured data frame
#' containing splits data for an NFL athlete
#'
#' @param data Raw JSON response from ESPN Web API athlete splits endpoint
#' @param athlete_id Character. Athlete ID used in request
#' @return data.frame with athlete splits information
#' @keywords internal
create_athlete_splits_dataset <- function(data, athlete_id) {
  # Initialize result data frame
  result_df <- data.frame(
    athlete_id = character(0),
    season_year = character(0),
    split_category = character(0),
    split_category_name = character(0),
    split_name = character(0),
    split_display_name = character(0),
    split_abbreviation = character(0),
    stat_category = character(0),
    stat_label = character(0),
    stat_name = character(0),
    stat_display_name = character(0),
    stat_description = character(0),
    stat_value = character(0),
    stat_display_value = character(0),
    stringsAsFactors = FALSE
  )

  # Extract season year from display name or filters
  season_year <- "2024"  # Default
  display_name <- extract_nested_splits(data, c("displayName"), "")
  if (grepl("\\d{4}", display_name)) {
    season_year <- regmatches(display_name, regexpr("\\d{4}", display_name))
  } else {
    # Try to get from filters
    filters <- extract_nested_splits(data, c("filters"), list())
    for (filter in filters) {
      if (extract_nested_splits(filter, c("name"), "") == "season") {
        season_year <- extract_nested_splits(filter, c("value"), "2024")
        break
      }
    }
  }

  # Get stat definitions
  labels <- extract_nested_splits(data, c("labels"), list())
  names_list <- extract_nested_splits(data, c("names"), list())
  display_names <- extract_nested_splits(data, c("displayNames"), list())
  descriptions <- extract_nested_splits(data, c("descriptions"), list())

  # Get categories to determine stat groupings
  categories <- extract_nested_splits(data, c("categories"), list())

  # Determine stat category boundaries (passing vs rushing)
  passing_count <- 10  # Default
  if (length(categories) > 0) {
    passing_count <- extract_nested_splits(categories[[1]], c("count"), 10)
  }

  # Extract split categories
  split_categories <- extract_nested_splits(data, c("splitCategories"), list())

  if (length(split_categories) == 0) {
    return(result_df)
  }

  # Process each split category
  for (split_category in split_categories) {
    category_name <- extract_nested_splits(split_category, c("name"), "Unknown")
    category_display_name <- extract_nested_splits(split_category, c("displayName"), category_name)

    # Extract individual splits within this category
    splits <- extract_nested_splits(split_category, c("splits"), list())

    if (length(splits) == 0) {
      next
    }

    # Process each individual split
    for (split in splits) {
      split_name <- extract_nested_splits(split, c("name"), "Unknown")
      split_display_name <- extract_nested_splits(split, c("displayName"), split_name)
      split_abbreviation <- extract_nested_splits(split, c("abbreviation"), "")

      # Extract statistics for this split
      stats <- extract_nested_splits(split, c("stats"), list())

      if (length(stats) == 0) {
        # Create row with split info but no stats
        split_row <- data.frame(
          athlete_id = as.character(athlete_id),
          season_year = as.character(season_year),
          split_category = as.character(category_name),
          split_category_name = as.character(category_display_name),
          split_name = as.character(split_name),
          split_display_name = as.character(split_display_name),
          split_abbreviation = as.character(split_abbreviation),
          stat_category = NA_character_,
          stat_label = NA_character_,
          stat_name = NA_character_,
          stat_display_name = NA_character_,
          stat_description = NA_character_,
          stat_value = NA_character_,
          stat_display_value = NA_character_,
          stringsAsFactors = FALSE
        )

        result_df <- rbind(result_df, split_row)
        next
      }

      # Process each statistic
      for (stat_idx in seq_along(stats)) {
        if (stat_idx <= length(labels) && stat_idx <= length(names_list) && stat_idx <= length(display_names)) {

          stat_value <- stats[[stat_idx]]

          # Determine stat category based on position
          if (stat_idx <= passing_count) {
            stat_category <- "Passing"
          } else {
            stat_category <- "Rushing"
          }

          # Create row for this statistic
          split_row <- data.frame(
            athlete_id = as.character(athlete_id),
            season_year = as.character(season_year),
            split_category = as.character(category_name),
            split_category_name = as.character(category_display_name),
            split_name = as.character(split_name),
            split_display_name = as.character(split_display_name),
            split_abbreviation = as.character(split_abbreviation),
            stat_category = as.character(stat_category),
            stat_label = as.character(labels[[stat_idx]]),
            stat_name = as.character(names_list[[stat_idx]]),
            stat_display_name = as.character(display_names[[stat_idx]]),
            stat_description = as.character(ifelse(length(descriptions) >= stat_idx, descriptions[[stat_idx]], "")),
            stat_value = as.character(stat_value),
            stat_display_value = as.character(stat_value),
            stringsAsFactors = FALSE
          )

          result_df <- rbind(result_df, split_row)
        }
      }
    }
  }

  # Clean up row names
  if (nrow(result_df) > 0) {
    rownames(result_df) <- NULL
  }

  return(result_df)
}

#' Fetch NFL athlete splits data using Web API
#'
#' Retrieves detailed splits statistics for NFL players from ESPN's Web API.
#' The function fetches player statistics broken down by various conditions
#' such as home/away, vs divisions, by quarter, situational splits, and more.
#'
#' @param athlete_id Character or Numeric. ESPN athlete ID (required).
#'   The unique identifier for the athlete in ESPN's database.
#' @param season Character or Numeric. Season year (optional).
#'   If not provided, fetches current/latest season data. Format: "2024" or 2024.
#' @param season_type Character. Season type to fetch (default: "all").
#'   Options are:
#'   \itemize{
#'     \item "regular" - Regular season splits only
#'     \item "postseason" - Playoff splits only
#'     \item "all" - Both regular season and postseason splits
#'   }
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'nfl_athlete_splits_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data frame. The main purpose is global
#'   environment assignment:
#'   \itemize{
#'     \item \code{nfl_athlete_splits} - Splits data frame with comprehensive breakdown statistics
#'     \item \code{nfl_athlete_splits_raw} - Raw JSON response (if raw = TRUE)
#'   }
#'
#' @details
#' The function creates a structured data frame with comprehensive splits information:
#'
#' **Split Categories** typically include:
#' \itemize{
#'   \item \strong{Outcome:} Performance in wins vs losses
#'   \item \strong{Victory Margin:} Performance by point differential
#'   \item \strong{Location:} Home vs away performance
#'   \item \strong{Weather:} Indoor vs outdoor conditions
#'   \item \strong{Surface:} Turf vs grass performance
#'   \item \strong{Day of Week:} Performance on different days
#'   \item \strong{Month:} Performance by month of season
#'   \item \strong{Opponent:} Performance vs specific teams
#'   \item \strong{Quarter/Half:} Performance by game period
#'   \item \strong{Down:} Performance on different downs
#'   \item \strong{Field Position:} Performance by field location
#'   \item \strong{Point Differential:} Performance when leading/trailing
#' }
#'
#' **Data Frame Structure:**
#' \itemize{
#'   \item \strong{Athlete Info:} athlete_id, season_year
#'   \item \strong{Split Classification:} split_category, split_name, split_display_name
#'   \item \strong{Statistical Data:} stat_category (Passing/Rushing), stat_name, stat_value
#'   \item \strong{Metadata:} stat_description, stat_display_name, stat_label
#' }
#'
#' **Statistical Categories** include:
#' \itemize{
#'   \item \strong{Passing:} Completions, attempts, yards, TDs, interceptions, passer rating
#'   \item \strong{Rushing:} Attempts, yards, touchdowns, long runs
#'   \item \strong{Efficiency:} Completion percentage, yards per attempt, etc.
#' }
#'
#' This data is valuable for identifying performance patterns, situational
#' strengths/weaknesses, and contextual analysis of player performance.
#'
#' @examples
#' \dontrun{
#' # Get splits for Josh Allen
#' fetch_nfl_athlete_splits(athlete_id = "3918298")
#' head(nfl_athlete_splits)
#'
#' # Get specific season splits
#' fetch_nfl_athlete_splits(athlete_id = "3918298", season = "2024")
#'
#' # Get only regular season splits
#' fetch_nfl_athlete_splits(athlete_id = "3918298", season_type = "regular")
#'
#' # Check available split categories
#' unique(nfl_athlete_splits$split_category_name)
#'
#' # Analyze home vs away performance
#' location_splits <- nfl_athlete_splits[nfl_athlete_splits$split_category == "byLocation", ]
#' print(unique(location_splits[c("split_name", "split_display_name")]))
#'
#' # View passing yards by outcome (wins vs losses)
#' outcome_passing <- nfl_athlete_splits[
#'   nfl_athlete_splits$split_category == "byOutcome" &
#'   nfl_athlete_splits$stat_name == "passingYards",
#'   c("split_name", "stat_value")
#' ]
#' print(outcome_passing)
#'
#' # Get raw data for debugging
#' fetch_nfl_athlete_splits(athlete_id = "3918298", raw = TRUE)
#' str(nfl_athlete_splits_raw, max.level = 3)
#' }
#'
#' @seealso \code{\link{fetch_multiple_nfl_athlete_splits}} for fetching
#'   multiple athletes' data, \code{\link{fetch_nfl_athlete_gamelog}} for
#'   game-by-game statistics
#'
#' @export
fetch_nfl_athlete_splits <- function(athlete_id, season = NULL, season_type = "all", raw = FALSE) {
  # Input validation
  if (missing(athlete_id)) {
    stop("'athlete_id' is a required parameter")
  }

  if (!season_type %in% c("regular", "postseason", "all")) {
    stop("'season_type' must be one of: 'regular', 'postseason', or 'all'")
  }

  # Convert athlete_id to character for URL building
  athlete_id <- as.character(athlete_id)

  # Build API URL for Web API
  url <- sprintf("https://site.web.api.espn.com/apis/common/v3/sports/football/nfl/athletes/%s/splits",
                 athlete_id)

  # Add query parameters if specified
  query_params <- list()
  if (!is.null(season)) {
    query_params$season <- as.character(season)
  }
  if (season_type != "all") {
    query_params$seasontype <- switch(season_type,
                                      "regular" = "2",
                                      "postseason" = "3")
  }

  # Fetch and parse data
  tryCatch({
    resp <- httr::GET(url, query = query_params, httr::timeout(60))

    if (httr::status_code(resp) != 200) {
      stop(sprintf("API request failed: HTTP %d - %s",
                   httr::status_code(resp),
                   httr::http_status(resp)$message))
    }

    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content_text, simplifyVector = FALSE, simplifyDataFrame = FALSE)

    # Handle raw data assignment for debugging
    if (isTRUE(raw)) {
      assign("nfl_athlete_splits_raw", data, envir = .GlobalEnv)
      message("Raw NFL athlete splits data assigned to: nfl_athlete_splits_raw")
      message("Data structure preview:")

      split_categories <- extract_nested_splits(data, c("splitCategories"), list())
      message("- Split categories count: ", length(split_categories))

      # Show available split categories
      if (length(split_categories) > 0) {
        category_names <- sapply(split_categories, function(x) {
          name <- extract_nested_splits(x, c("displayName"), "Unknown")
          splits_count <- length(extract_nested_splits(x, c("splits"), list()))
          paste0(name, " (", splits_count, " splits)")
        })
        message("- Available categories:")
        for (cat_name in category_names) {
          message("  * ", cat_name)
        }
      }

      return(invisible(data))
    }

    # Create dataset
    splits_df <- create_athlete_splits_dataset(data, athlete_id)
    assign("nfl_athlete_splits", splits_df, envir = .GlobalEnv)

    # Calculate summary statistics for user feedback
    unique_categories <- length(unique(splits_df$split_category[!is.na(splits_df$split_category)]))
    unique_splits <- length(unique(paste(splits_df$split_category, splits_df$split_name, sep = "_")))
    total_stats <- sum(!is.na(splits_df$stat_value))
    passing_stats <- sum(splits_df$stat_category == "Passing" & !is.na(splits_df$stat_value), na.rm = TRUE)
    rushing_stats <- sum(splits_df$stat_category == "Rushing" & !is.na(splits_df$stat_value), na.rm = TRUE)

    message(sprintf("NFL athlete splits data assigned to: nfl_athlete_splits (%d split categories, %d unique splits, %d total statistics: %d passing, %d rushing)",
                    unique_categories, unique_splits, total_stats, passing_stats, rushing_stats))

    return(invisible(splits_df))

  }, error = function(e) {
    stop(sprintf("Failed to fetch NFL athlete splits for athlete %s: %s",
                 athlete_id, e$message))
  })
}

#' Fetch multiple NFL athletes' splits data
#'
#' Retrieves splits data for multiple NFL athletes with rate limiting to
#' be respectful to ESPN's Web API. This function calls \code{\link{fetch_nfl_athlete_splits}}
#' for each athlete and combines the results.
#'
#' @param athlete_ids Character or Numeric vector. ESPN athlete IDs.
#'   Vector of unique identifiers for athletes in ESPN's database.
#' @param season Character or Numeric. Season year (optional).
#'   If not provided, fetches current/latest season data for all athletes.
#' @param season_type Character. Season type to fetch (default: "all").
#'   Same options as \code{\link{fetch_nfl_athlete_splits}}.
#' @param delay Numeric. Delay in seconds between API requests (default: 0.2).
#'   Used to be respectful to ESPN's servers and avoid rate limiting.
#' @param raw Logical. If TRUE, assigns raw JSON to global environment
#'   for the first athlete only (default: FALSE).
#'
#' @return Invisibly returns the combined data frame. The main purpose is global
#'   environment assignment:
#'   \itemize{
#'     \item \code{nfl_athlete_splits} - Combined splits data from all athletes
#'   }
#'
#' @details
#' The function processes athletes sequentially with a configurable delay
#' between requests. Failed requests for individual athletes are logged but
#' do not stop the overall process. Duplicate records are automatically
#' removed from the final combined dataset.
#'
#' The splits data is particularly useful for comparative analysis between
#' players under similar conditions (e.g., comparing quarterbacks' performance
#' in prime time games, or running backs' performance in red zone situations).
#'
#' @examples
#' \dontrun{
#' # Get splits for multiple quarterbacks
#' qb_ids <- c("3918298", "3139477", "4035671")  # Allen, Mahomes, Burrow
#' fetch_multiple_nfl_athlete_splits(qb_ids)
#'
#' # Get specific season splits for multiple players
#' fetch_multiple_nfl_athlete_splits(qb_ids, season = "2024")
#'
#' # Get regular season splits only
#' fetch_multiple_nfl_athlete_splits(qb_ids, season_type = "regular")
#'
#' # Use longer delay for larger requests
#' fetch_multiple_nfl_athlete_splits(qb_ids, delay = 0.5)
#'
#' # Analyze combined results
#' unique_athletes <- unique(nfl_athlete_splits$athlete_id)
#' cat("Retrieved splits for", length(unique_athletes), "athletes\n")
#'
#' # Compare home/away performance across players
#' location_data <- nfl_athlete_splits[
#'   nfl_athlete_splits$split_category == "byLocation" &
#'   nfl_athlete_splits$stat_name == "passingYards",
#'   c("athlete_id", "split_name", "stat_value")
#' ]
#'
#' # Compare performance in wins vs losses
#' outcome_data <- nfl_athlete_splits[
#'   nfl_athlete_splits$split_category == "byOutcome" &
#'   nfl_athlete_splits$stat_name == "QBRating",
#'   c("athlete_id", "split_name", "stat_value")
#' ]
#' }
#'
#' @seealso \code{\link{fetch_nfl_athlete_splits}} for single athlete data,
#'   \code{\link{fetch_multiple_nfl_athlete_gamelogs}} for game-by-game data
#'
#' @export
fetch_multiple_nfl_athlete_splits <- function(athlete_ids, season = NULL, season_type = "all",
                                              delay = 0.2, raw = FALSE) {
  # Input validation
  if (length(athlete_ids) == 0) {
    stop("'athlete_ids' must contain at least one athlete ID")
  }

  if (!season_type %in% c("regular", "postseason", "all")) {
    stop("'season_type' must be one of: 'regular', 'postseason', or 'all'")
  }

  # Initialize combined data container
  all_splits_data <- data.frame()

  message(sprintf("Starting to fetch splits data for %d athletes...", length(athlete_ids)))

  # Process each athlete sequentially
  for (i in seq_along(athlete_ids)) {
    athlete_id <- athlete_ids[i]
    message(sprintf("Fetching splits data for athlete %s (%d/%d)...", athlete_id, i, length(athlete_ids)))

    tryCatch({
      # Fetch individual athlete data
      athlete_data <- fetch_nfl_athlete_splits(
        athlete_id = athlete_id,
        season = season,
        season_type = season_type,
        raw = raw
      )

      # If raw data requested, return after first athlete
      if (isTRUE(raw)) {
        return(invisible(athlete_data))
      }

      # Combine data
      splits_df <- get("nfl_athlete_splits", envir = .GlobalEnv)
      all_splits_data <- rbind(all_splits_data, splits_df)

    }, error = function(e) {
      message(sprintf("Failed to fetch splits data for athlete %s: %s", athlete_id, e$message))
    })

    # Be respectful to the API with delay between requests
    if (i < length(athlete_ids)) {
      Sys.sleep(delay)
    }
  }

  # Remove duplicates and assign combined dataset to global environment
  if (nrow(all_splits_data) > 0) {
    # Remove duplicate records based on athlete, split, and stat
    all_splits_data <- all_splits_data[!duplicated(paste(all_splits_data$athlete_id,
                                                         all_splits_data$split_category,
                                                         all_splits_data$split_name,
                                                         all_splits_data$stat_name)), ]
    assign("nfl_athlete_splits", all_splits_data, envir = .GlobalEnv)

    # Calculate summary statistics for user feedback
    unique_athletes <- length(unique(all_splits_data$athlete_id))
    unique_categories <- length(unique(all_splits_data$split_category[!is.na(all_splits_data$split_category)]))
    unique_splits <- length(unique(paste(all_splits_data$split_category, all_splits_data$split_name, sep = "_")))
    total_stats <- sum(!is.na(all_splits_data$stat_value))
    passing_stats <- sum(all_splits_data$stat_category == "Passing" & !is.na(all_splits_data$stat_value), na.rm = TRUE)
    rushing_stats <- sum(all_splits_data$stat_category == "Rushing" & !is.na(all_splits_data$stat_value), na.rm = TRUE)

    message(sprintf("Completed! NFL athlete splits data assigned to: nfl_athlete_splits (%d athletes, %d split categories, %d unique splits, %d total statistics: %d passing, %d rushing)",
                    unique_athletes, unique_categories, unique_splits, total_stats, passing_stats, rushing_stats))
  } else {
    message("No splits data was successfully retrieved for any athlete.")
  }

  return(invisible(all_splits_data))
}
