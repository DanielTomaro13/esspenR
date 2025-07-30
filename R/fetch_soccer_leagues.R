#' Safe nested data extraction helper function for soccer leagues
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_soccer_leagues <- function(data, path, default = NA) {
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

#' Create soccer leagues data frame from ESPN API response
#'
#' Processes raw JSON response from ESPN soccer leagues API into structured data frame
#' @param data Raw JSON response from ESPN soccer API
#' @param limit Maximum number of leagues to process
#' @return Data frame with league information
#' @keywords internal
create_soccer_leagues_dataset <- function(data, limit = 100) {
  # Initialize leagues data frame
  leagues_df <- data.frame(
    league_id = character(0),
    league_name = character(0),
    league_slug = character(0),
    league_abbreviation = character(0),
    league_short_name = character(0),
    is_tournament = character(0),
    has_standings = character(0),
    league_uid = character(0),
    league_guid = character(0),
    logo_href = character(0),
    logo_alt = character(0),
    stringsAsFactors = FALSE
  )

  # Extract items (which contain $ref URLs)
  items <- extract_nested_soccer_leagues(data, c("items"), list())

  if (length(items) == 0 || !is.data.frame(items)) {
    warning("No items data found")
    return(leagues_df)
  }

  # Process only up to the requested limit
  items_to_process <- min(nrow(items), limit)
  if (items_to_process < nrow(items)) {
    message(sprintf("Processing first %d of %d available leagues (limit applied)", items_to_process, nrow(items)))
    items <- items[1:items_to_process, , drop = FALSE]
  }

  message(sprintf("Found %d league references, fetching details...", nrow(items)))

  # Process each league reference URL
  for (i in seq_len(nrow(items))) {
    ref_url <- items[i, "$ref"]

    if (!is.na(ref_url) && ref_url != "") {
      tryCatch({
        # Fetch individual league data
        message(sprintf("Fetching league %d/%d...", i, nrow(items)))
        resp <- httr::GET(ref_url, httr::timeout(10))

        if (httr::status_code(resp) == 200) {
          content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
          league_data <- jsonlite::fromJSON(content_text, simplifyVector = TRUE, simplifyDataFrame = TRUE)

          # Extract league information
          league_id <- as.character(extract_nested_soccer_leagues(league_data, c("id"), NA_character_))
          league_name <- as.character(extract_nested_soccer_leagues(league_data, c("name"), NA_character_))
          league_slug <- as.character(extract_nested_soccer_leagues(league_data, c("slug"), NA_character_))
          league_abbreviation <- as.character(extract_nested_soccer_leagues(league_data, c("abbreviation"), NA_character_))
          league_short_name <- as.character(extract_nested_soccer_leagues(league_data, c("shortName"), NA_character_))
          league_uid <- as.character(extract_nested_soccer_leagues(league_data, c("uid"), NA_character_))
          league_guid <- as.character(extract_nested_soccer_leagues(league_data, c("guid"), NA_character_))

          # Extract boolean flags
          is_tournament <- as.character(extract_nested_soccer_leagues(league_data, c("isTournament"), FALSE))

          # Check for standings in current season
          has_standings <- "FALSE"
          season_data <- extract_nested_soccer_leagues(league_data, c("season"), list())
          if (length(season_data) > 0) {
            season_types <- extract_nested_soccer_leagues(season_data, c("types"), list())
            if (length(season_types) > 0) {
              has_standings_list <- extract_nested_soccer_leagues(season_types, c("items"), list())
              if (is.list(has_standings_list) && length(has_standings_list) > 0) {
                for (type_item in has_standings_list) {
                  if (extract_nested_soccer_leagues(type_item, c("hasStandings"), FALSE)) {
                    has_standings <- "TRUE"
                    break
                  }
                }
              }
            }
          }

          # Extract logo information
          logo_href <- NA_character_
          logo_alt <- NA_character_

          logos <- extract_nested_soccer_leagues(league_data, c("logos"), list())
          if (is.list(logos) && length(logos) > 0) {
            # Get the first logo
            first_logo <- logos[[1]]
            if (is.list(first_logo)) {
              logo_href <- as.character(extract_nested_soccer_leagues(first_logo, c("href"), NA_character_))
              logo_alt <- as.character(extract_nested_soccer_leagues(first_logo, c("alt"), NA_character_))
            }
          }

          # Create league row
          league_df_row <- data.frame(
            league_id = league_id,
            league_name = league_name,
            league_slug = league_slug,
            league_abbreviation = league_abbreviation,
            league_short_name = league_short_name,
            is_tournament = is_tournament,
            has_standings = has_standings,
            league_uid = league_uid,
            league_guid = league_guid,
            logo_href = logo_href,
            logo_alt = logo_alt,
            stringsAsFactors = FALSE
          )

          leagues_df <- rbind(leagues_df, league_df_row)

          # Add small delay to be respectful to the API
          Sys.sleep(0.1)
        }
      }, error = function(e) {
        message(sprintf("Failed to fetch league data from %s: %s", ref_url, e$message))
      })
    }
  }

  # Clean up row names
  if (nrow(leagues_df) > 0) {
    rownames(leagues_df) <- NULL
  }

  return(leagues_df)
}

#' Fetch available soccer leagues from ESPN API
#'
#' Retrieves a comprehensive list of soccer leagues available through ESPN's API.
#' This function provides league information including names, slugs, and metadata
#' needed for further API calls to specific leagues.
#'
#' @param limit Integer. Maximum number of leagues to fetch (default: 100).
#'   ESPN API returns 25 leagues per page, so this controls how many to process.
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'soccer_leagues_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data frame. The main purpose is global
#'   environment assignment of:
#'   \itemize{
#'     \item \code{soccer_leagues} - Data frame with league information
#'     \item \code{soccer_leagues_raw} - Raw JSON response (if raw = TRUE)
#'   }
#'
#' @details
#' The function creates a structured data frame with comprehensive league information:
#'
#' **League Information** (\code{soccer_leagues}):
#' \itemize{
#'   \item \code{league_id} - Unique ESPN league identifier
#'   \item \code{league_name} - Full league name (e.g., "English Premier League")
#'   \item \code{league_slug} - URL-friendly league identifier (e.g., "eng.1")
#'   \item \code{league_abbreviation} - Short league code (e.g., "EPL")
#'   \item \code{league_short_name} - Abbreviated league name
#'   \item \code{is_tournament} - Boolean indicating if it's a tournament format
#'   \item \code{has_standings} - Boolean indicating if standings are available
#'   \item \code{league_uid} - ESPN universal identifier
#'   \item \code{league_guid} - ESPN global unique identifier
#'   \item \code{logo_href} - URL to league logo image
#'   \item \code{logo_alt} - Alt text for league logo
#' }
#'
#' **Common League Slugs**:
#' \itemize{
#'   \item \code{eng.1} - English Premier League
#'   \item \code{esp.1} - Spanish La Liga
#'   \item \code{ger.1} - German Bundesliga
#'   \item \code{ita.1} - Italian Serie A
#'   \item \code{fra.1} - French Ligue 1
#'   \item \code{uefa.champions} - UEFA Champions League
#'   \item \code{fifa.world} - FIFA World Cup
#'   \item \code{usa.1} - Major League Soccer (MLS)
#' }
#'
#' @examples
#' \dontrun{
#' # Get first 100 leagues (default)
#' fetch_soccer_leagues()
#'
#' # Get first 50 leagues
#' fetch_soccer_leagues(limit = 50)
#'
#' # Get major leagues only
#' fetch_soccer_leagues(limit = 10)
#'
#' # View the leagues data frame
#' head(soccer_leagues)
#' View(soccer_leagues)
#'
#' # Find specific leagues
#' premier_league <- soccer_leagues[soccer_leagues$league_slug == "eng.1", ]
#' champions_league <- soccer_leagues[grepl("Champions", soccer_leagues$league_name), ]
#'
#' # Get major European leagues
#' major_leagues <- soccer_leagues[
#'   soccer_leagues$league_slug %in% c("eng.1", "esp.1", "ger.1", "ita.1", "fra.1"),
#' ]
#'
#' # Find tournament competitions
#' tournaments <- soccer_leagues[soccer_leagues$is_tournament == "TRUE", ]
#'
#' # Find leagues with standings available
#' leagues_with_standings <- soccer_leagues[soccer_leagues$has_standings == "TRUE", ]
#'
#' # Search by league name
#' world_cup <- soccer_leagues[grepl("World Cup", soccer_leagues$league_name, ignore.case = TRUE), ]
#' mls <- soccer_leagues[grepl("Major League Soccer", soccer_leagues$league_name), ]
#'
#' # Get league slugs for API calls
#' league_slugs <- soccer_leagues$league_slug
#' cat("Available league slugs:\\n", paste(league_slugs, collapse = ", "))
#'
#' # Example: Use slug for further API calls
#' # fetch_soccer_standings("eng.1")  # Premier League standings
#' # fetch_soccer_schedule("esp.1")   # La Liga schedule
#'
#' # Get raw data for debugging
#' fetch_soccer_leagues(limit = 5, raw = TRUE)
#' }
#'
#' @seealso
#' Use the league slugs from this function for other soccer API calls:
#' \itemize{
#'   \item League standings: \code{/sports/soccer/{league_slug}/standings}
#'   \item League schedule: \code{/sports/soccer/{league_slug}/scoreboard}
#'   \item Team information: \code{/sports/soccer/{league_slug}/teams}
#' }
#'
#' @importFrom httr GET status_code content timeout
#' @importFrom jsonlite fromJSON
#' @export
fetch_soccer_leagues <- function(limit = 100, raw = FALSE) {
  # Input validation
  if (!is.numeric(limit) || limit <= 0) {
    stop("'limit' must be a positive number")
  }

  if (!is.logical(raw)) {
    stop("'raw' parameter must be logical (TRUE or FALSE)")
  }

  # Build API URL for ESPN soccer leagues with limit
  url <- sprintf("https://sports.core.api.espn.com/v2/sports/soccer/leagues?limit=%d", min(limit, 1000))

  message("Fetching available soccer leagues from ESPN API...")

  # Fetch and parse data
  tryCatch({
    resp <- httr::GET(url, httr::timeout(30))
    if (httr::status_code(resp) != 200) {
      stop(sprintf("API request failed: HTTP %d - %s",
                   httr::status_code(resp),
                   httr::http_status(resp)$message))
    }

    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content_text, simplifyVector = TRUE, simplifyDataFrame = TRUE)

    # Handle raw data assignment for debugging
    if (isTRUE(raw)) {
      assign("soccer_leagues_raw", data, envir = .GlobalEnv)
      message("Raw soccer leagues data assigned to: soccer_leagues_raw")
      message("Data structure preview:")

      # Show available sections
      sections <- names(data)
      message("- Available sections: ", paste(sections, collapse = ", "))

      # Show leagues info if available
      if ("items" %in% names(data)) {
        leagues <- data$items
        if (is.data.frame(leagues)) {
          message("- Total leagues available: ", data$count)
          message("- Leagues in this page: ", nrow(leagues))
          message("- Page ", data$pageIndex, " of ", data$pageCount)
        }
      }
      return(invisible(data))
    }

    # Create leagues dataset
    message("Processing soccer leagues data...")
    leagues_data <- create_soccer_leagues_dataset(data, limit)

    # Assign to global environment
    assign("soccer_leagues", leagues_data, envir = .GlobalEnv)

    message(sprintf("Soccer leagues data assigned to: soccer_leagues (%d leagues)", nrow(leagues_data)))

    # Show sample information
    if (nrow(leagues_data) > 0) {
      # Show major leagues
      major_leagues <- leagues_data[
        leagues_data$league_slug %in% c("eng.1", "esp.1", "ger.1", "ita.1", "fra.1"),
      ]

      if (nrow(major_leagues) > 0) {
        message("Major European leagues found:")
        for (i in seq_len(nrow(major_leagues))) {
          league <- major_leagues[i, ]
          message(sprintf("  - %s (%s)", league$league_name, league$league_slug))
        }
      }

      # Show tournament count
      tournament_count <- sum(leagues_data$is_tournament == "TRUE", na.rm = TRUE)
      league_count <- sum(leagues_data$is_tournament == "FALSE", na.rm = TRUE)
      message(sprintf("League breakdown: %d regular leagues, %d tournaments", league_count, tournament_count))

      # Show leagues with standings
      standings_count <- sum(leagues_data$has_standings == "TRUE", na.rm = TRUE)
      message(sprintf("Leagues with standings available: %d", standings_count))

      # Show sample slugs
      sample_slugs <- head(leagues_data$league_slug, 10)
      message("Sample league slugs: ", paste(sample_slugs, collapse = ", "))
    }

    return(invisible(leagues_data))

  }, error = function(e) {
    stop(sprintf("Failed to fetch soccer leagues: %s", e$message))
  })
}
