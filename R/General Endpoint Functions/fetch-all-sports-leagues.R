#' Fetch all sports and their leagues from ESPN Core API
#'
#' @description
#' Gets a comprehensive list of all sports and their associated leagues from ESPN's Core API.
#' This uses the newer ESPN Core API endpoints that are currently working.
#'
#' @param include_leagues Logical. If TRUE, fetches leagues for each sport (default: TRUE)
#' @return A data.frame with sport and league details combined
#' @export
#'
#' @examples
#' \dontrun{
#' # Fetch all sports and their leagues
#' all_data <- fetch_all_sports_and_leagues()
#' head(all_data)
#'
#' # Just get sports without leagues (faster)
#' just_sports <- fetch_all_sports_and_leagues(include_leagues = FALSE)
#' print(just_sports)
#'
#' # Check what sports are available
#' sports_only <- fetch_all_sports_and_leagues(include_leagues = FALSE)
#' unique(sports_only$name)
#'
#' # Filter for a specific sport after fetching all data
#' basketball_data <- all_data[all_data$slug == "basketball", ]
#' print(basketball_data)
#' }
fetch_all_sports_and_leagues <- function(include_leagues = TRUE) {
  # Validate input
  if (!is.logical(include_leagues) || length(include_leagues) != 1) {
    stop("include_leagues must be a single logical value (TRUE or FALSE)")
  }

  # First, try to fetch all sports using the Core API
  sports_url <- "https://sports.core.api.espn.com/v2/sports"
  sports_resp <- httr::GET(sports_url)

  if (httr::status_code(sports_resp) != 200) {
    stop(sprintf("ESPN core API sports endpoint not reachable. HTTP %d - %s",
                 httr::status_code(sports_resp), sports_url))
  }

  sports_data <- jsonlite::fromJSON(httr::content(sports_resp, as = "text", encoding = "UTF-8"),
                                    flatten = TRUE)

  # Check if we have the expected structure
  if (is.null(sports_data$items)) {
    stop("No sports items found in ESPN response")
  }

  # Extract sport references and fetch detailed info for each
  sport_refs <- sports_data$items$`$ref`

  message(sprintf("Found %d sports, fetching detailed information...", length(sport_refs)))

  # Function to fetch individual sport details
  fetch_sport_details <- function(sport_ref) {
    tryCatch({
      resp <- httr::GET(sport_ref)
      if (httr::status_code(resp) != 200) {
        warning(sprintf("Failed to fetch details for: %s", sport_ref))
        return(NULL)
      }

      sport_data <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                                       flatten = TRUE)

      # Extract basic sport info
      sport_info <- data.frame(
        id = if (is.null(sport_data$id)) NA_character_ else as.character(sport_data$id),
        guid = if (is.null(sport_data$guid)) NA_character_ else as.character(sport_data$guid),
        uid = if (is.null(sport_data$uid)) NA_character_ else as.character(sport_data$uid),
        name = if (is.null(sport_data$name)) NA_character_ else as.character(sport_data$name),
        displayName = if (is.null(sport_data$displayName)) NA_character_ else as.character(sport_data$displayName),
        slug = if (is.null(sport_data$slug)) NA_character_ else as.character(sport_data$slug),
        leagues_ref = if (is.null(sport_data$leagues$`$ref`)) NA_character_ else as.character(sport_data$leagues$`$ref`),
        stringsAsFactors = FALSE
      )

      return(sport_info)
    }, error = function(e) {
      warning(sprintf("Error fetching sport details from %s: %s", sport_ref, e$message))
      return(NULL)
    })
  }

  # Fetch all sport details
  all_sports_list <- lapply(sport_refs, fetch_sport_details)

  # Remove NULL results
  all_sports_list <- all_sports_list[!sapply(all_sports_list, is.null)]

  if (length(all_sports_list) == 0) {
    stop("No valid sports data could be retrieved")
  }

  # Combine all sports into one data.frame
  sports_df <- do.call(rbind, all_sports_list)

  # If we don't need leagues, return just the sports
  if (!include_leagues) {
    return(sports_df)
  }

  # If we need leagues, fetch them for each sport
  message("Fetching leagues for each sport...")

  # Function to safely fetch leagues for a single sport
  fetch_single_sport_leagues <- function(leagues_ref, sport_slug) {
    if (is.na(leagues_ref) || leagues_ref == "") {
      return(NULL)
    }

    tryCatch({
      league_resp <- httr::GET(leagues_ref)
      if (httr::status_code(league_resp) != 200) {
        warning(sprintf("Failed to fetch leagues for sport: %s", sport_slug))
        return(NULL)
      }

      league_data <- jsonlite::fromJSON(httr::content(league_resp, as = "text", encoding = "UTF-8"),
                                        flatten = TRUE)

      if (is.null(league_data$items) || length(league_data$items) == 0) {
        return(NULL)
      }

      # Extract league references and fetch details
      league_refs <- league_data$items$`$ref`

      # Function to fetch individual league details
      fetch_league_details <- function(league_ref) {
        tryCatch({
          resp <- httr::GET(league_ref)
          if (httr::status_code(resp) != 200) {
            return(NULL)
          }

          league_detail <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                                              flatten = TRUE)

          return(data.frame(
            sport_slug = sport_slug,
            league_id = if (is.null(league_detail$id)) NA_character_ else as.character(league_detail$id),
            league_name = if (is.null(league_detail$name)) NA_character_ else as.character(league_detail$name),
            league_abbreviation = if (is.null(league_detail$abbreviation)) NA_character_ else as.character(league_detail$abbreviation),
            league_slug = if (is.null(league_detail$slug)) NA_character_ else as.character(league_detail$slug),
            stringsAsFactors = FALSE
          ))
        }, error = function(e) {
          return(NULL)
        })
      }

      # Fetch all league details
      league_details_list <- lapply(league_refs, fetch_league_details)
      league_details_list <- league_details_list[!sapply(league_details_list, is.null)]

      if (length(league_details_list) == 0) {
        return(NULL)
      }

      return(do.call(rbind, league_details_list))

    }, error = function(e) {
      warning(sprintf("Error fetching leagues for %s: %s", sport_slug, e$message))
      return(NULL)
    })
  }

  # Fetch leagues for each sport
  all_leagues_list <- mapply(fetch_single_sport_leagues,
                             sports_df$leagues_ref,
                             sports_df$slug,
                             SIMPLIFY = FALSE)

  # Remove NULL results
  all_leagues_list <- all_leagues_list[!sapply(all_leagues_list, is.null)]

  # If no leagues found, return just sports
  if (length(all_leagues_list) == 0) {
    warning("No leagues data found for any sport")
    return(sports_df)
  }

  # Combine all leagues into one data.frame
  all_leagues <- do.call(rbind, all_leagues_list)

  # Join sports and leagues data
  combined_data <- merge(sports_df, all_leagues,
                         by.x = "slug", by.y = "sport_slug",
                         all.x = TRUE, all.y = FALSE)

  return(combined_data)
}

#' Fetch ESPN leagues for a given sport using Core API
#'
#' @description
#' Lists all leagues under a specific sport using ESPN's Core API.
#' This is a helper function that focuses on a single sport.
#'
#' @param sport Character. Sport slug, e.g. 'basketball', 'football', 'soccer'
#' @return A data.frame with leagues info (id, name, abbreviation, slug)
#' @export
#'
#' @examples
#' \dontrun{
#' # Fetch leagues for basketball
#' basketball_leagues <- fetch_sport_leagues("basketball")
#' head(basketball_leagues)
#'
#' # Fetch leagues for football
#' football_leagues <- fetch_sport_leagues("football")
#' print(football_leagues)
#'
#' # Check if any leagues were found
#' if (nrow(basketball_leagues) > 0) {
#'   print("Basketball leagues found:")
#'   print(basketball_leagues$league_name)
#' }
#' }
fetch_sport_leagues <- function(sport) {
  # Validate input
  if (!is.character(sport) || length(sport) != 1 || is.na(sport) || sport == "") {
    stop("sport must be a non-empty character string")
  }

  # First get sport details to get the leagues reference
  sport_url <- sprintf("https://sports.core.api.espn.com/v2/sports/%s", sport)
  sport_resp <- httr::GET(sport_url)

  if (httr::status_code(sport_resp) != 200) {
    stop(sprintf("ESPN core API endpoint not reachable for sport '%s'. HTTP %d - %s",
                 sport, httr::status_code(sport_resp), sport_url))
  }

  sport_data <- jsonlite::fromJSON(httr::content(sport_resp, as = "text", encoding = "UTF-8"),
                                   flatten = TRUE)

  # Check if leagues reference exists
  if (is.null(sport_data$leagues$`$ref`)) {
    warning(sprintf("No leagues reference found for sport: %s", sport))
    return(data.frame(
      sport = character(0),
      league_id = character(0),
      league_name = character(0),
      abbreviation = character(0),
      slug = character(0),
      stringsAsFactors = FALSE
    ))
  }

  # Fetch leagues
  leagues_url <- sport_data$leagues$`$ref`
  leagues_resp <- httr::GET(leagues_url)

  if (httr::status_code(leagues_resp) != 200) {
    stop(sprintf("ESPN core API leagues endpoint not reachable. HTTP %d - %s",
                 httr::status_code(leagues_resp), leagues_url))
  }

  leagues_data <- jsonlite::fromJSON(httr::content(leagues_resp, as = "text", encoding = "UTF-8"),
                                     flatten = TRUE)

  if (is.null(leagues_data$items) || length(leagues_data$items) == 0) {
    warning(sprintf("No leagues found for sport: %s", sport))
    return(data.frame(
      sport = character(0),
      league_id = character(0),
      league_name = character(0),
      abbreviation = character(0),
      slug = character(0),
      stringsAsFactors = FALSE
    ))
  }

  # Extract league references and fetch details
  league_refs <- leagues_data$items$`$ref`

  # Function to fetch individual league details
  fetch_league_details <- function(league_ref) {
    tryCatch({
      resp <- httr::GET(league_ref)
      if (httr::status_code(resp) != 200) {
        return(NULL)
      }

      league_detail <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                                          flatten = TRUE)

      return(data.frame(
        sport = sport,
        league_id = if (is.null(league_detail$id)) NA_character_ else as.character(league_detail$id),
        league_name = if (is.null(league_detail$name)) NA_character_ else as.character(league_detail$name),
        abbreviation = if (is.null(league_detail$abbreviation)) NA_character_ else as.character(league_detail$abbreviation),
        slug = if (is.null(league_detail$slug)) NA_character_ else as.character(league_detail$slug),
        stringsAsFactors = FALSE
      ))
    }, error = function(e) {
      warning(sprintf("Error fetching league details from %s: %s", league_ref, e$message))
      return(NULL)
    })
  }

  # Fetch all league details
  league_details_list <- lapply(league_refs, fetch_league_details)
  league_details_list <- league_details_list[!sapply(league_details_list, is.null)]

  if (length(league_details_list) == 0) {
    warning(sprintf("No valid league details could be retrieved for sport: %s", sport))
    return(data.frame(
      sport = character(0),
      league_id = character(0),
      league_name = character(0),
      abbreviation = character(0),
      slug = character(0),
      stringsAsFactors = FALSE
    ))
  }

  # Combine all leagues into one data.frame
  leagues_df <- do.call(rbind, league_details_list)

  return(leagues_df)
}

#' Fetch all sports from ESPN Core API
#'
#' @description
#' Gets a list of all sports available from ESPN's Core API.
#' This is a helper function that focuses only on sports.
#'
#' @return A data.frame with sport details (id, name, slug, uid, etc)
#' @export
#'
#' @examples
#' \dontrun{
#' # Fetch all available sports
#' sports <- fetch_all_sports()
#' head(sports)
#'
#' # Check what sports are available
#' print(sports$name)
#'
#' # Look for a specific sport
#' basketball_info <- sports[sports$slug == "basketball", ]
#' print(basketball_info)
#' }
fetch_all_sports <- function() {
  url <- "https://sports.core.api.espn.com/v2/sports"
  resp <- httr::GET(url)

  if (httr::status_code(resp) != 200) {
    stop(sprintf("ESPN core API endpoint not reachable. HTTP %d - %s",
                 httr::status_code(resp), url))
  }

  data <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                             flatten = TRUE)

  if (is.null(data$items)) {
    stop("No sports items found in ESPN response")
  }

  # Extract sport references and fetch detailed info for each
  sport_refs <- data$items$`$ref`

  message(sprintf("Found %d sports, fetching detailed information...", length(sport_refs)))

  # Function to fetch individual sport details
  fetch_sport_details <- function(sport_ref) {
    tryCatch({
      resp <- httr::GET(sport_ref)
      if (httr::status_code(resp) != 200) {
        warning(sprintf("Failed to fetch details for: %s", sport_ref))
        return(NULL)
      }

      sport_data <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                                       flatten = TRUE)

      # Extract basic sport info
      sport_info <- data.frame(
        id = if (is.null(sport_data$id)) NA_character_ else as.character(sport_data$id),
        guid = if (is.null(sport_data$guid)) NA_character_ else as.character(sport_data$guid),
        uid = if (is.null(sport_data$uid)) NA_character_ else as.character(sport_data$uid),
        name = if (is.null(sport_data$name)) NA_character_ else as.character(sport_data$name),
        displayName = if (is.null(sport_data$displayName)) NA_character_ else as.character(sport_data$displayName),
        slug = if (is.null(sport_data$slug)) NA_character_ else as.character(sport_data$slug),
        stringsAsFactors = FALSE
      )

      return(sport_info)
    }, error = function(e) {
      warning(sprintf("Error fetching sport details from %s: %s", sport_ref, e$message))
      return(NULL)
    })
  }

  # Fetch all sport details
  all_sports_list <- lapply(sport_refs, fetch_sport_details)

  # Remove NULL results
  all_sports_list <- all_sports_list[!sapply(all_sports_list, is.null)]

  if (length(all_sports_list) == 0) {
    stop("No valid sports data could be retrieved")
  }

  # Combine all sports into one data.frame
  sports_df <- do.call(rbind, all_sports_list)

  return(sports_df)
}
