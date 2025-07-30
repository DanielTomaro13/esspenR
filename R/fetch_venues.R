#' Fetch ESPN League Venue Details
#'
#' @description
#' Lists all venues for a given sport and league using ESPN's sports.core.api,
#' fetching full details for each venue (name, capacity, city, state).
#'
#' @param sport Character. Sport slug (e.g. 'football', 'basketball').
#' @param league Character. League slug (e.g. 'nfl', 'nba').
#' @param limit Integer. Maximum number of venues to fetch (default: 1000).
#' @param raw Logical, default FALSE. If TRUE, returns full parsed JSON data.
#' @param progress Logical, default TRUE. Show progress bar for venue fetching.
#'
#' @return A tibble with venue id, name, capacity, city, state or raw JSON if `raw = TRUE`.
#' @export
#'
#' @examples
#' fetch_venues("football", "nfl")
#' fetch_venues("basketball", "nba", raw = TRUE)
#' fetch_venues("football", "nfl", limit = 50, progress = FALSE)
fetch_venues <- function(sport, league, limit = 1000, raw = FALSE, progress = TRUE) {

  # Input validation
  if (!is.character(sport) || length(sport) != 1 || nchar(sport) == 0) {
    stop("sport must be a non-empty character string")
  }
  if (!is.character(league) || length(league) != 1 || nchar(league) == 0) {
    stop("league must be a non-empty character string")
  }
  if (!is.numeric(limit) || limit <= 0) {
    stop("limit must be a positive number")
  }

  # Build URL with limit parameter
  url <- sprintf(
    "https://sports.core.api.espn.com/v2/sports/%s/leagues/%s/venues?limit=%d",
    sport, league, limit
  )

  if (progress) {
    message("Fetching venue list from ESPN API...")
  }

  # Fetch venue list
  resp <- httr::GET(url)
  if (httr::status_code(resp) != 200) {
    stop(sprintf("ESPN endpoint error. HTTP %d - %s", httr::status_code(resp), url))
  }

  data <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)

  if (is.null(data$items) || length(data$items) == 0) {
    if (progress) {
      message(sprintf("No venues found for %s/%s", sport, league))
    }
    return(tibble::tibble())
  }

  venue_links <- data$items$`$ref`

  if (progress) {
    message(sprintf("Found %d venues. Fetching details...", length(venue_links)))
  }

  # Function to get individual venue information
  get_venue_info <- function(link) {
    tryCatch({
      v_resp <- httr::GET(link)
      if (httr::status_code(v_resp) != 200) {
        if (progress) {
          warning(sprintf("Failed to fetch venue: %s", link))
        }
        return(NULL)
      }

      v_data <- jsonlite::fromJSON(httr::content(v_resp, as = "text", encoding = "UTF-8"), flatten = TRUE)

      if (isTRUE(raw)) return(v_data)

      # Safe field extraction with null coalescing
      id_val <- if (!is.null(v_data$id)) v_data$id else NA_character_
      name_val <- if (!is.null(v_data$fullName)) v_data$fullName else
        if (!is.null(v_data$name)) v_data$name else NA_character_
      display_name_val <- if (!is.null(v_data$displayName)) v_data$displayName else NA_character_
      capacity_val <- if (!is.null(v_data$capacity)) as.integer(v_data$capacity) else NA_integer_
      city_val <- if (!is.null(v_data$address) && !is.null(v_data$address$city)) v_data$address$city else NA_character_
      state_val <- if (!is.null(v_data$address) && !is.null(v_data$address$state)) v_data$address$state else NA_character_
      zipcode_val <- if (!is.null(v_data$address) && !is.null(v_data$address$zipCode)) v_data$address$zipCode else NA_character_
      country_val <- if (!is.null(v_data$address) && !is.null(v_data$address$country)) v_data$address$country else NA_character_
      grass_val <- if (!is.null(v_data$grass)) as.logical(v_data$grass) else NA
      indoor_val <- if (!is.null(v_data$indoor)) as.logical(v_data$indoor) else NA
      latitude_val <- if (!is.null(v_data$latitude)) as.numeric(v_data$latitude) else NA_real_
      longitude_val <- if (!is.null(v_data$longitude)) as.numeric(v_data$longitude) else NA_real_

      tibble::tibble(
        id = id_val,
        name = name_val,
        display_name = display_name_val,
        capacity = capacity_val,
        city = city_val,
        state = state_val,
        zipcode = zipcode_val,
        country = country_val,
        grass = grass_val,
        indoor = indoor_val,
        latitude = latitude_val,
        longitude = longitude_val
      )
    }, error = function(e) {
      if (progress) {
        warning(sprintf("Error fetching venue %s: %s", link, e$message))
      }
      return(NULL)
    })
  }

  # Fetch venue details
  venue_list <- list()
  for (i in seq_along(venue_links)) {
    venue_info <- get_venue_info(venue_links[i])
    if (!is.null(venue_info)) {
      venue_list[[length(venue_list) + 1]] <- venue_info
    }
  }

  # Combine all venue data
  if (length(venue_list) > 0) {
    venue_details <- do.call(rbind, venue_list)
  } else {
    venue_details <- tibble::tibble()
  }

  if (progress) {
    message(sprintf("Successfully fetched %d venue details", nrow(venue_details)))
  }

  return(venue_details)
}
