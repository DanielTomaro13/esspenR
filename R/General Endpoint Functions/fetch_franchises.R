#' Fetch ESPN League Franchise Details
#'
#' @description
#' Lists all franchises (teams/clubs) for a given sport and league,
#' fetching full details (name, location, nickname, venue) using ESPN's sports.core.api.
#'
#' @param sport Character. Sport slug (e.g. 'football', 'basketball').
#' @param league Character. League slug (e.g. 'nfl', 'nba').
#' @param raw Logical. If TRUE, returns full parsed JSON. Default FALSE.
#' @return A tibble of franchises with cleaned columns or raw list if `raw = TRUE`.
#' @export
#'
#' @examples
#' fetch_franchises("football", "nfl")
#' fetch_franchises("basketball", "nba")
#' fetch_franchises("football", "nfl", raw = TRUE)
fetch_franchises <- function(sport, league, raw = FALSE) {
  url <- sprintf(
    "https://sports.core.api.espn.com/v2/sports/%s/leagues/%s/franchises",
    sport, league
  )

  resp <- httr::GET(url)
  if (httr::status_code(resp) != 200) {
    cli::cli_abort("ESPN endpoint no longer exists or is not reachable. HTTP {httr::status_code(resp)} - {url}")
  }

  data <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)

  if (is.null(data$items)) {
    message("No franchises found.")
    return(tibble::tibble())
  }

  franchise_links <- data$items$`$ref`

  get_franchise_info <- function(link) {
    tryCatch({
      f_resp <- httr::GET(link)
      if (httr::status_code(f_resp) != 200) return(NULL)
      f_data <- jsonlite::fromJSON(httr::content(f_resp, as = "text", encoding = "UTF-8"), flatten = TRUE)
      if (isTRUE(raw)) return(f_data)

      tibble::tibble(
        id = f_data$id,
        uid = f_data$uid,
        slug = f_data$slug,
        location = f_data$location,
        name = f_data$name,
        nickname = f_data$nickname,
        abbreviation = f_data$abbreviation,
        displayName = f_data$displayName,
        shortDisplayName = f_data$shortDisplayName,
        color = f_data$color,
        active = f_data$isActive,
        venue_name = if (!is.null(f_data$venue$fullName)) f_data$venue$fullName else NA_character_,
        venue_city = if (!is.null(f_data$venue$address$city)) f_data$venue$address$city else NA_character_,
        venue_state = if (!is.null(f_data$venue$address$state)) f_data$venue$address$state else NA_character_
      )
    }, error = function(e) NULL)
  }

  franchises_details <- purrr::map_dfr(franchise_links, get_franchise_info)
  return(franchises_details)
}
