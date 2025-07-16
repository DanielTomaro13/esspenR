#' ESPN General Search (v3)
#'
#' @description
#' Search ESPN's general database (players, teams, leagues, articles, etc) using v3 search API.
#' This function searches across multiple ESPN content types and returns structured results.
#'
#' @param query Character string. Search query (e.g. "nfl", "warriors", "lebron james").
#'   Must be non-empty and contain at least one alphanumeric character.
#' @param limit Integer. Maximum number of rows to return per content type.
#'   Must be between 1 and 100. Default is 50.
#' @param raw Logical. If TRUE, returns the full parsed JSON list instead of cleaned data.
#'   Default is FALSE.
#' @param timeout Numeric. Request timeout in seconds. Default is 10.
#' @param user_agent Character string. User agent for requests.
#'   Default identifies your package.
#'
#' @return A tibble with columns: type, name, description, link, sport_category, image_url
#'   If raw = TRUE, returns the full parsed JSON list.
#'   If no results found, returns an empty tibble with proper column structure.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic search
#' fetch_general_search("nba")
#'
#' # Search with limit
#' fetch_general_search("lebron", limit = 10)
#'
#' # Get raw JSON response
#' fetch_general_search("warriors", raw = TRUE)
#' }
#'
#' @importFrom httr GET modify_url status_code content user_agent timeout
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @importFrom cli cli_abort cli_warn
fetch_general_search <- function(query,
                                 limit = 50L,
                                 raw = FALSE,
                                 timeout = 10,
                                 user_agent = "espnR-package (https://github.com/your-username/espnR)") {

  # Input validation
  if (missing(query) || is.null(query) || !is.character(query) || length(query) != 1) {
    cli::cli_abort("query must be a single character string")
  }

  if (nchar(trimws(query)) == 0) {
    cli::cli_abort("query cannot be empty or whitespace only")
  }

  if (!grepl("[[:alnum:]]", query)) {
    cli::cli_abort("query must contain at least one alphanumeric character")
  }

  if (!is.numeric(limit) || length(limit) != 1 || limit < 1 || limit > 100) {
    cli::cli_abort("limit must be a single integer between 1 and 100")
  }

  if (!is.logical(raw) || length(raw) != 1) {
    cli::cli_abort("raw must be a single logical value (TRUE or FALSE)")
  }

  if (!is.numeric(timeout) || length(timeout) != 1 || timeout <= 0) {
    cli::cli_abort("timeout must be a positive number")
  }

  # Construct URL
  url <- httr::modify_url(
    "https://site.web.api.espn.com/apis/common/v3/search",
    query = list(query = trimws(query))
  )

  # Make request with proper error handling
  tryCatch({
    resp <- httr::GET(
      url,
      httr::user_agent(user_agent),
      httr::timeout(timeout)
    )
  }, error = function(e) {
    cli::cli_abort("Network error: {e$message}")
  })

  # Check response status
  if (httr::status_code(resp) == 404) {
    cli::cli_abort("ESPN search endpoint not found (404). The API may have changed.")
  } else if (httr::status_code(resp) == 429) {
    cli::cli_abort("Rate limit exceeded (429). Please wait before making more requests.")
  } else if (httr::status_code(resp) >= 400) {
    cli::cli_abort("ESPN API error: HTTP {httr::status_code(resp)}")
  }

  # Parse JSON response
  content_text <- httr::content(resp, as = "text", encoding = "UTF-8")

  if (nchar(content_text) == 0) {
    cli::cli_abort("Empty response received from ESPN API")
  }

  data <- tryCatch({
    jsonlite::fromJSON(content_text, flatten = TRUE)
  }, error = function(e) {
    cli::cli_abort("Failed to parse JSON response: {e$message}")
  })

  # Return raw data if requested
  if (raw) return(data)

  # Extract items
  items <- data$items

  # Handle no results
  if (is.null(items) || (is.data.frame(items) && nrow(items) == 0)) {
    cli::cli_warn("No results found for query: '{query}'")
    return(tibble::tibble(
      type = character(0),
      name = character(0),
      description = character(0),
      link = character(0),
      sport_category = character(0),
      image_url = character(0)
    ))
  }

  # Extract links safely
  link_column <- if (is.null(items$links)) {
    rep(NA_character_, nrow(items))
  } else {
    vapply(items$links, function(links_list) {
      if (is.null(links_list) || length(links_list) == 0) return(NA_character_)

      # Try to find non-external link first
      if ("isExternal" %in% names(links_list)) {
        internal_idx <- which(!links_list$isExternal)[1]
        if (length(internal_idx) && !is.na(internal_idx)) {
          return(as.character(links_list$href[internal_idx]))
        }
      }

      # Fallback to first available link
      if ("href" %in% names(links_list) && length(links_list$href) > 0) {
        return(as.character(links_list$href[1]))
      }

      return(NA_character_)
    }, character(1))
  }

  # Extract image URLs safely
  image_column <- if (is.null(items$images)) {
    rep(NA_character_, nrow(items))
  } else {
    vapply(items$images, function(images_list) {
      if (is.null(images_list) || length(images_list) == 0) return(NA_character_)

      if ("href" %in% names(images_list) && length(images_list$href) > 0) {
        return(as.character(images_list$href[1]))
      }

      return(NA_character_)
    }, character(1))
  }

  # Create clean tibble with safe extraction
  n_items <- nrow(items)

  general_search_results <- tibble::tibble(
    type = if (is.null(items$type)) rep(NA_character_, n_items) else as.character(items$type),
    name = if (is.null(items$displayName)) rep(NA_character_, n_items) else as.character(items$displayName),
    description = if (is.null(items$sport)) rep(NA_character_, n_items) else as.character(items$sport),
    link = link_column,
    sport_category = if (!is.null(items$league.displayName)) {
      as.character(items$league.displayName)
    } else if (!is.null(items$category)) {
      as.character(items$category)
    } else {
      rep(NA_character_, n_items)
    },
    image_url = image_column
  )

  # Apply limit per type
  if (!is.null(limit) && limit > 0) {
    general_search_results <- general_search_results[order(general_search_results$type), ]

    # Split by type and apply limit
    type_groups <- split(general_search_results, general_search_results$type)
    limited_groups <- lapply(type_groups, function(df) {
      if (nrow(df) > limit) df[1:limit, ] else df
    })

    general_search_results <- do.call(rbind, limited_groups)
    rownames(general_search_results) <- NULL
  }

  return(general_search_results)
}
