#' ESPN Search (v2 API)
#'
#' @description
#' Searches across players, teams, and articles using ESPN's search v2 API,
#' returning structured data matching the query.
#'
#' @param query Character. Search query (e.g. "lebron", "warriors").
#' @param limit Integer. Maximum number of results per category. Default is 10.
#' @param raw Logical. If TRUE, returns the full parsed JSON data. Default is FALSE.
#' @param timeout Numeric. Request timeout in seconds. Default is 10.
#'
#' @return
#' If `raw = TRUE`, returns a list with the full JSON response.
#' Otherwise, a tibble with columns:
#' \describe{
#'   \item{type}{Content type (e.g. "player", "team", "article")}
#'   \item{name}{Display name}
#'   \item{description}{Description text if available}
#'   \item{subtitle}{Additional context}
#'   \item{link}{URL to ESPN page}
#'   \item{id}{Unique identifier}
#'   \item{category}{Search result category}
#' }
#'
#' @details
#' Uses ESPN's undocumented search API. As it is unofficial, the endpoint may
#' change without notice. Includes robust error handling and returns an
#' empty tibble if no results are found.
#'
#' @examples
#' \dontrun{
#' # Search for a player
#' fetch_detailed_search("lebron james")
#'
#' # Search for a team with a smaller limit
#' fetch_detailed_search("golden state warriors", limit = 5)
#'
#' # Get full raw JSON response
#' fetch_detailed_search("nfl", raw = TRUE)
#' }
#'
#' @importFrom httr GET status_code content timeout
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @importFrom utils URLencode
#' @importFrom cli cli_abort cli_warn cli_inform
#' @export
fetch_detailed_search <- function(query, limit = 10, raw = FALSE, timeout = 10) {

  # Input validation
  if (missing(query) || !is.character(query) || length(query) != 1 || nchar(query) == 0) {
    cli::cli_abort("Query must be a non-empty character string.")
  }

  if (!is.numeric(limit) || length(limit) != 1 || limit < 1 || limit > 100) {
    cli::cli_abort("Limit must be a single integer between 1 and 100.")
  }

  if (!is.numeric(timeout) || length(timeout) != 1 || timeout < 1) {
    cli::cli_abort("Timeout must be a positive number.")
  }

  # Build URL with proper encoding
  base_url <- "https://site.web.api.espn.com/apis/search/v2"
  encoded_query <- utils::URLencode(query, reserved = TRUE)
  url <- paste0(base_url, "?query=", encoded_query, "&limit=", limit)

  # Perform HTTP GET with timeout and user agent
  tryCatch({
    resp <- httr::GET(
      url,
      httr::timeout(timeout),
      httr::user_agent("R ESPN Search Package")
    )

    # Check response status
    if (httr::status_code(resp) != 200) {
      cli::cli_abort(
        "ESPN API request failed with status {httr::status_code(resp)}.
        URL: {url}
        This may indicate the API endpoint has changed or is temporarily unavailable."
      )
    }

    # Parse JSON response
    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")

    if (nchar(content_text) == 0) {
      cli::cli_abort("Received empty response from ESPN API.")
    }

    data <- jsonlite::fromJSON(content_text, simplifyVector = FALSE)

  }, error = function(e) {
    cli::cli_abort("Failed to fetch data from ESPN API: {e$message}")
  })

  # Return raw data if requested
  if (raw == TRUE) {
    return(data)
  }

  # Check if results exist
  if (is.null(data$results) || length(data$results) == 0) {
    cli::cli_warn("No results found for query: '{query}'")
    return(tibble::tibble(
      type = character(0),
      name = character(0),
      description = character(0),
      subtitle = character(0),
      link = character(0),
      id = character(0),
      category = character(0)
    ))
  }

  # Helper function to safely extract fields
  safe_extract <- function(item, field, default = NA_character_) {
    if (is.null(item)) return(default)
    if (is.list(item) && !is.null(item[[field]])) {
      return(as.character(item[[field]]))
    }
    return(default)
  }

  # Process results into a clean tibble
  all_results <- list()

  for (result_category in data$results) {
    category_name <- safe_extract(result_category, "type", "unknown")

    if (!is.null(result_category$contents) && length(result_category$contents) > 0) {

      category_results <- lapply(result_category$contents, function(item) {
        tibble::tibble(
          type = safe_extract(item, "type"),
          name = safe_extract(item, "displayName"),
          description = safe_extract(item, "description"),
          subtitle = safe_extract(item, "subtitle"),
          link = safe_extract(item$link, "web"),
          id = safe_extract(item, "id"),
          category = category_name
        )
      })

      all_results <- append(all_results, category_results)
    }
  }

  # Combine all results
  if (length(all_results) == 0) {
    cli::cli_warn("No processable results found for query: '{query}'")
    return(tibble::tibble(
      type = character(0),
      name = character(0),
      description = character(0),
      subtitle = character(0),
      link = character(0),
      id = character(0),
      category = character(0)
    ))
  }

  final_results <- do.call(rbind, all_results)

  # Apply limit across all results
  if (nrow(final_results) > limit) {
    final_results <- final_results[1:limit, ]
  }

  cli::cli_inform("Found {nrow(final_results)} results for query: '{query}'")

  return(final_results)
}
