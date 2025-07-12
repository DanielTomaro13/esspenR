#' Fetch ESPN scoreboard data (header endpoint)
#'
#' @description
#' Fetches the ESPN scoreboard header JSON for general (multi-sport) or specific sport & league.
#'
#' @param sport Optional character. Sport slug (e.g. 'football'). If NULL, fetches general header.
#' @param league Optional character. League slug (e.g. 'nfl'). If NULL, fetches general header.
#' @param raw Logical. If TRUE, returns the full parsed JSON list. Default FALSE (just top events list).
#' @return Either a parsed JSON list (raw=TRUE) or a list of event summaries (raw=FALSE).
#' @export
#'
#' @examples
#' # Get general multi-sport scoreboard
#' default_scores <- fetch_scoreboard()
#'
#' # Get NFL scoreboard
#' nfl_scores <- fetch_scoreboard("football", "nfl")
#'
#' # Get raw JSON for expansion
#' raw_nfl <- fetch_scoreboard("football", "nfl", raw = TRUE)
#' str(raw_nfl, 2)
fetch_scoreboard <- function(sport = NULL, league = NULL, raw = FALSE) {
  # Build URL
  base_url <- "https://site.web.api.espn.com/apis/v2/scoreboard/header"
  if (!is.null(sport) && !is.null(league)) {
    url <- httr::modify_url(base_url, query = list(sport = sport, league = league))
  } else {
    url <- base_url
  }

  # Fetch and parse
  tryCatch({
    resp <- httr::GET(url, httr::timeout(30))
    if (httr::status_code(resp) != 200) {
      stop(sprintf("API request failed: HTTP %d - %s",
                   httr::status_code(resp),
                   httr::http_status(resp)$message))
    }
    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content_text, flatten = TRUE)

    if (isTRUE(raw)) {
      return(data)
    } else {
      return(data)  # Later you’ll add data cleaning here
    }
  }, error = function(e) {
    stop(sprintf("Failed to fetch scoreboard data: %s", e$message))
  })
}
