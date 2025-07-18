#' Fetch college football main league information using Core API
#'
#' Retrieves core college football league information from ESPN's Core API.
#' This is the root endpoint that provides foundational data about college football.
#' The function returns only the raw JSON response for exploration and debugging.
#'
#' @return Invisibly returns the raw JSON data. The main purpose is global
#'   environment assignment of \code{cfb_main_raw} containing the complete
#'   API response for inspection.
#'
#' @details
#' This function accesses ESPN's core league endpoint which serves as the root
#' for college football data. The raw JSON contains fundamental information
#' about the league structure, organization, and available data endpoints.
#'
#' The function is designed for exploration and understanding of the API structure
#' rather than creating processed datasets.
#'
#' @examples
#' \dontrun{
#' # Fetch core college football league data
#' fetch_college_football_main()
#'
#' # Examine the raw structure
#' str(cfb_main_raw, max.level = 2)
#'
#' # View available sections
#' names(cfb_main_raw)
#'
#' # Explore specific sections
#' if("leagues" %in% names(cfb_main_raw)) {
#'   str(cfb_main_raw$leagues, max.level = 1)
#' }
#'
#' if("sports" %in% names(cfb_main_raw)) {
#'   str(cfb_main_raw$sports, max.level = 1)
#' }
#' }
#'
#' @export
fetch_college_football_main <- function() {

  # Build API URL
  url <- "https://sports.core.api.espn.com/v2/sports/football/leagues/college-football/"

  # Fetch and parse data
  tryCatch({
    message("Fetching core college football league information...")

    resp <- httr::GET(url, httr::timeout(30))

    if (httr::status_code(resp) != 200) {
      stop(sprintf("API request failed: HTTP %d - %s",
                   httr::status_code(resp),
                   httr::http_status(resp)$message))
    }

    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content_text, simplifyVector = FALSE, simplifyDataFrame = FALSE)

    # Assign raw data to global environment
    assign("cfb_main_raw", data, envir = .GlobalEnv)
    message("Raw college football main data assigned to: cfb_main_raw")

    # Show basic structure preview
    sections <- names(data)
    message("- Available sections: ", paste(sections, collapse = ", "))
    message("- Total sections: ", length(sections))

    # Show some key information if available
    if ("$ref" %in% sections) {
      ref_url <- data[["$ref"]]
      message("- Reference URL: ", ref_url)
    }

    if ("id" %in% sections) {
      league_id <- data[["id"]]
      message("- League ID: ", league_id)
    }

    if ("name" %in% sections) {
      league_name <- data[["name"]]
      message("- League name: ", league_name)
    }

    message("\nUse str(cfb_main_raw, max.level = 2) to explore the structure")

    return(invisible(data))

  }, error = function(e) {
    stop(sprintf("Failed to fetch college football main data: %s", e$message))
  })
}
