#' Fetch football (soccer) transfer data using ESPN API
#'
#' @param league Character. League identifier (e.g., "eng.1", "usa.1", "esp.1", "ger.1", "ita.1", "fra.1").
#' @param limit Integer. Number of transfers to fetch (default: 100, max: 1000).
#' @param offset Integer. Starting position for pagination (default: 0).
#' @param raw Logical. If TRUE, assigns raw JSON to global environment as 'football_transfers_raw'. Default FALSE.
#' @return Invisibly returns the data, but main purpose is global environment assignment.
#' @export
#'
#' @examples
#' \dontrun{
#' # Get Premier League transfers
#' fetch_football_transfers("eng.1")
#' head(football_transfers)
#'
#' # Get MLS transfers with pagination
#' fetch_football_transfers("usa.1", limit = 50, offset = 100)
#'
#' # Get La Liga transfers
#' fetch_football_transfers("esp.1", limit = 200)
#'
#' # Get raw data for custom processing
#' fetch_football_transfers("ger.1", limit = 50, raw = TRUE)
#' str(football_transfers_raw, max.level = 2)
#' }
#'
fetch_football_transfers <- function(league, limit = 100, offset = 0, raw = FALSE) {

  # Validate inputs
  if (missing(league) || !is.character(league) || length(league) != 1) {
    stop("'league' must be a single character string (e.g., 'eng.1', 'usa.1', 'esp.1')")
  }

  if (!is.numeric(limit) || limit < 1 || limit > 1000) {
    stop("'limit' must be a number between 1 and 1000")
  }

  if (!is.numeric(offset) || offset < 0) {
    stop("'offset' must be a non-negative number")
  }

  if (!is.logical(raw)) {
    stop("'raw' must be TRUE or FALSE")
  }

  # Build URL using the ESPN transactions endpoint
  base_url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/soccer/%s/transactions", league)

  # Build query parameters
  params <- list(
    limit = limit,
    offset = offset
  )

  # Build URL with parameters
  url <- httr::modify_url(base_url, query = params)

  # Fetch and parse
  tryCatch({
    resp <- httr::GET(url, httr::timeout(60))

    if (httr::status_code(resp) != 200) {
      stop(sprintf("API request failed: HTTP %d - %s",
                   httr::status_code(resp),
                   httr::http_status(resp)$message))
    }

    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content_text, simplifyVector = FALSE, simplifyDataFrame = FALSE)

    # Handle raw data assignment
    if (isTRUE(raw)) {
      assign("football_transfers_raw", data, envir = .GlobalEnv)
      message("Raw football transfers data assigned to: football_transfers_raw")
      return(invisible(data))
    }

    # Create clean transfers dataset
    transfers_df <- create_clean_transfers_dataset(data, league)

    # Assign to global environment
    assign("football_transfers", transfers_df, envir = .GlobalEnv)

    # Create informative message
    filter_info <- create_transfers_filter_message(league)
    message(sprintf("Football transfers data assigned to: football_transfers (%d transfers%s)",
                    nrow(transfers_df), filter_info))

    return(invisible(transfers_df))

  }, error = function(e) {
    stop(sprintf("Failed to fetch football transfers data: %s", e$message))
  })
}

#' Create clean transfers dataset from ESPN transactions API response
#'
#' @param data Raw API response data
#' @param league League identifier
#' @return Clean data frame with transfer information
#' @keywords internal
create_clean_transfers_dataset <- function(data, league) {

  # Initialize result data frame
  result_df <- data.frame(
    transfer_date = character(0),
    player_id = character(0),
    player_first_name = character(0),
    player_last_name = character(0),
    player_display_name = character(0),
    player_jersey = character(0),
    from_team_id = character(0),
    from_team_name = character(0),
    from_team_abbreviation = character(0),
    to_team_id = character(0),
    to_team_name = character(0),
    to_team_abbreviation = character(0),
    transfer_type = character(0),
    transfer_amount = numeric(0),
    transfer_display_amount = character(0),
    league = character(0),
    stringsAsFactors = FALSE
  )

  # Check if we have transaction data
  if (is.null(data) || !"transactions" %in% names(data) || length(data[["transactions"]]) == 0) {
    return(result_df)
  }

  transactions <- data[["transactions"]]

  # Process each transaction [[1]], [[2]], [[3]]...
  for (i in seq_along(transactions)) {
    transaction <- transactions[[i]]

    # Extract transfer date
    transfer_date <- if ("date" %in% names(transaction)) transaction[["date"]] else NA_character_

    # Extract athlete information
    player_id <- player_first_name <- player_last_name <- player_display_name <- player_jersey <- NA_character_
    if ("athlete" %in% names(transaction)) {
      athlete <- transaction[["athlete"]]
      player_id <- if ("id" %in% names(athlete)) athlete[["id"]] else NA_character_
      player_first_name <- if ("firstName" %in% names(athlete)) athlete[["firstName"]] else NA_character_
      player_last_name <- if ("lastName" %in% names(athlete)) athlete[["lastName"]] else NA_character_
      player_display_name <- if ("displayName" %in% names(athlete)) athlete[["displayName"]] else NA_character_
      player_jersey <- if ("jersey" %in% names(athlete)) athlete[["jersey"]] else NA_character_
    }

    # Extract "from" team information
    from_team_id <- from_team_name <- from_team_abbreviation <- NA_character_
    if ("from" %in% names(transaction)) {
      from_team <- transaction[["from"]]
      from_team_id <- if ("id" %in% names(from_team)) from_team[["id"]] else NA_character_
      from_team_name <- if ("displayName" %in% names(from_team)) from_team[["displayName"]] else NA_character_
      from_team_abbreviation <- if ("abbreviation" %in% names(from_team)) from_team[["abbreviation"]] else NA_character_
    }

    # Extract "to" team information
    to_team_id <- to_team_name <- to_team_abbreviation <- NA_character_
    if ("to" %in% names(transaction)) {
      to_team <- transaction[["to"]]
      to_team_id <- if ("id" %in% names(to_team)) to_team[["id"]] else NA_character_
      to_team_name <- if ("displayName" %in% names(to_team)) to_team[["displayName"]] else NA_character_
      to_team_abbreviation <- if ("abbreviation" %in% names(to_team)) to_team[["abbreviation"]] else NA_character_
    }

    # Extract transfer financial information
    transfer_type <- if ("type" %in% names(transaction)) transaction[["type"]] else NA_character_
    transfer_amount <- if ("amount" %in% names(transaction)) as.numeric(transaction[["amount"]]) else NA_real_
    transfer_display_amount <- if ("displayAmount" %in% names(transaction)) transaction[["displayAmount"]] else NA_character_

    # Create transfer row
    transfer_row <- data.frame(
      transfer_date = transfer_date,
      player_id = player_id,
      player_first_name = player_first_name,
      player_last_name = player_last_name,
      player_display_name = player_display_name,
      player_jersey = player_jersey,
      from_team_id = from_team_id,
      from_team_name = from_team_name,
      from_team_abbreviation = from_team_abbreviation,
      to_team_id = to_team_id,
      to_team_name = to_team_name,
      to_team_abbreviation = to_team_abbreviation,
      transfer_type = transfer_type,
      transfer_amount = transfer_amount,
      transfer_display_amount = transfer_display_amount,
      league = league,
      stringsAsFactors = FALSE
    )

    # Add to result
    result_df <- rbind(result_df, transfer_row)
  }

  # Clean up and sort
  if (nrow(result_df) > 0) {
    rownames(result_df) <- NULL
    # Sort by date (most recent first)
    if (any(!is.na(result_df$transfer_date))) {
      result_df <- result_df[order(result_df$transfer_date, decreasing = TRUE, na.last = TRUE), ]
    }
    rownames(result_df) <- NULL
  }

  return(result_df)
}

#' Create informative filter message for transfers
#'
#' @param league League identifier
#' @return Character string describing applied filters
#' @keywords internal
create_transfers_filter_message <- function(league) {
  filters <- character(0)

  # League names
  league_names <- list(
    "eng.1" = "Premier League",
    "esp.1" = "La Liga",
    "ger.1" = "Bundesliga",
    "ita.1" = "Serie A",
    "fra.1" = "Ligue 1",
    "usa.1" = "MLS",
    "mex.1" = "Liga MX",
    "ned.1" = "Eredivisie",
    "por.1" = "Primeira Liga",
    "bra.1" = "Brasileirão"
  )

  league_display <- if (league %in% names(league_names)) league_names[[league]] else league
  filters <- c(filters, sprintf("league: %s", league_display))

  if (length(filters) > 0) {
    return(sprintf(", filters: %s", paste(filters, collapse = ", ")))
  }

  return("")
}

#' Fetch all football transfers with pagination
#'
#' @param league Character. League identifier (e.g., "eng.1", "usa.1", "esp.1").
#' @param batch_size Integer. Number of transfers to fetch per request (default: 1000).
#' @param max_transfers Integer. Maximum total transfers to fetch (default: 10000).
#' @param raw Logical. If TRUE, assigns raw JSON to global environment. Default FALSE.
#' @return Invisibly returns the data, but main purpose is global environment assignment.
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all Premier League transfers
#' fetch_all_football_transfers("eng.1")
#'
#' # Get all MLS transfers with smaller batches
#' fetch_all_football_transfers("usa.1", batch_size = 500)
#'
#' # Get all La Liga transfers
#' fetch_all_football_transfers("esp.1")
#' }
#'
fetch_all_football_transfers <- function(league, batch_size = 1000, max_transfers = 10000, raw = FALSE) {

  if (missing(league)) {
    stop("'league' parameter is required (e.g., 'eng.1', 'usa.1', 'esp.1')")
  }

  all_transfers <- data.frame()
  offset <- 0
  total_fetched <- 0

  message(sprintf("Starting to fetch football transfers data for league: %s...", league))

  while (total_fetched < max_transfers) {
    # Calculate how many to fetch this round
    current_limit <- min(batch_size, max_transfers - total_fetched)

    # Fetch batch
    batch_data <- fetch_football_transfers(
      league = league,
      limit = current_limit,
      offset = offset,
      raw = raw
    )

    # If raw requested, return after first batch
    if (isTRUE(raw)) {
      return(invisible(batch_data))
    }

    # Get the data that was assigned to global environment
    batch_df <- get("football_transfers", envir = .GlobalEnv)

    # If no more data, break
    if (nrow(batch_df) == 0) {
      break
    }

    # Add to combined dataset
    all_transfers <- rbind(all_transfers, batch_df)
    total_fetched <- nrow(all_transfers)
    offset <- offset + current_limit

    message(sprintf("Fetched %d transfers so far...", total_fetched))

    # If we got less than requested, we've reached the end
    if (nrow(batch_df) < current_limit) {
      break
    }

    # Small delay to be respectful to the API
    Sys.sleep(0.1)
  }

  # Remove duplicates based on player and date (just in case)
  if (nrow(all_transfers) > 0) {
    all_transfers <- all_transfers[!duplicated(paste(all_transfers$player_id, all_transfers$transfer_date)), ]
  }

  # Assign final dataset to global environment
  assign("football_transfers", all_transfers, envir = .GlobalEnv)

  # Create informative message
  filter_info <- create_transfers_filter_message(league)

  message(sprintf("Completed! Football transfers data assigned to: football_transfers (%d total transfers%s)",
                  nrow(all_transfers), filter_info))

  return(invisible(all_transfers))
}
