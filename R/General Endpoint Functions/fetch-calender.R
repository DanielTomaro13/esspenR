#' Fetch ESPN League Calendar Data and Create Cleaned Dataframes
#'
#' @description
#' Fetches ESPN calendar data (ondays, offdays, whitelist, blacklist) and
#' creates cleaned dataframes directly in the global environment with
#' standardized naming: {sport}_{league}_{calendar_type}_{dates|sections|entries}.
#' Optionally returns raw JSON with \code{raw = TRUE}.
#'
#' @section Calendar Types:
#' \describe{
#'   \item{ondays}{Returns dates when games/events ARE scheduled to occur}
#'   \item{offdays}{Returns dates when games/events are NOT scheduled to occur}
#'   \item{whitelist}{Returns dates that are approved/allowed for scheduling events}
#'   \item{blacklist}{Returns dates that are blocked/prohibited from having events}
#' }
#'
#' @param sport Character. Sport slug (e.g. 'football', 'basketball', 'baseball').
#' @param league Character. League slug (e.g. 'nfl', 'nba', 'mlb').
#' @param calendar_type Character. One of 'ondays', 'offdays', 'whitelist', 'blacklist'.
#' @param raw Logical, default FALSE. If TRUE, returns full parsed JSON.
#' @param prefix Character, optional. Custom prefix for dataframe names instead of auto-generated.
#' @param timeout Numeric, default 30. Timeout for HTTP request in seconds.
#' @param user_agent Character, default NULL. Custom user agent string.
#'
#' @return Invisibly returns a character vector of created dataframe names.
#'         If \code{raw = TRUE}, returns the parsed JSON response.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all NFL game dates (creates: football_nfl_ondays_dates, etc.)
#' fetch_calender("football", "nfl", "ondays")
#'
#' # Check what dataframes were created
#' ls(pattern = "football_nfl_ondays")
#'
#' # Use the created dataframes
#' head(football_nfl_ondays_dates)
#' head(football_nfl_ondays_sections)
#' head(football_nfl_ondays_entries)
#'
#' # Get NBA off-season dates
#' fetch_calender("basketball", "nba", "offdays")
#'
#' # Get MLB dates available for scheduling
#' fetch_calender("baseball", "mlb", "whitelist")
#'
#' # Get NHL dates blocked from scheduling
#' fetch_calender("hockey", "nhl", "blacklist")
#'
#' # Use custom prefix for dataframe names
#' fetch_calender("football", "nfl", "ondays", prefix = "nfl_schedule")
#' # Creates: nfl_schedule_dates, nfl_schedule_sections, nfl_schedule_entries
#'
#' # Get raw JSON response instead of processed dataframes
#' raw_data <- fetch_calender("football", "nfl", "ondays", raw = TRUE)
#'
#' # College football calendar
#' fetch_calender("football", "college-football", "ondays")
#'
#' # Women's college basketball
#' fetch_calender("basketball", "womens-college-basketball", "ondays")
#' }
#'
#' @importFrom httr GET status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom utils head
#'
fetch_calender <- function(sport,
                           league,
                           calendar_type = c("ondays", "offdays", "whitelist", "blacklist"),
                           raw = FALSE,
                           prefix = NULL,
                           timeout = 30,
                           user_agent = NULL) {

  # Input validation
  if (!is.character(sport) || length(sport) != 1 || sport == "") {
    stop("'sport' must be a non-empty character string")
  }

  if (!is.character(league) || length(league) != 1 || league == "") {
    stop("'league' must be a non-empty character string")
  }

  calendar_type <- match.arg(calendar_type)

  if (!is.logical(raw) || length(raw) != 1) {
    stop("'raw' must be a logical value")
  }

  if (!is.null(prefix) && (!is.character(prefix) || length(prefix) != 1)) {
    stop("'prefix' must be NULL or a character string")
  }

  if (!is.numeric(timeout) || length(timeout) != 1 || timeout <= 0) {
    stop("'timeout' must be a positive numeric value")
  }

  if (!is.null(user_agent) && (!is.character(user_agent) || length(user_agent) != 1)) {
    stop("'user_agent' must be NULL or a character string")
  }

  # Build URL
  url <- sprintf(
    "https://sports.core.api.espn.com/v2/sports/%s/leagues/%s/calendar/%s",
    sport, league, calendar_type
  )

  # Set up request configuration
  if (is.null(user_agent)) {
    user_agent <- "R package fetch_calender"
  }

  # Make HTTP request with error handling
  tryCatch({
    resp <- httr::GET(url,
                      httr::timeout(timeout),
                      httr::user_agent(user_agent))
  }, error = function(e) {
    stop("HTTP request failed: ", e$message)
  })

  # Check response status
  if (httr::status_code(resp) != 200) {
    stop("ESPN endpoint returned HTTP ", httr::status_code(resp), " for URL: ", url)
  }

  # Parse JSON response
  tryCatch({
    response_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(response_text, flatten = FALSE)
  }, error = function(e) {
    stop("Failed to parse JSON response: ", e$message)
  })

  # Return raw data if requested
  if (isTRUE(raw)) {
    return(data)
  }

  # Generate dataframe names
  if (is.null(prefix)) {
    # Clean up sport and league names for valid R object names
    sport_clean <- gsub("[^a-zA-Z0-9_]", "_", sport)
    league_clean <- gsub("[^a-zA-Z0-9_]", "_", league)
    base_name <- paste(sport_clean, league_clean, calendar_type, sep = "_")
  } else {
    base_name <- gsub("[^a-zA-Z0-9_]", "_", prefix)
  }

  dates_name <- paste0(base_name, "_dates")
  sections_name <- paste0(base_name, "_sections")
  entries_name <- paste0(base_name, "_entries")

  # Build and clean dates dataframe
  dates_df <- data.frame()

  # Extract dates from multiple possible locations
  dates_vector <- NULL
  if (!is.null(data$eventDate) && !is.null(data$eventDate$dates)) {
    dates_vector <- data$eventDate$dates
  } else if (!is.null(data$dates)) {
    dates_vector <- data$dates
  } else if (!is.null(data$items)) {
    # Sometimes dates are in items
    if (is.list(data$items) && length(data$items) > 0) {
      dates_vector <- unlist(lapply(data$items, function(x) x$date))
    }
  }

  if (!is.null(dates_vector) && length(dates_vector) > 0) {
    # Convert to Date objects, handling potential parsing errors
    parsed_dates <- tryCatch({
      as.Date(dates_vector)
    }, error = function(e) {
      # Try alternative parsing methods
      tryCatch({
        as.Date(dates_vector, format = "%Y-%m-%d")
      }, error = function(e2) {
        as.Date(dates_vector, format = "%m/%d/%Y")
      })
    })

    if (any(!is.na(parsed_dates))) {
      dates_df <- data.frame(date = parsed_dates[!is.na(parsed_dates)])

      # Add comprehensive date analysis columns
      dates_df$year <- as.integer(format(dates_df$date, "%Y"))
      dates_df$month <- as.integer(format(dates_df$date, "%m"))
      dates_df$day <- as.integer(format(dates_df$date, "%d"))
      dates_df$weekday <- weekdays(dates_df$date, abbreviate = TRUE)
      dates_df$weekday_num <- as.integer(format(dates_df$date, "%w"))
      dates_df$week_of_year <- as.integer(format(dates_df$date, "%V"))
      dates_df$quarter <- ceiling(dates_df$month / 3)
      dates_df$is_weekend <- dates_df$weekday_num %in% c(0, 6)
      dates_df$month_name <- months(dates_df$date, abbreviate = TRUE)
      dates_df$julian_day <- as.integer(format(dates_df$date, "%j"))

      # Sort by date
      dates_df <- dates_df[order(dates_df$date), ]
      rownames(dates_df) <- NULL
    }
  }

  # Build and clean sections dataframe
  sections_df <- data.frame()
  if (!is.null(data$sections) && is.list(data$sections) && length(data$sections) > 0) {
    tryCatch({
      # Handle sections as list or data.frame
      if (is.data.frame(data$sections)) {
        sections_df <- data$sections
      } else {
        # Convert list to data.frame
        sections_list <- lapply(data$sections, function(x) {
          if (is.list(x)) {
            # Flatten nested lists
            flat_list <- unlist(x, recursive = FALSE)
            # Convert to data.frame row
            as.data.frame(flat_list, stringsAsFactors = FALSE)
          } else {
            data.frame(value = x, stringsAsFactors = FALSE)
          }
        })
        sections_df <- do.call(rbind, sections_list)
      }

      # Clean up sections dataframe
      if (nrow(sections_df) > 0) {
        # Convert date columns
        date_cols <- names(sections_df)[grepl("date|Date", names(sections_df), ignore.case = TRUE)]
        for (col in date_cols) {
          if (col %in% names(sections_df)) {
            sections_df[[col]] <- tryCatch({
              as.Date(sections_df[[col]])
            }, error = function(e) {
              sections_df[[col]]  # Keep original if conversion fails
            })
          }
        }

        # Clean character columns
        char_cols <- names(sections_df)[sapply(sections_df, is.character)]
        for (col in char_cols) {
          sections_df[[col]] <- trimws(sections_df[[col]])
        }

        rownames(sections_df) <- NULL
      }
    }, error = function(e) {
      warning("Failed to process sections data: ", e$message)
    })
  }

  # Build and clean entries dataframe
  entries_df <- data.frame()
  if (!is.null(data$sections) && is.list(data$sections) && length(data$sections) > 0) {
    tryCatch({
      all_entries <- list()

      # Process each section
      for (i in seq_along(data$sections)) {
        section <- data$sections[[i]]
        section_label <- if (!is.null(section$label)) section$label else paste("Section", i)

        if (!is.null(section$entries) && length(section$entries) > 0) {
          if (is.data.frame(section$entries)) {
            entry_df <- section$entries
          } else if (is.list(section$entries)) {
            # Convert list entries to data.frame
            entry_list <- lapply(section$entries, function(entry) {
              if (is.list(entry)) {
                # Flatten and convert to data.frame
                flat_entry <- unlist(entry, recursive = FALSE)
                as.data.frame(flat_entry, stringsAsFactors = FALSE)
              } else {
                data.frame(value = entry, stringsAsFactors = FALSE)
              }
            })
            entry_df <- do.call(rbind, entry_list)
          } else {
            entry_df <- data.frame(value = section$entries, stringsAsFactors = FALSE)
          }

          # Add section identifier
          entry_df$section <- section_label
          all_entries[[i]] <- entry_df
        }
      }

      # Combine all entries
      if (length(all_entries) > 0) {
        entries_df <- do.call(rbind, all_entries)

        # Clean up entries dataframe
        if (nrow(entries_df) > 0) {
          # Convert date columns
          date_cols <- names(entries_df)[grepl("date|Date", names(entries_df), ignore.case = TRUE)]
          for (col in date_cols) {
            if (col %in% names(entries_df)) {
              entries_df[[col]] <- tryCatch({
                as.Date(entries_df[[col]])
              }, error = function(e) {
                entries_df[[col]]  # Keep original if conversion fails
              })
            }
          }

          # Clean character columns
          char_cols <- names(entries_df)[sapply(entries_df, is.character)]
          for (col in char_cols) {
            entries_df[[col]] <- trimws(entries_df[[col]])
          }

          # Move section column to the front if it exists
          if ("section" %in% names(entries_df)) {
            col_order <- c("section", setdiff(names(entries_df), "section"))
            entries_df <- entries_df[, col_order, drop = FALSE]
          }

          rownames(entries_df) <- NULL
        }
      }
    }, error = function(e) {
      warning("Failed to process entries data: ", e$message)
    })
  }

  # Assign dataframes to global environment
  assign(dates_name, dates_df, envir = .GlobalEnv)
  assign(sections_name, sections_df, envir = .GlobalEnv)
  assign(entries_name, entries_df, envir = .GlobalEnv)

  # Print summary of what was created
  message("Created ", dates_name, " with ", nrow(dates_df), " rows")
  message("Created ", sections_name, " with ", nrow(sections_df), " rows")
  message("Created ", entries_name, " with ", nrow(entries_df), " rows")

  # Return names of created dataframes invisibly
  invisible(c(dates_name, sections_name, entries_name))
}
