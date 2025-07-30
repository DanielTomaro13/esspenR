#' Fetch ESPN season & event data
#'
#' @description
#' Retrieves season structures, week details, and events from ESPN's sports.core.api
#' for a given sport and league. Can fetch:
#' - Available seasons
#' - Season details (types)
#' - Weeks within a season type
#' - Events for a season, week, or date range
#'
#' Automatically follows `$ref` links to get complete data structures.
#'
#' @param sport Character. Sport slug (e.g. "football", "basketball").
#' @param league Character. League slug (e.g. "nfl", "nba").
#' @param year Optional integer. Season year (e.g. 2025).
#' @param st Optional integer. Season type (1: Pre, 2: Reg, 3: Post, 4: Off).
#' @param wn Optional integer. Week number.
#' @param dates Optional character. Year ("2025"), date ("20250830"), or range ("20250801-20250831").
#' @param limit Max items when listing seasons/events. Default 100.
#' @param raw Logical. If TRUE, returns raw JSON data, following `$ref` links.
#' @param extract_all Logical. If TRUE, creates dataframes in global environment.
#'
#' @return
#' - If `raw = TRUE`: raw nested list.
#' - If `extract_all = TRUE`: invisibly returns list and creates dataframes.
#' - Else: tibble with primary data.
#'
#' @details
#' Simplifies calls to multiple ESPN endpoints for seasons, weeks, and events. Designed to be
#' CRAN-compliant, with robust error checks and no pipes. ESPN API is unofficial & may change.
#'
#' @examples
#' \dontrun{
#' fetch_seasons("football", "nfl")                  # List all seasons
#' fetch_seasons("football", "nfl", year = 2025)     # Season types
#' fetch_seasons("football", "nfl", year = 2025, st = 2)   # Weeks in regular season
#' fetch_seasons("football", "nfl", dates = "2025")  # All events for 2025
#' fetch_seasons("football", "nfl", raw = TRUE)      # Raw nested JSON
#' }
#'
#' @export
fetch_seasons <- function(sport, league, year = NULL, st = NULL, wn = NULL,
                                     dates = NULL, limit = 100, raw = FALSE, extract_all = FALSE) {
  base_url <- "https://sports.core.api.espn.com/v2"

  get_json <- function(url) {
    resp <- httr::GET(url)
    if (httr::status_code(resp) != 200) {
      cli::cli_abort("Request failed: HTTP {httr::status_code(resp)} - {url}")
    }
    jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)
  }

  safe_extract <- function(data, field, default = NA) {
    if (is.null(data[[field]])) return(default)
    data[[field]]
  }

  # Handle general events by date
  if (!is.null(dates)) {
    url <- sprintf("%s/sports/%s/leagues/%s/events?dates=%s&limit=%s",
                   base_url, sport, league, dates, limit)
    data <- get_json(url)

    if (isTRUE(raw)) {
      # If raw and has $ref links, fetch the linked content
      if (!is.null(data$items) && !is.null(data$items$`$ref`)) {
        raw_events <- list()
        for (i in seq_along(data$items$`$ref`)) {
          raw_events[[i]] <- get_json(data$items$`$ref`[i])
        }
        return(raw_events)
      }
      return(data)
    }

    if (isTRUE(extract_all)) {
      # Extract event schedule
      if (!is.null(data$items) && !is.null(data$items$`$ref`)) {
        events_list <- list()
        for (i in seq_along(data$items$`$ref`)) {
          event_data <- get_json(data$items$`$ref`[i])
          events_list[[i]] <- tibble::tibble(
            id = safe_extract(event_data, "id", NA_character_),
            date = safe_extract(event_data, "date", NA_character_),
            name = safe_extract(event_data, "name", NA_character_),
            shortName = safe_extract(event_data, "shortName", NA_character_),
            season_year = safe_extract(event_data$season, "year", NA_integer_),
            week_number = safe_extract(event_data$week, "number", NA_integer_)
          )
        }
        season_events <- do.call(rbind, events_list)
        assign("season_events", tibble::as_tibble(season_events), envir = .GlobalEnv)
        message("Created dataframe: season_events (", nrow(season_events), " rows)")
        return(invisible(season_events))
      }
    }

    return(tibble::as_tibble(data$items))
  }

  # Handle events by week
  if (!is.null(year) && !is.null(st) && !is.null(wn)) {
    # Get week events
    events_url <- sprintf("%s/sports/%s/leagues/%s/seasons/%s/types/%s/weeks/%s/events",
                          base_url, sport, league, year, st, wn)
    events_data <- get_json(events_url)

    # Get week details
    week_url <- sprintf("%s/sports/%s/leagues/%s/seasons/%s/types/%s/weeks/%s",
                        base_url, sport, league, year, st, wn)
    week_data <- get_json(week_url)

    if (isTRUE(raw)) {
      result <- list(
        week_details = week_data,
        events = events_data
      )

      # If events has $ref links, fetch the linked content
      if (!is.null(events_data$items) && !is.null(events_data$items$`$ref`)) {
        raw_events <- list()
        for (i in seq_along(events_data$items$`$ref`)) {
          raw_events[[i]] <- get_json(events_data$items$`$ref`[i])
        }
        result$events_detailed <- raw_events
      }

      return(result)
    }

    if (isTRUE(extract_all)) {
      # Week info
      week_info <- tibble::tibble(
        type_id = st,
        week_number = safe_extract(week_data, "number", NA_integer_),
        startDate = safe_extract(week_data, "startDate", NA_character_),
        endDate = safe_extract(week_data, "endDate", NA_character_),
        text = safe_extract(week_data, "text", NA_character_),
        detail = safe_extract(week_data, "detail", NA_character_)
      )

      assign("week_info", week_info, envir = .GlobalEnv)
      message("Created dataframe: week_info (", nrow(week_info), " rows)")

      # Week events
      if (!is.null(events_data$items) && !is.null(events_data$items$`$ref`)) {
        events_list <- list()
        for (i in seq_along(events_data$items$`$ref`)) {
          event_data <- get_json(events_data$items$`$ref`[i])
          events_list[[i]] <- tibble::tibble(
            id = safe_extract(event_data, "id", NA_character_),
            date = safe_extract(event_data, "date", NA_character_),
            name = safe_extract(event_data, "name", NA_character_),
            shortName = safe_extract(event_data, "shortName", NA_character_),
            week_number = safe_extract(event_data$week, "number", NA_integer_)
          )
        }
        week_events <- do.call(rbind, events_list)
        assign("week_events", tibble::as_tibble(week_events), envir = .GlobalEnv)
        message("Created dataframe: week_events (", nrow(week_events), " rows)")
      }

      return(invisible(list(week_info = week_info, week_events = week_events)))
    }

    return(list(
      week_info = tibble::as_tibble(week_data),
      week_events = tibble::as_tibble(events_data$items)
    ))
  }

  # Handle season type and weeks
  if (!is.null(year) && !is.null(st)) {
    url <- sprintf("%s/sports/%s/leagues/%s/seasons/%s/types/%s/weeks",
                   base_url, sport, league, year, st)
    data <- get_json(url)

    if (isTRUE(raw)) {
      # If has $ref links, fetch the linked content
      if (!is.null(data$items) && !is.null(data$items$`$ref`)) {
        raw_weeks <- list()
        for (i in seq_along(data$items$`$ref`)) {
          raw_weeks[[i]] <- get_json(data$items$`$ref`[i])
        }
        return(raw_weeks)
      }
      return(data)
    }

    return(tibble::as_tibble(data$items))
  }

  # Handle season details
  if (!is.null(year)) {
    url <- sprintf("%s/sports/%s/leagues/%s/seasons/%s", base_url, sport, league, year)
    data <- get_json(url)

    if (isTRUE(raw)) {
      result <- list(season_details = data)

      # If has types with $ref links, fetch them
      if (!is.null(data$types$items) && !is.null(data$types$items$`$ref`)) {
        raw_types <- list()
        for (i in seq_along(data$types$items$`$ref`)) {
          raw_types[[i]] <- get_json(data$types$items$`$ref`[i])
        }
        result$season_types_detailed <- raw_types
      }

      return(result)
    }

    if (isTRUE(extract_all)) {
      # Season info
      season_info <- tibble::tibble(
        year = safe_extract(data, "year", NA_integer_),
        displayName = safe_extract(data, "displayName", NA_character_),
        startDate = safe_extract(data, "startDate", NA_character_),
        endDate = safe_extract(data, "endDate", NA_character_),
        sport = sport,
        league = league
      )

      assign("season_info", season_info, envir = .GlobalEnv)
      message("Created dataframe: season_info (", nrow(season_info), " rows)")

      # Season types
      if (!is.null(data$types$items) && !is.null(data$types$items$`$ref`)) {
        types_list <- list()
        for (i in seq_along(data$types$items$`$ref`)) {
          type_data <- get_json(data$types$items$`$ref`[i])
          types_list[[i]] <- tibble::tibble(
            type_id = safe_extract(type_data, "id", NA_character_),
            type_number = safe_extract(type_data, "type", NA_integer_),
            name = safe_extract(type_data, "name", NA_character_),
            abbreviation = safe_extract(type_data, "abbreviation", NA_character_),
            year = safe_extract(type_data, "year", NA_integer_),
            startDate = safe_extract(type_data, "startDate", NA_character_),
            endDate = safe_extract(type_data, "endDate", NA_character_)
          )
        }
        season_types <- do.call(rbind, types_list)
        assign("season_types", tibble::as_tibble(season_types), envir = .GlobalEnv)
        message("Created dataframe: season_types (", nrow(season_types), " rows)")
      }

      return(invisible(list(season_info = season_info, season_types = season_types)))
    }

    return(tibble::as_tibble(data))
  }

  # List all seasons
  url <- sprintf("%s/sports/%s/leagues/%s/seasons?limit=%s", base_url, sport, league, limit)
  data <- get_json(url)

  if (isTRUE(raw)) {
    # If has $ref links, fetch the linked content
    if (!is.null(data$items) && !is.null(data$items$`$ref`)) {
      raw_seasons <- list()
      for (i in seq_along(data$items$`$ref`)) {
        raw_seasons[[i]] <- get_json(data$items$`$ref`[i])
      }
      return(raw_seasons)
    }
    return(data)
  }

  if (isTRUE(extract_all)) {
    if (!is.null(data$items) && !is.null(data$items$`$ref`)) {
      seasons_list <- list()
      for (i in seq_along(data$items$`$ref`)) {
        season_data <- get_json(data$items$`$ref`[i])
        seasons_list[[i]] <- tibble::tibble(
          year = safe_extract(season_data, "year", NA_integer_),
          startDate = safe_extract(season_data, "startDate", NA_character_),
          endDate = safe_extract(season_data, "endDate", NA_character_),
          displayName = safe_extract(season_data, "displayName", NA_character_),
          sport = sport,
          league = league
        )
      }
      all_seasons <- do.call(rbind, seasons_list)
      assign("all_seasons", tibble::as_tibble(all_seasons), envir = .GlobalEnv)
      message("Created dataframe: all_seasons (", nrow(all_seasons), " rows)")
      return(invisible(all_seasons))
    }
  }

  return(tibble::as_tibble(data$items))
}
