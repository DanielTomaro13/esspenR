#' Fetch multiple NFL athletes' overview data
#'
#' @param athlete_ids Character or Numeric vector. ESPN athlete IDs.
#' @param return_type Character. Type of data to return: "stats", "news", "gamelog", "rotowire", "fantasy", or "all" (default: "all").
#' @param delay Numeric. Delay in seconds between API requests (default: 0.1).
#' @param raw Logical. If TRUE, assigns raw JSON to global environment. Default FALSE.
#' @return Invisibly returns the combined data, but main purpose is global environment assignment.
#' @export
#'
#' @examples
#' # Get stats data for multiple players
#' mahomes_id <- "3139477"
#' allen_id <- "3918298"
#' fetch_multiple_nfl_athlete_overviews(c(mahomes_id, allen_id), return_type = "stats")
#'
#' # Get all data types for multiple players
#' fetch_multiple_nfl_athlete_overviews(c(mahomes_id, allen_id), return_type = "all")
#'

#' Safe nested data extraction helper function
#'
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
extract_nested <- function(data, path, default = NA) {
  tryCatch({
    result <- data
    for (key in path) {
      if (is.null(result) || !key %in% names(result)) {
        return(default)
      }
      result <- result[[key]]
    }
    if (is.null(result)) default else result
  }, error = function(e) {
    default
  })
}

#' Create athlete statistics data frame from overview response
#'
#' @param data Raw JSON response from ESPN Web API athlete overview endpoint
#' @param athlete_id Athlete ID used in request
#' @return Clean data frame with athlete statistics information
create_athlete_stats_dataset <- function(data, athlete_id) {

  # Initialize result data frame for statistics
  result_df <- data.frame(
    athlete_id = character(0),
    stat_display_name = character(0),
    stat_category = character(0),
    stat_split = character(0),
    stat_label = character(0),
    stat_name = character(0),
    stat_display_name_full = character(0),
    stat_value = character(0),
    stringsAsFactors = FALSE
  )

  # Extract statistics from the overview structure
  stats_data <- extract_nested(data, c("statistics"), list())

  if (length(stats_data) == 0) {
    return(result_df)
  }

  # Extract main statistics info
  display_name <- extract_nested(stats_data, c("displayName"), NA_character_)
  labels <- extract_nested(stats_data, c("labels"), list())
  names_list <- extract_nested(stats_data, c("names"), list())
  display_names <- extract_nested(stats_data, c("displayNames"), list())

  # Process categories
  categories <- extract_nested(stats_data, c("categories"), list())

  # Process splits (different season types: Regular Season, Postseason, Career)
  splits <- extract_nested(stats_data, c("splits"), list())

  if (length(splits) > 0 && length(labels) > 0) {
    for (split_idx in seq_along(splits)) {
      split_data <- splits[[split_idx]]
      split_name <- extract_nested(split_data, c("displayName"), paste("Split", split_idx))
      split_stats <- extract_nested(split_data, c("stats"), list())

      # Process each stat within this split
      for (stat_idx in seq_along(split_stats)) {
        if (stat_idx <= length(labels) && stat_idx <= length(names_list) && stat_idx <= length(display_names)) {
          stat_row <- data.frame(
            athlete_id = athlete_id,
            stat_display_name = display_name,
            stat_category = ifelse(length(categories) > 0, extract_nested(categories[[1]], c("name"), "Unknown"), "Unknown"),
            stat_split = split_name,
            stat_label = ifelse(length(labels) >= stat_idx, labels[[stat_idx]], ""),
            stat_name = ifelse(length(names_list) >= stat_idx, names_list[[stat_idx]], ""),
            stat_display_name_full = ifelse(length(display_names) >= stat_idx, display_names[[stat_idx]], ""),
            stat_value = split_stats[[stat_idx]],
            stringsAsFactors = FALSE
          )

          result_df <- rbind(result_df, stat_row)
        }
      }
    }
  }

  # Clean up row names
  if (nrow(result_df) > 0) {
    rownames(result_df) <- NULL
  }

  return(result_df)
}

#' Create athlete rotowire data frame from overview response
#'
#' @param data Raw JSON response from ESPN Web API athlete overview endpoint
#' @param athlete_id Athlete ID used in request
#' @return Clean data frame with athlete rotowire information
create_athlete_rotowire_dataset <- function(data, athlete_id) {

  # Initialize result data frame for rotowire
  result_df <- data.frame(
    athlete_id = character(0),
    headline = character(0),
    story = character(0),
    description = character(0),
    published = character(0),
    stringsAsFactors = FALSE
  )

  # Extract rotowire data
  rotowire_data <- extract_nested(data, c("rotowire"), NULL)

  if (is.null(rotowire_data)) {
    return(result_df)
  }

  headline <- extract_nested(rotowire_data, c("headline"), NA_character_)
  story <- extract_nested(rotowire_data, c("story"), NA_character_)
  description <- extract_nested(rotowire_data, c("description"), NA_character_)
  published <- extract_nested(rotowire_data, c("published"), NA_character_)

  rotowire_row <- data.frame(
    athlete_id = athlete_id,
    headline = headline,
    story = story,
    description = description,
    published = published,
    stringsAsFactors = FALSE
  )

  result_df <- rbind(result_df, rotowire_row)

  # Clean up row names
  if (nrow(result_df) > 0) {
    rownames(result_df) <- NULL
  }

  return(result_df)
}

#' Create athlete fantasy data frame from overview response
#'
#' @param data Raw JSON response from ESPN Web API athlete overview endpoint
#' @param athlete_id Athlete ID used in request
#' @return Clean data frame with athlete fantasy information
create_athlete_fantasy_dataset <- function(data, athlete_id) {

  # Initialize result data frame for fantasy
  result_df <- data.frame(
    athlete_id = character(0),
    draft_rank = character(0),
    position_rank = character(0),
    percent_owned = character(0),
    last_7_days = character(0),
    projection = character(0),
    stringsAsFactors = FALSE
  )

  # Extract fantasy data
  fantasy_data <- extract_nested(data, c("fantasy"), NULL)

  if (is.null(fantasy_data)) {
    return(result_df)
  }

  draft_rank <- extract_nested(fantasy_data, c("draftRank"), NA_character_)
  position_rank <- extract_nested(fantasy_data, c("positionRank"), NA_character_)
  percent_owned <- extract_nested(fantasy_data, c("percentOwned"), NA_character_)
  last_7_days <- extract_nested(fantasy_data, c("last7Days"), NA_character_)
  projection <- extract_nested(fantasy_data, c("projection"), NA_character_)

  fantasy_row <- data.frame(
    athlete_id = athlete_id,
    draft_rank = draft_rank,
    position_rank = position_rank,
    percent_owned = percent_owned,
    last_7_days = last_7_days,
    projection = projection,
    stringsAsFactors = FALSE
  )

  result_df <- rbind(result_df, fantasy_row)

  # Clean up row names
  if (nrow(result_df) > 0) {
    rownames(result_df) <- NULL
  }

  return(result_df)
}

#' Create athlete news data frame from overview response
#'
#' @param data Raw JSON response from ESPN Web API athlete overview endpoint
#' @param athlete_id Athlete ID used in request
#' @return Clean data frame with athlete news information
create_athlete_news_dataset <- function(data, athlete_id) {

  # Initialize result data frame for news
  result_df <- data.frame(
    athlete_id = character(0),
    news_id = character(0),
    headline = character(0),
    description = character(0),
    published = character(0),
    last_modified = character(0),
    byline = character(0),
    section = character(0),
    type = character(0),
    premium = logical(0),
    web_link = character(0),
    mobile_link = character(0),
    stringsAsFactors = FALSE
  )

  # Extract news data
  news_data <- extract_nested(data, c("news"), list())

  if (length(news_data) == 0) {
    return(result_df)
  }

  # Process each news item
  for (news_item in news_data) {
    news_id <- extract_nested(news_item, c("id"), NA_character_)
    headline <- extract_nested(news_item, c("headline"), NA_character_)
    description <- extract_nested(news_item, c("description"), NA_character_)
    published <- extract_nested(news_item, c("published"), NA_character_)
    last_modified <- extract_nested(news_item, c("lastModified"), NA_character_)
    byline <- extract_nested(news_item, c("byline"), NA_character_)
    section <- extract_nested(news_item, c("section"), NA_character_)
    type <- extract_nested(news_item, c("type"), NA_character_)
    premium <- extract_nested(news_item, c("premium"), NA)
    web_link <- extract_nested(news_item, c("links", "web", "href"), NA_character_)
    mobile_link <- extract_nested(news_item, c("links", "mobile", "href"), NA_character_)

    if (!is.na(premium)) premium <- as.logical(premium)

    news_row <- data.frame(
      athlete_id = athlete_id,
      news_id = ifelse(is.na(news_id), NA_character_, as.character(news_id)),
      headline = headline,
      description = description,
      published = published,
      last_modified = last_modified,
      byline = byline,
      section = section,
      type = type,
      premium = premium,
      web_link = web_link,
      mobile_link = mobile_link,
      stringsAsFactors = FALSE
    )

    result_df <- rbind(result_df, news_row)
  }

  # Clean up row names
  if (nrow(result_df) > 0) {
    rownames(result_df) <- NULL
  }

  return(result_df)
}

#' Create athlete game log data frame from overview response
#'
#' @param data Raw JSON response from ESPN Web API athlete overview endpoint
#' @param athlete_id Athlete ID used in request
#' @return Clean data frame with athlete game log information
create_athlete_gamelog_dataset <- function(data, athlete_id) {

  # Initialize result data frame for game log
  result_df <- data.frame(
    athlete_id = character(0),
    event_id = character(0),
    game_date = character(0),
    opponent_id = character(0),
    opponent_name = character(0),
    opponent_abbreviation = character(0),
    home_away = character(0),
    game_result = character(0),
    score = character(0),
    week = character(0),
    stat_category = character(0),
    stat_label = character(0),
    stat_name = character(0),
    stat_display_name = character(0),
    stat_value = character(0),
    stringsAsFactors = FALSE
  )

  # Extract game log data
  gamelog_data <- extract_nested(data, c("gameLog"), list())

  if (length(gamelog_data) == 0) {
    return(result_df)
  }

  # Extract statistics definitions
  statistics <- extract_nested(gamelog_data, c("statistics"), list())
  events <- extract_nested(gamelog_data, c("events"), list())

  # Process each statistic category
  for (stat_category in statistics) {
    category_name <- extract_nested(stat_category, c("displayName"), "Unknown")
    labels <- extract_nested(stat_category, c("labels"), list())
    names_list <- extract_nested(stat_category, c("names"), list())
    display_names <- extract_nested(stat_category, c("displayNames"), list())
    category_events <- extract_nested(stat_category, c("events"), list())

    # Process each game event for this category
    for (event_idx in seq_along(category_events)) {
      event_data <- category_events[[event_idx]]
      event_id <- extract_nested(event_data, c("eventId"), NA_character_)
      event_stats <- extract_nested(event_data, c("stats"), list())

      # Get event details from the main events object
      event_details <- NULL
      if (!is.null(events) && event_id %in% names(events)) {
        event_details <- events[[event_id]]
      }

      if (!is.null(event_details)) {
        game_date <- extract_nested(event_details, c("gameDate"), NA_character_)
        opponent_id <- extract_nested(event_details, c("opponent", "id"), NA_character_)
        opponent_name <- extract_nested(event_details, c("opponent", "displayName"), NA_character_)
        opponent_abbreviation <- extract_nested(event_details, c("opponent", "abbreviation"), NA_character_)
        home_away <- extract_nested(event_details, c("atVs"), NA_character_)
        game_result <- extract_nested(event_details, c("gameResult"), NA_character_)
        score <- extract_nested(event_details, c("score"), NA_character_)
        week <- extract_nested(event_details, c("week"), NA_character_)
      } else {
        game_date <- opponent_id <- opponent_name <- opponent_abbreviation <- NA_character_
        home_away <- game_result <- score <- week <- NA_character_
      }

      # Process each stat for this game
      for (stat_idx in seq_along(event_stats)) {
        if (stat_idx <= length(labels) && stat_idx <= length(names_list) && stat_idx <= length(display_names)) {
          game_row <- data.frame(
            athlete_id = athlete_id,
            event_id = event_id,
            game_date = game_date,
            opponent_id = opponent_id,
            opponent_name = opponent_name,
            opponent_abbreviation = opponent_abbreviation,
            home_away = home_away,
            game_result = game_result,
            score = score,
            week = ifelse(is.na(week), NA_character_, as.character(week)),
            stat_category = category_name,
            stat_label = ifelse(length(labels) >= stat_idx, labels[[stat_idx]], ""),
            stat_name = ifelse(length(names_list) >= stat_idx, names_list[[stat_idx]], ""),
            stat_display_name = ifelse(length(display_names) >= stat_idx, display_names[[stat_idx]], ""),
            stat_value = event_stats[[stat_idx]],
            stringsAsFactors = FALSE
          )

          result_df <- rbind(result_df, game_row)
        }
      }
    }
  }

  # Clean up row names
  if (nrow(result_df) > 0) {
    rownames(result_df) <- NULL
  }

  return(result_df)
}

#' Fetch NFL athlete overview data using Web API
#'
#' @param athlete_id Character or Numeric. ESPN athlete ID (required).
#' @param return_type Character. Type of data to return: "stats", "news", "gamelog", "rotowire", "fantasy", or "all" (default: "all").
#' @param raw Logical. If TRUE, assigns raw JSON to global environment as 'nfl_athlete_overview_raw'. Default FALSE.
#' @return Invisibly returns the data, but main purpose is global environment assignment.
#' @export
#'
#' @examples
#' # Get athlete statistics
#' fetch_nfl_athlete_overview(athlete_id = "3139477", return_type = "stats")
#' head(nfl_athlete_stats)
#'
#' # Get athlete news
#' fetch_nfl_athlete_overview(athlete_id = "3139477", return_type = "news")
#' head(nfl_athlete_news)
#'
#' # Get athlete game log
#' fetch_nfl_athlete_overview(athlete_id = "3139477", return_type = "gamelog")
#' head(nfl_athlete_gamelog)
#'
#' # Get rotowire information
#' fetch_nfl_athlete_overview(athlete_id = "3139477", return_type = "rotowire")
#' head(nfl_athlete_rotowire)
#'
#' # Get fantasy information
#' fetch_nfl_athlete_overview(athlete_id = "3139477", return_type = "fantasy")
#' head(nfl_athlete_fantasy)
#'
#' # Get all data types
#' fetch_nfl_athlete_overview(athlete_id = "3139477", return_type = "all")
#'
#' # Get raw data for custom processing
#' fetch_nfl_athlete_overview(athlete_id = "3139477", raw = TRUE)
#' str(nfl_athlete_overview_raw, max.level = 2)
#'
fetch_nfl_athlete_overview <- function(athlete_id, return_type = "all", raw = FALSE) {

  # Validate inputs
  if (missing(athlete_id)) {
    stop("'athlete_id' is a required parameter")
  }

  if (!return_type %in% c("stats", "news", "gamelog", "rotowire", "fantasy", "all")) {
    stop("'return_type' must be one of: 'stats', 'news', 'gamelog', 'rotowire', 'fantasy', or 'all'")
  }

  # Convert athlete_id to character for URL building
  athlete_id <- as.character(athlete_id)

  # Build URL
  url <- sprintf("https://site.web.api.espn.com/apis/common/v3/sports/football/nfl/athletes/%s/overview",
                 athlete_id)

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
      assign("nfl_athlete_overview_raw", data, envir = .GlobalEnv)
      message("Raw NFL athlete overview data assigned to: nfl_athlete_overview_raw")
      return(invisible(data))
    }

    # Create datasets based on return_type
    result_data <- list()

    if (return_type %in% c("stats", "all")) {
      stats_df <- create_athlete_stats_dataset(data, athlete_id)
      assign("nfl_athlete_stats", stats_df, envir = .GlobalEnv)
      result_data$stats <- stats_df

      message(sprintf("NFL athlete stats data assigned to: nfl_athlete_stats (%d stat records)",
                      nrow(stats_df)))
    }

    if (return_type %in% c("news", "all")) {
      news_df <- create_athlete_news_dataset(data, athlete_id)
      assign("nfl_athlete_news", news_df, envir = .GlobalEnv)
      result_data$news <- news_df

      message(sprintf("NFL athlete news data assigned to: nfl_athlete_news (%d news items)",
                      nrow(news_df)))
    }

    if (return_type %in% c("gamelog", "all")) {
      gamelog_df <- create_athlete_gamelog_dataset(data, athlete_id)
      assign("nfl_athlete_gamelog", gamelog_df, envir = .GlobalEnv)
      result_data$gamelog <- gamelog_df

      message(sprintf("NFL athlete game log data assigned to: nfl_athlete_gamelog (%d game records)",
                      nrow(gamelog_df)))
    }

    if (return_type %in% c("rotowire", "all")) {
      rotowire_df <- create_athlete_rotowire_dataset(data, athlete_id)
      assign("nfl_athlete_rotowire", rotowire_df, envir = .GlobalEnv)
      result_data$rotowire <- rotowire_df

      message(sprintf("NFL athlete rotowire data assigned to: nfl_athlete_rotowire (%d records)",
                      nrow(rotowire_df)))
    }

    if (return_type %in% c("fantasy", "all")) {
      fantasy_df <- create_athlete_fantasy_dataset(data, athlete_id)
      assign("nfl_athlete_fantasy", fantasy_df, envir = .GlobalEnv)
      result_data$fantasy <- fantasy_df

      message(sprintf("NFL athlete fantasy data assigned to: nfl_athlete_fantasy (%d records)",
                      nrow(fantasy_df)))
    }

    # Return appropriate data structure
    if (return_type == "all") {
      return(invisible(result_data))
    } else if (return_type == "stats") {
      return(invisible(get("nfl_athlete_stats", envir = .GlobalEnv)))
    } else if (return_type == "news") {
      return(invisible(get("nfl_athlete_news", envir = .GlobalEnv)))
    } else if (return_type == "gamelog") {
      return(invisible(get("nfl_athlete_gamelog", envir = .GlobalEnv)))
    } else if (return_type == "rotowire") {
      return(invisible(get("nfl_athlete_rotowire", envir = .GlobalEnv)))
    } else if (return_type == "fantasy") {
      return(invisible(get("nfl_athlete_fantasy", envir = .GlobalEnv)))
    }

  }, error = function(e) {
    stop(sprintf("Failed to fetch NFL athlete overview for athlete %s: %s", athlete_id, e$message))
  })
}

#' Fetch multiple NFL athletes' overview data
#'
#' @param athlete_ids Character or Numeric vector. ESPN athlete IDs.
#' @param return_type Character. Type of data to return: "stats", "news", "gamelog", "rotowire", "fantasy", or "all" (default: "all").
#' @param delay Numeric. Delay in seconds between API requests (default: 0.1).
#' @param raw Logical. If TRUE, assigns raw JSON to global environment. Default FALSE.
#' @return Invisibly returns the combined data, but main purpose is global environment assignment.
#' @export
#'
#' @examples
#' # Get stats data for multiple players
#' mahomes_id <- "3139477"
#' allen_id <- "3918298"
#' fetch_multiple_nfl_athlete_overviews(c(mahomes_id, allen_id), return_type = "stats")
#'
#' # Get all data types for multiple players
#' fetch_multiple_nfl_athlete_overviews(c(mahomes_id, allen_id), return_type = "all")
#'
fetch_multiple_nfl_athlete_overviews <- function(athlete_ids, return_type = "all",
                                                 delay = 0.1, raw = FALSE) {

  if (length(athlete_ids) == 0) {
    stop("'athlete_ids' must contain at least one athlete ID")
  }

  all_stats_data <- data.frame()
  all_news_data <- data.frame()
  all_gamelog_data <- data.frame()
  all_rotowire_data <- data.frame()
  all_fantasy_data <- data.frame()

  message(sprintf("Starting to fetch overview data for %d athletes...", length(athlete_ids)))

  for (i in seq_along(athlete_ids)) {
    athlete_id <- athlete_ids[i]

    message(sprintf("Fetching data for athlete %s (%d/%d)...", athlete_id, i, length(athlete_ids)))

    tryCatch({
      # Fetch individual athlete data
      athlete_data <- fetch_nfl_athlete_overview(
        athlete_id = athlete_id,
        return_type = return_type,
        raw = raw
      )

      # If raw requested, return after first athlete
      if (isTRUE(raw)) {
        return(invisible(athlete_data))
      }

      # Combine data based on return type
      if (return_type %in% c("stats", "all")) {
        stats_df <- get("nfl_athlete_stats", envir = .GlobalEnv)
        all_stats_data <- rbind(all_stats_data, stats_df)
      }

      if (return_type %in% c("news", "all")) {
        news_df <- get("nfl_athlete_news", envir = .GlobalEnv)
        all_news_data <- rbind(all_news_data, news_df)
      }

      if (return_type %in% c("gamelog", "all")) {
        gamelog_df <- get("nfl_athlete_gamelog", envir = .GlobalEnv)
        all_gamelog_data <- rbind(all_gamelog_data, gamelog_df)
      }

      if (return_type %in% c("rotowire", "all")) {
        rotowire_df <- get("nfl_athlete_rotowire", envir = .GlobalEnv)
        all_rotowire_data <- rbind(all_rotowire_data, rotowire_df)
      }

      if (return_type %in% c("fantasy", "all")) {
        fantasy_df <- get("nfl_athlete_fantasy", envir = .GlobalEnv)
        all_fantasy_data <- rbind(all_fantasy_data, fantasy_df)
      }

    }, error = function(e) {
      message(sprintf("Failed to fetch data for athlete %s: %s", athlete_id, e$message))
    })

    # Be respectful to the API
    if (i < length(athlete_ids)) {
      Sys.sleep(delay)
    }
  }

  # Remove duplicates and assign combined datasets to global environment
  result_data <- list()

  if (return_type %in% c("stats", "all") && nrow(all_stats_data) > 0) {
    all_stats_data <- all_stats_data[!duplicated(paste(all_stats_data$athlete_id,
                                                       all_stats_data$stat_split,
                                                       all_stats_data$stat_name)), ]
    assign("nfl_athlete_stats", all_stats_data, envir = .GlobalEnv)
    result_data$stats <- all_stats_data

    message(sprintf("Completed! NFL athlete stats data assigned to: nfl_athlete_stats (%d stat records)",
                    nrow(all_stats_data)))
  }

  if (return_type %in% c("news", "all") && nrow(all_news_data) > 0) {
    all_news_data <- all_news_data[!duplicated(paste(all_news_data$athlete_id,
                                                     all_news_data$news_id)), ]
    assign("nfl_athlete_news", all_news_data, envir = .GlobalEnv)
    result_data$news <- all_news_data

    message(sprintf("Completed! NFL athlete news data assigned to: nfl_athlete_news (%d news items)",
                    nrow(all_news_data)))
  }

  if (return_type %in% c("gamelog", "all") && nrow(all_gamelog_data) > 0) {
    all_gamelog_data <- all_gamelog_data[!duplicated(paste(all_gamelog_data$athlete_id,
                                                           all_gamelog_data$event_id,
                                                           all_gamelog_data$stat_name)), ]
    assign("nfl_athlete_gamelog", all_gamelog_data, envir = .GlobalEnv)
    result_data$gamelog <- all_gamelog_data

    message(sprintf("Completed! NFL athlete game log data assigned to: nfl_athlete_gamelog (%d game records)",
                    nrow(all_gamelog_data)))
  }

  if (return_type %in% c("rotowire", "all") && nrow(all_rotowire_data) > 0) {
    all_rotowire_data <- all_rotowire_data[!duplicated(all_rotowire_data$athlete_id), ]
    assign("nfl_athlete_rotowire", all_rotowire_data, envir = .GlobalEnv)
    result_data$rotowire <- all_rotowire_data

    message(sprintf("Completed! NFL athlete rotowire data assigned to: nfl_athlete_rotowire (%d records)",
                    nrow(all_rotowire_data)))
  }

  if (return_type %in% c("fantasy", "all") && nrow(all_fantasy_data) > 0) {
    all_fantasy_data <- all_fantasy_data[!duplicated(all_fantasy_data$athlete_id), ]
    assign("nfl_athlete_fantasy", all_fantasy_data, envir = .GlobalEnv)
    result_data$fantasy <- all_fantasy_data

    message(sprintf("Completed! NFL athlete fantasy data assigned to: nfl_athlete_fantasy (%d records)",
                    nrow(all_fantasy_data)))
  }

  # Return appropriate data structure
  if (return_type == "all") {
    return(invisible(result_data))
  } else if (return_type == "stats") {
    return(invisible(result_data$stats))
  } else if (return_type == "news") {
    return(invisible(result_data$news))
  } else if (return_type == "gamelog") {
    return(invisible(result_data$gamelog))
  } else if (return_type == "rotowire") {
    return(invisible(result_data$rotowire))
  } else if (return_type == "fantasy") {
    return(invisible(result_data$fantasy))
  }
}
