#' Fetch actual NFL standings data
#'
#' @param season Numeric. Season year (e.g., 2023, 2024). Default current year.
#' @param raw Logical. If TRUE, assigns raw JSON to global environment. Default FALSE.
#' @return Invisibly returns the data, but main purpose is global environment assignment.
#' @export
#'
#' @examples
#' # Get current regular season standings
#' fetch_nfl_standings(season = 2024)
#' head(nfl_standings)
#'
#' # Get raw data for custom processing
#' fetch_nfl_standings(season = 2024, raw = TRUE)
#' str(nfl_standings_raw, max.level = 3)
#'
fetch_nfl_standings <- function(season = as.numeric(format(Sys.Date(), "%Y")), raw = FALSE) {
  # Validate inputs
  if (!is.numeric(season) || season < 1990 || season > as.numeric(format(Sys.Date(), "%Y")) + 2) {
    stop("season must be a valid numeric year between 1990 and two years in the future")
  }

  # Build URL - CDN endpoint with xhr=1
  base_url <- "https://cdn.espn.com/core/nfl/standings"
  query_params <- list(
    season = season,
    xhr = 1
  )

  # Note: CDN endpoint doesn't use seasontype parameter the same way

  url <- httr::modify_url(base_url, query = query_params)

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
      assign("nfl_standings_raw", data, envir = .GlobalEnv)
      message("Raw NFL standings data assigned to: nfl_standings_raw")
      return(invisible(data))
    }

    # Create clean dataset
    clean_dataset <- create_standings_dataset(data, season)
    assign("nfl_standings", clean_dataset, envir = .GlobalEnv)
    message(sprintf("NFL standings (%s) assigned to: nfl_standings", season))

    return(invisible(clean_dataset))

  }, error = function(e) {
    stop(sprintf("Failed to fetch NFL standings data: %s", e$message))
  })
}

#' Create clean standings dataset from ESPN CDN
#'
#' @param data Raw JSON response from ESPN CDN standings
#' @param season Season parameter used in request
#' @return Clean data frame with standings information
create_standings_dataset <- function(data, season) {
  result_df <- data.frame(
    season = numeric(0),
    conference = character(0),
    division = character(0),
    team_id = character(0),
    team_uid = character(0),
    team_abbreviation = character(0),
    team_short_name = character(0),
    team_display_name = character(0),
    team_location = character(0),
    team_logo = character(0),
    team_link = character(0),
    seed = character(0),
    clincher = character(0),
    wins = numeric(0),
    losses = numeric(0),
    ties = numeric(0),
    win_percentage = numeric(0),
    home_record = character(0),
    away_record = character(0),
    division_record = character(0),
    conference_record = character(0),
    points_for = numeric(0),
    points_against = numeric(0),
    point_differential = numeric(0),
    streak = character(0),
    stringsAsFactors = FALSE
  )

  # Navigate to standings structure
  if ("content" %in% names(data) && "standings" %in% names(data[["content"]]) &&
      "groups" %in% names(data[["content"]][["standings"]])) {
    conferences <- data[["content"]][["standings"]][["groups"]]

    for (conf in conferences) {
      conf_name <- if ("name" %in% names(conf)) conf[["name"]] else NA_character_

      if ("groups" %in% names(conf)) {
        for (div in conf[["groups"]]) {
          div_name <- if ("name" %in% names(div)) div[["name"]] else NA_character_

          if ("standings" %in% names(div) && "entries" %in% names(div[["standings"]])) {
            for (entry in div[["standings"]][["entries"]]) {

              # Extract team info
              team_info <- if ("team" %in% names(entry)) entry[["team"]] else list()

              team_id <- if ("id" %in% names(team_info)) as.character(team_info[["id"]]) else NA_character_
              team_uid <- if ("uid" %in% names(team_info)) team_info[["uid"]] else NA_character_
              team_abbreviation <- if ("abbreviation" %in% names(team_info)) team_info[["abbreviation"]] else NA_character_
              team_short_name <- if ("shortDisplayName" %in% names(team_info)) team_info[["shortDisplayName"]] else NA_character_
              team_display_name <- if ("displayName" %in% names(team_info)) team_info[["displayName"]] else NA_character_
              team_location <- if ("location" %in% names(team_info)) team_info[["location"]] else NA_character_
              team_link <- if ("link" %in% names(team_info)) team_info[["link"]] else NA_character_
              seed <- if ("seed" %in% names(team_info)) team_info[["seed"]] else NA_character_
              clincher <- if ("clincher" %in% names(team_info)) team_info[["clincher"]] else NA_character_

              # Extract logo
              team_logo <- NA_character_
              if ("logos" %in% names(team_info) && length(team_info[["logos"]]) > 0) {
                if ("href" %in% names(team_info[["logos"]][[1]])) {
                  team_logo <- team_info[["logos"]][[1]][["href"]]
                }
              }

              # Extract stats
              stats_info <- if ("stats" %in% names(entry)) entry[["stats"]] else list()

              # Initialize stat variables
              wins <- NA_real_
              losses <- NA_real_
              ties <- NA_real_
              win_percentage <- NA_real_
              home_record <- NA_character_
              away_record <- NA_character_
              division_record <- NA_character_
              conference_record <- NA_character_
              points_for <- NA_real_
              points_against <- NA_real_
              point_differential <- NA_real_
              streak <- NA_character_

              # Parse stats
              for (stat in stats_info) {
                if ("name" %in% names(stat)) {
                  stat_name <- stat[["name"]]
                  stat_value <- if ("value" %in% names(stat)) stat[["value"]] else NA
                  stat_display <- if ("displayValue" %in% names(stat)) stat[["displayValue"]] else NA_character_

                  if (stat_name == "wins") wins <- as.numeric(stat_value)
                  else if (stat_name == "losses") losses <- as.numeric(stat_value)
                  else if (stat_name == "ties") ties <- as.numeric(stat_value)
                  else if (stat_name == "winPercent") win_percentage <- as.numeric(stat_value)
                  else if (stat_name == "Home") home_record <- stat_display
                  else if (stat_name == "Road") away_record <- stat_display
                  else if (stat_name == "vs. Div.") division_record <- stat_display
                  else if (stat_name == "vs. Conf.") conference_record <- stat_display
                  else if (stat_name == "pointsFor") points_for <- as.numeric(stat_value)
                  else if (stat_name == "pointsAgainst") points_against <- as.numeric(stat_value)
                  else if (stat_name == "differential") point_differential <- as.numeric(stat_value)
                  else if (stat_name == "streak") streak <- stat_display
                }
              }

              # Add row to result
              result_df <- rbind(result_df, data.frame(
                season = season,
                conference = conf_name,
                division = div_name,
                team_id = team_id,
                team_uid = team_uid,
                team_abbreviation = team_abbreviation,
                team_short_name = team_short_name,
                team_display_name = team_display_name,
                team_location = team_location,
                team_logo = team_logo,
                team_link = team_link,
                seed = seed,
                clincher = clincher,
                wins = wins,
                losses = losses,
                ties = ties,
                win_percentage = win_percentage,
                home_record = home_record,
                away_record = away_record,
                division_record = division_record,
                conference_record = conference_record,
                points_for = points_for,
                points_against = points_against,
                point_differential = point_differential,
                streak = streak,
                stringsAsFactors = FALSE
              ))
            }
          }
        }
      }
    }
  }

  # Clean up row names and sort by conference and win percentage
  if (nrow(result_df) > 0) {
    rownames(result_df) <- NULL
    result_df <- result_df[order(result_df$conference, -result_df$win_percentage, na.last = TRUE), ]
  }

  return(result_df)
}
