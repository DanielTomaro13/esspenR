#' Fetch NFL teams data using Site API
#'
#' @param raw Logical. If TRUE, assigns raw JSON to global environment as 'nfl_teams_raw'. Default FALSE.
#' @return Invisibly returns the data, but main purpose is global environment assignment.
#' @export
#'
#' @examples
#' # Get all NFL teams
#' fetch_nfl_teams()
#' head(nfl_teams)
#'
#' # Get raw data for custom processing
#' fetch_nfl_teams(raw = TRUE)
#' str(nfl_teams_raw, max.level = 2)
#'
fetch_nfl_teams <- function(raw = FALSE) {
  # Build URL
  url <- "https://site.api.espn.com/apis/site/v2/sports/football/nfl/teams"

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
      assign("nfl_teams_raw", data, envir = .GlobalEnv)
      message("Raw NFL teams data assigned to: nfl_teams_raw")
      return(invisible(data))
    }

    # Create clean teams dataset
    teams_df <- create_clean_teams_dataset(data)

    # Assign to global environment
    assign("nfl_teams", teams_df, envir = .GlobalEnv)

    message(sprintf("NFL teams data assigned to: nfl_teams (%d teams)", nrow(teams_df)))

    return(invisible(teams_df))

  }, error = function(e) {
    stop(sprintf("Failed to fetch NFL teams data: %s", e$message))
  })
}

#' Create clean teams dataset from ESPN Site API response
#'
#' @param data Raw JSON response from ESPN Site API teams endpoint
#' @return Clean data frame with one row per team
create_clean_teams_dataset <- function(data) {
  # Initialize result data frame
  result_df <- data.frame(
    team_id = character(0),
    team_uid = character(0),
    team_slug = character(0),
    team_abbreviation = character(0),
    team_display_name = character(0),
    team_short_display_name = character(0),
    team_name = character(0),
    team_nickname = character(0),
    team_location = character(0),
    team_color = character(0),
    team_alternate_color = character(0),
    is_active = logical(0),
    is_all_star = logical(0),
    # Logos
    team_logo = character(0),
    team_logo_dark = character(0),
    team_logo_scoreboard = character(0),
    team_logo_scoreboard_dark = character(0),
    # Links
    team_clubhouse_link = character(0),
    team_roster_link = character(0),
    team_stats_link = character(0),
    team_schedule_link = character(0),
    team_tickets_link = character(0),
    team_depthchart_link = character(0),
    stringsAsFactors = FALSE
  )

  # Navigate to sports data - handle the actual structure
  sports_data <- NULL
  if ("sports" %in% names(data) && length(data[["sports"]]) > 0) {
    sport <- data[["sports"]][[1]]  # Should be NFL
    if ("leagues" %in% names(sport) && length(sport[["leagues"]]) > 0) {
      league <- sport[["leagues"]][[1]]  # Should be NFL league
      if ("teams" %in% names(league)) {
        sports_data <- league[["teams"]]
      }
    }
  }

  if (is.null(sports_data) || length(sports_data) == 0) {
    return(result_df)
  }

  # Process each team
  for (i in seq_along(sports_data)) {
    team_data <- sports_data[[i]]
    if (!"team" %in% names(team_data)) next

    team <- team_data[["team"]]

    # Basic team info
    team_id <- if ("id" %in% names(team)) team[["id"]] else NA_character_
    team_uid <- if ("uid" %in% names(team)) team[["uid"]] else NA_character_
    team_slug <- if ("slug" %in% names(team)) team[["slug"]] else NA_character_
    team_abbreviation <- if ("abbreviation" %in% names(team)) team[["abbreviation"]] else NA_character_
    team_display_name <- if ("displayName" %in% names(team)) team[["displayName"]] else NA_character_
    team_short_display_name <- if ("shortDisplayName" %in% names(team)) team[["shortDisplayName"]] else NA_character_
    team_name <- if ("name" %in% names(team)) team[["name"]] else NA_character_
    team_nickname <- if ("nickname" %in% names(team)) team[["nickname"]] else NA_character_
    team_location <- if ("location" %in% names(team)) team[["location"]] else NA_character_
    team_color <- if ("color" %in% names(team)) team[["color"]] else NA_character_
    team_alternate_color <- if ("alternateColor" %in% names(team)) team[["alternateColor"]] else NA_character_
    is_active <- if ("isActive" %in% names(team)) team[["isActive"]] else TRUE
    is_all_star <- if ("isAllStar" %in% names(team)) team[["isAllStar"]] else FALSE

    # Logos - extract based on rel types
    team_logo <- team_logo_dark <- team_logo_scoreboard <- team_logo_scoreboard_dark <- NA_character_

    if ("logos" %in% names(team) && length(team[["logos"]]) > 0) {
      for (logo in team[["logos"]]) {
        if ("href" %in% names(logo) && "rel" %in% names(logo) && length(logo[["rel"]]) > 0) {
          rel_types <- logo[["rel"]]

          # Check for specific logo types based on rel values
          if (all(c("full", "default") %in% rel_types)) {
            team_logo <- logo[["href"]]
          } else if (all(c("full", "dark") %in% rel_types) && !"scoreboard" %in% rel_types) {
            team_logo_dark <- logo[["href"]]
          } else if (all(c("full", "scoreboard") %in% rel_types) && !"dark" %in% rel_types) {
            team_logo_scoreboard <- logo[["href"]]
          } else if (all(c("full", "scoreboard", "dark") %in% rel_types)) {
            team_logo_scoreboard_dark <- logo[["href"]]
          }
        }
      }
    }

    # Links - extract based on rel types
    team_clubhouse_link <- team_roster_link <- team_stats_link <- team_schedule_link <- NA_character_
    team_tickets_link <- team_depthchart_link <- NA_character_

    if ("links" %in% names(team) && length(team[["links"]]) > 0) {
      for (link in team[["links"]]) {
        if ("rel" %in% names(link) && length(link[["rel"]]) > 0 && "href" %in% names(link)) {
          rel_types <- link[["rel"]]

          # Check for specific link types
          if ("clubhouse" %in% rel_types) {
            team_clubhouse_link <- link[["href"]]
          } else if ("roster" %in% rel_types) {
            team_roster_link <- link[["href"]]
          } else if ("stats" %in% rel_types) {
            team_stats_link <- link[["href"]]
          } else if ("schedule" %in% rel_types) {
            team_schedule_link <- link[["href"]]
          } else if ("tickets" %in% rel_types) {
            team_tickets_link <- link[["href"]]
          } else if ("depthchart" %in% rel_types) {
            team_depthchart_link <- link[["href"]]
          }
        }
      }
    }

    # Create team row
    team_row <- data.frame(
      team_id = team_id,
      team_uid = team_uid,
      team_slug = team_slug,
      team_abbreviation = team_abbreviation,
      team_display_name = team_display_name,
      team_short_display_name = team_short_display_name,
      team_name = team_name,
      team_nickname = team_nickname,
      team_location = team_location,
      team_color = team_color,
      team_alternate_color = team_alternate_color,
      is_active = is_active,
      is_all_star = is_all_star,
      team_logo = team_logo,
      team_logo_dark = team_logo_dark,
      team_logo_scoreboard = team_logo_scoreboard,
      team_logo_scoreboard_dark = team_logo_scoreboard_dark,
      team_clubhouse_link = team_clubhouse_link,
      team_roster_link = team_roster_link,
      team_stats_link = team_stats_link,
      team_schedule_link = team_schedule_link,
      team_tickets_link = team_tickets_link,
      team_depthchart_link = team_depthchart_link,
      stringsAsFactors = FALSE
    )

    # Add to result
    result_df <- rbind(result_df, team_row)
  }

  # Clean up row names and sort by team name
  if (nrow(result_df) > 0) {
    rownames(result_df) <- NULL
    # Sort by team display name
    result_df <- result_df[order(result_df$team_display_name), ]
    rownames(result_df) <- NULL
  }

  return(result_df)
}
