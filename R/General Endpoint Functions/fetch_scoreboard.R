#' Fetch ESPN scoreboard data (header endpoint)
#'
#' @description
#' Fetches the ESPN scoreboard header JSON for general (multi-sport) or specific sport & league.
#' Returns comprehensive game information and assigns clean datasets to global environment.
#'
#' @param sport Optional character. Sport slug (e.g. 'football'). If NULL, fetches general header.
#' @param league Optional character. League slug (e.g. 'nfl'). If NULL, fetches general header.
#' @param raw Logical. If TRUE, assigns raw JSON to global environment. Default FALSE (assigns clean datasets).
#' @return Invisibly returns the data, but main purpose is global environment assignment.
#' @export
#'
#' @examples
#' # Get general multi-sport scoreboard (assigns scoreboard_data to global env)
#' fetch_scoreboard()
#' head(scoreboard_games)
#'
#' # Get NFL scoreboard (assigns nfl_* datasets to global env)
#' fetch_scoreboard("football", "nfl")
#' head(nfl_games)
#'
#' # Get raw NBA data (assigns nba_scoreboard to global env)
#' fetch_scoreboard("basketball", "nba", raw = TRUE)
#' str(nba_scoreboard, max.level = 2)
#'
#' # Get raw general data (assigns all_scoreboard to global env)
#' fetch_scoreboard(raw = TRUE)
#' str(all_scoreboard, max.level = 2)
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
    data <- jsonlite::fromJSON(content_text, simplifyVector = FALSE, simplifyDataFrame = FALSE)

    # Handle raw data assignment
    if (isTRUE(raw)) {
      if (!is.null(sport) && !is.null(league)) {
        # Sport-specific raw data
        var_name <- paste0(tolower(league), "_scoreboard")
        assign(var_name, data, envir = .GlobalEnv)
        message(sprintf("Raw data assigned to: %s", var_name))
      } else {
        # General raw data
        assign("all_scoreboard", data, envir = .GlobalEnv)
        message("Raw data assigned to: all_scoreboard")
      }
      return(invisible(data))
    }

    # Clean and structure the data inline
    result <- list()

    # Check if we have sports data
    if ("sports" %in% names(data) && is.list(data[["sports"]]) && length(data[["sports"]]) > 0) {
      result[["sports"]] <- list()

      for (i in seq_along(data[["sports"]])) {
        sport_data <- data[["sports"]][[i]]

        # Ensure sport_data is a list
        if (!is.list(sport_data)) {
          next
        }

        sport_info <- list(
          id = if ("id" %in% names(sport_data) && !is.null(sport_data[["id"]])) sport_data[["id"]] else NA_character_,
          name = if ("name" %in% names(sport_data) && !is.null(sport_data[["name"]])) sport_data[["name"]] else NA_character_,
          slug = if ("slug" %in% names(sport_data) && !is.null(sport_data[["slug"]])) sport_data[["slug"]] else NA_character_
        )

        # Process leagues within sport
        if ("leagues" %in% names(sport_data) && is.list(sport_data[["leagues"]]) && length(sport_data[["leagues"]]) > 0) {
          sport_info[["leagues"]] <- list()

          for (j in seq_along(sport_data[["leagues"]])) {
            league_data <- sport_data[["leagues"]][[j]]

            # Ensure league_data is a list
            if (!is.list(league_data)) {
              next
            }

            league_info <- list(
              id = if ("id" %in% names(league_data) && !is.null(league_data[["id"]])) league_data[["id"]] else NA_character_,
              name = if ("name" %in% names(league_data) && !is.null(league_data[["name"]])) league_data[["name"]] else NA_character_,
              abbreviation = if ("abbreviation" %in% names(league_data) && !is.null(league_data[["abbreviation"]])) league_data[["abbreviation"]] else NA_character_,
              slug = if ("slug" %in% names(league_data) && !is.null(league_data[["slug"]])) league_data[["slug"]] else NA_character_,
              is_tournament = if ("isTournament" %in% names(league_data) && !is.null(league_data[["isTournament"]])) league_data[["isTournament"]] else FALSE
            )

            # Process smartdates if available
            if ("smartdates" %in% names(league_data) && is.list(league_data[["smartdates"]]) && length(league_data[["smartdates"]]) > 0) {
              current_week <- league_data[["smartdates"]][[1]]
              if (is.list(current_week)) {
                league_info[["current_week"]] <- list(
                  label = if ("label" %in% names(current_week) && !is.null(current_week[["label"]])) current_week[["label"]] else NA_character_,
                  season = if ("season" %in% names(current_week) && !is.null(current_week[["season"]])) current_week[["season"]] else NA_integer_,
                  season_type = if ("seasontype" %in% names(current_week) && !is.null(current_week[["seasontype"]])) current_week[["seasontype"]] else NA_integer_,
                  week = if ("week" %in% names(current_week) && !is.null(current_week[["week"]])) current_week[["week"]] else NA_integer_
                )
              }
            }

            # Process events (games)
            if ("events" %in% names(league_data) && is.list(league_data[["events"]]) && length(league_data[["events"]]) > 0) {
              league_info[["games"]] <- list()

              for (k in seq_along(league_data[["events"]])) {
                event <- league_data[["events"]][[k]]

                # Ensure event is a list
                if (!is.list(event)) {
                  next
                }

                # Extract basic game info
                game <- list(
                  id = if ("id" %in% names(event) && !is.null(event[["id"]])) event[["id"]] else NA_character_,
                  uid = if ("uid" %in% names(event) && !is.null(event[["uid"]])) event[["uid"]] else NA_character_,
                  name = if ("name" %in% names(event) && !is.null(event[["name"]])) event[["name"]] else NA_character_,
                  short_name = if ("shortName" %in% names(event) && !is.null(event[["shortName"]])) event[["shortName"]] else NA_character_,
                  date = if ("date" %in% names(event) && !is.null(event[["date"]])) event[["date"]] else NA_character_,
                  status = if ("status" %in% names(event) && !is.null(event[["status"]])) event[["status"]] else NA_character_,
                  summary = if ("summary" %in% names(event) && !is.null(event[["summary"]])) event[["summary"]] else NA_character_,
                  week = if ("week" %in% names(event) && !is.null(event[["week"]])) as.numeric(event[["week"]]) else NA_integer_,
                  week_text = if ("weekText" %in% names(event) && !is.null(event[["weekText"]])) event[["weekText"]] else NA_character_,
                  season = if ("season" %in% names(event) && !is.null(event[["season"]])) as.numeric(event[["season"]]) else NA_integer_,
                  season_type = if ("seasonType" %in% names(event) && !is.null(event[["seasonType"]])) event[["seasonType"]] else NA_character_,
                  period = if ("period" %in% names(event) && !is.null(event[["period"]])) as.numeric(event[["period"]]) else 0,
                  clock = if ("clock" %in% names(event) && !is.null(event[["clock"]])) event[["clock"]] else "0:00",
                  location = if ("location" %in% names(event) && !is.null(event[["location"]])) event[["location"]] else NA_character_,
                  neutral_site = if ("neutralSite" %in% names(event) && !is.null(event[["neutralSite"]])) event[["neutralSite"]] else FALSE
                )

                # Extract special notes
                if ("notes" %in% names(event) && is.list(event[["notes"]]) && length(event[["notes"]]) > 0) {
                  game[["notes"]] <- character(length(event[["notes"]]))
                  for (n in seq_along(event[["notes"]])) {
                    note_item <- event[["notes"]][[n]]
                    if (is.list(note_item) && "text" %in% names(note_item)) {
                      game[["notes"]][n] <- if (!is.null(note_item[["text"]])) note_item[["text"]] else ""
                    }
                  }
                }

                # Extract team information
                if ("competitors" %in% names(event) && is.list(event[["competitors"]]) && length(event[["competitors"]]) > 0) {
                  all_teams <- list()
                  home_team <- NULL
                  away_team <- NULL

                  for (t in seq_along(event[["competitors"]])) {
                    team <- event[["competitors"]][[t]]

                    # Ensure team is a list
                    if (!is.list(team)) {
                      next
                    }

                    team_info <- list(
                      id = if ("id" %in% names(team) && !is.null(team[["id"]])) team[["id"]] else NA_character_,
                      name = if ("name" %in% names(team) && !is.null(team[["name"]])) team[["name"]] else NA_character_,
                      display_name = if ("displayName" %in% names(team) && !is.null(team[["displayName"]])) team[["displayName"]] else NA_character_,
                      abbreviation = if ("abbreviation" %in% names(team) && !is.null(team[["abbreviation"]])) team[["abbreviation"]] else NA_character_,
                      location = if ("location" %in% names(team) && !is.null(team[["location"]])) team[["location"]] else NA_character_,
                      home_away = if ("homeAway" %in% names(team) && !is.null(team[["homeAway"]])) team[["homeAway"]] else NA_character_,
                      order = if ("order" %in% names(team) && !is.null(team[["order"]])) as.numeric(team[["order"]]) else NA_integer_,
                      winner = if ("winner" %in% names(team) && !is.null(team[["winner"]])) team[["winner"]] else FALSE,
                      score = if ("score" %in% names(team) && !is.null(team[["score"]])) team[["score"]] else "",
                      record = if ("record" %in% names(team) && !is.null(team[["record"]])) team[["record"]] else NA_character_,
                      color = if ("color" %in% names(team) && !is.null(team[["color"]])) team[["color"]] else NA_character_,
                      alternate_color = if ("alternateColor" %in% names(team) && !is.null(team[["alternateColor"]])) team[["alternateColor"]] else NA_character_,
                      logo = if ("logo" %in% names(team) && !is.null(team[["logo"]])) team[["logo"]] else NA_character_,
                      logo_dark = if ("logoDark" %in% names(team) && !is.null(team[["logoDark"]])) team[["logoDark"]] else NA_character_
                    )

                    # Record stats
                    if ("recordStats" %in% names(team) && is.list(team[["recordStats"]])) {
                      rs <- team[["recordStats"]]
                      team_info[["wins"]] <- if ("wins" %in% names(rs) && is.list(rs[["wins"]]) && "value" %in% names(rs[["wins"]])) {
                        as.numeric(rs[["wins"]][["value"]])
                      } else 0
                      team_info[["losses"]] <- if ("losses" %in% names(rs) && is.list(rs[["losses"]]) && "value" %in% names(rs[["losses"]])) {
                        as.numeric(rs[["losses"]][["value"]])
                      } else 0
                    } else {
                      team_info[["wins"]] <- 0
                      team_info[["losses"]] <- 0
                    }

                    all_teams[[t]] <- team_info

                    # Assign home/away
                    if ("homeAway" %in% names(team) && !is.null(team[["homeAway"]])) {
                      if (team[["homeAway"]] == "home") {
                        home_team <- team_info
                      } else if (team[["homeAway"]] == "away") {
                        away_team <- team_info
                      }
                    }
                  }

                  game[["teams"]] <- list(
                    home = home_team,
                    away = away_team,
                    all = all_teams
                  )
                }

                # Extract betting odds
                if ("odds" %in% names(event) && is.list(event[["odds"]])) {
                  odds <- event[["odds"]]

                  odds_info <- list(
                    details = if ("details" %in% names(odds) && !is.null(odds[["details"]])) odds[["details"]] else NA_character_,
                    over_under = if ("overUnder" %in% names(odds) && !is.null(odds[["overUnder"]])) as.numeric(odds[["overUnder"]]) else NA_real_,
                    spread = if ("spread" %in% names(odds) && !is.null(odds[["spread"]])) as.numeric(odds[["spread"]]) else NA_real_
                  )

                  # Provider info
                  if ("provider" %in% names(odds) && is.list(odds[["provider"]])) {
                    provider <- odds[["provider"]]
                    odds_info[["provider"]] <- list(
                      id = if ("id" %in% names(provider) && !is.null(provider[["id"]])) provider[["id"]] else NA_character_,
                      name = if ("name" %in% names(provider) && !is.null(provider[["name"]])) provider[["name"]] else NA_character_
                    )
                  }

                  # Point spread details
                  if ("pointSpread" %in% names(odds) && is.list(odds[["pointSpread"]])) {
                    ps <- odds[["pointSpread"]]
                    odds_info[["point_spread"]] <- list(
                      display_name = if ("displayName" %in% names(ps) && !is.null(ps[["displayName"]])) ps[["displayName"]] else NA_character_
                    )

                    if ("home" %in% names(ps) && is.list(ps[["home"]])) {
                      home_ps <- ps[["home"]]
                      home_line <- if ("close" %in% names(home_ps) && is.list(home_ps[["close"]]) && "line" %in% names(home_ps[["close"]])) {
                        home_ps[["close"]][["line"]]
                      } else if ("open" %in% names(home_ps) && is.list(home_ps[["open"]]) && "line" %in% names(home_ps[["open"]])) {
                        home_ps[["open"]][["line"]]
                      } else {
                        NA_character_
                      }
                      odds_info[["point_spread"]][["home_line"]] <- home_line
                    }

                    if ("away" %in% names(ps) && is.list(ps[["away"]])) {
                      away_ps <- ps[["away"]]
                      away_line <- if ("close" %in% names(away_ps) && is.list(away_ps[["close"]]) && "line" %in% names(away_ps[["close"]])) {
                        away_ps[["close"]][["line"]]
                      } else if ("open" %in% names(away_ps) && is.list(away_ps[["open"]]) && "line" %in% names(away_ps[["open"]])) {
                        away_ps[["open"]][["line"]]
                      } else {
                        NA_character_
                      }
                      odds_info[["point_spread"]][["away_line"]] <- away_line
                    }
                  }

                  # Moneyline
                  if ("moneyline" %in% names(odds) && is.list(odds[["moneyline"]])) {
                    ml <- odds[["moneyline"]]
                    odds_info[["moneyline"]] <- list()

                    if ("home" %in% names(ml) && is.list(ml[["home"]])) {
                      home_ml <- ml[["home"]]
                      home_odds <- if ("close" %in% names(home_ml) && is.list(home_ml[["close"]]) && "odds" %in% names(home_ml[["close"]])) {
                        home_ml[["close"]][["odds"]]
                      } else if ("open" %in% names(home_ml) && is.list(home_ml[["open"]]) && "odds" %in% names(home_ml[["open"]])) {
                        home_ml[["open"]][["odds"]]
                      } else {
                        NA_character_
                      }
                      odds_info[["moneyline"]][["home_odds"]] <- home_odds
                    }

                    if ("away" %in% names(ml) && is.list(ml[["away"]])) {
                      away_ml <- ml[["away"]]
                      away_odds <- if ("close" %in% names(away_ml) && is.list(away_ml[["close"]]) && "odds" %in% names(away_ml[["close"]])) {
                        away_ml[["close"]][["odds"]]
                      } else if ("open" %in% names(away_ml) && is.list(away_ml[["open"]]) && "odds" %in% names(away_ml[["open"]])) {
                        away_ml[["open"]][["odds"]]
                      } else {
                        NA_character_
                      }
                      odds_info[["moneyline"]][["away_odds"]] <- away_odds
                    }
                  }

                  # Total (over/under)
                  if ("total" %in% names(odds) && is.list(odds[["total"]])) {
                    total <- odds[["total"]]
                    odds_info[["total"]] <- list()

                    if ("over" %in% names(total) && is.list(total[["over"]])) {
                      over_total <- total[["over"]]
                      over_line <- if ("close" %in% names(over_total) && is.list(over_total[["close"]]) && "line" %in% names(over_total[["close"]])) {
                        over_total[["close"]][["line"]]
                      } else if ("open" %in% names(over_total) && is.list(over_total[["open"]]) && "line" %in% names(over_total[["open"]])) {
                        over_total[["open"]][["line"]]
                      } else {
                        NA_character_
                      }
                      odds_info[["total"]][["over_line"]] <- over_line
                    }

                    if ("under" %in% names(total) && is.list(total[["under"]])) {
                      under_total <- total[["under"]]
                      under_line <- if ("close" %in% names(under_total) && is.list(under_total[["close"]]) && "line" %in% names(under_total[["close"]])) {
                        under_total[["close"]][["line"]]
                      } else if ("open" %in% names(under_total) && is.list(under_total[["open"]]) && "line" %in% names(under_total[["open"]])) {
                        under_total[["open"]][["line"]]
                      } else {
                        NA_character_
                      }
                      odds_info[["total"]][["under_line"]] <- under_line
                    }
                  }

                  game[["odds"]] <- odds_info
                }

                # Extract broadcast information
                if ("broadcasts" %in% names(event) && is.list(event[["broadcasts"]]) && length(event[["broadcasts"]]) > 0) {
                  game[["broadcasts"]] <- list()

                  for (b in seq_along(event[["broadcasts"]])) {
                    broadcast <- event[["broadcasts"]][[b]]
                    if (is.list(broadcast)) {
                      game[["broadcasts"]][[b]] <- list(
                        type = if ("type" %in% names(broadcast) && !is.null(broadcast[["type"]])) broadcast[["type"]] else NA_character_,
                        name = if ("name" %in% names(broadcast) && !is.null(broadcast[["name"]])) broadcast[["name"]] else NA_character_,
                        short_name = if ("shortName" %in% names(broadcast) && !is.null(broadcast[["shortName"]])) broadcast[["shortName"]] else NA_character_,
                        call_letters = if ("callLetters" %in% names(broadcast) && !is.null(broadcast[["callLetters"]])) broadcast[["callLetters"]] else NA_character_,
                        is_national = if ("isNational" %in% names(broadcast) && !is.null(broadcast[["isNational"]])) broadcast[["isNational"]] else FALSE,
                        priority = if ("priority" %in% names(broadcast) && !is.null(broadcast[["priority"]])) as.numeric(broadcast[["priority"]]) else NA_integer_,
                        language = if ("lang" %in% names(broadcast) && !is.null(broadcast[["lang"]])) broadcast[["lang"]] else NA_character_,
                        region = if ("region" %in% names(broadcast) && !is.null(broadcast[["region"]])) broadcast[["region"]] else NA_character_
                      )
                    }
                  }
                } else if ("broadcast" %in% names(event) && !is.null(event[["broadcast"]])) {
                  game[["broadcast_summary"]] <- event[["broadcast"]]
                }

                # Extract detailed status
                if ("fullStatus" %in% names(event) && is.list(event[["fullStatus"]])) {
                  fs <- event[["fullStatus"]]
                  game[["detailed_status"]] <- list()

                  if ("type" %in% names(fs) && is.list(fs[["type"]])) {
                    type_info <- fs[["type"]]
                    game[["detailed_status"]][["type_id"]] <- if ("id" %in% names(type_info) && !is.null(type_info[["id"]])) type_info[["id"]] else NA_character_
                    game[["detailed_status"]][["type_name"]] <- if ("name" %in% names(type_info) && !is.null(type_info[["name"]])) type_info[["name"]] else NA_character_
                    game[["detailed_status"]][["state"]] <- if ("state" %in% names(type_info) && !is.null(type_info[["state"]])) type_info[["state"]] else NA_character_
                    game[["detailed_status"]][["completed"]] <- if ("completed" %in% names(type_info) && !is.null(type_info[["completed"]])) type_info[["completed"]] else FALSE
                    game[["detailed_status"]][["description"]] <- if ("description" %in% names(type_info) && !is.null(type_info[["description"]])) type_info[["description"]] else NA_character_
                    game[["detailed_status"]][["detail"]] <- if ("detail" %in% names(type_info) && !is.null(type_info[["detail"]])) type_info[["detail"]] else NA_character_
                    game[["detailed_status"]][["short_detail"]] <- if ("shortDetail" %in% names(type_info) && !is.null(type_info[["shortDetail"]])) type_info[["shortDetail"]] else NA_character_
                  }
                }

                league_info[["games"]][[k]] <- game
              }
            }

            sport_info[["leagues"]][[j]] <- league_info
          }
        }

        result[["sports"]][[i]] <- sport_info
      }
    }

    # Add metadata
    total_games <- 0
    if (length(result[["sports"]]) > 0) {
      for (s in seq_along(result[["sports"]])) {
        if ("leagues" %in% names(result[["sports"]][[s]]) && length(result[["sports"]][[s]][["leagues"]]) > 0) {
          for (l in seq_along(result[["sports"]][[s]][["leagues"]])) {
            if ("games" %in% names(result[["sports"]][[s]][["leagues"]][[l]])) {
              total_games <- total_games + length(result[["sports"]][[s]][["leagues"]][[l]][["games"]])
            }
          }
        }
      }
    }

    result[["metadata"]] <- list(
      fetched_at = Sys.time(),
      api_endpoint = if (is.null(sport)) "general" else paste(sport, league, sep = "/"),
      total_sports = length(result[["sports"]]),
      total_games = total_games
    )

    # Convert to clean datasets and assign to global environment
    clean_datasets <- create_clean_datasets(result)

    # Determine prefix for variable names
    if (!is.null(sport) && !is.null(league)) {
      prefix <- paste0(tolower(league), "_")
    } else {
      prefix <- "scoreboard_"
    }

    # Assign each dataset to global environment
    for (dataset_name in names(clean_datasets)) {
      if (dataset_name != "metadata") {
        var_name <- paste0(prefix, dataset_name)
        assign(var_name, clean_datasets[[dataset_name]], envir = .GlobalEnv)
      }
    }

    # Assign metadata separately
    metadata_var <- paste0(prefix, "metadata")
    assign(metadata_var, clean_datasets[["metadata"]], envir = .GlobalEnv)

    # Print what was assigned
    assigned_vars <- paste0(prefix, names(clean_datasets))
    message(sprintf("Clean datasets assigned to global environment: %s", paste(assigned_vars, collapse = ", ")))

    return(invisible(clean_datasets))

  }, error = function(e) {
    stop(sprintf("Failed to fetch scoreboard data: %s", e$message))
  })
}

#' Convert nested scoreboard data to clean datasets
#'
#' @param nested_data Output from fetch_scoreboard with nested lists
#' @return List of clean data.frames for analysis
create_clean_datasets <- function(nested_data) {

  # Initialize data collectors
  games_data <- list()
  teams_data <- list()
  odds_data <- list()
  broadcasts_data <- list()
  leagues_data <- list()
  sports_data <- list()

  # Process each sport
  if ("sports" %in% names(nested_data) && length(nested_data[["sports"]]) > 0) {

    for (s in seq_along(nested_data[["sports"]])) {
      sport <- nested_data[["sports"]][[s]]

      # Sports data
      sports_data[[length(sports_data) + 1]] <- data.frame(
        sport_id = if (is.null(sport[["id"]])) NA_character_ else sport[["id"]],
        sport_name = if (is.null(sport[["name"]])) NA_character_ else sport[["name"]],
        sport_slug = if (is.null(sport[["slug"]])) NA_character_ else sport[["slug"]],
        stringsAsFactors = FALSE
      )

      # Process leagues within sport
      if ("leagues" %in% names(sport) && length(sport[["leagues"]]) > 0) {

        for (l in seq_along(sport[["leagues"]])) {
          league <- sport[["leagues"]][[l]]

          # League data
          current_week_info <- if ("current_week" %in% names(league)) league[["current_week"]] else list()

          leagues_data[[length(leagues_data) + 1]] <- data.frame(
            sport_id = if (is.null(sport[["id"]])) NA_character_ else sport[["id"]],
            sport_name = if (is.null(sport[["name"]])) NA_character_ else sport[["name"]],
            league_id = if (is.null(league[["id"]])) NA_character_ else league[["id"]],
            league_name = if (is.null(league[["name"]])) NA_character_ else league[["name"]],
            league_abbreviation = if (is.null(league[["abbreviation"]])) NA_character_ else league[["abbreviation"]],
            league_slug = if (is.null(league[["slug"]])) NA_character_ else league[["slug"]],
            is_tournament = if (is.null(league[["is_tournament"]])) FALSE else league[["is_tournament"]],
            current_week_label = if (is.null(current_week_info[["label"]])) NA_character_ else current_week_info[["label"]],
            current_season = if (is.null(current_week_info[["season"]])) NA_integer_ else current_week_info[["season"]],
            current_season_type = if (is.null(current_week_info[["season_type"]])) NA_integer_ else current_week_info[["season_type"]],
            current_week = if (is.null(current_week_info[["week"]])) NA_integer_ else current_week_info[["week"]],
            stringsAsFactors = FALSE
          )

          # Process games within league
          if ("games" %in% names(league) && length(league[["games"]]) > 0) {

            for (g in seq_along(league[["games"]])) {
              game <- league[["games"]][[g]]

              # Basic game data
              notes_text <- if ("notes" %in% names(game) && length(game[["notes"]]) > 0) {
                paste(game[["notes"]], collapse = "; ")
              } else NA_character_

              # Detailed status info
              detailed_status <- if ("detailed_status" %in% names(game)) game[["detailed_status"]] else list()

              games_data[[length(games_data) + 1]] <- data.frame(
                sport_id = if (is.null(sport[["id"]])) NA_character_ else sport[["id"]],
                sport_name = if (is.null(sport[["name"]])) NA_character_ else sport[["name"]],
                league_id = if (is.null(league[["id"]])) NA_character_ else league[["id"]],
                league_name = if (is.null(league[["name"]])) NA_character_ else league[["name"]],
                game_id = if (is.null(game[["id"]])) NA_character_ else game[["id"]],
                game_uid = if (is.null(game[["uid"]])) NA_character_ else game[["uid"]],
                game_name = if (is.null(game[["name"]])) NA_character_ else game[["name"]],
                game_short_name = if (is.null(game[["short_name"]])) NA_character_ else game[["short_name"]],
                game_date = if (is.null(game[["date"]])) NA_character_ else game[["date"]],
                game_status = if (is.null(game[["status"]])) NA_character_ else game[["status"]],
                game_summary = if (is.null(game[["summary"]])) NA_character_ else game[["summary"]],
                week = if (is.null(game[["week"]])) NA_integer_ else game[["week"]],
                week_text = if (is.null(game[["week_text"]])) NA_character_ else game[["week_text"]],
                season = if (is.null(game[["season"]])) NA_integer_ else game[["season"]],
                season_type = if (is.null(game[["season_type"]])) NA_character_ else game[["season_type"]],
                period = if (is.null(game[["period"]])) 0 else game[["period"]],
                clock = if (is.null(game[["clock"]])) "0:00" else game[["clock"]],
                location = if (is.null(game[["location"]])) NA_character_ else game[["location"]],
                neutral_site = if (is.null(game[["neutral_site"]])) FALSE else game[["neutral_site"]],
                notes = notes_text,
                status_type_id = if (is.null(detailed_status[["type_id"]])) NA_character_ else detailed_status[["type_id"]],
                status_type_name = if (is.null(detailed_status[["type_name"]])) NA_character_ else detailed_status[["type_name"]],
                status_state = if (is.null(detailed_status[["state"]])) NA_character_ else detailed_status[["state"]],
                status_completed = if (is.null(detailed_status[["completed"]])) FALSE else detailed_status[["completed"]],
                status_description = if (is.null(detailed_status[["description"]])) NA_character_ else detailed_status[["description"]],
                status_detail = if (is.null(detailed_status[["detail"]])) NA_character_ else detailed_status[["detail"]],
                status_short_detail = if (is.null(detailed_status[["short_detail"]])) NA_character_ else detailed_status[["short_detail"]],
                stringsAsFactors = FALSE
              )

              # Teams data
              if ("teams" %in% names(game) && "all" %in% names(game[["teams"]])) {
                for (t in seq_along(game[["teams"]][["all"]])) {
                  team <- game[["teams"]][["all"]][[t]]

                  teams_data[[length(teams_data) + 1]] <- data.frame(
                    sport_id = if (is.null(sport[["id"]])) NA_character_ else sport[["id"]],
                    sport_name = if (is.null(sport[["name"]])) NA_character_ else sport[["name"]],
                    league_id = if (is.null(league[["id"]])) NA_character_ else league[["id"]],
                    league_name = if (is.null(league[["name"]])) NA_character_ else league[["name"]],
                    game_id = if (is.null(game[["id"]])) NA_character_ else game[["id"]],
                    team_id = if (is.null(team[["id"]])) NA_character_ else team[["id"]],
                    team_name = if (is.null(team[["name"]])) NA_character_ else team[["name"]],
                    team_display_name = if (is.null(team[["display_name"]])) NA_character_ else team[["display_name"]],
                    team_abbreviation = if (is.null(team[["abbreviation"]])) NA_character_ else team[["abbreviation"]],
                    team_location = if (is.null(team[["location"]])) NA_character_ else team[["location"]],
                    home_away = if (is.null(team[["home_away"]])) NA_character_ else team[["home_away"]],
                    team_order = if (is.null(team[["order"]])) NA_integer_ else team[["order"]],
                    is_winner = if (is.null(team[["winner"]])) FALSE else team[["winner"]],
                    score = if (is.null(team[["score"]])) "" else team[["score"]],
                    record = if (is.null(team[["record"]])) NA_character_ else team[["record"]],
                    wins = if (is.null(team[["wins"]])) 0 else team[["wins"]],
                    losses = if (is.null(team[["losses"]])) 0 else team[["losses"]],
                    team_color = if (is.null(team[["color"]])) NA_character_ else team[["color"]],
                    team_alt_color = if (is.null(team[["alternate_color"]])) NA_character_ else team[["alternate_color"]],
                    team_logo = if (is.null(team[["logo"]])) NA_character_ else team[["logo"]],
                    team_logo_dark = if (is.null(team[["logo_dark"]])) NA_character_ else team[["logo_dark"]],
                    stringsAsFactors = FALSE
                  )
                }
              }

              # Odds data
              if ("odds" %in% names(game)) {
                odds <- game[["odds"]]
                provider_info <- if ("provider" %in% names(odds)) odds[["provider"]] else list()
                point_spread <- if ("point_spread" %in% names(odds)) odds[["point_spread"]] else list()
                moneyline <- if ("moneyline" %in% names(odds)) odds[["moneyline"]] else list()
                total <- if ("total" %in% names(odds)) odds[["total"]] else list()

                odds_data[[length(odds_data) + 1]] <- data.frame(
                  sport_id = if (is.null(sport[["id"]])) NA_character_ else sport[["id"]],
                  sport_name = if (is.null(sport[["name"]])) NA_character_ else sport[["name"]],
                  league_id = if (is.null(league[["id"]])) NA_character_ else league[["id"]],
                  league_name = if (is.null(league[["name"]])) NA_character_ else league[["name"]],
                  game_id = if (is.null(game[["id"]])) NA_character_ else game[["id"]],
                  odds_details = if (is.null(odds[["details"]])) NA_character_ else odds[["details"]],
                  over_under = if (is.null(odds[["over_under"]])) NA_real_ else odds[["over_under"]],
                  spread = if (is.null(odds[["spread"]])) NA_real_ else odds[["spread"]],
                  provider_id = if (is.null(provider_info[["id"]])) NA_character_ else provider_info[["id"]],
                  provider_name = if (is.null(provider_info[["name"]])) NA_character_ else provider_info[["name"]],
                  spread_display_name = if (is.null(point_spread[["display_name"]])) NA_character_ else point_spread[["display_name"]],
                  spread_home_line = if (is.null(point_spread[["home_line"]])) NA_character_ else point_spread[["home_line"]],
                  spread_away_line = if (is.null(point_spread[["away_line"]])) NA_character_ else point_spread[["away_line"]],
                  moneyline_home_odds = if (is.null(moneyline[["home_odds"]])) NA_character_ else moneyline[["home_odds"]],
                  moneyline_away_odds = if (is.null(moneyline[["away_odds"]])) NA_character_ else moneyline[["away_odds"]],
                  total_over_line = if (is.null(total[["over_line"]])) NA_character_ else total[["over_line"]],
                  total_under_line = if (is.null(total[["under_line"]])) NA_character_ else total[["under_line"]],
                  stringsAsFactors = FALSE
                )
              }

              # Broadcasts data
              if ("broadcasts" %in% names(game) && length(game[["broadcasts"]]) > 0) {
                for (b in seq_along(game[["broadcasts"]])) {
                  broadcast <- game[["broadcasts"]][[b]]

                  broadcasts_data[[length(broadcasts_data) + 1]] <- data.frame(
                    sport_id = if (is.null(sport[["id"]])) NA_character_ else sport[["id"]],
                    sport_name = if (is.null(sport[["name"]])) NA_character_ else sport[["name"]],
                    league_id = if (is.null(league[["id"]])) NA_character_ else league[["id"]],
                    league_name = if (is.null(league[["name"]])) NA_character_ else league[["name"]],
                    game_id = if (is.null(game[["id"]])) NA_character_ else game[["id"]],
                    broadcast_type = if (is.null(broadcast[["type"]])) NA_character_ else broadcast[["type"]],
                    broadcast_name = if (is.null(broadcast[["name"]])) NA_character_ else broadcast[["name"]],
                    broadcast_short_name = if (is.null(broadcast[["short_name"]])) NA_character_ else broadcast[["short_name"]],
                    broadcast_call_letters = if (is.null(broadcast[["call_letters"]])) NA_character_ else broadcast[["call_letters"]],
                    is_national = if (is.null(broadcast[["is_national"]])) FALSE else broadcast[["is_national"]],
                    broadcast_priority = if (is.null(broadcast[["priority"]])) NA_integer_ else broadcast[["priority"]],
                    broadcast_language = if (is.null(broadcast[["language"]])) NA_character_ else broadcast[["language"]],
                    broadcast_region = if (is.null(broadcast[["region"]])) NA_character_ else broadcast[["region"]],
                    stringsAsFactors = FALSE
                  )
                }
              } else if ("broadcast_summary" %in% names(game)) {
                broadcasts_data[[length(broadcasts_data) + 1]] <- data.frame(
                  sport_id = if (is.null(sport[["id"]])) NA_character_ else sport[["id"]],
                  sport_name = if (is.null(sport[["name"]])) NA_character_ else sport[["name"]],
                  league_id = if (is.null(league[["id"]])) NA_character_ else league[["id"]],
                  league_name = if (is.null(league[["name"]])) NA_character_ else league[["name"]],
                  game_id = if (is.null(game[["id"]])) NA_character_ else game[["id"]],
                  broadcast_type = NA_character_,
                  broadcast_name = if (is.null(game[["broadcast_summary"]])) NA_character_ else game[["broadcast_summary"]],
                  broadcast_short_name = if (is.null(game[["broadcast_summary"]])) NA_character_ else game[["broadcast_summary"]],
                  broadcast_call_letters = if (is.null(game[["broadcast_summary"]])) NA_character_ else game[["broadcast_summary"]],
                  is_national = NA,
                  broadcast_priority = NA_integer_,
                  broadcast_language = NA_character_,
                  broadcast_region = NA_character_,
                  stringsAsFactors = FALSE
                )
              }
            }
          }
        }
      }
    }
  }

  # Convert lists to data.frames
  result_datasets <- list()

  # Sports dataset
  if (length(sports_data) > 0) {
    result_datasets[["sports"]] <- do.call(rbind, sports_data)
    rownames(result_datasets[["sports"]]) <- NULL
  } else {
    result_datasets[["sports"]] <- data.frame(
      sport_id = character(0),
      sport_name = character(0),
      sport_slug = character(0),
      stringsAsFactors = FALSE
    )
  }

  # Leagues dataset
  if (length(leagues_data) > 0) {
    result_datasets[["leagues"]] <- do.call(rbind, leagues_data)
    rownames(result_datasets[["leagues"]]) <- NULL
  } else {
    result_datasets[["leagues"]] <- data.frame(
      sport_id = character(0),
      sport_name = character(0),
      league_id = character(0),
      league_name = character(0),
      league_abbreviation = character(0),
      league_slug = character(0),
      is_tournament = logical(0),
      current_week_label = character(0),
      current_season = integer(0),
      current_season_type = integer(0),
      current_week = integer(0),
      stringsAsFactors = FALSE
    )
  }

  # Games dataset
  if (length(games_data) > 0) {
    result_datasets[["games"]] <- do.call(rbind, games_data)
    rownames(result_datasets[["games"]]) <- NULL
  } else {
    result_datasets[["games"]] <- data.frame(
      sport_id = character(0),
      sport_name = character(0),
      league_id = character(0),
      league_name = character(0),
      game_id = character(0),
      game_uid = character(0),
      game_name = character(0),
      game_short_name = character(0),
      game_date = character(0),
      game_status = character(0),
      game_summary = character(0),
      week = integer(0),
      week_text = character(0),
      season = integer(0),
      season_type = character(0),
      period = numeric(0),
      clock = character(0),
      location = character(0),
      neutral_site = logical(0),
      notes = character(0),
      status_type_id = character(0),
      status_type_name = character(0),
      status_state = character(0),
      status_completed = logical(0),
      status_description = character(0),
      status_detail = character(0),
      status_short_detail = character(0),
      stringsAsFactors = FALSE
    )
  }

  # Teams dataset
  if (length(teams_data) > 0) {
    result_datasets[["teams"]] <- do.call(rbind, teams_data)
    rownames(result_datasets[["teams"]]) <- NULL
  } else {
    result_datasets[["teams"]] <- data.frame(
      sport_id = character(0),
      sport_name = character(0),
      league_id = character(0),
      league_name = character(0),
      game_id = character(0),
      team_id = character(0),
      team_name = character(0),
      team_display_name = character(0),
      team_abbreviation = character(0),
      team_location = character(0),
      home_away = character(0),
      team_order = integer(0),
      is_winner = logical(0),
      score = character(0),
      record = character(0),
      wins = numeric(0),
      losses = numeric(0),
      team_color = character(0),
      team_alt_color = character(0),
      team_logo = character(0),
      team_logo_dark = character(0),
      stringsAsFactors = FALSE
    )
  }

  # Odds dataset
  if (length(odds_data) > 0) {
    result_datasets[["odds"]] <- do.call(rbind, odds_data)
    rownames(result_datasets[["odds"]]) <- NULL
  } else {
    result_datasets[["odds"]] <- data.frame(
      sport_id = character(0),
      sport_name = character(0),
      league_id = character(0),
      league_name = character(0),
      game_id = character(0),
      odds_details = character(0),
      over_under = numeric(0),
      spread = numeric(0),
      provider_id = character(0),
      provider_name = character(0),
      spread_display_name = character(0),
      spread_home_line = character(0),
      spread_away_line = character(0),
      moneyline_home_odds = character(0),
      moneyline_away_odds = character(0),
      total_over_line = character(0),
      total_under_line = character(0),
      stringsAsFactors = FALSE
    )
  }

  # Broadcasts dataset
  if (length(broadcasts_data) > 0) {
    result_datasets[["broadcasts"]] <- do.call(rbind, broadcasts_data)
    rownames(result_datasets[["broadcasts"]]) <- NULL
  } else {
    result_datasets[["broadcasts"]] <- data.frame(
      sport_id = character(0),
      sport_name = character(0),
      league_id = character(0),
      league_name = character(0),
      game_id = character(0),
      broadcast_type = character(0),
      broadcast_name = character(0),
      broadcast_short_name = character(0),
      broadcast_call_letters = character(0),
      is_national = logical(0),
      broadcast_priority = integer(0),
      broadcast_language = character(0),
      broadcast_region = character(0),
      stringsAsFactors = FALSE
    )
  }

  # Add metadata
  result_datasets[["metadata"]] <- nested_data[["metadata"]]

  # Add summary information
  result_datasets[["summary"]] <- data.frame(
    dataset = c("sports", "leagues", "games", "teams", "odds", "broadcasts"),
    rows = c(
      nrow(result_datasets[["sports"]]),
      nrow(result_datasets[["leagues"]]),
      nrow(result_datasets[["games"]]),
      nrow(result_datasets[["teams"]]),
      nrow(result_datasets[["odds"]]),
      nrow(result_datasets[["broadcasts"]])
    ),
    stringsAsFactors = FALSE
  )

  return(result_datasets)
}
