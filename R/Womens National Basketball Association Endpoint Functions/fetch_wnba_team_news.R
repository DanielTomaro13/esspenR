#' Safe nested data extraction helper function for wnba team news
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_wnba_news <- function(data, path, default = NA) {
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

#' Clean and process text content for wnba news
#'
#' Removes HTML tags and cleans text content from news articles
#' @param text Character. Raw text that may contain HTML
#' @return Character. Cleaned text
#' @keywords internal
clean_wnba_text_content <- function(text) {
  if (is.na(text) || is.null(text) || text == "") {
    return(NA_character_)
  }

  # Remove HTML tags
  text <- gsub("<[^>]*>", "", text)
  # Remove extra whitespace
  text <- gsub("\\s+", " ", text)
  # Trim leading and trailing whitespace
  text <- trimws(text)

  if (text == "") {
    return(NA_character_)
  }

  return(text)
}

#' Create wnba team news data frame from Site API response
#'
#' Processes raw JSON response from ESPN Site API into structured data frame
#' containing team-specific news information
#'
#' @param data Raw JSON response from ESPN Site API news endpoint
#' @param team_id Character. Team ID used in request
#' @return Data frame with news articles information
#' @keywords internal
create_wnba_news_dataset <- function(data, team_id) {
  # Initialize news data frame
  news_df <- data.frame(
    team_id = character(0),
    article_id = character(0),
    headline = character(0),
    description = character(0),
    story = character(0),
    published = character(0),
    last_modified = character(0),
    premium = character(0),
    byline = character(0),
    data_source_identifier = character(0),
    keywords = character(0),
    section = character(0),
    type = character(0),
    url = character(0),
    video_id = character(0),
    video_headline = character(0),
    video_caption = character(0),
    video_description = character(0),
    video_premium = character(0),
    video_duration = character(0),
    image_id = character(0),
    image_name = character(0),
    image_caption = character(0),
    image_url = character(0),
    image_width = character(0),
    image_height = character(0),
    stringsAsFactors = FALSE
  )

  # Extract articles
  articles <- extract_nested_wnba_news(data, c("articles"), list())

  for (i in seq_along(articles)) {
    article <- articles[[i]]

    # Basic article information
    article_id <- extract_nested_wnba_news(article, c("id"), NA_character_)
    headline <- extract_nested_wnba_news(article, c("headline"), NA_character_)
    description <- extract_nested_wnba_news(article, c("description"), NA_character_)
    story <- extract_nested_wnba_news(article, c("story"), NA_character_)
    published <- extract_nested_wnba_news(article, c("published"), NA_character_)
    last_modified <- extract_nested_wnba_news(article, c("lastModified"), NA_character_)
    premium <- extract_nested_wnba_news(article, c("premium"), "false")
    byline <- extract_nested_wnba_news(article, c("byline"), NA_character_)
    data_source_identifier <- extract_nested_wnba_news(article, c("dataSourceIdentifier"), NA_character_)
    section <- extract_nested_wnba_news(article, c("section"), NA_character_)
    type <- extract_nested_wnba_news(article, c("type"), NA_character_)

    # Keywords
    keywords_list <- extract_nested_wnba_news(article, c("keywords"), list())
    keywords <- if (length(keywords_list) > 0) {
      paste(keywords_list, collapse = ", ")
    } else {
      NA_character_
    }

    # Links - find article URL
    links <- extract_nested_wnba_news(article, c("links"), list())
    url <- NA_character_
    if (length(links) > 0) {
      web_link <- links[["web"]]
      if (!is.null(web_link)) {
        url <- extract_nested_wnba_news(web_link, c("href"), NA_character_)
      }
    }

    # Video information
    video_id <- NA_character_
    video_headline <- NA_character_
    video_caption <- NA_character_
    video_description <- NA_character_
    video_premium <- NA_character_
    video_duration <- NA_character_

    video <- extract_nested_wnba_news(article, c("video"), list())
    if (length(video) > 0 && !is.null(video)) {
      video_id <- extract_nested_wnba_news(video, c("id"), NA_character_)
      video_headline <- extract_nested_wnba_news(video, c("headline"), NA_character_)
      video_caption <- extract_nested_wnba_news(video, c("caption"), NA_character_)
      video_description <- extract_nested_wnba_news(video, c("description"), NA_character_)
      video_premium <- extract_nested_wnba_news(video, c("premium"), "false")
      video_duration <- extract_nested_wnba_news(video, c("duration"), NA_character_)
    }

    # Image information
    image_id <- NA_character_
    image_name <- NA_character_
    image_caption <- NA_character_
    image_url <- NA_character_
    image_width <- NA_character_
    image_height <- NA_character_

    images <- extract_nested_wnba_news(article, c("images"), list())
    if (length(images) > 0) {
      # Take the first image
      first_image <- images[[1]]
      if (!is.null(first_image)) {
        image_id <- extract_nested_wnba_news(first_image, c("id"), NA_character_)
        image_name <- extract_nested_wnba_news(first_image, c("name"), NA_character_)
        image_caption <- extract_nested_wnba_news(first_image, c("caption"), NA_character_)
        image_url <- extract_nested_wnba_news(first_image, c("url"), NA_character_)
        image_width <- extract_nested_wnba_news(first_image, c("width"), NA_character_)
        image_height <- extract_nested_wnba_news(first_image, c("height"), NA_character_)
      }
    }

    # Clean text content
    headline <- clean_wnba_text_content(headline)
    description <- clean_wnba_text_content(description)
    story <- clean_wnba_text_content(story)
    byline <- clean_wnba_text_content(byline)
    video_headline <- clean_wnba_text_content(video_headline)
    video_caption <- clean_wnba_text_content(video_caption)
    video_description <- clean_wnba_text_content(video_description)
    image_caption <- clean_wnba_text_content(image_caption)

    # Create row
    news_row <- data.frame(
      team_id = as.character(team_id),
      article_id = as.character(article_id),
      headline = as.character(headline),
      description = as.character(description),
      story = as.character(story),
      published = as.character(published),
      last_modified = as.character(last_modified),
      premium = as.character(premium),
      byline = as.character(byline),
      data_source_identifier = as.character(data_source_identifier),
      keywords = as.character(keywords),
      section = as.character(section),
      type = as.character(type),
      url = as.character(url),
      video_id = as.character(video_id),
      video_headline = as.character(video_headline),
      video_caption = as.character(video_caption),
      video_description = as.character(video_description),
      video_premium = as.character(video_premium),
      video_duration = as.character(video_duration),
      image_id = as.character(image_id),
      image_name = as.character(image_name),
      image_caption = as.character(image_caption),
      image_url = as.character(image_url),
      image_width = as.character(image_width),
      image_height = as.character(image_height),
      stringsAsFactors = FALSE
    )

    news_df <- rbind(news_df, news_row)
  }

  # Clean up row names
  if (nrow(news_df) > 0) rownames(news_df) <- NULL

  return(news_df)
}

#' Get wnba team ID mapping
#'
#' Returns a mapping of team abbreviations to team IDs for ESPN wnba API
#' @return Named vector with team abbreviations as names and IDs as values
#' @export
get_wnba_team_ids <- function() {
  team_ids <- c(
    "ATL" = "1", "BOS" = "2", "BKN" = "17", "CHA" = "30", "CHI" = "4", "CLE" = "5",
    "DAL" = "6", "DEN" = "7", "DET" = "8", "GSW" = "9", "HOU" = "10", "IND" = "11",
    "LAC" = "12", "LAL" = "13", "MEM" = "29", "MIA" = "14", "MIL" = "15", "MIN" = "16",
    "NO" = "3", "NYK" = "18", "OKC" = "25", "ORL" = "19", "PHI" = "20", "PHX" = "21",
    "POR" = "22", "SAC" = "23", "SA" = "24", "TOR" = "28", "UTA" = "26", "WAS" = "27"
  )
  return(team_ids)
}

#' Fetch wnba team-specific news using Site API
#'
#' Retrieves team-specific news articles from ESPN's wnba Site API.
#' The function fetches comprehensive news information including articles,
#' videos, images, and metadata for a specific wnba team.
#'
#' @param team Character. Team abbreviation (e.g., "LAL", "BOS") or team ID.
#'   If abbreviation is provided, it will be converted to team ID automatically.
#' @param limit Integer. Maximum number of articles to retrieve (default: 50).
#'   ESPN typically returns 10-50 articles per request.
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'wnba_team_news_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data frame. The main purpose is global
#'   environment assignment of \code{wnba_team_news} containing:
#'   \itemize{
#'     \item Article metadata: ID, headline, description, story content
#'     \item Publishing information: published date, last modified, byline
#'     \item Content classification: premium status, section, type, keywords
#'     \item Media content: video and image information with URLs
#'     \item Links: direct URLs to full articles
#'   }
#'
#' @details
#' The function creates a structured data frame with comprehensive news information.
#' Each row represents a news article with associated metadata, media content,
#' and publishing details.
#'
#' **Article Information**:
#' \itemize{
#'   \item Basic content: headline, description, full story text
#'   \item Publishing details: publication date, last modified date, author byline
#'   \item Classification: section, article type, keywords, premium status
#' }
#'
#' **Media Content**:
#' \itemize{
#'   \item Video: ID, headline, description, duration, premium status
#'   \item Images: ID, caption, URL, dimensions
#'   \item Links: direct URLs to full articles on ESPN
#' }
#'
#' **Team ID Mapping**:
#' The function accepts both team abbreviations (e.g., "LAL", "BOS") and team IDs.
#' Use \code{get_wnba_team_ids()} to see all available team mappings.
#'
#' @examples
#' \dontrun{
#' # Get news for Los Angeles Lakers
#' fetch_wnba_team_news("LAL")
#'
#' # Get news using team ID
#' fetch_wnba_team_news("13")
#'
#' # Get limited number of articles
#' fetch_wnba_team_news("BOS", limit = 20)
#'
#' # Check the data
#' head(wnba_team_news)
#'
#' # View recent headlines
#' recent_news <- wnba_team_news[1:10, c("headline", "published", "byline")]
#' print(recent_news)
#'
#' # Find video content
#' video_news <- wnba_team_news[!is.na(wnba_team_news$video_id),
#'                            c("headline", "video_headline", "video_duration")]
#' print(video_news)
#'
#' # Get articles with images
#' image_news <- wnba_team_news[!is.na(wnba_team_news$image_url),
#'                            c("headline", "image_caption", "image_url")]
#' print(image_news)
#'
#' # See all available team IDs
#' team_ids <- get_wnba_team_ids()
#' print(team_ids)
#' }
#'
#' @seealso \code{\link{get_wnba_team_ids}} for team ID mappings
#' @importFrom httr GET status_code content timeout http_status
#' @importFrom jsonlite fromJSON
#' @export
fetch_wnba_team_news <- function(team, limit = 50, raw = FALSE) {
  # Input validation
  if (missing(team) || is.null(team) || team == "") {
    stop("'team' parameter is required. Provide team abbreviation (e.g., 'LAL') or team ID.")
  }

  if (!is.numeric(limit) || limit < 1 || limit > 200) {
    stop("'limit' must be a number between 1 and 200")
  }

  if (!is.logical(raw)) {
    stop("'raw' parameter must be logical (TRUE or FALSE)")
  }

  # Convert team abbreviation to ID if necessary
  team_ids <- get_wnba_team_ids()

  if (nchar(team) <= 3 && toupper(team) %in% names(team_ids)) {
    # It's a team abbreviation
    team_abbr <- toupper(team)
    team_id <- team_ids[team_abbr]
    message(sprintf("Using team: %s (ID: %s)", team_abbr, team_id))
  } else {
    # Assume it's already a team ID
    team_id <- as.character(team)
    # Validate team ID
    if (!team_id %in% team_ids) {
      available_teams <- paste(names(team_ids), collapse = ", ")
      stop(sprintf("Invalid team ID '%s'. Available teams: %s", team_id, available_teams))
    }
    team_abbr <- names(team_ids)[team_ids == team_id]
    message(sprintf("Using team: %s (ID: %s)", team_abbr, team_id))
  }

  # Build API URL for wnba
  url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/basketball/wnba/news?team=%s&limit=%d",
                 team_id, limit)

  message(sprintf("Fetching wnba news for team %s...", team_abbr))

  # Fetch and parse data
  tryCatch({
    resp <- httr::GET(url, httr::timeout(30))

    if (httr::status_code(resp) != 200) {
      stop(sprintf("API request failed: HTTP %d - %s",
                   httr::status_code(resp),
                   httr::http_status(resp)$message))
    }

    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content_text, simplifyVector = FALSE, simplifyDataFrame = FALSE)

    # Handle raw data assignment for debugging
    if (isTRUE(raw)) {
      assign("wnba_team_news_raw", data, envir = .GlobalEnv)
      message("Raw wnba team news data assigned to: wnba_team_news_raw")
      message("Data structure preview:")

      # Show available sections
      sections <- names(data)
      message("- Available sections: ", paste(sections, collapse = ", "))

      # Show articles count
      articles <- extract_nested_wnba_news(data, c("articles"), list())
      message("- Total articles found: ", length(articles))

      if (length(articles) > 0) {
        first_article <- articles[[1]]
        article_sections <- names(first_article)
        message("- First article sections: ", paste(article_sections, collapse = ", "))
      }

      return(invisible(data))
    }

    # Create news dataset
    news_df <- create_wnba_news_dataset(data, team_id)

    # Assign to global environment
    assign("wnba_team_news", news_df, envir = .GlobalEnv)

    # Summary message
    total_articles <- nrow(news_df)
    video_count <- sum(!is.na(news_df$video_id))
    image_count <- sum(!is.na(news_df$image_url))
    premium_count <- sum(news_df$premium == "true", na.rm = TRUE)

    message(sprintf("wnba team news assigned to: wnba_team_news (%d articles)", total_articles))
    message(sprintf("  - Articles with video: %d", video_count))
    message(sprintf("  - Articles with images: %d", image_count))
    message(sprintf("  - Premium articles: %d", premium_count))

    if (total_articles > 0) {
      # Show recent headlines
      recent_headlines <- news_df$headline[1:min(5, total_articles)]
      recent_headlines <- recent_headlines[!is.na(recent_headlines)]

      if (length(recent_headlines) > 0) {
        message("\nRecent headlines:")
        for (i in seq_along(recent_headlines)) {
          message(sprintf("  %d. %s", i, recent_headlines[i]))
        }
      }
    }

    return(invisible(news_df))

  }, error = function(e) {
    stop(sprintf("Failed to fetch wnba team news for team %s: %s", team_abbr, e$message))
  })
}

#' Fetch wnba news for multiple teams
#'
#' Retrieves team-specific news for multiple wnba teams with rate limiting.
#' This function calls \code{\link{fetch_wnba_team_news}} for each team
#' and combines the results.
#'
#' @param teams Character vector. Team abbreviations or IDs to fetch news for.
#' @param limit Integer. Maximum number of articles per team (default: 25).
#' @param delay Numeric. Delay in seconds between API requests (default: 0.5).
#'   Used to be respectful to ESPN's servers.
#' @param raw Logical. If TRUE, assigns raw JSON to global environment
#'   for the first team only (default: FALSE).
#'
#' @return Invisibly returns the combined data frame. The main purpose is global
#'   environment assignment of combined \code{wnba_team_news} from all teams.
#'
#' @details
#' The function processes teams sequentially with a configurable delay
#' between requests. Failed requests for individual teams are logged but
#' do not stop the overall process. The final dataset contains news data from
#' all successfully processed teams.
#'
#' This is particularly useful for league-wide news analysis,
#' content aggregation, or monitoring multiple teams simultaneously.
#'
#' @examples
#' \dontrun{
#' # Get news for Western Conference contenders
#' fetch_multiple_wnba_team_news(c("LAL", "GSW", "DEN", "PHX"))
#'
#' # Get news for specific teams with fewer articles each
#' fetch_multiple_wnba_team_news(c("BOS", "MIA", "NYK"), limit = 15)
#'
#' # Use longer delay for larger requests
#' east_teams <- c("BOS", "MIA", "PHI", "MIL", "ATL")
#' fetch_multiple_wnba_team_news(east_teams, delay = 1.0)
#'
#' # Check combined results
#' head(wnba_team_news)
#' table(wnba_team_news$team_id)
#'
#' # Find trade deadline coverage
#' trade_news <- wnba_team_news[
#'   grepl("trade|deadline", wnba_team_news$headline, ignore.case = TRUE),
#'   c("team_id", "headline", "published")
#' ]
#' print(trade_news)
#' }
#'
#' @seealso \code{\link{fetch_wnba_team_news}} for single team data
#' @importFrom httr GET status_code content timeout
#' @export
fetch_multiple_wnba_team_news <- function(teams, limit = 25, delay = 0.5, raw = FALSE) {
  # Input validation
  if (length(teams) == 0) {
    stop("'teams' must contain at least one team")
  }

  if (!is.numeric(limit) || limit < 1 || limit > 200) {
    stop("'limit' must be a number between 1 and 200")
  }

  if (!is.numeric(delay) || delay < 0) {
    stop("'delay' must be a non-negative number")
  }

  if (!is.logical(raw)) {
    stop("'raw' parameter must be logical (TRUE or FALSE)")
  }

  # Initialize combined data container
  all_news <- data.frame()

  message(sprintf("Starting to fetch wnba team news for %d teams...", length(teams)))

  # Process each team sequentially
  for (i in seq_along(teams)) {
    team <- teams[i]
    message(sprintf("Fetching wnba team news for %s (%d/%d)...", team, i, length(teams)))

    tryCatch({
      # Fetch individual team data
      team_data <- fetch_wnba_team_news(
        team = team,
        limit = limit,
        raw = raw
      )

      # If raw data requested, return after first team
      if (isTRUE(raw)) {
        return(invisible(team_data))
      }

      # Combine data
      news_df <- get("wnba_team_news", envir = .GlobalEnv)
      all_news <- rbind(all_news, news_df)

    }, error = function(e) {
      message(sprintf("Failed to fetch wnba team news for %s: %s", team, e$message))
    })

    # Be respectful to the API with delay between requests
    if (i < length(teams)) {
      Sys.sleep(delay)
    }
  }

  # Remove duplicates and assign combined dataset to global environment
  if (nrow(all_news) > 0) {
    all_news <- all_news[!duplicated(all_news$article_id), ]
    assign("wnba_team_news", all_news, envir = .GlobalEnv)

    unique_teams <- length(unique(all_news$team_id))
    total_articles <- nrow(all_news)

    message(sprintf("Combined wnba team news assigned to: wnba_team_news (%d teams, %d articles)",
                    unique_teams, total_articles))

    # Show team breakdown
    team_counts <- table(all_news$team_id)
    message("Articles per team:")

    team_ids <- get_wnba_team_ids()
    for (team_id in names(team_counts)) {
      team_abbr <- names(team_ids)[team_ids == team_id]
      message(sprintf("  %s: %d articles", team_abbr, team_counts[team_id]))
    }

    # Show content summary
    video_count <- sum(!is.na(all_news$video_id))
    image_count <- sum(!is.na(all_news$image_url))
    premium_count <- sum(all_news$premium == "true", na.rm = TRUE)

    message(sprintf("\nCombined content summary:"))
    message(sprintf("  - Total articles: %d", total_articles))
    message(sprintf("  - Articles with video: %d", video_count))
    message(sprintf("  - Articles with images: %d", image_count))
    message(sprintf("  - Premium articles: %d", premium_count))

  } else {
    message("No news articles retrieved for any teams")
  }

  return(invisible(all_news))
}
