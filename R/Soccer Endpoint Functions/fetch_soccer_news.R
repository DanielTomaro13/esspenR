#' Safe nested data extraction helper function for soccer news
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_soccer_news <- function(data, path, default = NA) {
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

#' Clean and process text content for soccer news
#'
#' Removes HTML tags and cleans text content from news articles
#' @param text Character. Raw text that may contain HTML
#' @return Character. Cleaned text
#' @keywords internal
clean_soccer_text_content <- function(text) {
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

#' Create soccer news data frame from Site API response
#'
#' Processes raw JSON response from ESPN Site API into structured data frame
#' containing soccer league news information
#'
#' @param data Raw JSON response from ESPN Site API news endpoint
#' @param league League slug used in request
#' @return Data frame with news articles information
#' @keywords internal
create_soccer_news_dataset <- function(data, league) {

  # Initialize news data frame
  news_df <- data.frame(
    league = character(0),
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
  articles <- extract_nested_soccer_news(data, c("articles"), list())

  for (i in seq_along(articles)) {
    article <- articles[[i]]

    # Basic article information
    article_id <- extract_nested_soccer_news(article, c("id"), NA_character_)
    headline <- extract_nested_soccer_news(article, c("headline"), NA_character_)
    description <- extract_nested_soccer_news(article, c("description"), NA_character_)
    story <- extract_nested_soccer_news(article, c("story"), NA_character_)
    published <- extract_nested_soccer_news(article, c("published"), NA_character_)
    last_modified <- extract_nested_soccer_news(article, c("lastModified"), NA_character_)
    premium <- extract_nested_soccer_news(article, c("premium"), "false")
    byline <- extract_nested_soccer_news(article, c("byline"), NA_character_)
    data_source_identifier <- extract_nested_soccer_news(article, c("dataSourceIdentifier"), NA_character_)
    section <- extract_nested_soccer_news(article, c("section"), NA_character_)
    type <- extract_nested_soccer_news(article, c("type"), NA_character_)

    # Keywords
    keywords_list <- extract_nested_soccer_news(article, c("keywords"), list())
    keywords <- if (length(keywords_list) > 0) {
      paste(keywords_list, collapse = ", ")
    } else {
      NA_character_
    }

    # Links - find article URL
    links <- extract_nested_soccer_news(article, c("links"), list())
    url <- NA_character_
    if (length(links) > 0) {
      web_link <- links[["web"]]
      if (!is.null(web_link)) {
        url <- extract_nested_soccer_news(web_link, c("href"), NA_character_)
      }
    }

    # Video information
    video_id <- NA_character_
    video_headline <- NA_character_
    video_caption <- NA_character_
    video_description <- NA_character_
    video_premium <- NA_character_
    video_duration <- NA_character_

    video <- extract_nested_soccer_news(article, c("video"), list())
    if (length(video) > 0 && !is.null(video)) {
      video_id <- extract_nested_soccer_news(video, c("id"), NA_character_)
      video_headline <- extract_nested_soccer_news(video, c("headline"), NA_character_)
      video_caption <- extract_nested_soccer_news(video, c("caption"), NA_character_)
      video_description <- extract_nested_soccer_news(video, c("description"), NA_character_)
      video_premium <- extract_nested_soccer_news(video, c("premium"), "false")
      video_duration <- extract_nested_soccer_news(video, c("duration"), NA_character_)
    }

    # Image information
    image_id <- NA_character_
    image_name <- NA_character_
    image_caption <- NA_character_
    image_url <- NA_character_
    image_width <- NA_character_
    image_height <- NA_character_

    images <- extract_nested_soccer_news(article, c("images"), list())
    if (length(images) > 0) {
      # Take the first image
      first_image <- images[[1]]
      if (!is.null(first_image)) {
        image_id <- extract_nested_soccer_news(first_image, c("id"), NA_character_)
        image_name <- extract_nested_soccer_news(first_image, c("name"), NA_character_)
        image_caption <- extract_nested_soccer_news(first_image, c("caption"), NA_character_)
        image_url <- extract_nested_soccer_news(first_image, c("url"), NA_character_)
        image_width <- extract_nested_soccer_news(first_image, c("width"), NA_character_)
        image_height <- extract_nested_soccer_news(first_image, c("height"), NA_character_)
      }
    }

    # Clean text content
    headline <- clean_soccer_text_content(headline)
    description <- clean_soccer_text_content(description)
    story <- clean_soccer_text_content(story)
    byline <- clean_soccer_text_content(byline)
    video_headline <- clean_soccer_text_content(video_headline)
    video_caption <- clean_soccer_text_content(video_caption)
    video_description <- clean_soccer_text_content(video_description)
    image_caption <- clean_soccer_text_content(image_caption)

    # Create row
    news_row <- data.frame(
      league = as.character(league),
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

#' Fetch soccer league news using Site API
#'
#' Retrieves league-specific news articles from ESPN's Soccer Site API.
#' The function fetches comprehensive news information including articles,
#' videos, images, and metadata for a specific soccer league.
#'
#' @param league Character. League slug (e.g., "eng.1", "esp.1", "ger.1").
#'   Common leagues: "eng.1" (Premier League), "esp.1" (La Liga),
#'   "ger.1" (Bundesliga), "ita.1" (Serie A), "fra.1" (Ligue 1), "usa.1" (MLS).
#' @param limit Integer. Maximum number of articles to retrieve (default: 50).
#'   ESPN typically returns 10-50 articles per request.
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'soccer_news_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data frame. The main purpose is global
#'   environment assignment of \code{soccer_news} containing:
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
#' and publishing details for the specified soccer league.
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
#' **League Support**:
#' The function supports all major soccer leagues available through ESPN's API.
#' Each league has its own news feed with league-specific content.
#'
#' @examples
#' \dontrun{
#' # Get Premier League news
#' fetch_soccer_news("eng.1")
#'
#' # Get La Liga news
#' fetch_soccer_news("esp.1")
#'
#' # Get limited number of articles
#' fetch_soccer_news("ger.1", limit = 20)
#'
#' # Check the data
#' head(soccer_news)
#'
#' # View recent headlines
#' recent_news <- soccer_news[1:10, c("headline", "published", "byline")]
#' print(recent_news)
#'
#' # Find video content
#' video_news <- soccer_news[!is.na(soccer_news$video_id),
#'                          c("headline", "video_headline", "video_duration")]
#' print(video_news)
#'
#' # Get articles with images
#' image_news <- soccer_news[!is.na(soccer_news$image_url),
#'                          c("headline", "image_caption", "image_url")]
#' print(image_news)
#'
#' # Filter by article type
#' match_reports <- soccer_news[
#'   grepl("report|recap", soccer_news$type, ignore.case = TRUE),
#'   c("headline", "type", "published")
#' ]
#' print(match_reports)
#'
#' # Get transfer news
#' transfer_news <- soccer_news[
#'   grepl("transfer|signing", soccer_news$headline, ignore.case = TRUE),
#'   c("headline", "description", "published")
#' ]
#' print(transfer_news)
#' }
#'
#' @seealso
#' Common league slugs:
#' \itemize{
#'   \item \code{eng.1} - English Premier League
#'   \item \code{esp.1} - Spanish La Liga
#'   \item \code{ger.1} - German Bundesliga
#'   \item \code{ita.1} - Italian Serie A
#'   \item \code{fra.1} - French Ligue 1
#'   \item \code{usa.1} - Major League Soccer (MLS)
#'   \item \code{uefa.champions} - UEFA Champions League
#'   \item \code{uefa.europa} - UEFA Europa League
#' }
#'
#' @importFrom httr GET status_code content timeout http_status
#' @importFrom jsonlite fromJSON
#' @export
fetch_soccer_news <- function(league, limit = 50, raw = FALSE) {

  # Input validation
  if (missing(league) || is.null(league) || league == "") {
    stop("'league' parameter is required. Provide league slug (e.g., 'eng.1', 'esp.1', 'ger.1').")
  }

  if (!is.numeric(limit) || limit < 1 || limit > 200) {
    stop("'limit' must be a number between 1 and 200")
  }

  if (!is.logical(raw)) {
    stop("'raw' parameter must be logical (TRUE or FALSE)")
  }

  # League name mapping for user-friendly messages
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
    "bra.1" = "Brasileirão",
    "uefa.champions" = "Champions League",
    "uefa.europa" = "Europa League"
  )

  league_display <- if (league %in% names(league_names)) league_names[[league]] else league

  # Build API URL for Soccer
  url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/soccer/%s/news?limit=%d",
                 league, limit)

  message(sprintf("Fetching soccer news for %s (%s)...", league_display, league))

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
      assign("soccer_news_raw", data, envir = .GlobalEnv)
      message("Raw soccer news data assigned to: soccer_news_raw")
      message("Data structure preview:")

      # Show available sections
      sections <- names(data)
      message("- Available sections: ", paste(sections, collapse = ", "))

      # Show articles count
      articles <- extract_nested_soccer_news(data, c("articles"), list())
      message("- Total articles found: ", length(articles))

      if (length(articles) > 0) {
        first_article <- articles[[1]]
        article_sections <- names(first_article)
        message("- First article sections: ", paste(article_sections, collapse = ", "))
      }

      return(invisible(data))
    }

    # Create news dataset
    news_df <- create_soccer_news_dataset(data, league)

    # Assign to global environment
    assign("soccer_news", news_df, envir = .GlobalEnv)

    # Summary message
    total_articles <- nrow(news_df)
    video_count <- sum(!is.na(news_df$video_id))
    image_count <- sum(!is.na(news_df$image_url))
    premium_count <- sum(news_df$premium == "true", na.rm = TRUE)

    message(sprintf("Soccer news assigned to: soccer_news (%d articles)", total_articles))
    message(sprintf("  - Articles with video: %d", video_count))
    message(sprintf("  - Articles with images: %d", image_count))
    message(sprintf("  - Premium articles: %d", premium_count))

    if (total_articles > 0) {
      # Show recent headlines
      recent_headlines <- news_df$headline[1:min(5, total_articles)]
      recent_headlines <- recent_headlines[!is.na(recent_headlines)]

      if (length(recent_headlines) > 0) {
        message(sprintf("\nRecent %s headlines:", league_display))
        for (i in seq_along(recent_headlines)) {
          message(sprintf("  %d. %s", i, recent_headlines[i]))
        }
      }
    }

    return(invisible(news_df))

  }, error = function(e) {
    stop(sprintf("Failed to fetch soccer news for %s: %s", league_display, e$message))
  })
}

#' Fetch soccer news for multiple leagues
#'
#' Retrieves league-specific news for multiple soccer leagues with rate limiting.
#' This function calls \code{\link{fetch_soccer_news}} for each league
#' and combines the results.
#'
#' @param leagues Character vector. League slugs to fetch news for.
#'   Examples: c("eng.1", "esp.1", "ger.1") for Premier League, La Liga, Bundesliga.
#' @param limit Integer. Maximum number of articles per league (default: 25).
#' @param delay Numeric. Delay in seconds between API requests (default: 0.5).
#'   Used to be respectful to ESPN's servers.
#' @param raw Logical. If TRUE, assigns raw JSON to global environment
#'   for the first league only (default: FALSE).
#'
#' @return Invisibly returns the combined data frame. The main purpose is global
#'   environment assignment of combined \code{soccer_news} from all leagues.
#'
#' @details
#' The function processes leagues sequentially with a configurable delay
#' between requests. Failed requests for individual leagues are logged but
#' do not stop the overall process. The final dataset contains news data from
#' all successfully processed leagues.
#'
#' This is particularly useful for multi-league news analysis,
#' content aggregation, or monitoring multiple competitions simultaneously.
#'
#' @examples
#' \dontrun{
#' # Get news for major European leagues
#' major_leagues <- c("eng.1", "esp.1", "ger.1", "ita.1", "fra.1")
#' fetch_multiple_soccer_news(major_leagues)
#'
#' # Get news for specific leagues with fewer articles each
#' fetch_multiple_soccer_news(c("eng.1", "usa.1", "mex.1"), limit = 15)
#'
#' # Use longer delay for larger requests
#' fetch_multiple_soccer_news(major_leagues, delay = 1.0)
#'
#' # Check combined results
#' head(soccer_news)
#' table(soccer_news$league)
#'
#' # Find transfer window coverage across leagues
#' transfer_news <- soccer_news[
#'   grepl("transfer|signing|window", soccer_news$headline, ignore.case = TRUE),
#'   c("league", "headline", "published")
#' ]
#' print(transfer_news)
#'
#' # Compare news volume by league
#' league_counts <- table(soccer_news$league)
#' print(league_counts)
#' }
#'
#' @seealso \code{\link{fetch_soccer_news}} for single league data
#' @importFrom httr GET status_code content timeout
#' @export
fetch_multiple_soccer_news <- function(leagues, limit = 25, delay = 0.5, raw = FALSE) {

  # Input validation
  if (length(leagues) == 0) {
    stop("'leagues' must contain at least one league slug")
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

  message(sprintf("Starting to fetch soccer news for %d leagues...", length(leagues)))

  # Process each league sequentially
  for (i in seq_along(leagues)) {
    league <- leagues[i]
    message(sprintf("Fetching soccer news for %s (%d/%d)...", league, i, length(leagues)))

    tryCatch({
      # Fetch individual league data
      league_data <- fetch_soccer_news(
        league = league,
        limit = limit,
        raw = raw
      )

      # If raw data requested, return after first league
      if (isTRUE(raw)) {
        return(invisible(league_data))
      }

      # Combine data
      news_df <- get("soccer_news", envir = .GlobalEnv)
      all_news <- rbind(all_news, news_df)

    }, error = function(e) {
      message(sprintf("Failed to fetch soccer news for %s: %s", league, e$message))
    })

    # Be respectful to the API with delay between requests
    if (i < length(leagues)) {
      Sys.sleep(delay)
    }
  }

  # Remove duplicates and assign combined dataset to global environment
  if (nrow(all_news) > 0) {
    all_news <- all_news[!duplicated(all_news$article_id), ]
    assign("soccer_news", all_news, envir = .GlobalEnv)

    unique_leagues <- length(unique(all_news$league))
    total_articles <- nrow(all_news)

    message(sprintf("Combined soccer news assigned to: soccer_news (%d leagues, %d articles)",
                    unique_leagues, total_articles))

    # Show league breakdown
    league_counts <- table(all_news$league)
    message("Articles per league:")

    # League name mapping for display
    league_names <- list(
      "eng.1" = "Premier League", "esp.1" = "La Liga", "ger.1" = "Bundesliga",
      "ita.1" = "Serie A", "fra.1" = "Ligue 1", "usa.1" = "MLS"
    )

    for (league_slug in names(league_counts)) {
      league_display <- if (league_slug %in% names(league_names)) {
        league_names[[league_slug]]
      } else {
        league_slug
      }
      message(sprintf("  %s (%s): %d articles", league_display, league_slug, league_counts[league_slug]))
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
    message("No news articles retrieved for any leagues")
  }

  return(invisible(all_news))
}
