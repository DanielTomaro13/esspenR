#' Safe nested data extraction helper function for college football news
#'
#' Safely extracts values from nested list structures with error handling
#' @param data Nested list or data structure
#' @param path Character vector representing the nested path
#' @param default Default value to return if path doesn't exist
#' @return Value at the specified path or default value
#' @keywords internal
extract_nested_cfb_news <- function(data, path, default = NA) {
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

#' Clean and process news text content
#'
#' Removes HTML tags and cleans text content from news articles
#' @param text Character. Raw text that may contain HTML
#' @return Character. Cleaned text
#' @keywords internal
clean_cfb_news_text <- function(text) {
  if (is.na(text) || is.null(text) || text == "") {
    return(NA_character_)
  }

  # Remove HTML tags
  text <- gsub("<[^>]*>", "", text)

  # Remove extra whitespace
  text <- gsub("\\s+", " ", text)

  # Trim leading and trailing whitespace
  text <- trimws(text)

  # Remove common HTML entities
  text <- gsub("&amp;", "&", text)
  text <- gsub("&lt;", "<", text)
  text <- gsub("&gt;", ">", text)
  text <- gsub("&quot;", '"', text)
  text <- gsub("&#39;", "'", text)
  text <- gsub("&nbsp;", " ", text)

  if (text == "") {
    return(NA_character_)
  }

  return(text)
}

#' Create college football news data frame from Site API response
#'
#' Processes raw JSON response from ESPN Site API into structured data frame
#' containing college football news information
#'
#' @param data Raw JSON response from ESPN Site API news endpoint
#' @return Data frame with news articles information
#' @keywords internal
create_cfb_news_dataset <- function(data) {

  # Initialize news data frame
  news_df <- data.frame(
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
    video_thumbnail = character(0),
    image_id = character(0),
    image_name = character(0),
    image_caption = character(0),
    image_url = character(0),
    image_width = character(0),
    image_height = character(0),
    related_teams = character(0),
    categories = character(0),
    stringsAsFactors = FALSE
  )

  # Extract articles
  articles <- extract_nested_cfb_news(data, c("articles"), list())

  for (i in seq_along(articles)) {
    article <- articles[[i]]

    # Basic article information
    article_id <- extract_nested_cfb_news(article, c("id"), NA_character_)
    headline <- extract_nested_cfb_news(article, c("headline"), NA_character_)
    description <- extract_nested_cfb_news(article, c("description"), NA_character_)
    story <- extract_nested_cfb_news(article, c("story"), NA_character_)
    published <- extract_nested_cfb_news(article, c("published"), NA_character_)
    last_modified <- extract_nested_cfb_news(article, c("lastModified"), NA_character_)
    premium <- extract_nested_cfb_news(article, c("premium"), "false")
    byline <- extract_nested_cfb_news(article, c("byline"), NA_character_)
    data_source_identifier <- extract_nested_cfb_news(article, c("dataSourceIdentifier"), NA_character_)
    section <- extract_nested_cfb_news(article, c("section"), NA_character_)
    type <- extract_nested_cfb_news(article, c("type"), NA_character_)

    # Keywords
    keywords_list <- extract_nested_cfb_news(article, c("keywords"), list())
    keywords <- if (length(keywords_list) > 0) {
      paste(keywords_list, collapse = ", ")
    } else {
      NA_character_
    }

    # Categories
    categories_list <- extract_nested_cfb_news(article, c("categories"), list())
    categories <- if (length(categories_list) > 0) {
      category_names <- sapply(categories_list, function(cat) {
        extract_nested_cfb_news(cat, c("description"),
                                extract_nested_cfb_news(cat, c("name"), ""))
      })
      paste(category_names[category_names != ""], collapse = ", ")
    } else {
      NA_character_
    }

    # Links - find article URL
    links <- extract_nested_cfb_news(article, c("links"), list())
    url <- NA_character_

    if (length(links) > 0) {
      web_link <- links[["web"]]
      if (!is.null(web_link)) {
        url <- extract_nested_cfb_news(web_link, c("href"), NA_character_)
      }

      # If no web link, try api link or first available
      if (is.na(url)) {
        api_link <- links[["api"]]
        if (!is.null(api_link)) {
          url <- extract_nested_cfb_news(api_link, c("href"), NA_character_)
        } else if (length(links) > 0) {
          first_link <- links[[1]]
          url <- extract_nested_cfb_news(first_link, c("href"), NA_character_)
        }
      }
    }

    # Related teams
    related_teams <- NA_character_
    teams_list <- extract_nested_cfb_news(article, c("teams"), list())
    if (length(teams_list) > 0) {
      team_names <- sapply(teams_list, function(team) {
        extract_nested_cfb_news(team, c("shortDisplayName"),
                                extract_nested_cfb_news(team, c("abbreviation"),
                                                        extract_nested_cfb_news(team, c("name"), "")))
      })
      related_teams <- paste(team_names[team_names != ""], collapse = ", ")
    }

    # Video information
    video_id <- NA_character_
    video_headline <- NA_character_
    video_caption <- NA_character_
    video_description <- NA_character_
    video_premium <- NA_character_
    video_duration <- NA_character_
    video_thumbnail <- NA_character_

    video <- extract_nested_cfb_news(article, c("video"), list())
    if (length(video) > 0 && !is.null(video)) {
      video_id <- extract_nested_cfb_news(video, c("id"), NA_character_)
      video_headline <- extract_nested_cfb_news(video, c("headline"), NA_character_)
      video_caption <- extract_nested_cfb_news(video, c("caption"), NA_character_)
      video_description <- extract_nested_cfb_news(video, c("description"), NA_character_)
      video_premium <- extract_nested_cfb_news(video, c("premium"), "false")
      video_duration <- extract_nested_cfb_news(video, c("duration"), NA_character_)
      video_thumbnail <- extract_nested_cfb_news(video, c("thumbnail"), NA_character_)
    }

    # Image information
    image_id <- NA_character_
    image_name <- NA_character_
    image_caption <- NA_character_
    image_url <- NA_character_
    image_width <- NA_character_
    image_height <- NA_character_

    images <- extract_nested_cfb_news(article, c("images"), list())
    if (length(images) > 0) {
      # Take the first/featured image
      first_image <- images[[1]]
      if (!is.null(first_image)) {
        image_id <- extract_nested_cfb_news(first_image, c("id"), NA_character_)
        image_name <- extract_nested_cfb_news(first_image, c("name"), NA_character_)
        image_caption <- extract_nested_cfb_news(first_image, c("caption"), NA_character_)
        image_url <- extract_nested_cfb_news(first_image, c("url"), NA_character_)
        image_width <- extract_nested_cfb_news(first_image, c("width"), NA_character_)
        image_height <- extract_nested_cfb_news(first_image, c("height"), NA_character_)
      }
    }

    # Clean text content
    headline <- clean_cfb_news_text(headline)
    description <- clean_cfb_news_text(description)
    story <- clean_cfb_news_text(story)
    byline <- clean_cfb_news_text(byline)
    video_headline <- clean_cfb_news_text(video_headline)
    video_caption <- clean_cfb_news_text(video_caption)
    video_description <- clean_cfb_news_text(video_description)
    image_caption <- clean_cfb_news_text(image_caption)

    # Create row
    news_row <- data.frame(
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
      video_thumbnail = as.character(video_thumbnail),
      image_id = as.character(image_id),
      image_name = as.character(image_name),
      image_caption = as.character(image_caption),
      image_url = as.character(image_url),
      image_width = as.character(image_width),
      image_height = as.character(image_height),
      related_teams = as.character(related_teams),
      categories = as.character(categories),
      stringsAsFactors = FALSE
    )

    news_df <- rbind(news_df, news_row)
  }

  # Clean up row names
  if (nrow(news_df) > 0) rownames(news_df) <- NULL

  return(news_df)
}

#' Fetch college football news using Site API
#'
#' Retrieves latest college football news from ESPN's Site API.
#' The function fetches comprehensive news information including articles,
#' videos, images, and metadata for college football.
#'
#' @param limit Integer. Maximum number of articles to retrieve (default: 50).
#'   ESPN typically returns 10-50 articles per request.
#' @param team Character. Team abbreviation to filter news by specific team (default: NULL for all news).
#'   Examples: "GT", "UGA", "BAMA", etc.
#' @param raw Logical. If TRUE, assigns raw JSON response to global environment
#'   as 'cfb_news_raw' for debugging purposes (default: FALSE).
#'
#' @return Invisibly returns the processed data frame. The main purpose is global
#'   environment assignment of \code{cfb_news} containing:
#'   \itemize{
#'     \item Article metadata: ID, headline, description, story content
#'     \item Publishing information: published date, last modified, byline
#'     \item Content classification: premium status, section, type, keywords, categories
#'     \item Media content: video and image information with URLs
#'     \item Links: direct URLs to full articles
#'     \item Related content: associated teams, categories
#'   }
#'
#' @details
#' The function creates a structured data frame with comprehensive college football news information.
#' Each row represents a news article with associated metadata, media content,
#' and publishing details.
#'
#' **Article Information**:
#' \itemize{
#'   \item Content: headline, description, full story text
#'   \item Publishing: publication date, last modified date, author byline
#'   \item Classification: section, article type, keywords, categories, premium status
#' }
#'
#' **Media Content**:
#' \itemize{
#'   \item Video: ID, headline, description, duration, premium status, thumbnail
#'   \item Images: ID, caption, URL, dimensions
#'   \item Links: direct URLs to full articles on ESPN
#' }
#'
#' **Related Content**:
#' \itemize{
#'   \item Teams: associated college football teams mentioned in articles
#'   \item Categories: article categories and classifications
#'   \item Keywords: searchable keywords and tags
#' }
#'
#' @examples
#' \dontrun{
#' # Get latest college football news
#' fetch_college_football_news()
#'
#' # Get limited number of articles
#' fetch_college_football_news(limit = 20)
#'
#' # Get news for specific team
#' fetch_college_football_news(team = "GT")
#'
#' # Check the data
#' head(cfb_news)
#'
#' # View recent headlines
#' recent_news <- cfb_news[1:10, c("headline", "published", "byline", "related_teams")]
#' print(recent_news)
#'
#' # Find video content
#' video_news <- cfb_news[!is.na(cfb_news$video_id),
#'                       c("headline", "video_headline", "video_duration")]
#' if(nrow(video_news) > 0) {
#'   print("Articles with video content:")
#'   print(video_news)
#' }
#'
#' # Get articles with images
#' image_news <- cfb_news[!is.na(cfb_news$image_url),
#'                       c("headline", "image_caption", "image_url")]
#' if(nrow(image_news) > 0) {
#'   print("Articles with images:")
#'   print(head(image_news))
#' }
#'
#' # Search for specific topics
#' search_term <- "playoff"
#' relevant_articles <- cfb_news[
#'   grepl(search_term, cfb_news$headline, ignore.case = TRUE) |
#'   grepl(search_term, cfb_news$description, ignore.case = TRUE) |
#'   grepl(search_term, cfb_news$keywords, ignore.case = TRUE),
#'   c("headline", "description", "published", "related_teams")
#' ]
#'
#' if(nrow(relevant_articles) > 0) {
#'   print(sprintf("Articles mentioning '%s':", search_term))
#'   print(relevant_articles)
#' }
#'
#' # Analyze news by categories
#' if(require(dplyr, quietly = TRUE)) {
#'   category_analysis <- cfb_news %>%
#'     filter(!is.na(categories) & categories != "") %>%
#'     separate_rows(categories, sep = ", ") %>%
#'     count(categories, sort = TRUE) %>%
#'     head(10)
#'
#'   print("Most common news categories:")
#'   print(category_analysis)
#' }
#'
#' # Team mention analysis
#' team_mentions <- cfb_news[!is.na(cfb_news$related_teams) & cfb_news$related_teams != "", ]
#' if(nrow(team_mentions) > 0) {
#'   print("Articles with team mentions:")
#'   print(head(team_mentions[, c("headline", "related_teams")], 10))
#' }
#'
#' # Premium vs free content
#' premium_summary <- table(cfb_news$premium)
#' print("Premium content breakdown:")
#' print(premium_summary)
#'
#' # Recent vs older articles
#' if(!all(is.na(cfb_news$published))) {
#'   cfb_news$published_date <- as.Date(substr(cfb_news$published, 1, 10))
#'   recent_cutoff <- Sys.Date() - 7  # Last 7 days
#'
#'   recent_count <- sum(cfb_news$published_date >= recent_cutoff, na.rm = TRUE)
#'   total_count <- nrow(cfb_news)
#'
#'   message(sprintf("Recent articles (last 7 days): %d/%d", recent_count, total_count))
#' }
#'
#' # Article types analysis
#' type_summary <- table(cfb_news$type)
#' print("Article types:")
#' print(sort(type_summary, decreasing = TRUE))
#' }
#'
#' @export
fetch_college_football_news <- function(limit = 50, team = NULL, raw = FALSE) {

  # Input validation
  if (!is.numeric(limit) || limit < 1 || limit > 200) {
    stop("'limit' must be a number between 1 and 200")
  }

  # Build API URL
  url <- "https://site.api.espn.com/apis/site/v2/sports/football/college-football/news"

  # Build query parameters
  params <- list()
  params$limit <- limit

  if (!is.null(team)) {
    params$team <- team
    message(sprintf("Fetching college football news for team: %s (limit: %d)", team, limit))
  } else {
    message(sprintf("Fetching latest college football news (limit: %d)...", limit))
  }

  # Add query parameters to URL
  if (length(params) > 0) {
    param_strings <- paste0(names(params), "=", params)
    url <- paste0(url, "?", paste(param_strings, collapse = "&"))
  }

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
      assign("cfb_news_raw", data, envir = .GlobalEnv)
      message("Raw college football news data assigned to: cfb_news_raw")
      message("Data structure preview:")

      # Show available sections
      sections <- names(data)
      message("- Available sections: ", paste(sections, collapse = ", "))

      # Show articles count
      articles <- extract_nested_cfb_news(data, c("articles"), list())
      message("- Total articles found: ", length(articles))

      if (length(articles) > 0) {
        first_article <- articles[[1]]
        article_sections <- names(first_article)
        message("- First article sections: ", paste(article_sections, collapse = ", "))

        # Show sample content
        headline <- extract_nested_cfb_news(first_article, c("headline"), "No headline")
        byline <- extract_nested_cfb_news(first_article, c("byline"), "No byline")
        message(sprintf("- Sample headline: %s", headline))
        message(sprintf("- Sample byline: %s", byline))
      }

      return(invisible(data))
    }

    # Create news dataset
    news_df <- create_cfb_news_dataset(data)

    # Assign to global environment
    assign("cfb_news", news_df, envir = .GlobalEnv)

    # Summary message
    total_articles <- nrow(news_df)

    if (!is.null(team)) {
      message(sprintf("College football news for %s assigned to: cfb_news (%d articles)", team, total_articles))
    } else {
      message(sprintf("College football news assigned to: cfb_news (%d articles)", total_articles))
    }

    if (total_articles > 0) {
      # Count different content types
      video_count <- sum(!is.na(news_df$video_id) & news_df$video_id != "")
      image_count <- sum(!is.na(news_df$image_url) & news_df$image_url != "")
      premium_count <- sum(news_df$premium == "true", na.rm = TRUE)
      team_mentions <- sum(!is.na(news_df$related_teams) & news_df$related_teams != "")

      message(sprintf("  - Articles with video: %d", video_count))
      message(sprintf("  - Articles with images: %d", image_count))
      message(sprintf("  - Premium articles: %d", premium_count))
      message(sprintf("  - Articles with team mentions: %d", team_mentions))

      # Show article types
      type_counts <- table(news_df$type[!is.na(news_df$type) & news_df$type != ""])
      if (length(type_counts) > 0) {
        top_types <- head(sort(type_counts, decreasing = TRUE), 3)
        type_summary <- paste(names(top_types), collapse = ", ")
        message(sprintf("  - Top article types: %s", type_summary))
      }

      # Show recent headlines
      if (total_articles > 0) {
        recent_headlines <- news_df$headline[1:min(5, total_articles)]
        recent_headlines <- recent_headlines[!is.na(recent_headlines) & recent_headlines != ""]

        if (length(recent_headlines) > 0) {
          message("\nRecent headlines:")
          for (i in seq_along(recent_headlines)) {
            # Truncate long headlines
            headline <- recent_headlines[i]
            if (nchar(headline) > 80) {
              headline <- paste0(substr(headline, 1, 77), "...")
            }
            message(sprintf("  %d. %s", i, headline))
          }
        }
      }

      # Show teams mentioned
      if (team_mentions > 0) {
        all_teams <- paste(news_df$related_teams[!is.na(news_df$related_teams) & news_df$related_teams != ""],
                           collapse = ", ")
        unique_teams <- unique(unlist(strsplit(all_teams, ", ")))
        unique_teams <- unique_teams[unique_teams != ""]

        if (length(unique_teams) > 0) {
          team_sample <- paste(head(unique_teams, 8), collapse = ", ")
          if (length(unique_teams) > 8) {
            team_sample <- paste0(team_sample, "...")
          }
          message(sprintf("\nTeams mentioned: %s", team_sample))
        }
      }
    }

    return(invisible(news_df))

  }, error = function(e) {
    if (!is.null(team)) {
      stop(sprintf("Failed to fetch college football news for team %s: %s", team, e$message))
    } else {
      stop(sprintf("Failed to fetch college football news: %s", e$message))
    }
  })
}
