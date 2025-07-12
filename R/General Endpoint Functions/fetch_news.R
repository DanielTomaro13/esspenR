#' Fetch ESPN News Data
#'
#' @description
#' A comprehensive function to fetch and clean ESPN news data from multiple endpoints:
#' - League news articles with full metadata
#' - General ESPN news feed
#'
#' @param type One of 'league_news', 'news_feed'.
#' @param sport Character. Sport slug (e.g. 'football', 'basketball', 'baseball').
#' @param league Optional character. League slug (e.g. 'nfl', 'nba', 'mlb').
#'   Required for 'league_news'.
#' @param limit Optional numeric. Number of articles/items to return (default: 100).
#' @param raw Logical, default FALSE. If TRUE, returns full parsed JSON data.
#' @param flatten_nested Logical. Whether to flatten nested data structures (default: TRUE).
#'
#' @return A tibble with comprehensive, cleaned data.
#' @export
#'
#' @examples
#' # Get NFL news with full details
#' nfl_news <- fetch_news("league_news", "football", "nfl", limit = 50)
#'
#' # Get general football news feed
#' football_feed <- fetch_news("news_feed", "football", limit = 100)
fetch_news <- function(type = c("league_news", "news_feed"),
                       sport, league = NULL, limit = 100, flatten_nested = TRUE, raw = FALSE) {

  # Validate inputs
  type <- match.arg(type)

  if (type == "league_news" && is.null(league)) {
    stop("League parameter is required for type 'league_news'")
  }

  if (!is.character(sport) || length(sport) != 1) {
    stop("Sport must be a single character string")
  }

  if (!is.numeric(limit) || limit <= 0) {
    stop("Limit must be a positive number")
  }

  # Build URL based on type
  url <- switch(
    type,
    "league_news" = glue::glue("https://site.api.espn.com/apis/site/v2/sports/{sport}/{league}/news?limit={limit}"),
    "news_feed" = httr::modify_url(
      "https://now.core.api.espn.com/v1/sports/news",
      query = list(sport = sport, limit = limit)
    )
  )

  # Make request with error handling
  tryCatch({
    resp <- httr::GET(url, httr::timeout(30))

    if (httr::status_code(resp) != 200) {
      stop(glue::glue("API request failed: HTTP {httr::status_code(resp)} - {httr::http_status(resp)$message}"))
    }

    # Parse JSON response
    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content_text, flatten = flatten_nested)

    if (isTRUE(raw)) return(data)

  }, error = function(e) {
    stop(glue::glue("Failed to fetch data: {e$message}"))
  })

  # Process data based on type
  if (type == "league_news") {
    if (is.null(data$articles) || length(data$articles) == 0) {
      message("No league news articles found.")
      return(tibble::tibble())
    }

    articles <- data$articles
    if (isTRUE(raw)) return(articles)
    n_articles <- nrow(articles)

    # Initialize vectors for safe extraction
    web_urls <- rep(NA_character_, n_articles)
    mobile_urls <- rep(NA_character_, n_articles)
    api_urls <- rep(NA_character_, n_articles)
    categories_text <- rep(NA_character_, n_articles)
    keywords_text <- rep(NA_character_, n_articles)

    # Extract links safely
    if ("links" %in% names(articles)) {
      for (i in seq_len(n_articles)) {
        if (!is.na(articles$links[i]) && !is.null(articles$links[[i]])) {
          links <- articles$links[[i]]
          if (is.list(links)) {
            if ("web" %in% names(links) && "href" %in% names(links$web)) {
              web_urls[i] <- links$web$href
            }
            if ("mobile" %in% names(links) && "href" %in% names(links$mobile)) {
              mobile_urls[i] <- links$mobile$href
            }
            if ("api" %in% names(links) && "self" %in% names(links$api) && "href" %in% names(links$api$self)) {
              api_urls[i] <- links$api$self$href
            }
          }
        }
      }
    }

    # Extract categories safely
    if ("categories" %in% names(articles)) {
      for (i in seq_len(n_articles)) {
        if (!is.na(articles$categories[i]) && !is.null(articles$categories[[i]])) {
          cats <- articles$categories[[i]]
          if (is.data.frame(cats) && "description" %in% names(cats)) {
            valid_descriptions <- cats$description[!is.na(cats$description) & cats$description != ""]
            if (length(valid_descriptions) > 0) {
              categories_text[i] <- paste(valid_descriptions, collapse = ", ")
            }
          }
        }
      }
    }

    # Extract keywords safely
    if ("keywords" %in% names(articles)) {
      for (i in seq_len(n_articles)) {
        if (!is.na(articles$keywords[i]) && !is.null(articles$keywords[[i]])) {
          keywords <- articles$keywords[[i]]
          if (is.data.frame(keywords) && "description" %in% names(keywords)) {
            valid_keywords <- keywords$description[!is.na(keywords$description) & keywords$description != ""]
            if (length(valid_keywords) > 0) {
              keywords_text[i] <- paste(valid_keywords, collapse = ", ")
            }
          } else if (is.character(keywords) && length(keywords) > 0) {
            valid_keywords <- keywords[!is.na(keywords) & keywords != ""]
            if (length(valid_keywords) > 0) {
              keywords_text[i] <- paste(valid_keywords, collapse = ", ")
            }
          }
        }
      }
    }

    # Create comprehensive dataset
    news_df <- tibble::tibble(
      # Basic article info
      id = if ("id" %in% names(articles)) articles$id else NA_character_,
      headline = if ("headline" %in% names(articles)) articles$headline else NA_character_,
      description = if ("description" %in% names(articles)) articles$description else NA_character_,
      published = if ("published" %in% names(articles)) articles$published else NA_character_,

      # Story details
      story_type = if ("story" %in% names(articles)) articles$story else NA_character_,
      type = if ("type" %in% names(articles)) articles$type else NA_character_,
      premium = if ("premium" %in% names(articles)) articles$premium else FALSE,

      # Links
      web_url = web_urls,
      mobile_url = mobile_urls,
      api_url = api_urls,

      # Categories and tags
      categories = categories_text,
      keywords = keywords_text,

      # Byline information
      byline = if ("byline" %in% names(articles)) articles$byline else NA_character_,

      # Timestamps (convert to proper datetime)
      published_date = if ("published" %in% names(articles)) {
        as.POSIXct(articles$published, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      } else {
        as.POSIXct(NA)
      },
      last_modified = if ("lastModified" %in% names(articles)) {
        as.POSIXct(articles$lastModified, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      } else {
        as.POSIXct(NA)
      },

      # Source information
      source = if ("source" %in% names(articles)) articles$source else "ESPN",

      # Data collection metadata
      fetched_at = Sys.time(),
      data_source = "ESPN League News API"
    )

    # Clean and format the data
    if ("headline" %in% names(news_df)) {
      news_df$headline <- stringr::str_trim(news_df$headline)
    }
    if ("description" %in% names(news_df)) {
      news_df$description <- stringr::str_trim(news_df$description)
    }

    # Create readable published time
    news_df$published_readable <- format(news_df$published_date, "%Y-%m-%d %H:%M:%S")

    # Extract domain from web URLs
    news_df$web_domain <- stringr::str_extract(news_df$web_url, "https?://([^/]+)")

    # Create article length category based on description
    news_df$article_length <- ifelse(
      is.na(news_df$description), "Unknown",
      ifelse(nchar(news_df$description) < 100, "Short",
             ifelse(nchar(news_df$description) < 300, "Medium", "Long"))
    )

    # Remove rows with missing essential data
    valid_rows <- (!is.na(news_df$headline) & news_df$headline != "") |
      (!is.na(news_df$description) & news_df$description != "")
    news_df <- news_df[valid_rows, ]

    # Sort by published date (newest first)
    if (nrow(news_df) > 0) {
      news_df <- news_df[order(news_df$published_date, decreasing = TRUE, na.last = TRUE), ]
    }

    return(news_df)

  } else if (type == "news_feed") {
    if (is.null(data$headlines) || length(data$headlines) == 0) {
      message("No news feed items found.")
      return(tibble::tibble())
    }

    items <- data$headlines
    if (isTRUE(raw)) return(items)
    n_items <- nrow(items)

    # Initialize vectors for safe extraction
    web_urls <- rep(NA_character_, n_items)
    mobile_urls <- rep(NA_character_, n_items)
    categories_text <- rep(NA_character_, n_items)
    tags_text <- rep(NA_character_, n_items)

    # Extract links safely
    if ("links" %in% names(items)) {
      for (i in seq_len(n_items)) {
        if (!is.na(items$links[i]) && !is.null(items$links[[i]])) {
          links <- items$links[[i]]
          if (is.list(links)) {
            if ("web" %in% names(links) && "href" %in% names(links$web)) {
              web_urls[i] <- links$web$href
            }
            if ("mobile" %in% names(links) && "href" %in% names(links$mobile)) {
              mobile_urls[i] <- links$mobile$href
            }
          }
        }
      }
    }

    # Extract categories safely
    if ("categories" %in% names(items)) {
      for (i in seq_len(n_items)) {
        if (!is.na(items$categories[i]) && !is.null(items$categories[[i]])) {
          cats <- items$categories[[i]]
          if (is.data.frame(cats) && "description" %in% names(cats)) {
            valid_descriptions <- cats$description[!is.na(cats$description) & cats$description != ""]
            if (length(valid_descriptions) > 0) {
              categories_text[i] <- paste(valid_descriptions, collapse = ", ")
            }
          }
        }
      }
    }

    # Extract tags safely
    if ("tags" %in% names(items)) {
      for (i in seq_len(n_items)) {
        if (!is.na(items$tags[i]) && !is.null(items$tags[[i]])) {
          tags <- items$tags[[i]]
          if (is.data.frame(tags) && "description" %in% names(tags)) {
            valid_tags <- tags$description[!is.na(tags$description) & tags$description != ""]
            if (length(valid_tags) > 0) {
              tags_text[i] <- paste(valid_tags, collapse = ", ")
            }
          }
        }
      }
    }

    # Create comprehensive dataset
    feed_df <- tibble::tibble(
      # Basic item info
      id = if ("id" %in% names(items)) items$id else NA_character_,
      title = if ("title" %in% names(items)) items$title else NA_character_,
      description = if ("description" %in% names(items)) items$description else NA_character_,
      published = if ("published" %in% names(items)) items$published else NA_character_,

      # Content details
      content_type = if ("contentType" %in% names(items)) items$contentType else NA_character_,
      data_source_identifier = if ("dataSourceIdentifier" %in% names(items)) items$dataSourceIdentifier else NA_character_,

      # Links
      web_url = web_urls,
      mobile_url = mobile_urls,

      # Categories and classification
      categories = categories_text,
      tags = tags_text,

      # Timestamps
      published_date = if ("published" %in% names(items)) {
        as.POSIXct(items$published, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      } else {
        as.POSIXct(NA)
      },

      # Engagement metrics (if available)
      view_count = if ("viewCount" %in% names(items)) items$viewCount else NA_integer_,
      share_count = if ("shareCount" %in% names(items)) items$shareCount else NA_integer_,

      # Data collection metadata
      fetched_at = Sys.time(),
      data_source = "ESPN News Feed API"
    )

    # Clean and enhance the data
    if ("title" %in% names(feed_df)) {
      feed_df$title <- stringr::str_trim(feed_df$title)
    }
    if ("description" %in% names(feed_df)) {
      feed_df$description <- stringr::str_trim(feed_df$description)
    }

    # Create readable timestamps
    feed_df$published_readable <- format(feed_df$published_date, "%Y-%m-%d %H:%M:%S")

    # Calculate time since published
    feed_df$hours_since_published <- as.numeric(difftime(Sys.time(), feed_df$published_date, units = "hours"))

    # Categorize content freshness
    feed_df$content_freshness <- ifelse(
      feed_df$hours_since_published < 1, "Just posted",
      ifelse(feed_df$hours_since_published < 24, "Today",
             ifelse(feed_df$hours_since_published < 168, "This week", "Older"))
    )

    # Remove rows with missing essential data
    valid_rows <- (!is.na(feed_df$title) & feed_df$title != "")
    feed_df <- feed_df[valid_rows, ]

    # Sort by published date (newest first)
    if (nrow(feed_df) > 0) {
      feed_df <- feed_df[order(feed_df$published_date, decreasing = TRUE, na.last = TRUE), ]
    }

    return(feed_df)
  }
}
