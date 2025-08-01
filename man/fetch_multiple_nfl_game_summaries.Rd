% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_nfl_game_summary.R
\name{fetch_multiple_nfl_game_summaries}
\alias{fetch_multiple_nfl_game_summaries}
\title{Fetch multiple NFL game summaries}
\usage{
fetch_multiple_nfl_game_summaries(
  event_ids,
  return_type = "all",
  delay = 0.3,
  raw = FALSE
)
}
\arguments{
\item{event_ids}{Character or Numeric vector. ESPN event IDs.
Vector of unique identifiers for games in ESPN's database.}

\item{return_type}{Character. Type of data to return (default: "all").
Same options as \code{\link{fetch_nfl_game_summary}}.}

\item{delay}{Numeric. Delay in seconds between API requests (default: 0.3).
Used to be respectful to ESPN's servers and avoid rate limiting.}

\item{raw}{Logical. If TRUE, assigns raw JSON to global environment
for the first game only (default: FALSE).}
}
\value{
Invisibly returns the combined data frames. The main purpose is global
environment assignment of combined datasets from all games.
}
\description{
Retrieves game summary data for multiple NFL games with rate limiting to
be respectful to ESPN's Site API. This function calls \code{\link{fetch_nfl_game_summary}}
for each game and combines the results.
}
\details{
The function processes games sequentially with a configurable delay
between requests. Failed requests for individual games are logged but
do not stop the overall process. The final datasets contain data from
all successfully processed games.

This is particularly useful for analyzing multiple games from a week,
comparing team performance across games, or building historical datasets.
}
\examples{
\dontrun{
# Get summaries for multiple games from same week
week1_games <- c("401671617", "401671618", "401671619")
fetch_multiple_nfl_game_summaries(week1_games)

# Get only box scores for multiple games
fetch_multiple_nfl_game_summaries(week1_games, return_type = "all")

# Analyze combined results
unique_games <- unique(nfl_game_info$event_id)
cat("Retrieved summaries for", length(unique_games), "games\n")
}

}
\seealso{
\code{\link{fetch_nfl_game_summary}} for single game data
}
