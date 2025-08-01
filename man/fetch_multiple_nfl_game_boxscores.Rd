% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_nfl_game_boxscore.R
\name{fetch_multiple_nfl_game_boxscores}
\alias{fetch_multiple_nfl_game_boxscores}
\title{Fetch multiple NFL game boxscores}
\usage{
fetch_multiple_nfl_game_boxscores(
  event_ids,
  return_type = "all",
  delay = 0.2,
  raw = FALSE
)
}
\arguments{
\item{event_ids}{Character or Numeric vector. ESPN event IDs.
Vector of unique identifiers for games in ESPN's database.}

\item{return_type}{Character. Type of data to return (default: "all").
Same options as \code{\link{fetch_nfl_game_boxscore}}.}

\item{delay}{Numeric. Delay in seconds between API requests (default: 0.2).
Used to be respectful to ESPN's servers and avoid rate limiting.}

\item{raw}{Logical. If TRUE, assigns raw JSON to global environment
for the first game only (default: FALSE).}
}
\value{
Invisibly returns the combined data frames. The main purpose is global
environment assignment of combined datasets from all games.
}
\description{
Retrieves boxscore data for multiple NFL games with rate limiting to
be respectful to ESPN's CDN API. This function calls \code{\link{fetch_nfl_game_boxscore}}
for each game and combines the results.
}
\details{
The function processes games sequentially with a configurable delay
between requests. Failed requests for individual games are logged but
do not stop the overall process. The final datasets contain data from
all successfully processed games.

This is particularly useful for building comprehensive statistical databases,
comparing player performance across multiple games, or analyzing team
performance trends over time.
}
\examples{
\dontrun{
# Get boxscores for multiple games from same week
week1_games <- c("401671617", "401671618", "401671619")
fetch_multiple_nfl_game_boxscores(week1_games)

# Get only player stats for multiple games
fetch_multiple_nfl_game_boxscores(week1_games, return_type = "player_stats")

# Use longer delay for larger requests
fetch_multiple_nfl_game_boxscores(week1_games, delay = 0.5)

# Analyze combined results
unique_games <- unique(nfl_boxscore_game_info$event_id)
cat("Retrieved boxscores for", length(unique_games), "games\n")

# Compare quarterback performance across multiple games
qb_comparison <- nfl_boxscore_player_stats[
  nfl_boxscore_player_stats$stat_category == "passing" &
  nfl_boxscore_player_stats$stat_name == "passingYards",
  c("event_id", "player_name", "stat_value")
]
}

}
\seealso{
\code{\link{fetch_nfl_game_boxscore}} for single game data
}
