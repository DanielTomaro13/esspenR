% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_scoreboard.R
\name{fetch_scoreboard}
\alias{fetch_scoreboard}
\title{Fetch ESPN scoreboard data (header endpoint)}
\usage{
fetch_scoreboard(sport = NULL, league = NULL, raw = FALSE)
}
\arguments{
\item{sport}{Optional character. Sport slug (e.g. 'football'). If NULL, fetches general header.}

\item{league}{Optional character. League slug (e.g. 'nfl'). If NULL, fetches general header.}

\item{raw}{Logical. If TRUE, assigns raw JSON to global environment. Default FALSE (assigns clean datasets).}
}
\value{
Invisibly returns the data, but main purpose is global environment assignment.
}
\description{
Fetches the ESPN scoreboard header JSON for general (multi-sport) or specific sport & league.
Returns comprehensive game information and assigns clean datasets to global environment.
}
\examples{
# Get general multi-sport scoreboard (assigns scoreboard_data to global env)
fetch_scoreboard()
head(scoreboard_games)

# Get NFL scoreboard (assigns nfl_* datasets to global env)
fetch_scoreboard("football", "nfl")
head(nfl_games)

# Get raw NBA data (assigns nba_scoreboard to global env)
fetch_scoreboard("basketball", "nba", raw = TRUE)
str(nba_scoreboard, max.level = 2)

# Get raw general data (assigns all_scoreboard to global env)
fetch_scoreboard(raw = TRUE)
str(all_scoreboard, max.level = 2)
}
