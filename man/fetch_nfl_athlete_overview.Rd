% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_nfl_athlete_overview.R
\name{fetch_nfl_athlete_overview}
\alias{fetch_nfl_athlete_overview}
\title{Fetch NFL athlete overview data using Web API}
\usage{
fetch_nfl_athlete_overview(athlete_id, return_type = "all", raw = FALSE)
}
\arguments{
\item{athlete_id}{Character or Numeric. ESPN athlete ID (required).}

\item{return_type}{Character. Type of data to return: "stats", "news", "gamelog", "rotowire", "fantasy", or "all" (default: "all").}

\item{raw}{Logical. If TRUE, assigns raw JSON to global environment as 'nfl_athlete_overview_raw'. Default FALSE.}
}
\value{
Invisibly returns the data, but main purpose is global environment assignment.
}
\description{
Fetch NFL athlete overview data using Web API
}
\examples{
# Get athlete statistics
fetch_nfl_athlete_overview(athlete_id = "3139477", return_type = "stats")
head(nfl_athlete_stats)

# Get athlete news
fetch_nfl_athlete_overview(athlete_id = "3139477", return_type = "news")
head(nfl_athlete_news)

# Get athlete game log
fetch_nfl_athlete_overview(athlete_id = "3139477", return_type = "gamelog")
head(nfl_athlete_gamelog)

# Get rotowire information
fetch_nfl_athlete_overview(athlete_id = "3139477", return_type = "rotowire")
head(nfl_athlete_rotowire)

# Get fantasy information
fetch_nfl_athlete_overview(athlete_id = "3139477", return_type = "fantasy")
head(nfl_athlete_fantasy)

# Get all data types
fetch_nfl_athlete_overview(athlete_id = "3139477", return_type = "all")

# Get raw data for custom processing
fetch_nfl_athlete_overview(athlete_id = "3139477", raw = TRUE)
str(nfl_athlete_overview_raw, max.level = 2)

}
