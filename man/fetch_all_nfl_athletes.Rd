% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_nfl_athletes.R
\name{fetch_all_nfl_athletes}
\alias{fetch_all_nfl_athletes}
\title{Fetch all NFL athletes with pagination}
\usage{
fetch_all_nfl_athletes(
  batch_size = 1000,
  max_athletes = 10000,
  active = TRUE,
  team_id = NULL,
  position = NULL,
  raw = FALSE
)
}
\arguments{
\item{batch_size}{Integer. Number of athletes to fetch per request (default: 1000).}

\item{max_athletes}{Integer. Maximum total athletes to fetch (default: 10000).}

\item{active}{Logical. Filter for active players only (default: TRUE).}

\item{team_id}{Character or Numeric. Filter by specific team ID (optional).}

\item{position}{Character. Filter by position abbreviation (optional).}

\item{raw}{Logical. If TRUE, assigns raw JSON to global environment. Default FALSE.}
}
\value{
Invisibly returns the data, but main purpose is global environment assignment.
}
\description{
Fetch all NFL athletes with pagination
}
\examples{
# Get all active NFL athletes
fetch_all_nfl_athletes()

# Get all quarterbacks
fetch_all_nfl_athletes(position = "QB")

}
