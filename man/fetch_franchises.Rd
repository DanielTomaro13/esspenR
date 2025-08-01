% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_franchises.R
\name{fetch_franchises}
\alias{fetch_franchises}
\title{Fetch ESPN League Franchise Details}
\usage{
fetch_franchises(sport, league, raw = FALSE)
}
\arguments{
\item{sport}{Character. Sport slug (e.g. 'football', 'basketball').}

\item{league}{Character. League slug (e.g. 'nfl', 'nba').}

\item{raw}{Logical. If TRUE, returns full parsed JSON. Default FALSE.}
}
\value{
A tibble of franchises with cleaned columns or raw list if \code{raw = TRUE}.
}
\description{
Lists all franchises (teams/clubs) for a given sport and league,
fetching full details (name, location, nickname, venue) using ESPN's sports.core.api.
}
\examples{
fetch_franchises("football", "nfl")
fetch_franchises("basketball", "nba")
fetch_franchises("football", "nfl", raw = TRUE)
}
