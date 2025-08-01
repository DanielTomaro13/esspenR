% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_nfl_leaders.R
\name{create_nflleaders_datasets}
\alias{create_nflleaders_datasets}
\title{Create NFL leaders data frames from Core API response}
\usage{
create_nflleaders_datasets(
  data,
  year,
  season_type,
  fetch_athlete_info = TRUE,
  athlete_delay = 0.1
)
}
\arguments{
\item{data}{Raw JSON response from ESPN Core API leaders endpoint}

\item{year}{Character. Season year used in request}

\item{season_type}{Character. Season type used in request}

\item{fetch_athlete_info}{Logical. Whether to fetch detailed athlete info from reference URLs (default: TRUE)}

\item{athlete_delay}{Numeric. Delay between athlete API requests in seconds (default: 0.1)}
}
\value{
List containing multiple data frames with leader information
}
\description{
Processes raw JSON response from ESPN Core API into structured data frames
containing detailed league leader information
}
\keyword{internal}
