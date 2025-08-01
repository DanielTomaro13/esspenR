% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_nhl_team_roster.R
\name{create_clean_nhl_roster_dataset}
\alias{create_clean_nhl_roster_dataset}
\title{Create clean NHL roster dataset from ESPN Site API response}
\usage{
create_clean_nhl_roster_dataset(data, team_id)
}
\arguments{
\item{data}{Raw JSON response from ESPN Site API roster endpoint}

\item{team_id}{Team ID used in request}
}
\value{
Clean data frame with roster information
}
\description{
Create clean NHL roster dataset from ESPN Site API response
}
