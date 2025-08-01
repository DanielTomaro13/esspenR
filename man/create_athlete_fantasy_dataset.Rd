% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_nfl_athlete_overview.R
\name{create_athlete_fantasy_dataset}
\alias{create_athlete_fantasy_dataset}
\title{Create athlete fantasy data frame from overview response}
\usage{
create_athlete_fantasy_dataset(data, athlete_id)
}
\arguments{
\item{data}{Raw JSON response from ESPN Web API athlete overview endpoint}

\item{athlete_id}{Athlete ID used in request}
}
\value{
Clean data frame with athlete fantasy information
}
\description{
Create athlete fantasy data frame from overview response
}
