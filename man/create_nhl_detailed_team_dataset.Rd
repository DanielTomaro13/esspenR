% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_nhl_team_detailed.R
\name{create_nhl_detailed_team_dataset}
\alias{create_nhl_detailed_team_dataset}
\title{Create detailed NHL team data frame from Site API response}
\usage{
create_nhl_detailed_team_dataset(data, team_identifier)
}
\arguments{
\item{data}{Raw JSON response from ESPN Site API team endpoint}

\item{team_identifier}{Character. Team identifier used in request}
}
\value{
Data frame with detailed team information
}
\description{
Processes raw JSON response from ESPN Site API into structured data frame
containing comprehensive NHL team information
}
\keyword{internal}
