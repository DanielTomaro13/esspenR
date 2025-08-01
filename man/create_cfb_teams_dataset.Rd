% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_college_football_teams.R
\name{create_cfb_teams_dataset}
\alias{create_cfb_teams_dataset}
\title{Create college football teams data frame from Site API response}
\usage{
create_cfb_teams_dataset(data)
}
\arguments{
\item{data}{Raw JSON response from ESPN Site API teams endpoint}
}
\value{
Data frame with teams information
}
\description{
Processes raw JSON response from ESPN Site API into structured data frame
containing basic college football team information
}
\keyword{internal}
