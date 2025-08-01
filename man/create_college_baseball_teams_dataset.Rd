% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_college_baseball_teams.R
\name{create_college_baseball_teams_dataset}
\alias{create_college_baseball_teams_dataset}
\title{Create college_baseball teams data frame from Site API response}
\usage{
create_college_baseball_teams_dataset(data)
}
\arguments{
\item{data}{Raw JSON response from ESPN Site API teams endpoint}
}
\value{
Data frame with teams information
}
\description{
Processes raw JSON response from ESPN Site API into structured data frame
containing college_baseball teams information
}
\keyword{internal}
