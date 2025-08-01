% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_nfl_draft.R
\name{create_nfldraft_datasets}
\alias{create_nfldraft_datasets}
\title{Create NFL draft data frames from Core API response - FIXED VERSION}
\usage{
create_nfldraft_datasets(data, year, fetch_details = TRUE, detail_delay = 0.1)
}
\arguments{
\item{data}{Raw JSON response from ESPN Core API draft endpoint}

\item{year}{Character. Season year used in request}

\item{fetch_details}{Logical. Whether to fetch detailed information from reference URLs}

\item{detail_delay}{Numeric. Delay between detail API requests in seconds}
}
\value{
List containing multiple data frames with draft information
}
\description{
Processes raw JSON response from ESPN Core API into structured data frames
containing detailed draft information including complete draft order
}
\keyword{internal}
