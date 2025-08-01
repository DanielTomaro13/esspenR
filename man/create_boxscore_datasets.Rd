% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_nfl_game_boxscore.R
\name{create_boxscore_datasets}
\alias{create_boxscore_datasets}
\title{Create boxscore data frames from API response}
\usage{
create_boxscore_datasets(data, event_id)
}
\arguments{
\item{data}{Raw JSON response from ESPN CDN API boxscore endpoint}

\item{event_id}{Character. Event ID used in request}
}
\value{
List containing multiple data frames with boxscore information
}
\description{
Processes raw JSON response from ESPN CDN API into structured data frames
containing detailed boxscore information
}
\keyword{internal}
