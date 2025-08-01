% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_mens_college_basketball_game_playbyplay.R
\name{create_mens_college_basketball_playbyplay_datasets}
\alias{create_mens_college_basketball_playbyplay_datasets}
\title{Create mens_college_basketball play-by-play data frames from API response}
\usage{
create_mens_college_basketball_playbyplay_datasets(data, event_id)
}
\arguments{
\item{data}{Raw JSON response from ESPN mens_college_basketball API play-by-play endpoint}

\item{event_id}{Character. Event ID used in request}
}
\value{
List containing multiple data frames with play-by-play information
}
\description{
Processes raw JSON response from ESPN mens_college_basketball API into structured data frames
containing detailed play-by-play information
}
\keyword{internal}
