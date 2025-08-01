% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_mlb_boxscore.R
\name{create_mlb_team_stats_dataset}
\alias{create_mlb_team_stats_dataset}
\title{Create MLB team statistics data frame from ESPN boxscore API}
\usage{
create_mlb_team_stats_dataset(data, event_id)
}
\arguments{
\item{data}{Raw JSON response from ESPN boxscore API}

\item{event_id}{Event ID used in the request}
}
\value{
Data frame with team statistics
}
\description{
Processes team-level statistics from ESPN boxscore teams data
}
\keyword{internal}
