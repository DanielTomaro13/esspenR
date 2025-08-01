% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_mens_college_basketball_boxscore.R,
%   R/fetch_nba_boxscore.R, R/fetch_wnba_boxscore.R,
%   R/fetch_womens_college_basketball_boxscore.R
\name{create_team_stats_dataset}
\alias{create_team_stats_dataset}
\title{Create team statistics data frame from ESPN boxscore API}
\usage{
create_team_stats_dataset(data, event_id)

create_team_stats_dataset(data, event_id)

create_team_stats_dataset(data, event_id)

create_team_stats_dataset(data, event_id)
}
\arguments{
\item{data}{Raw JSON response from ESPN boxscore API}

\item{event_id}{Event ID used in the request}
}
\value{
Data frame with team statistics

Data frame with team statistics

Data frame with team statistics

Data frame with team statistics
}
\description{
Processes team-level statistics from ESPN boxscore teams data

Processes team-level statistics from ESPN boxscore teams data

Processes team-level statistics from ESPN boxscore teams data

Processes team-level statistics from ESPN boxscore teams data
}
\keyword{internal}
