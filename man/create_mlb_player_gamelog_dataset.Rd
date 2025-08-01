% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_mlb_player_stats.R
\name{create_mlb_player_gamelog_dataset}
\alias{create_mlb_player_gamelog_dataset}
\title{Create detailed MLB player game log data frame from ESPN API}
\usage{
create_mlb_player_gamelog_dataset(data, player_id, player_info)
}
\arguments{
\item{data}{Raw JSON response from ESPN API}

\item{player_id}{Player ID used in the request}

\item{player_info}{Player information from separate API call}
}
\value{
Data frame with detailed game log
}
\description{
Processes game-by-game data from the seasonTypes$categories structure
}
\keyword{internal}
