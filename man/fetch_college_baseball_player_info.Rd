% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_college_baseball_player_stats.R
\name{fetch_college_baseball_player_info}
\alias{fetch_college_baseball_player_info}
\title{Get college_baseball player information from ESPN player profile API}
\usage{
fetch_college_baseball_player_info(player_id)
}
\arguments{
\item{player_id}{ESPN player ID}
}
\value{
List with player information
}
\description{
Since the gamelog API doesn't include athlete info, fetch it separately
}
\keyword{internal}
