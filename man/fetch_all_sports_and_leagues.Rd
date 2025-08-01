% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_all_sports_leagues.R
\name{fetch_all_sports_and_leagues}
\alias{fetch_all_sports_and_leagues}
\title{Fetch all sports and their leagues from ESPN Core API}
\usage{
fetch_all_sports_and_leagues(include_leagues = TRUE, raw = FALSE)
}
\arguments{
\item{include_leagues}{Logical. If TRUE, fetches leagues for each sport (default: TRUE)}

\item{raw}{Logical, default FALSE. If TRUE, returns full parsed JSON data.}
}
\value{
A data.frame with sport and league details combined
}
\description{
Gets a comprehensive list of all sports and their associated leagues from ESPN's Core API.
This uses the newer ESPN Core API endpoints that are currently working.
}
\examples{
\dontrun{
# Fetch all sports and their leagues
all_data <- fetch_all_sports_and_leagues()
head(all_data)

# Just get sports without leagues (faster)
just_sports <- fetch_all_sports_and_leagues(include_leagues = FALSE)
print(just_sports)

# Check what sports are available
sports_only <- fetch_all_sports_and_leagues(include_leagues = FALSE)
unique(sports_only$name)

# Filter for a specific sport after fetching all data
basketball_data <- all_data[all_data$slug == "basketball", ]
print(basketball_data)
}
}
