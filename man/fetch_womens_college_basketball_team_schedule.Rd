% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_womens_college_basketball_team_schedule.R
\name{fetch_womens_college_basketball_team_schedule}
\alias{fetch_womens_college_basketball_team_schedule}
\title{Fetch womens_college_basketball team schedule data using Site API}
\usage{
fetch_womens_college_basketball_team_schedule(
  team_id,
  season = NULL,
  raw = FALSE
)
}
\arguments{
\item{team_id}{Character or Numeric. ESPN team ID (e.g., "12" for LA Clippers).}

\item{season}{Numeric. Season year (e.g., 2023, 2024). Optional - defaults to current season.}

\item{raw}{Logical. If TRUE, assigns raw JSON to global environment as 'womens_college_basketball_schedule_raw'. Default FALSE.}
}
\value{
Invisibly returns the data, but main purpose is global environment assignment.
}
\description{
Fetch womens_college_basketball team schedule data using Site API
}
\examples{
# Get current season schedule for LA Clippers
fetch_womens_college_basketball_team_schedule(team_id = "12")
head(womens_college_basketball_team_schedule)

# Get raw data for custom processing
fetch_womens_college_basketball_team_schedule(team_id = "12", season = 2023, raw = TRUE)
str(womens_college_basketball_schedule_raw, max.level = 2)

}
