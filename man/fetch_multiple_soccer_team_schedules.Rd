% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_soccer_team_schedule.R
\name{fetch_multiple_soccer_team_schedules}
\alias{fetch_multiple_soccer_team_schedules}
\title{Fetch multiple soccer team schedules}
\usage{
fetch_multiple_soccer_team_schedules(
  teams_data,
  return_type = "all",
  delay = 0.5,
  raw = FALSE
)
}
\arguments{
\item{teams_data}{Data frame or list with team information.
Must contain 'league_slug' and 'team_id' columns/elements.
Can also contain optional 'season' column/element.}

\item{return_type}{Character. Type of data to return (default: "all").
Same options as \code{\link{fetch_soccer_team_schedule}}.}

\item{delay}{Numeric. Delay in seconds between API requests (default: 0.5).
Used to be respectful to ESPN's servers and avoid rate limiting.}

\item{raw}{Logical. If TRUE, assigns raw JSON to global environment
for the first team only (default: FALSE).}
}
\value{
Invisibly returns the combined data frames. The main purpose is global
environment assignment of combined datasets from all teams.
}
\description{
Retrieves schedule data for multiple soccer teams with rate limiting to
be respectful to ESPN's API. This function calls \code{\link{fetch_soccer_team_schedule}}
for each team and combines the results.
}
\details{
The function processes teams sequentially with a configurable delay
between requests. Failed requests for individual teams are logged but
do not stop the overall process. The final datasets contain data from
all successfully processed teams.
}
\examples{
\dontrun{
# Create team data frame for multiple Premier League teams
teams_df <- data.frame(
  league_slug = c("eng.1", "eng.1", "eng.1"),
  team_id = c("364", "382", "360"),
  season = c("2024", "2024", "2024")
)
fetch_multiple_soccer_team_schedules(teams_df)

# Get schedules for teams across different leagues
teams_df <- data.frame(
  league_slug = c("eng.1", "esp.1", "ger.1"),
  team_id = c("364", "83", "80")
)
fetch_multiple_soccer_team_schedules(teams_df)

# Get schedules only
fetch_multiple_soccer_team_schedules(teams_df, return_type = "schedule")

# Use longer delay for larger requests
fetch_multiple_soccer_team_schedules(teams_df, delay = 1.0)

# Analyze combined results
unique_teams <- unique(soccer_team_schedule$team_id)
cat("Retrieved schedules for", length(unique_teams), "teams\n")

# Compare team performance
completed_matches <- soccer_team_schedule[
  soccer_team_schedule$event_status == "STATUS_FINAL",
]

team_performance <- aggregate(
  completed_matches$result,
  by = list(team = completed_matches$team_id),
  FUN = table
)
}

}
\seealso{
\code{\link{fetch_soccer_team_schedule}} for single team data
}
