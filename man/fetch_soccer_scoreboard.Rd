% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_soccer_scoreboard.R
\name{fetch_soccer_scoreboard}
\alias{fetch_soccer_scoreboard}
\title{Fetch soccer scoreboard/schedule data using ESPN API}
\usage{
fetch_soccer_scoreboard(
  league_slug,
  dates = NULL,
  return_type = "all",
  raw = FALSE
)
}
\arguments{
\item{league_slug}{Character. ESPN league slug (required).
Examples: "eng.1" (Premier League), "esp.1" (La Liga), "usa.1" (MLS).
Use \code{fetch_soccer_leagues()} to get available league slugs.}

\item{dates}{Character. Date range for matches (optional).
Format: "YYYYMMDD" for single date or "YYYYMMDD-YYYYMMDD" for range.
If not specified, returns current/recent matches.}

\item{return_type}{Character. Type of data to return (default: "all").
Options are:
\itemize{
\item "league_info" - Basic league and season information
\item "matches" - Match details, scores, and status
\item "all" - Both league info and matches
}}

\item{raw}{Logical. If TRUE, assigns raw JSON response to global environment
as 'soccer_scoreboard_raw' for debugging purposes (default: FALSE).}
}
\value{
Invisibly returns the processed data. The main purpose is global
environment assignment based on return_type:
\itemize{
\item \code{soccer_scoreboard_league_info} - League summary data frame
\item \code{soccer_scoreboard_matches} - Match details data frame
\item \code{soccer_scoreboard_raw} - Raw JSON response (if raw = TRUE)
}
}
\description{
Retrieves soccer match schedules and scores from ESPN's API for a specific league.
The function fetches comprehensive match data including scores, status, teams, and venues.
}
\details{
The function creates structured data frames with comprehensive match information:

\strong{League Info} (\code{soccer_scoreboard_league_info}):
\itemize{
\item League details: slug, name, abbreviation
\item Season information: year, type
\item Date range and total events count
}

\strong{Matches} (\code{soccer_scoreboard_matches}):
\itemize{
\item Match details: event_id, name, date, status
\item Team information: home/away teams with IDs, names, abbreviations
\item Scores: current match scores
\item Venue: stadium name and city
\item Competition: tournament/league round information
\item Broadcast: TV/streaming information
}

\strong{Match Status Examples}:
\itemize{
\item "STATUS_SCHEDULED" - Upcoming match
\item "STATUS_IN_PROGRESS" - Live match
\item "STATUS_FINAL" - Completed match
\item "STATUS_POSTPONED" - Postponed match
}
}
\examples{
\dontrun{
# Get Premier League current matches
fetch_soccer_scoreboard("eng.1")

# Get La Liga matches for specific date
fetch_soccer_scoreboard("esp.1", dates = "20250125")

# Get MLS matches for date range
fetch_soccer_scoreboard("usa.1", dates = "20250101-20250131")

# Get Bundesliga matches (Germany)
fetch_soccer_scoreboard("ger.1")

# Get Champions League matches
fetch_soccer_scoreboard("uefa.champions")

# Check what data was created
head(soccer_scoreboard_league_info)
head(soccer_scoreboard_matches)

# Get only match data
fetch_soccer_scoreboard("eng.1", return_type = "matches")

# Find live matches
live_matches <- soccer_scoreboard_matches[
  soccer_scoreboard_matches$event_status == "STATUS_IN_PROGRESS",
]

# Find completed matches with scores
completed_matches <- soccer_scoreboard_matches[
  soccer_scoreboard_matches$event_status == "STATUS_FINAL" &
  !is.na(soccer_scoreboard_matches$home_score),
]

# Find upcoming matches
upcoming_matches <- soccer_scoreboard_matches[
  soccer_scoreboard_matches$event_status == "STATUS_SCHEDULED",
]

# Get matches by team (example: Arsenal)
arsenal_matches <- soccer_scoreboard_matches[
  grepl("Arsenal", soccer_scoreboard_matches$home_team_name) |
  grepl("Arsenal", soccer_scoreboard_matches$away_team_name),
]

# Get matches by venue
wembley_matches <- soccer_scoreboard_matches[
  grepl("Wembley", soccer_scoreboard_matches$venue_name),
]

# View broadcast information
matches_with_tv <- soccer_scoreboard_matches[
  !is.na(soccer_scoreboard_matches$broadcast_info) &
  soccer_scoreboard_matches$broadcast_info != "",
]

# Get raw data for debugging
fetch_soccer_scoreboard("eng.1", raw = TRUE)
}

}
\seealso{
\code{\link{fetch_soccer_leagues}} to get available league slugs

Common league slugs:
\itemize{
\item \code{eng.1} - English Premier League
\item \code{esp.1} - Spanish La Liga
\item \code{ger.1} - German Bundesliga
\item \code{ita.1} - Italian Serie A
\item \code{fra.1} - French Ligue 1
\item \code{usa.1} - Major League Soccer (MLS)
\item \code{uefa.champions} - UEFA Champions League
\item \code{uefa.europa} - UEFA Europa League
}
}
