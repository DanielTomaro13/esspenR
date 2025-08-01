% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_mens_college_basketball_team_detailed.R
\name{fetch_mens_college_basketball_team_detailed}
\alias{fetch_mens_college_basketball_team_detailed}
\title{Fetch detailed mens_college_basketball team information using Site API}
\usage{
fetch_mens_college_basketball_team_detailed(team, raw = FALSE)
}
\arguments{
\item{team}{Character. Team identifier - can be team abbreviation (e.g., "LAL", "BOS")
or team ID (e.g., "13", "2"). Team abbreviations are preferred.}

\item{raw}{Logical. If TRUE, assigns raw JSON response to global environment
as 'mens_college_basketball_team_detailed_raw' for debugging purposes (default: FALSE).}
}
\value{
Invisibly returns the processed data frame. The main purpose is global
environment assignment of \code{mens_college_basketball_team_detailed} containing:
\itemize{
\item Team identity: ID, UID, slug, abbreviation, names, nickname, location
\item Team branding: colors, logos (regular and dark versions)
\item Team status: active status, all-star designation
\item Venue details: arena name, capacity, surface, location, indoor/dome status
\item Conference info: conference ID, name, abbreviation
\item Division info: division ID, name, abbreviation
\item Season data: current season year, type, name
\item Records: overall, home, away, conference win-loss records with percentages
\item Standings: conference/division/league rank, games played, games behind, playoff seed
}
}
\description{
Retrieves comprehensive information for a specific mens_college_basketball team from ESPN's Site API.
The function fetches detailed team data including venue, conference, records, standings,
and complete team identity information.
}
\details{
The function creates a comprehensive data frame with detailed mens_college_basketball team information.
This provides much more detail than the basic teams list, including current season
performance data, complete venue information, and standings.

\strong{Team Identity}:
\itemize{
\item Complete naming: display name, short name, nickname, location
\item Identifiers: team ID (for API calls), UID, slug, abbreviation
\item Status: active status, special designations
}

\strong{Team Organization}:
\itemize{
\item Conference: Eastern or Western Conference affiliation
\item Division: Atlantic, Central, Southeast, Northwest, Pacific, Southwest
\item Structure: hierarchical organization within mens_college_basketball
}

\strong{Venue Information}:
\itemize{
\item Arena: name, full name, capacity
\item Location: city, state, zip code, country code
\item Features: indoor/outdoor status, dome designation
}

\strong{Performance Data}:
\itemize{
\item Records: overall, home, away, conference records with win percentages
\item Standings: conference, division, and league rankings
\item Season: current season information and playoff seeding
}
}
\examples{
\dontrun{
# Get Lakers detailed information
fetch_mens_college_basketball_team_detailed("LAL")

# Get team by ID
fetch_mens_college_basketball_team_detailed("13")

# Get multiple teams (call individually)
teams_to_fetch <- c("LAL", "BOS", "GSW", "MIA")
for(team in teams_to_fetch) {
  fetch_mens_college_basketball_team_detailed(team)
  # Process data here or combine later
}

# Check the detailed data
head(mens_college_basketball_team_detailed)

# View team identity and branding
team_identity <- mens_college_basketball_team_detailed[, c("team_abbreviation", "team_display_name",
                                      "team_location", "team_color", "team_alternate_color")]
print("Team identity:")
print(team_identity)

# View venue information
venue_info <- mens_college_basketball_team_detailed[, c("venue_name", "venue_full_name", "venue_capacity",
                                   "venue_city", "venue_state", "venue_indoor")]
print("Venue information:")
print(venue_info)

# View conference and division information
org_info <- mens_college_basketball_team_detailed[, c("conference_name", "conference_short_name",
                                 "division_name", "division_short_name")]
print("Organizational information:")
print(org_info)

# View current season records
record_info <- mens_college_basketball_team_detailed[, c("current_season_year", "record_overall_summary",
                                    "record_home_summary", "record_away_summary",
                                    "record_overall_percentage")]
print("Season records:")
print(record_info)

# View standings information
standing_info <- mens_college_basketball_team_detailed[, c("standing_conference_rank", "standing_division_rank",
                                      "standing_league_rank", "standing_games_played",
                                      "standing_games_behind", "standing_win_percentage",
                                      "standing_playoff_seed")]
print("Standings information:")
print(standing_info)
}

}
