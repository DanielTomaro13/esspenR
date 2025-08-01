% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_wnba_teams.R
\name{fetch_wnba_teams}
\alias{fetch_wnba_teams}
\title{Fetch wnba teams using Site API}
\usage{
fetch_wnba_teams(raw = FALSE)
}
\arguments{
\item{raw}{Logical. If TRUE, assigns raw JSON response to global environment
as 'wnba_teams_raw' for debugging purposes (default: FALSE).}
}
\value{
Invisibly returns the processed data frame. The main purpose is global
environment assignment of \code{wnba_teams} containing:
\itemize{
\item Team identity: ID, UID, slug, abbreviation, names, nickname, location
\item Team branding: colors, logos (regular and dark versions)
\item Team status: active status, all-star designation
\item Conference info: conference ID, name, abbreviation
\item Division info: division ID, name, abbreviation
\item Venue details: arena name, capacity, location, indoor status
}
}
\description{
Retrieves comprehensive wnba teams information from ESPN's Site API.
The function fetches detailed team data including identities, conferences,
divisions, venues, and branding information.
}
\details{
The function creates a comprehensive data frame with all wnba teams information.
This provides complete team data for analysis, visualization, and API reference.

\strong{Team Identity}:
\itemize{
\item Complete naming: display name, short name, nickname, location
\item Identifiers: team ID (for API calls), UID, slug, abbreviation
\item Status: active status, special designations
}

\strong{Team Organization}:
\itemize{
\item Conference: Eastern or Western Conference details
\item Division: Atlantic, Central, Southeast, Northwest, Pacific, Southwest
\item Structure: hierarchical organization within wnba
}

\strong{Team Branding}:
\itemize{
\item Colors: primary and alternate team colors (hex codes)
\item Logos: URLs for regular and dark theme logos with dimensions
}

\strong{Venue Information}:
\itemize{
\item Arena: name, full name, capacity
\item Location: city, state, zip code, country code
\item Features: indoor status (typically true for wnba)
}
}
\examples{
\dontrun{
# Get all wnba teams
fetch_wnba_teams()

# Check the data
head(wnba_teams)
str(wnba_teams)

# View team identity and branding
team_identity <- wnba_teams[, c("team_abbreviation", "team_display_name",
                              "team_location", "team_color", "team_alternate_color")]
print("Team identity:")
print(team_identity)

# View conference and division breakdown
conf_division <- wnba_teams[, c("team_abbreviation", "team_display_name",
                              "conference_name", "division_name")]
print("Conference and division breakdown:")
print(conf_division)

# Analyze conferences
if(require(dplyr, quietly = TRUE)) {
  conference_breakdown <- wnba_teams \%>\%
    filter(!is.na(conference_name) & conference_name != "") \%>\%
    count(conference_name, sort = TRUE)

  print("Teams by conference:")
  print(conference_breakdown)

  # Division breakdown
  division_breakdown <- wnba_teams \%>\%
    filter(!is.na(division_name) & division_name != "") \%>\%
    count(conference_name, division_name) \%>\%
    arrange(conference_name, division_name)

  print("Teams by division:")
  print(division_breakdown)
}

# Venue analysis
venue_info <- wnba_teams[!is.na(wnba_teams$venue_full_name),
                       c("team_abbreviation", "venue_full_name", "venue_capacity",
                         "venue_city", "venue_state")]

if(nrow(venue_info) > 0) {
  venue_info$capacity_num <- as.numeric(venue_info$venue_capacity)
  venue_info <- venue_info[order(-venue_info$capacity_num, na.last = TRUE), ]

  print("Venues by capacity:")
  print(head(venue_info, 10))
}

# Eastern Conference teams
eastern_teams <- wnba_teams[wnba_teams$conference_name == "Eastern Conference" &
                          !is.na(wnba_teams$conference_name), ]

if(nrow(eastern_teams) > 0) {
  print("Eastern Conference teams:")
  print(eastern_teams[, c("team_abbreviation", "team_display_name", "division_name")])
}

# Western Conference teams
western_teams <- wnba_teams[wnba_teams$conference_name == "Western Conference" &
                          !is.na(wnba_teams$conference_name), ]

if(nrow(western_teams) > 0) {
  print("Western Conference teams:")
  print(western_teams[, c("team_abbreviation", "team_display_name", "division_name")])
}

# Team colors analysis
colors_data <- wnba_teams[!is.na(wnba_teams$team_color), ]
if(nrow(colors_data) > 0) {
  print("Team colors:")
  for(i in 1:min(10, nrow(colors_data))) {
    team <- colors_data[i, ]
    cat(sprintf("\%s: Primary #\%s", team$team_abbreviation, team$team_color))
    if(!is.na(team$team_alternate_color) && team$team_alternate_color != "") {
      cat(sprintf(", Secondary #\%s", team$team_alternate_color))
    }
    cat("\n")
  }
}

# Find teams with largest arenas
large_venues <- wnba_teams[!is.na(wnba_teams$venue_capacity) & wnba_teams$venue_capacity != "", ]
if(nrow(large_venues) > 0) {
  large_venues$capacity_num <- as.numeric(large_venues$venue_capacity)
  large_venues <- large_venues[order(-large_venues$capacity_num), ]

  print("Largest wnba arenas:")
  print(head(large_venues[, c("team_abbreviation", "venue_full_name", "capacity_num")], 5))
}

# Geographic distribution
if(require(dplyr, quietly = TRUE)) {
  geo_distribution <- wnba_teams \%>\%
    filter(!is.na(venue_state) & venue_state != "") \%>\%
    count(venue_state, sort = TRUE)

  print("Teams by state:")
  print(head(geo_distribution, 10))
}

# Get team ID for API calls
lakers_info <- wnba_teams[wnba_teams$team_abbreviation == "LAL", ]
if(nrow(lakers_info) > 0) {
  cat(sprintf("Lakers team ID: \%s\n", lakers_info$team_id))
  cat(sprintf("Lakers venue: \%s\n", lakers_info$venue_full_name))
}
}

}
