% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_college_football_game_summary.R
\name{fetch_multiple_cfb_game_summaries}
\alias{fetch_multiple_cfb_game_summaries}
\title{Fetch game summaries for multiple college football games}
\usage{
fetch_multiple_cfb_game_summaries(game_ids, delay = 0.5, raw = FALSE)
}
\arguments{
\item{game_ids}{Character vector. ESPN game IDs for the games to fetch.}

\item{delay}{Numeric. Delay in seconds between API requests (default: 0.5).
Used to be respectful to ESPN's servers.}

\item{raw}{Logical. If TRUE, assigns raw JSON to global environment
for the first game only (default: FALSE).}
}
\value{
Invisibly returns the combined data frame. The main purpose is global
environment assignment of combined \code{cfb_game_summary} from all games.
}
\description{
Retrieves comprehensive game summary information for multiple college football games with rate limiting.
This function calls \code{\link{fetch_college_football_game_summary}} for each game
and combines the results.
}
\examples{
\dontrun{
# Get summaries for multiple games
game_ids <- c("400934572", "401012678", "401520281")
fetch_multiple_cfb_game_summaries(game_ids)

# Check combined results
head(cfb_game_summary)

# Compare attendance across games
attendance_comparison <- cfb_game_summary[, c("game_name", "venue_full_name",
                                             "attendance", "venue_capacity")]
attendance_comparison$attendance_num <- as.numeric(attendance_comparison$attendance)
attendance_comparison$capacity_num <- as.numeric(attendance_comparison$venue_capacity)
attendance_comparison$capacity_pct <- round((attendance_comparison$attendance_num /
                                           attendance_comparison$capacity_num) * 100, 1)

print("Attendance comparison:")
print(attendance_comparison[order(-attendance_comparison$attendance_num), ])

# Analyze game outcomes
game_outcomes <- cfb_game_summary[, c("game_name", "home_team_display_name", "home_team_score",
                                     "away_team_display_name", "away_team_score",
                                     "home_team_winner", "away_team_winner")]
print("Game outcomes:")
print(game_outcomes)
}

}
\seealso{
\code{\link{fetch_college_football_game_summary}} for single game data
}
