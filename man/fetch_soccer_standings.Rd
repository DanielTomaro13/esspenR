% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_soccer_standings.R
\name{fetch_soccer_standings}
\alias{fetch_soccer_standings}
\title{Fetch soccer league standings data using ESPN API}
\usage{
fetch_soccer_standings(
  league_slug,
  season = NULL,
  return_type = "all",
  raw = FALSE
)
}
\arguments{
\item{league_slug}{Character. ESPN league slug (required).
Examples: "eng.1" (Premier League), "esp.1" (La Liga), "ger.1" (Bundesliga).}

\item{season}{Character. Season year (optional).
Format: "YYYY" (e.g., "2024"). If not specified, returns current season.}

\item{return_type}{Character. Type of data to return (default: "all").
Options: "league_info", "standings", "all".}

\item{raw}{Logical. If TRUE, assigns raw JSON response to global environment
as 'soccer_standings_raw' for debugging purposes (default: FALSE).}
}
\value{
Invisibly returns the processed data.
}
\description{
Retrieves soccer league standings and team positions from ESPN's API for a specific league.
The function fetches comprehensive standings data including team statistics, positions, and form.
}
\examples{
\dontrun{
# Get Premier League current standings
fetch_soccer_standings("eng.1")

# Get La Liga standings for specific season
fetch_soccer_standings("esp.1", season = "2023")

# Get raw data for debugging
fetch_soccer_standings("eng.1", raw = TRUE)
}

}
