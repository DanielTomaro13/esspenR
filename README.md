# esspenR

An R package for scraping and accessing ESPN's undocumented API endpoints. `esspenR` provides clean, structured data frames from ESPN's sports data or allows users to work with raw API responses.

## ⚠️ Important Disclaimer

This package uses **unofficial, undocumented ESPN API endpoints** discovered through community reverse-engineering efforts. These APIs:

- Are **not officially supported** by ESPN
- May **change or break without notice**
- Could potentially violate ESPN's Terms of Service
- Are intended for **educational and personal use only**

**Use at your own risk** and please respect ESPN's servers by implementing appropriate rate limiting.

## Installation

You can install the development version of esspenR from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("DanielTomaro13/esspenR")
```

## Quick Start

```r
library(esspenR)

# Get NFL scoreboard data as a clean data frame
nfl_scores <- get_espn_scoreboard("football", "nfl", date = "20240101")

# Get NBA team information
nba_teams <- get_espn_teams("basketball", "nba")

# Get MLB player statistics
mlb_stats <- get_espn_player_stats("baseball", "mlb", season = 2023)

# Access raw API response if you prefer
raw_data <- get_espn_data("site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard", 
                          raw = TRUE)
```

## Features

### Sports Coverage
- **Football**: NFL, College Football
- **Basketball**: NBA, WNBA, Men's/Women's College Basketball  
- **Baseball**: MLB, College Baseball
- **Hockey**: NHL
- **Soccer**: Premier League, MLS, and other major leagues
- **And more**: Golf, Tennis, Racing, and other ESPN-covered sports

### Data Types
- **Scoreboards & Schedules**: Live scores, upcoming games, results
- **Team Information**: Rosters, stats, schedules, news
- **Player Data**: Stats, profiles, game logs, injury reports
- **Standings**: League tables, conference standings, rankings
- **News**: Latest sports news and updates
- **Advanced Stats**: Detailed game statistics and analytics

### Key Functions

#### Core Data Retrieval
```r
# Scoreboards and schedules
get_espn_scoreboard(sport, league, date = NULL, week = NULL)
get_espn_schedule(sport, league, team_id = NULL, season = NULL)

# Team and player data
get_espn_teams(sport, league)
get_espn_team_roster(sport, league, team_id)
get_espn_players(sport, league, active_only = TRUE)
get_espn_player_stats(sport, league, player_id = NULL, season = NULL)

# League information
get_espn_standings(sport, league, season = NULL)
get_espn_news(sport, league, limit = 50)
```

#### Advanced Features
```r
# Fantasy sports data
get_espn_fantasy_league(league_id, season = NULL, view = "mTeam")
get_espn_fantasy_players(season, view = "players_wl")

# Betting and odds (where available)
get_espn_odds(sport, league, game_id)
get_espn_win_probability(sport, league, game_id)

# Historical data
get_espn_historical_stats(sport, league, season_range)
```

## Data Output Options

### Clean Data Frames (Default)
By default, all functions return cleaned, structured data frames ready for analysis:

```r
# Returns a tibble with standardized column names
scoreboard <- get_espn_scoreboard("football", "nfl")
head(scoreboard)
#> # A tibble: 6 × 12
#>   game_id date       home_team away_team home_score away_score status    week  season
#>   <chr>   <date>     <chr>     <chr>          <int>      <int> <chr>    <int>  <int>
#> 1 401547  2024-01-01 Cowboys   Giants            24         17 Final        1   2024
#> 2 401548  2024-01-01 Patriots  Jets              14         21 Final        1   2024
#> ...
```

### Raw API Responses
Set `raw = TRUE` to get the unprocessed JSON response:

```r
# Returns the raw list structure from the API
raw_scores <- get_espn_scoreboard("football", "nfl", raw = TRUE)
str(raw_scores, max.level = 2)
```

## Configuration

### Rate Limiting
Implement responsible usage with built-in rate limiting:

```r
# Set global rate limit (requests per second)
set_espn_rate_limit(requests_per_second = 1)

# Check current rate limit
get_rate_limit_status()
```

## Contributing

Please contribute if you can, the ESPN api is massive and I have not reached every sport and endpoint yet.
Endpoints also change so if you notice any issues please contribute or raise and issue.

### Reporting Issues
- **Bug reports**: Use the [issue tracker](https://github.com/DanielTomaro13/esspenR/issues)
- **Feature requests**: Open a discussion or issue
- **API changes**: If you discover endpoint changes, please report them

## License

MIT © [Daniel Tomaro](https://github.com/DanielTomaro13)

## Citation

If you use `esspenR` in academic research, please cite:

```
Tomaro, D. (2024). esspenR: An R package for accessing ESPN API data. 
R package version 0.1.0. https://github.com/DanielTomaro13/esspenR
```

---

**Disclaimer**: This package is not affiliated with or endorsed by ESPN. Use responsibly and in accordance with ESPN's Terms of Service.
