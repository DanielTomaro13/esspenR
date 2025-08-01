% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_mens_college_basketball_team_schedule.R,
%   R/fetch_nba_team_schedule.R, R/fetch_nfl_team_schedule.R,
%   R/fetch_wnba_team_schedule.R,
%   R/fetch_womens_college_basketball_team_schedule.R
\name{create_clean_team_schedule_dataset}
\alias{create_clean_team_schedule_dataset}
\title{Create clean team schedule dataset from ESPN Site API response}
\usage{
create_clean_team_schedule_dataset(data, team_id, season)

create_clean_team_schedule_dataset(data, team_id, season)

create_clean_team_schedule_dataset(data, team_id, season)

create_clean_team_schedule_dataset(data, team_id, season)

create_clean_team_schedule_dataset(data, team_id, season)
}
\arguments{
\item{data}{Raw JSON response from ESPN Site API schedule endpoint}

\item{team_id}{Team ID used in request}

\item{season}{Season used in request (can be NULL)}
}
\value{
Clean data frame with schedule information

Clean data frame with schedule information

Clean data frame with schedule information

Clean data frame with schedule information

Clean data frame with schedule information
}
\description{
Create clean team schedule dataset from ESPN Site API response

Create clean team schedule dataset from ESPN Site API response

Create clean team schedule dataset from ESPN Site API response

Create clean team schedule dataset from ESPN Site API response

Create clean team schedule dataset from ESPN Site API response
}
