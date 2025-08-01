% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_college_baseball_playbyplay.R
\name{extract_nested_college_baseball_pbp}
\alias{extract_nested_college_baseball_pbp}
\title{Safe nested data extraction helper function for college_baseball play-by-play}
\usage{
extract_nested_college_baseball_pbp(data, path, default = NA)
}
\arguments{
\item{data}{Nested list or data structure}

\item{path}{Character vector representing the nested path}

\item{default}{Default value to return if path doesn't exist}
}
\value{
Value at the specified path or default value
}
\description{
Safely extracts values from nested list structures with error handling
}
\keyword{internal}
