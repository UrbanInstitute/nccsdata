% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ntee_main.R
\name{parse_ntee}
\alias{parse_ntee}
\title{Parse NTEE2 Codes}
\usage{
parse_ntee(ntee.group, ntee.code, ntee.orgtype)
}
\arguments{
\item{ntee.group}{character vector. Vector of desired Industry Group codes
to filter. Use "all" to include all possible codes.}

\item{ntee.code}{character vector. Sequence of desired Industry, Division and
Subdivision codes (old code structure) to use in filtering. Use "all" to
include all possible codes. Can also provide only partial codes. For example
"A" or "Axx" will query NTEE2 codes based on Industry group "A" and all
division and subdivisions.}

\item{ntee.orgtype}{character vector. Vector of Organization Types.
Use "all" to include all possible codes.}
}
\value{
list of matched NTEE2 codes
}
\description{
Take arguments for Industry Group, Industry, Division,
Subdivision and Organization Types and filters population of NTEE2 codes to
return matching codes
}
