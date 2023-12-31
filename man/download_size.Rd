% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_data_helpers.R
\name{download_size}
\alias{download_size}
\title{Data size calculator}
\usage{
download_size(dsname, append.bmf, urls = NULL)
}
\arguments{
\item{dsname}{character scalar. Name of data series to query from S3.
Valid inputs are either "core" or "bmf", not both.}

\item{append.bmf}{boolean. Option to merge queried core data with bmf data.
Involves downloading the bmf dataset and will take longer.}

\item{urls}{character vector. Vector of urls to s3 buckets for download.}
}
\value{
a response by the user indicating whether they want to proceed with
the download
}
\description{
This function calculates the size of required downloads
for a user's query.
}
