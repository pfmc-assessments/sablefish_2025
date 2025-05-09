% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-.R
\name{get_data_survey}
\alias{get_data_survey}
\title{Retrieve data from web-based resources}
\usage{
get_data_survey(
  name = "sablefish",
  directory = here::here(),
  remove_old = TRUE
)
}
\arguments{
\item{name}{A string providing the relevant population or species name.
The vector must be of length one.}

\item{directory}{The path to the assessment package of interest. The default
uses \code{\link[here:here]{here::here()}} to locate the appropriate directory but you can set it
to whatever directory you want as long as that directory contains
\code{data-raw}.}

\item{remove_old}{A logical specifying if you want to delete the old data
prior to downloading the new data. The default is \code{TRUE} because why
would you need old data?}
}
\value{
\code{TRUE} is invisibly returned if the function is successful. Otherwise, an
error message is printed to the screen if the function is not successful.
Relevant files will be saved to the disk in the \code{data-raw} folder.
}
\description{
Retrieve data from web-based resources
}
