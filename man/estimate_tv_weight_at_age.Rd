% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-estimate-tv.R
\name{estimate_tv_weight_at_age}
\alias{estimate_tv_weight_at_age}
\title{Estimate time-varying weight at age}
\usage{
estimate_tv_weight_at_age(max_age = 30, first_year = 1975)
}
\arguments{
\item{max_age}{An integer specifying the maximum age of the modeled data
in the Stock Synthesis model. All age data beyond this will be assigned
to the maximum age. This is typically the age beyond which data are sparse
and weight and length are essentially the same across ages.}

\item{first_year}{A four-digit integer specifying the first year of data in
the assessment model. Sometimes weight-at-age data might begin before this
year but the data are so sparse for other data that the model should not
include this year as a separate year. For example, weight-at-age data for
sablefish start in the early 80s but are sparse until the late 90s,
which means that the cohorts are predicted for years prior to this,
i.e., year - age, so we want to remove these early years with little data.}
}
\value{
A data frame in long format with time-varying weight-at-age data.
}
\description{
Estimate time-varying weight at age
}
\details{
Predict weight-at-age for fishery and survey data without spatial
information, where the model includes age, year, cohort, and sex. There is a
smoother on age, year is modeled as a random effect, cohort is also a random
effect, and sex is a linear predictor to estimate weight, i.e.,
(weight ~ 1 + s(age, by = sex) + (1|fcohort) + (1|fyear) + sex + area).
}
\author{
Kelli F. Johnson and Chantel Wetzel
}
