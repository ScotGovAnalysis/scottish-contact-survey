% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reg_data_updates.R
\name{reg_data_updates}
\alias{reg_data_updates}
\title{Updates to registration data}
\usage{
reg_data_updates(reg_data, resp_data, date_updated)
}
\arguments{
\item{reg_data}{Registration data}

\item{resp_data}{Response data}

\item{date_updated}{Date data updates were made; usually last date survey
open.}
}
\value{
Registration data with updates from responses.
}
\description{
Update registration data including area of residence,
employment/education status, occupation and household income from
Scottish Contact Survey (SCS) response data.
}
