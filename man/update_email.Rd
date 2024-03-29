% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_email.R
\name{update_email}
\alias{update_email}
\title{Update email address in registration data}
\usage{
update_email(old_email, new_email, reg_data)
}
\arguments{
\item{old_email}{Character string; email address to update.}

\item{new_email}{Character string; email address to replace \code{old_email}.}

\item{reg_data}{Data frame of registration data.}
}
\value{
Data frame of registration data with \code{old_email} replaced with
\code{new_email}.
}
\description{
Update email address in registration data
}
