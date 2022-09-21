#' @title Response data variable names and types
#'
#' @description A dataset containing variable names and types for Scottish
#' Contact Survey (SCS) response data.
#'
#' @format A data frame with 960 rows and 2 variables: \code{names} and
#' \code{type}.

"resp_names"


#' @title Dummy registration data
#'
#' @description A dataset containing dummy registration data for 100
#' participants of the Scottish Contact Survey (SCS).
#'
#' @format A data frame with 100 rows and 73 variables:
#' \itemize{
#'   \item \strong{CP Number}; unique participant identifier.
#'   \item \strong{Status} of survey participation; 'active', 'reserve'
#'         or 'opt-out'.
#'   \item \strong{Panel} of survey; A, B or Z.
#'   \item \strong{Personal data} including; email, date of birth, gender,
#'         postcode, local authority, ethnicity, employment status, studying
#'         status, occupation, occupation of household highest earner, total
#'         household income, number of vaccine doses received and date this
#'         information was last updated.
#'   \item \strong{Household member data} including; name, age, gender,
#'         occupation and student status for up to ten household members.
#' }

"dummy_reg"


#' @title Dummy response data
#'
#' @description A dataset containing dummy response data for 50
#' participants of the Scottish Contact Survey (SCS). \cr
#'
#' \strong{Note:} \code{dummy_resp} does not contain all variables expected in
#' the actual response data, but just those required for unit tests. For full
#' list of variables in actual data, see \code{scs::resp_names}.
#'
#' @format A data frame with 50 rows and 48 variables including:
#' \itemize{
#'   \item \strong{CP Number}; unique participant identifier.
#'   \item \strong{Email} address.
#'   \item \strong{Household member} name/nickname for up to 10 people.
#'   \item \strong{New household member} name/nickname for up to 4 people.
#'   \item \strong{Contact} name/nickname for up to 30 people.
#'   \item \strong{Vaccine} dose information.
#' }

"dummy_resp"