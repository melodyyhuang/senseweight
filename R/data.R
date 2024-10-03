#' National Jobs Training Partnership Act (JTPA) Experimental Data
#'
#' A dataset containing the earnings and other demographic characteristics of
#' individuals identified as women from the JTPA experiment, spanning 16 different
#' experimental sites.
#'
#' @format A data frame with 6109 total observations, and 11 columns
#' \describe{
#'   \item{site}{Experimental site that the individual belonged to}
#'   \item{Y}{Earnings, in US dollars}
#'   \item{T}{Treatment assignment status}
#'   \item{prevearn}{Preivous earnings, in US dollars}
#'   \item{age}{Age of individual}
#'   \item{married}{Marital status (1 for married, 0 otherwise)}
#'   \item{hrwage}{Hourly Wage}
#'   \item{black}{Indicator for whether or not the individual is black}
#'   \item{hispanic}{Indicator for whether or not the individual is Hispanic}
#'   \item{hsorged}{Indicator for whether or not the individual received a high school degree, or GED}
#'   \item{yrs_educ}{Number of years of education completed.}
#'   ...
#' }
#' @source \url{https://www.upjohn.org/data-tools/employment-research-data-center/national-jtpa-study}
"jtpa_women"
