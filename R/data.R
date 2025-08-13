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

#' Synthetic Polling Data
#' 
#' A synthetically generated dataset that simulates polling data for a hypothetical support.
#' The covariates are constructed to match commonly used demographic covariates in practice.
#' 
#' @name poll.data
#' 
#' @description
#' - `pid`: party indicator, one of `c('Democrat', 'Republican', 'Independent', 'Other')`
#' - `educ`: education level, one of `c('High School or Less', 'Some college', 'College', 'Post-grad')`
#' - `age`: age of the individual
#' - `age_buckets`: grouped age buckets, one of `c('18to35', '36to50', '51to64', 'Over65')`
#' - `bornagain`: indicator for whether the individual identifies as born-again Christian, one of `c('Yes', 'No')`
#' - `gender`: categorical gender, one of `c('Men', 'Women')`
#' - `race`: categorical race, one of `c('White', 'Black', 'Hispanic', 'Asian', 'Other')`
#' - `vvweight_post`: post-election verified voter weight for the individual
#' 
#' @references
#' 
#' "Sensitivity analysis for survey weights," Political Analysis, 32(1) (2024), 1-16.
#' #' 
#' @usage
#' data('poll.data')
#' 
#' @examples
#' data('poll.data')
NULL
