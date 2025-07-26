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

#' Cooperative Election Survey
#' 
#' A dataset containing TODO (document the goal of the data)
#' 
#' @name ces
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
#' TODO (document where you got this from)
#' 
#' @usage
#' data('ces')
#' 
#' @examples
#' data('ces')
NULL

#' Washington Post TODO (what is this)
#' 
#' A dataset containing data from the WaPo on TODO (document the goal of the data)
#' 
#' @name wapo
#' 
#' @description
#' - `candidate`: candidate indicator, one of `c(0, 1)`
#' - `pid`: party indicator, one of `c('Democrat', 'Republican', 'Independent', 'Other')`
#' - `educ`: education level, one of `c('High School or Less', 'Some college', 'College', 'Post-grad')`
#' - `age`: age of the individual
#' - `age_buckets`: grouped age buckets, one of `c('18to35', '36to50', '51to64', 'Over65')` 
#' - `bornagain`: indicator for whether the individual identifies as born-again Christian, one of `c('Yes', 'No')`
#' - `gender`: categorical gender, one of `c('Men', 'Women')`
#' - `race`: categorical race, one of `c('White', 'Black', 'Hispanic', 'Asian', 'Other')`
#' - `weight`: survey weight
#' - `political_interest`: binary indicator for political interest
#' - `political_interest_full`: full political interest, one of `c('Not so closely', 'Somewhat closely', 'Very closely')`
#' 
#' @references
#' TODO (document where you got this from)
#' 
#' @usage
#' data('wapo')
#' 
#' @examples
#' data('wapo')
NULL

#' Lalonde Experimental Data
#' 
#' Data from the National Supported Work Demonstration, a job training program
#' studied by Lalonde (1986). The dataset includes both treated and control
#' groups, with various demographic and earnings characteristics. This extract
#' was created by Dehejia and Wahba (1999) and is commonly used in causal inference
#' research.
#' 
#' @name nsw
#' 
#' @description
#' - `treat`: Treatment assignment status (1 for treated, 0 for control)
#' - `age`: Age of the individual
#' - `education`: Years of education completed
#' - `black`: Indicator for whether the individual is black
#' - `hispanic`: Indicator for whether the individual is Hispanic
#' - `married`: Indicator for whether the individual is married
#' - `nodegree`: Indicator for whether the individual has no degree
#' - `re75`: Earnings in 1975
#' - `re78`: Earnings in 1978 (outcome variable)
#' 
#' @references
#' "Causal Effects in Non-Experimental Studies: Reevaluating the Evaluation of 
#' Training Programs," Journal of the American Statistical Association, Vol. 94, 
#' No. 448 (December 1999), pp. 1053-1062.
#' 
#' "Propensity Score Matching Methods for Non-Experimental Causal Studies," 
#' Review of Economics and Statistics, Vol. 84, (February 2002), pp. 151-161.
#' 
#' @return A data frame with 722 observations and 9 variables
#' 
#' @usage
#' data('nsw')
#' 
#' @examples
#' data('nsw')
NULL

#' Lalonde Experimental Data (Dehejia and Wahba 1999 Subset)
#' 
#' Data from the National Supported Work Demonstration, a job training program
#' studied by Lalonde (1986). The dataset includes both treated and control
#' groups, with various demographic and earnings characteristics. This extract
#' was created by Dehejia and Wahba (1999) to include one year of pre-treatment 
#' earnings.
#' 
#' @name nsw_dw
#' 
#' @description
#' - `treat`: Treatment assignment status (1 for treated, 0 for control)
#' - `age`: Age of the individual
#' - `education`: Years of education completed
#' - `black`: Indicator for whether the individual is black
#' - `hispanic`: Indicator for whether the individual is Hispanic
#' - `married`: Indicator for whether the individual is married
#' - `nodegree`: Indicator for whether the individual has no degree
#' - `re74`: Earnings in 1974
#' - `re75`: Earnings in 1975
#' - `re78`: Earnings in 1978 (outcome variable)
#' 
#' @references
#' "Causal Effects in Non-Experimental Studies: Reevaluating the Evaluation of 
#' Training Programs," Journal of the American Statistical Association, Vol. 94, 
#' No. 448 (December 1999), pp. 1053-1062.
#' 
#' "Propensity Score Matching Methods for Non-Experimental Causal Studies," 
#' Review of Economics and Statistics, Vol. 84, (February 2002), pp. 151-161.
#' 
#' @return A data frame with 445 observations and 10 variables
#' 
#' @usage
#' data('nsw_dw')
#' 
#' @examples
#' data('nsw_dw')
NULL

#' Lalonde Experimental Data (Dehejia and Wahba 1999 Subset)
#' 
#' Data from the National Supported Work Demonstration, a job training program
#' studied by Lalonde (1986). The dataset includes both treated and control
#' groups, with various demographic and earnings characteristics. This extract
#' was created Lalonde and distributed by Dehejia and Wahba (1999).
#' 
#' @name psid_controls
#' 
#' @description
#' - `treat`: Treatment assignment status (1 for treated, 0 for control)
#' - `age`: Age of the individual
#' - `education`: Years of education completed
#' - `black`: Indicator for whether the individual is black
#' - `hispanic`: Indicator for whether the individual is Hispanic
#' - `married`: Indicator for whether the individual is married
#' - `nodegree`: Indicator for whether the individual has no degree
#' - `re74`: Earnings in 1974
#' - `re75`: Earnings in 1975
#' - `re78`: Earnings in 1978 (outcome variable)
#' 
#' @references
#' "Causal Effects in Non-Experimental Studies: Reevaluating the Evaluation of 
#' Training Programs," Journal of the American Statistical Association, Vol. 94, 
#' No. 448 (December 1999), pp. 1053-1062.
#' 
#' "Propensity Score Matching Methods for Non-Experimental Causal Studies," 
#' Review of Economics and Statistics, Vol. 84, (February 2002), pp. 151-161.
#' 
#' @return A data frame with 2490 observations and 10 variables
#' 
#' @usage
#' data('psid_controls')
#' 
#' @examples
#' data('psid_controls')
NULL
