#' Compute algorithm metrics
#' @param sleep_data an object returned from as_sleep_logs or typical_sleep
#' @return A dataframe with the following columns:
#' \describe{ 
#'  \item{person_id}{person ID number}
#'  \item{median_msp}{Median of the median sleep period}
#'  \item{median_sleep_start}{Median sleep start}
#'  \item{median_sleep_end}{Median sleep end}
#'  \item{bt_wt_by_msp}{Indicates whether the bedtime and waketime were determined by MSP +/- 3.5 hours. This happens when the median BT > median WT}
#'  \item{bedtime_gt_waketime}{Indicates whether the median BT > median WT} 
#' }
#' @export
#' 
get_algorithm_data <- function(sleep_data) UseMethod("get_algorithm_data",sleep_data)

#' @export
get_algorithm_data.typical_sleep <- function(sleep_data)
{
  dt <- sleep_data$algorithm_data
  return(dt)
}