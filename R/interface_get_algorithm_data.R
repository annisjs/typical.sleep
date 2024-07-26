#' Compute nap metrics for each person_id and date.
#' @param sleep_data an object returned from as_sleep_logs or typical_sleep
#' @return A dataframe with the following columns (by person_id and date):
#' \describe{ 
#'  \item{nap_count}{Number of sleep logs outside of primary sleep period}
#'  \item{nap_length}{Total duration of sleep across sleep logs outside of the primary sleep period}
#' }
#' @export
#' @examples
#'\dontrun{
#' # If parsing from JSON format
#' dat <- parse_fitbit_json("sleep_data.json")
#' dat_logs <- as_sleep_logs(dat)
#' tsp <- get_typical_sleep(dat_logs)
#' get_algorithm_data(tsp)
#'}
#' 
get_algorithm_data <- function(sleep_data) UseMethod("get_algorithm_data",sleep_data)

#' @export
get_algorithm_data.typical_sleep <- function(sleep_data)
{
  dt <- sleep_data$algoritm_data
  return(dt)
}