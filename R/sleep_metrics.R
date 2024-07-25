#' Compute sleep metrics for each person_id and date.
#' @param sleep_data an object returned from as_sleep_logs or typical_sleep
#' @return A dataframe with the following columns (by person_id and date):
#' \describe{
#'   \item{sleep_onset}{The start datetime of the first sleep segment, where sleep segment levels are not wake, awake, or restless.}
#'   \item{sleep_offset}{The end datetime of the last sleep segment, where sleep segment levels are not wake, awake, or restless. The end datetime is computed by adding sleep duration to start_datetime.}
#'   \item{sleep_duration}{Duration of sleep in minutes. (sleep offset - sleep onset) / 60}
#'   \item{midsleep_point}{Midpoint between sleep_onset and sleep_offset. (sleep onset + sleep offset) / 2}
#'   \item{total_sleep_time}{Sum of all sleep segment durations, where level is not awake, wake, or restless.}
#'   \item{rem_duration}{Sum of all sleep segment durations, where level is rem.}
#'   \item{deep_duration}{Sum of all sleep segment durations, where level is deep.}
#'   \item{light_duration}{Sum of all sleep segment durations, where level is light.}
#'   \item{pct_rem}{Percentage of rem sleep duration. Denominator is the sum of all sleep segment durations whose level is not awake, wake, or restless. If no rem levels exist, then pct_rem is NA.}
#'   \item{pct_deep}{Percentage of deep sleep duration. Denominator is the sum of all sleep segment durations whose level is not awake, wake, or restless. If no deep levels exist, then pct_deep is NA.}
#'   \item{pct_light}{Percentage of light sleep duration. Denominator is the sum of all sleep segment durations whose level is not awake, wake, or restless. If no light levels exist, then pct_light is NA.}
#'   \item{imputed_awake_duration}{Total duration of all sleep segments whose level is imputed_awake.}
#'   \item{awake_duration}{Total duration of all sleep segments whose level is awake.}
#'   \item{wake_duration}{Total duration of all sleep segments whose level is wake.}
#'   \item{restless_duration}{Total duration of all sleep segments whose level is restless.}
#'   \item{bedtime}{start datetime of first sleep log.}
#'   \item{waketime}{The end datetime of the final sleep log.}
#'   \item{time_in_bed}{Time in bed in minutes. (bedtime - waketime) / 60.}
#'   \item{num_awakenings}{Number of contiguous sleep segments indcating an awakening. Segments of differing levels will be combined to form a single contiguous sleep segment given the level is one of awake, wake, or restless.}
#'   \item{num_long_awakenings}{Count of all contiguous wake, awake, and restless sleep segments >=30 minutes in duration between sleep onset and sleep offset.}
#'   \item{longest_wake_duration}{Duration of the longest contiguous period spent awake after sleep onset}
#'   \item{wake_after_sleep_onset}{Total duration of all ‘wake’, ‘awake’ and ‘restless’ sleep segments between sleep onset and sleep offset}
#'   \item{wake_to_end_of_log_latency}{Time difference in hours between the final awakening to wake time}
#'   \item{sleep_efficiency}{Total time spent asleep as a proportion of the primary sleep period. 100 * total_sleep_time / time_in_bed}
#'   \item{nap_count}{Number of sleep logs outside of primary sleep period}
#'   \item{nap_length}{Total duration of sleep across sleep logs outside of the primary sleep period}
#'   \item{sleep_onset_latency}{Time between bedtime and sleep onset}
#'   \item{total_sleep_time_24h}{Total duration of sleep for a given calendar date.}
#' }
#'@export
#' 
#'@examples
#'\dontrun{
#' # If parsing from JSON format
#' dat <- parse_fitbit_json("sleep_data.json")
#' metrics <- compute_sleep_metrics(dat,"main_sleep")
#' print(metrics)
#' tsp_dat <- typical_sleep(dat)
#' tsp_metrics <- compute_sleep_metrics(tsp_dat,"typical_sleep")
#' print(tsp_metrics)
#'}
#' 
get_sleep_metrics <- function(sleep_data) UseMethod("get_sleep_metrics",sleep_data)

#' @export
get_sleep_metrics.sleep_logs <- function(sleep_data)
{
  dt <- sleep_data$sleep_data
  date_col <- "sleep_date"
  nap_agg <- get_naps(dt,date_col,"is_main_sleep")
  dt <- dt[is_main_sleep == TRUE]
  metrics <- get_metrics(dt,date_col)
  metrics <- add_naps(metrics,nap_agg,date_col)
  return(metrics)
}

#' @export
get_sleep_metrics.typical_sleep <- function(sleep_data)
{
  dt <- sleep_data$sleep_data
  nap_agg <- get_naps(dt,"sleep_date","is_typical_sleep")
  dt <- dt[is_typical_sleep == TRUE]
  metrics <- get_metrics(dt,"typical_sleep_date")
  nap_agg[, typical_sleep_date := sleep_date]
  metrics <- add_naps(metrics,nap_agg,"typical_sleep_date")
  return(metrics)
}

#' Function for merging aggregated nap data with metrics. Not exported.
#' @param out metric output from get_metrics
#' @param nap_agg output from get_naps
#' @param date_col typical_sleep_date or sleep_date
#' @noRd
add_naps <- function(out,nap_agg,date_col)
{
  # Add naps
  out <- merge(out,nap_agg,by=c("person_id",date_col),all.x=T)
  out[, nap_count := fifelse(is.na(nap_count),0,nap_count)]
  out[, nap_length := fifelse(is.na(nap_length),0,nap_length)]
  return(out)
}
