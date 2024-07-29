#' Typical sleep period
#' @param sleep_data sleep-levels dataset containing the following columns: person_id, sleep_date, start_datetime, level, duration_in_min, and is_main_sleep.
#' This is the default schema from the All of Us sleep_levels table.
#' @import data.table
#' @return a typical sleep object
#' @description The typical sleep algorithm returns a typical sleep object that contains the original data plus inserted awake levels and additioanl columns.
#' The additional columns are the result of the typical sleep algorithm: typical_sleep_date, is_typical_sleep, median_msp, median_sleep_start, median_sleep_end 
#'
#' @export
#' @import data.table
as_typical_sleep <- function(sleep_data) UseMethod("as_typical_sleep",sleep_data)

#' @export 
as_typical_sleep.sleep_logs <- function(sleep_data)
{
  dt <- sleep_data$sleep_data
  dt <- filter_relevant_sleep(dt)
  dt <- filter_tsp(dt)
  # Return column names to original
  setnames(dt,"date_tsp","typical_sleep_date")
  setnames(dt,"date_new","relevant_log_date")
  setnames(dt,"date_nap","nap_date")
  algo_data <- dt[,c("person_id","median_msp","median_sleep_start","median_sleep_end","bt_wt_by_msp","bedtime_gt_waketime")]
  algo_data <- algo_data[!duplicated(algo_data)]
  algo_data[, median_msp := hms::as_hms(median_msp)]
  algo_data[, median_sleep_start := hms::as_hms(median_sleep_start)]
  algo_data[, median_sleep_end := hms::as_hms(median_sleep_end)]
  dt <- dt[,c("person_id","sleep_date","start_datetime","level","duration_in_min",
              "is_main_sleep","sleep_log","start_datetime_log","end_time_log","typical_sleep_date","is_typical_sleep",
              "nap_date","is_nap")]
  dt <- structure(list(sleep_data=dt,algorithm_data=algo_data),class="typical_sleep")
  return(dt)
}

