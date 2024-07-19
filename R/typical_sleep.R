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
typical_sleep <- function(sleep_data) UseMethod("typical_sleep",sleep_data)

#' @export 
typical_sleep.sleep_logs <- function(sleep_data)
{
  dt <- sleep_data$sleep_data
  dt <- find_relevant_sleep(dt)
  dt <- run_tsp(dt)
  # Return column names to original
  setnames(dt,"date_new","typical_sleep_date")
  algo_data <- dt[,c("person_id","median_msp","median_sleep_start","median_sleep_end")]
  algo_data <- algo_data[!duplicated(algo_data)]
  dt <- dt[,c("person_id","sleep_date","start_datetime","level","duration_in_min",
  "is_main_sleep","sleep_log","start_datetime_log","end_time_log","typical_sleep_date","is_typical_sleep")]
  dt <- structure(list(sleep_data=dt,algorithm_data=algo_data),class="typical_sleep")
  return(dt)
}

#' Typical sleep algorithm
#' @param all_sleep_dat sleep-levels dataset containing the following columns: person_id, sleep_date, start_datetime, level, duration_in_min, and is_main_sleep.
#' @noRd
run_tsp <- function(all_sleep_dat)
{
    setkey(all_sleep_dat, person_id, start_datetime)
    first_last_asleep <- all_sleep_dat[ level != "awake" & level != "wake" & level != "restless" & msp == TRUE,
                                    .(start_datetime_log = start_datetime_log[1],
                                      end_time_log = end_time_log[.N]),
                                    .(person_id,date_new)]
    first_last_asleep[, first_asleep_minute := time_to_minute(start_datetime_log)]
    first_last_asleep[, last_asleep_minute := time_to_minute(end_time_log)]
    first_last_asleep[, last_asleep_minute_new := center(last_asleep_minute)]
    first_last_asleep[, first_asleep_minute_new := center(first_asleep_minute)]
    first_last_asleep[, median_sleep_start := median(first_asleep_minute_new),.(person_id)]
    first_last_asleep[, median_sleep_end := median(last_asleep_minute_new),.(person_id)]
    first_last_asleep[, msp := (first_asleep_minute_new + last_asleep_minute_new) / 2]
    first_last_asleep[, median_msp := median(msp),.(person_id)]
    first_last_asleep_ranges <- first_last_asleep[, c("person_id","median_sleep_start","median_sleep_end","start_datetime_log","median_msp")]
    first_last_asleep_ranges <- first_last_asleep_ranges[!duplicated(first_last_asleep_ranges)]
    setkey(all_sleep_dat, person_id, start_datetime_log, end_time_log)
    first_last_asleep_ranges[, bt_wt_by_msp := median_sleep_end - median_sleep_start <= 300]
    first_last_asleep_ranges[bt_wt_by_msp == TRUE, median_sleep_start := median_msp - 3.5*60]
    first_last_asleep_ranges[bt_wt_by_msp == TRUE, median_sleep_end := median_msp + 3.5*60]
    first_last_asleep_ranges[, tsp_median_sleep_start := median_sleep_start]
    first_last_asleep_ranges[, tsp_median_sleep_end := median_sleep_end]
    first_last_asleep_ranges[, median_sleep_start1 := lubridate::ymd(lubridate::as_date(start_datetime_log)) + lubridate::seconds(median_sleep_start*60)]
    first_last_asleep_ranges[, median_sleep_end1 := lubridate::ymd(lubridate::as_date(start_datetime_log)) + lubridate::seconds(median_sleep_end*60)]
    first_last_asleep_ranges[, median_sleep_start2 := median_sleep_start1 + lubridate::days(1)]
    first_last_asleep_ranges[, median_sleep_end2 := median_sleep_end1 + lubridate::days(1)]
    first_last_asleep_ranges[, median_sleep_start3 := median_sleep_start1 - lubridate::days(1)]
    first_last_asleep_ranges[, median_sleep_end3 := median_sleep_end1 - lubridate::days(1)]
    setkey(all_sleep_dat, person_id, start_datetime_log, end_time_log)
    dt_overlaps <- rbindlist(lapply(1:3,function(x)
                    find_overlaps(all_sleep_dat,
                                first_last_asleep_ranges, 
                                paste0("median_sleep_start",x),
                                paste0("median_sleep_end",x))))
    dt_overlaps <- dt_overlaps[!duplicated(dt_overlaps[,c("xid")])]
    all_sleep_dat[, is_typical_sleep := FALSE]
    all_sleep_dat[dt_overlaps$xid, is_typical_sleep := TRUE]
    all_sleep_dat[dt_overlaps$xid, date_new := lubridate::as_date(dt_overlaps$date)]
    all_sleep_dat <- insert_wake_levels(all_sleep_dat)
    median_msp_dt <- first_last_asleep_ranges[,c("person_id","median_msp")]
    median_msp_dt <- median_msp_dt[!duplicated(person_id)]
    median_msp_dt[, median_msp := hms::as_hms(uncenter(median_msp) * 60)]
    median_bt_wt <- first_last_asleep_ranges[,c("person_id","tsp_median_sleep_start","tsp_median_sleep_end")]
    median_bt_wt <- median_bt_wt[!duplicated(person_id)]
    data.table::setnames(median_bt_wt,"tsp_median_sleep_start","median_sleep_start")
    data.table::setnames(median_bt_wt,"tsp_median_sleep_end","median_sleep_end")
    median_bt_wt[, median_sleep_start := hms::as_hms(uncenter(median_sleep_start) * 60)]
    median_bt_wt[, median_sleep_end := hms::as_hms(uncenter(median_sleep_end) * 60)]
    all_sleep_dat <- merge(all_sleep_dat,median_msp_dt,by="person_id")
    all_sleep_dat <- merge(all_sleep_dat,median_bt_wt,by="person_id")
    return(all_sleep_dat)
}
