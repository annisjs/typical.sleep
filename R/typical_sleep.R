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
  dt <- .tsp(dt)
  # Return column names to original
  setnames(dt,"date_new","typical_sleep_date")
  algo_data <- dt[,c("person_id","typical_sleep_date","median_msp","median_sleep_start","median_sleep_end")]
  dt <- dt[,c("person_id","sleep_date","start_datetime","level","duration_in_min",
  "is_main_sleep","sleep_log","start_datetime_log","end_time_log","typical_sleep_date","is_typical_sleep")]
  dt <- structure(list(sleep_data=dt,algorithm_data=algo_data),class="typical_sleep")
  return(dt)
}

#' Typical sleep algorithm
#' @param all_sleep_dat sleep-levels dataset containing the following columns: person_id, sleep_date, start_datetime, level, duration_in_min, and is_main_sleep.
#' @noRd
.tsp <- function(all_sleep_dat)
{
    setkey(all_sleep_dat, person_id, start_datetime)
    # all_sleep_dat now only contains relevant sleep logs from the MSP.
    # The next step is to estimate the median bedtime and waketime from relevant sleeps. 
    first_last_asleep <- all_sleep_dat[ level != "awake" & level != "wake" & level != "restless" & msp == TRUE,
                                    .(start_datetime_log = start_datetime_log[1],
                                      end_time_log = end_time_log[.N]),
                                    .(person_id,date_new)]
    # Center over midnight
    first_last_asleep[, first_asleep_minute := time_to_minute(start_datetime_log)]
    first_last_asleep[, last_asleep_minute := time_to_minute(end_time_log)]
    first_last_asleep[, last_asleep_minute_new := center(last_asleep_minute)]
    first_last_asleep[, first_asleep_minute_new := center(first_asleep_minute)]
    # Compute the median bedtime and waketime
    first_last_asleep[, median_sleep_start := median(first_asleep_minute_new),.(person_id)]
    first_last_asleep[, median_sleep_end := median(last_asleep_minute_new),.(person_id)]
    first_last_asleep[, msp := (first_asleep_minute_new + last_asleep_minute_new) / 2]
    first_last_asleep[, median_msp := median(msp),.(person_id)]
    # Any sleep log with a BT or WT between "True BT" and "True WT"
    first_last_asleep_ranges <- first_last_asleep[, c("person_id","median_sleep_start","median_sleep_end","start_datetime_log","median_msp")]
    first_last_asleep_ranges <- first_last_asleep_ranges[!duplicated(first_last_asleep_ranges)]
    # Key by sleep log
    setkey(all_sleep_dat, person_id, start_datetime_log, end_time_log)
    first_last_asleep_ranges[, bt_wt_by_msp := median_sleep_end - median_sleep_start <= 300]
    first_last_asleep_ranges[bt_wt_by_msp == TRUE, median_sleep_start := median_msp - 3.5*60]
    first_last_asleep_ranges[bt_wt_by_msp == TRUE, median_sleep_end := median_msp + 3.5*60]
    first_last_asleep_ranges[, tsp_median_sleep_start := median_sleep_start]
    first_last_asleep_ranges[, tsp_median_sleep_end := median_sleep_end]
    # In order to capture these days, we need to add 1 day to the ranges
    first_last_asleep_ranges[, median_sleep_start := lubridate::ymd(lubridate::as_date(start_datetime_log)) + lubridate::seconds(median_sleep_start*60)]
    first_last_asleep_ranges[, median_sleep_end := lubridate::ymd(lubridate::as_date(start_datetime_log)) + lubridate::seconds(median_sleep_end*60)]
    first_last_asleep_ranges[, median_sleep_start2 := median_sleep_start + lubridate::days(1)]
    first_last_asleep_ranges[, median_sleep_end2 := median_sleep_end + lubridate::days(1)]
    first_last_asleep_ranges[, median_sleep_start3 := median_sleep_start - lubridate::days(1)]
    first_last_asleep_ranges[, median_sleep_end3 := median_sleep_end - lubridate::days(1)]
    # Now that we've taken care of the special cases, find all logs that overlap or are within the TSP.
    setkey(first_last_asleep_ranges, person_id, median_sleep_start, median_sleep_end)
    setkey(all_sleep_dat, person_id, start_datetime_log, end_time_log)
    dt_overlaps <- foverlaps(all_sleep_dat, first_last_asleep_ranges, type = "any", nomatch = NULL, which = TRUE)
    dt_overlaps[, date := lubridate::as_datetime(first_last_asleep_ranges$median_sleep_end[dt_overlaps$yid])]
    setkey(first_last_asleep_ranges, person_id, median_sleep_start2, median_sleep_end2)
    dt_overlaps2 <- foverlaps(all_sleep_dat, first_last_asleep_ranges, type = "any", nomatch = NULL, which = TRUE)
    dt_overlaps2[, date := lubridate::as_datetime(first_last_asleep_ranges$median_sleep_end2[dt_overlaps2$yid])]
    setkey(first_last_asleep_ranges, person_id, median_sleep_start3, median_sleep_end3)
    dt_overlaps3 <- foverlaps(all_sleep_dat, first_last_asleep_ranges, type = "any", nomatch = NULL, which = TRUE)
    dt_overlaps3[, date := lubridate::as_datetime(first_last_asleep_ranges$median_sleep_end3[dt_overlaps3$yid])]
    dt_overlaps <- dt_overlaps[,c("xid","date")]
    dt_overlaps2 <- dt_overlaps2[, c("xid","date")]
    dt_overlaps3 <- dt_overlaps3[, c("xid","date")]
    dt_overlaps <- rbind(dt_overlaps,dt_overlaps2,dt_overlaps3)
    dt_overlaps <- dt_overlaps[!duplicated(dt_overlaps[,c("xid")])]
    all_sleep_dat[, is_typical_sleep := FALSE]
    all_sleep_dat[dt_overlaps$xid, is_typical_sleep := TRUE]
    all_sleep_dat[dt_overlaps$xid, date_new := lubridate::as_date(dt_overlaps$date)]
    all_sleep_dat <- insert_wakes(all_sleep_dat)
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

#' Insert wake levels between stitched logs
#' @param all_sleep_dat dataframe containing the following columns: person_id, date_new, start_datetime, end_time.
#' @noRd 
insert_wakes <- function(all_sleep_dat)
{
    setkey(all_sleep_dat, person_id, start_datetime)
    # Insert wakes
    # Add wake between levels
    # Create variable that holds end_time i - 1
    all_sleep_dat[, end_time_lead_day := shift(end_time,1),.(person_id,date_new)]
    # Get the difference between start_time and end_time i - 1
    all_sleep_dat[, lead_diff := as.numeric(start_datetime - end_time_lead_day) / 60]
    # Differences > 0 indicate a gap in the sleep logs
    # Get all gaps
    wake_between <- all_sleep_dat[lead_diff > 0]
    # Relabel the variables to create a new wake sleep logs
    wake_between[, end_time := start_datetime]
    wake_between[, start_datetime := end_time_lead_day]
    wake_between[, duration_in_min := lead_diff]
    wake_between[, level := "imputed_awake"]
    # Cleanup
    wake_between[, end_time_lead_day := NULL]
    wake_between[, lead_diff := NULL]
    all_sleep_dat <- rbind(all_sleep_dat,wake_between,fill=TRUE)
    setkey(all_sleep_dat,person_id,start_datetime)
}