#' Typical sleep period
#' @param sleep_data sleep-levels dataset containing the following columns: person_id, sleep_date, start_datetime, level, duration_in_min, and is_main_sleep.
#' This is the default schema from the All of Us sleep_levels table.
#' @import data.table
#' @return Returns the original data with two appended columns, sleep_date_new and tsp. sleep_date_new is the recomputed date of sleep based on the typical sleep period. \cr
#' tsp is TRUE if the sleep log falls within the typical sleep period. Otherwise, it is false.
#' @description The typical sleep period is computed in three steps: 
#'    1. Finds relevant sleep logs are those that fall within +/-8 hours of the midsleep point. 
#'    2. Determines the median bedtime and waketime from the relevant logs.
#'    3. Labels each sleep log as either part of the typical sleep period or not by filtering out those
#'    that are outside of the bedtime/waketime interval.
#' @export
#' @import data.table
typical_sleep <- function(sleep_data)
{
  if (is.null(attr(sleep_data,"format")))
  {
    stop("Not a sleep_logs object. Run as_sleep_logs() on dataset first.")
  }
  sleep_data <- find_relevant_sleep(sleep_data)
  sleep_data <- .tsp(sleep_data)
  # Return column names to original
  setnames(sleep_data,"date_new","typical_sleep_date")
  sleep_data <- sleep_data[,c("person_id","sleep_date","start_datetime","level","duration_in_min",
  "is_main_sleep","sleep_log","start_datetime_log","end_time_log","typical_sleep_date","is_typical_sleep")]
  setattr(sleep_data,"sleep_type","typical")
  setattr(sleep_data,"format","log")
  return(sleep_data)
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
    first_last_asleep_ranges <- first_last_asleep[, c("person_id","median_sleep_start","median_sleep_end","start_datetime_log")]
    first_last_asleep_ranges <- first_last_asleep_ranges[!duplicated(first_last_asleep_ranges)]
    # Key by sleep log
    setkey(all_sleep_dat, person_id, start_datetime_log, end_time_log)
    all_sleep_dat[, sleep_start_new := center(time_to_minute(start_datetime_log))]
    all_sleep_dat[, sleep_end_new := center(time_to_minute(end_time_log))]
    all_sleep_dat[, median_msp_new := median((sleep_start_new + sleep_end_new) / 2),.(person_id)]
    first_last_asleep_ranges[, bt_wt_by_msp := median_sleep_end - median_sleep_start <= 300]
    first_last_asleep_ranges[bt_wt_by_msp == TRUE, median_sleep_start := median_sleep_start - 3.5*60]
    first_last_asleep_ranges[bt_wt_by_msp == TRUE, median_sleep_end := median_sleep_end + 3.5*60]
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