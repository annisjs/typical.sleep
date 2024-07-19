#' Find relevant sleep logs
#' @param all_sleep_dat sleep-levels dataset containing the following columns: person_id, date, start_time, level, duration, and is_main_sleep.
#' @description finds relevant sleep logs by computing the median sleep point when is_main_sleep is TRUE and filtering out sleep logs that are outside of that interval.
#' @noRd
find_relevant_sleep <- function(all_sleep_dat)
{
    # Use the median MSP to look for all relevant sleep
    # Median MSP ± 8hrs
    # Anything intersecting/overlapping with this range
    # Get all main sleep start and end times
    setkey(all_sleep_dat, person_id, start_datetime)
    first_last_asleep <- all_sleep_dat[is_main_sleep == TRUE & level != "awake" & level != "wake" & level != "restless", 
                                    .(start_datetime = start_datetime[1],
                                      end_time = end_time[.N]),
                                    .(person_id,sleep_date)]
    # Center over midnight (i.e. noon-to-noon)
    first_last_asleep[, first_asleep_minute := time_to_minute(start_datetime)]
    first_last_asleep[, last_asleep_minute := time_to_minute(end_time)]
    first_last_asleep[, last_asleep_minute_new := center(last_asleep_minute)]
    first_last_asleep[, first_asleep_minute_new := center(first_asleep_minute)]
    # Compute the MSP
    first_last_asleep[, msp := (first_asleep_minute_new + last_asleep_minute_new) / 2]
    first_last_asleep[, median_msp := median(msp),.(person_id)]
    # Recenter over noon (i.e. midnight to midnight), so we can convert to timestamps
    first_last_asleep[, median_msp := uncenter(median_msp)]
    first_last_asleep[, median_msp := hms::as_hms(median_msp * 60)]
    # Setup ranges
    # Relevant sleep logs fall within +/- 8 lubridate::hours of the median msp
    first_last_asleep[, median_msp_date := lubridate::ymd(lubridate::as_date(start_datetime)) + lubridate::hms(median_msp)]
    first_last_asleep[, range_begin1 := median_msp_date - lubridate::hours(8)]
    first_last_asleep[, range_end1 := median_msp_date + lubridate::hours(8)]
    # Additionally, create ranges that start prior to and after the initial MSP
    # This will be used to capture all relevant sleep logs and assign the corresponding date
    first_last_asleep[, range_begin2 := range_begin1 + lubridate::days(1)]
    first_last_asleep[, range_end2 := range_end1 + lubridate::days(1)]
    first_last_asleep[, range_begin3 := range_begin1 - lubridate::days(1)]
    first_last_asleep[, range_end3 := range_end1 - lubridate::days(1)]
    # Get the range data computed from the MSP
    first_last_asleep_ranges <- first_last_asleep[, c("person_id","range_begin1","range_end1",
                                                    "range_begin2","range_end2",
                                                    "range_begin3","range_end3")]
    first_last_asleep_ranges <- first_last_asleep_ranges[!duplicated(first_last_asleep_ranges)]
    # Find all overlapping sleep logs within +/- 8 lubridate::hours of the MSP
    setkey(all_sleep_dat, person_id, start_datetime_log, end_time_log)
    dt_overlaps <- rbindlist(lapply(1:3,function(x)
                    find_overlaps(all_sleep_dat,
                                  first_last_asleep_ranges, 
                                  paste0("range_begin",x),
                                  paste0("range_end",x))))
    dt_overlaps <- dt_overlaps[!duplicated(dt_overlaps[,c("xid")])]
    # Keep relevant sleep logs overlapping with MSP +/-8 lubridate::hours
    all_sleep_dat[, msp := FALSE]
    all_sleep_dat[dt_overlaps$xid, msp := TRUE]
    all_sleep_dat[dt_overlaps$xid, date_new := lubridate::as_date(dt_overlaps$date)]
    return(all_sleep_dat)
}