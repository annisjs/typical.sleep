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
    # Order violating edge-cases
    # 1. Noon-crossing
    #     12PM ==x2_obs==== 12AM ========x1====== 12PM =====x2_act======= 12AM
    #     ------------------- +++++++++++++++++++++ ----------------------- 
    # 2. Hypersomnia cases
    #     1a.
    #      12PM ==x2_obs==x1==== 12AM ============== 12PM ======x2_act==== 12AM
    #      ----------------------- ++++++++++++++++++++ ----------------------
    #     1b.
    #      12PM =========== 12AM =====x2_obs==x1===== 12PM ========== 12AM =====x2_act==== 12PM 
    #      ------------------ +++++++++++++++++++++++++ --------------- +++++++++++++++++++++
    #  Correction: If datetime(x2_obs) - datetime(x1) != numeric(x2_obs) - numeric(x1), then x2_obs + 1440 = x2_act
    # Order preserving edge-cases
    # 1. Hypersomnia
    #   1a. 
    #     12PM ==x1===x2_obs=== 12AM ============== 12PM =====x2_act===== 12AM
    #     ----------------------- +++++++++++++++++++ ----------------------
    #   2b. 
    #     12PM ======== 12AM =x1====x2_obs== 12PM ============ 12AM ====x2_act=== 12PM
    #     --------------- +++++++++++++++++++++ ---------------- +++++++++++++++++++
    #   2c.
    #     12PM ====x1== 12AM =======x2_obs== 12PM =========== 12AM ======x2_act== 12PM
    #     --------------- ++++++++++++++++++++++++ ------------- +++++++++++++++++++
    # Correction: If datetime(x2_obs) - datetime(x1) != numeric(x2_obs) - numeric(x1), then x2_obs + 1440 = x2_act
    first_last_asleep[, diff_time := last_asleep_minute_new - first_asleep_minute_new]
    first_last_asleep[, diff_datetime := lubridate::interval(
                                         lubridate::as_datetime(start_datetime),
                                         lubridate::as_datetime(end_time))/lubridate::minutes(1)]
    first_last_asleep[diff_time != diff_datetime, last_asleep_minute_new := last_asleep_minute_new + 1440]
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
    fl_melt1 <- first_last_asleep[,c("person_id","range_begin1","range_end1")]
    fl_melt2 <- first_last_asleep[,c("person_id","range_begin2","range_end2")]
    fl_melt3 <- first_last_asleep[,c("person_id","range_begin3","range_end3")]
    colnames(fl_melt1) <- c("person_id","range_begin","range_end")
    colnames(fl_melt2) <- c("person_id","range_begin","range_end")
    colnames(fl_melt3) <- c("person_id","range_begin","range_end")
    first_last_asleep_ranges <- rbindlist(list(fl_melt1,fl_melt2,fl_melt3))
    first_last_asleep_ranges <- first_last_asleep_ranges[!duplicated(first_last_asleep_ranges)]
    # Find all overlapping sleep logs within +/- 8 lubridate::hours of the MSP
    setkey(all_sleep_dat, person_id, start_datetime_log, end_time_log)
    dt_overlaps <- find_overlaps(all_sleep_dat,first_last_asleep_ranges,"range_begin","range_end")
    dt_overlaps <- dt_overlaps[!duplicated(dt_overlaps[,c("xid")])]
    # Keep relevant sleep logs overlapping with MSP +/-8 lubridate::hours
    all_sleep_dat[, msp := FALSE]
    all_sleep_dat[dt_overlaps$xid, msp := TRUE]
    all_sleep_dat[dt_overlaps$xid, date_new := lubridate::as_date(dt_overlaps$date)]
    return(all_sleep_dat)
}