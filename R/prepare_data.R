#' Prepares raw data for later processing
#' @param all_sleep_dat  sleep-levels dataset. Only the person_id and start_time are needed, but the full dataset is usually passed in containing \cr
#' person_id, sleep_date, start_datetime, levels, duration_in_min, and is_main_sleep.
prepare_data <- function(all_sleep_dat)
{
    setnames(all_sleep_dat,"start_datetime","start_time")
    setnames(all_sleep_dat,"duration_in_min","duration")
    setnames(all_sleep_dat,"sleep_date","date")
    all_sleep_dat[, end_time := start_time + lubridate::seconds(duration * 60)]

    # If the end time overlaps the next start time, clip it and adjust the duration and start time.
    # This is rare. 
    setkey(all_sleep_dat, person_id, start_time, end_time)
    all_sleep_dat[,start_time_lag := shift(start_time,-1),.(person_id)]
    all_sleep_dat[end_time > start_time_lag, duration := as.numeric(start_time_lag - start_time) /60]
    all_sleep_dat[end_time > start_time_lag, end_time := start_time_lag]

    # Additional help variables that will be used later to compute the MSP
    setkey(all_sleep_dat, person_id, start_time, end_time)
    all_sleep_dat[, sleep_start_new := center(time_to_minute(start_time))]
    all_sleep_dat[, sleep_end_new := center(time_to_minute(end_time))]
    return(all_sleep_dat)
}