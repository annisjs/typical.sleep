#' Prepares raw data for later processing
#' @param all_sleep_dat  sleep-levels dataset. Only the person_id and start_datetime are needed, but the full dataset is usually passed in containing \cr
#' person_id, sleep_date, start_datetime, levels, duration_in_min, and is_main_sleep.
#' @export
#' @import data.table
as_sleep_logs <- function(all_sleep_dat)
{
    cols <- c("person_id","sleep_date","start_datetime","level","duration_in_min","is_main_sleep")
    not_in <- !cols %in% colnames(all_sleep_dat)
    if (any(not_in==TRUE)) {
        cols_not_in <- cols[not_in]
        stop(paste0("Missing columns:",cols_not_in))
    }
    all_sleep_dat[, end_time := start_datetime + lubridate::seconds(duration_in_min * 60)]
    # If the end time overlaps the next start time, clip it and adjust the duration and start time.
    # This is rare. 
    setkey(all_sleep_dat, person_id, start_datetime, end_time)
    all_sleep_dat[,start_time_lag := shift(start_datetime,-1),.(person_id)]
    all_sleep_dat[end_time > start_time_lag, duration_in_min := as.numeric(start_time_lag - start_datetime) /60]
    all_sleep_dat[end_time > start_time_lag, end_time := start_time_lag]
    # Find sleep logs
    all_sleep_dat[, end_datetime_lead := shift(end_time,1),.(person_id)]
    all_sleep_dat[, diff := as.numeric(start_datetime - end_datetime_lead) / 60,.(person_id)]
    all_sleep_dat[, sleep_log := cumsum(diff >= 1 | is.na(diff))]
    all_sleep_dat[, start_datetime_log := start_datetime[1],.(person_id,sleep_log)]
    all_sleep_dat[, end_time_log := end_time[.N],.(person_id,sleep_log)]
    all_sleep_dat[, end_datetime_lead := NULL]
    all_sleep_dat[, start_time_lag := NULL]
    all_sleep_dat[, diff := NULL]
    setattr(all_sleep_dat,"format","log")
    return(all_sleep_dat)
}