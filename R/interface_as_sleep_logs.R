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
    dt <- all_sleep_dat
    dt[, end_time := start_datetime + lubridate::seconds(duration_in_min * 60)]
    # If the end time overlaps the next start time, clip it and adjust the duration and start time.
    # This is rare. 
    setkey(dt, person_id, start_datetime, end_time)
    dt[,start_time_lag := shift(start_datetime,-1),.(person_id)]
    dt[end_time > start_time_lag, duration_in_min := as.numeric(start_time_lag - start_datetime) /60]
    dt[end_time > start_time_lag, end_time := start_time_lag]
    # Find sleep logs
    dt[, end_datetime_lead := shift(end_time,1),.(person_id)]
    dt[, diff := as.numeric(start_datetime - end_datetime_lead) / 60,.(person_id)]
    dt[, sleep_log := cumsum(diff >= 1 | is.na(diff))]
    dt[, start_datetime_log := start_datetime[1],.(person_id,sleep_log)]
    dt[, end_time_log := end_time[.N],.(person_id,sleep_log)]
    dt[, end_datetime_lead := NULL]
    dt[, start_time_lag := NULL]
    dt[, diff := NULL]
    staged_level <- c("wake","rem","deep","light")
    classic_level <- c("restless","awake","asleep")
    dt[, hybrid_log_flag := any(level %in% staged_level) & any(level %in% classic_level), .(person_id,sleep_log)]
    dt <- structure(list(sleep_data=dt),class="sleep_logs")
    return(dt)
}