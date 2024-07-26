#' Function for computing 24hr sleep duration. Not exported.
#' @param all_sleep_dat Dataset containing sleep segments. Must contain person_id, is_main_sleep, date_col, level, and start_datetime.
#' @return dataframe with person_id, date_col, nap_count, and nap_length
#' @noRd 
get_tst_24hr <- function(all_sleep_dat){
    dt <- all_sleep_dat[!level %in% AWAKE_LEVELS()]
    dt[, end := start_datetime + lubridate::seconds(duration_in_min * 60)]
    dt[, start_date := lubridate::as_date(start_datetime)]
    dt[, end_date :=  lubridate::as_date(end)]
    dt[, start_datetime_temp := lubridate::as_datetime(paste0(start_date," 23:59:59"))]
    dt[, end_datetime_temp := lubridate::as_datetime(paste0(end_date," 00:00:00"))]
    d1 <- dt[start_date == end_date, .(duration = duration_in_min),.(start_date,person_id)]
    d2 <- dt[start_date != end_date, .(duration =  as.numeric(start_datetime_temp - start_datetime) / 60),.(start_date,person_id)]
    d3 <- dt[start_date != end_date, .(duration = as.numeric(end - end_datetime_temp) / 60),.(end_date,person_id)]
    colnames(d3)[1] <- "start_date"
    d4 <- rbindlist(list(d1,d2,d3))
    d_agg <- d4[,.(total_sleep_time_24h = sum(duration)),.(start_date,person_id)] 
    return(d_agg)
}
