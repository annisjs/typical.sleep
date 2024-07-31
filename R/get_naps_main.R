#' Function for computing naps outside of TSP
#' @param all_sleep_dat Dataset containing sleep segments. Must contain person_id, is_main_sleep, date_col, level, and start_datetime.
#' @return dataframe with person_id, date_col, nap_count, and nap_length
#' @noRd 
get_naps_main <- function(all_sleep_dat){
    all_sleep_dat <- all_sleep_dat[is_main_sleep == FALSE]
    dt <- all_sleep_dat[!level %in% AWAKE_LEVELS()]
    dt[, end := start_datetime + lubridate::seconds(duration_in_min * 60)]
    dt[, start_date := lubridate::as_date(start_datetime)]
    dt[, end_date :=  lubridate::as_date(end)]
    dt[, start_datetime_temp := lubridate::as_datetime(paste0(start_date," 23:59:59"))]
    dt[, end_datetime_temp := lubridate::as_datetime(paste0(end_date," 00:00:00"))]
    d1 <- dt[start_date == end_date, .(duration = sum(duration_in_min),
                                       n_logs = length(unique(sleep_log))),
                                       .(start_date,person_id)]
    d2 <- dt[start_date != end_date, .(duration = sum(as.numeric(start_datetime_temp - start_datetime) / 60),
                                       n_logs = length(unique(sleep_log))),
                                       .(start_date,person_id)]
    d3 <- dt[start_date != end_date, .(duration = sum(as.numeric(end - end_datetime_temp) / 60),
                                       n_logs = length(unique(sleep_log))),
                                       .(end_date,person_id)]
    colnames(d3)[1] <- "start_date"
    d4 <- rbindlist(list(d1,d2,d3))
    nap_agg <- d4[,.(nap_count = sum(n_logs),
                     nap_length = sum(duration)),
                    by=c("person_id","start_date")]
    colnames(nap_agg)[2] <- "nap_date"
    return(nap_agg)
}
