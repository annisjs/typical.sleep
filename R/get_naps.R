#' Function for computing naps outside of TSP
#' @param all_sleep_dat Dataset containing sleep segments. Must contain person_id, is_main_sleep, date_col, level, and start_datetime.
#' @return dataframe with person_id, date_col, nap_count, and nap_length
#' @noRd 
get_naps <- function(all_sleep_dat)
{
    naps <- all_sleep_dat[is_nap == TRUE]
    naps[, sleep_flag := !level %in% AWAKE_LEVELS()]
    nap_agg <- naps[,.(nap_count = length(unique(sleep_log)),
                       nap_length = sum(duration_in_min[sleep_flag == TRUE])),
                    by=c("person_id","nap_date")]
    return(nap_agg)
}