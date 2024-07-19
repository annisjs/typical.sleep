#' Function for determining nap counts and duration. Not exported.
#' @param all_sleep_dat Dataset containing sleep segments. Must contain person_id, is_main_sleep, date_col, level, and start_datetime.
#' @param date_col name of date column
#' @param sleep_period_type string representing sleep period type
#' @return dataframe with person_id, date_col, nap_count, and nap_length
#' @noRd 
get_naps <- function(all_sleep_dat,date_col,sleep_period_type)
{
  all_sleep_dat_temp <- all_sleep_dat[get(sleep_period_type)==FALSE][order(person_id,start_datetime)]
  all_sleep_dat_temp[, sleep_flag := !level %in% AWAKE_LEVELS()]
  nap_agg <- all_sleep_dat_temp[,.(nap_count = length(unique(sleep_log)),
                                   nap_length = sum(duration_in_min[sleep_flag == TRUE])),by=c("person_id",date_col)]
}
