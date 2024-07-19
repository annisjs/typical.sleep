#' Additional long wake variables used to compute num_long_awakenings, longest_awake_length metrics
#' @param sleep_data The sleep_data element from either a typical sleep or sleep logs object
#' @param date_col character vector, either typical_sleep_date or sleep_date
#' @noRd 
add_long_wake_vars <- function(sleep_data,date_col)
{
  # The longest awakening variable
  sleep_data[, longest_wake_duration := fifelse(any(wake_flag_split), sum(duration_in_min[wake_flag_split == TRUE]),as.double(NA)),
             by=c("person_id",date_col,"wake_seq")]
  # Number of long awakenings
  sleep_data[, long_awakenings_flag :=  fifelse(any(wake_flag_split), sum(duration_in_min[wake_flag_split == TRUE]) >= 30,FALSE),
             by=c("person_id",date_col,"wake_seq")]
  sleep_data[, long_flag_lag := shift(long_awakenings_flag,1),by=c("person_id",date_col)]
  sleep_data[, long_diff := long_awakenings_flag - long_flag_lag]
  sleep_data[, long_wake_seq := cumsum(long_diff==1 & !is.na(long_diff)),by=c("person_id",date_col)]
  sleep_data[long_awakenings_flag == FALSE, long_wake_seq := 0]
  return(sleep_data)
}