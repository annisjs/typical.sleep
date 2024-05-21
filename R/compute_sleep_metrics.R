#' Compute sleep metrics. All metrics are computed for each person_id and date.
#' @param sleep_data sleep-levels dataset containing the following columns: person_id, date, start_time, level, duration, and is_main_sleep. 
#' @param date_col name of date column
#' @return A dataframe with the following columns:
#' \describe{
#'   \item{sleep_onset}{The start_datetime of the first sleep segment, where sleep segment levels are not wake, awake, or restless.}
#'   \item{sleep_offset}{The end datetime of the last sleep segment, where sleep segment levels are not wake, awake, or restless. The end datetime is computed by adding sleep duration to start_datetime.}
#'   \item{sleep_duration}{Duration of sleep in minutes. eqn{(sleep_offset - sleep_onset) / 60}}
#'   \item{midsleep_point}{Midpoint between sleep_onset and sleep_offset. eqn{(sleep_onset + sleep_offset) / 2}}
#'   \item{total_sleep_time}{Sum of all sleep segment durations, where level is not awake, wake, or restless.}
#'   \item{rem_duration}{Sum of all sleep segment durations, where level is rem.}
#'   \item{deep_duration}{Sum of all sleep segment durations, where level is deep.}
#'   \item{light_duration}{Sum of all sleep segment durations, where level is light.}
#'   \item{pct_rem}{Percentage of rem sleep duration. Denominator is the sum of all sleep segment durations whose level is not awake, wake, or restless. If no rem levels exist, then pct_rem is NA.}
#'   \item{pct_deep}{Percentage of deep sleep duration. Denominator is the sum of all sleep segment durations whose level is not awake, wake, or restless. If no deep levels exist, then pct_deep is NA.}
#'   \item{pct_light}{Percentage of light sleep duration. Denominator is the sum of all sleep segment durations whose level is not awake, wake, or restless. If no light levels exist, then pct_light is NA.}
#'   \item{pct_asleep}{Percentage of asleep sleep levels. Denominator is the sum of all sleep segment durations whose level is not awake, wake, or restless. If no asleep levels exist, then pct_asleep is NA.}
#'   \item{awake_duration}{Total duration of all sleep segments whose level is awake.}
#'   \item{wake_duration}{Total duration of all sleep segments whose level is wake.}
#'   \item{restless_duration}{Total duration of all sleep segments whose level is restless.}
#'   \item{pct_restless}{Percentage of restless sleep levels. Denominator is the sum of all sleep segment durations.}
#'   \item{pct_awake}{Percentage of awake sleep levels. Denominator is the sum of all sleep segment durations.}
#'   \item{pct_wake}{Percentage of wake sleep levels. Denominator is the sum of all sleep segment durations.}
#'   \item{bedtime}{start_datetime of first sleep segment.}
#'   \item{waketime}{The end datetime of the final sleep segment.}
#'   \item{time_in_bed}{Time in bed in minutes. eqn{(bedtime - waketime) / 60}.}
#'   \item{num_awakenings}{Number of contiguous sleep segments indcating an awakening. Segments of differing levels will be combined to form a single contiguous sleep segment given the level is one of awake, wake, or restless.}
#'   \item{num_long_awakenings}{Number of wake levels >= 30 minutes.}
#'   \item{longest_wake_duration}{Longest wake duration in minutes.}
#'   \item{wake_after_sleep_onset}{Duration in minutes of contiguous segments of awake, wake, and/or restless following at least one segment of sleep.}
#'   \item{wake_to_end_of_log_latency}{Duration in minutes}
#' }
#'@export
compute_sleep_metrics <- function(sleep_data,date_col)
{
  setkey(sleep_data, person_id, start_datetime)
  sleep_data[, end_time := start_datetime + lubridate::seconds(duration_in_min * 60)]
  sleep_data[, sleep_start_new := center(time_to_minute(start_datetime))]
  sleep_data[, sleep_end_new := center(time_to_minute(end_time))]
  # The following creates variables to help compute wake metrics
  # Identify all wake levels
  sleep_data[, wake_flag := level == "wake" | level == "awake" | level == "restless"]
  # Move wake flag forward one row
  sleep_data[, flag_lag := shift(wake_flag,1),by=c("person_id",date_col)]
  # Find the difference between wake_flag i and wake_flag i - 1
  sleep_data[, diff := wake_flag - flag_lag]
  # If we have a diff == 1, then this indicates a new awake level
  # Create a sequence variable that indicates the number of awakenings (1,2,3,...,N awakenings)
  sleep_data[, wake_seq := cumsum(diff==1 & !is.na(diff)),by=c("person_id",date_col)]
  sleep_data[wake_flag == FALSE, wake_seq := 0]
  # Create a logical variable that indicates when a new awakening occurs
  sleep_data[, wake_flag_split := wake_seq > 0]
  # If any awakenings occur after sleep we mark TRUE for the entire night
  sleep_data[, has_wake_after_sleep := any(wake_flag_split),by=c("person_id",date_col)]
  
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
  
  # Last awake
  sleep_data[level == "wake" | level == "awake" | level == "restless", last_awake := duration_in_min[.N],by=c("person_id",date_col)]
  
  sleep_agg <- sleep_data[level != "wake" & level != "awake" & level != "restless",
  .(
    sleep_onset = start_datetime[1],
    sleep_offset = end_time[.N],
    sleep_duration = as.numeric(end_time[.N] - start_datetime[1])/60,
    midsleep_point = hms::as_hms(((sleep_start_new[1] + sleep_end_new[.N]) / 2) * 60),
    total_sleep_time = sum(duration_in_min),
    rem_duration = fifelse(any(level=="rem"),sum(duration_in_min[level=="rem"]),as.double(NA)),
    deep_duration = fifelse(any(level=="deep"),sum(duration_in_min[level=="deep"]),as.double(NA)),
    light_duration = fifelse(any(level=="light"),sum(duration_in_min[level=="light"]),as.double(NA)),
    asleep_duration = fifelse(any(level=="asleep"),sum(duration_in_min[level=="asleep"]),as.double(NA)), 
    pct_rem = fifelse(any(level=="rem"),sum(duration_in_min[level=="rem"])/sum(duration_in_min),as.double(NA)),
    pct_deep = fifelse(any(level=="deep"),sum(duration_in_min[level=="deep"])/sum(duration_in_min),as.double(NA)),
    pct_light = fifelse(any(level=="light"),sum(duration_in_min[level=="light"])/sum(duration_in_min),as.double(NA)),
    pct_asleep = fifelse(any(level=="asleep"),sum(duration_in_min[level=="asleep"])/sum(duration_in_min),as.double(NA))
   ),
  by=c("person_id",date_col)]
  
  awake_agg <- sleep_data[,
  .(
    awake_duration = fifelse(any(level=="awake"),sum(duration_in_min[level=="awake"]),as.double(NA)),
    wake_duration = fifelse(any(level=="wake"),sum(duration_in_min[level=="wake"]),as.double(NA)),
    awake_combined =  fifelse(any(level=="awake" | level=="wake"),
                              sum(duration_in_min[level=="awake"|level=="wake"]),as.double(NA)),  
    restless_duration = fifelse(any(level=="restless"),sum(duration_in_min[level=="restless"]),as.double(NA)),
    pct_restless = fifelse(any(level=="restless"),sum(duration_in_min[level=="restless"])/sum(duration_in_min),as.double(NA)),
    pct_awake = fifelse(any(level=="awake"),sum(duration_in_min[level=="awake"])/sum(duration_in_min),as.double(NA)),
    pct_wake = fifelse(any(level=="wake"),sum(duration_in_min[level=="wake"])/sum(duration_in_min),as.double(NA)),
    pct_wake_combined = fifelse(any(level=="awake" | level=="wake"),
                              sum(duration_in_min[level=="awake"|level=="wake"])/sum(duration_in_min),as.double(NA)),
    bedtime = start_datetime[1],
    waketime = end_time[.N],
    time_in_bed = as.numeric(end_time[.N] - start_datetime[1]) / 60,
    num_awakenings = max(wake_seq),
    num_long_awakenings = max(long_wake_seq),
    longest_awake_length = fifelse(any(!is.na(longest_wake_duration)),max(longest_wake_duration,na.rm=T),as.double(NA)),
    wake_after_sleep_onset = fifelse(any(has_wake_after_sleep),
                   sum(duration_in_min[wake_flag_split == TRUE]),
                   as.double(NA)),
    wake_to_end_of_log_latency = ifelse(any(!is.na(last_awake)),last_awake[!is.na(last_awake)][1],NA)
  ),
  by = c("person_id",date_col)]

  out <- merge(awake_agg, sleep_agg, by=c("person_id",date_col))
  
  # Add naps
  nap_agg <- get_naps(sleep_data,date_col)
  out <- merge(out,nap_agg,by=c("person_id",date_col),all.x=T)
  out[, nap_count := fifelse(is.na(nap_count),0,nap_count)]
  out[, nap_length := fifelse(is.na(nap_length),0,nap_length)]
  
  # Second-order metrics
  out[, sleep_efficiency := round(100 * total_sleep_time / time_in_bed,1)]
  out[, sleep_onset_latency := interval(bedtime, sleep_onset) / minutes(1)]
  
  return(out)
}

#' Function for determining nap counts and duration. Not exported.
#' @param all_sleep_dat Dataset containing sleep segments. Must contain person_id, is_main_sleep, date_col, level, and start_datetime.
#' @param date_col name of date column
#' @return dataframe with person_id, date_col, nap_count, and nap_length
get_naps <- function(all_sleep_dat,date_col)
{
  all_sleep_dat_temp <- all_sleep_dat[is_main_sleep==FALSE][order(person_id,start_datetime)]
  # The following creates variables to help compute wake metrics
  # Identify all wake levels
  all_sleep_dat_temp[, sleep_flag := level != "wake" & level != "awake" & level != "restless"]
  # Move wake flag forward one row
  all_sleep_dat_temp[, flag_lag := shift(sleep_flag,1),by=c("person_id",date_col)]
  # Find the difference between wake_flag i and wake_flag i - 1
  all_sleep_dat_temp[, diff := sleep_flag - flag_lag]
  # If we have a diff == 1, then this indicates a new awake level
  # Create a sequence variable that indicates the number of awakenings (1,2,3,...,N awakenings)
  all_sleep_dat_temp[, sleep_seq := cumsum(diff==1 & !is.na(diff)),by=c("person_id",date_col)]
  all_sleep_dat_temp[sleep_flag == FALSE, sleep_seq := 0]
  # Create a logical variable that indicates when a new awakening occurs
  all_sleep_dat_temp[, sleep_flag_split := sleep_seq > 0]
  if (nrow(all_sleep_dat_temp) > 0)
  {
    nap_agg <- all_sleep_dat_temp[,.(nap_count = fifelse(any(!is.na(sleep_seq)),max(sleep_seq),as.double(NA)),
                                   nap_length = sum(duration_in_min[sleep_flag_split == TRUE])),by=c("person_id",date_col)]
  } else {
    nap_agg <- all_sleep_dat[, .(nap_count = 0, nap_length = 0),by=c("person_id",date_col)]
  }
  return(nap_agg)
}