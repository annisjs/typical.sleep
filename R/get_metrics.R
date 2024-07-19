#' Function for computing sleep metrics. Not exported
#' @param sleep_data The sleep_data element from either a typical sleep or sleep logs object
#' @param date_col character vector, either typical_sleep_date or sleep_date
#' @noRd 
get_metrics <- function(sleep_data,date_col)
{
  setkey(sleep_data, person_id, start_datetime)
  tst_24 <- get_tst_24hr(sleep_data)
  sleep_data[, end_time := start_datetime + lubridate::seconds(duration_in_min * 60)]
  sleep_data[, sleep_start_new := center(time_to_minute(start_datetime))]
  sleep_data[, sleep_end_new := center(time_to_minute(end_time))]
  sleep_data <- add_wakes(sleep_data,date_col)
  sleep_data <- add_long_wake_vars(sleep_data,date_col)
  
  sleep_agg <- sleep_data[!level %in% AWAKE_LEVELS(),
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
  
  # Need sleep offset in sleep_data
  sleep_data <- merge(sleep_data,sleep_agg[,.SD,.SDcols=c("person_id",date_col,"sleep_offset")],by=c("person_id",date_col))
  
  awake_agg <- sleep_data[,
  .(
    imputed_awake_duration = fifelse(any(level=="imputed_awake"),sum(duration_in_min[level=="imputed_awake"]),as.double(NA)),
    awake_duration = fifelse(any(level=="awake"),sum(duration_in_min[level=="awake"]),as.double(NA)),
    wake_duration = fifelse(any(level=="wake"),sum(duration_in_min[level=="wake"]),as.double(NA)),
    restless_duration = fifelse(any(level=="restless"),sum(duration_in_min[level=="restless"]),as.double(NA)),
    pct_restless = fifelse(any(level=="restless"),sum(duration_in_min[level=="restless"])/sum(duration_in_min),as.double(NA)),
    pct_awake = fifelse(any(level=="awake"),sum(duration_in_min[level=="awake"])/sum(duration_in_min),as.double(NA)),
    pct_wake = fifelse(any(level=="wake"),sum(duration_in_min[level=="wake"])/sum(duration_in_min),as.double(NA)),
    bedtime = start_datetime[1],
    waketime = end_time[.N],
    time_in_bed = as.numeric(end_time[.N] - start_datetime[1]) / 60,
    num_awakenings = max_no_warn(wake_seq),
    num_long_awakenings = max_no_warn(long_wake_seq),
    longest_awake_length = fifelse(any(!is.na(longest_wake_duration)),max_no_warn(longest_wake_duration,na.rm=T),as.double(NA)),
    wake_after_sleep_onset = fifelse(any(has_wake_after_sleep),
                   sum(duration_in_min[wake_flag_split == TRUE]),
                   as.double(NA)),
    wake_to_end_of_log_latency = as.numeric(end_time_log[.N] -  sleep_offset[1]) / 60
  ),
  by = c("person_id",date_col)]

  out <- merge(awake_agg, sleep_agg, by=c("person_id",date_col))

  # Add 24hr sleep
  out <- merge(out,tst_24,by.x=c("person_id",date_col),by.y=c("person_id","start_date"),all.x=T)
  
  # Second-order metrics
  out[, sleep_efficiency := round(100 * total_sleep_time / time_in_bed,1)]
  out[, sleep_onset_latency := lubridate::interval(bedtime, sleep_onset) / lubridate::minutes(1)]
  
  return(out)
}

max_no_warn <- function(x,na.rm=T) {if (length(x)>0 & any(!is.na(x))) max(x,na.rm=na.rm) else as.double(NA)}