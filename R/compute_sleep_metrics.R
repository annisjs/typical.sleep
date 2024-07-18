#' Compute sleep metrics. All metrics are computed for each person_id and date.
#' @param sleep_data sleep-levels dataset containing the following columns: person_id, date, start_time, level, duration, and is_main_sleep. 
#' @param sleep_type character string specifying the primary sleep period to operate over, "main_sleep" or "typical_sleep". If sleep_type is "typical_sleep," then the typical_sleep algorithm must be run first.
#' @return A dataframe with the following columns:
#' \describe{
#'   \item{sleep_onset}{The start datetime of the first sleep segment, where sleep segment levels are not wake, awake, or restless.}
#'   \item{sleep_offset}{The end datetime of the last sleep segment, where sleep segment levels are not wake, awake, or restless. The end datetime is computed by adding sleep duration to start_datetime.}
#'   \item{sleep_duration}{Duration of sleep in minutes. (sleep offset - sleep onset) / 60}
#'   \item{midsleep_point}{Midpoint between sleep_onset and sleep_offset. (sleep onset + sleep offset) / 2}
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
#'   \item{bedtime}{start datetime of first sleep log.}
#'   \item{waketime}{The end datetime of the final sleep log.}
#'   \item{time_in_bed}{Time in bed in minutes. (bedtime - waketime) / 60.}
#'   \item{num_awakenings}{Number of contiguous sleep segments indcating an awakening. Segments of differing levels will be combined to form a single contiguous sleep segment given the level is one of awake, wake, or restless.}
#'   \item{num_long_awakenings}{Count of all contiguous wake, awake, and restless sleep segments >=30 minutes in duration between sleep onset and sleep offset.}
#'   \item{longest_wake_duration}{Duration of the longest contiguous period spent awake after sleep onset}
#'   \item{wake_after_sleep_onset}{Total duration of all ‘wake’, ‘awake’ and ‘restless’ sleep segments between sleep onset and sleep offset}
#'   \item{wake_to_end_of_log_latency}{Time difference in hours between the final awakening to wake time}
#'   \item{sleep_efficiency}{Total time spent asleep as a proportion of the primary sleep period. 100 * total_sleep_time / time_in_bed}
#'   \item{nap_count}{Number of sleep logs outside of primary sleep period}
#'   \item{nap_length}{Total duration of sleep across sleep logs outside of the primary sleep period}
#'   \item{sleep_onset_latency}{Time between bedtime and sleep onset}
#'   \item{total_sleep_time_24h}{Total duration of sleep for a given calendar date.}
#' }
#'@export
#' 
#'@examples
#'\dontrun{
#' # If parsing from JSON format
#' dat <- parse_fitbit_json("sleep_data.json")
#' metrics <- compute_sleep_metrics(dat,"main_sleep")
#' print(metrics)
#' tsp_dat <- typical_sleep(dat)
#' tsp_metrics <- compute_sleep_metrics(tsp_dat,"typical_sleep")
#' print(tsp_metrics)
#'}
compute_sleep_metrics <- function(sleep_data)
{
  if (is.null(attr(sleep_data,"format")))
  {
    stop("Not a sleep_logs object. Run as_sleep_logs() on dataset first.")
  }
  if (is.null(attr(sleep_data,"sleep_type")))
  {
     date_col <- "sleep_date"
     nap_agg <- get_naps(sleep_data,date_col,"is_main_sleep")
     sleep_data <- sleep_data[is_main_sleep == TRUE]
     attr(sleep_data,"format") <- "log"
  } else if (attr(sleep_data,"sleep_type") == "typical")
  {
    date_col <- "typical_sleep_date"
    nap_agg <- get_naps(sleep_data,date_col,"is_typical_sleep")
    sleep_data <- sleep_data[is_typical_sleep == TRUE]
  }
  tst_24 <- get_tst_24hr(sleep_data)
  setkey(sleep_data, person_id, start_datetime)
  sleep_data[, end_time := start_datetime + lubridate::seconds(duration_in_min * 60)]
  sleep_data[, sleep_start_new := center(time_to_minute(start_datetime))]
  sleep_data[, sleep_end_new := center(time_to_minute(end_time))]
  # The following creates variables to help compute wake metrics
  # Identify all wake levels
  sleep_data[, wake_flag := level %in% AWAKE_LEVELS()]
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
  
  # Add naps
  out <- merge(out,nap_agg,by=c("person_id",date_col),all.x=T)
  out[, nap_count := fifelse(is.na(nap_count),0,nap_count)]
  out[, nap_length := fifelse(is.na(nap_length),0,nap_length)]

  # Add 24hr sleep
  out <- merge(out,tst_24,by.x=c("person_id",date_col),by.y=c("person_id","start_date"),all.x=T)
  
  # Second-order metrics
  out[, sleep_efficiency := round(100 * total_sleep_time / time_in_bed,1)]
  out[, sleep_onset_latency := lubridate::interval(bedtime, sleep_onset) / lubridate::minutes(1)]
  
  return(out)
}

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

#' Function for computing 24hr sleep duration. Not exported.
#' @param all_sleep_dat Dataset containing sleep segments. Must contain person_id, is_main_sleep, date_col, level, and start_datetime.
#' @return dataframe with person_id, date_col, nap_count, and nap_length
#' @noRd 
get_tst_24hr <- function(all_sleep_date){
    dt <- all_sleep_date[!level %in% AWAKE_LEVELS()]
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

max_no_warn <- function(x,na.rm=T) {if (length(x)>0 & any(!is.na(x))) max(x,na.rm=na.rm) else as.double(NA)}