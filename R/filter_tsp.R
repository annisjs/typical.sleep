#' Typical sleep algorithm
#' @param all_sleep_dat sleep-levels dataset containing the following columns: person_id, sleep_date, start_datetime, level, duration_in_min, and is_main_sleep.
#' @noRd
filter_tsp <- function(all_sleep_dat)
{
    setkey(all_sleep_dat, person_id, start_datetime)
    first_last_asleep <- all_sleep_dat[ level != "awake" & level != "wake" & level != "restless" & msp == TRUE,
                                    .(start_datetime_log = start_datetime_log[1],
                                      end_time_log = end_time_log[.N]),
                                    .(person_id,date_new)]
    first_last_asleep[, first_asleep_minute_new := center(time_to_minute(start_datetime_log))]
    first_last_asleep[, last_asleep_minute_new := center(time_to_minute(end_time_log))]
    first_last_asleep[, diff_time := last_asleep_minute_new - first_asleep_minute_new]
    first_last_asleep[, diff_datetime := lubridate::interval(
                                         lubridate::as_datetime(start_datetime_log),
                                         lubridate::as_datetime(end_time_log))/lubridate::minutes(1)]
    first_last_asleep[diff_time != diff_datetime, last_asleep_minute_new := last_asleep_minute_new + 1440]
    first_last_asleep[, median_sleep_start := median(first_asleep_minute_new),.(person_id)]
    first_last_asleep[, median_sleep_end := median(last_asleep_minute_new),.(person_id)]
    first_last_asleep[, msp := (first_asleep_minute_new + last_asleep_minute_new) / 2]
    first_last_asleep[, median_msp := median(msp),.(person_id)]
    first_last_asleep_ranges <- first_last_asleep[, c("person_id","median_sleep_start","median_sleep_end","start_datetime_log","median_msp")]
    first_last_asleep_ranges <- first_last_asleep_ranges[!duplicated(first_last_asleep_ranges)]
    first_last_asleep_ranges[, bt_wt_by_msp := median_sleep_end - median_sleep_start <= 300]
    first_last_asleep_ranges[, bedtime_gt_waketime := median_sleep_start > median_sleep_end]
    first_last_asleep_ranges[bt_wt_by_msp == TRUE, median_sleep_start := median_msp - 3.5*60]
    first_last_asleep_ranges[bt_wt_by_msp == TRUE, median_sleep_end := median_msp + 3.5*60]
    first_last_asleep_ranges[, tsp_median_sleep_start := median_sleep_start]
    first_last_asleep_ranges[, tsp_median_sleep_end := median_sleep_end]
    first_last_asleep_ranges[, median_sleep_start2 := lubridate::ymd(lubridate::as_date(start_datetime_log)) + lubridate::seconds(median_sleep_start*60)]
    first_last_asleep_ranges[, median_sleep_end2 := lubridate::ymd(lubridate::as_date(start_datetime_log)) + lubridate::seconds(median_sleep_end*60)]
    first_last_asleep_ranges[, median_sleep_start3 := median_sleep_start2 + lubridate::days(1)]
    first_last_asleep_ranges[, median_sleep_end3 := median_sleep_end2 + lubridate::days(1)]
    first_last_asleep_ranges[, median_sleep_start1 := median_sleep_start2 - lubridate::days(1)]
    first_last_asleep_ranges[, median_sleep_end1 := median_sleep_end2 - lubridate::days(1)]
    fl_melt1 <- first_last_asleep_ranges[,c("person_id","median_sleep_start1","median_sleep_end1")]
    fl_melt2 <- first_last_asleep_ranges[,c("person_id","median_sleep_start2","median_sleep_end2")]
    fl_melt3 <- first_last_asleep_ranges[,c("person_id","median_sleep_start3","median_sleep_end3")]
    colnames(fl_melt1) <- c("person_id","median_sleep_start","median_sleep_end")
    colnames(fl_melt2) <- c("person_id","median_sleep_start","median_sleep_end")
    colnames(fl_melt3) <- c("person_id","median_sleep_start","median_sleep_end")
    ranges <- rbindlist(list(fl_melt1,fl_melt2,fl_melt3))
    ranges <- ranges[!duplicated(ranges)]
    setkey(all_sleep_dat, person_id, start_datetime_log, end_time_log)
    dt_overlaps <- filter_overlaps(all_sleep_dat,ranges,"median_sleep_start","median_sleep_end")
    dt_overlaps <- dt_overlaps[!duplicated(dt_overlaps[,c("xid")])]
    all_sleep_dat[, is_typical_sleep := FALSE]
    all_sleep_dat[dt_overlaps$xid, is_typical_sleep := TRUE]
    all_sleep_dat[dt_overlaps$xid, date_tsp := lubridate::as_date(dt_overlaps$date)]
    fl_melt1 <- first_last_asleep_ranges[,c("person_id","median_sleep_end1","median_sleep_start2")]
    fl_melt2 <- first_last_asleep_ranges[,c("person_id","median_sleep_end2","median_sleep_start3")]
    colnames(fl_melt1) <- c("person_id","median_sleep_start","median_sleep_end")
    colnames(fl_melt2) <- c("person_id","median_sleep_start","median_sleep_end")
    ranges <- rbindlist(list(fl_melt1,fl_melt2))
    ranges <- ranges[!duplicated(ranges)]
    dt_overlaps <- filter_overlaps(all_sleep_dat,ranges,"median_sleep_start","median_sleep_end","within")
    dt_overlaps <- dt_overlaps[!duplicated(dt_overlaps[,c("xid")])]
    all_sleep_dat[, is_nap := FALSE]
    all_sleep_dat[dt_overlaps$xid, is_nap := TRUE]
    all_sleep_dat[dt_overlaps$xid, date_nap := lubridate::as_date(dt_overlaps$date)]
    all_sleep_dat <- insert_wake_levels(all_sleep_dat)
    median_msp_dt <- first_last_asleep_ranges[,c("person_id","median_msp","bt_wt_by_msp","bedtime_gt_waketime")]
    median_msp_dt <- median_msp_dt[!duplicated(person_id)]
    median_msp_dt[, median_msp := hms::as_hms(uncenter(median_msp) * 60)]
    median_bt_wt <- first_last_asleep_ranges[,c("person_id","tsp_median_sleep_start","tsp_median_sleep_end")]
    median_bt_wt <- median_bt_wt[!duplicated(person_id)]
    data.table::setnames(median_bt_wt,"tsp_median_sleep_start","median_sleep_start")
    data.table::setnames(median_bt_wt,"tsp_median_sleep_end","median_sleep_end")
    median_bt_wt[, median_sleep_start := hms::as_hms(uncenter(median_sleep_start) * 60)]
    median_bt_wt[, median_sleep_end := hms::as_hms(uncenter(median_sleep_end) * 60)]
    all_sleep_dat <- merge(all_sleep_dat,median_msp_dt,by="person_id")
    all_sleep_dat <- merge(all_sleep_dat,median_bt_wt,by="person_id")
    return(all_sleep_dat)
}
