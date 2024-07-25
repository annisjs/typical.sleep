#' Insert wake levels between stitched logs
#' @param all_sleep_dat dataframe containing the following columns: person_id, date_new, start_datetime, end_time.
#' @noRd 
insert_wake_levels <- function(all_sleep_dat)
{
    setkey(all_sleep_dat, person_id, start_datetime)
    tsp_dat <- all_sleep_dat[is_typical_sleep == TRUE]
    # Insert wakes
    # Add wake between levels
    # Create variable that holds end_time i - 1
    tsp_dat[, end_time_lead_day := shift(end_time,1),.(person_id,date_tsp)]
    # Get the difference between start_time and end_time i - 1
    tsp_dat[, lead_diff := as.numeric(start_datetime - end_time_lead_day) / 60]
    # Differences > 0 indicate a gap in the sleep logs
    # Get all gaps
    wake_between <- tsp_dat[lead_diff > 0]
    # Relabel the variables to create a new wake sleep logs
    wake_between[, end_time := start_datetime]
    wake_between[, start_datetime := end_time_lead_day]
    wake_between[, duration_in_min := lead_diff]
    wake_between[, level := "imputed_awake"]
    # Cleanup
    wake_between[, end_time_lead_day := NULL]
    wake_between[, lead_diff := NULL]
    all_sleep_dat <- rbind(all_sleep_dat,wake_between,fill=TRUE)
    setkey(all_sleep_dat,person_id,start_datetime)
}