#' Imputes wakes after logs are stitched together
#' @param sleep_data the sleep_data element from either a sleep logs or typical sleep object
#' @param date_col string, either "typical_sleep_date" or "sleep_date"
#' @noRd
add_wake_vars <- function(sleep_data,date_col)
{
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
    return(sleep_data)
}