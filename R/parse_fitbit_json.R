#' Parses the JSON object returned from the Fitbit API
#' @param input_file the Fitbit JSON object
#' @param person_id optional person ID. Default is 1.
#' @export
parse_fitbit_json <- function(input_file,person_id = 1)
{
  dat <- jsonlite::fromJSON(input_file)
  levels <- rbindlist(Map(cbind, date=dat$dateOfSleep, dat$levels$data, is_main_sleep=dat$mainSleep))
  levels[, duration := seconds/60]
  levels[, start_time := lubridate::as_datetime(dateTime)]
  levels[, seconds := NULL]
  levels[, dateTime := NULL]
  levels[, person_id := person_id]
  return(levels)
}