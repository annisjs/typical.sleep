#' Converts time to minute
#' @param x datetime object
#' @noRd
time_to_minute <- function(x)
{
    hour(x)*60 + minute(x) + second(x) / 60
}