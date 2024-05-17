#' Center the time over midnight
#' @param x integer on the interval [0,1439]
#' @description Centers on midnight such that Noon to 11:59 has negative sign. \cr
#' Midnight to Noon as positive sign. \cr
#' Minutes are mapped thusly: \cr
#' 720:1439 -> -720:-1 \cr
#' 0:720 -> 0:720 \cr
center <- function(x)
{
    fifelse(x >= 12*60, x - 24*60, x)
}