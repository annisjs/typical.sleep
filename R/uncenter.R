#' Reverses center
#' @param x integer on the interval [-720,720]
#' @noRd
uncenter <- function(x)
{
    # Reverses center
    fifelse(x < 0, x + 24*60, x)
}