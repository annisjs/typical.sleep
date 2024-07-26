#' Uses foverlaps from data.table to filter relevant sleep logs
#' @param x sleep data
#' @param y data.table containing date ranges
#' @param start character string representing the start window column
#' @param end character string representing the end window column
#' @noRd 
filter_overlaps <- function(x,y,start,end,type="any")
{
    person_id <- "person_id"
    data.table::setkeyv(y, c(person_id, start, end))
    dt_overlaps <- data.table::foverlaps(x, y, type = type, nomatch = NULL, which = TRUE)
    dt_overlaps[, date := lubridate::as_datetime(y[,.SD,.SDcols=end][[1]][dt_overlaps$yid])]
    dt_overlaps <- dt_overlaps[,c("xid","date")]
    return(dt_overlaps)
}