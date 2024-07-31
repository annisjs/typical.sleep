#' Location of the example dataset
#' @export 
#' @examples
#'\dontrun{
#' # If parsing from JSON format
#' file_loc <- example_dataset()
#' out <- parse_fitbit_json(file_loc)
#'}
example_dataset <- function() {
  system.file("extdata", "test_dataset.json", package = "typical.sleep", mustWork = TRUE)
}