#' Path function
#'
#' This function allows you to safely check if the input path is created. If not, then the input path will be automatically created. Finally, the function return the input path to lessen life complication.
#' @param directory  a chatacters for input directory
#' @keywords Path
#' @export
path <- function(directory) {
    dir.create(directory, showWarnings = FALSE)
    directory
}
