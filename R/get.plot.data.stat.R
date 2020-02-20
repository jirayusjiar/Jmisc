#' Get Statistical Values of Boxplots Data
#'
#' This function makes life simple by automatically summarising statistical values of boxplots data.
#'
#' @param input.plot.data  a data frame for data
#' @param summary.index 
#' @param file.name
#' @keywords summary
#' @export
get.plot.data.stat <- function(input.plot.data, summary.index, file.name){
  plot.data.stats <- do.call(rbind, lapply(input.plot.data, function(x) {
    tmp.x <- as.data.frame(x)
    tmp.x <- tmp.x[, summary.index]
    data.frame(t(c(summary(tmp.x))))
  }))
  write.table(plot.data.stats, file = paste0(file.name, '.csv'), sep=',', row.names = T)
}