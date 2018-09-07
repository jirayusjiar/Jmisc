#' @import ggplot2
#' @export
get.density.plot <- function(input.data, scale = F){

    if(scale){
        input.data <- as.data.frame(apply(input.data, 2, function(x) scale(x, scale = T, center = F)))
    }

    plot.data <- melt(input.data)

    #Plot
    g <- ggplot(plot.data, aes(x = value, fill = variable)) + geom_density(alpha = 0.5)
    return(g)
}
