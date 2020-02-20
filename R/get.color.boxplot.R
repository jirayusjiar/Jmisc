#' Get Colorful Boxplots Function
#'
#' This function makes life simple by automatically generating colorful boxplots.
#'
#' @param input.data  a data frame for data
#' @param melted  
#' @param xlab 
#' @param ylab
#' @param fill.color
#' @param range.y
#' @param range.x
#' @param reverse.x
#' @param reverse.y
#' @param sort.x
#' @param detailed.y.ticks
#' @param y.tick.div
#' @param text.size
#' @param facet
#' @param width.size
#' @param height.size
#' @param file.name
#' @param color.pallete
#' @keywords boxplot
#' @export
get.color.boxplot <-
  function(input.data,
           melted = F,
           xlab = '',
           ylab = '',
           fill.color = F,
           range.y = NULL,
           range.x = NULL,
           reverse.x = T,
           reverse.y = T,
           sort.x = F,
           detailed.y.ticks = F,
           y.tick.div = 1,
           text.size = 20,
           facet = F,
           width.size = 10,
           height.size = 7.5,
           file.name = 'plot-output',
           color.pallete = c('#f1a340', '#f7f7f7', '#998ec3')) {
    if (melted) {
      plot.data <- input.data
      if (ncol(plot.data) == 2)
        names(plot.data) <- c('model', 'value')
      else
        names(plot.data) <- c('model', 'variable', 'value')
    } else{
      plot.data <- melt(input.data)
    }
    
    if (ncol(plot.data) == 3) {
      names(plot.data)[1] <- 'model'
    }
    # color.pallette <- c('#f1a340', '#f7f7f7', '#998ec3')
    
    if (reverse.y) {
      plot.data$variable <-
        change.factor.order(plot.data$variable, rev(levels(plot.data$variable)))
    }
    if (reverse.x) {
      plot.data$model <-
        change.factor.order(plot.data$model, rev(levels(plot.data$model)))
    }
    if (fill.color) {
      if (sort.x)
        g <-
          ggplot(plot.data, aes(
            x = reorder(model,-value, median, order = T),
            y = value,
            fill = model
          )) + scale_fill_manual(values = color.pallete)
      else
        g <-
          ggplot(plot.data, aes(x = model, y = value, fill = model)) + scale_fill_manual(values = color.pallete)
    } else {
      if (sort.x)
        g <-
          ggplot(plot.data, aes(x = reorder(model,-value, median, order = T), y = value))
      else
        g <- ggplot(plot.data, aes(x = model, y = value))
    }
    
    g <- g +
      geom_boxplot() + labs(x = xlab, y = ylab) +
      coord_flip() + guides(fill = FALSE) + theme(text = element_text(size =
                                                                        text.size),
                                                  axis.text.x = element_text(angle =
                                                                               0, hjust = 0.5))
    
    if (facet) {
      g <- g + facet_grid(rows = vars(variable))
    }
    if (!is.null(range.y))
      g <- g + ylim(range.y)
    if (!is.null(range.x))
      g <- g + xlim(range.x)
    if (detailed.y.ticks){
      g <-
      g + scale_y_continuous(breaks = seq(
        from = floor(min(plot.data$value) / y.tick.div),
        by = y.tick.div,
        length.out = (round(max(plot.data$value) / y.tick.div) + 1)
      ))
    }
    g
    ggsave(
      filename = paste0(file.name, '.pdf'),
      plot = g,
      width = width.size,
      height = height.size
    )
    return(g)
  }