library(ggplot2)
library(dplyr)
options(dplyr.summarise.inform = FALSE)

plot.track <- function(cage.data, 
                       group.by = NULL,
                       split.by = NULL,
                       y.axis = NULL,
                       sample.subset = NULL) {
  
  
  if(!is.null(group.by) | !is.null(split.by)) {
    pre.plot.dat <- pull.dat(c(group.by, split.by), cage.data)
    colnames(pre.plot.dat)[which(colnames(pre.plot.dat) == y.axis)] <- "y.axis"
    num.vals <- length(c(group.by, split.by))
    if(num.vals == 1) {
      if(!is.null(split.by)) {
        colnames(pre.plot.dat)[ncol(pre.plot.dat)] <- "split.by"
        pre.plot.dat <- pre.plot.dat %>%
          group_by(Interval_num, split.by) %>%
          summarise(y.axis = mean(y.axis))
      } else {
        colnames(pre.plot.dat)[ncol(pre.plot.dat)] <- "group.by"
        pre.plot.dat <- pre.plot.dat %>%
          group_by(Interval_num, group.by) %>%
          summarise(y.axis = mean(y.axis))
      }
    } else {
      colnames(pre.plot.dat)[(ncol(pre.plot.dat)-1):ncol(pre.plot.dat)] <- c("group.by", "split.by")
      pre.plot.dat <- pre.plot.dat %>%
        group_by(Interval_num, group.by, split.by) %>%
        summarise(y.axis = mean(y.axis))
    }
    colnames(pre.plot.dat)[1] <- "x.axis"
  } else {
    pre.plot.dat <- bind_rows(cage.data[[1]])
    colnames(pre.plot.dat)[which(colnames(pre.plot.dat) == y.axis)] <- "y.axis"
    pre.plot.dat <- pre.plot.dat %>%
      group_by(Interval_num) %>%
      summarise(y.axis = mean(y.axis))
    colnames(pre.plot.dat) <- c("x.axis","y.axis")
  }
  
  if(is.null(group.by)) {
    plot <- ggplot(pre.plot.dat, aes(x=as.numeric(x.axis), y = y.axis)) + 
      stat_smooth(formula = y ~ s(x, bs = "cs"),
        geom = 'area', method = 'gam', span = 1/3, fill = "grey") + 
      ylab(y.axis) + 
      xlab("Time Interval") + 
      theme_classic()
  } else {
    plot <- ggplot(pre.plot.dat, aes(x=as.numeric(x.axis), y = y.axis)) + 
      stat_smooth(formula = y ~ s(x, bs = "cs"),
        geom = 'area', method = 'gam', span = 1/3,
        aes(fill = group.by)) + 
      ylab(y.axis) + 
      xlab("Time Interval") + 
      theme_classic()
  }
  if(!is.null(split.by)) {
    plot <- plot + facet_grid(.~split.by)
  }
  return(plot)
}
