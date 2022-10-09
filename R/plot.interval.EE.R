library(ggplot2)
library(viridis)
library(dplyr)
plot.interval.EE <- function(cage.data,
                             sample.subset = NULL, 
                             by.proportion = FALSE) {
  model.spline <- readRDS( "./models/model.spline.rds")
  model.rf <- readRDS("./models/model.rf.rds")
  model.glm <- readRDS("./models/model.glm.rds")
  if(!is.null(sample.subset)) {
    cage.data <- subset.data(cage.data, sample.subset)
  }
  pre.plot.dat <- dplyr::bind_rows(cage.data[[1]])
  indep.vairables <- pre.plot.dat[,c("Feed", "Drink", "Heat", "Fat.Mass", "Lean.Mass", "Interval_num")]
  indep.vairables <- indep.vairables %>%
    mutate("Ambulation" = sqrt(pre.plot.dat$Xamb^2 + pre.plot.dat$Yamb^2)) 
  indep.vairables <- na.omit(indep.vairables)
  
  outliers <- NULL
  for(i in c(1,2,4,5)) {
    x <- EnvStats::rosnerTest(as.matrix(indep.vairables[,i]), k = 5)$all.stats
    x <- x[x$Value >= 1,]
    x <- x[x$Outlier == TRUE,]
    x <- x$Obs.Num
    outliers <- c(outliers, x)
  }
  outliers <- unique(outliers)
  if (length(outliers) > 0) {
    indep.vairables <- indep.vairables[-outliers,]
  }
  
  indep.vairables[,"Ambulation"] <- sqrt(indep.vairables[,"Ambulation"])
  indep.vairables <- indep.vairables[indep.vairables$Heat > 5,]
  
  heat.rf <- predict(model.rf, indep.vairables)
  
  heat.spline<- suppressWarnings(npreg::predict.sm(model.spline, indep.vairables))
  heat.spline[heat.spline < 0] <- NA
  
  heat.glm <- predict(model.glm, indep.vairables)
  
  indep.vairables <- data.frame(indep.vairables, heat.spline, heat.rf, heat.glm)
  
  indep.vairables$heat.mean <- rowMeans(as.matrix(indep.vairables[,c("heat.spline", "heat.rf", "heat.glm")]))
  indep.vairables$SF <- indep.vairables$heat.mean/indep.vairables$Heat
  indep.vairables$Ambulatory.EE <- (indep.vairables$Ambulation*0.3228)#/indep.vairables$SF
  indep.vairables$ThermicEffect.EE <- (indep.vairables$Feed*8.4165)#/indep.vairables$SF
  
  indep.vairables$Adaptive.EE <- indep.vairables$Heat - indep.vairables$heat.mean
  indep.vairables$Adaptive.EE[indep.vairables$Adaptive.EE < 0] <- 0
  
  #indep.vairables$BMR.EE <- indep.vairables$heat.mean/indep.vairables$SF - (indep.vairables$Adaptive.EE + 8.8652 + 0.6976*indep.vairables$Weight)
  
  indep.vairables$BMR.EE <- indep.vairables$Heat - (indep.vairables$Adaptive.EE + indep.vairables$Ambulatory.EE + indep.vairables$ThermicEffect.EE)
  
  rho <- cor.test(indep.vairables$Heat, indep.vairables$heat.mean)
  annotations <- data.frame(
    xpos = c(-Inf),
    ypos =  c(Inf),
    annotateText = c(paste0("Thermoneutral rho = ", round(rho$estimate, 3))),
    hjustvar = c(-0.05) ,
    vjustvar = c(1))
  
  
  indep.vairables <- na.omit(indep.vairables)
  
  EE.summary <- indep.vairables %>%
    group_by(Interval_num) %>%
    summarise(across(c(3,8:15), mean)) %>%
    as.data.frame()
  
  melted <- reshape2::melt(EE.summary[,c(1,7:10)], id.vars = 1)
  
  melted$variable <- factor(melted$variable, levels = c("Adaptive.EE", "Ambulatory.EE", "ThermicEffect.EE", "BMR.EE"))
  plot <- ggplot(melted, aes(x=as.numeric(Interval_num), y =as.numeric(value))) + 
    scale_fill_viridis(option = "H", discrete = TRUE, direction = -1) + 
    theme_classic() + 
    ylab("Heat") + 
    xlab("Time Interval") 
  if(by.proportion) {
    plot <- plot + geom_bar(aes(fill = variable), stat = "identity", position = "fill")
  } else {
    plot <- plot + geom_bar(aes(fill = variable), stat = "identity")
  }
  plot <- plot + geom_text(data=annotations,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText), size = 2)
  return(plot)
}
