library(ggplot2)
library(viridis)
library(dplyr)
library(matrixStats)
plot.interval.EE <- function(cage.data,
                             sample.subset = NULL, 
                             by.proportion = FALSE, 
                             plot.area = FALSE) {
  model.spline <- readRDS( "./models/model.earth.rds")
  model.rf <- readRDS("./models/model.rf.rds")
  model.glm <- readRDS("./models/model.glm.rds")
  if(!is.null(sample.subset)) {
    cage.data <- subset.data(cage.data, sample.subset)
  }
  pre.plot.dat <- dplyr::bind_rows(cage.data[[1]])
  pre.plot.dat <- pre.plot.dat[pre.plot.dat$Cage %!in% c("", "==============="),]
  interval.assignments <- time.handler(pre.plot.dat)
  interval.assignments2 <- as.data.frame(table(interval.assignments))
  interval.assignments <- interval.assignments2 %>%
    group_by(Interval_num) %>%
    slice_max(order_by = Freq, n = 1, with_ties = FALSE)
  interval.assignments <- interval.assignments[,1:2]
  indep.vairables <- pre.plot.dat[,c("Feed", "Drink", "Heat", "Fat.Mass", "Lean.Mass", "Interval_num")]
  indep.vairables <- indep.vairables %>%
    mutate("Ambulation" = sqrt(pre.plot.dat$Xamb^2 + pre.plot.dat$Yamb^2)) 
  indep.vairables <- na.omit(indep.vairables)
  
  outliers <- NULL
  for(i in c(1:3,7)) {
    x <- EnvStats::rosnerTest(as.matrix(indep.vairables[,i]), k = 10)$all.stats
    x <- x[x$Value >= 1,]
    x <- x[x$Outlier == TRUE,]
    x <- x$Obs.Num
    outliers <- c(outliers, x)
  }
  outliers <- na.omit(unique(outliers))
  if (length(outliers) > 0) {
    indep.vairables <- indep.vairables[-outliers,]
  }
  
  indep.vairables[,"Ambulation"] <- sqrt(indep.vairables[,"Ambulation"])
  indep.vairables <- indep.vairables[indep.vairables$Heat > 7,]
  
  heat.rf <- predict(model.rf, indep.vairables)
  
  heat.spline<- predict(model.spline, indep.vairables)
  heat.spline[heat.spline < 0] <- NA
  
  heat.glm <- predict(model.glm, indep.vairables)
  
  indep.vairables <- data.frame(indep.vairables, heat.spline = as.vector(heat.spline), heat.rf, heat.glm)
  
  indep.vairables$heat.mean <- rowMedians(as.matrix(indep.vairables[,c("heat.spline", "heat.rf", "heat.glm")]), na.rm = TRUE)
  
  ######################
  #General Calculations
  #####################
  SF <- indep.vairables$heat.mean/indep.vairables$Heat
  #SF <- ifelse(SF < 1, 1-(SF/2), SF)
  #SF <- ifelse(SF > 1, 1-(1-SF)/2, SF)
  indep.vairables$SF <- SF
  #Tendency to overestimate BMR if we do not use the scale factor
  indep.vairables$Ambulatory.EE <- (indep.vairables$Ambulation*0.3228)/indep.vairables$SF
  indep.vairables$ThermicEffect.EE <- (indep.vairables$Feed*8.4165)/indep.vairables$SF
  
  indep.vairables$Adaptive.EE <- indep.vairables$Heat - indep.vairables$heat.mean
  indep.vairables$Adaptive.EE[indep.vairables$Adaptive.EE < 0] <- 0
  
  indep.vairables$BMR.EE <- indep.vairables$Heat - (indep.vairables$Adaptive.EE + indep.vairables$Ambulatory.EE + indep.vairables$ThermicEffect.EE)
  #Impute mean BMR if estimated BMR > or < 1.5 z.score
  median.BMR <- mean(indep.vairables$BMR.EE, na.rm = T)
  sd.BMR <- sd(indep.vairables$BMR.EE)
  BMR.position <- which(indep.vairables$BMR.EE > median.BMR+1.5*sd.BMR | indep.vairables$BMR.EE < median.BMR-1.5*sd.BMR)
  indep.vairables$BMR.EE[BMR.position] <- median.BMR
  
  rho <- cor.test(indep.vairables$Heat, indep.vairables$heat.mean)
  annotations <- data.frame(
    xpos = c(-Inf),
    ypos =  c(Inf),
    annotateText = c(paste0("Thermoneutral rho = ", round(rho$estimate, 3))),
    hjustvar = c(-0.05) ,
    vjustvar = c(1))
  
  
  #indep.vairables <- na.omit(indep.vairables)
  
  EE.summary <- indep.vairables %>%
    group_by(Interval_num) %>%
    summarise(across(c(3,8:15), mean)) %>%
    as.data.frame()
  
  melted <- reshape2::melt(EE.summary[,c(1,7:10)], id.vars = 1)
  melted <- merge(melted, interval.assignments, by = 1)
  melted$variable <- factor(melted$variable, levels = c("Adaptive.EE", "Ambulatory.EE", "ThermicEffect.EE", "BMR.EE"))
  
  
  if(!by.proportion) {
    top.bar <- round(max(indep.vairables$Heat)) / median(table(melted$Interval_num))
  } else {
    top.bar = 1.2 / median(table(melted$Interval_num))
  }
  
  plot <- ggplot(melted, aes(x=as.numeric(Interval_num), y =as.numeric(value))) + 
    stat_summary(data = subset(melted, time.cycle == "night"), 
             geom = "bar", 
             fun = sum,
             mapping = aes(x=as.numeric(Interval_num), y =top.bar), 
             color = "grey", 
             lwd = 0, 
             alpha = 0.5) + 
    scale_fill_viridis(option = "H", discrete = TRUE, direction = -1) + 
    theme_classic() + 
    ylab("Heat") + 
    xlab("Time Interval") 
  if(!plot.area) {
    if (by.proportion) {
      plot <- plot + geom_bar(aes(fill = variable), stat = "identity", position = "fill")
    } else {
      plot <- plot + geom_bar(aes(fill = variable), stat = "identity")
    }
  }
  if (plot.area){
    if (by.proportion) {
      plot <- plot +stat_smooth(formula = y ~ s(x, bs = "cs"),
                                geom = 'area', 
                                method = 'gam', 
                                position = "fill",
                                aes(fill = variable))
    } else {
     plot <- plot +stat_smooth(formula = y ~ s(x, bs = "cs"),
                        geom = 'area', 
                        method = 'gam', 
                        position = "stack",
                        aes(fill = variable))
    }
  }
  plot <- plot + geom_text(data=annotations,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText), size = 2)
  return(plot)
}

plot.EE <- function(cage.data,
                    sample.subset = NULL, 
                    by.proportion = FALSE) {
  model.spline <- readRDS( "./models/model.earth.rds")
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
  for(i in c(1:3,7)) {
    x <- EnvStats::rosnerTest(as.matrix(indep.vairables[,i]), k = 10)$all.stats
    x <- x[x$Value >= 1,]
    x <- x[x$Outlier == TRUE,]
    x <- x$Obs.Num
    outliers <- c(outliers, x)
  }
  outliers <- na.omit(unique(outliers))
  if (length(outliers) > 0) {
    indep.vairables <- indep.vairables[-outliers,]
  }
  
  indep.vairables[,"Ambulation"] <- sqrt(indep.vairables[,"Ambulation"])
  indep.vairables <- indep.vairables[indep.vairables$Heat > 6,]
  
  heat.rf <- predict(model.rf, indep.vairables)
  
  heat.spline<- predict(model.spline, indep.vairables)
  heat.spline[heat.spline < 0] <- NA
  
  heat.glm <- predict(model.glm, indep.vairables)
  
  indep.vairables <- data.frame(indep.vairables, heat.spline = as.vector(heat.spline), heat.rf, heat.glm)
  
  indep.vairables$heat.mean <- rowMedians(as.matrix(indep.vairables[,c("heat.spline", "heat.rf", "heat.glm")]), na.rm = TRUE)
  SF <- indep.vairables$heat.mean/indep.vairables$Heat
  #SF <- ifelse(SF < 1, 1-(SF/2), SF)
  #SF <- ifelse(SF > 1, 1-(1-SF)/2, SF)
  indep.vairables$SF <- SF
  #Tendency to overestimate BMR if we do not use the scale factor
  indep.vairables$Ambulatory.EE <- (indep.vairables$Ambulation*0.3228)/indep.vairables$SF
  indep.vairables$ThermicEffect.EE <- (indep.vairables$Feed*8.4165)/indep.vairables$SF
  
  indep.vairables$Adaptive.EE <- indep.vairables$Heat - indep.vairables$heat.mean
  indep.vairables$Adaptive.EE[indep.vairables$Adaptive.EE < 0] <- 0
  
  indep.vairables$BMR.EE <- indep.vairables$Heat - (indep.vairables$Adaptive.EE + indep.vairables$Ambulatory.EE + indep.vairables$ThermicEffect.EE)
  #Impute mean BMR if estimated BMR > or < 1.5 z.score
  median.BMR <- mean(indep.vairables$BMR.EE, na.rm = T)
  sd.BMR <- sd(indep.vairables$BMR.EE)
  BMR.position <- which(indep.vairables$BMR.EE > median.BMR+1.5*sd.BMR | indep.vairables$BMR.EE < median.BMR-1.5*sd.BMR)
  indep.vairables$BMR.EE[BMR.position] <- median.BMR
  
  EE.summary <- indep.vairables %>%
    summarise(across(c(3,8:ncol(indep.vairables)), mean)) %>%
    as.data.frame()
  
  melted <- reshape2::melt(EE.summary[,c("Adaptive.EE", "Ambulatory.EE", "ThermicEffect.EE", "BMR.EE")])
  
  melted$variable <- factor(melted$variable, levels = c("Adaptive.EE", "Ambulatory.EE", "ThermicEffect.EE", "BMR.EE"))
  plot <- ggplot(melted, aes(x=1, y =as.numeric(value))) + 
    scale_fill_viridis(option = "H", discrete = TRUE, direction = -1) + 
    theme_classic() + 
    ylab("Heat") + 
    theme(axis.title.x = element_blank(), 
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank())
    if(by.proportion) {
      plot <- plot + geom_bar(aes(fill = variable), stat = "identity", position = "fill")
    } else {
    plot <- plot + geom_bar(aes(fill = variable), stat = "identity")
    }
  return(plot)
}

#################
##What if BMR is crazy?
########################
#off.BMR <- which(indep.vairables$BMR.EE < median.BMR-sd(indep.vairables$BMR.EE)*1.3)
#if (any(off.BMR)) {
#  for(i in seq_along(off.BMR)) {
#    indep.vairables$BMR.EE[off.BMR[i]] <- median.BMR
#    remainder.EE <- indep.vairables$Heat[off.BMR[i]] - (indep.vairables$BMR.EE[off.BMR[i]] + indep.vairables$Adaptive.EE[off.BMR[i]])
#    if(remainder.EE <= 0) {
#      indep.vairables$Ambulatory.EE[off.BMR[i]] <- 0
#      indep.vairables$ThermicEffect.EE[off.BMR[i]] <- 0
#      indep.vairables$Adaptive.EE[off.BMR[i]] <- 0
#    } else {
#      ratio <- indep.vairables$Ambulatory.EE[off.BMR[i]]/indep.vairables$ThermicEffect.EE[off.BMR[i]]
#      if (is.nan(ratio)) {
#        indep.vairables$ThermicEffect.EE[off.BMR[i]] <- remainder.EE/2
#        indep.vairables$Ambulatory.EE[off.BMR[i]] <- remainder.EE/2
#      } else if (is.infinite(ratio)) {
#        indep.vairables$ThermicEffect.EE[off.BMR[i]] <- remainder.EE
#      } else if (ratio == 0) {
#        indep.vairables$Ambulatory.EE[off.BMR[i]] <- remainder.EE
#      } else if (ratio < 1 & ratio != 0) {
#        ratio <- 1-ratio
#        indep.vairables$ThermicEffect.EE[off.BMR[i]] <- remainder.EE*ratio
#        indep.vairables$Ambulatory.EE[off.BMR[i]] <- remainder.EE - indep.vairables$ThermicEffect.EE[off.BMR[i]]
#      } else {
#        indep.vairables$ThermicEffect.EE[off.BMR[i]] <- remainder.EE/ratio
#        indep.vairables$Ambulatory.EE[off.BMR[i]] <- remainder.EE - indep.vairables$ThermicEffect.EE[off.BMR[i]]
#      }
#    }
#  }
#}


#off.BMR <- which(indep.vairables$BMR.EE > median.BMR+ sd(indep.vairables$BMR.EE)*1.3)
#if (any(off.BMR)) {
#  for(i in seq_along(off.BMR)) {
#    indep.vairables$BMR.EE[off.BMR[i]] <- median.BMR
#    remainder.EE <- indep.vairables$Heat[off.BMR[i]] - (indep.vairables$BMR.EE[off.BMR[i]] + indep.vairables$Adaptive.EE[off.BMR[i]])
#    if(remainder.EE <= 0) {
#      indep.vairables$Ambulatory.EE[off.BMR[i]] <- 0
#      indep.vairables$ThermicEffect.EE[off.BMR[i]] <- 0
#      indep.vairables$Adaptive.EE[off.BMR[i]] <- 0
#    } else {
#      ratio <- indep.vairables$Ambulatory.EE[off.BMR[i]]/indep.vairables$ThermicEffect.EE[off.BMR[i]]
#      if (is.nan(ratio)) {
#        indep.vairables$ThermicEffect.EE[off.BMR[i]] <- remainder.EE/2
#        indep.vairables$Ambulatory.EE[off.BMR[i]] <- remainder.EE/2
#      } else if (is.infinite(ratio)) {
#        indep.vairables$ThermicEffect.EE[off.BMR[i]] <- remainder.EE
#     } else if (ratio == 0) {
#        indep.vairables$Ambulatory.EE[off.BMR[i]] <- remainder.EE
#      } else if (ratio < 1 & ratio != 0) {
 #       ratio <- 1-ratio
#        indep.vairables$ThermicEffect.EE[off.BMR[i]] <- remainder.EE*ratio
#        indep.vairables$Ambulatory.EE[off.BMR[i]] <- remainder.EE - indep.vairables$ThermicEffect.EE[off.BMR[i]]
#      } else {
#        indep.vairables$ThermicEffect.EE[off.BMR[i]] <- remainder.EE/ratio
#        indep.vairables$Ambulatory.EE[off.BMR[i]] <- remainder.EE - indep.vairables$ThermicEffect.EE[off.BMR[i]]
#      }
#    }
#  }
#}
