
bin.energy <- function(test.set, group.by, split.by) {
  output <- NULL
  for(i in seq_len(nrow(test.set))) {
    predict.value <- test.set[i,]$Feed* 9.4248 + test.set[i,]$Drink*0.6929 + test.set[i,]$Ambulation* 0.3777 + test.set[i,]$Temp.Delta*-0.919  + 8.35
    measured.value <- test.set[i,]$Heat
    scale.value <- measured.value/predict.value
    
    TEF <- (test.set[i,]$Feed* 9.4248 + test.set[i,]$Drink*0.6929)*scale.value
    Act <- (test.set[i,]$Ambulation* 0.377)*scale.value
    AT <- (test.set[i,]$Temp.Delta*-0.9235)*scale.value
    if (AT < 0) {
      AT <- 0
    }
    BMR <- measured.value - TEF - Act - AT
    out <- c(BMR, TEF, Act, AT, measured.value, predict.value, scale.value)
    names(out) <- c("BMR", "TEF", "Act", "AT", "measured.Heat", "predict.Heat", "scale.factor")
    if(!is.null(group.by)) {
      group <- test.set[i,]$group.by
      out <- c(out,"group.by" = group)
    } 
    if(!is.null(group.by)) {
      split <- test.set[i,]$split.by
      out <- c(out,"split.by" = split)
    }
    
    output <- rbind(output, out)
  }
  output <- as.data.frame(output)
  
  return(output)
}
