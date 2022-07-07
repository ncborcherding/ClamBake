library(stringr)
library(dplyr)
load.data <- function(file.dir, 
                      meta.file,
                      equilibrate.interval = 8,
                      contrast = NULL,
                      baseline = NULL,
                      vars.to.contrast = NULL,
                      headers = NULL) {
  
  ##########################
  #Set headers for the files
  ##########################
  if(!is.null(headers)) {
    colnames.to.be <- headers
  } else {
    colnames.to.be <- c("Interval_num", "Cage", "Time", 
                        "VO2", "O2in", "O2out", "DO2", "ACCO2", 
                        "VCO2", "CO2in", "CO2out", "DCO2", "ACCCO2", 
                        "RER", "Heat", "Flow", "Pressure", "Status",
                        "Feed", "Feed_acc", "Drink", "Drink_acc",
                        "Xtot", "Xamb", "Ytot", "Yamb", "Ztot", 
                        "Resp", "RH_samp", "RH_ref", "RH_cage", 
                        "Cage_temp", "Enclosure_temp", "Setpoint", 
                        "LED_hue", "LED_saturation", "LED_percent")
  }
  ###################
  #Loading Cage Data
  ###################
  files.to.load <- list.files(file.dir, pattern = "CSV|csv")
  list <- lapply(files.to.load, FUN = function(x) { 
    tmp <- read.csv(paste0(file.dir, "/", x), skip =25, header = FALSE) 
    colnames(tmp) <- colnames.to.be
    #Little off the top for equilibration purposes
    if(!is.null(equilibrate.interval)) {
      tmp <- tmp[-seq_len(equilibrate.interval),]
    }
    tmp <- tmp[,seq_len(length(colnames.to.be))] #Only selected columns
    tmp <- tmp[-nrow(tmp),] #remove ":EVENT"
    tmp$filename <- str_remove(files.to.load[x], ".csv|.CSV")
    tmp
  })
  files.to.load <- str_remove(files.to.load, ".csv|.CSV")
  names(list) <- files.to.load
  
  ##################
  #Loading Meta Data
  ##################
  if(!is.null(meta.file)) {
    meta.file <- read.csv(meta.file)
    meta.file$filename <- str_remove(meta.file$filename, ".csv|.CSV")
    if(!is.null(contrast)) {
      meta.file <- make.contrast(meta.file, 
                                 contrast, 
                                 baseline,
                                 vars.to.contrast)
    }
  } else {
    meta.file <- NA
  }
  
  #######################
  #Creating Return Object
  #######################
  final.object <- list(cage_data = list, meta_data = meta.file)
  
  return(final.object)
}

make.contrast <- function(meta.file, 
                          contrast, 
                          baseline,
                          vars.to.contrast) {
  if(contrast %!in% colnames(meta.file)) {
    stop("Contrasting variable not present to base calculation on")
  }
  # Baseline either entered above or selected alphabetically
  if (is.null(baseline)) {
    baseline <- sort(unique(meta.file[,contrast]))[1]
  }
  #Selecting variables to contrast
  #If not entered will select numerical values
  contrast.idx <- which(colnames(meta.file) == contrast)
  if (is.null(vars.to.contrast)) {
    vars.to.contrast <- names(which(sapply(meta.file, is.numeric)))
    vars.index <- unname(which(sapply(meta.file, is.numeric)))
    vars.to.contrast <- vars.to.contrast[-1]
    vars.idx <- vars.index[-1]
  }
  #Looping through the vars.to.contrast to find differentce
  for(i in seq_along(vars.to.contrast)) {
    tmp <- meta.file[,c(colnames(meta.file)[1],vars.to.contrast[i], contrast)] 
    colnames(tmp)[2] <- "value"
    diff.values <- tmp %>%
      summarise(delta = value - value[tmp[,3] == baseline])
    
    #Preponderance of 0s detector
    where.to.remove <- which(diff.values == 0)
    group.size <- nrow(diff.values)/2
    teamA <- which(where.to.remove %in% seq_len(group.size))
    if(length(teamA) == group.size) {
      diff.values <- diff.values[-seq_len(group.size),]
    } else {
      diff.values <- diff.values[-c(group.size+1:nrow(diff.values)),]
    }
    names(diff.values) <- unique(meta.file[,colnames(meta.file)[1]])
    if (i == 1) {
      contrasted.values <- diff.values
    } else {
      contrasted.values <- rbind(contrasted.values, diff.values)
    }
  }
  new.tmp <- unique(meta.file[,-which(colnames(meta.file) %in% c(vars.to.contrast, contrast))])
  contrasted.values <- as.data.frame(t(contrasted.values))
  colnames(contrasted.values) <- vars.to.contrast
  new.tmp <- cbind(new.tmp, contrasted.values)
  return(new.tmp)
}
