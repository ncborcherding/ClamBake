library(stringr)
load.data <- function(file.dir, 
                      meta.file,
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
  } else {
    meta.file <- NA
  }
  
  #######################
  #Creating Return Object
  #######################
  final.object <- list(cage_data = list)
  if(!is.null(meta.file)) {
    final.object[[2]] <- meta.file
  }
  names(final.object)[2] <- "meta_data"
  
  return(final.object)
}