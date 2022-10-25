"%!in%" <- Negate("%in%")

pull.dat <- function(header.list, original.dat) {
  meta <- original.dat[[2]]
  for(i in seq_along(original.dat[[1]])) {
    list.tmp.it <- original.dat[[1]][[i]]
    for(j in seq_along(header.list)) {
      list.tmp.it[,header.list[j]] <- unique(meta[which(meta$filename == names(original.dat[[1]])[i]), header.list[j]])
    }
    if(i == 1) {
      out.data <- list.tmp.it 
    } else {}
    out.data <- rbind(out.data, list.tmp.it)
  }
  return(out.data)
}

subset.data <- function(original.dat, samples) {
  original.dat[[1]] <- original.dat[[1]][-which(names(original.dat[[1]]) %!in% samples)]
  original.dat[[2]] <- original.dat[[2]][original.dat[[2]]$filename %in% samples,]
  return(original.dat)
}

library(stringr)
time.handler <- function(pre.plot.dat) {
  dtime <- str_split(pre.plot.dat$Time, " ", simplify = TRUE)[,2]
  no.dates <- which(dtime %in% c("AM", "PM"))
  actual.time <- str_split(pre.plot.dat$Time[no.dates], " ", simplify = TRUE)[,1]
  dtime[no.dates] <- actual.time
  #dtime <- chron(time = dtime, format = c("h:m:s"))
  
  dtime2 <- as.POSIXct(dtime, format = "%H:%M:%S")
  time.of.day <- str_split(pre.plot.dat$Time, " ", simplify = TRUE)[,3]
  noon <- intersect(grep("12", str_split(dtime, ":", simplify = TRUE)[,1]), which(time.of.day == "PM"))
  midnights <- intersect(grep("12", str_split(dtime, ":", simplify = TRUE)[,1]), which(time.of.day == "AM"))
  #Adding 12 hours to PM times or exactly midnight
  dtime2[which(time.of.day %in% c("PM", ""))] <- dtime2[which(time.of.day %in% c("PM", ""))] + 12*60*60
  #Removing 12 hours from first hour past midnight
  
  dtime2[midnights] <- dtime2[midnights] -  12*60*60
  
  dtime2[noon] <- dtime2[noon] -  12*60*60
  
  day <- which(dtime2 >= as.POSIXct("06:00:00", format = "%H:%M:%S") & dtime2 <= as.POSIXct("18:00:00", format = "%H:%M:%S"))
  pre.plot.dat$time.cycle <- "night"
  pre.plot.dat$time.cycle[day] <- "day"
  pre.plot.dat <- pre.plot.dat[,c("Interval_num", "time.cycle")]
  return(pre.plot.dat)
}
