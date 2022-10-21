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
library(chron)
time.handler <- function(pre.plot.dat) {
 
  dtime <- str_split(pre.plot.dat$Time, " ", simplify = TRUE)[,2]
  no.dates <- which(dtime %in% c("AM", "PM"))
  actual.time <- str_split(pre.plot.dat$Time[no.dates], " ", simplify = TRUE)[,1]
  dtime[no.dates] <- actual.time
  dtime <- chron(time = dtime, format = c("h:m:s"))
  
  dtime2 <- as.POSIXct(dtime, format = "%H:%M:%S")
  time.of.day <- str_split(pre.plot.dat$Time, " ", simplify = TRUE)[,3]
  dtime2 <- ifelse(time.of.day == "PM", dtime2 + 12*60*60, dtime)
}