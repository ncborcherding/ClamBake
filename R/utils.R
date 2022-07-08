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

subset.data <- function(orginal.dat, samples) {
  original.dat[[1]] <- original.dat[[1]][-which(names(original.dat[[1]]) %!in% samples)]
  original.dat[[2]] <- original.dat[[2]][original.dat[[2]]$filename %in% samples,]
  
}