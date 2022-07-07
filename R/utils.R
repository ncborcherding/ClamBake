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