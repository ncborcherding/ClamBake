merge.data <- function(list.data) {
  
  for(i in seq_along(list.data)) {
    runs <- list.data[[i]][[1]]
    meta <- list.data[[i]][[2]]
    if(i == 1) {
      new.run.list <- runs
      new.meta <- meta
    } else {
      new.run.list <- c(new.run.list, runs)
      col.match <- intersect(colnames(meta), colnames(new.meta))
      meta <- meta[colnames(meta) %in% col.match,]
      new.meta <- new.meta[colnames(new.meta) %in% col.match,]
      new.meta <- rbind(new.meta,meta)
    }
  }
  new_data <- list(cage_data = new.run.list, 
                   meta_data = new.meta)
  return(new_data)
}