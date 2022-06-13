pathIdentification <- function(edgelist, node = 'gene1', upper_bound = 1000) {
  current_path_list <- vector(mode = 'list', upper_bound)
  index <- 1
  # identify promoters
  for (i in 1:nrow(edgelist)) {
    if (edgelist[i,2] == node) {
      # identify promoters
      promoters <- unlist(promoterIdentification(edgelist, edgelist[i,1]))
      for (j in 1:length(promoters)) {
        current_path_list[[index]] <- c(promoters[j],edgelist[i,1])
        index <- index + 1
      }
    }
  }
  current_path_list <- current_path_list[lengths(current_path_list) != 0]

  # remove redundant paths
  promoters_set <- sapply(current_path_list, "[[", 1)
  promoters_set <- promoters_set[!duplicated(promoters_set)]
  for (i in 1:length(current_path_list)) {
    if(!is.na(match(current_path_list[[i]][2], promoters_set)) == TRUE){
      current_path_list[[i]] <- NA
    }
  }
  current_path_list <- current_path_list[is.na(current_path_list) == 0]
  return(current_path_list)
}

