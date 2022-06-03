pathIdentification <- function(edgelist, node = 'gene1', upper_bound = 1000) {
  current_path_list <- vector(mode = 'list', upper_bound)
  index <- 1
  # identify promoters
  for (i in 1:nrow(edgelist)) {
    if (edgelist[i,2] == node) {
      # identify enhancers
      enhancers <- unlist(enhancerIdentification(edgelist, edgelist[i,1]))
      for (j in 1:length(enhancers)) {
        current_path_list[[index]] <- c(enhancers[j],edgelist[i,1])
        index <- index + 1
      }
    }
  }
  current_path_list <- current_path_list[lengths(current_path_list) != 0]

  # remove redundant paths
  enhancers_set <- sapply(current_path_list, "[[", 1)
  enhancers_set <- enhancers_set[!duplicated(enhancers_set)]
  for (i in 1:length(current_path_list)) {
    if(!is.na(match(current_path_list[[i]][2], enhancers_set)) == TRUE){
      current_path_list[[i]] <- NA
    }
  }
  current_path_list <- current_path_list[is.na(current_path_list) == 0]
  return(current_path_list)
}


