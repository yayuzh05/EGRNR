pathConsistency <- function(path_list, labels = NA){
  if (is.na(labels)) {
    if (length(names(path_list)) == 0) {
      labels <- as.character(1:length(path_list))
    }
    else {
      labels <- names(path_list)
    }
  }
  all_paths <- unique(unlist(path_list, recursive = FALSE))
  consistency_matrix <- data.frame(matrix(ncol = length(path_list), nrow = length(path_list)))
  colnames(consistency_matrix) <- labels
  for (i in 1:length(path_list)){
    for (j in 1:length(path_list)) {
      consistency_matrix[i,j] <- length(intersect(path_list[[i]], path_list[[j]]))/length(all_paths)
    }
  }
  return(consistency_matrix)
}
