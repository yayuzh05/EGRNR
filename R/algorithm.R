#' algorithm
#'
#' Reconstruct the EGRN using Bayesian Network
#'
#'
#'
#'
#' @return strength table
#' @export
#'
#' @examples hello("Alice")

inference <- function(data, gene_name = "gene1", algorithm_name = "aracne", criterion = "bic", threshold = 0.2){
  col <- colnames(data)
  col1 <- grepl("ca", col)
  col2 <- grepl("Cg", col)
  num1 <- match(TRUE, col1)
  num2 <- match(TRUE, col2)
  col <- paste0(c(rep("gene", 1),rep("ca", num2-num1),rep("cg", length(col),length(col)-num2+1)),1:length(col))
  colnames(data) <-col
  print(length(col))
  arcs = bnlearn::boot.strength(data, algorithm = algorithm_name)

  edge_num <- length(rownames(arcs[arcs$to == 'gene1' & arcs$strength > threshold, ]))
  if(edge_num == 0) {
    edge_num <- length(rownames(arcs[arcs$to == 'gene1' & arcs$strength > 0.1, ]))
    if(edge_num == 0) {
      print("No significant edges")
      edge <- arcs[arcs$to == 'gene1' & arcs$strength > 0, ]
      new_arcs <- edge
    }else{
      edge <- arcs[arcs$to == 'gene1' & arcs$strength > 0.1, ]
      whitelist <- edge[,1:2]
      res = bnlearn::hc(data,whitelist = whitelist)
      new_arcs <- data.frame(res$arcs)
    }
  }else{
    edge <- arcs[arcs$to == 'gene1' & arcs$strength > threshold, ]
    whitelist <- edge[,1:2]
    res = bnlearn::hc(data,whitelist = whitelist)
    new_arcs <- data.frame(res$arcs)
  }

  new_arcs <- getClipedGraph(new_arcs, col)

  e = bnlearn::empty.graph(col)
  bnlearn::arcs(e) = sapply(new_arcs[,1:2], as.character)
  strength <- bnlearn::arc.strength(e, data, criterion = criterion)
  return(strength)
}
