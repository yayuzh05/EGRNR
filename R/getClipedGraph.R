getClipedGraph <- function(arcs, nodes, whitelist = c('gene1')) {
  while(length(existRedunLeaf(arcs, nodes, whitelist)) != 0) {
    for(i in existRedunLeaf(arcs, nodes, whitelist)) {
      arcs <- clearNodes(arcs, i)
    }
  }
  return(arcs)
}
