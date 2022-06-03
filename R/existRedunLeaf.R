existRedunLeaf <- function(arcs, nodes, whitelist){
  end_nodes <- setdiff(nodes,unique(arcs$from)) # nodes that has no children
  link_nodes <- unique(arcs$to) # nodes that have at least a link to other nodes
  leaf_nodes <- intersect(end_nodes, link_nodes)
  leaf_nodes <- setdiff(leaf_nodes, whitelist)
  return(leaf_nodes)
}
