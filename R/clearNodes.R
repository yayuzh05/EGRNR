clearNodes <- function(arcs, node){
  new_arcs <- arcs[arcs$to != node, ]
  return(new_arcs)
}
