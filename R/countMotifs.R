countMotifs <- function(network) {


g <- graph_from_data_frame(network, directed=TRUE)


# patterns
pattern_1 <- make_graph(c(1,2,1,3), directed=TRUE)
pattern_2 <- make_graph(c(1,2,3,2), directed=TRUE)
pattern_3 <- make_graph(c(1,2,2,3), directed=TRUE)
pattern_4 <- make_graph(c(1,2,2,3,3,2), directed=TRUE)
pattern_5 <- make_graph(c(1,2,2,1,2,3), directed=TRUE)
pattern_6 <- make_graph(c(1,2,2,1,2,3,3,2), directed=TRUE)
pattern_7 <- make_graph(c(1,2,2,3,1,3), directed=TRUE)
pattern_8 <- make_graph(c(1,2,2,3,3,1), directed=TRUE)
pattern_9 <- make_graph(c(1,2,1,3,2,3,3,2), directed=TRUE)

valid_patterns <- list(pattern_1, pattern_2, pattern_3, pattern_7)
motif_count <- vector(length = 4, mode = "numeric")
type <- 1

# make statistics
for (pattern in valid_patterns) {
  motif_count[type] <- count_subgraph_isomorphisms(pattern, g)
  type <- type + 1
}

return(motif_count)

}
