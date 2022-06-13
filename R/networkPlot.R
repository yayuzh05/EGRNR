networkPlot <- function(strength, title)
{
  g <- igraph::from_data_frame(strength, directed=TRUE)
  e <- as_edgelist(g, names = TRUE)
  eName <-paste0(e[,1], "_", e[,2])
  colnames(e) <- c("from","to")
  eDf <- merge(e, strength)
  rownames(eDf) <- paste0(eDf$from, "_", eDf$to)
  eDf <- eDf[eName, ]
  g <- set.edge.attribute(g, "color", index=E(g), 1-eDf$strength/max(eDf$strength))
  g <- set.edge.attribute(g, "label", index=E(g), NA)

  V(g)$shape <- rep(19, length(V(g)))

  E(g)$width <- E(g)$color
  edgeWName <- "strength"



  ## Node

  gene <- grepl("gene", names(V(g)))
  ca <- grepl("ca", names(V(g)))
  cg <- grepl("cg", names(V(g)))
  group <- gene + ca*2 + cg*3

  V(g)$color[group==1] <- "gene"
  V(g)$color[group==2] <- "ca"
  V(g)$color[group==3] <- "cg"
  V(g)$size[group==1] <- 10
  V(g)$size[group==2] <- 7
  V(g)$size[group==3] <- 7

  ## Plot

  p <- ggraph(g, layout="nicely") + geom_edge_diagonal(edge_alpha=1,
                                                       position="identity",
                                                       aes_(edge_colour=~I(color), width=~I(width), label=~I(label)),
                                                       label_size=3*(4/4),
                                                       label_colour=NA,
                                                       angle_calc = "along",
                                                       label_dodge=unit(3,'mm'),
                                                       arrow=arrow(angle=20, length=unit(3, 'mm'), type = "closed"),
                                                       end_cap=circle(1, 'mm'))+
    geom_node_point(aes_(color=~color, size = 10, shape=~shape), show.legend=TRUE)+
    scale_colour_manual(values = c("red", "yellow", "green"), name='node type') +
    scale_edge_width(range=c(1.2, 1.5), guide="none")+
    scale_edge_color_continuous(low="dodgerblue", high="tomato", name="strength")+
    guides(edge_color = guide_edge_colorbar(title.vjust = 3))+
    # geom_node_text(aes_(label=~name), check_overlap=TRUE, repel=TRUE, size = labelSize) +
    scale_shape_identity()+
    theme_graph()+ ggtitle(title)

  p <- p + geom_node_text(aes_(label=~stringr::str_wrap(name, width = 25)),
                          check_overlap=TRUE, repel=TRUE, size = 4)

  return(p)
}

