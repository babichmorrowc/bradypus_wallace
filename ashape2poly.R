# Function to convert alpha hull to a polygon

ashape2poly <- function(hull){
  require(alphahull)
  require(igraph)
  hull$ashape.obj$edges[,1] <- as.character(hull$ashape.obj$edges[,1])
  hull_graph <- graph_from_edgelist(hull$ashape.obj$edges[,1:2], directed = FALSE)
  if (!is.connected(hull_graph)) {
    stop("Graph not connected")
  }
  if (any(degree(hull_graph) != 2)) {
    stop("Graph not circular")
  }
  if (clusters(hull_graph)$no > 1) {
    stop("Graph composed of more than one circle")
  }
  cut_graph <- hull_graph - E(hull_graph)[1]
  # find chain end points
  ends = names(which(degree(cut_graph) == 1))
  path = get.shortest.paths(cut_graph, ends[1], ends[2])$vpath[[1]]
  # this is an index into the points
  pathX = as.numeric(V(hull_graph)[path]$name)
  # join the ends
  pathX = c(pathX, pathX[1])
  return(pathX)
}

poly <- ahull2poly(var_bg_alpha20)

# now show the alpha shape plot with our poly on top
plot(var_bg_alpha20, lwd = 10, col = "gray")
# get the points from the ashape object
lines(var_bg_alpha20$xahull[pathX, ], lwd = 2)
