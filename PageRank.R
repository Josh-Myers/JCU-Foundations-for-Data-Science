# Page rank code
library(igraph)
edges <- c(1,2, 2,1, 4,2, 3,2, 1,3) # 1st edge = (1,2), 2nd edge = (2,1), ..., 5th edge = (1,3)
my_graph <- make_graph(edges, directed = TRUE) # Create graph with directed edges

PR <-page_rank(my_graph)
PR$vector # The scores are stored in var. "vector"

my_adjacency_matrix<-as_adjacency_matrix(my_graph)
my_adjacency_matrix

AdjMat_transposed<-t(as.matrix(my_adjacency_matrix)) # t() computes the transpose of a matrix
AdjMat_transposed

P <- apply(AdjMat_transposed, MARGIN = 2, FUN = function(x){x/sum(x)})
P

n <- gorder(my_graph) # Number of vertices
no_it <- 30 # Number of iterations
v0 <- rep(1,n)/n # Probability that each page is chosen at random by the surfer to start
v <- v0
for(i in 1:no_it){v <- P %*% v} # Iterate v = P*v for no_it= 30 iterations
v

n <- gorder(my_graph) # Number of vertices
no_it <- 30 # Number of iterations
v0 <- rep(1,n)/n # Probability that each page is chosen at random by the surfer to start
d <- 0.85 # Damping factor
v <- v0
Z <- (1 - d)*v0
for(i in 1:no_it){v <- d*(P %*% v) + Z} # Iterate v = d*(P*v) + (1–d)*v0 for no_it= 30 iterations
v


# Do the previous exercise item to get matrix P, then run:
edges <- c(2,1, 2,3, 3,4, 4,3, 5,2, 5,3, 5,6, 6,3, 6,5, 7,5, 8,5, 9,5, 9,3, 10,5, 10,3, 11,5, 11,3) # Vertex 1 is a "spider trap" (or "sink": it has no outgoing edge)
my_graph <- make_graph(edges, directed = TRUE)

artificial_edges <- c(1,2, 1,3, 1,4, 1,5, 1,6, 1,7, 1,8, 1,9, 1,10, 1,11) # Artificial edges from vertex 1 (sink) to every other
edges <- c(edges, artificial_edges)
my_augmented_graph <- make_graph(edges, directed = TRUE) # Augmented graph with artificial edges

my_augmented_adj_mat <- as_adjacency_matrix(my_augmented_graph)
my_adj_mat_transposed <- t(as.matrix(my_augmented_adj_mat))
Pc <- apply(my_adj_mat_transposed, MARGIN = 2, FUN = function(x){x/sum(x)}) # Transition Matrix
Pc

n <- 11          # Number of Vertices
no_it <- 20      # Number of Iterations
v0 <- rep(1,n)/n # Initial (even) Probabilities
d <- 0.85        # Damping Factor
v <- v0
Z <- (1 - d)*v0
for (i in 1:no_it){v <- d*(Pc %*% v) + Z} # Iterate 20 iterations
as.vector(v) # Result Iterative PageRank (Graph with Artificial Edges)
page_rank(my_augmented_graph)$vector # Built-in PageRank (Graph with Artificial Edges)
page_rank(my_graph)$vector # Built-in PageRank (Graph without Artificial Edges)
