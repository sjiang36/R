
library(igraph)

g <- read.graph(file = "state_graph.gml", format = "gml")
plot(g, layout=layout.fruchterman.reingold)
plot(g, layout=layout.circle)

community <- edge.betweenness.community(g, directed=F)
community$membership
plot(as.dendrogram(community))

community2 <- fastgreedy.community(g)
community2$membership
community2$modularity # why 5 communities?
plot(as.dendrogram(community2))
cut_at(community2, 3) # returns a 3-community solution

set.seed(1) # Since layout.fruchterman.reingold is a random algorithm, set seed can 'fix' the layout
# 5-community solution (default)
plot(g,
     vertex.color = community2$membership, vertex.size = log(degree(g) + 1),
     mark.groups = by(seq_along(community2$membership), community2$membership, invisible),
     layout=layout.fruchterman.reingold)
# 3-communitiy solution
plot(g,
     vertex.color = cut_at(community2, 3), vertex.size = log(degree(g) + 1),
     mark.groups = by(seq_along(cut_at(community2, 3)), cut_at(community2, 3), invisible),
     layout=layout.fruchterman.reingold)


# combine the nodes to meta-nodes according to communities
comm.graph <- contract.vertices(g, community2$membership, vertex.attr.comb=list(size="sum", "ignore"))
plot(comm.graph)
comm.graph <- simplify(comm.graph)
plot(comm.graph)

# maximize the modularity over all possible solution (using integer programming)
community3 <- cluster_optimal(g)
community3$modularity
plot(g,
     vertex.color = community3$membership, vertex.size = log(degree(g) + 1),
     mark.groups = by(seq_along(community3$membership), community3$membership, invisible),
     layout=layout.fruchterman.reingold)

# Bonus: Spectral Clustering
L <- embed_laplacian_matrix(g, no = 2, type="D-A")
L$X
k_means_sol <- kmeans(L$X, centers = 8)
k_means_sol$cluster
plot(g,
     vertex.color = k_means_sol$cluster, vertex.size = log(degree(g) + 1),
     mark.groups = by(seq_along(k_means_sol$cluster), k_means_sol$cluster, invisible),
     layout=layout.fruchterman.reingold)


