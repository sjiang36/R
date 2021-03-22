library(igraph)
library(tidyverse)
library(e1071)
library(Metrics)
te <- as.data.frame(read_csv("~/Desktop/658/network.csv"))%>% mutate_all(as.character)
nodes<- read_csv("~/Desktop/658/nodes.csv")
g1 <- graph.data.frame(te, directed = F)
pos_sample_frac <- 0.10
pos_edges <- sample(E(g1), pos_sample_frac * ecount(g1))
pos_edges <- as_ids(pos_edges) # as_ids actually returns node names if the names are available
pos_edges <- as.data.frame(pos_edges)
pos_edges <- pos_edges %>% separate( pos_edges, c('X1', 'X2'))

X1 <- as_ids(sample(V(g1), 256)) # as_ids actually returns node names if the names are available
X2 <- as_ids(sample(V(g1), 256)) # as_ids actually returns node names if the names are available
neg_edges <- cbind(X1, X2)
neg_edges <- as.data.frame(neg_edges)
neg_edges <- as.data.frame(t(apply(neg_edges, 1, sort))) %>% mutate_all(as.character)
colnames(neg_edges) <- c('X1', 'X2')
neg_edges <- neg_edges %>% anti_join(te) %>% filter(X1 != X2)
pos_edges <- transform(pos_edges,y=1)
neg_edges <- transform(neg_edges,y=0)
pn_edges <- as.data.frame(rbind(pos_edges,neg_edges))
nodes <- sapply(nodes, toString)
tese_n <- nodes[1:100]
nodes[1]
find_possible_neighbours <- function(node_name){
  current_neighbours <- c(toString(node_name), as.vector(neighbors(g1, toString(node_name))$name))
  possible_neighbours <- setdiff(as_ids(ego(g1, nodes=node_name, order=3)[[1]]), current_neighbours)
  return(data.frame(node_name, possible_neighbours, stringsAsFactors = FALSE))
}
find_possible_neighbours(nodes[60])[1:5, ]
test_e <- lapply(tese_n, find_possible_neighbours)
test_e <- bind_rows(test_e)
colnames(test_e) <- c('X1','X2')
dice_sim <- similarity.dice(g1) 
adamic_adar_sim <- similarity.invlogweighted(g1)
jaccard_sim <- similarity.jaccard(g1)
# name the rows and cols of the similarity matrices using node names
node_names = as.vector(V(g1)$name)
rownames(dice_sim) <- node_names
colnames(dice_sim) <- node_names
rownames(adamic_adar_sim) <- node_names
colnames(adamic_adar_sim) <- node_names
rownames(jaccard_sim) <- node_names
colnames(jaccard_sim) <- node_names

nodes_sim <- function(a, b, sim_matrix){
  return(sim_matrix[cbind(a, b)])
}
# compute jaccard and adamic-adar sim for a pair of nodes
nodes_sim('1', '151', dice_sim)
nodes_sim('4', '210', jaccard_sim)
nodes_sim('12', '57', adamic_adar_sim)


pn_edges <- pn_edges %>% mutate(dice = nodes_sim(X1, X2, dice_sim))
pn_edges <- transform(pn_edges,jaccard=nodes_sim(X1,X2,jaccard_sim),invlogweighted=nodes_sim(X1, X2, adamic_adar_sim))




pn_edges <- pn_edges %>% mutate(degree1 = degree(g1,v=X1,normalized=T))
pn_edges <- pn_edges %>% mutate(degree2 = degree(g1,v=X2,normalized=T))
pn_edgess <- pn_edges %>% mutate(betweenness1 = betweenness(g1,v=X1,normalized=T))
pn_edges <- pn_edgess %>% mutate(betweenness2 = betweenness(g1,v=X2,normalized=T))



reg_model <- lm("y~dice+jaccard+invlogweighted+degree1+degree2+betweenness1+betweenness2", data = pn_edges)
test_e <- test_e %>% mutate(dice = nodes_sim(X1, X2, dice_sim))
test_e <- test_e %>% mutate(jaccard=nodes_sim(X1,X2,jaccard_sim))
test_e <- test_e %>% mutate(invlogweighted=nodes_sim(X1, X2, adamic_adar_sim))
test_e <- test_e %>% mutate(degree1 = degree(g1,v=X1,normalized=T))
test_e <- test_e %>% mutate(degree2 = degree(g1,v=X2,normalized=T))
test_e <- test_e %>% mutate(betweenness1 = betweenness(g1,v=X1,normalized=T))
test_e <- test_e %>% mutate(betweenness2 = betweenness(g1,v=X2,normalized=T))





test_e$p_hat <- predict(reg_model, test_e, type='response')

recommend_links <- function(node_name, n_recommend = 6){
  # for a given node find the 10 nodes with highest p_pat and predict the links 
  recommended_nodes <- test_e %>% filter(X1 == node_name) %>% top_n(n=n_recommend, wt=p_hat) %>% .$X2
  return(recommended_nodes)
}

recommend_links(nodes[30])



