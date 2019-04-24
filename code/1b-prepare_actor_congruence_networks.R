#--------------------------------------------------------------------
# Us and Them: Mapping Discourse Coalitions in the EU Copyright Debate
# Bruno St-Jacques (Hertie School of Governance)
# 2019
#--------------------------------------------------------------------

# compute congruence network (agreement/disagreement b/w organizations)
congruence_A <- dna_network(copyright,
                            networkType = "onemode",
                            statementType = "DNA Statement",
                            normalization = "average",
                            variable1 = "organization",
                            variable2 = "concept",
                            qualifier = "agreement",
                            qualifierAggregation = "congruence",
                            duplicates = "document")
# convert to network object
congA.net <- network(congruence_A)

### congruence network of actors over time (compounded) ###

# 2017
congruence_A2017 <- dna_network(copyright,
                                networkType = "onemode",
                                statementType = "DNA Statement",
                                normalization = "average",
                                variable1 = "organization",
                                variable2 = "concept",
                                qualifier = "agreement",
                                qualifierAggregation = "congruence",
                                duplicates = "document",
                                start.date = "01.01.2017",
                                stop.date = "26.05.2018") # Council agrees on position
congA2017.net <- network(congruence_A2017)

sub2017 <- as.data.frame(congruence_A2017)
sub2017 <- tibble::rownames_to_column(sub2017, var = "rowname")
setnames(sub2017, "rowname", "value")
sub2017 <- select(sub2017, "value")
at2017 <- select(atA, "value", "type")
subatA2017 <- inner_join(at2017, sub2017) # list of actors and type of organisations

# 2018a
congruence_A2018a <- dna_network(copyright,
                                 networkType = "onemode",
                                 statementType = "DNA Statement",
                                 normalization = "average",
                                 variable1 = "organization",
                                 variable2 = "concept",
                                 qualifier = "agreement",
                                 qualifierAggregation = "congruence",
                                 duplicates = "document",
                                 start.date = "01.01.2017",
                                 stop.date = "13.09.2018") # Parliament agrees on position
congA2018a.net <- network(congruence_A2018a)

sub2018a <- as.data.frame(congruence_A2018a)
sub2018a <- tibble::rownames_to_column(sub2018a, var = "rowname")
setnames(sub2018a, "rowname", "value")
sub2018a <- select(sub2018a, "value")
at2018a <- select(atA, "value", "type")
subatA2018a <- inner_join(at2018a, sub2018a) # list of actors and type of organisations

# 2018b
congruence_A2018b <- dna_network(copyright,
                                 networkType = "onemode",
                                 statementType = "DNA Statement",
                                 normalization = "average",
                                 variable1 = "organization",
                                 variable2 = "concept",
                                 qualifier = "agreement",
                                 qualifierAggregation = "congruence",
                                 duplicates = "document",
                                 start.date = "01.01.2017",
                                 stop.date = "04.02.2019") # final Trilogue announced
congA2018b.net <- network(congruence_A2018b)

sub2018b <- as.data.frame(congruence_A2018b)
sub2018b <- tibble::rownames_to_column(sub2018b, var = "rowname")
setnames(sub2018b, "rowname", "value")
sub2018b <- select(sub2018b, "value")
at2018b <- select(atA, "value", "type")
subatA2018b <- inner_join(at2018b, sub2018b) # list of actors and type of organisations

#2019
congruence_A2019 <- dna_network(copyright,
                                networkType = "onemode",
                                statementType = "DNA Statement",
                                normalization = "average",
                                variable1 = "organization",
                                variable2 = "concept",
                                qualifier = "agreement",
                                qualifierAggregation = "congruence",
                                duplicates = "document",
                                start.date = "01.01.2017",
                                stop.date = "31.12.2019") # directive adopted
congA2019.net <- network(congruence_A2019)

sub2019 <- as.data.frame(congruence_A2019)
sub2019 <- tibble::rownames_to_column(sub2019, var = "rowname")
setnames(sub2019, "rowname", "value")
sub2019 <- select(sub2019, "value")
at2019 <- select(atA, "value", "type")
subatA2019 <- inner_join(at2019, sub2019) # list of actors and type of organisations

#--------------------------------------------------------------------

##### IGRAPH #####

net <- dna_toIgraph(congruence_A, weighted = TRUE)

## full congruence network ##
net <- simplify(net, remove.multiple = F, remove.loops = T)

#set attributes
net <- set_vertex_attr(net, "type", index = V(net), as.character(atA$type))

# Compute node degrees (#links) and use that to set node size:
deg <- degree(net, mode="all")
V(net)$size <- deg/4
# Set edge width based on weight:
E(net)$width <- E(net)$weight*1.5
summary(E(net)$weight)

# edge threshold to remove low-intensity ties without discriminating against organisations with a low profile
#net <- delete.edges(net, which(E(net)$weight <0.20962)) # 1st Q value of edges
net <- delete.edges(net, which(E(net)$weight <0.30769)) # median value of edges

# remove EU Committee on Civil Liberties, Justice and Home Affairs (outsider)
net <- delete.vertices(net, "EU Committee on Civil Liberties, Justice and Home Affairs")

# discourse coalitions

# community detection
clp <- cluster_optimal(net) # justify why use this community detection algorithm
com <- cluster_fast_greedy(net)
edgebet <- cluster_edge_betweenness(net)
walk <- cluster_walktrap(net)
multi <- multilevel.community(net)
cle <- cluster_leading_eigen(net)

sizes(clp)
sizes(com)
sizes(edgebet)
sizes(walk)
sizes(multi)
sizes(cle)

sizes_co2019 <- sizes(clp)

# set vertex community attribute according to membership
V(net)$community <- clp$membership
#V(net)$community <- com$membership
#V(net)$community <- edgebet$membership
#V(net)$community <- walk$membership
#V(net)$community <- multi$membership
#V(net)$community <- cle$membership

# subgraph
coa2019_1 <- induced.subgraph(net, V(net)[community %in% 1])
coa2019_2 <- induced.subgraph(net, V(net)[community %in% 2])
coa2019_3 <- induced.subgraph(net, V(net)[community %in% 3])
#coa2019_4 <- induced.subgraph(net, V(net)[community %in% 4])

# get attributes of each discourse coalitions
attr_coa2019_1 <- vertex_attr(coa2019_1)
attr_coa2019_2 <- vertex_attr(coa2019_2)
attr_coa2019_3 <- vertex_attr(coa2019_3)
#attr_coa2019_4 <- vertex_attr(coa2019_4)

# Generate colors based on actor type:
V(net)$color <- V(net)$type
V(net)$color=gsub("Creators","tomato",V(net)$color)
V(net)$color=gsub("Decision Makers","lightskyblue3",V(net)$color) 
V(net)$color=gsub("Media", "mediumpurple4",V(net)$color)
V(net)$color=gsub("Nonprofits & CSOs","seagreen4",V(net)$color) 
V(net)$color=gsub("Rightsholders & Publishers","orange2",V(net)$color)
V(net)$color=gsub("Scientific Organisations","darkolivegreen4",V(net)$color) 
V(net)$color=gsub("Tech Companies","lightgoldenrod2",V(net)$color)
V(net)$color=gsub("Various","snow3",V(net)$color)
colors.vect <- as.factor(V(net)$color)

attr_net2019 <- vertex.attributes(net)

net2019size <- as.data.frame(V(net)$size)
net2019size <- setDT(net2019size, keep.rownames = TRUE)[]
net2019sort <- sort(net2019size, decreasing = TRUE)

#--------------------------------------------------------------------

## OVER TIME ##

## 2017 ##
net2017 <- dna_toIgraph(congruence_A2017, weighted = TRUE)

## full congruence net2017work ##

net2017 <- simplify(net2017, remove.multiple = F, remove.loops = T)

#set attributes
net2017 <- set_vertex_attr(net2017, "type", index = V(net2017), as.character(atA$type))

# Compute node degrees (#links) and use that to set node size:
deg <- degree(net2017, mode="all")
V(net2017)$size <- deg/2
# Set edge width based on weight:
E(net2017)$width <- E(net2017)$weight*1.5
summary(E(net2017)$weight)

# edge threshold to remove low-intensity ties without discriminating against organisations with a low profile
#net2017 <- delete.edges(net2017, which(E(net2017)$weight <0.20962)) # 1st Q value of edges
net2017 <- delete.edges(net2017, which(E(net2017)$weight <0.22222)) # median value of edges

# remove EU Committee on Civil Liberties, Justice and Home Affairs (outsider)
net2017 <- delete.vertices(net2017, "EU Committee on Civil Liberties, Justice and Home Affairs")

# discourse coalitions

# community detection
clp2017 <- cluster_optimal(net2017) # justify why use this community detection algorithm
#com <- cluster_fast_greedy(net2017)
#edgebet <- cluster_edge_betweenness(net2017)
#walk <- cluster_walktrap(net2017)
#multi <- multilevel.community(net2017)

sizes(clp2017)
#sizes(com)
#sizes(edgebet)
#sizes(walk)
#sizes(multi)
sizes_co2017 <- sizes(clp2017)

# choose one of the three use
V(net2017)$community <- clp2017$membership
#V(net2017)$community <- com$membership
#V(net2017)$community <- edgebet$membership
#V(net2017)$community <- walk$membership
#V(net2017)$community <- multi$membership

# fix community order to make consistent with 2018a,2018b and 2019
V(net2017)$community1 <- as.character(V(net2017)$community)
V(net2017)$community1[V(net2017)$community1 == "2"] <- "three"
V(net2017)$community1[V(net2017)$community1 == "1"] <- "two"
V(net2017)$community1[V(net2017)$community1 == "3"] <- "one"
V(net2017)$community1[V(net2017)$community1 == "one"] <- "1"
V(net2017)$community1[V(net2017)$community1 == "two"] <- "2"
V(net2017)$community1[V(net2017)$community1 == "three"] <- "3"
V(net2017)$community <- as.numeric(V(net2017)$community1)


# plot
coa2017_1 <- induced.subgraph(net2017, V(net2017)[community %in% 1])
coa2017_2 <- induced.subgraph(net2017, V(net2017)[community %in% 2])
coa2017_3 <- induced.subgraph(net2017, V(net2017)[community %in% 3])
#coa2017_4 <- induced.subgraph(net2017, V(net2017)[community %in% 4])
attr_coa2017_1 <- vertex_attr(coa2017_1)
attr_coa2017_2 <- vertex_attr(coa2017_2)
attr_coa2017_3 <- vertex_attr(coa2017_3)
#attr_coa2017_4 <- vertex_attr(coa2017_4)

# Generate colors based on media type:
V(net2017)$color <- V(net2017)$type
V(net2017)$color=gsub("Creators","tomato",V(net2017)$color)
V(net2017)$color=gsub("Decision Makers","lightskyblue3",V(net2017)$color) 
V(net2017)$color=gsub("Media", "mediumpurple4",V(net2017)$color)
V(net2017)$color=gsub("Nonprofits & CSOs","seagreen4",V(net2017)$color) 
V(net2017)$color=gsub("Rightsholders & Publishers","orange2",V(net2017)$color)
V(net2017)$color=gsub("Scientific Organisations","darkolivegreen4",V(net2017)$color) 
V(net2017)$color=gsub("Tech Companies","lightgoldenrod2",V(net2017)$color)
V(net2017)$color=gsub("Various","snow3",V(net2017)$color)
colors.vect <- as.factor(V(net2017)$color)


## 2018a ##
net2018a <- dna_toIgraph(congruence_A2018a, weighted = TRUE)

net2018a <- simplify(net2018a, remove.multiple = F, remove.loops = T)

#set attributes
net2018a <- set_vertex_attr(net2018a, "type", index = V(net2018a), as.character(atA$type))

# Compute node degrees (#links) and use that to set node size:
deg <- degree(net2018a, mode="all")
V(net2018a)$size <- deg/2
# Set edge width based on weight:
E(net2018a)$width <- E(net2018a)$weight*1.5
summary(E(net2018a)$weight)

# edge threshold to remove low-intensity ties without discriminating against organisations with a low profile
net2018a <- delete.edges(net2018a, which(E(net2018a)$weight <0.20000)) # 1st Q value of edges
#net2018a <- delete.edges(net2018a, which(E(net2018a)$weight <0.30769)) # median value of edges

# remove EU Committee on Civil Liberties, Justice and Home Affairs (outsider)
net2018a <- delete.vertices(net2018a, "EU Committee on Civil Liberties, Justice and Home Affairs")

# discourse coalitions

# community detection
clp2018a <- cluster_optimal(net2018a) # justify why use this community detection algorithm
#com <- cluster_fast_greedy(net2018a)
#edgebet <- cluster_edge_betweenness(net2018a)
#walk <- cluster_walktrap(net2018a)
#multi <- multilevel.community(net2018a)

sizes(clp2018a)
sizes_co2018a <- sizes(clp2018a)

#sizes(com)
#sizes(edgebet)
#sizes(walk)
#sizes(multi)

# choose one of the three use
V(net2018a)$community <- clp2018a$membership
#V(net2018a)$community <- com$membership
#V(net2018a)$community <- edgebet$membership
#V(net2018a)$community <- walk$membership
#V(net2018a)$community <- multi$membership

# plot
coa2018a_1 <- induced.subgraph(net2018a, V(net2018a)[community %in% 1])
coa2018a_2 <- induced.subgraph(net2018a, V(net2018a)[community %in% 2])
coa2018a_3 <- induced.subgraph(net2018a, V(net2018a)[community %in% 3])
#coa2018a_4 <- induced.subgraph(net2018a, V(net2018a)[community %in% 4])
attr_coa2018a_1 <- vertex_attr(coa2018a_1)
attr_coa2018a_2 <- vertex_attr(coa2018a_2)
attr_coa2018a_3 <- vertex_attr(coa2018a_3)
#attr_coa2018a_4 <- vertex_attr(coa2018a_4)

## 2018b ##
net2018b <- dna_toIgraph(congruence_A2018b, weighted = TRUE)

net2018b <- simplify(net2018b, remove.multiple = F, remove.loops = T)

#set attributes
net2018b <- set_vertex_attr(net2018b, "type", index = V(net2018b), as.character(atA$type))

# Compute node degrees (#links) and use that to set node size:
deg <- degree(net2018b, mode="all")
V(net2018b)$size <- deg/3
# Set edge width based on weight:
E(net2018b)$width <- E(net2018b)$weight*1.5
summary(E(net2018b)$weight)

# edge threshold to remove low-intensity ties without discriminating against organisations with a low profile
#net2018b <- delete.edges(net2018b, which(E(net2018b)$weight <0.20000)) # 1st Q value of edges
net2018b <- delete.edges(net2018b, which(E(net2018b)$weight <0.28571)) # median value of edges

# remove EU Committee on Civil Liberties, Justice and Home Affairs (outsider)
net2018b <- delete.vertices(net2018b, "EU Committee on Civil Liberties, Justice and Home Affairs")

# discourse coalitions

# community detection
clp2018b <- cluster_optimal(net2018b) # justify why use this community detection algorithm
#com <- cluster_fast_greedy(net2018b)
#edgebet <- cluster_edge_betweenness(net2018b)
#walk <- cluster_walktrap(net2018b)
#multi <- multilevel.community(net2018b)

sizes(clp2018b)
#sizes(com)
#sizes(edgebet)
#sizes(walk)
#sizes(multi)
sizes_co2018b <- sizes(clp2018b)

# choose one of the three use
V(net2018b)$community <- clp2018b$membership
#V(net2018b)$community <- com$membership
#V(net2018b)$community <- edgebet$membership
#V(net2018b)$community <- walk$membership
#V(net2018b)$community <- multi$membership

# plot
coa2018b_1 <- induced.subgraph(net2018b, V(net2018b)[community %in% 1])
coa2018b_2 <- induced.subgraph(net2018b, V(net2018b)[community %in% 2])
coa2018b_3 <- induced.subgraph(net2018b, V(net2018b)[community %in% 3])
#coa2018b_4 <- induced.subgraph(net2018b, V(net2018b)[community %in% 4])
attr_coa2018b_1 <- vertex_attr(coa2018b_1)
attr_coa2018b_2 <- vertex_attr(coa2018b_2)
attr_coa2018b_3 <- vertex_attr(coa2018b_3)
#attr_coa2018b_4 <- vertex_attr(coa2018b_4)
