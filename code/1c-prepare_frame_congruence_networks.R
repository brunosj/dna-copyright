#---------------------------------------------------------------------
# Us and Them: Mapping Discourse Coalitions in the EU Copyright Debate
# Bruno St-Jacques (Hertie School of Governance)
# 2019
#---------------------------------------------------------------------

### DNA ###

# compute congruence network of frames
congruence_F <- dna_network(copyright,
                            networkType = "onemode",
                            statementType = "DNA Statement",
                            normalization = "average",
                            variable1 = "concept",
                            variable2 = "organization",
                            qualifier = "agreement",
                            qualifierAggregation = "congruence",
                            duplicates = "document")
# convert to network object
congF.net <- network(congruence_F)
# retrieve frames attributes
atF <- dna_getAttributes(copyright,
                         statementType = "DNA Statement",
                         variable = "concept")


# 2017 
congruence_F2017 <- dna_network(copyright,
                                networkType = "onemode",
                                statementType = "DNA Statement",
                                normalization = "average",
                                variable1 = "concept",
                                variable2 = "organization",
                                qualifier = "agreement",
                                qualifierAggregation = "congruence",
                                duplicates = "document",
                                start.date = "01.01.2017",
                                stop.date = "26.05.2018") # Council agrees on position
congF2017.net <- network(congruence_F2017)

# 2018a
congruence_F2018a <- dna_network(copyright,
                                 networkType = "onemode",
                                 statementType = "DNA Statement",
                                 normalization = "average",
                                 variable1 = "concept",
                                 variable2 = "organization",
                                 qualifier = "agreement",
                                 qualifierAggregation = "congruence",
                                 duplicates = "document",
                                 start.date = "01.01.2017",
                                 stop.date = "13.09.2018") # Parliament agrees on position
congF2018a.net <- network(congruence_F2018a)

# 2018b
congruence_F2018b <- dna_network(copyright,
                                 networkType = "onemode",
                                 statementType = "DNA Statement",
                                 normalization = "average",
                                 variable1 = "concept",
                                 variable2 = "organization",
                                 qualifier = "agreement",
                                 qualifierAggregation = "congruence",
                                 duplicates = "document",
                                 start.date = "01.01.2017",
                                 stop.date = "04.02.2019") # final Trilogue announced
congF2018b.net <- network(congruence_F2018b)


#2019
congruence_F2019 <- dna_network(copyright,
                                networkType = "onemode",
                                statementType = "DNA Statement",
                                normalization = "average",
                                variable1 = "concept",
                                variable2 = "organization",
                                qualifier = "agreement",
                                qualifierAggregation = "congruence",
                                duplicates = "document",
                                start.date = "01.01.2017",
                                stop.date = "31.12.2019") # directive adopted
congF2019.net <- network(congruence_F2019)

#--------------------------------------------------------------------

### IGRAPH ###

# frames networks
fnet <- dna_toIgraph(congruence_F, weighted = TRUE)
fnet2017 <- dna_toIgraph(congruence_F2017, weighted = TRUE)
fnet2018a <- dna_toIgraph(congruence_F2018a, weighted = TRUE)
fnet2018b <- dna_toIgraph(congruence_F2018b, weighted = TRUE)

## full congruence fnetwork ##
fnet <- simplify(fnet, remove.multiple = F, remove.loops = T)

#set attributes
fnet <- set_vertex_attr(fnet, "type", index = V(fnet), as.character(atF$type))
fnet <- set_vertex_attr(fnet, "alias", index = V(fnet), as.character(atF$alias))

# Compute node degrees (#links) and use that to set node size:
deg <- degree(fnet, mode="all")
V(fnet)$size <- deg/3
# Set edge width based on weight:
E(fnet)$width <- E(fnet)$weight*3
V(fnet)$shape <- "circle"

# filter network
summary(E(fnet)$weight)
summary(V(fnet)$size)
fnet <- delete.edges(fnet, which(E(fnet)$weight <0.21429)) # median value
fnet <- delete.vertices(fnet, which(V(fnet)$size <6.0)) # median value
#fnet <- delete.edges(fnet, which(E(fnet)$weight < 0.28060)) # mean value
#fnet <- delete.vertices(fnet, which(V(fnet)$size < 6.089)) # mean value
#fnet <- delete.edges(fnet, which(E(fnet)$weight < 0.367)) # 3rd Q value
#fnet <- delete.vertices(fnet, which(V(fnet)$size < 8.3)) # 3d Q value

# Generate colors based on media type:
V(fnet)$color <- V(fnet)$type
V(fnet)$color=gsub("Civil Rights","red2",V(fnet)$color)
V(fnet)$color=gsub("Research and Development","plum3",V(fnet)$color) 
V(fnet)$color=gsub("Economic Development", "rosybrown2",V(fnet)$color)
V(fnet)$color=gsub("Cultural Implications","lightgoldenrod1",V(fnet)$color) 
V(fnet)$color=gsub("Legal Principles","orange1",V(fnet)$color)
V(fnet)$color=gsub("Decision-Making Process","chocolate3",V(fnet)$color)
fcolors.vect <- as.factor(V(fnet)$color)

attr_fnet <- vertex_attr(fnet)

# discourse coalitions

# optimal clustering
fclp <- cluster_optimal(fnet) # justify why use this community detection algorithm
sizes_co2019_F_CLP <- sizes(fclp)
V(fnet)$community <- fclp$membership

# multilevel clustering
fml <- multilevel.community(fnet) # justify why use this community detection algorithm
sizes_co2019_F_ML <- sizes(fml)

# subgraph and coalition attributes
fcoa2019_1 <- induced.subgraph(fnet, V(fnet)[community %in% 1])
fcoa2019_2 <- induced.subgraph(fnet, V(fnet)[community %in% 2])
fcoa2019_3 <- induced.subgraph(fnet, V(fnet)[community %in% 3])
fcoa2019_4 <- induced.subgraph(fnet, V(fnet)[community %in% 4])
attr_fcoa2019_1 <- vertex_attr(fcoa2019_1)
attr_fcoa2019_2 <- vertex_attr(fcoa2019_2)
attr_fcoa2019_3 <- vertex_attr(fcoa2019_3)
attr_fcoa2019_4 <- vertex_attr(fcoa2019_4)