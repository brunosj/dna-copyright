#--------------------------------------------------------------------
# Us and Them: Mapping Discourse Coalitions in the EU Copyright Debate
# Bruno St-Jacques (Hertie School of Governance)
# 2019
#--------------------------------------------------------------------

# compute actors conflict network (agreement/disagreement b/w organizations)
conflict_A <- dna_network(copyright,
                          networkType = "onemode",
                          statementType = "DNA Statement",
                          variable1 = "organization",
                          variable2 = "concept",
                          qualifier = "agreement",
                          qualifierAggregation = "conflict",
                          duplicates = "document")
# convert to network object
confA.net <- network(conflict_A)
# retrieve actors attributes
atconfA <- dna_getAttributes(copyright,
                             statementType = "DNA Statement",
                             variable = "organization")

# compute conflict network of frames
conflict_F <- dna_network(copyright,
                          networkType = "onemode",
                          statementType = "DNA Statement",
                          variable1 = "concept",
                          variable2 = "organization",
                          qualifier = "agreement",
                          qualifierAggregation = "conflict",
                          duplicates = "document")
# convert to network object
confF.net <- network(conflict_F)
# retrieve frames attributes
atconfF <- dna_getAttributes(copyright,
                             statementType = "DNA Statement",
                             variable = "concept")


# actors conflict network

confnet <- dna_toIgraph(conflict_A, weighted = TRUE)
confnet <- simplify(confnet, remove.multiple = F, remove.loops = F)

confnet <- set_vertex_attr(confnet, "type", index = V(confnet), as.character(atconfA$type))

# Generate colors based on organisation type:
V(confnet)$color <- V(confnet)$type
V(confnet)$color=gsub("Creators","tomato",V(confnet)$color)
V(confnet)$color=gsub("Decision Makers","lightskyblue3",V(confnet)$color) 
V(confnet)$color=gsub("Media", "mediumpurple4",V(confnet)$color)
V(confnet)$color=gsub("Nonprofits & CSOs","seagreen4",V(confnet)$color) 
V(confnet)$color=gsub("Rightsholders & Publishers","orange2",V(confnet)$color)
V(confnet)$color=gsub("Scientific Organisations","darkolivegreen4",V(confnet)$color) 
V(confnet)$color=gsub("Tech Companies","lightgoldenrod2",V(confnet)$color)
V(confnet)$color=gsub("Various","snow3",V(confnet)$color)
colors.vect <- as.factor(V(confnet)$color)

deg <- degree(confnet, mode="all")
V(confnet)$size <- deg/5
E(confnet)$width <- E(confnet)$weight/6
summary(E(confnet)$weight)
summary(V(confnet)$size)

confnet <- delete.edges(confnet, which(E(confnet)$weight <1)) # median value of edges
confnet <- delete.vertices(confnet, which(V(confnet)$size <2.6))


# frames conflict network
confnet.F <- dna_toIgraph(conflict_F, weighted = TRUE)
confnet.F <- simplify(confnet.F, remove.multiple = F, remove.loops = F)
confnet.F <- set_vertex_attr(confnet.F, "type", index = V(confnet.F), as.character(atconfF$type))
confnet.F <- set_vertex_attr(confnet.F, "alias", index = V(confnet.F), as.character(atconfF$alias))

# Generate colors based on frame type:
V(confnet.F)$color <- V(confnet.F)$type
V(confnet.F)$color=gsub("Civil Rights","cadetblue3",V(confnet.F)$color)
V(confnet.F)$color=gsub("Research and Development","darkgoldenrod2",V(confnet.F)$color) 
V(confnet.F)$color=gsub("Economic Development", "darkolivegreen2",V(confnet.F)$color)
V(confnet.F)$color=gsub("Cultural Implications","coral3",V(confnet.F)$color) 
V(confnet.F)$color=gsub("Legal Principles","bisque2",V(confnet.F)$color)
colorsF.vect <- as.factor(V(confnet.F)$color)

deg <- degree(confnet.F, mode="all")
V(confnet.F)$size <- deg/5
E(confnet.F)$width <- E(confnet.F)$weight/6

summary(E(confnet.F)$weight)
summary(V(confnet.F)$size)

confnet.F <- delete.edges(confnet.F, which(E(confnet)$weight <1)) # median value of edges
confnet.F <- delete.vertices(confnet.F, which(V(confnet)$size <1.591))