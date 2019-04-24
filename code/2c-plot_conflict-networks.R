#---------------------------------------------------------------------
# Us and Them: Mapping Discourse Coalitions in the EU Copyright Debate
# Bruno St-Jacques (Hertie School of Governance)
# 2019
#---------------------------------------------------------------------

# actor conflict networks

confnet <- delete.edges(confnet, which(E(confnet)$weight <2))
pconf2019 <- plot.igraph(confnet, 
                         vertex.label.color="black",
                         vertex.label.cex=0.8,
                         vertex.label.font=2,
                         vertex.label.dist=-1.4,
                         vertex.label.family="Avenir",
                         edge.color = "black",
                         vertex.color = adjustcolor(colors.vect, alpha.f = .7),
                         edge.curved=.5, 
                         vertex.frame.color = NA,
                         layout=layout_with_fr)

# frames conflict network
pconf2019.F <- plot.igraph(confnet.F, 
                           vertex.label.color="black",
                           vertex.label = V(confnet.F)$alias,
                           vertex.label.cex=0.8,
                           vertex.label.font=2,
                           vertex.label.dist=-1.4,
                           vertex.label.family="Avenir",
                           edge.color = "black",
                           vertex.color = adjustcolor(colors.vect, alpha.f = .7),
                           edge.curved=.5, 
                           vertex.frame.color = NA,
                           layout=layout_with_fr)

