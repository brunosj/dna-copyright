#---------------------------------------------------------------------
# Us and Them: Mapping Discourse Coalitions in the EU Copyright Debate
# Bruno St-Jacques (Hertie School of Governance)
# 2019
#---------------------------------------------------------------------

# subgraph of discourse coaltions

png("p_coa2017_1.png", 600, 600)
p_coa2017_1 <- plot.igraph(coa2017_1, 
                           vertex.label=NA, 
                           edge.color = "black",
                           edge.curved=.5, 
                           vertex.frame.color = NA,
                           layout=layout_with_fr)
dev.off()
png("p_coa2017_2.png", 600, 600)
p_coa2017_2 <- plot.igraph(coa2017_2, 
                           vertex.label=NA,
                           edge.color = "black",
                           edge.curved=.5, 
                           vertex.frame.color = NA,
                           layout=layout_with_fr)
dev.off()
png("p_coa2018a_1.png", 600, 600)
p_coa2018a_1 <- plot.igraph(coa2018a_1, 
                            vertex.label=NA, 
                            edge.color = "black",
                            edge.curved=.5, 
                            vertex.frame.color = NA,
                            layout=layout_with_fr)
dev.off()
png("p_coa2018a_2.png", 600, 600)
p_coa2018a_2 <- plot.igraph(coa2018a_2, 
                            vertex.label=NA,
                            edge.color = "black",
                            edge.curved=.5, 
                            vertex.frame.color = NA,
                            layout=layout_with_fr)
dev.off()
png("p_coa2018b_1.png", 600, 600)
p_coa2018b_1 <- plot.igraph(coa2018b_1, 
                            vertex.label=NA, 
                            edge.color = "black",
                            edge.curved=.5, 
                            vertex.frame.color = NA,
                            layout=layout_with_fr)
dev.off()
png("p_coa2018b_2.png", 600, 600)
p_coa2018b_2 <- plot.igraph(coa2018b_2, 
                            vertex.label=NA,
                            edge.color = "black",
                            edge.curved=.5, 
                            vertex.frame.color = NA,
                            layout=layout_with_fr)
dev.off()
png("p_coa2019_1.png", 600, 600)
p_coa2019_1 <- plot.igraph(coa2019_1, 
                           vertex.label=NA, 
                           edge.color = "black",
                           edge.curved=.5, 
                           vertex.frame.color = NA,
                           layout=layout_with_fr)
dev.off()

png("p_coa2019_2.png", 600, 600)
p_coa2019_2 <- plot.igraph(coa2019_2, 
                           vertex.label=NA,
                           edge.color = "black",
                           edge.curved=.5, 
                           vertex.frame.color = NA,
                           layout=layout_with_fr)
dev.off()
