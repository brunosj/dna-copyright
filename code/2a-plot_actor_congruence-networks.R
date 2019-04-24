#---------------------------------------------------------------------
# Us and Them: Mapping Discourse Coalitions in the EU Copyright Debate
# Bruno St-Jacques (Hertie School of Governance)
# 2019
#---------------------------------------------------------------------

# actors congruence networks over time

png("p2017.png", 600, 600)
p2017 <- plot.igraph(net2017, 
                     vertex.label=NA, 
                     edge.color = "black",
                     edge.curved=.5, 
                     vertex.frame.color = NA,
                     layout=layout_with_fr)
dev.off()

png("p2018a.png", 600, 600)
p2018a <- plot.igraph(net2018a, 
                      vertex.label=NA, 
                      edge.color = "black",
                      edge.curved=.5, 
                      vertex.frame.color = NA,
                      layout=layout_with_fr)
dev.off()

png("p2018b.png", 600, 600)
p2018b <- plot.igraph(net2018b, 
                      vertex.label=NA, 
                      edge.color = "black",
                      edge.curved=.5, 
                      vertex.frame.color = NA,
                      layout=layout_with_fr)
dev.off()

# with colors as vectors to include alpha

# c("tomato","lightskyblue3", "mediumpurple4","seagreen4","orange2","darkolivegreen4","lightgoldenrod2","snow3")

legend(x=-1.5, y=-0.9, c("Creators","Decision Makers", "Media", "Nonprofits & CSOs", "Rightsholders & Publishers", "Scientific Organisations", "Tech Companies", "Various"),  
       pt.cex=2, cex=1.0, bty="n", ncol=2, pch = 16, col = adjustcolor(c("tomato","lightskyblue3", "mediumpurple4","seagreen4","orange2","darkolivegreen4","lightgoldenrod2","snow3"), alpha.f = .7))

net <- delete.edges(net, which(E(net)$weight <0.30769))

png("p2019.png", 1200, 1200)
p2019 <- plot.igraph(net, 
                     vertex.label=V(net)$name, 
                     vertex.label.color="black",
                     vertex.label.cex=2.0,
                     vertex.label.font=2,
                     vertex.label.dist=-1.4,
                     vertex.label.family="Avenir",
                     edge.color = "black",
                     vertex.color = adjustcolor(colors.vect, alpha.f = .7),
                     edge.curved=.5, 
                     vertex.frame.color = NA,
                     layout=layout_with_fr)
dev.off()


# discourse coaltions (joined) over time -> have to re-verify if coalitions colors are consistent

png("dc2017.png", 600, 600)
colrs <- adjustcolor(c("seagreen4", "dodgerblue3","purple3"), alpha=.7)
dc2017 <- plot.igraph(net2017, 
                      vertex.label=NA, 
                      vertex.color=colrs[V(net2017)$community],
                      edge.color = "black",
                      edge.curved=.5, 
                      vertex.frame.color = NA,
                      layout=layout_with_fr)
dev.off()

png("dc2018a.png", 600, 600)
colrs <- adjustcolor(c("seagreen4","dodgerblue3","purple3"), alpha=.7)
dc2018a <- plot.igraph(net2018a, 
                       vertex.label=NA, 
                       vertex.color=colrs[V(net2018a)$community],
                       edge.color = "black",
                       edge.curved=.5, 
                       vertex.frame.color = NA,
                       layout=layout_with_fr)
dev.off()

png("dc2018b.png", 600, 600)
colrs <- adjustcolor(c("seagreen4","dodgerblue3","purple3"), alpha=.7)
dc2018b <- plot.igraph(net2018b, 
                       vertex.label=NA, 
                       vertex.color=colrs[V(net2018b)$community],
                       edge.color = "black",
                       edge.curved=.5, 
                       vertex.frame.color = NA,
                       layout=layout_with_fr)
dev.off()

png("dc2019.png", 600, 600)
colrs <- adjustcolor(c("seagreen4","dodgerblue3", "purple3"), alpha=.7)
dc2019 <- plot.igraph(net, 
                      vertex.label=NA, 
                      vertex.color=colrs[V(net)$community],
                      edge.color = "black",
                      edge.curved=.5, 
                      vertex.frame.color = NA,
                      layout=layout_with_fr)
dev.off()

# full network with coalition plot  (optimal clustering)
colrs <- adjustcolor(c("dodgerblue3", "seagreen4", "tomato", "lightblue1"), alpha=.7)
png("fulldc2019.png", 600, 600)
layout <-layout_with_fr(net)
new_cols <- c("seagreen4", "dodgerblue3", "purple3")[membership(clp)]
colrs <- adjustcolor(c("seagreen4", "dodgerblue3","purple3"), alpha=.7)[membership(clp)]
plot(clp, net, col=colrs, mark.border="black", mark.col=c("darkseagreen1", "lightblue1", "pink"), 
     layout=layout, edge.curved=.5, vertex.label=NA, edge.arrow.size=.2, vertex.frame.color = "NA")
dev.off()

# full network with coalition plot (optimal clustering) -> with org_type colors
png("fulldc2019.png", 600, 600)
V(net)$community <- clp$membership
layout <-layout_with_fr(net)
new_cols <- c("seagreen4", "dodgerblue3", "purple3")[membership(clp)]
colrs <- adjustcolor(c("tomato","lightskyblue3","mediumpurple4","seagreen4",
                       "orange2","darkolivegreen4","lightgoldenrod2","snow3"), alpha=.7)["type"(V(net))]
colrs <- adjustcolor(colors.vect, alpha.f = .7) # to keep organisation type colors

plot(clp, net, 
     mark.border="black", mark.col=c("darkseagreen1", "lightblue1", "pink"), 
     layout=layout, edge.curved=.5, vertex.label=NA, edge.arrow.size=.2, 
     vertex.frame.color = "NA",
     vertex.color = adjustcolor(colors.vect, alpha.f = .7)
)

dev.off()

# full network with coalition plot (edge betweeness)
colrs <- adjustcolor(c("dodgerblue3", "seagreen4", "tomato", "lightblue1"), alpha=.7)
png("fulldc2019.png", 600, 600)
layout <-layout_with_fr(net)
new_cols <- c("seagreen4", "dodgerblue3", "tomato", "lightblue1")[membership(edgebet)]
colrs <- adjustcolor(c("seagreen4", "dodgerblue3"), alpha=.7)[membership(edgebet)]
plot(edgebet, net, col=colrs, mark.border="black", mark.col=c("darkseagreen1", "lightblue1", "tomato", "lightblue1"), 
     layout=layout, edge.curved=.5, vertex.label=NA, edge.arrow.size=.2, vertex.frame.color = "NA")
dev.off()

# full network with coalition plot (multilevel)
png("fulldc2019_multi.png", 1200, 1200)
V(net)$community <- multi$membership
colrs <- adjustcolor(c("dodgerblue3", "seagreen4", "tomato", "lightblue1"), alpha=.7)
layout <-layout_with_fr(net)
new_cols <- c("seagreen4", "dodgerblue3", "tomato", "purple3")[membership(multi)]
colrs <- adjustcolor(c("seagreen4", "dodgerblue3", "tomato", "purple3"), alpha=.7)[membership(multi)]
plot(multi, net, col=colrs, mark.border="black", mark.col=c("darkseagreen1", "lightblue1", "pink", "plum"), 
     layout=layout, edge.curved=.5, vertex.label=NA, edge.arrow.size=.2, vertex.frame.color = "NA")
dev.off()
