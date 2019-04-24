#---------------------------------------------------------------------
# Us and Them: Mapping Discourse Coalitions in the EU Copyright Debate
# Bruno St-Jacques (Hertie School of Governance)
# 2019
#---------------------------------------------------------------------


# frames congruence network

# fnet <- delete.edges(fnet, which(E(fnet)$weight <2))
#c("red2", "plum3", "rosybrown2", "lightgoldenrod1", "orange1", "chocolate3")
# legend(x=-1.5, y=-1.1, c("Civil Rights","Research and Development", "Economic Development", "Cultural Implications", "Legal Principles", "Decision-Making Process"), pch=22,col=NA, pt.cex=2, cex=.7, bty="n", ncol=2, fill = c("red2", "plum3", "rosybrown2", "lightgoldenrod1", "orange1", "chocolate3"))

png("f2019.png", 1200, 1200)
par(mar=c(2,6,2,6))
f2019 <- plot.igraph(fnet, 
                     vertex.label=V(fnet)$alias, 
                     vertex.label.color="black",
                     vertex.label.cex=1.7,
                     vertex.label.font=2,
                     vertex.label.family="Avenir",
                     edge.color = "darkgrey",
                     vertex.color = adjustcolor(fcolors.vect, alpha.f = .7),
                     edge.curved=.5, 
                     vertex.frame.color = NA,
                     layout=layout_nicely)
dev.off()

dev.print(png, 'f20192.png')

# full network with coalition plot (optimal clustering)
colrs_F <- adjustcolor(c("seagreen4", "slateblue4", "tan3"), alpha=.7)

png("fulldc2019.png", 1200, 1200)
par(mar=c(2,8,2,6))
new_cols_F <- c("seagreen4", "slateblue4", "darkorange3", "royalblue4")[membership(fclp)]
colrs_F <- adjustcolor(c("seagreen4", "slateblue4", "orangered1", "royalblue4"), alpha=.7)[membership(fclp)]
plot(fclp, fnet, col=colrs_F, 
     mark.border="black", 
     mark.col=c("palegreen", "plum", "sandybrown", "skyblue"), 
     edge.curved=.5, 
     vertex.frame.color = "NA",
     vertex.label=V(fnet)$alias, 
     vertex.label.color="black",
     vertex.label.cex=1.7,
     vertex.label.font=2,
     vertex.label.dist=-1.4,
     vertex.label.family="Avenir",
     layout=layout_nicely)
dev.off()

# full network with coalition plot (multilevel)
colrs_F <- adjustcolor(c("seagreen4", "slateblue4", "tan3"), alpha=.7)

png("fulldc2019.png", 1200, 1200)
layout <-layout_with_fr(fnet)

new_cols_F <- c("seagreen4", "purple3", "darkorange3", "royalblue4")[membership(fml)]
colrs_F <- adjustcolor(c("seagreen4", "purple3", "orangered1","royalblue4" ), alpha=.7)[membership(fml)]
plot(fml, fnet, col=colrs_F, 
     mark.border="black", 
     mark.col=c("palegreen", "plum", "sandybrown", "skyblue"), 
     edge.curved=.5, 
     vertex.frame.color = "NA",
     vertex.label=V(fnet)$alias, 
     vertex.label.color="black",
     vertex.label.cex=0.9,
     vertex.label.font=2,
     vertex.label.dist=-1.4,
     vertex.label.family="Avenir",
     layout=layout_with_dh)

dev.off()