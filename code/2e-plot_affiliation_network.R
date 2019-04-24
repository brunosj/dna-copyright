#---------------------------------------------------------------------
# Us and Them: Mapping Discourse Coalitions in the EU Copyright Debate
# Bruno St-Jacques (Hertie School of Governance)
# 2019
#---------------------------------------------------------------------

png("net2.png", 1200, 1200)
plot.igraph(net2, 
            edge.color = "black",
            edge.curved=.5, 
            vertex.frame.color = "grey",
            vertex.color = adjustcolor(colors.vect, alpha.f = .7),
            vertex.label=V(net2)$type2, # just frames 
            vertex.label.color="black",
            vertex.label.cex=0.9,
            vertex.label.font=2,
            vertex.label.family="Avenir",
            layout=layout_nicely)
dev.off()


# concept x organisation
png("paffilnet.png", 1200, 1200)
par(mar=c(2,8,2,6))
paffilnet <- plot.igraph(net3, 
                         edge.color = "ivory4",
                         edge.curved=.5, 
                         vertex.frame.color = "grey",
                         vertex.color = adjustcolor(colors.vect3, alpha.f = .7),
                         vertex.label=V(net3)$type1, # just frames 
                         vertex.label.color="black",
                         vertex.label.cex=1.7,
                         vertex.label.font=2,
                         vertex.label.family="Avenir",
                         layout=layout_nicely)
dev.off()

# with org names
paffilnet <- plot.igraph(net3, 
                         edge.color = "ivory4",
                         edge.curved=.5, 
                         vertex.frame.color = "grey",
                         vertex.color = adjustcolor(colors.vect3, alpha.f = .7),
                         vertex.label=V(net3)$org, # just frames 
                         vertex.label.color="black",
                         vertex.label.cex=1.0,
                         vertex.label.font=2,
                         vertex.label.family="Avenir",
                         layout=layout_nicely)