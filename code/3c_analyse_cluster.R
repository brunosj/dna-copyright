
#---------------------------------------------------------------------
# Us and Them: Mapping Discourse Coalitions in the EU Copyright Debate
# Bruno St-Jacques (Hertie School of Governance)
# 2019
#---------------------------------------------------------------------


# ==============================================================================
# Cluster analysis
# ==============================================================================


# cluster analysis

# plot agreement vs disagreement
dna_barplot(conn, 
            of = "concept", 
            fontSize = 6,
            barWidth =0.5,
            lab.neg = "Pro-Copyright Directive",
            lab.pos = "anti-Copyright Directive", 
            colours = FALSE
)

dna_barplot(conn, of = "organization", fontSize = 6)

# cluster analysis 

clustcolorsA <- c("tomato","lightskyblue3","mediumpurple4","seagreen4",
                  "orange2","darkolivegreen4","lightgoldenrod2","snow3")
clustcolorsF <- c("red2","plum3","rosybrown2","lightgoldenrod1","orange1","chocolate3")

clustF <- dna_cluster(conn,
                      variable1 = "concept",
                      variable2 = "organization",
                      clust.method = "ward.D2",
                      duplicates = "acrossrange",
                      attribute1 = "type",
                      attribute2 = "alias",
                      cutree.k = 3
)

pclustF <- dna_plotDendro(clustF, 
                          shape = "elbows", 
                          colours = "manual", 
                          rectangles = "red", 
                          font_size = 8, 
                          custom_colours = clustcolorsF) +
  ggtitle("Cluster of Frames") +
  theme(
    plot.title=element_text(face='bold', size=15)
  )
pclustF

clustA <- dna_cluster(conn,
                      variable1 = "organization",
                      variable2 = "concept",
                      clust.method = "ward.D2",
                      duplicates = "acrossrange",
                      attribute1 = "type",
                      attribute2 = "alias",
                      cutree.k = 3
)

pclustA <- dna_plotDendro(clustA, 
                          shape = "elbows", 
                          colours = "manual", 
                          rectangles = "black", 
                          leaf_linetype = "a",
                          branch_linetype = "a",
                          font_size = 9, 
                          line_width = 0.8,
                          custom_colours = clustcolorsA) +
  ggtitle("Cluster of Actors") +
  theme(
    plot.title=element_text(face='bold', size=15)
  )
pclustA
