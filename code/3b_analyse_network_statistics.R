
#---------------------------------------------------------------------
# Us and Them: Mapping Discourse Coalitions in the EU Copyright Debate
# Bruno St-Jacques (Hertie School of Governance)
# 2019
#---------------------------------------------------------------------

# ==============================================================================
# Network statistics
# ==============================================================================


# centrality

deg.in <- as.data.frame(degree(net, mode = 'in'))
deg.in <- tibble::rownames_to_column(deg.in, var = "rowname")
names(deg.in)[names(deg.in) == 'degree(net, mode = "in")'] <- 'deg'
names(deg.in)[names(deg.in) == 'rowname'] <- 'name'

degcoa1 <- as.data.frame(select(attr_coa2019_1, name))  
degcoa2 <- as.data.frame(select(attr_coa2019_2, name))  
degcoa3 <- as.data.frame(select(attr_coa2019_3, name))  
deg.incoa1 <- left_join(degcoa1, deg.in)
deg.incoa2 <- left_join(degcoa2, deg.in)
deg.incoa3 <- left_join(degcoa3, deg.in)
deg.in2019 <- cbind(mean(deg.incoa1$deg), mean(deg.incoa2$deg), mean(deg.incoa3$deg))

deg.in.w <- as.data.frame(graph.strength(net, mode = 'all', weights = E(net)$weight)) # weighted
deg.in.w <- tibble::rownames_to_column(deg.in.w, var = "rowname")
names(deg.in.w)[names(deg.in.w) == 'graph.strength(net, mode = "all", weights = E(net)$weight)'] <- 'deg'
names(deg.in.w)[names(deg.in.w) == 'rowname'] <- 'name'
deg.in.wcoa1 <- left_join(degcoa1, deg.in.w)
deg.in.wcoa2 <- left_join(degcoa2, deg.in.w)
deg.in.wcoa3 <- left_join(degcoa3, deg.in.w)
deg.in.w2019 <- cbind(mean(deg.in.wcoa1$deg), mean(deg.in.wcoa2$deg), mean(deg.in.wcoa3$deg))


# eigenvector centrality 2019
eigen <- as.data.frame(eigen_centrality(net, directed=FALSE, weights=E(net)$weight)$vector)
eigen <- tibble::rownames_to_column(eigen, var = "rowname")
names(eigen)[names(eigen) == 'eigen_centrality(net, directed = FALSE, weights = E(net)$weight)$vector'] <- 'eigen'
names(eigen)[names(eigen) == 'rowname'] <- 'name'

coa1 <- as.data.frame(select(attr_coa2019_1, name))  
coa2 <- as.data.frame(select(attr_coa2019_2, name))  
coa3 <- as.data.frame(select(attr_coa2019_3, name))  
eigen1 <- left_join(coa1, eigen)
eigen2 <- left_join(coa2, eigen)
eigen3 <- left_join(coa3, eigen)

eigencoa1 <- eigen_centrality(coa2019_1, directed=FALSE, weights=E(coa2019_1)$weight)$vector
eigencoa2 <- eigen_centrality(coa2019_2, directed=FALSE, weights=E(coa2019_2)$weight)$vector
eigencoa3 <- eigen_centrality(coa2019_3, directed=FALSE, weights=E(coa2019_3)$weight)$vector
eigen2019 <- cbind(mean(eigencoa1), mean(eigencoa2), mean(eigencoa3))


# actor congruence networks

# sizes of coalitions
sizecoa <- cbind(sizes_co2017, sizes_co2018a, sizes_co2018b, sizes_co2019)
sizecoa # first community membership has to be swapped 
sizecoa[1, 1] = 5 # by hand for now
sizecoa[2, 1] = 8
rownames(sizecoa) <- c("Coalition A", "Coalition B", "Coalition C")
colnames(sizecoa) <- c("Period 1", "Period 2", "Period 3", "Period 4")

# members of coalitions
member2017 <- membership(clp2017) # not right numbers for this one
member2018a <- membership(clp2018a)
member2018b <- membership(clp2018b)
member2019 <- membership(clp)

# edge density of coalitions
dens2017_1 <- edge_density(coa2017_1, loops = TRUE) 
dens2017_2 <- edge_density(coa2017_2, loops = TRUE)
dens2017_3 <- edge_density(coa2017_3, loops = TRUE)
dens2018a_1 <- edge_density(coa2018a_1, loops = TRUE)
dens2018a_2 <- edge_density(coa2018a_2, loops = TRUE)
dens2018a_3 <- edge_density(coa2018a_3, loops = TRUE)
dens2018b_1 <- edge_density(coa2018b_1, loops = TRUE)
dens2018b_2 <- edge_density(coa2018b_2, loops = TRUE)
dens2018b_3 <- edge_density(coa2018b_3, loops = TRUE)
dens2019_1 <- edge_density(coa2019_1, loops = TRUE)
dens2019_2 <- edge_density(coa2019_2, loops = TRUE)
dens2019_3 <- edge_density(coa2019_3, loops = TRUE)

# frames
fdens2019_1 <- edge_density(fcoa2019_1, loops = TRUE)
fdens2019_2 <- edge_density(fcoa2019_2, loops = TRUE)
fdens2019_3 <- edge_density(fcoa2019_3, loops = TRUE)


denscoa <- cbind((rbind(dens2017_1, dens2018a_1, dens2018b_1, dens2019_1)),(rbind(dens2017_2, dens2018a_2, dens2018b_2, dens2019_2)),(rbind(dens2017_3, dens2018a_3, dens2018b_3, dens2019_3))) 
colnames(denscoa) <- c("Coalition A", "Coalition B", "Coalition C")
rownames(denscoa) <- c("Period 1", "Period 2", "Period 3", "Period 4")

# global clustering coefficient
trans2017_1 <- transitivity(coa2017_1, type = c("average"), weights = TRUE) 
trans2017_2 <- transitivity(coa2017_2, type = c("average"),weights = TRUE)
trans2017_3 <- transitivity(coa2017_3, type = c("average"),weights = TRUE)
trans2018a_1 <- transitivity(coa2018a_1, type = c("average"), weights = TRUE)
trans2018a_2 <- transitivity(coa2018a_2, type = c("average"), weights = TRUE)
trans2018a_3 <- transitivity(coa2018a_3, type = c("average"), weights = TRUE)
trans2018b_1 <- transitivity(coa2018b_1, type = c("average"), weights = TRUE)
trans2018b_2 <- transitivity(coa2018b_2, type = c("average"), weights = TRUE)
trans2018b_3 <- transitivity(coa2018b_3, type = c("average"), weights = TRUE)
trans2019_1 <- transitivity(coa2019_1, type = c("average"), weights = TRUE)
trans2019_2 <- transitivity(coa2019_2, type = c("average"), weights = TRUE)
trans2019_3 <- transitivity(coa2019_3, type = c("average"), weights = TRUE)

# frames
ftrans2019_1 <- transitivity(fcoa2019_1, type = c("average"), weights = TRUE)
ftrans2019_2 <- transitivity(fcoa2019_2, type = c("average"), weights = TRUE)
ftrans2019_3 <- transitivity(fcoa2019_3, type = c("average"), weights = TRUE)

transcoa <- cbind((rbind(trans2017_1, trans2018a_1, trans2018b_1, trans2019_1)),(rbind(trans2017_2, trans2018a_2, trans2018b_2, trans2019_2)),(rbind(trans2017_3, trans2018a_3, trans2018b_3, trans2019_3)))
colnames(transcoa) <- c("Coalition A", "Coalition B", "Coalition C")
rownames(transcoa) <- c("Period 1", "Period 2", "Period 3", "Period 4")

# plots

coacolors <- c("seagreen4", "seagreen4", "seagreen4", "seagreen4", "dodgerblue3", "dodgerblue3",  "dodgerblue3","dodgerblue3", "purple3", "purple3", "purple3", "purple3")

# number of actors
sizecoa_values <- c((sizecoa[1,1]), (sizecoa[1,2]), (sizecoa[1,3]), (sizecoa[1,4]), (sizecoa[2,1]), (sizecoa[2,2]), (sizecoa[2,3]), (sizecoa[2,4]), (sizecoa[3,1]), (sizecoa[3,2]), (sizecoa[3,3]), (sizecoa[3,4]))


sizecoa_df <- data.frame(period=rep(c("Period 1", "Period 2", "Period 3", "Period 4"), 3),
                         value=sizecoa_values,
                         coa=rep(c("Coalition A", "Coalition B", "Coalition C"), each=4))

p_sizecoa <- ggplot(data=sizecoa_df, aes(x=period, y=value, group=coa, color = coa)) +
  geom_line()+
  geom_point() +
  labs(title="Size of Coalitions",
       subtitle = "Number of actors per time period") +
  xlab("Time Period") + 
  ylab("Number of Actors") +
  scale_colour_manual(name = " ", 
                      values = c("seagreen4","dodgerblue3", "purple3")) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(face="bold"),
        axis.title = element_text(size=8))

# density
denscoa <- t(denscoa)
denscoa_values <- c((denscoa[1,1]), (denscoa[1,2]), (denscoa[1,3]), (denscoa[1,4]), (denscoa[2,1]), (denscoa[2,2]), (denscoa[2,3]), (denscoa[2,4]), (denscoa[3,1]), (denscoa[3,2]), (denscoa[3,3]), (denscoa[3,4]))


denscoa_df <- data.frame(period=rep(c("Period 1", "Period 2", "Period 3", "Period 4"), 3),
                         value=denscoa_values,
                         coa=rep(c("Coalition A", "Coalition B", "Coalition C"), each=4))

p_denscoa <- ggplot(data=denscoa_df, aes(x=period, y=value, group=coa, color = coa)) +
  geom_line()+
  geom_point() +
  labs(title="Density of Coalitions",
       subtitle = "Density per time period") +
  xlab("Time Period") + 
  ylab("Weighted Density") +
  scale_colour_manual(name = " ", 
                      values = c("seagreen4","dodgerblue3", "purple3")) +
  theme_classic() +
  theme(legend.position="none",
        plot.title = element_text(face="bold"),
        axis.title = element_text(size=8))

# global clustering coefficient
transcoa <- t(transcoa)
transcoa_values <- c((transcoa[1,1]), (transcoa[1,2]), (transcoa[1,3]), (transcoa[1,4]), (transcoa[2,1]), (transcoa[2,2]), (transcoa[2,3]), (transcoa[2,4]), (transcoa[3,1]), (transcoa[3,2]), (transcoa[3,3]), (transcoa[3,4]))


transcoa_df <- data.frame(period=rep(c("Period 1", "Period 2", "Period 3", "Period 4"), 3),
                          value=transcoa_values,
                          coa=rep(c("Coalition A", "Coalition B", "Coalition C"), each=4))

p_transcoa <- ggplot(data=transcoa_df, aes(x=period, y=value, group=coa, color = coa)) +
  geom_line()+
  geom_point() +
  labs(title="Transitivity of Coalitions",
       subtitle = "Clustering of actors per time period") +
  xlab("Time Period") + 
  ylab("Global Clustering Coefficient") +
  scale_colour_manual(name = " ", 
                      values = c("seagreen4","dodgerblue3", "purple3")) +
  
  theme_classic() +
  theme(legend.position="none",
        plot.title = element_text(face="bold"),
        axis.title = element_text(size=8))

# save plots

ggplot2::ggsave(filename = "p_sizecoa_norm.png", plot = p_sizecoa, scale = 1, width = 4, height = 2.5, dpi = "retina")
ggplot2::ggsave(filename = "p_denscoa_norm.png", plot = p_denscoa, scale = 1, width = 4, height = 2.5, dpi = "retina")
ggplot2::ggsave(filename = "p_transcoa_norm.png", plot = p_transcoa, scale = 1, width = 4, height = 2.5, dpi = "retina")


# actor congruence over time (sliced)

sizecoa <- cbind(sizes_co2017S, sizes_co2018aS, sizes_co2018bS, sizes_co2019S)
#sizecoa # first community membership has to be swapped 
#sizecoa[1, 1] = 5 # by hand for now
#sizecoa[2, 1] = 8
rownames(sizecoa) <- c("Coalition A", "Coalition B", "Coalition C")
colnames(sizecoa) <- c("Period 1", "Period 2", "Period 3", "Period 4")

# members of coalitions
member2017S <- membership(clp2017S) # not right numbers for this one
member2018aS <- membership(clp2018aS)
member2018bS <- membership(clp2018bS)
member2019S <- membership(clp)

# edge density of coalitions
dens2017S_1 <- edge_density(coa2017S_1, loops = TRUE) 
dens2017S_2 <- edge_density(coa2017S_2, loops = TRUE)
dens2018aS_1 <- edge_density(coa2018aS_1, loops = TRUE)
dens2018aS_2 <- edge_density(coa2018aS_2, loops = TRUE)
dens2018bS_1 <- edge_density(coa2018bS_1, loops = TRUE)
dens2018bS_2 <- edge_density(coa2018bS_2, loops = TRUE)
dens2018bS_3 <- edge_density(coa2018bS_3, loops = TRUE)
dens2018bS_4 <- edge_density(coa2018bS_4, loops = TRUE)
dens2019S_1 <- edge_density(coa2019S_1, loops = TRUE)
dens2019S_2 <- edge_density(coa2019S_2, loops = TRUE)
denscoa <- cbind((rbind(dens2017S_1, dens2018aS_1, dens2018bS_1, dens2019S_1)),(rbind(dens2017S_2, dens2018aS_2, dens2018bS_2, dens2019S_2)),(rbind(NA, NA, dens2018bS_3, NA)),(rbind(NA, NA,dens2018bS_4,NA))) 
colnames(denscoa) <- c("Coalition A", "Coalition B", "Coalition C")
rownames(denscoa) <- c("Period 1", "Period 2", "Period 3", "Period 4")

# global clustering coefficient
trans2017S_1 <- transitivity(coa2017S_1, type = c("average"), weights = TRUE) 
trans2017S_2 <- transitivity(coa2017S_2, type = c("average"),weights = TRUE)
trans2018aS_1 <- transitivity(coa2018aS_1, type = c("average"), weights = TRUE)
trans2018aS_2 <- transitivity(coa2018aS_2, type = c("average"), weights = TRUE)
trans2018bS_1 <- transitivity(coa2018bS_1, type = c("average"), weights = TRUE)
trans2018bS_2 <- transitivity(coa2018bS_2, type = c("average"), weights = TRUE)
trans2018bS_3 <- transitivity(coa2018bS_3, type = c("average"), weights = TRUE)
trans2018bS_4 <- transitivity(coa2018bS_4, type = c("average"), weights = TRUE)
trans2019S_1 <- transitivity(coa2019S_1, type = c("average"), weights = TRUE)
trans2019S_2 <- transitivity(coa2019S_2, type = c("average"), weights = TRUE)
trans2019S_3 <- transitivity(coa2019S_3, type = c("average"), weights = TRUE)

transcoa <- cbind((rbind(trans2017S_1, trans2018aS_1, trans2018bS_1, trans2019S_1)),(rbind(trans2017S_2, trans2018aS_2, trans2018bS_2, trans2019S_2)),(rbind(NA, NA, trans2018bS_3, NA)), (rbind(NA, NA, trans2018bS_4, NA)))
colnames(transcoa) <- c("Coalition A", "Coalition B", "Coalition C")
rownames(transcoa) <- c("Period 1", "Period 2", "Period 3", "Period 4")


# concept congruence networks

sizesfcoa2019 <- sizes(fclp)

densfcoa2019 <- cbind((edge_density(fcoa2019_1, loops = TRUE)), (edge_density(fcoa2019_2, loops = TRUE)), (edge_density(fcoa2019_3, loops = TRUE)),  (edge_density(fcoa2019_4, loops = TRUE)))

transfcoa2019 <- cbind((transitivity(fcoa2019_1, type = c("average"), weights = TRUE)), (transitivity(fcoa2019_2, type = c("average"), weights = TRUE)), (transitivity(fcoa2019_3, type = c("average"), weights = TRUE)),  (transitivity(fcoa2019_4, type = c("average"), weights = TRUE)))

fcoastats <- rbind(sizesfcoa2019,densfcoa2019, transfcoa2019)
rownames(fcoastats) <- c("Size", "Density", "Transitivity")
colnames(fcoastats) <- c("Coalition A", "Coalition B", "Coalition C", "Coalition D")
fcoastats <- t(fcoastats)
fcoastats

# 2017
sizesfcoa2017 <- sizes(fclp2017)

densfcoa2017 <- cbind((edge_density(fcoa2017_1, loops = TRUE)), (edge_density(fcoa2017_2, loops = TRUE)), (edge_density(fcoa2017_3, loops = TRUE)),  (edge_density(fcoa2017_4, loops = TRUE)))

transfcoa2017 <- cbind((transitivity(fcoa2017_1, type = c("average"), weights = TRUE)), (transitivity(fcoa2017_2, type = c("average"), weights = TRUE)), (transitivity(fcoa2017_3, type = c("average"), weights = TRUE)),  (transitivity(fcoa2017_4, type = c("average"), weights = TRUE)))

fcoastats2017 <- rbind(sizesfcoa2017,densfcoa2017, transfcoa2017)
rownames(fcoastats2017) <- c("Size", "Density", "Transitivity")
colnames(fcoastats2017) <- c("Coalition A", "Coalition B", "Coalition C", "Coalition D")
fcoastats2017 <- t(fcoastats2017)
fcoastats2017

sizesfcoa2018a <- sizes(fclp2018a)

densfcoa2018a <- cbind((edge_density(fcoa2018a_1, loops = TRUE)), (edge_density(fcoa2018a_2, loops = TRUE)), (edge_density(fcoa2018a_3, loops = TRUE)),  (edge_density(fcoa2018a_4, loops = TRUE)), (edge_density(fcoa2018a_5, loops = TRUE)), (edge_density(fcoa2018a_6, loops = TRUE)))

transfcoa2018a <- cbind((transitivity(fcoa2018a_1, type = c("average"), weights = TRUE)), (transitivity(fcoa2018a_2, type = c("average"), weights = TRUE)), (transitivity(fcoa2018a_3, type = c("average"), weights = TRUE)),  (transitivity(fcoa2018a_4, type = c("average"), weights = TRUE)), (transitivity(fcoa2018a_5, type = c("average"), weights = TRUE)), (transitivity(fcoa2018a_6, type = c("average"), weights = TRUE)))

fcoastats2018a <- rbind(sizesfcoa2018a,densfcoa2018a, transfcoa2018a)
rownames(fcoastats2018a) <- c("Size", "Density", "Transitivity")
colnames(fcoastats2018a) <- c("Coalition A", "Coalition B", "Coalition C", "Coalition D", "Coalition E", "Coalition F")
fcoastats2018a <- t(fcoastats2018a)
fcoastats2018a

sizesfcoa2018b <- sizes(fclp2018b)

densfcoa2018b <- cbind((edge_density(fcoa2018b_1, loops = TRUE)), (edge_density(fcoa2018b_2, loops = TRUE)), (edge_density(fcoa2018b_3, loops = TRUE)),  (edge_density(fcoa2018b_4, loops = TRUE)))

transfcoa2018b <- cbind((transitivity(fcoa2018b_1, type = c("average"), weights = TRUE)), (transitivity(fcoa2018b_2, type = c("average"), weights = TRUE)), (transitivity(fcoa2018b_3, type = c("average"), weights = TRUE)),  (transitivity(fcoa2018b_4, type = c("average"), weights = TRUE)))

fcoastats2018b <- rbind(sizesfcoa2018b,densfcoa2018b, transfcoa2018b)
rownames(fcoastats2018b) <- c("Size", "Density", "Transitivity")
colnames(fcoastats2018b) <- c("Coalition A", "Coalition B", "Coalition C", "Coalition D")
fcoastats2018b <- t(fcoastats2018b)
fcoastats2018b
```

