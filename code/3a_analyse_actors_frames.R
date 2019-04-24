
#---------------------------------------------------------------------
# Us and Them: Mapping Discourse Coalitions in the EU Copyright Debate
# Bruno St-Jacques (Hertie School of Governance)
# 2019
#---------------------------------------------------------------------

# ==============================================================================
# Distribution of actors/frames
# ==============================================================================


# lists
actors_list <- select(atA, "value", "type")
frames_list <- select(atF, "type", "alias", "value")
write.csv(actors_list, file = "actors_list.csv")
write.csv(frames_list, file = "frames_list.csv")


# coalitions actors + statements
act_coa2019_1 <- read.csv(file = "act_coa2019_1.csv")
act_coa2019_2 <- read.csv(file = "act_coa2019_2.csv")
act_coa2019_3 <- read.csv(file = "act_coa2019_3.csv")

# coalition members & frames
library(xlsx)
eventlist <- as.data.frame(read.xlsx("eventlist_0904.xlsx", sheetIndex = 1, as.data.frame = TRUE, header = TRUE))
elist3 <- select(eventlist, 
                 agreement, concept, organization, person, source)
atflist <- select(atF, value, type, alias)  
names(atflist)[names(atflist) == 'value'] <- 'concept'
AFlist <- left_join(atflist, elist3)

names(AFlist)[names(AFlist) == 'concept'] <- 'statement'
names(AFlist)[names(AFlist) == 'type'] <- 'thematic'
names(AFlist)[names(AFlist) == 'alias'] <- 'frame'
names(AFlist)[names(AFlist) == 'source'] <- 'org_type'

act_coa2019_1 <- as.data.frame(select(attr_coa2019_1, name))
names(act_coa2019_1)[names(act_coa2019_1) == 'name'] <- 'organization'
act_coa2019_1 <- left_join(act_coa2019_1, AFlist)
act_coa2019_2 <- as.data.frame(select(attr_coa2019_2, name))
names(act_coa2019_2)[names(act_coa2019_2) == 'name'] <- 'organization'
act_coa2019_2 <- left_join(act_coa2019_2, AFlist)
act_coa2019_3 <- as.data.frame(select(attr_coa2019_3, name))
names(act_coa2019_3)[names(act_coa2019_3) == 'name'] <- 'organization'
act_coa2019_3 <- left_join(act_coa2019_3, AFlist)

names(act_coa2019_1)[names(act_coa2019_1) == 'concept'] <- 'statement'
names(act_coa2019_1)[names(act_coa2019_1) == 'type'] <- 'thematic'
names(act_coa2019_1)[names(act_coa2019_1) == 'alias'] <- 'frame'
names(act_coa2019_1)[names(act_coa2019_1) == 'source'] <- 'org_type'
names(act_coa2019_2)[names(act_coa2019_2) == 'concept'] <- 'statement'
names(act_coa2019_2)[names(act_coa2019_2) == 'type'] <- 'thematic'
names(act_coa2019_2)[names(act_coa2019_2) == 'alias'] <- 'frame'
names(act_coa2019_2)[names(act_coa2019_2) == 'source'] <- 'org_type'
names(act_coa2019_3)[names(act_coa2019_3) == 'concept'] <- 'statement'
names(act_coa2019_3)[names(act_coa2019_3) == 'type'] <- 'thematic'
names(act_coa2019_3)[names(act_coa2019_3) == 'alias'] <- 'frame'
names(act_coa2019_3)[names(act_coa2019_3) == 'source'] <- 'org_type'

act_coa2019_1 <- act_coa2019_1[c(5,2,4,7,1,3,6)]
act_coa2019_2 <- act_coa2019_2[c(5,2,4,7,1,3,6)]
act_coa2019_3 <- act_coa2019_3[c(5,2,4,7,1,3,6)]
#write.csv(act_coa2019_1, file = "act_coa2019_1.csv")
#write.csv(act_coa2019_2, file = "act_coa2019_2.csv")
#write.csv(act_coa2019_3, file = "act_coa2019_3.csv")

fcoa1 <- summary(act_coa2019_1$frame)
fcoa2 <- summary(act_coa2019_2$frame)
fcoa3 <- summary(act_coa2019_3$frame)
fcoa1 <- (fcoa1/sum(fcoa1)*100) 
fcoa1 <- sort(fcoa1, decreasing = TRUE)
fcoa2 <- (fcoa2/sum(fcoa2)*100) 
fcoa2 <- sort(fcoa2, decreasing = TRUE)
fcoa3 <- (fcoa3/sum(fcoa3)*100) 
fcoa3 <- sort(fcoa3, decreasing = TRUE)

tcoa1 <- summary(act_coa2019_1$thematic)
tcoa2 <- summary(act_coa2019_2$thematic)
tcoa3 <- summary(act_coa2019_3$thematic)
tcoa1 <- (tcoa1/sum(tcoa1)*100) 
tcoa1 <- sort(tcoa1, decreasing = TRUE)
tcoa2 <- (tcoa2/sum(tcoa2)*100) 
tcoa2 <- sort(tcoa2, decreasing = TRUE)
tcoa3 <- (tcoa3/sum(tcoa3)*100) 
tcoa3 <- sort(tcoa3, decreasing = TRUE)

# statements per org type per coalition

coa1_C <- act_coa2019_1 %>% 
  filter(org_type == "Creators")  
coa1_DM <- act_coa2019_1 %>% 
  filter(org_type == "Decision Makers")
coa1_M <- act_coa2019_1 %>% 
  filter(org_type == "Media")
coa1_NPO <- act_coa2019_1 %>% 
  filter(org_type == "Nonprofits & CSOs")
coa1_RP <- act_coa2019_1 %>% 
  filter(org_type == "Rightsholders & Publishers")
coa1_SO <- act_coa2019_1 %>% 
  filter(org_type == "Scientific Organisations")
coa1_TC <- act_coa2019_1 %>% 
  filter(org_type == "Tech Companies")
coa1_V <- act_coa2019_1 %>% 
  filter(org_type == "Various")

write.xlsx(coa1_C, file = "coa1_C.xlsx")
write.xlsx(coa1_DM, file = "coa1_DM.xlsx")
write.xlsx(coa1_M, file = "coa1_M.xlsx")
write.xlsx(coa1_NPO, file = "coa1_NPO.xlsx")
write.xlsx(coa1_RP, file = "coa1_SO.xlsx")
write.xlsx(coa1_TC, file = "coa1_TC.xlsx")
write.xlsx(coa1_V, file = "coa1_V.xlsx")

library(data.table)
elist2 <- select(eventlist, 
                 agreement, concept, organization, source)
atflist <- select(atF, value, type, alias)  
names(atflist)[names(atflist) == 'value'] <- 'concept'

elist2 <- left_join(atflist, elist2)
elist2 <- elist2[c(4,6,5,1,3,2)]
names(elist2)[names(elist2) == 'alias'] <- 'frame'
names(elist2)[names(elist2) == 'type'] <- 'thematic'
names(elist2)[names(elist2) == 'concept'] <- 'statement'

elist_C <- elist2 %>% 
  filter(source == "Creators") 
elist_DM <- elist2 %>% 
  filter(source == "Decision Makers")
elist_M <- elist2 %>% 
  filter(source == "Media")
elist_NPO <- elist2 %>% 
  filter(source == "Nonprofits & CSOs")
elist_RP <- elist2 %>% 
  filter(source == "Rightsholders & Publishers")
elist_SO <- elist2 %>% 
  filter(source == "Scientific Organisations")
elist_TC <- elist2 %>% 
  filter(source == "Tech Companies")
elist_V <- elist2 %>% 
  filter(source == "Various")


# distribution of actors/frames

# total number of actors
act <- summary(atA$type)
act1 <- c(9,12,3,4,13,5,6,4)
act1per <- (act1/56)*100
round(act1per)
sta <- summary(elist2$source)
sta1 <- c(57,66,12,36,96,53,54,51)
sta1per <- (sta1/425)*100

# group frame and actors' attributes using the event list
eventlist <- as.data.frame(read.xlsx("eventlist_0904.xlsx", sheetIndex = 1, as.data.frame = TRUE, header = TRUE))
unique(eventlist$concept)

elist2 <- select(eventlist, 
                 agreement, concept, organization, source)

elist <- select(eventlist, statement.ID, source, organization, concept)
atflist <- select(atF, value, type, alias)  
names(atflist)[names(atflist) == 'value'] <- 'concept'

AFlist <- left_join(atflist, elist)
names(AFlist)[names(AFlist) == 'concept'] <- 'statement'
names(AFlist)[names(AFlist) == 'type'] <- 'thematic'
names(AFlist)[names(AFlist) == 'type'] <- 'thematic'
names(AFlist)[names(AFlist) == 'alias'] <- 'frame'
names(AFlist)[names(AFlist) == 'source'] <- 'org_type'
AFlist <- AFlist[c(4,1,3,2,6,5)]

# plot organisations statements by thematic frames
orgthem <- select(AFlist, org_type, thematic)
orgthem.table <- as.matrix(table(orgthem))
orgthem.df <- as.data.frame.matrix(table(orgthem))
orgthem.melt <- melt(orgthem.table)

# percentages per group
cre <- orgthem.melt %>% filter(org_type == "Creators") 
cre <- group_by(cre, thematic) %>% mutate(percent = value/sum(value)*100)
dm <- orgthem.melt %>% filter(org_type == "Decision Makers")
dm <- group_by(dm, thematic) %>% mutate(percent = value/sum(value)*100)
me <- orgthem.melt %>% filter(org_type == "Media")
me <- group_by(me, thematic) %>% mutate(percent = value/sum(value)*100)
npo <- orgthem.melt %>% filter(org_type == "Nonprofits & CSOs")
npo <- group_by(npo, thematic) %>% mutate(percent = value/sum(value)*100)
rp <- orgthem.melt %>% filter(org_type == "Rightsholders & Publishers")
rp <- group_by(rp, thematic) %>% mutate(percent = value/sum(value)*100)
sci <- orgthem.melt %>% filter(org_type == "Scientific Organisations")
sci <- group_by(sci, thematic) %>% mutate(percent = value/sum(value)*100)
tech <- orgthem.melt %>% filter(org_type == "Tech Companies")
tech <- group_by(tech, thematic) %>% mutate(percent = value/sum(value)*100)
vari <- orgthem.melt %>% filter(org_type == "Various")
vari <- group_by(vari, thematic) %>% mutate(percent = value/sum(value)*100)
orgthem.percent <- rbind(cre, dm, me, npo, rp, sci, tech, vari)

# custom palette
palette1 <- c("#0D1B2A", "#1A2A3A", "#415B77", "#758EA8", "#DBDEE0", "#96C5F7", "#25578C", "#404D5B")
palette2 <- c("#ee6055", "#60d394", "#aaf683", "#ffd97d", "#ff9b85", "#52414c", "#465775", "#e6e6e6")
scale_fill_manual(values=palette1) # add to ggplot
scale_fill_brewer("Set1")
scale_fill_brewer("Set1")

orgthem.melt$org_type <- revalue(orgthem.melt$org_type, c("Scientific Organisations"="Scientific\nOrganisations"))
orgthem.melt$org_type <- revalue(orgthem.melt$org_type, c("Rightsholders & Publishers"="Rightsholders\n& Publishers"))
orgthem.melt$org_type <- revalue(orgthem.melt$org_type, c("Nonprofits & CSOs"="Nonprofits\n& CSOs"))
orgthem.melt$org_type <- revalue(orgthem.melt$org_type, c("Tech Companies"="Tech\nCompanies"))
orgthem.melt$org_type <- revalue(orgthem.melt$org_type, c("Decision Makers"="Decision\nMakers"))

orgthem.melt$thematic <- revalue(orgthem.melt$thematic, c("Cultural Implications"="Cultural Implications  "))
orgthem.melt$thematic <- revalue(orgthem.melt$thematic, c("Decision-Making Process"="Decision-Making Process  "))
orgthem.melt$thematic <- revalue(orgthem.melt$thematic, c("Economic Development"="Economic Development  "))


# plot organisations statements by thematic frames
pthem <- ggplot(orgthem.melt,aes(x = org_type, y = value, fill = thematic)) + 
  scale_fill_brewer("Set2") +
  theme_classic() +
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  theme(legend.position = "top", 
        legend.justification = "left") +
  theme(panel.grid.major.x = element_line(color="#B8B8B8"), 
        panel.grid.major.y=element_blank(),
        axis.text=element_text(size=10),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.line = element_blank())

#labs(title="Organisations and Statements",
#subtitle = "% of statements by thematic area") +

# organisations statements by frames
orgframe <- select(AFlist, org_type, frame)
orgframe.table <- as.matrix(table(orgframe))
orgframe.df <- as.data.frame.matrix(table(orgframe))
orgframe.melt <- melt(orgframe.table)


# plot organisations statements by frames
pframe <- ggplot(orgframe.melt,aes(x = org_type, y = value, fill = frame)) + 
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "top", 
        legend.justification = "left") +
  theme(panel.grid.major.x = element_line(color="#B8B8B8"), 
        panel.grid.major.y=element_blank(),
        axis.text=element_text(size=8),
        legend.text = element_text(size = 8),
        legend.title = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.background = element_rect(fill = "#F5F5F5")) 

#labs(title="Organisational Type by Frame",
#subtitle = "% of statements by frame") +

# save plots
finalise_plot(plot_name = pthem,
              source = " ",
              save_filepath = "pthem.png",
              width_pixels = 640,
              height_pixels = 450
)

# frames distribution
sum_thematic <- atF %>% 
  group_by(type) %>% 
  summarise(frequency = sum(frequency))
sum_frames <- atF %>% 
  group_by(type, alias) %>% 
  summarise(frequency = sum(frequency))

