#NMDS two variables - guide from https://jkzorz.github.io/2019/06/06/NMDS.html

library(ggplot2)
library(plyr)
library(vegan)

Wide_data <- dcast(family, Date + Bee.genus + Bee.species + Sub.genus + Sample.name + Proportion ~ Plant.genus, value.var="Trans.proportion")

#removes empty rows
rm.empty <- Wide_data[!apply(Wide_data == "", 1, all),]

#makes na values 0
Wide_data[is.na(Wide_data)] = 0

#gets rid of variable columns, leaving only a grid with species counts

poll = sum.genus[,3:ncol(sum.genus)]

#poll.scl <- poll %>% scale %>% data.frame #this scales all of the data and turns it into a dataframe.

#poll.hel <- decostand(matrix, "rrank") #this runs the "relative rank transformation" on the data

#this makes it a matrix for the NMDS 
m_com = as.matrix(poll) 

#this runs the NMDS with the regular Bray-curtis distance. For presence/absence, use "jaccard"
nmds = metaMDS(m_com, distance = "bray") 

plot(nmds)

# You have to use this with new version of vegan! No site variable, its just refering to nmds code nonsense. this creates a dataframe of just the scores of the NMDS.
data.scores = as.data.frame(scores(nmds)$sites)

#Adds back variable columns removed earlier. Add columns from origina dataset to data.scores matrix. 
data.scores$Month = sum.genus$Month 
data.scores$Genus =  sum.genus$Genus 
data.scores$Bee.species =  Wide_data$Bee.species 
data.scores$Sub.genus =  Wide_data$Sub.genus


#optional code to remove weird rows or columns
data.scores2 <- data.scores[-c(57, 64), ]

#Using the scores function from vegan to extract the species scores and convert to a data.frame
data.scores = as.data.frame(scores(nmds)$sites)

# create a column of species, from the rownames of species.scores
species.scores$species <- rownames(species.scores)  

#these create groups based on a variable of your choosing and connects them with polygons/lines. I don't usually use this because it can look messy. 
find_hull <- function(data.scores) data.scores[chull(data.scores$NMDS1, data.scores$NMDS2), ]
hulls <- ddply(data.scores , "Bee.genus", find_hull) #This code creates groups based on region.


#Makes a final graph. If something looks weird make sure you have the right data.scores and species.scores files entered. 
ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = Genus), position=position_jitter(width=2,height=2)) + 
  theme(axis.text.y = element_text(colour = "black", size = 12), 
        axis.text.x = element_text(colour = "black", size = 12), 
        legend.text = element_text(size = 12, colour ="black"), 
        legend.position = "right", axis.title.y = element_text(size = 14), 
        axis.title.x = element_text( size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank())  +
  labs(x = "NMDS1", colour = "Bee Genus", shape = "Month", y = "NMDS2") +
  #scale_colour_manual(values = c("#34FF33", "#D4D835", "#A035D8" , "#359ED8","#FFCE30", "#44eed0"))+
  #geom_polygon(data = hulls, aes(x=NMDS1, y=NMDS2, colour=Bee.genus), fill=NA) +
 geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5, position=position_jitter(width=1,height=1))


#This tests a variable of your choosing to see if points significantly cluster by that variable. The closer R is to one, the more different clusters are. 
ano = anosim(m_com, sum.genus$Month, distance = "bray", permutations = 9999)
ano
