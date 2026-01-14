library(igraph)
library(stringr)
library(bipartite)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(tidyr)

Melt_data <- melt(dat, id.vars=c("Month", "Bee.genus"))

June[is.na(June)] = 0

My_data3 <- my_data[, c(1, 2, 4, 3)] ## 1 is plant, 2 is bee taxa, 3 is grouping, 4 is frequency of interaction

#reorganizes columns 

newdat <- newdat[, c(2, 1, 3, 4)]

#makes an array to make the other functions work 

frame2webs(newdat, varnames = c("Bee", "Plant", "ID", "Frequency"), type.out="array")

# get connectance scores - must had dataset where 1 is plant, 2 is bee taxa, 3 is grouping, 4 is frequency of interaction
sapply(frame2webs(newdat, varnames = c("Bee", "Plant", "ID", "Frequency"), type.out="list"), networklevel, index=c("connectance", "C score"))

#makes dataset have row names rather than a first column with genera names (makes everything numeric). This dataset should be like Safariland. 

rownames(may) <- may[,1]
may <- may[,-1]


#plots
visweb(may, labsize = 2)

plotweb(as.data.frame(may),text.rot= 90,labsize=1)

#How are species ranked from specialist to generalist? Closer to 1 means perfect specialist

dfun(webdat, abuns=NULL)

#nested rank

ranks <- sapply(c("nodf", "binmatnest", "wine", "sort"), function(x)
  nestedrank(webdat, method=x)[[2]])
