#ISOM673_HW2
rm(list = ls(all = TRUE))
library(igraph)
library(lubridate)
library(data.table)
library(zoo)

outcome = fread("C:\\Users\\BSim0\\Downloads\\Venture_capital_firm_outcomes.csv", header = TRUE)
events1 = fread("C:\\Users\\BSim0\\Downloads\\Funding_events_7.14.csv", header = TRUE)
events2 = fread("C:\\Users\\BSim0\\Downloads\\Funding_events_7.14_page2.csv", header = TRUE)
events2 <- as.data.frame(events2)
#View(events2)

# summary(events1)
# sum(is.na(outcome))
# sum(is.na(events1$Investors))
# sum(is.na(events2))
# #sum(is.na(events))

events1 <- events1[ !is.na(events1$Investors) & events1$Investors !="", ]
events2 <- events2[ !is.na(events2$Investors) & events2$Investors !="", ]
#View(events1)

colnames(events1) = colnames(events2)
events <- rbind(events1,events2)

#events <- merge.data.frame(events1, events2, all=TRUE)
events <- events[ , c(1,4,11)]
events2 <- events2[, c(1,4,11)]
#View(events)
dim(events)

# events <- events[!duplicated(events),]
# events$`Deal Date` <- as.Date(events$`Deal Date`,format = '%m/%d/%y') 

edgties_monthatrix <- matrix(,,3)
colnames(edgties_monthatrix) = c("from", "to" ,"date")

for (row in 1:nrow(events)) {
  nodes <- trimws( unlist(strsplit(events[row,]$Investors, ",") ) )
  if(length(nodes) >= 2 ) {
    firm <- t(combn(nodes, 2))
    firm <- cbind(firm, matrix(events$`Deal Date`[row], nrow(firm) ) )
    edgties_monthatrix <- rbind(edgties_monthatrix, firm)
  }
}
df_edge <- as.data.frame(edgties_monthatrix[-1,])

# df_edge2 <- df_edge2[which(df_edge2[,2] != "Ltd."), ]
# df_edge2 <- df_edge2[which(df_edge2[,2] != "Inc."), ]
# df_edge2 <- df_edge2[which(df_edge2[,1] != "Inc."), ]
df_edge <- df_edge[which(nchar(df_edge[,1]) > 4), ]
df_edge <- df_edge[which(nchar(df_edge[,2]) > 4), ]

# which(df_edge2[,2] != "Ltd.")
# which(df_edge2[,2] != "Inc.")
# which(nchar(df_edge2[,1]) < 5)

View(df_edge)

#Visualize full network
#edge_network = graph.data.frame(df_edge, directed = FALSE)

#plot.igraph(edge_network, edge.color = "black", edge.arrow.size=.1, vertex.size = 5, layout=layout.kamada.kawai, vertex.label.size = .1)

#nchar() #compare 1st and 2nd columns to return rows where value < 5
# just remove the shortest ones, even with the abbreviations

######################   1.    ####################
# 1. First, perform the Kevin Bacon Hollywood Actor exercise on the venture capital rm network.
# For constructing this network, we want to avoid the possibility that the network simply
# grows more dense over time, so we want to allow old ties to drop out of the network. We will
# consider a tie to have decayed if it is not renewed within a certain amount of time: a time
# window greater than 90 percent of all renewal windows for all ties in the network.
#
# These decayed ties should be removed from the current construction of the network. Use this
# trimmed network for the remainder of the exercises.
#
# Which rm is the center of the venture capital rm network as of July 2014? Consider the
# most central rm to be the rm with the highest closeness centrality, as in the Hollywood
# Actor example.

df_edge <- df_edge[!duplicated(df_edge), ]
df_edge$date <- as.Date(df_edge$date, format = '%m/%d/%y')
df_edge$date <- floor_date(df_edge$date, unit = "month")
df_edge <- df_edge[order(df_edge$date), ]

df_edge <- df_edge[df_edge$date <= '2014-07-08',]
row.has.na <- apply(df_edge, 1, function(x){any(is.na(x))})
sum(row.has.na)
df_edge <- df_edge[!row.has.na,]

table_ties <- data.table(date = df_edge$date, V1 = df_edge$from, V2 = df_edge$to,
                         month = as.yearmon(df_edge$date))

#data table with DECAYED ties
table_ties[,update := date - shift(date, 1L), by=c("V1","V2")]

#THRESHOLD
threshold <- quantile(table_ties[!is.na(update)]$update, .9, na.rm=TRUE)
threshold

#DECAYED ties per month
months <- sort(unique(table_ties$month))
min <- length(months) - (as.numeric(threshold)/30.417)
min <- floor(min)
months <- months[min:length(months)]
ties_month <- list()

for(i in seq_along(months)) {
  ties_month[[i]] <- table_ties[month <= months[i]]
  ties_month[[i]][,current_month := months[i]]
  ties_month[[i]][,tie_age := current_month - month]
  ties_month[[i]] <- ties_month[[i]][tie_age <= threshold,]
}
View(ties_month)

# GRAPH per each month
plot.igraph( graph.data.frame(ties_month[[1]]), layout=layout.kamada.kawai, vertex.size=1 , label.size = .5, vertex.label=NA)
# plot.igraph(network_social,layout=layout.kamada.kawai, vertex.label.color="black",edge.color="black",edge.width=E(network_social)$weight,vertex.size = 14, edge.arrow.size=.2,edge.curved=FALSE)

#closness for all nodes
closeness(monthly_graphs[[length(monthly_graphs)]])[which.max(closeness(monthly_graphs[[length(monthly_graphs)]]))]


## floor_date()  round date
## quantile() with vector scores
## group by month
## 1981 - 2014
## closeness.centrality - closeness() which of companies have highest
## exclude ties that only appear once
## 3 years
## graph per month and edgelist per month and subset based on age with current month subtract date tie occured

############# 2. ################
# 2. Next, we will look at the development of the local group membership of the co-investment
# network over time. Allow the network to be up dated monthly for each month in the data,
# adding the new ties that occur through investments in the current month to be added to the
# existing network of ties that have occurred in previous months.
#
# In Class Session 4, the figure on Slide 24 plotted over time the industry average of the highest-
#   degree k-core each venture capital firm in the co-investment network belonged to. When a
# node is a member of a k-core with a high degree, its surrounding ties are very dense. When
# many no desire members of k-cores with high degrees, this suggests that there may exist
# dense subgroups within the network.
#
# Construct a figure similar to Class Session 4's, plotting the average k-core of each venture
# capital rm in the network over time. This can be computed using the igraph function
# coreness. On the x-axis should be time. On the y-axis should be the highest-degree k-core
# each venture capital firm belongs to, averaged over all firms in the network up to that month

for (graph in monthly_graphs) {
  coreness_month <- coreness(monthly_graphs[graph], mode = "all")
  time <- seq.Date(min(monthly_graphs[graph]$month), max(monthly_graphs[graph]$month), by = "month")
  plot(coreness_month,time)
    
}


############# 3. ###############
#### 3. Next, we will look at the development of the venture capital firm co-investment network in
# terms of its global core-periphery structure. Allow the network to be up dated monthly, as
# in Question 2.

###### (A) Does the network appear to conform to a core-periphery structure?
#   Calculate the co-investment network's concentration scores to determine if it tends to-
# wards a core-periphery structure across the data. Illustrate a figure, with one plot for
# one month from each calendar year in the data, that shows the range of concentration
# scores for each partition size p for the range of p in 1 to the number of nodes in the
# networks in the network for that month's cross-section. You can exclude the very early
# period of the data when all of the rms have the same eigenvector centrality.

## sort by eigenvector centrality of each sub network from highest to lowest
## for each of those sorted vectors, take correlation between that vector and
## the ideal parition vector (vector of 1s and 0s : number of parition sizes up to num of nodes in network)
## generte vector with 1 1 in it and rest 0s, for each of those vectors take the cor
## with the eigenvector centrality scores
## ideal parition vector = vector of 1s and 0s represent that core nodes all connnected and
## outside nodes not connected.



###### (B) Provide one other piece of descriptive evidence outside of the concentration scores to
# support your conclusion. You can see Slide 29 of Class 4 for some examples of evidence
# to use.

# historgram of closeness centrality to show high/low closeness or multi dimensional scaling
# would show dense in one location with surrounding OR
# large percentage of nodes are in largest connected component. Get components out of network using
# component_distribution = tells how many nodes in each compenent. Show one big component
# that all nodes are connected to

## first 13 months of data have same eigenvector centrality scores

############### 4. ##################
# 4. Next, we will compare the core-periphery structure of the network to the p otential for a
# clustered structure. In Slide 25 of Class 4 we saw that the network tended towards a strong
# core-p eriphery structure at the end of the animation in the year 2000. Before this p oint,
# some clustering structures might b e more apparent. Let's try to see if a clustering structure
# exists in the year 1996.

# If we tried to cluster the network in June of 1996 using partitioning around medoids, what
# would b e the recommended numb er of clusters? Are any of the clustering solutions suitable
# under the rule of thumb of achieving an average silhouette width greater than 0.7 (or even
# the weaker threshold of 0.5)? What does this suggest about the clustering approach versus
# the core-periphery approach?

## if avg silohoute width is less than .7 than still not good representation of the network
## compared to core periphery, the peak in the graph shows core preiphery is a better
## representaiton of the network

# For the clustering, you can use the function pam from the cluster library. You can use the
# default distance matrix of the network provided by proxy::dist or dist as the input for
# the clustering. Show the silhouette plot for the betting clustering solution for the June
# 1996 network.

# In addition to the reference guide on Slide 60 of Class 3, you may also nd useful this short
# example using the iris dataset that is built in to R. You can load this data using data(iris).

## inputs for matrix: dist(  get.adjacency(network)    )
## optimal number of clusters -> pam() from cluster library