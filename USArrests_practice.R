##### _ _ _ _ _ _ _ _ USArrests _ _ _ _ _ _ _ _ ####

library(datasets)

dt_usa <- USArrests
dt_usa <- dt_usa[c("Murder", "Assault", "Rape", "UrbanPop")]


### PART 1 - EXTRACT SPECIFIC DATA ###

## P1) Extract the 25% of states with less arrest by murder

# first we sort the data by murder
sortedDF <- USArrests[order(USArrests$Murder), ]

# the are two ways of getting the 25% states:
# way 1:
numrows <- round(nrow(sortedDF)*0.25)
way1 <- sortedDF[1:numrows, ]

# way 2:
summary(sortedDF)  # here you check the Q1 value (in this case 4)
way2 <- sortedDF[sortedDF$Murder < 4, ]

## P2) Extract the states whose Assault is >200 or Rape >20
assault_rape = dt_usa[dt_usa$Assault>200 | dt_usa$Rape>20, ]
  

### PART 2 - DATA MINING ###  

#basic:
summary(dt_usa)  
boxplot(dt_usa)  
  # Assault is the highest by far.

#pie chart:
library(RColorBrewer)
means = round(c(mean(dt_usa$Murder), mean(dt_usa$Assault), mean(dt_usa$Rape)), 2)
lbls<- c("Murder", "Assault", "Rape")
pct <- round(means/sum(means)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(means,labels = lbls, col=brewer.pal(3, "Set1"))
  
#correlations:
library(psych)
pairs.panels(dt_usa[,1:4], 
             method = "pearson", 
             hist.col = "#00AFBB",
             density = TRUE,
             ellipses = F,
             pch = 20,
             lm=T,
             size
)  

# Assault is strongly correlated with Murder (0.8), and a bit with Rape (0.67)
# Crimes are not correlated with the population living in urban areas!


### PARTE 3 - MACHINE LEARNING ###
       ## NOT SUPERVISED ##

## Data normalization:
znorm <- function(x){
  x.mean <- mean(x)
  x.dev <- sd(x)
  (x - x.mean)/x.dev
}

dt_usa_norm <- as.data.frame(lapply(dt_usa, znorm))
row.names(dt_usa_norm) <- row.names(dt_usa)

## k-means:

# number of k:

wss <- (nrow(dt_usa_norm)-1)*sum(apply(dt_usa_norm,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(dt_usa_norm,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

  # optimal number of clusters = 4

kms <- kmeans(dt_usa_norm, 4)
 
cluster_clas <- data.frame(kms$cluster) # see to which cluster do the states
                                        # belong

plot(dt_usa, col = kms$cluster, pch=19)

## PAM:       # *just to see the difference between Kmeans and this method
library(ggfortify)
library(cluster)

autoplot(pam(dt_usa_norm, 4), frame = TRUE, frame.type = 'norm', label = T,
         loadings=TRUE, loadings.colour = 'black', loadings.label = TRUE, 
         loadings.label.size = 4, loadings.label.colour = "black")


### PART 4 - VISUALIZATION ###
library(ggplot2)
library(maps)

statesMap <- map_data("state")
str(USA_map) # the states names are in lowercase, so we need to change that
             # in our dataset in order to merge info
dt_usa$region <- tolower(row.names(dt_usa))
dt_usa$cluster <- cluster_clas$kms.cluster

ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")
  # group: groups states by variable group
  # fill: map color
  # color: outline color

arrest.map <- merge(statesMap, dt_usa, by="region") 
head(arrest.map) # check the merge result

# Assault plot:
ggplot(arrest.map, aes(x = long, y = lat, group = group, fill = Assault)) + 
  ggtitle("Assault map") +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "white", high = "red", guide = "legend")
  
# Murder plot:
ggplot(arrest.map, aes(x = long, y = lat, group = group, fill = Murder)) + 
  ggtitle("Murder map") +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "white", high = "blue", guide = "legend")

# Rape plot:
ggplot(arrest.map, aes(x = long, y = lat, group = group, fill = Rape)) + 
  ggtitle("Rape map") +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "white", high = "purple", guide = "legend")

# Urban population plot:
ggplot(arrest.map, aes(x = long, y = lat, group = group, fill = UrbanPop)) + 
  ggtitle("Urban population map") +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "white", high = "green", guide = "legend")

# Cluster plot:

arrest.map$cluster <- factor(arrest.map$cluster) # convert cluster variable
                                                 # to factor
ggplot(arrest.map, aes(x = long, y = lat, group = group, fill= cluster)) + 
  ggtitle("Cluster clasification map") +
  geom_polygon(color = "black") + 
  scale_colour_discrete()

# we can see how cluster clasiffication follows geography for cluster 1, 
# and mostly for cluste 2 and 3, while the states of cluster 4 are dispersed


### PART 5 - WAHT DO THE CLUSTER HAVE IN COMMON ###

autoplot(prcomp(arrest.map[,c("Murder", "Assault", "Rape", "UrbanPop")]),
         data = arrest.map, col = 'cluster', loadings=TRUE,
         loadings.colour = 'black', loadings.label = TRUE, 
         loadings.label.size = 4, loadings.label.colour = "black")

# seems like Assault is high in the state belonging to clusters 1 and 3
# and the lowest in cluster 2
plot(arrest.map$cluster, arrest.map$Assault, col = rainbow(4)) # is true

# in the other hand, UrbanPop seems at clusters 3 and 4
plot(arrest.map$cluster, arrest.map$Assault, col = rainbow(4)) # is true

# finally, Murder and Rape don't seem to have much power when defining clusters
par(mfrow = c(1, 2))
plot(arrest.map$cluster, arrest.map$Murder, col = rainbow(4), main="Murder")
plot(arrest.map$cluster, arrest.map$Rape, col = rainbow(4), main="Rape") 
  # Murder a bit higher at cluster 1 and 3      
  # Rape is a bit higher in cluster 3


            ###########        THE END!!!!   ##########
                   