#### hierarichal clustering  for Airline  #############
airline <- EastWestAirlines_csv
View(airline)
k <- sqrt(3999/2)
k
normalized <- scale(airline[,-1])
d <- dist(normalized,method = "euclidean")
clus <- hclust(d,method = "complete")
plot(clus)
plot(clus,hang = -1)
rect.hclust(clus,k=44,border = "red")
groups <- cutree(clus,k=44)
membership <- as.matrix(groups)
final <- data.frame(airline,membership)
View(final)
write.csv(final, file="finalairlinehcluster.csv",row.names = F)
getwd()
aggregate(airline[,-1],by=list(final$membership),mean)
#####################   kmeans clustering for airline dataset########################################

normalized_data<-scale(airline[,-1])
k <- sqrt(3999/2)
k
fit<- kmeans(normalized_data, 44) 
plot(fit)
final2<- data.frame(airline[,-1], fit$cluster) 
View(final2)
write.csv(final2, file="finalairlinekmeancluster.csv",row.names = F)
getwd()
plot(final2)
aggregate(crime, by=list(fit$cluster), FUN=mean)
