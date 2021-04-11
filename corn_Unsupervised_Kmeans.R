library(wooldridge)
head(corn)

cor(corn)

par(mfrow=c(2,2))
plot(corn$cornhec, corn$cornpix)
plot(corn$soyhec, corn$soypix)
plot(corn$cornhec, corn$soyhec)
plot(corn$cornpix, corn$soypix)

cornCluster <- kmeans(corn[2:5], 3, nstart = 20)
cornCluster

library(cluster)
clusplot(corn[2:5], cornCluster$cluster, color=TRUE, shade=TRUE, labels=0,lines=0, )
