library(ggplot2)

df1 = read.csv("C:/Program Files/R/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Machine Learning Projects/CSV files for ML Projects/winequality-red.csv", sep=';')
df2 = read.csv("C:/Program Files/R/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Machine Learning Projects/CSV files for ML Projects/winequality-white.csv", sep=';')

head(df1)
head(df2)

#merge the dataframes
df1$label <- 'red'
df2$label <- 'white'
wine = rbind(df1,df2)
str(wine)

#Histogram of residual sugar from the wine data. Color by red and white wines.
pl1 <- ggplot(data=wine, aes(x=residual.sugar, fill=factor(label)))
pl1 + geom_histogram(alpha=0.4)

#Histogram of citric.acid from the wine data. Color by red and white wines
pl2 <- ggplot(data=wine, aes(x=citric.acid, fill=factor(label)))
pl2 + geom_histogram(alpha=0.4, bins=50)

#Histogram of alcohol from the wine data. Color by red and white wines.
pl3 <- ggplot(data=wine, aes(x=alcohol, fill=factor(label)))
pl3 + geom_histogram(alpha=0.4, bins=50)

#Scatterplot of residual.sugar versus citric.acid, color by red and white wine.
pl4 <- ggplot(data=wine, aes(x=citric.acid, y=residual.sugar))
pl4 + geom_point(aes(color=factor(label)))

#Scatterplot of volatile.acidity versus residual.sugar, color by red and white wine.
pl5 <- ggplot(data=wine, aes(x=volatile.acidity, y=residual.sugar))
pl5 + geom_point(aes(color=factor(label)))

clus.data <- wine
clus.data <- clus.data[, -13]

wine.cluster <- kmeans(clus.data[, 1:12], 2)

print(wine.cluster$centers)

table(wine$label, wine.cluster$cluster)
