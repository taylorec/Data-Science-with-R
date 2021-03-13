# This project performs PCA on the USArrests data set

states = row.names(USArrests)

# ?USArrests # run for more info on the data
names(USArrests) 

# Examine the data
apply(USArrests, 2, mean)
apply(USArrests, 2, var)
pairs(USArrests)
cor(USArrests)

pr.out <- princomp(USArrests, scores=TRUE, cor=TRUE)
summary(pr.out) # shows importance of components; proportion of variance
		    # explains how much of the data is explained by that prinicipal component

names(pr.out)
pr.out$loadings # check how much of each variable is contained in each component

plot(pr.out) 








