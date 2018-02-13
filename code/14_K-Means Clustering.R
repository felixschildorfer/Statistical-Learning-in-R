# Set Seed and Plot Data
set.seed(34)
x <- matrix(rnorm(100*2),100,2)
x_mean <- matrix(rnorm(8,sd=4),4,2)
which <- sample(1:4,100,replace=T)
x <- x + x_mean[which,]
plot(x,col=which,pch=19)

# K-Means
km.out <- kmeans(x,4,nstart=15)
km.out

# Plot Results
plot(x,col=km.out$cluster,cex = 2, pch=1,lwd=2)
points(x,col=which,pch=19)
points(x,col=c(4,3,2,1)[which],pch=19)

# Hirarchical Clustering
hc.complete <- hclust(dist(x),method="complete")
plot(hc.complete)
hc.single=hclust(dist(x),method = "single")
plot(hc.single)
hc.average=hclust(dist(x),method = "average")
plot(hc.average)

hc.cut = cutree(hc.complete,4)
table(hc.cut,which)
table(hc.cut,km.out$cluster)

plot(hc.complete,labels=which)
