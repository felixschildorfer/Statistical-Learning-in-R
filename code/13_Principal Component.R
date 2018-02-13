# inspect data
dimnames(USArrests)
apply(USArrests, 2,mean)
apply(USArrests,2,var)

# PCA
pca.out = prcomp(USArrests,scale = T)
pca.out
names(pca.out)
biplot(pca.out,scale = 0, cex =.6)

