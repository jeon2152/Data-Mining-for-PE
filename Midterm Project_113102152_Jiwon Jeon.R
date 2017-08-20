#Select data
rawdata = read.csv("data.csv", header = TRUE)
attach(rawdata)

#Select log columns to be analyzed
logdata = rawdata[, 4:8]
attach(logdata)

#Scale logdata
logdata_scale = scale(logdata)

#Determine number of clusters - K-Means clustering (SSW & SSB)
wss = (nrow(logdata_scale)-1)*sum(apply(logdata_scale,2,var)) 
for (i in 2:15) {
  wss[i] = sum(kmeans(logdata_scale, centers=i)$withinss)
}

bss = (nrow(logdata_scale)-1)*sum(apply(logdata_scale,2,var)) 
for (i in 2:15) {
  bss[i] = sum(kmeans(logdata_scale, centers=i)$betweenss)
}

plot(wss)
points(bss)
mtext("bss", side = 4)
axis(side = 4, labels = TRUE)

#Compute clusters - K-Means clustering
kmeans = kmeans(logdata_scale, 4, iter.max = 100, nstart = 10)
kmeans

cluster = kmeans$cluster
centroid = kmeans$centers
size = kmeans$size

#Unscale the centroids
library(DMwR)
centroid_original = unscale(centroid, logdata_scale)

#Assign original data to clusters
eletrofacies = as.matrix(cluster)
output = cbind(rawdata, eletrofacies)
write.csv(output, "electrofacies_output.csv")

#Visualize clusters
##Reduce dimensions for 3D visualization - PCA
pca = prcomp(logdata_scale)
pca_print = pca$rotation
comp = data.frame(pca$x[,1:3])
plot(comp, pch=16, col = kmeans$cluster)


library(rgl)
plot3d(comp$PC1,comp$PC2,comp$PC3,col=kmeans$cluster)
