#Select core data (PCA)
rawdata = read.csv("Data.csv", header = TRUE)
library(kohonen)
attach(rawdata)
rawdata
pca_rawdata = prcomp(rawdata)
pca_print = pca_rawdata$rotation
pca_print
write.csv(pca_print, "pca_print.csv")
biplot(pca_rawdata)

#Reduced(Selected) core data
coredata = read.csv("Data_selected.csv", header = TRUE)
attach(coredata)
coredata

#Scale core data
coredata_scale = as.matrix(scale(coredata))

#SOM analysis of core data
som_grid = somgrid(xdim = 8, ydim = 8, topo = "hexagonal")
som_model = som(coredata_scale, grid = som_grid, rlen = 500, alpha = c(0.05, 0.01), keep.data = TRUE, n.hood = "circular")

plot(som_model, type = "count")
plot(som_model, type = "changes")
plot(som_model, type = "dist.neighbours")

#Determine number of clusters - compare properties
par(mfrow = c(3,3))
plot(som_model, type = "property", property = som_model$codes[,1], main = names(som_model$data)[1])
plot(som_model, type = "property", property = som_model$codes[,2], main = names(som_model$data)[2])
plot(som_model, type = "property", property = som_model$codes[,3], main = names(som_model$data)[3])
plot(som_model, type = "property", property = som_model$codes[,4], main = names(som_model$data)[4])
plot(som_model, type = "property", property = som_model$codes[,5], main = names(som_model$data)[5])
plot(som_model, type = "property", property = som_model$codes[,6], main = names(som_model$data)[6])
plot(som_model, type = "property", property = som_model$codes[,7], main = names(som_model$data)[7])
plot(som_model, type = "property", property = som_model$codes[,8], main = names(som_model$data)[8])

#Determine number of cluster - compare wss & bss
wss = (nrow(coredata)-1)*sum(apply(coredata,2,var)) 
for (i in 2:15) {
  wss[i] = sum(kmeans(coredata, centers=i)$withinss)
}

bss = (nrow(coredata)-1)*sum(apply(coredata,2,var)) 
for (i in 2:15) {
  bss[i] = sum(kmeans(coredata, centers=i)$betweenss)
}

plot(wss)
points(bss)
mtext("bss", side = 4)
axis(side = 4, labels = TRUE)

#Compute clusters
som_cluster = cutree(hclust(dist(som_model$codes)), 4)

#Visualize clusters
color_palette = c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')
plot(som_model, type="codes", bgcol = color_palette[som_cluster], main = "Clusters")
add.cluster.boundaries(som_model, som_cluster)

#Petrofacies
som_cluster

output = som_model$codes
facies_output = cbind(output, som_cluster)
cluster1 = subset(facies_output, som_cluster == 1)
cluster2 = subset(facies_output, som_cluster == 2)
cluster3 = subset(facies_output, som_cluster == 3)
cluster4 = subset(facies_output, som_cluster == 4)

summary(cluster1)
summary(cluster2)
summary(cluster3)
summary(cluster4)

write.csv(facies_output, "facies_output.csv")


#Import new test data for Prediction
newdata = read.csv("Data_predict.csv", header = TRUE)
attach(newdata)
newdata
newdata_scale = as.matrix(scale(newdata))

#Predict new test data
som_predict = predict(som_model, newdata = newdata_scale, trainY = coredata_scale)
som_predict

#Petrofacies of new test data - compare the grid to the original clusters
som_predict_grid = som_predict$unit.classif
facies_output_new = cbind(newdata_scale,som_predict_grid)
facies_output_new

write.csv(facies_output_new, "facies_output_new.csv")
