library(misc3d)
library(rgl)

##Load "AdamSouthwest coredata": Use file 'AdamSouthwest_Core_for use'
coredata_raw = read.csv(file.choose(), header = TRUE)
attach(coredata_raw)

##Read major coredata 
coredata = as.data.frame(coredata_raw[,c(1:2,4:8,15)])
attach(coredata)

##Scale coredata
coredata_coreonly = as.matrix(coredata[,2:8])
coredata_scale = scale(coredata_coreonly)

##Determine # of clusters
ssplot = function(coredata_scale, nc=15){
  wss = (nrow(coredata_scale)-1)*sum(apply(coredata_scale,2,var)) 
  for (i in 2:nc) {
    wss[i] = sum(kmeans(coredata_scale, centers=i)$withinss)
  }
  
  bss = (nrow(coredata_scale)-1)*sum(apply(coredata_scale,2,var))
  for (i in 2:nc) {
    bss[i] = sum(kmeans(coredata_scale, centers=i)$betweenss)
  }
  
  plot(1:nc, wss, type = "b", xlab = "Number of Clusters", ylab = "SSW") 
  lines(bss)
  points(bss)
  mtext("SSB", side = 4)
  axis(side = 4, labels = TRUE)  
}
ssplot(coredata_scale)

##Run K-Means clustering
km = kmeans(coredata_scale, 4, iter.max = 100, nstart = 10)
km
centroid = km$centers
size = km$size

##Unscale the centroids
library(DMwR)
centroid_original = unscale(centroid, coredata_scale) ##This is to check the core facies properties

##Assign petrofacies to coredata
facies = as.factor(km$cluster)
coredata_facies = cbind(coredata, facies)
write.csv(coredata_facies, "ASW_coredata_facies.csv")

##Assign petrofacies to logdata of ASW using Excel function, VLOOKUP

##Use logdata with core facies: Use file 'ASW_logdata_for use'
logdata = read.csv(file.choose(), header = TRUE) ##ASW logdata where core-based-facies column is included in match with depth
attach(logdata)

##SVM of ASW logdata using core facies 
library(e1071)
y = as.factor(logdata[,7]) ##ASW core-based-facies column

##Find optimum cost for linear kernel
SVM_tune = tune.svm(facies ~ PEF+RHOB+NPHI+GR+AT90, data = logdata, kernel = 'radial', cost = 10^(-1:2), gamma = 10^(-2:1))
SVM_tune
SVM_tune$best.parameters

##Run SVM for ASW logdata
SVM_logdata = svm(y ~ PEF+RHOB+NPHI+GR+AT90, scale = TRUE, cost=10, data = logdata, kernel='radial', gamma = 0.1)
SVM_logdata$fitted
summary(SVM_logdata)

##Assign electrofacies to logdata
facies_SVM = as.factor(SVM_logdata$fitted)
logdata_facies_SVM = cbind(logdata, facies_SVM)
write.csv(logdata_facies_SVM, "ASW_coredata&SVM_facies.csv")

##Showing SV's and 3D hyperplanes
logdata_only = logdata[,-1]
plot3d(logdata_only[,-6], col=facies)

newdat.list = lapply(logdata_only[,-6], function(x) seq(min(x), max(x), len=10))
newdat = expand.grid(newdat.list)
newdat.pred = predict(SVM_logdata, newdata = newdat, decision.values = T)
newdat.dv = attr(newdat.pred, 'decision.values')
newdat.dv = array(newdat.dv, dim = rep(10,3))

contour3d(newdat.dv, level=0, x=newdat.list$PEF, y=newdat.list$RHOB, z=newdat.list$NPHI, add = T)

##SVM prediction of new test data
newlog = read.csv(file.choose(), header = TRUE)
attach(newlog)
SVM_predict = predict(SVM_logdata, newlog)
SVM_predict_facies = as.factor(SVM_predict)
newlog_facies = data.frame(newlog, SVM_predict_facies)
newlog_facies_predict = cbind(newlog, SVM_predict_facies)
write.csv(newlog_facies_predict, "New_SVM predict_facies.csv")



##Log-only classification
logdata_original = read.csv(file.choose(), header = TRUE)
logdata_raw = as.data.frame(logdata_original[,c(2:6)])
attach(logdata_raw)
logdata_raw_scale = scale(logdata_raw)
km2 = kmeans(logdata_raw_scale, 4, iter.max = 100, nstart = 10)
km2
size2 = km2$size

log_facies = as.factor(km2$cluster)
logdata_facies = cbind(logdata_original, log_facies)
write.csv(logdata_facies, "ASW_logdata_facies.csv")

y2 = as.factor(logdata_facies[,7])
SVM_logdata2 = svm(y2 ~ PEF+RHOB+NPHI+GR+AT90, scale = TRUE, cost=10, data = logdata_facies, kernel='radial', gamma = 0.1)
SVM_logdata2$fitted
summary(SVM_logdata2)

SVM_predict2 = predict(SVM_logdata2, newlog)
SVM_predict_facies2 = as.factor(SVM_predict2)
newlog_facies2 = data.frame(newlog, SVM_predict_facies2)
newlog_facies_predict2 = cbind(newlog, SVM_predict_facies2)
write.csv(newlog_facies_predict2, "New_SVM predict_facies2.csv")

