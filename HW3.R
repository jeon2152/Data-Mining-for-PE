install.packages("e1071")
library(e1071)

## Load “ASW.dat”
data = read.table(file.choose(), header = TRUE)

## Run K-means clustering 
m = kmeans(data, 3)

## The result of clustering is saved as factor “y”
y = as.factor(m$cluster)

## Run SVM (cost = .1)
data_svm = svm(y ~ Corrected.Por + TOC, scale=F, cost=0.1, data = data, kernel='linear')

## The (+) are support vectors
plot(data$Corrected.Por, data$TOC, col = as.integer(y), pch = c("o","+")[1:150 %in% data_svm$index + 1], xlab = "Corrected.Por", ylab = "TOC")

## Showing a 2D slice
plot(data_svm, data, Corrected.Por ~ TOC, slice = list(Corrected.Por = 1, TOC = 2))

## Check the Result
data_pred = predict(data_svm, data) 
table(pred = data_pred, true = y)

## Run SVM (cost = 10)
data_svm2 = svm(y ~ Corrected.Por + TOC, scale=F, cost=10, data = data, kernel='linear')
data_pred2 = predict(data_svm2, data) 
table(pred = data_pred2, true = y)

## Run SVM using the polynomial Kernel (0.1x1·x2 + 1)5 with cost of 0.1
data_svm3 = svm(y ~ Corrected.Por + TOC, scale=F, cost=0.1, data = data, kernel='polynomial', degree=5, gamma=0.1, coef0=1)
plot(data_svm3, data, Corrected.Por ~ TOC, slice = list(Corrected.Por = 1, TOC = 2))

data_pred3 = predict(data_svm3, data) 
table(pred = data_pred3, true = y)

## Run SVM using the polynomial Kernel (0.1x1·x2 + 1)5 with cost of 10
data_svm4 = svm(y ~ Corrected.Por + TOC, scale=F, cost=10, data = data, kernel='polynomial', degree=5, gamma=0.1, coef0=1)
plot(data_svm4, data, Corrected.Por ~ TOC, slice = list(Corrected.Por = 1, TOC = 2))

data_pred4 = predict(data_svm4, data) 
table(pred = data_pred4, true = y)

## Run SVM using the radial Kernel exp(-0.1*|x1-x2|^2 with cost of 0.1
data_svm5 = svm(y ~ Corrected.Por + TOC, scale=F, cost=0.1, data = data, kernel='radial', gamma=0.1)
plot(data_svm5, data, Corrected.Por ~ TOC, slice = list(Corrected.Por = 1, TOC = 2))

data_pred5 = predict(data_svm5, data)
table(pred = data_pred5, true = y)

## Run SVM using the radial Kernel exp(-0.1*|x1-x2|^2 with cost of 10
data_svm6 = svm(y ~ Corrected.Por + TOC, scale=F, cost=10, data = data, kernel='radial', gamma=0.1)
plot(data_svm6, data, Corrected.Por ~ TOC, slice = list(Corrected.Por = 1, TOC = 2))

data_pred6 = predict(data_svm6, data)
table(pred = data_pred6, true = y)
