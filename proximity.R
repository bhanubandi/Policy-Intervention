library(ggplot2)
library(cowplot)
library(randomForest)
data = read.csv(file = 'F:\\Bhanu\\DS_Project\\standardize\\w_1_9_excel_66.67.csv', header = TRUE)
head(data)
str(data)
#dim(data)
#df = data[,-c(1)]
data$Country = as.factor(data$Country)
data$KM_Cluster = as.factor(data$KM_Cluster)
str(data)
set.seed(0)
model <- randomForest(KM_Cluster ~ ., data=data, proximity=TRUE, random_state = 0)
model
#dim(distance.matrix)
set.seed(0)
distance.matrix <- as.dist(1-model$proximity)
#distance.matrix1 = dist(scale(t(df), center = TRUE, scale = TRUE), method = "euclidean")

mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)
#dim(mds.var.per)
#mds.var.per[1:10]
## now make a fancy looking plot that shows the MDS axes and the variation:
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status= data$KM_Cluster)
ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) +
   
  ##geom_density_2d(aes(color=Status)) +
  geom_point(aes(color=Status)) +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities)")
summary(model)


con = model$confusion
con1 = con[, -4]
accuracy = sum(diag(con1))/sum(con1)
diag = diag(con1)
nrow(con1)
sum(con1)
rowsums = apply(con1, 1, sum)
colsums = apply(con1, 2, sum)


precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall)


metrics = data.frame(precision, recall, f1) 

#write.csv(metrics, file = 'F:\\Bhanu\\DS_Project\\standardize\\RF_metrics\\w_1_9_randon_forest_66.67.csv', row.names = TRUE)
