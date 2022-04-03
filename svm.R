library('e1071')
set.seed(0)
data = read.csv(file = 'F:\\Bhanu\\DS_Project\\standardize\\w_1_9_excel_66.67.csv', header = TRUE)
head(data)
str(data)
data$Country = as.factor(data$Country)
data$KM_Cluster = as.factor(data$KM_Cluster)
str(data)
#ind = sample(nrow(data), nrow(data)*0.8)
#train = data[ind,]
#test = data[-ind,]
svmfit = svm(formula = data$KM_Cluster~., 
             data = data, scale = FALSE, kernal = 'radial', cost = 1)

#plot(svmfit, data)
summary(svmfit)




tuning = tune(svm, KM_Cluster~., data = data, 
            ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)),
            tunecontrol = tune.control(sampling = "boot"))
plot(obj)
obj$best.model
obj$best.parameters
summary(obj)





best_svm = svm(formula = data$KM_Cluster~., 
             data = data, scale = FALSE, kernal = 'radial', cost = 4, gamma = 0.5)
pred = predict(best_svm, data)
con = table(data[, 2], pred)
#write.csv(conf, file = "F:\\Bhanu\\DS_Project\\standardize\\confusion_svm.csv", row.names = F)

diag = diag(con)
nrow(con)
sum(con)
rowsums = apply(con, 1, sum)
colsums = apply(con, 2, sum)
p = rowsums/sum(con)
q = colsums/sum(con)



accuracy = sum(diag(con))/sum(con)
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall)


metrics = data.frame(precision, recall, f1) 
#write.csv(metrics, file = 'F:\\Bhanu\\DS_Project\\standardize\\w_1_9_svm_66.67.csv', row.names = TRUE)


