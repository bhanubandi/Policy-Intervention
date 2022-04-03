library(neuralnet)
data = read.csv(file = 'F:\\Bhanu\\DS_Project\\standardize\\w_1_9_excel_66.67.csv', header = TRUE)
head(data)
str(data)
data$Country = as.factor(data$Country)
data$KM_Cluster = as.factor(data$KM_Cluster)
str(data)

set.seed(0)
#ind = sample(nrow(data), nrow(data)*0.8)
#train_data = data[ind,]
#test_data = data[-ind,]
#test_data[,2]
#softmax <- function(x) log(1 + exp(x))


n = neuralnet(data$KM_Cluster ~TP1+CT1+SI1+TP2+CT2+SI2+TP3+CT3+SI3+TP4+CT4+SI4
              +TP5+CT5+SI5+TP6+CT6+SI6+TP7+CT7+SI7+TP8+CT8+SI8+TP9+CT9+SI9,data=data, 
              algorithm = 'rprop+', act.fct = 'logistic', err.fct = 'ce', threshold = 0.001, hidden = c(10, 7), linear.output = FALSE)
#plot(n)
df = n$net.result

#write.csv(df,"C:\\Users\\admin\\Desktop\\prob_1_1_66.67.csv", row.names = FALSE)

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

#write.csv(metrics, file = 'F:\\Bhanu\\DS_Project\\standardize\\RF_metrics\\w_1_9_neural_network_66.67.csv', row.names = TRUE)

