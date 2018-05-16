install.packages("rattle.data")
install.packages("rnn")
library("rattle.data")
library("rnn")

data("weatherAUS")
View(weatherAUS)

#for location Albury -  humidity and raining forecast  
data1 <- weatherAUS[1:3040 , c(1,14)]
summary(data1)
data <- weatherAUS[1:3040]
data_clean <- na.omit(data1)
summary(data_clean)
data_used <- data_clean[1:3000,]
x = data_used[,1]
y=data_used[,2]

a = matrix(x, nrow=30)
b = matrix(y, nrow=30)

normalise = function(x)
{
  x  = (x-min(x))/(max(x) - min(x))
}
normalise(b)

B_scaled = ((b-min(b))/(max(b) - min(b)))
B = t(B_scaled)
train = 1:70
test = 71:100

model <- trainr(Y=B[train,],
                X=B[train,],
                learningrate = 0.05,
                hidden_dim = 16,
                numepochs = 1000)
plot(colMeans(model$error),type='l',xlab='epoch',ylab='errors')

predictions <- predictr(model , B[test,])
predictions

#graphical representation of the  comparison between actual and predicted value of humidiity
plot(as.vector(t(B[test,])) , col = 'red' , type = 'l' , main = ' first RNN')
lines(as.vector(t(predictions)) , type = 'l' , col = 'black')
