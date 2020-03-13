#ADM Paper 1

set.seed(222)
par(mfrow=c(1, 1))
library(polynom)


f <- function(x) sin(2*x*pi)

N <- 10
x <- runif(N)
t <- f(x) + rnorm(N, sd=0.2)  

polyfit <- lm(t ~ poly(x, 1, raw=TRUE))
p <- polynomial(coef(polyfit))

plot(data.frame(x, t),ylim=c(-1.25,1.25))
curve(f, type="l", col="green", add=TRUE)
fp <- as.function(p)
curve(fp, col="red", add=TRUE)

#'where in black we see our observed data, in green the distribution the 
#'data is following and in red the prediction we make with a polynomial
#'of degree one

par(mfrow=c(2, 3))

for (i in c(1, 2, 3, 4, 6, 9)) 
{
  plot(data.frame(x, t), xlab="x", main=paste("Polynomial fit of degree", i), ylab="f(x)",ylim=c(-1.25,1.25))
  curve(f, type="l", col="green", add=TRUE)
  polyfit <- lm(t ~ poly(x, i, raw=TRUE))
  p <- as.function(polynomial(coef(polyfit)))
  curve(p, col="red", add=TRUE)
}


par(mfrow=c(2, 3))

for (i in 1:6)
{
  x <- runif(N)                 
  t <- f(x) + rnorm(N, sd=0.2)
  
  plot(data.frame(x, t), xlab="x", main=paste("Polynomial of degree 1 \nData sample", i),
       ylab="f(x)",xlim=c(0,1),ylim=c(-1.25,1.25))
  curve(f, type="l", col="green", add=TRUE)
  polyfit <- lm(t ~ poly(x, 1, raw=TRUE))
  p <- as.function(polynomial(coef(polyfit)))
  curve(p, col="red", add=TRUE)
}

#'
#'

par(mfrow=c(2, 3))
set.seed(446)
for (i in 1:6)
{
  x <- runif(N)                 
  t <- f(x) + rnorm(N, sd=0.2)
  
  plot(data.frame(x, t), xlab="x", main=paste("Polynomial of degree 9 \nData sample", i), ylab="f(x)",xlim=c(0,1),ylim=c(-1.25,1.25))
  curve(f, type="l", col="green", add=TRUE)
  polyfit <- lm(t ~ poly(x, 8, raw=TRUE))
  test <- coef(polyfit)
  test[is.na(test)]<-0
  p <- as.function(polynomial(test))
  curve(p, col="red", add=TRUE)
}


bias_variance <- function(f){
vector <- c(seq(1,5), seq(6,20,2), seq(22,48,4), 49)
results <- matrix (nrow=length(vector), ncol=4)
results[,1]<-vector
colnames(results) <- c("Degree","Bias","Variance", "Total Error")
for (i in vector)
{
  a <- c()
  for (j in 1:1000)
  {
    x <- runif(50)              
    t <- f(x) + rnorm(50, sd=0.4) 
    polyfit <- lm(t ~ poly(x, i, raw=TRUE))
    test <- coef(polyfit)
    test[is.na(test)]<-0
    p <- as.function(polynomial(test))
    a <- c(a, p(0.3))
  }
  print(paste("x(0.3) for degree", i, "is", mean(a)))
  bias <- (f(0.3)-mean(a))
  variance <- var(a) #mean((a-mean(a))^2)
  print(paste("Bias for degree", i, "is", bias))
  print(paste("Variance for degree", i, "is", variance))
  results[results[,1]==i, "Bias"] <- bias
  results[results[,1]==i, "Variance"] <- variance
  results[results[,1]==i, "Total Error"] <- bias^2 + variance
}
return(results)
}

set.seed(123)
f <- function(x) sin(2*x*pi)
first <- bias_variance(f)
par(mfrow=c(1, 1))
plot(first[,1], first[,2], col="blue", type="l", xlab="Polynomial Degree", ylab="Error")
points(first[,1], first[,3], col="green", type="l")
abline(v=first[which.min(first[,4]),"Degree"], lty=2, col="red")
legend("topright", legend=c("Bias", "Variance"),
       col=c("blue", "green"), lty=1, cex=0.8)


first[which.min(first[,4]),"Degree"]
#minimum error in this case is degree 5. 

# library(rpart)
# library(rpart.plot)

titanic <- read.csv('https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv')

preprocess <- function(titanic,i){
  shuffle_index <- sample(1:nrow(titanic))
  titanic <- titanic[shuffle_index, ]

  titanic <- titanic[,-c(1, 4,9,11,13)]
  titanic$pclass<- as.factor(titanic$pclass)
  titanic$survived<- as.factor(titanic$survived)
  levels(titanic$survived)<-c("yes","no")
  titanic <- na.omit(titanic)
  titanic_clean <- titanic[titanic$age!="?",]
  titanic_clean$age <- as.numeric(titanic_clean$age)
  summary(titanic_clean)
  
  cut <- round(nrow(titanic_clean)*0.75)
  training <<- titanic_clean[1:cut,]
  testing <<- titanic_clean[(cut+1):nrow(titanic_clean),]
  
  l <- list(training, testing)
  return(l)
}

#Decision Trees: abandoned idea
# set.seed(333)
# par(mfrow=c(1, 3))
# 
# l <- preprocess(titanic)
# training <- l[[1]]
# testing <- l[[2]]
# first <- rpart(survived ~ sex + age + sibsp + pclass, data = training, method = "class")
# prediction_1 <- predict(first, testing, type = "class")
# training_1 <- predict(first, training, type = "class")
# rpart.plot(first, extra = 106)
# (conf.matrix <- table(training$survived, training_1))
# (CR <- sum(diag(conf.matrix))/sum(conf.matrix))
# (conf.matrix <- table(testing$survived, prediction_1)) 
# (CR <- sum(diag(conf.matrix))/sum(conf.matrix))
# 
# l <- preprocess(titanic)
# training <- l[[1]]
# testing <- l[[2]]
# second <- rpart(survived ~ sex + age + sibsp + pclass, data = training, method = "class")
# rpart.plot(second, extra = 106)
# prediction_2 <- predict(second, testing, type = "class")
# 
# l <- preprocess(titanic)
# training <- l[[1]]
# testing <- l[[2]]
# third <- rpart(survived ~ sex + age + sibsp + pclass, data = training, method = "class")
# rpart.plot(third, extra = 106)
# prediction_3 <- predict(third, testing, type = "class")
# 
# #esto es el plot para decir que cada uno es diferente, que son classifiers con lowbias, que high 
# #variance, que noise.
# 
# set.seed(333)
# vector <- matrix(nrow=50, ncol=3)
# colnames(vector) <- c("Deapth", "Training Error", "Testing Error")
# 
# for (i in 1:50){
#   first <- decision_trees(titanic, i)
#   prediction_1 <- predict(first, testing, type = "class")
#   training_1 <- predict(first, training, type = "class")
#   conf.matrix <- table(training$survived, training_1)
#   Etrain <- sum(diag(conf.matrix))/sum(conf.matrix)
#   conf.matrix <- table(testing$survived, prediction_1)
#   Etest <- sum(diag(conf.matrix))/sum(conf.matrix)
#   vector[i,] <- c(i,Etrain,Etest)
#   
# }
# 
# par(mfrow=c(1, 1))
# plot(vector[,1], vector[,3], type="l", col="green", ylim = c(0,1))
# points(vector[,1], vector[,2], type="l")
# #There are ensemble algorithms, such as bootstrapping aggregation and random forest, 
# #which aim to reduce variance at the small cost of bias in decision tree. Pruning too
# 
# #More Variance = error from the model being more complex (fits the data too well, 
#and learns the noise in addition to the inherent patterns in the data), 
#because models are not really simple

#puedo plotear % of error from training data and testing data, and say overfitting. 
#hacer diferentes particiones de la data into training and testing y decir que todas predicen 
#igual de bien, pero que la variance es alta, porque todos los trees son diferentes

#Underfitting: when model is too simple, both training and test errors are large. Overfitting, 
#training low but testing high.



#ANN

library("nnet");

set.seed(333)
l <- preprocess(titanic)
training <- l[[1]]
testing <- l[[2]]
newtesting <- testing
newtraining <- training

newtraining <- newtraining[,-c(6,7,8)]

newtesting <- newtesting[,-c(6,7,8)]

model <- nnet(survived ~ sex + age + sibsp + pclass, newtraining, size = 5)



set.seed(444) 
vector <- matrix(nrow=50, ncol=3)
colnames(vector) <- c("units", "Training Error", "Testing Error")

for (i in 1:30){
  a <- c()
  e <- c()
  for (j in 1:10){
    model <- nnet(survived ~ sex + age + sibsp + pclass, newtraining, size = i)
    test_pred <- predict(model, newtesting, type = "class")
    train_pred <- predict(model, newtraining, type = "class")
    conf.matrix <- table(newtraining$survived, train_pred)
    Etrain <- sum(diag(conf.matrix))/sum(conf.matrix)
    conf.matrix <- table(newtesting$survived, test_pred)
    Etest <- sum(diag(conf.matrix))/sum(conf.matrix)
    a <- c(a, Etrain)
    e <- c(e, Etest)
  }
  a <- mean(a)
  e <- mean(e)
  vector[i,] <- c(i,a,e)
}

par(mfrow=c(1, 1))
plot(vector[,1], vector[,2], type="l", col="blue", ylab = "Prediction Error", xlab="Number of units in the hidden layer")
points(vector[,1], vector[,3], type="l", col="green")
legend("topright", legend=c("Training Error", "Test Error"),
       col=c("blue", "green"), lty=1, cex=0.8)

