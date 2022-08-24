install.packages('ISLR')
library(ISLR)

head(Caravan)
str(Caravan)
summary(Caravan$Purchase)
any(is.na(Caravan))
var(Caravan[, 1])
var(Caravan[, 2])

purchase <- Caravan[, 86]
standard.Caravan <- scale(Caravan[, -86])

var(standard.Caravan[, 1])
var(standard.Caravan[, 2])

# train test split
test.index <- 1:1000
test.data <- standard.Caravan[test.index, ]
test.purchase <- purchase[test.index] 

train.data <- standard.Caravan[-test.index, ]
train.purchase <- purchase[-test.index]

### build the model 
install.packages('class')
library(class)
set.seed(101)

predicted.purchase <- knn(train.data, test.data, train.purchase, k=6)
head(predicted.purchase)
misclasserror1 <- mean(test.purchase != predicted.purchase) 
print(misclasserror1)


### choosing the K value
predicted.purchase <- NULL
error.rate <- NULL

for (i in 1:20){
  set.seed(101)
  predicted.purchase <- knn(train.data, test.data, train.purchase, k=i)
  error.rate[i] <- mean(predicted.purchase != test.purchase)
}

print(error.rate)
k.value = 1:20

error.rate.df <- data.frame(k.value, error.rate)
error.rate.df

ggplot(error.rate.df, aes(k.value, error.rate)) + geom_point() + geom_line(color='red', alpha=0.5)

#### rebuild the model
predicted.purchase.final <- knn(train.data, test.data, train.purchase, k=9)
misclasserror.final <- mean(test.purchase != predicted.purchase) 
print(misclasserror.final)
