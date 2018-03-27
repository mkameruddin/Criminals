train <- read.csv("G:/DATA/criminals/criminal_train.csv")
test <- read.csv("G:/DATA/criminals/criminal_test.csv")

str(train)

# using randomForest to find variable importance
library(randomForest)
mod <- randomForest(train[,-c(1,72)], as.factor(train[,72]), importance = T)
varImpPlot(mod)
im <- mod$importance
im1 <- sort(im[,4], decreasing = T) # sorting by gini index
n <- names(im1)[1:12]

trainx <- train[n]
trainy <- as.factor(train[,72])

library("caret")
split<-createDataPartition(y = trainy, p = 0.7, list = FALSE)

dev<-trainx[split,]
val<-trainx[-split,]

mod1 <- randomForest(dev, trainy[split], importance = T)
mod1
pre <- predict(mod1, val)
pre
table(pre, trainy[-split])
library(e1071)
confusionMatrix(pre, trainy[-split])

pre_test <- predict(mod1, test)
sub <- cbind("PERID"= test$PERID,"Criminal" = pre_test)

write.csv(sub,"G:/DATA/criminals/sub1.csv",row.names=F)
