## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
library(caret)
library(mlbench)
library(statPack)

data(BostonHousing)


## ------------------------------------------------------------------------
data("BostonHousing") #load a data
boston_data <- BostonHousing #set a data to variable
indexes = createDataPartition(boston_data$medv, p = .75, list = FALSE, times = 1)
training<- boston_data[indexes,] #assigninng 75% data to test
testing<- boston_data[-indexes,]  #assigning remaining 25% data to training set


## ------------------------------------------------------------------------
set.seed(-312312L)
ridgereg_fit <- train(rm ~ . , data = training, method = "lm")
print(ridgereg_fit)

ridgereg_forward_fit <- train(rm ~ ., data = training, method = "leapForward")
print(ridgereg_forward_fit)

## ------------------------------------------------------------------------
ridge <- list(type="Regression", 
              library="statPack",
              loop=NULL,
              prob=NULL)
ridge$parameters <- data.frame(parameter="lambda",
                               class="numeric",
                               label="lambda")
ridge$grid <- function(y,x, len=NULL, search="grid"){
  data.frame(lambda=c(0.1,0.5,1,2))
}
ridge$fit <- function (x, y, wts, param, lev, last, classProbs, ...) {
  dat <- if (is.data.frame(x)) 
    x
  else as.data.frame(x)
  dat$.outcome <- y
  out <- ridgereg$new(.outcome ~ ., data = dat, lambda=param$lambda, ...)
  out
}
ridge$predict <- function (modelFit, newdata, submodels = NULL) {
  if (!is.data.frame(newdata)) 
    newdata <- as.data.frame(newdata)
  newdata[,apply(newdata, MARGIN=2, sd)!=0] <- scale(newdata[,apply(newdata, MARGIN=2, sd)!=0])
  modelFit$predict(newdata)
}

## ------------------------------------------------------------------------
#result will store in train function
result <- train( medv ~ ., data=training, method=ridge)

## ------------------------------------------------------------------------
fitControl <- control <- trainControl(method = "repeatedcv",
                        number=10,
                        repeats = 10)

 result <- train(crim ~ ., data = training,method = ridge,preProc = c("scale","center"),
       tuneLength = 10,trControl = fitControl)

