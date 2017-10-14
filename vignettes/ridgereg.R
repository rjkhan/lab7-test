## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
# warnings=FALSE becauce the warning were only about when the loaded packages were built
library(lab7)
library(caret)
library(mlbench)
data(BostonHousing)

## ------------------------------------------------------------------------
set.seed(12345)
train_index <- createDataPartition(y = BostonHousing$medv, p = 0.75, list = FALSE)
training <- BostonHousing[train_index,]
testing <- BostonHousing[-train_index,]

## ---- comment=NA---------------------------------------------------------
set.seed(12345)

(lm1 <- caret::train(medv ~ ., data=training, method = 'lm'))

## ---- comment=NA---------------------------------------------------------
set.seed(12345)

(lm2 <- caret::train(medv ~ ., data=training, method = 'leapForward'))

## ---- comment=NA---------------------------------------------------------
summary(lm2)$which

## ------------------------------------------------------------------------
ridge <- list(type="Regression", 
              library="lab7",
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
control <- trainControl(method = "repeatedcv",
                        number=10,
                        repeats = 10)

## ---- comment=NA---------------------------------------------------------
set.seed(12345)
# (lm.ridge <- train(medv ~ ., data=training, method=ridge, trControl=control))

## ------------------------------------------------------------------------
# Linear regression
set.seed(12345)
test_lm1 <- predict(lm1, testing)
postResample(pred=test_lm1, obs=testing$medv )

# Linear regression leap forward
set.seed(12345)
test_lm2 <- predict(lm2, testing)
postResample(pred=test_lm2, obs=testing$medv )

# Ridge regression
set.seed(12345)
# test_ridge <- predict(lm.ridge, testing)
# postResample(pred=test_ridge, obs=testing$medv )

