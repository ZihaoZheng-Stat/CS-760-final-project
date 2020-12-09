load("E:/Fall 2020/CS 760/Final project/data.RData")

## year and month should be categorical variable

dat$Year = as.factor(dat$Year)

dat$Month = as.factor(dat$Month)

## for cross validation, I used 50% percent observations as training set and the rest as test set

set.seed(1)

ok = sample(dim(dat)[1], round(0.75 * dim(dat)[1]))

d = c(1, 9, 14:16, 21:22)

dat.train = dat[ok,d]

dat.test = dat[!c(1:13244) %in% ok, d]

nn = c("snowstorm","insolation","sleet","hail","fog","pressure","seapressure")

colnames(dat.train) <- colnames(dat.test) <- nn

dat.train$snowstorm = as.factor(dat.train$snowstorm)

dat.test$snowstorm = as.factor(dat.test$snowstorm)

## I will think of the following several models as candidates and do model comparison

## library(rminer) ## an R package for machine learning models

## library(pROC) ## an interesting R package for ROC analysis

## first, logistic regression, the simplest attempt

fit.logit = glm(snowstorm~., data = dat.train, family = binomial)

predict.logit = predict(fit.logit, dat.test[,-1], type = "response")

roc.logit = roc(dat.test$snowstorm, predict.logit)

mean(as.numeric(predict.logit>0.5) == dat.test$snowstorm)

## second, support vector machine

fit.svm = fit(snowstorm~., data = dat.train, model = "svm")

predict.svm = predict(fit.svm, dat.test[,-1], type = "response")[,2]

roc.svm = roc(dat.test$snowstorm, predict.svm)

mean(as.numeric(predict.svm>0.5) == dat.test$snowstorm)

## third, random forest

fit.forest = fit(snowstorm~., data = dat.train, model = "randomforest")

predict.forest = predict(fit.forest, dat.test[,-1], type = "response")[,2]

roc.forest = roc(dat.test$snowstorm, predict.forest)

mean(as.numeric(predict.forest>0.5) == dat.test$snowstorm)

## Naive Bayes
fit.NB = fit(snowstorm~., data = dat.train, model = "naiveBayes")

predict.NB = predict(fit.NB, dat.test[,-1], type = "response")[,2]

roc.NB = roc(dat.test$snowstorm, predict.NB)

mean(as.numeric(predict.NB>0.5) == dat.test$snowstorm)

save(fit.logit,fit.svm,fit.forest,fit.NB,
     predict.logit, predict.svm, predict.forest, predict.NB,
     roc.logit, roc.svm, roc.forest, roc.NB,
     file = "E:/Fall 2020/CS 760/Final project/ROCresult.RData")