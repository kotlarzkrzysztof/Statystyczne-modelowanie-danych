dane <- read.table('http://theta.edu.pl/wp-content/uploads/2018/03/dane_kosiarki-1.txt', header = TRUE)

head(dane)
attach(dane)

dane$owner <- as.factor(dane$owner)

library(MASS)

model.qda <- qda(owner~income + lotsize, data = dane)
model.qda

qda.predict <- predict(model.qda, dane[,1:2])

qda.predict$posterior[20,]

table(Original = dane$owner, Predicted = qda.predict$class)

qda.classes <- qda.predict$class

sum(qda.classes==dane$owner)/length(dane$owner)      




model.qda.cv <- qda(owner~income + lotsize, data = dane, CV=TRUE)


table(Original = dane$owner, Predicted = model.qda.cv$class)

sum(model.qda.cv$class==dane$owner)/length(dane$owner)

predict(model.qda, new=data.frame(income=100,lotsize=22.5))



mypredict.lda <- function(object, newdata ){
  predict(object, newdata = newdata )$class
}

require(ipred)
attach(dane)
errorest(owner ~ income + lotsize, data = dane, model = lda, estimator = "cv", predict= mypredict.lda)

errorest(owner ~ income + lotsize, data = dane, model = qda, estimator = "cv", predict= mypredict.lda)


qda.predict.res <- predict(model.qda, dane[,1:2], type = 'class')


par(mfrow=c(2,2))

roc.qda.performance <- prediction(qda.predict.res$posterior[,1], dane$owner)
plot(performance(roc.qda.performance, 'tpr', 'fpr'), col='red', ylab='Czułość', xlab='1 - Specificzność')

roc.qda.performance <- prediction(qda.predict.res$posterior[,2], dane$owner)
plot(performance(roc.qda.performance, 'tpr', 'fpr'), col='red', ylab='Czułość', xlab='1 - Specificzność')


model.lda <- lda(owner~income + lotsize, data = dane)

lda.predict.res <- predict(model.lda, dane[,1:2])
roc.lda.performance <- prediction(lda.predict.res$posterior[,1], dane$owner)
plot(performance(roc.lda.performance, 'tpr', 'fpr'), col='red', ylab='Czułość', xlab='1 - Specificzność')

roc.lda.performance <- prediction(lda.predict.res$posterior[,2], dane$owner)
plot(performance(roc.lda.performance, 'tpr', 'fpr'), col='red', ylab='Czułość', xlab='1 - Specificzność')


library(MASS)

str(Pima.te)

model.lda.pima <- lda(type~., data = Pima.te)

model.qda.pima <- qda(type~., data = Pima.te)

lda.prima.predict <- predict(model.lda.pima, newadata=Pima.te)
qda.prima.predict <- predict(model.qda.pima, newdata = Pima.te)

1 - sum(lda.prima.predict$class==Pima.te$type)/length(Pima.te$type)
1 - sum(qda.prima.predict$class==Pima.te$type)/length(Pima.te$type)


model.lda.pima.cv <- lda(type~., data = Pima.te, CV=TRUE)
mean(model.lda.pima.cv$class != Pima.te$type)


model.qda.pima.cv <- qda(type~., data = Pima.te, CV=TRUE)
mean(model.qda.pima.cv$class != Pima.te$type)


errorest(type~., data = Pima.te, model = lda, estimator = 'boot', predict = mypredict.lda)

errorest(type~., data = Pima.te, model = qda, estimator = 'boot', predict = mypredict.lda)


par(mfrow=c(2,2))

roc.lda.performance.pima <- prediction(lda.prima.predict$posterior[,1], Pima.te$type)
plot(performance(roc.lda.performance.pima, 'tpr', 'fpr'), col='red', ylab='Czułość', xlab='1 - Specificzność')

roc.lda.performance.pima <- prediction(lda.prima.predict$posterior[,2], Pima.te$type)
plot(performance(roc.lda.performance.pima, 'tpr', 'fpr'), col='red', ylab='Czułość', xlab='1 - Specificzność')

roc.qda.performance.pima <- prediction(qda.prima.predict$posterior[,1], Pima.te$type)
plot(performance(roc.qda.performance.pima, 'tpr', 'fpr'), col='red', ylab='Czułość', xlab='1 - Specificzność')

roc.qda.performance.pima <- prediction(qda.prima.predict$posterior[,2], Pima.te$type)
plot(performance(roc.qda.performance.pima, 'tpr', 'fpr'), col='red', ylab='Czułość', xlab='1 - Specificzność')

