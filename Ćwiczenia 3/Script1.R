dane <- read.table('http://theta.edu.pl/wp-content/uploads/2018/03/dane_kosiarki-1.txt', header = TRUE)

head(dane)
attach(dane)

dane$owner <- as.factor(dane$owner)

library(MASS)

model.1 <- lda(owner~income + lotsize, data = dane)
model.1

LD1 <- predict(model.1)$x



sum(LD1 * (dane$owner == 1)) / sum(dane$owner == 1)

sum(LD1 * (dane$owner == 2)) / sum(dane$owner == 2)

owner.predict <- predict(model.1, dane[,1:2])

owner.predict$posterior[20,]


table(Original = dane$owner, Predicted =predict(model.1)$class)

owner.classes <- owner.predict$class

owner.classperc <- sum(owner.classes==dane$owner)/length(dane$owner)      