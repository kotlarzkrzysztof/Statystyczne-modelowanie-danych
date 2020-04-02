library(tidyverse)

dane3 <- read.table('http://theta.edu.pl/wp-content/uploads/2018/03/puls2.csv', header = TRUE, sep = ';')

attach(dane3)

dane3$Palacz <- as.factor(dane3$Palacz)

set.seed(123)

smp_size <- floor(0.75 * nrow(dane3))

train_ind <- sample(seq_len(nrow(dane3)), size = smp_size)

train <- dane3[train_ind, ]
test <- dane3[-train_ind, ]

model.4 <- glm(TetnoSpocz~Palacz+Waga, data = train, family = binomial)
summary(model.4)

model.5 <- glm(TetnoSpocz~Waga, data = train, family = binomial)
summary(model.5)

model.m1 <- glm(TetnoSpocz~Palacz*Waga, data = train, family = binomial)
summary(model.m1)

model.m2 <- glm(TetnoSpocz~Waga + Palacz:Waga, data = train, family = binomial)
summary(model.m2)

model.m3 <- glm(TetnoSpocz~ Palacz + Palacz:Waga, data = train, family = binomial)
summary(model.m3)

model.m4 <- glm(TetnoSpocz~Palacz:Waga, data = train, family = binomial)
summary(model.m4)

#####
model.4 <- glm(TetnoSpocz~Palacz+Waga, data = train, family = binomial)
summary(model.4)


predictGLM <- predict(model.4, newdata = train)
plot(predictGLM, pch = as.numeric(dane3$TetnoSpocz))

predictLR <- predict(model.4, newdata = test)

TAB <- table(test[,1], predictLR > 0)
TAB

mcrlr <- 1-sum(diag(TAB))/sum(TAB)
mcrlr

library(MASS)
model.lda <- lda(TetnoSpocz~Palacz + Waga, data = train)
model.lda

lda.predict <- predict(model.lda, test[ ,2:3])
pid.classperc <- sum(lda.predict$class == test[ ,1]) / nrow(test)
pid.classperc

1 - pid.classperc
