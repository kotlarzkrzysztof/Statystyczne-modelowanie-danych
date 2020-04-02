dane <- read.csv('http://theta.edu.pl/wp-content/uploads/2018/03/urine.csv', header = T, dec= '.', sep = ',', col.names = c('id','r','gravity','ph','osmo','cond','urea','calc'))

dane <- dane[,2:8]

dane$r <- as.factor(dane$r)

dane <- na.omit(dane)

set.seed(123)

train_ind <- sample(seq_len(nrow(dane)), size = 50)

train <- dane[train_ind, ]
test <- dane[-train_ind, ]

model.glm <- glm(r ~ . , data = train, family = binomial)

summary(model.glm)

model.glm.1 <- glm(r~. -gravity, data = train, family = binomial)
summary(model.glm.1)

model.glm.2 <- glm(r~. -gravity -ph, data = train, family = binomial)
summary(model.glm.2)

predictGLM <- predict(model.glm.2, newdata = train)
plot(predictGLM, pch = as.numeric(dane$r))

predictLR <- predict(model.glm.2, newdata = test)

TAB <- table(test[,1], predictLR > 0)
TAB

mcrlr <- 1-sum(diag(TAB))/sum(TAB)
mcrlr

library(MASS)
model.lda <- lda(r~. -gravity -ph, data = train)


lda.predict <- predict(model.lda, test[ ,2:7])
pid.classperc <- sum(lda.predict$class == test[ ,1]) / nrow(test)
pid.classperc

1 - pid.classperc




