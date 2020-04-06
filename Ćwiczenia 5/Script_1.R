library(MASS)

# (iris$Species~iris$Sepal.Length+iris$Petal.Width)

iris$Sepal.Length <- as.factor(as.numeric(cut(iris$Sepal.Length, 3)))

levels(iris$Sepal.Length) <- c('krótki', 'średni', 'długi')

set.seed(123)

smp_size <- floor(0.80 * nrow(iris))

train_ind <- sample(seq_len(nrow(iris)), size = smp_size)

train <- iris[train_ind, ]
test <- iris[-train_ind, ]

table(train$Sepal.Length, train$Species)

srednie.1 <- aggregate(train$Petal.Width, by=list(train$Sepal.Length), FUN = 'mean')
colnames(srednie.1) <- c('Sepal.Length','Mean')

sd.1 <- aggregate(train$Petal.Width, by=list(train$Sepal.Length), FUN = 'sd')
colnames(sd.1) <- c('Sepal.Length','Sd')

X1 = cbind(srednie.1, sd.1$Sd)
colnames(X1) <- c('Sepal.Length', 'Mean', 'Sd')

srednie.2 <- aggregate(train$Petal.Width, by=list(train$Species), FUN = 'mean')
colnames(srednie.2) <- c('Species', 'Mean')

sd.2 <- aggregate(train$Petal.Width, by=list(train$Species), FUN = 'sd')
colnames(sd.2) <- c('Species', 'Sd')

X2 = cbind(srednie.2, sd.2$Sd)
colnames(X2) <- c('Species', 'Mean', 'Sd')



library(nnet)
model.multinom <- multinom(train$Species ~ train$Sepal.Length + train$Petal.Width, data = train)

summary(model.multinom)

data.predict <- train

data.predict$Petal.Width <- mean(data.predict$Petal.Width)

predict(model.multinom, data.predict, type = 'prob')

predict(model.multinom, data.predict)

# CM

cm.multinom <- table(predict(model.multinom), train$Species)
print(cm.multinom)

# Miss
1 - sum(diag(cm.multinom))/sum(cm.multinom)

# LDA
model.lda <- lda(train$Species ~ train$Sepal.Length + train$Petal.Width, data = train)

cm.lda <- table(predict(model.lda)$class, train$Species)
print(cm.lda)

1 - sum(diag(cm.lda))/sum(cm.lda)

good_class_multinom <- c()

good_class_lda <- c()

for (i in seq(10)){
  smp_size <- floor(0.80 * nrow(iris))
  
  train_ind <- sample(seq_len(nrow(iris)), size = smp_size)
  
  train <- iris[train_ind, ]
  test <- iris[-train_ind, ]
  
  library(nnet)
  model.multinom <- multinom(train$Species ~ Sepal.Length + Petal.Width, data = train)
  
  data.predict <- train
  
  data.predict$Petal.Width <- mean(data.predict$Petal.Width)
  
  # CM
  
  cm.multinom <- table(predict(model.multinom), train$Species)
  print(cm.multinom)
  
  # Good
  good_class_multinom <- c(good_class_multinom, sum(diag(cm.multinom))/sum(cm.multinom))
  
  # LDA
  model.lda <- lda(train$Species ~ Sepal.Length + Petal.Width, data = train)
  
  cm.lda <- table(predict(model.lda)$class, train$Species)
  print(cm.lda)
  
  # Good
  good_class_lda <- c(good_class_lda, sum(diag(cm.lda))/sum(cm.lda))
  
}

clasif_score_mulitnom <- mean(good_class_multinom)

clasif_score_lda <- mean(good_class_lda)

# Bootstrap



boot.Length <- c(rep('krótki', 41), rep('średni', 41), rep('długi',41))
boot.Width <- rep(seq(min(iris$Petal.Width, max(iris$Petal.Width)), length.out = 41), 3)

bootstrap.data <- data.frame('Sepal.Length' = boot.Length, 'Petal.Width' = boot.Width)

# multinom

boot.predict.multinom <- predict(model.multinom, newdata = bootstrap.data, type = 'prob')


boot.predict.lda <- predict(model.lda, newdata = bootstrap.data)$posterior

library(tidyverse)

require(gridExtra)
grid.arrange(p1, p2, p3,nrow=3)
p1 <- ggplot(data = bootstrap.data) + geom_point(mapping = aes(bootstrap.data$Petal.Width, boot.predict.multinom[,1], color = Sepal.Length), position = 'jitter') + 
  ggtitle("Plot of prob \n by Species") +
  xlab("Petal Width") + ylab("Probability of Setosa")
p2 <- ggplot(data = bootstrap.data) + geom_point(mapping = aes(bootstrap.data$Petal.Width, boot.predict.multinom[,2], color = Sepal.Length), position = 'jitter')+
  xlab("Petal Width") + ylab("Probability of Versicolor")
p3 <- ggplot(data = bootstrap.data) + geom_point(mapping = aes(bootstrap.data$Petal.Width, boot.predict.multinom[,3], color = Sepal.Length), position = 'jitter')+
  xlab("Petal Width") + ylab("Probability of Virginica")

grid.arrange(p11, p22, p33,nrow=3)
p11 <- ggplot(data = bootstrap.data) + geom_point(mapping = aes(bootstrap.data$Petal.Width, boot.predict.lda[,1], color = Sepal.Length), position = 'jitter')+ 
  ggtitle("Plot of prob \n by Species") +
  xlab("Petal Width") + ylab("Probability of Setosa")
p22 <- ggplot(data = bootstrap.data) + geom_point(mapping = aes(bootstrap.data$Petal.Width, boot.predict.lda[,2], color = Sepal.Length), position = 'jitter')+
  xlab("Petal Width") + ylab("Probability of Versicolor")
p33 <- ggplot(data = bootstrap.data) + geom_point(mapping = aes(bootstrap.data$Petal.Width, boot.predict.lda[,3], color = Sepal.Length), position = 'jitter')+
  xlab("Petal Width") + ylab("Probability of Versicolor")
