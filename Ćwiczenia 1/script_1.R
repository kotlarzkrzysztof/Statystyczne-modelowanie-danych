library(tidyverse)

data <- read.table('http://theta.edu.pl/wp-content/uploads/2018/02/drukarki.txt', header = T)

data <- rename(data, time = X6, count = X13)

attach(data)

plot(time, count, xlab='Czas', ylab='Liczba drukarek')

hist(time, xlab = 'Czas')

model.1 <- lm(count~time)

summary(model.1)

# count = 2.98 * time + 0,86

hist(model.1$residuals)

plot(time, count, xlab='Czas', ylab='Liczba drukarek')
abline(model.1, col ='blue')

#Wstawia do modelu wartości X pochodzące z modelu
fitted(model.1)

plot(fitted(model.1), model.1$residuals)

#Wykres QQ: W przypadku normalności reszt, wykres powinien mieć przebieg na skos liniowy, reszty nie posiadaja rozkłądu normalnego
qqnorm(model.1$residuals)

new <- data.frame(time=1.5:8.5)
pc <- predict(model.1 , int ="c", newdata = new)
pp <- predict(model.1 , int ="p", newdata = new)

plot(time, count, xlab='Czas', ylab='Liczba drukarek')
matlines(new$time, pc, lty =c(1 ,2 ,2), col = "red")
matlines(new$time, pp, lty =c(1 ,3 ,3), col = "blue")

predict(model.1 , int = "c")[3,]
# lwr: 8.408596

cor(data)

cor.test(count,time)
