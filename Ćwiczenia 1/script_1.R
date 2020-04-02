library(tidyverse)

data <- read.table('http://theta.edu.pl/wp-content/uploads/2018/02/drukarki.txt')

data <- rename(data, time = V1, count = V2)

attach(data)

plot(time, count, xlab='Czas', ylab='Liczba drukarek')

hist(time, xlab = 'Czas')

model.1 <- lm(count~time)

summary(model.1)

# count = 2.95 * time + 0,81

hist(model.1$residuals)

plot(time, count, xlab='Czas', ylab='Liczba drukarek')
abline(model.1, col ='blue')

#Wstawia do modelu wartości X pochodzące z modelu
fitted(model.1)

plot(fitted(model.1), model.1$residuals)

#Wykres QQ: W przypadku normalności reszt, wykres powinien mieć przebieg na skos liniowy, reszty nie posiadaja rozkłądu normalnego
qqnorm(model.1$residuals)

new <- data.frame(time=1.5:8.5)
pc <- predict(model.1 , int ="c", newdata = data[2:20,])
pp <- predict(model.1 , int ="p", newdata = data[2:20,])

plot(time, count, xlab='Czas', ylab='Liczba drukarek')
matlines(data$time[2:20], pc, lty =c(1 ,2 ,2), col = "red")
matlines(data$time[2:20], pp, lty =c(1 ,3 ,3), col = "blue")

predict(model.1 , int = "c")[3,]
# lwr: 22.666

cor(data)
#Cor: 0.932

cor.test(count,time)
#p-val < 5%: Korelacja istotnie różna od zera
