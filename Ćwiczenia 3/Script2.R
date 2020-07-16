library(tidyverse)

dane2 <- read.table('http://theta.edu.pl/wp-content/uploads/2018/03/dane_wino.txt', sep = ',')
attach(dane2)

dane2 <- rename(dane2, t_uprawy=V1)

dane2$t_uprawy <- as.factor(dane2$t_uprawy)

model.2 <- lda(t_uprawy ~ ., data = dane2)

model.2$means

LD1 <- predict(model.2)$x[,1]

LD2 <- predict(model.2)$x[,2]

plot(LD1, LD2, xlab = " Pierwsza zmienna kanoniczna ", ylab = " Druga zmienna
kanoniczna ", type ="n" )
text (cbind (LD1, LD2 ), labels = unclass(dane2$t_uprawy))

sum(LD1 * (dane2$t_uprawy == 1)) / sum(dane2$t_uprawy == 1)
sum(LD2 * (dane2$t_uprawy == 1)) / sum(dane2$t_uprawy == 1)

sum(LD1 * (dane2$t_uprawy == 2)) / sum(dane2$t_uprawy == 2)
sum(LD2 * (dane2$t_uprawy == 2)) / sum(dane2$t_uprawy == 2)

sum(LD1 * (dane2$t_uprawy == 3)) / sum(dane2$t_uprawy == 3)
sum(LD2 * (dane2$t_uprawy == 3)) / sum(dane2$t_uprawy == 3)

