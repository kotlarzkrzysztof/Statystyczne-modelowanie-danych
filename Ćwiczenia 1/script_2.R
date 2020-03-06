library(tidyverse)
library(ISwR)
attach(kfm)

kfm <- kfm
kfm$sex <- as.numeric(kfm$sex)
kfm$sex <- as.factor(kfm$sex)


model.1 <- lm(dl.milk~sex + weight + ml.suppl + mat.weight + mat.height)
summary(model.1)

#Zmienne istotne:
#weight, mat.height
#Zmienna sex jako factor ponieważ ta zmienna przyjmuje tylko 2 poziomy, jest to zmienna jakościowa


pairs(kfm)
cor(kfm)

#Z wykresu zależności parami jak i macierzy korelacji wynikają następujące silnie skorelowane pary:
#Weight <-> dl.milk
#mat.weight <-> mat.height

model.all <- lm(dl.milk ~ ., data = kfm)
summary(model.all)

model.all_1 <- lm(dl.milk ~ no + sex + weight + ml.suppl + mat.height, data = kfm)
summary(model.all_1)

model.all_2 <- lm(dl.milk ~ sex + weight + ml.suppl + mat.height, data = kfm)
summary(model.all_2)

model.all_3 <- lm(dl.milk ~ weight + ml.suppl + mat.height, data = kfm)
summary(model.all_3)

model.all_4 <- lm(dl.milk ~ weight + mat.height, data = kfm)
summary(model.all_4)

#Ostateczny model:
#dl.milk = weight * 1.43 + mat.height * 0.07 -11.92

