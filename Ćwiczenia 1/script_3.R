iris <- iris

head(iris)

hist(iris$Sepal.Length[iris$Species=='setosa'])
hist(iris$Sepal.Width[iris$Species=='setosa'])

hist(iris$Sepal.Length[iris$Species=='versicolor'])
hist(iris$Sepal.Width[iris$Species=='versicolor'])

hist(iris$Sepal.Length[iris$Species=='virginica'])
hist(iris$Sepal.Width[iris$Species=='virginica'])

pairs(iris[iris$Species=='setosa',1:4]) #Silna korelacja
pairs(iris[iris$Species=='versicolor',1:4]) #Silna korelacja
pairs(iris[iris$Species=='virginica',1:4]) #Silna korelacja


cor(iris[iris$Species=='setosa',c(1,3)]) #0.26
cor(iris[iris$Species=='versicolor',c(1,3)]) #0.75
cor(iris[iris$Species=='virginica',c(1,3)]) #0.86

cor.test(iris$Sepal.Length[iris$Species=='setosa'], iris$Petal.Length[iris$Species=='setosa']) #0.26 Wynik nieostotny
cor.test(iris$Sepal.Length[iris$Species=='versicolor'], iris$Petal.Length[iris$Species=='versicolor']) #0.26 Wynik istotny
cor.test(iris$Sepal.Length[iris$Species=='virginica'], iris$Petal.Length[iris$Species=='virginica']) #0.26 Wynik istotny

iris.setosa <- iris[iris$Species=='setosa',1:4]

attach(iris.setosa)
model.all <- lm(Petal.Length ~ Sepal.Length + Sepal.Width + Petal.Width, data = iris.setosa)
summary(model.all)

model.1 <- lm(Petal.Length ~ Sepal.Length + Petal.Width, data = iris.setosa)
summary(model.1)

model.2 <- lm(Petal.Length ~ Petal.Width, data = iris.setosa)
summary(model.2)

#Petal.Length = Petal.Width * 0.54 + 1.32: Przy wzroście wartości Petal.Width o 1, zmienna Petal.Length wzrośnie o 0.54


