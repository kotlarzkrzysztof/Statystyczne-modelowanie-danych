attach(stackloss)
head(stackloss)

model.1 <- lm(stack.loss~Water.Temp + Acid.Conc.)
summary(model.1)

#Jedna zmienna (Water.Temp), zmienna Acid.Conc. jest nieisttona na poziomie a 5%

model.2 <- lm(stack.loss~Water.Temp)
summary(model.2)

plot(stack.loss~Water.Temp)
abline(model.2, col= 'red')

#Brak potencjanych punktów o dużej dzwigni

reszty.stud <- rstudent(model.2)
outlier <- reszty.stud[abs(reszty.stud) > 2]

#Brak reszta odstających wyznaczonych za pomocą reszt studentyzowanych

outlierTest(model.2)

#Test wykrywa jedną obserwacje odstającą, jesdnak jest ona nieistotna statystycznie na poziomie istotnosci 5%

influencePlot(model.2)
# n/21 = 0,19 # 1,3

influence.measures(model.2)

# 1

inf.ommit <- stackloss[-c(1),]

model.3 <- lm(inf.ommit$stack.loss~inf.ommit$Water.Temp)

plot(inf.ommit$stack.loss~inf.ommit$Water.Temp)
abline(model.3, col= 'red')

summary(model.3)
