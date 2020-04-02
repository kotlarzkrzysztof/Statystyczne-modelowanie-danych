attach(stackloss)
head(stackloss)

model.1 <- lm(stack.loss~Air.Flow)

plot(stack.loss~Air.Flow)
abline(model.1, col= 'red')

laver.ommit <- stackloss[!Air.Flow==70,]

model.2 <- lm(laver.ommit$stack.loss~laver.ommit$Air.Flow)

plot(stack.loss~Air.Flow, )

abline(model.1, col= 'red')
abline(model.2, col= 'blue')

legend("bottomright",
       legend = c("Outliner included", "Outliner ommited"),
       col = c('red', 'blue'),
       lwd = 2)

summary(model.1)
summary(model.2)

reszty.stud <- rstudent(model.1)
outlier <- reszty.stud[abs(reszty.stud) > 2]

library(car)
outlierTest(model.1)

influencePlot(model.1)
#1 ,4, 21

influence.measures(model.1)
#1 ,4, 21

inf.ommit <- stackloss[-c(1,4,21),]

model.3 <- lm(inf.ommit$stack.loss~inf.ommit$Air.Flow)

plot(inf.ommit$stack.loss~inf.ommit$Air.Flow)
abline(model.3, col= 'red')