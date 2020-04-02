library(tidyverse)

dane <-
  read.table('http://theta.edu.pl/wp-content/uploads/2018/03/DANE_predkosc_reakcji.txt')

dane <- rename(dane, predkosc = V1, koncetracja = V2)


attach(dane)
plot(predkosc ~ koncetracja)

# Nie ma

par(mfrow = c(1, 3))

plot(
  predkosc ~ koncetracja,
  main = 'Prędkość dla koncetracji',
  xlab = 'Koncetracja',
  ylab = 'log(Prędkość)',
  log = 'y'
)
plot(
  predkosc ~ koncetracja,
  main = 'Prędkość dla koncetracji',
  xlab = 'log(Koncetracja)',
  ylab = 'log(Prędkość)',
  log = 'xy'
)
plot(
  predkosc ~ koncetracja,
  main = 'Prędkość dla koncetracji',
  xlab = 'log(Koncetracja)',
  ylab = 'Prędkość',
  log = 'x'
)


model.1 <- lm(log(predkosc) ~ koncetracja)
plot(model.1$fitted, model.1$residuals)

model.2 <- lm(log(predkosc) ~ log(koncetracja))
plot(model.2$fitted, model.2$residuals)

model.3 <- lm(predkosc ~ log(koncetracja))
plot(model.3$fitted, model.3$residuals)

# Losowy charakter reszt dla modelu 1, 2 i 3. Dla modelu  2 reszty skupiają sie wokół 0, ten model sugeruje dobre dopasowanie

model.4 <- lm(predkosc ~ koncetracja + I(koncetracja ^ 2))

summary(model.4)

plot(predkosc ~ koncetracja)

curve(10.69371 + 2.77768 * x - .09782 * x ^ 2, add = T)

# Wszystkie wspołczynniki są nieistotne statystycznie ,współczynnik dopasowania R2 = 0.17

plot(model.4$fitted.values ~ model.4$residuals)

# Charakter reszt jest losowy, lecz nie skupia sie wokół zera. Model nie jestodpowiedni

model.5 <-
  lm(
    predkosc ~ koncetracja + I(koncetracja ^ 2) + I(koncetracja ^ 3) + I(koncetracja ^
                                                                           4) + I(koncetracja ^ 5) + I(koncetracja ^ 6)
  )

summary(model.5)
plot(predkosc ~ koncetracja)
curve(
  -120.76838 + 325.45467 * x - 246.63164 * x ^ 2 + 83.777377 * x ^ 3+-14.25726 * x ^
    4 + 1.18499 * x ^ 5 - 0.03785 * x ^ 6,
  add = T,
  col = 'red'
)

# Wspołczynnik dopasowania = 1. Nie jest to model odpowiedni, gdyż doapasowana krzywa jest dopasowana do wszystkich punktów zbioru , nie ma ona wartości predykcyjnej dla przyszłych obserwacji