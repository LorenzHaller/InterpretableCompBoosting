# Weißweindatensatz einlesen
white <- read.csv2("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", dec = ".", header = TRUE) 

modell <- as.formula("quality ~ fixed.acidity + volatile.acidity + 
                      citric.acid + residual.sugar + chlorides + 
                      free.sulfur.dioxide + total.sulfur.dioxide + 
                      density + pH + sulphates + alcohol")
white$quality <- as.factor(white$quality)

class(white$quality)

mboost(white, family = Binomial(), formula = modell)
