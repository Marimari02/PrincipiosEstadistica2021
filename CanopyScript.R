# Maria Fernanda Viveros Segovia 
#19171915
# 22.04.2021
# clase 


# Exportacion de datos ----------------------------------------------------

copa <- read.csv("https://raw.githubusercontent.com/Marimari02/PrincipiosEstadistica2021/main/canopy.csv", header = T)
head(copa)

summary(copa)

plot(copa$Cnpy, copa$LAI4, pch=16)
cor.test(copa$Cnpy, copa$LAI4)


copa.lm<- lm(copa$LAI4 ~ copa$Cnpy)
copa.lm

summary(copa.lm)

plot(copa$Cnpy, copa$LAI4, pch=16, xlab = "Apertura del dosel (x)", ylab = "Indice de area foliar")
abline(copa.lm, col = "red")
text(23, 1.0, "Y = 2.737 - 0.047*(X)")


# Cuales son los valores de la linea de regresion? Agregar una columna "Predichos" en la copa 
copa.lm$fitted.values
# Donde estan almacenados esos valores?
# Estan almacenados en copa.lm$fitted.values
copa$predichos <- copa.lm$fitted.values
# cuantos grados de libertad (df) tiene el analisis de regresion? 
# 178 GL a df

# Determinar mediante la ecuacion de regresion los siguientes valores: 
# 20, 22, 24, 25, 26, 28.3, 30.3, 31.8, 33, 35

valores <- c(20, 22, 24, 25, 26, 28.3, 30.3, 31.8, 33, 35)
2.737 - 0.247*(valores)
