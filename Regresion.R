# Maria Fernanda Viveros Segovia 
# 1917915
# 14.04.2021
# regresion, trabajo ene clase 

xi <- c(15, 14, 13, 12, 9, 8)
yi <- c(8, 7, 7, 6, 5, 4)

plot(xi, yi, pch = 16)

# Crear un objeto con dos columnas diam (xi) y altura (yi)
data <- data.frame(xi, yi)

# Crear una nueva columna en datos que tenga la observacion en  menos la media de x
data$xi_m <- round(data$xi - mean(data$xi),2)


# Crear una nueva columna en datos que tenga la observacion  y menos la media de y
data$yi_m <- round(data$yi - mean(data$yi),2)

# Crear la ultiplicacion de xi_m * yi_m 
data$xiyi_M <- round(data$xi_m*data$yi_m,2)

data$xi_m2 <- round(data$xi_m^2,2)

# Determinar beta  --------------------------------------------------------

# Estimar el coeficiente Beta (pendiente de la linea de regresion)
beta <- sum(data$xiyi_M)/sum(data$xi_m2)

# Valor de beta es 
beta


# Dterminar Alfa ----------------------------------------------------------

# Alfa- media Y - beta*media de X 

alfa <- mean(data$yi)- beta*mean(data$xi)
alfa

data$yprima <- round(alfa - beta*data$xi,2)

plot(data$xi, data$yi, pch = 16, col = "red")
lines(data$xi, data$yprima, type= "l", lty=2, lwd=2)

lines() # Ejercicios en clase. Encontrar diametros

diametros <- c(8.5, 10.3, 11.4, 12.5, 13.6, 14.3)

yprima <- round(alfa + beta * diametros, 2)

estimados <- data.frame(diametros, yprima)

# Regresion usando la funcion lm  -----------------------------------------

diam.lm <- lm(data$yi ~ data$xi)
diam.lm
summary(diam.lm)

diam.lm$fitted.values
sum(diam.lm$residuals)

plot(data$xi, data$yi, col= "green", pch=16)
abline(diam.lm)

cor.test(data$xi, data$yi)
