# Maria Fernanda Viveros Segovia 
# 1917915
# 19.05.2021
#  Prueba de T de dos muestras dependientes 

prod <- read.csv("https://raw.githubusercontent.com/Marimari02/PrincipiosEstadistica2021/main/mainproduccion.csv", header = T)
head(prod)
summary(prod)
is.factor(prod$Tiempo)

boxplot(prod$Kgsem ~ prod$Tiempo, col= "lightgreen")

# Pregunta de investigacion 
# Estamos interesados en conocer si la produccion en Kg de semillas de los individuos de Pinos es diferente en el
# ano 2012 y 2013

# Hipotesis nula (H0): no existe diferencia entre la prod. en Kg en los anos 2012 y 2013 
# Hipotesis alternativa (H1): existe diferencia entre la prod. en Kg de los anos 2012 y 2013 
# alfa = 0.05


# Revisar normalidad 
shapiro.test(prod$Kgsem)

# Revisar la homogeneidad de varianza 
var.test(prod$Kgsem ~ prod$Tiempo)

# Prueba de T para muestras dependientes
t.test(prod$Kgsem ~ prod$Tiempo, paired= T)
