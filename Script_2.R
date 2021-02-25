# Maria Fernanda Viveros Segovia 
# Matricula
# 25.02.2021
# 


dbh <- c(16.5, 25.3, 22.1, 17.2, 16.1, 8.1, 34.3, 5.4, 5.7, 11.2, 24.1,  
             14.5, 7.7, 15.6, 15.9, 10, 17.5, 20.5, 7.8, 27.3, 9.7, 6.5,  
             23.4, 8.2, 28.5, 10.4, 11.5, 14.3, 17.2, 16.8)
length(dbh)


# Medidas de tendencia central --------------------------------------------

# Media 

sum(dbh)/length(dbh)

# Mediana

median(dbh)

# Media geometrica

exp(mean(log(dbh)))

# Moda 

moda = function(x)
{
m1 <- sort(table(x), decreasing = TRUE)
moda <- names(m1[m1==m1[1]])
moda <- as.numeric(moda)
return(moda)
}



# Medidas de dispersion ---------------------------------------------------

# Rango

range(dbh)

# Varianza (S^2)

var(dbh)

# Desviacion estandar (s)sd

sd(dbh)
sqrt(var(dbh)) # Obtener la raiz cuadrada de la varianza sd

# Funcion fivenum
fivenum(dbh)

# Coeficiente de variacion (CV %)
100*sd(dbh) / mean(dbh)


# Representacion grafica  -------------------------------------------------

# Grafica de Boxplot o de cajas 
boxplot(dbh, horizontal = TRUE, col = "blue", main = "Grafica de boxplot", ylab = "Diametro (cm")

# Grafica de Tallo y hoja (stem)
stem(dbh, scale= 3)

# Grafica de histograma 
hist(dbh, main = "Histogram", xlab = "Diametro (cm)", ylab = "Frecuencia", 
     ylim = c(0,10), col = "green")
     
    