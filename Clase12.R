# Maria Fernanda Viveros Segivia 
# 1917915
# 12.05.2021
# Pruebas de T de una muestra 

copa <- read.csv("https://raw.githubusercontent.com/Marimari02/PrincipiosEstadistica2021/main/canopy.csv", header = T)
head(copa)
 
mean(copa$Cnpy)
length(copa$Cnpy)

# H0 la media de la variable apertura del dosel (observadas en campo) es = a la media de 31% (media teoretica).
#H1 la media de la variable apaertura del dosel (observada en campo) es diferente a la media de 31%.
# alfa = 0.05

# Aplicar prueba de t de una muestra 
t.test(copa$Cnpy, mu = 30.4)
