# Maria Fernanda Viveros Segovia
# 1917915
# 28.04.2021
# Trabajo en clase

# transporte de datos "Canopy" --------------------------------------------

copa <- read.csv("https://raw.githubusercontent.com/Marimari02/PrincipiosEstadistica2021/main/canopy.csv", header = T)
head(copa)

cnpy.x <- density(copa$Cnpy)
hist(copa$Cnpy, freq = FALSE)
lines(cnpy.x, col = "red", lwd=2)

# alfa = 0.05
# H0 los datos provienen de una distribuccion normal
# H1 los datos no provienen de una distribuccion normal

shapiro.test(copa$Cnpy)

hist(copa$LAI4)
shapiro.test(copa$LAI4)

hist(copa$GLI)
shapiro.test(copa$GLI)

# Prueba de kolmogorov ----------------------------------------------------

# alfa = 0.05
# H0 los datos provienen de una distribuccion normal
# H1 los datos no provienen de una distribuccion normal

ks.test(copa$Cnpy, "pnorm", mean=mean(copa$Cnpy), sd=sd(copa$Cnpy))

ks.test(copa$LAI4, "pnorm", mean=mean(copa$LAI4), sd=sd(copa$LAI4))

ks.test(copa$GLI, "pnorm", mean=mean(copa$GLI), sd=sd(copa$GLI))

# Determinar la homogeneidad de las varianzas  ----------------------------

BE <- subset(copa, Forest == "CBE")
SR <- subset(copa, Forest == "CSR")

var(BE$Cnpy)
var(SR$Cnpy)

# H0 = las varianzas son iguales (homogeneas)
# H1 = las varianzas no son iguales 

var.test(BE$Cnpy, SR$Cnpy)

# Varianzas de la variable LAI4

var(BE$LAI4)
var(SR$LAI4)

var.test(BE$LAI4, SR$LAI4)

# Varianzas de la variable GLI

var(BE$GLI)
var(SR$GLI)

var.test(BE$GLI, SR$GLI)

copa$Forest <- factor(copa$Forest)
library(car)
densityPlot(copa$LAI4 ~ copa$Forest)
summary(copa)

densityPlot(copa$Cnpy ~ copa$Forest)
densityPlot(copa$GLI ~ copa$Forest)
