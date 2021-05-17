# Maria Fernanda Viveros Segovia 
# 1917915
# 13.05.2021
# Pruebas de T de dos muestras independientes

copa <- read.csv("https://raw.githubusercontent.com/Marimari02/PrincipiosEstadistica2021/main/canopy.csv", header = T)
head(copa)

summary(copa)
copa$Forest <- factor(copa$Forest)
summary(copa)

boxplot(copa$Cnpy ~ copa$Forest, col = "blue")

shapiro.test(copa$Cnpy)
var.test(copa$Cnpy ~ copa$Forest)

t.test(copa$Cnpy ~ copa$Forest, var.equal=TRUE)


# Variable LAI4 ------------------------------------------------------

summary("LAI4")
copa$LAI4 <- factor(copa$LAI4)
summary("LAI4")

boxplot(copa$Cnpy ~ copa$LAI4, col = "red")

shapiro.test(copa$Cnpy)


t.test(copa$Cnpy ~ copa$LAI4, var.equal=TRUE)



# Variable GLI ------------------------------------------------------------

summary(copa)
boxplot(copa$Cnpy ~ copa$GLI, col = "purple")

shapiro.test(copa$Cnpy)

var.test(copa$GLI ~ copa$Forest)
