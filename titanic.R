#Librerías utilizadas
if(!require(dplyr)){
    install.packages('dplyr', repos='http://cran.us.r-project.org')
    library(dplyr)
}

if(!require(nortest)){
    install.packages('nortest', repos='http://cran.us.r-project.org')
    library(nortest)
}

#Lectura del fichero
titanic1<-read.csv("D:/Aitor/Escritorio/train.csv",header=TRUE)

#Dimensiones de los datos
dim(titanic1)
 
#Estructura de los datos
str(titanic1)

#Eliminación de columnas no útiles
titanic <- select(titanic1, Survived, Pclass, Sex, Age, SibSp, Parch)

#Tranformar Pclass a factorial
titanic$Pclass <- factor(titanic$Pclass)



#Nueva estructura de datos
str(titanic)

#Contar numero de vacios por columnas
sapply(titanic, function(x) sum(is.na(x)))

#Eliminación de registros con Age vacios
titanic <- titanic[!is.na(titanic$Age),]

#Contar numero de vacios por columnas
sapply(titanic, function(x) sum(is.na(x)))

#Dimensiones de los datos resultante
dim(titanic)

#Identificacion de valores extremos
boxplot.stats(titanic$Survived)$out
boxplot.stats(titanic$Age)$out
boxplot.stats(titanic$SibSp)$out
boxplot.stats(titanic$Parch)$out

#Exportar datos
write.csv(titanic,"clean_titanic.csv") 

#Agrupacion por clase
titanic.Grupo1 <-titanic[titanic$Pclass.type == "1"]
titanic.Grupo2 <-titanic[titanic$Pclass.type == "2"]
titanic.Grupo3 <-titanic[titanic$Pclass.type == "3"]

#Agrupacion por Sex
titanic.Male <-titanic[titanic$Sex.type == "male"]
titanic.Female <-titanic[titanic$Sex.type == "female"]

#Revisar si las variables cuantitativas estan normalizadas
#Age
lillie.test(titanic$Age)
#SibSP
lillie.test(titanic$SibSp)
#Parch
lillie.test(titanic$Parch)

#Test Fligner-Killeen
fligner.test(Survived ~ Sex, data=titanic )

#Calculo del coeficiente de correlacion de Kendall para cada variable cuantitativa respecto al campo Survived
#Age
spearman_test1 = cor.test(titanic$Age, titanic$Survived, method = "kendall")
#SibSP
spearman_test2 = cor.test(titanic$SibSp, titanic$Survived, method = "kendall")
#Parch
spearman_test3 = cor.test(titanic$Parch, titanic$Survived, method = "kendall")

#Tabla con las correlaciones de spearman
tabla.corr <- matrix(c("Age",spearman_test1$estimate,spearman_test1$p.value,
                       "SibSP",spearman_test2$estimate,spearman_test2$p.value,
                       "Parch",spearman_test3$estimate,spearman_test3$p.value),ncol=3,byrow = TRUE)
colnames(tabla.corr) <- c("campo","estimate","p-value")
print(tabla.corr)

#Modelos de regresión lineal
#Regresores cuantitativos
Age=titanic$Age
SibSP=titanic$SibSp
Parch=titanic$Parch

#Regresores cualitativos
Pclass = titanic$Pclass
Sex = titanic$Sex

#Variable a predecir
Survived = titanic$Survived

#Definicion de modelos
modelo1 <- lm (Survived ~ Pclass + Sex + Age + SibSP + Parch, data = titanic )
modelo2 <- lm (Survived ~ Pclass + Sex + Age + SibSP , data = titanic )
modelo3 <- lm (Survived ~ Pclass + Sex + Age + Parch, data = titanic )
modelo4 <- lm (Survived ~ Pclass + Sex , data = titanic )
modelo5 <- lm (Survived ~ Sex + Age , data = titanic )

#Tabla con los coeficientes de determinación de cada modelo
tabla.coe <- matrix(c("Modelo1",summary(modelo1)$r.squared,
                       "Modelo2",summary(modelo2)$r.squared,
                       "Modelo3",summary(modelo3)$r.squared,
                       "Modelo4",summary(modelo4)$r.squared,
                       "Modelo5",summary(modelo5)$r.squared),ncol=2,byrow = TRUE)
colnames(tabla.coe) <- c("modelo","coeficiente determinacion")
print(tabla.coe)


#Datos para probar prediccion
Superviviente1 <- data.frame(Pclass = "1", Sex = "male", Age = 14, SibSP = 3, Parch = 2)

#Predicion a partir del modelo1
predict(modelo1, Superviviente1)