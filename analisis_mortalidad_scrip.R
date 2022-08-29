## Rutas para la carga de la base de datos
ruta_mac <- ''
ruta_win <- 'D:\\Datasets\\defunciones_deis_abierto\\'
print('Ingresar el sistema operativo (win o mac)')
so = readline(); 

## Cargar la base de datos
if (so == 'win'){
nombre_archivo <- paste(ruta_win,'DEF_1990_2018.csv', sep="")
bd = read.csv2(nombre_archivo)
} else {
  print('Agregar ruta del mac')
}

## Ver la base de datos y dejarla atada 
View(bd)
attach(bd)

año_incio = 2013
año_termino = 2015
periodo = paste(año_incio, "-", año_termino, sep="")
desagregacion = "Región de Antofagasta"
  
#### Analisis univariado de las variables Edad, Sexo, 
## Evaluar la distribucion normal de la variable edad
library(nortest)
edad = bd$EDAD_CANT[EDAD_CANT < 999 & EDAD_TIPO == 1 & FECHA_DEF >= año_incio 
                    & FECHA_DEF <= año_termino & REG_RES == 2]
summary(edad)
var(edad)
sqrt(var(edad))
sd(edad)
sd(edad)/mean(edad)*100
max(edad)-min(edad)


## test de Shapiro - wilk
shapiro.test(edad[0:5000])

## test de Anderson/Darling para la normalidad
ad.test(edad)

## Histograma para la edad
titulo <- paste('Histograma para edad', periodo, desagregacion)
hist(edad, breaks = "Sturges", freq = F, plot = T,
     main = titulo,
     xlab = 'Edad en años',
     ylab = 'Frecuencia relativa acumulada')

## construcción tabla de frecuencias para el polígono de frecuencias
range(edad)
cortes <- seq(0, 125, by=5)
edad.cut <- cut(edad, cortes, right = FALSE)
edad.freq <- table(edad.cut)

total <- rep(sum(edad.freq), 25)
edad.relfreq = edad.freq / total
edad.cumfreq <- c(0,cumsum(edad.freq))
edad.relcumfreq <- c(cumsum(edad.relfreq))

cbind(edad.freq, edad.relfreq, edad.cumfreq, edad.relcumfreq*100)


## polígono de frecuencias
plot(cortes[1:(length(cortes)-1)], (edad.relcumfreq)*100,
     main="Edad defunciones",
     xlab = "Edad en años",
     ylab = "Frecuencias relativas acumuladas")
lines(cortes[1:(length(cortes)-1)], (edad.relcumfreq)*100)


boxplot(edad, main = 'Diagrama de caja bigotes para la edad', ylab= 'Edad')

## curtosis y asimetria
## asimetria
library(moments)

skewness(edad)
?skewness
## curtosis
kurtosis(edad)

jarque.test(edad)


