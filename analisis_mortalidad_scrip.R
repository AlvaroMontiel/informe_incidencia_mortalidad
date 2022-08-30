## Rutas para la carga de la base de datos
ruta_mac <- '/Users/alvaro/Documents/Data_Science/datasets/defunciones/'
ruta_win <- 'D:\\Datasets\\defunciones_deis_abierto\\'

## Ingreso de variables:
so = readline(prompt = 'Ingresar el sistema operativo (win o mac): ')
año_inicio = readline(prompt = 'Ingresar el año de inicio: ')
año_termino = readline(prompt = "Ingresar el año de término: ")
desagregacion = readline(prompt = "Ingresar el nivel de desagregación (nacional
                         o antofagasta): ")
comuna_analisis = readline(prompt = "Ingresar la comuna (antofagasta, calama, 
                           tocopilla, na):")
tipo_mortalidad = readline(prompt = "Mortalidad general o por cáncer: ")
periodo = paste(año_incio, "-", año_termino, sep="")

## Cargar la base de datos
if (so == 'win'){
nombre_archivo <- paste(ruta_win,'DEF_1990_2018.csv', sep="")
bd = read.csv2(nombre_archivo)
} else if (so == 'mac'){
nombre_archivo <- paste(ruta_mac,'DEF_2010_2018.csv', sep="")
bd = read.csv2(nombre_archivo)
}
###########################################################################
###########################################################################
############################## PRE ANALITICA ##############################
###########################################################################
###########################################################################

## Ver la base de datos y dejarla atada 
View(bd)
attach(bd)

bd = bd %>% mutate (TIPO_MUERTE = case_when(
  grepl(patron,bd$CAPITULO_DIAG1) == T ~ 1,
  grepl(patron,bd$CAPITULO_DIAG1) == F ~ 0))


## FILTROS POR PERIODO Y DESAGREGACION TERRITORIAL
if (desagregacion == 'nacional'){
  bd_ = subset(bd, ANO_DEF >= as.integer(año_inicio) & 
               ANO_DEF <= as.integer(año_termino))
} else if (desagregacion == 'antofagasta' & comuna_analisis == 'na'){
  bd_ = subset(bd, ANO_DEF >= año_inicio &
               ANO_DEF <= año_termino & REG_RES == 2)
} else if (desagregacion == 'antofagasta' & comuna_analisis == 'antofagasta') {
  bd_ = subset(bd, ANO_DEF >= año_inicio & 
               ANO_DEF <= año_termino & REG_RES == 2 &
               COMUNA == 2101)
} else if (desagregacion == 'antofagasta' & comuna_analisis == 'calama') {
  bd_ = subset(bd, ANO_DEF >= año_inicio & 
               ANO_DEF <= año_termino & REG_RES == 2 &
               COMUNA == 2201)
} else if (desagregacion == 'antofagasta' & comuna_analisis == 'tocopilla') {
  bd_ = subset(bd, ANO_DEF >= año_inicio & 
              ANO_DEF <= año_termino & REG_RES == 2 &
              COMUNA == 2301)
}

bd_ = subset(bd_, EDAD_CANT != 999)
summary(bd_$EDAD_CANT)

bd_ = bd_ %>% mutate (EDAD = case_when(
  bd_$EDAD_TIPO > 1 ~ 0,
  bd_$EDAD_TIPO == 1 ~ as.double(bd_$EDAD_CANT)))



###########################################################################
###########################################################################
############################### ANALITICA #################################
###########################################################################
###########################################################################

###########################################################################
################# ANALISIS UNIVARIADO MORTALIDAD GENERAL ##################
###########################################################################

###############################  EDAD  ####################################
library(dplyr)
library(moments)
attach(bd_)
print(paste(desagregacion, periodo, paste=""))
bd_ %>% summarise(Media = round(mean(EDAD),2),
                  Mediana = round(median(EDAD),2),
                  Varianza = round(var(EDAD),2),
                  Desviación_Estándar = round(sd(EDAD),2),
                  coeficiente_variacion = round(sd(EDAD)/mean(EDAD)*100,2),
                  Asimetría = round(skewness(EDAD),2),
                  Curtosis = round(kurtosis(EDAD),2),
                  Mínimo = min(EDAD),
                  Máximo = max(EDAD),
                  Rango = max(EDAD)-min(EDAD),
                  Percentil_25 = quantile(EDAD, probs = (.25)),
                  Percentil_50 = quantile(EDAD, probs = (.5)),
                  Percentil_75 = quantile(EDAD, probs = (.75)),
                  Rango_intercuartil = quantile(EDAD, probs = (.75)) - 
                    quantile(EDAD, probs = (.25))
                  )


## Evaluar la distribucion normal de la variable edad
library(nortest)
## test de Shapiro - wilk
shapiro.test(bd_$EDAD[0:5000])

## test de Anderson/Darling para la normalidad
ad.test(bd_$EDAD)

## Test de Jarque-Bera para la normalidad
jarque.test(bd_$EDAD_CANT)

## Histograma para la edad
titulo <- paste('Histograma según edad', periodo, desagregacion)
hist(bd_$EDAD, breaks = "Sturges", freq = F, plot = T,
     main = titulo,
     xlab = 'Edad en años',
     ylab = 'Frecuencia relativa acumulada')

## construcción tabla de frecuencias para el polígono de frecuencias
cortes <- seq(0, 125, by=5)
edad.cut <- cut(bd_$EDAD, cortes, right = FALSE)
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

## BOXPLOT 
titulo <- paste('Diagrama de caja bigotes para edad,',
                " ",periodo,", ",desagregacion, sep = "")
boxplot(bd_$EDAD,
        main = titulo, 
        ylab= 'Edad en años')



###############################  SEXO  ####################################
tabla_sexo <- table(factor(bd_$SEXO, 
levels = c(1,2,9),
labels = c('Hombres', 'Mujeres', 'Desconocido')))
addmargins(tabla_sexo)
addmargins(round(prop.table(tabla_sexo)*100,2))

###############################  REGION  ####################################
tabla_region <- table(bd_$GLOSA_REG_RES)
addmargins(tabla_region)

addmargins(round(prop.table(tabla_region)*100,2))


###########################################################################
################# ANALISIS UNIVARIADO MORTALIDAD CÁNCER ###################
###########################################################################

###############################  EDAD  ####################################

print(paste(desagregacion, periodo, paste=""))
bd_cancer <- subset(bd_, TIPO_MUERTE == 1)
bd_cancer  %>% summarise(Media = round(mean(EDAD),2),
                  Mediana = round(median(EDAD),2),
                  Varianza = round(var(EDAD),2),
                  Desviación_Estándar = round(sd(EDAD),2),
                  coeficiente_variacion = round(sd(EDAD)/mean(EDAD)*100,2),
                  Asimetría = round(skewness(EDAD),2),
                  Curtosis = round(kurtosis(EDAD),2),
                  Mínimo = min(EDAD),
                  Máximo = max(EDAD),
                  Rango = max(EDAD)-min(EDAD),
                  Percentil_25 = quantile(EDAD, probs = (.25)),
                  Percentil_50 = quantile(EDAD, probs = (.5)),
                  Percentil_75 = quantile(EDAD, probs = (.75)),
                  Rango_intercuartil = quantile(EDAD, probs = (.75)) - 
                    quantile(EDAD, probs = (.25))
)

shapiro.test(bd_cancer$EDAD[0:5000])
ad.test(bd_cancer$EDAD)
jarque.test(bd_cancer$EDAD_CANT)


## Histograma para la edad por cáncer
titulo <- paste('Histograma por cáncer según edad', periodo, desagregacion)
hist(bd_cancer$EDAD, breaks = "Sturges", freq = F, plot = T,
     main = titulo,
     xlab = 'Edad en años',
     ylab = 'Frecuencia relativa acumulada')

## construcción tabla de frecuencias para el polígono de frecuencias por cáncer
cortes <- seq(0, 125, by=5)
edad.cut <- cut(bd_cancer$EDAD, cortes, right = FALSE)
edad.freq <- table(edad.cut)

total <- rep(sum(edad.freq), 25)
edad.relfreq = edad.freq / total
edad.cumfreq <- c(0,cumsum(edad.freq))
edad.relcumfreq <- c(cumsum(edad.relfreq))

cbind(edad.freq, edad.relfreq, edad.cumfreq, edad.relcumfreq*100)

## polígono de frecuencias por cáncer
plot(cortes[1:(length(cortes)-1)], (edad.relcumfreq)*100,
     main="Edad defunciones por cáncer",
     xlab = "Edad en años",
     ylab = "Frecuencias relativas acumuladas")
lines(cortes[1:(length(cortes)-1)], (edad.relcumfreq)*100)

## BOXPLOT 
titulo <- paste('Diagrama de caja bigotes para cáncer y edad,',
                " ",periodo,", ",desagregacion, sep = "")

boxplot(bd_cancer$EDAD,
        main = titulo, 
        ylab= 'Edad en años')


###############################  SEXO  ####################################
tabla_sexo_cancer <- table(factor(bd_cancer$SEXO, 
                           levels = c(1,2,9),
                           labels = c('Hombres', 'Mujeres', 'Desconocido')))
addmargins(tabla_sexo_cancer)
addmargins(round(prop.table(tabla_sexo_cancer)*100,2))

###############################  REGION  ####################################
tabla_region_cancer <- table(bd_cancer$GLOSA_REG_RES)
addmargins(tabla_region_cancer)

addmargins(round(prop.table(tabla_region_cancer)*100,2))


###########################################################################
################# ANALISIS BIVARIADO MORTALIDAD GENERAL ##################
###########################################################################

## VARIABLES CATEGÓRGICAS (SEXO, REGION, COMUNA ACTIVIDAD)

## VARIABLE CUANTITATIVA EDAD POR SEXO

bd_hombres <- subset(bd_, SEXO == 1)
bd_hombres %>% summarise(Media = round(mean(EDAD),2),
                         Mediana = round(median(EDAD),2),
                         Varianza = round(var(EDAD),2),
                         Desviación_Estándar = round(sd(EDAD),2),
                         coeficiente_variacion = round(sd(EDAD)/mean(EDAD)*100,2),
                         Asimetría = round(skewness(EDAD),2),
                         Curtosis = round(kurtosis(EDAD),2),
                         Mínimo = min(EDAD),
                         Máximo = max(EDAD),
                         Rango = max(EDAD)-min(EDAD),
                         Percentil_25 = quantile(EDAD, probs = (.25)),
                         Percentil_50 = quantile(EDAD, probs = (.5)),
                         Percentil_75 = quantile(EDAD, probs = (.75)),
                         Rango_intercuartil = quantile(EDAD, probs = (.75)) - 
                           quantile(EDAD, probs = (.25))
)

shapiro.test(bd_hombres$EDAD[0:5000])
ad.test(bd_hombres$EDAD)
jarque.test(bd_hombres$EDAD)


bd_mujeres <- subset(bd_, SEXO == 2)
bd_mujeres %>% summarise(Media = round(mean(EDAD),2),
                         Mediana = round(median(EDAD),2),
                         Varianza = round(var(EDAD),2),
                         Desviación_Estándar = round(sd(EDAD),2),
                         coeficiente_variacion = round(sd(EDAD)/mean(EDAD)*100,2),
                         Asimetría = round(skewness(EDAD),2),
                         Curtosis = round(kurtosis(EDAD),2),
                         Mínimo = min(EDAD),
                         Máximo = max(EDAD),
                         Rango = max(EDAD)-min(EDAD),
                         Percentil_25 = quantile(EDAD, probs = (.25)),
                         Percentil_50 = quantile(EDAD, probs = (.5)),
                         Percentil_75 = quantile(EDAD, probs = (.75)),
                         Rango_intercuartil = quantile(EDAD, probs = (.75)) - 
                           quantile(EDAD, probs = (.25))
)

shapiro.test(bd_mujeres$EDAD[0:5000])
ad.test(bd_mujeres$EDAD)
jarque.test(bd_mujeres$EDAD)

titulo <- paste('Histograma para edad en hombres', periodo, desagregacion)
hist(bd_hombres$EDAD, breaks = "Sturges", freq = F, plot = T,
     main = titulo,
     xlab = 'Edad en años',
     ylab = 'Frecuencia relativa acumulada')

titulo <- paste('Histograma para edad en mujeres', periodo, desagregacion)
hist(bd_mujeres$EDAD, breaks = "Sturges", freq = F, plot = T,
     main = titulo,
     xlab = 'Edad en años',
     ylab = 'Frecuencia relativa acumulada')


## Polígono  de frecuencias por SEXO
## HOMBRES
cortes <- seq(0, 125, by=5)
edad.cut <- cut(bd_hombres$EDAD, cortes, right = FALSE)
edad.freq <- table(edad.cut)

total <- rep(sum(edad.freq), 25)
edad.relfreq = edad.freq / total
edad.cumfreq <- c(0,cumsum(edad.freq))
edad.relcumfreq <- c(cumsum(edad.relfreq))

cbind(edad.freq, edad.relfreq, edad.cumfreq, edad.relcumfreq*100)

## gráfico
plot(cortes[1:(length(cortes)-1)], (edad.relcumfreq)*100,
     main="Edad defunciones hombres",
     xlab = "Edad en años",
     ylab = "Frecuencias relativas acumuladas")
lines(cortes[1:(length(cortes)-1)], (edad.relcumfreq)*100)

## MUJERES
cortes <- seq(0, 125, by=5)
edad.cut <- cut(bd_mujeres$EDAD, cortes, right = FALSE)
edad.freq <- table(edad.cut)

total <- rep(sum(edad.freq), 25)
edad.relfreq = edad.freq / total
edad.cumfreq <- c(0,cumsum(edad.freq))
edad.relcumfreq <- c(cumsum(edad.relfreq))

cbind(edad.freq, edad.relfreq, edad.cumfreq, edad.relcumfreq*100)

## gráfico
plot(cortes[1:(length(cortes)-1)], (edad.relcumfreq)*100,
     main="Edad defunciones mujeres",
     xlab = "Edad en años",
     ylab = "Frecuencias relativas acumuladas")
lines(cortes[1:(length(cortes)-1)], (edad.relcumfreq)*100)



titulo <- paste('Diagrama de caja bigotes para edad según sexo,',
                " ",periodo,", ",desagregacion, sep = "")
boxplot(bd_$EDAD~
          factor(bd_$SEXO, levels = c(1,2), 
                 labels = c('Hombres', 'Mujeres')), 
        subset = (bd_$SEXO != 9), 
        main = titulo, 
        ylab= 'Edad en años',
        xlab = 'Sexo' )

## VARIABLE CUANTITATIVA EDAD POR SEXO

bd_hombres_cancer <- subset(bd_cancer, SEXO == 1)
bd_hombres_cancer %>% summarise(Media = round(mean(EDAD),2),
                                Mediana = round(median(EDAD),2),
                                Varianza = round(var(EDAD),2),
                                Desviación_Estándar = round(sd(EDAD),2),
                                coeficiente_variacion = round(sd(EDAD)/mean(EDAD)*100,2),
                                Asimetría = round(skewness(EDAD),2),
                                Curtosis = round(kurtosis(EDAD),2),
                                Mínimo = min(EDAD),
                                Máximo = max(EDAD),
                                Rango = max(EDAD)-min(EDAD),
                                Percentil_25 = quantile(EDAD, probs = (.25)),
                                Percentil_50 = quantile(EDAD, probs = (.5)),
                                Percentil_75 = quantile(EDAD, probs = (.75)),
                                Rango_intercuartil = quantile(EDAD, probs = (.75)) - 
                                  quantile(EDAD, probs = (.25))
)

shapiro.test(bd_hombres_cancer$EDAD[0:5000])
ad.test(bd_hombres_cancer$EDAD)
jarque.test(bd_hombres_cancer$EDAD)


bd_mujeres_cancer <- subset(bd_cancer, SEXO == 2)
bd_mujeres_cancer %>% summarise(Media = round(mean(EDAD),2),
                                Mediana = round(median(EDAD),2),
                                Varianza = round(var(EDAD),2),
                                Desviación_Estándar = round(sd(EDAD),2),
                                coeficiente_variacion = round(sd(EDAD)/mean(EDAD)*100,2),
                                Asimetría = round(skewness(EDAD),2),
                                Curtosis = round(kurtosis(EDAD),2),
                                Mínimo = min(EDAD),
                                Máximo = max(EDAD),
                                Rango = max(EDAD)-min(EDAD),
                                Percentil_25 = quantile(EDAD, probs = (.25)),
                                Percentil_50 = quantile(EDAD, probs = (.5)),
                                Percentil_75 = quantile(EDAD, probs = (.75)),
                                Rango_intercuartil = quantile(EDAD, probs = (.75)) - 
                                  quantile(EDAD, probs = (.25))
)

shapiro.test(bd_mujeres$EDAD[0:5000])
ad.test(bd_mujeres$EDAD)
jarque.test(bd_mujeres$EDAD)

titulo <- paste('Histograma por cáncer según edad en hombres', periodo, desagregacion)
hist(bd_hombres_cancer$EDAD, breaks = "Sturges", freq = F, plot = T,
     main = titulo,
     xlab = 'Edad en años',
     ylab = 'Frecuencia relativa acumulada')

titulo <- paste('Histograma por cáncer según edad en mujeres', periodo, desagregacion)
hist(bd_mujeres_cancer$EDAD, breaks = "Sturges", freq = F, plot = T,
     main = titulo,
     xlab = 'Edad en años',
     ylab = 'Frecuencia relativa acumulada')


## Polígono  de frecuencias por cáncer según SEXO
## HOMBRES
cortes <- seq(0, 125, by=5)
edad.cut <- cut(bd_hombres_cancer$EDAD, cortes, right = FALSE)
edad.freq <- table(edad.cut)

total <- rep(sum(edad.freq), 25)
edad.relfreq = edad.freq / total
edad.cumfreq <- c(0,cumsum(edad.freq))
edad.relcumfreq <- c(cumsum(edad.relfreq))

cbind(edad.freq, edad.relfreq, edad.cumfreq, edad.relcumfreq*100)

## gráfico
plot(cortes[1:(length(cortes)-1)], (edad.relcumfreq)*100,
     main="Edad defunciones cáncer hombres",
     xlab = "Edad en años",
     ylab = "Frecuencias relativas acumuladas")
lines(cortes[1:(length(cortes)-1)], (edad.relcumfreq)*100)

## MUJERES
cortes <- seq(0, 125, by=5)
edad.cut <- cut(bd_mujeres_cancer$EDAD, cortes, right = FALSE)
edad.freq <- table(edad.cut)

total <- rep(sum(edad.freq), 25)
edad.relfreq = edad.freq / total
edad.cumfreq <- c(0,cumsum(edad.freq))
edad.relcumfreq <- c(cumsum(edad.relfreq))

cbind(edad.freq, edad.relfreq, edad.cumfreq, edad.relcumfreq*100)

## gráfico
plot(cortes[1:(length(cortes)-1)], (edad.relcumfreq)*100,
     main="Edad defunciones cáncer mujeres",
     xlab = "Edad en años",
     ylab = "Frecuencias relativas acumuladas")
lines(cortes[1:(length(cortes)-1)], (edad.relcumfreq)*100)


titulo <- paste('Diagrama de caja bigotes para cáncer y edad según sexo,',
                " ",periodo,", ",desagregacion, sep = "")
boxplot(bd_cancer$EDAD~
          factor(bd_cancer$SEXO, levels = c(1,2), 
                 labels = c('Hombres', 'Mujeres')), 
        subset = (bd_cancer$SEXO != 9), 
        main = titulo, 
        ylab= 'Edad en años',
        xlab = 'Sexo' )
