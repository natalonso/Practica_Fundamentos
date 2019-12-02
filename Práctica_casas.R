# práctica casas

# https://www.kaggle.com/harlfoxem/housesalesprediction

# librerias ----
library(lattice)
library(dplyr)
library(VIM)
library(mice)
library(DMwR2)
library(knitr)
library(kableExtra)
library(htmltools)
library(bsplus)
library(RColorBrewer)
library(GGally)
library(ggplot2)

datos <- read.csv("C:/Users/natal/OneDrive/Documentos/0_MIS_DOCUMENTOS/2.MÁSTER/2_Curso_2019-2020/Primer_Cuatrimestre/2.Fundamentos_de_Análisis/BloqueIV_Métodos/Práctica final/kc_house_data.csv")
#datos <- read.csv("C:/Users/susi_/Desktop/Datas/kc_house_data.csv")
#datos <- read.csv("C:/Users/Beatriz/Desktop/Máster/1er trimestre/Fundamentos/Parte 4_Métodos de Análisis de datos/Práctica/kc_house_data.csv")

# datos------
str(datos)
summary(datos)


######################################
#analisis univariante----
###################################### 


# price- NUMERICA
hist(datos$price)
#vamos a tener que hacer una tranformacion
#lO QUE VAMOS A HACER ES CREAR una nueva variable tranformada con el logaritmo.

datos$log_price<- log10(datos$price)
datos %>%  select(log_price) %>% 
  na.omit() %>%
  ggplot(aes(x= log_price))+
  geom_density()


# bedrooms-CATEGORICA
table(datos$bedrooms)
hist(datos$bedrooms)
class(datos$bedrooms) # como es numerica, la pasamos a factor
datos$bedrooms <- as.factor(datos$bedrooms)
class(datos$bedrooms)

# datos extraños -> casas con 0 y 33 (mice)
#HACER EN CLASE EL PROXIMO DIA

# bathrooms-CATEGORICA
table(datos$bathrooms)
hist(datos$bathrooms)
datos$bathrooms <- as.factor(datos$bathrooms)

datos %>%  select(bathrooms) %>% 
  na.omit() %>%
  ggplot(aes(x= bathrooms))+
  geom_density()

# baños -> cada baño tiene 4 piezas. 0,25 es una pieza, 0.5 2 piezas, 0.75 3 piezas y 1 4 piezas piezas
# tenemos casas con 0 baños

# sqft_living- pies^2 del espacio interior habitable-NUMERICA
# 1 PIE son 0.3048 METROS
hist(datos$sqft_living)
# la transformamos
datos$log_sqft_living<- log10(datos$sqft_living)
datos %>%  select(log_sqft_living) %>% 
  na.omit() %>%
  ggplot(aes(x= log_sqft_living))+
  geom_density()

# sqft_lot- pies^2 del terreno-NUMERICA, 
hist(datos$sqft_lot, breaks=20000)
# los lejanos de 0  hay que estudiarlos
#hay que ver que sea mayor que sqt_living
dim(datos[datos$sqft_lot > datos$sqft_living,])
# 20824 cumplen esta condiciones, lo que quiere decir que en teoria 789 datos estan mal

# floors-CATEGORICA
table(datos$floors)
#https://www.houseplans.net/floorplans/?sqft_min=&sqft_max=&submit=Search&plan_name=&levels%5B%5D=2.5&min_width=&max_width=&min_depth=&max_depth=
# hemos deducido que el entero se refiere a el numero de plantas y si tiene 0.5 quiere decir
# que tiene una planta más pero no todos los metros están construidos ("Buhardilla")
class(datos$floors) # numeric, lo pasamos a factor
datos$floors <- as.factor(datos$floors)
class(datos$floors)

# waterfront- vistas al agua-CATEGORICA, valores 0, 1
#HAY QUE VER QUE ES CERO Y QUE ES UNO
#transformo la variable waterfront en factor
datos$waterfront<-as.factor(datos$waterfront)
str(datos$waterfront)
class(datos$waterfront)
table(datos$waterfront)

# view-como de buenas son las vistas-CATEGORICA
table(datos$view)
class(datos$view)# es numeric, lo pasamos a factor
datos$view <- as.factor(datos$view)

# condition-condiciones del apartamento-CATEGORICA
table(datos$condition)
class(datos$view)

# grade-grados de calidad en la cosntruccion-CATEGORICA
#indices: 1-3:construccion mala, 7:nivel medio, 11-13: nivel alto
table(datos$grade)
# volver a categorizar
datos$grade_categ <- cut(datos$grade, breaks = c(1,4,9,13), labels = c(0,1,2))
head(datos)
# 0 -> contruccion mala
# 1 -> nivel medio
# 2 -> nivel alto

# sqft_above-pies^2 por encima del suelo (todo menos el sotano)-NUMERICA
hist(datos$sqft_above)
# se normaliza???

# yr_built: es el año en el cual la casa fue construida. (1900-2015)
table(datos$yr_built)
hist(datos$yr_built)
#tiene que ser categorica o numerica???

# yr_renovated: año en el que en la casa se produjo una ultima renovación,
#hay un año = 0 que hay que ver que significa, puede significar que no 
#ha tenido ninguna renovacion la casa, o mirar el año de construcción.
table(datos$yr_renovated)
hist(datos$yr_renovated)


# zipcode -  Código de area, Son códigos postales de Seattle. Variable categórica
table(datos$zipcode)
hist(datos$zipcode)
class(datos$zipcode) #en entero, lo pasamos a factor

#sqft_basement- metros cuadrados del sótano. variable numérica
# 1 PIE son 0.3048 METROS
#casi seguro que habrá que transformarla.
hist(datos$sqft_basement)
datos$log_basement <- log10(datos$sqft_basement)
datos %>%  select(log_basement) %>% 
  na.omit() %>%
  ggplot(aes(x= log_basement))+
  geom_density()
# no se si esta tranformacion es la mejor...

# lat, latitud. Variable numérica
hist(datos$lat)

# long, longitud-NUMERICA
hist(datos$long)

# sqft_living15, metros cuadrados del interior de la casa respecto a los 15 vecinos
#más cercanos
hist(datos$sqft_living15)

# sqft_lot15,metros cuadrados de los terrenos respecto a los 15 vecinos
#más cercanos
hist(datos$sqft_lot15)

# grafico para variable numericas
datos %>% ggpairs(c(3:8,20,21))








