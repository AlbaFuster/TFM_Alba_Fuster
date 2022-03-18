# Protocolo para evaluar la modelización de los índices de biomasa relativa y CPUE

## Introducción 
<p align="justify">
En este repositorio se introduce un ejemplo a la simulación de un modelo espacio-temporal donde la variable simulada es la biomasa real de una especie de interés pesquero. La modelización de la variable biomasa será con una distribución Gamma cuya media irá enlazada a un predictor de lineal con el logaritmo. Dicho predictor lineal incluirá un intercepto, una covariable que represente la batimetría y una efecto espacio-temporal. Una vez simulada la biomasa se seleccionaran una serie de puntos de dos formas distintas: (1) muestreo aleatorio y (2) muestreo preferencial. A continuación, con los puntos seleccionados se obtendrán las variables respuestas para ajustar los modelos: (II) biomasa relativa para campañas y (II) capturas por unidad de esfuerzo para las pesquerías.  Ambas variables serán ajustados de forma diferente. La simulación del modelo se ha automatizado de manera que únicamente se han de fijar una serie de parámetros y tendremos un report con los bancos de datos que se han generado con la simulación. A continuación se procede a ajustar un modelo espacio temporal del que obtendremos un serie de biomasa relativa a través de la predicción. 
  
En resumen, este repositorio está compuesto por tres carpetas principales: (1) simulacion, donde se han elaborado una serie de funciones para automatizar la simulación de un escenario real de biomasa y muestreo, (2) ajuste de un modelo geostadístico, se trata de un script con el ajuste de un modelo espacio-temporal así como la predicción de la biomasa en todo el campo, (3) una última carpeta contiune los inputs necesario para ajustar un modelo de stock assesment. 
</p>

## Requisitos 

<p align="justify">
Para el correcto funcionamiento del repositorio es necesario tener instalado R-INLA y los siguientes paquetes.

1. Instalar R-INLA (en caso de problemas con la instalación acceder a [r-inla.org](https://www.r-inla.org/)):

```
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)
```

2. Instalar paquetes:

```  
install. packages(c("lattice","gridExtra", "RColorBrewer", "raster", "fields", "reshape", "ggplot2")) 
```
  
3. Parámetros a fijar: la elección de estos parámetros depende del usuario

```
coord1 <- 0 # Coordenadas a fijar
coord2 <- 10 # Coordenadas a fijar
coord3 <- 0 # Coordenadas a fijar
coord4 <- 10 # Coordenadas a fijar
variance <- 1 # Varianza del efecto espacial
kappa <- 1.2 # El rango = sqrt(8)/kappa
rho <- 0.9 # Rho es la correlacion temporal
k <- 10 # k indica el numero de anyos
beta_0 <- 5.5 # Valor del intercepto
beta_1 <- -1.5 # Valor del termino de grado uno de la batimetria
beta_2 <- -1 # Valor del termino de grado dos de la batimetria
phi <- 1 # valor de phi (dispersion)
m <- 200 # El numero de puntos que quieras muestrear
q_random <- 0.3 # La proporcion que extraemos de la biomasa total
q_pref <- 0.6 # La prorpocion que extraemos de la biomasa total
```
</p>
