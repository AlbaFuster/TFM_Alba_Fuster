# Protocolo para evaluar la modelización de los índices de biomasa relativa y CPUE

## Introducción 
<p align="justify">
En este repositorio se introduce un ejemplo a la simulación de un modelo espacio-temporal donde la variable simulada es la biomasa real de una especie de interés pesquero. La modelización de la variable biomasa será con una distribución Gamma cuya media irá enlazada a un predictor de lineal con el logaritmo. Dicho predictor lineal incluirá un intercepto, una covariable que represente la batimetría, una efecto espacial correlado y una tendencia temporal. Una vez simulada la biomasa se seleccionaran una serie de puntos de dos formas distintas: (1) muestreo aleatorio y (2) muestreo preferencial. A continuación, con los puntos seleccionados se obtendrán las variables respuestas para ajustar los modelos: (II) biomasa relativa para campañas oceanográficas y (II) capturas por unidad de esfuerzo (CPUE) para la actividad pesquera. Para ambas variables se plantean distintas modelizaciones.

La simulación del modelo se ha automatizado de manera que únicamente se han de fijar una serie de parámetros la biomasa simulada en el espacio-tiempo y los bancos de datos con los índices de biomasa relativa derivados de campañas oceanográficas (muestreo independiente de la pesca) y lo índices CPUE derivados de la actividad pesquera (muestreo dependiente de la pesca). A continuación, procedemos a inferir y predecir los parámetros de distintas modelizaciones desde la perspectiva frecuentista y bayesiana.
  
En resumen, este repositorio está compuesto por tres carpetas principales: (1) simulacion, donde se han elaborado una serie de funciones para automatizar la simulación de un escenario real de biomasa y muestreo, (2) ajuste de modelos, se trata de una serie de scripts con el ajuste y la predicción de GLMs, GAMs, modelos geostadísticos y un modelo de patrón puntual marcado o preferencial, (3) una última carpeta contiune el código para ajustar los modelos SPiCT de producción excedentaria (evaluación del stock pesquero). 
</p>

## Requisitos 

<p align="justify">
Para el correcto funcionamiento del repositorio es necesario tener instalado R-INLA, inlabru, SPiCT y los siguientes paquetes.

1. Instalar R-INLA (en caso de problemas con la instalación acceder a [r-inla.org](https://www.r-inla.org/)):

```
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)
```
  
2. Instalar inlabru (en caso de problemas con la instalación acceder a [inlabru](https://sites.google.com/inlabru.org/inlabru)):

```
remotes::install_github("inlabru-org/inlabru")
```
  
3. Instalar SPiCT (en caso de problemas con la instalación acceder a [SPiCT](https://github.com/DTUAqua/spict)):

```
install.packages("TMB", type="source")

library(remotes)
install_github("DTUAqua/spict/spict")  
```

4. Instalar paquetes:

```  
install. packages(c("lattice","gridExtra", "RColorBrewer", "raster", "fields", "reshape", "ggplot2", "mgcv", "R2BayesX", "tidyverse", "INLAutils", "devtools", "rgdal", "Metrics", "MLmetrics", "knitr", "formatR", "ellipse", "corrplot2", "icesAdvice")) 
```
  
5. Parámetros a fijar: la elección de estos parámetros depende del usuario

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
