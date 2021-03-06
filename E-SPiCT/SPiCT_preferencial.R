#*********************************
# SPiCT                          #
# Indice CPUE                    #
# Muestreo preferencial          #
#*********************************
# Alba Fuster-Alonso             #
#*********************************

# Paquetes ---------------------------
library(spict)
library(knitr)
library(formatR)
library(ellipse)
library(ggplot2)
library(corrplot)
library(icesAdvice)

# INPUT 1: serie de capturas ---------------------------
## Extraer la biomasa_simulada cada anyo ---------------------------
biomasa_simulada <- list()
for (i in 1:k) {
  biomasa_simulada[[i]] <- sum(data_variables[[i]]$biomasa)
}
biomasa_simulada <- unlist(biomasa_simulada)

## Fijar parametros de la curva de produccion de Schafer ---------------------------
r <- 1.4
K <- sum(biomasa_simulada)/0.5

## Aproximar las capturas ---------------------------
capturas_simuladas <- list()
for (i in 1: (k-1)) {
  capturas_simuladas[[i]] <- biomasa_simulada[i] - biomasa_simulada[(i + 1)] + (r*biomasa_simulada[i] * (1-(biomasa_simulada[i]/K)))
}

capturas_simuladas[[10]] <-  r*biomasa_simulada[10]*(1-(biomasa_simulada[10]/K)) # Ultimo anyo en equilibrio biomasa_10 = biomasa_11
capturas_simuladas <- unlist(capturas_simuladas)

## Capturas ---------------------------
C_sp_i <- data.frame(obsC = capturas_simuladas, timeC = rep(2000:2009))

# INPUT 2: serie indice CPUE ---------------------------
I_sp_i <- data.frame(ObsI = geo_pref_b_CPUE, timeI = 2000:2009)

# Juntar INPUTS en una lista ---------------------------
inp <- list(
  timeC = C_sp_i$timeC, # Anyos capturas
  obsC = C_sp_i$obsC, # Serie de capturas
  timeI = list(I_sp_i$timeI), # Anyos indices
  obsI = list(I_sp_i$ObsI) # Indice CPUE
)

inp <- check.inp(inp)
inp$dtc
inp$dteuler <- 1/16
inp$ini$logn <- log(2) # Fijar p (parametro asimetria) para que la curva de produccion sea simetrica (Schafer)
inp$phases$logn <- -1

# Ajustar SPiCT ---------------------------
res <- fit.spict(inp)

# Diagnostico de calidad ---------------------------
residuos <- calc.osa.resid(res)