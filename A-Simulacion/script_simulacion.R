#*********************************
# SIMULACION ESPACIO-TEMPORAL    #
# Biomasa y muestreos            #
#*********************************
# Alba Fuster-Alonso             #
#*********************************

# Paquetes ---------------------------
library(INLA)
library(fields)
library(lattice)
library(reshape)
library(gridExtra)
library(RColorBrewer)
library(raster)
library(ggplot2)

# Directorio ---------------------------
getwd()

# Funcion INLA termino espacio-temporal ---------------------------
source("./simulacion/funcion.R")

# Parametros a fijar ---------------------------
coord1 <- 0 # Coordenadas a fijar
coord2 <- 10 # Coordenadas a fijar
coord3 <- 0 # Coordenadas a fijar
coord4 <- 10 # Coordenadas a fijar
variance <- 1 # Varianza del efecto espacial
kappa <- 0.5 # El rango = sqrt(8)/kappa
rho <- 0.9 # Rho es la correlacion temporal
k <- 10 # k indica el numero de anyos
beta_0 <- 3 # Valor del intercepto
beta_1 <- -2.5 # Valor del termino de grado uno de la batimetria
beta_2 <- -1.5 # Valor del termino de grado dos de la batimetria
vector_tiempo <- c(1, # año y el valor del parámetro alpha
                   1.5,
                   1.6,
                   1.8,
                   2.2,
                   2.4,
                   2.8,
                   2.5,
                   2.2,
		   2) # vector para la tendencia en el tiempo
bg <- 1 # valor de phi (dispersion) en una distribución Gamma
m <- 100 # El numero de puntos que quieras muestrear
q_random <- 0.3 # La proporcion que extraemos de la biomasa total
q_pref <- 0.6 # La prorpocion que extraemos de la biomasa total

# Campo de estudio ---------------------------
campo_estudio <- function(coord1, coord2, coord3, coord4) {
  xy <- expand.grid(
    seq(coord1, coord2, length.out = (100)),
    seq(coord3, coord4, length.out = (100))
  )
  x <- as.numeric(xy[, 1])
  y <- as.numeric(xy[, 2])
  loc_xy <- cbind(x, y)

  return(loc_xy)
}

loc_xy <- campo_estudio(coord1, coord2, coord3, coord4)

# Generacion de variables explicativas ---------------------------
simulacion_variables <- function(variance, kappa, rho, k) {

  # Generacion del Mesh
  prmesh1 <- inla.mesh.2d(loc = loc_xy, max.edge = c(0.4, 1))

  # Marcar parametro funcion INLA
  params <- c(variance = variance, kappa = kappa)

  # Aplicar funcion INLA
  x_k <- book.rspde(loc_xy,
    range = sqrt(8) / params[2],
    sigma = sqrt(params[1]), n = k, mesh = prmesh1,
    return.attributes = TRUE
  )

  # Anyadir correlacion temporal
  x <- x_k
  for (j in 2:k) {
    x[, j] <- rho * x[, j - 1] + sqrt(1 - rho^2) * x_k[, j]
  }

  # Funcion para generar la variable batimetria
  batimetria <- function(x, y) {
    x_bat1 <- log(x * y + 1) * 100
    return(x_bat1)
  }

  # Aplicar funcion para batimetria
  x_bat1 <- batimetria(loc_xy[, 1], loc_xy[, 2])

  # Escalar la covariable batimetria
  x_bat <- scale(x_bat1)
  
  # Generar data.frame con los terminos que formaran parte del predictor lineal
  variables <- data.frame(x = x, x_bat1 = x_bat1, x_bat = x_bat)

  # Cambiar nombres del banco de datos variables
  names(variables) <- c(
    "x1", "x2", "x3", "x4","x5", "x6", "x7", "x8", "x9", "x10", "x_bat1", "x_bat"
  ) # Este comando se
  # debe de modificar en el caso de que k sea un numero de anyos distinto a 10

  return(variables)
}

variables <- simulacion_variables(variance, kappa, rho, k)

# Simulacion variables respuesta ---------------------------
simulacion_respuesta <- function(beta_0, beta_1, beta_2, bg, n) {

  # Predictor lineal de la media de la distribucion
  lin_pred_mu <- list()
  for (i in 1:k) {
    lin_pred_mu[[i]] <- exp(beta_0 + beta_1 * variables$x_bat + beta_2 * (variables$x_bat)^2 + variables[, i] + vector_tiempo[i])
  }

  # Parametro alpha de la distribucion gamma
  alpha <- list()
  for (i in 1:k) {
    alpha[[i]] <- (lin_pred_mu[[i]]) * bg
  }


  # Simulacion de la biomasa con una distribucion gamma
  biomasa_real <- list()
  for (i in 1:k) {
    biomasa_real[[i]] <- rgamma(n, alpha[[i]], bg)
  }

  # Bancos de datos con la biomasa, la covariable y las coordenadas para cada anyo
  data_variables <- list()
  for (i in 1:k) {
    data_variables[[i]] <- data.frame(
      biomasa = biomasa_real[[i]], batimetria = variables$x_bat1, xcoord = loc_xy[, 1],
      ycoord = loc_xy[, 2]
    )
  }
  
  return(data_variables)
}

n <- as.numeric(dim(variables)[1]) # numero de filas del banco de datos en un anyo

data_variables <- simulacion_respuesta(beta_0, beta_1, beta_2, bg, n)

# Sumar 0.1 a la biomasa (facilita la convergencia en los modelos)
for (i in 1:k) {
  data_variables$biomasa[[i]] <- data_variables$biomasa[[i]] + 0.1
}

# Muestreo ---------------------------
## Muestreo aleatorio ---------------------------
muestreo_random <- function(m, q_random) {

  # Selecionamos 200 puntos de la totalidad simulada
  data_random <- list()
  for (i in 1:k) {
    isel <- sample(1:n, m)
    data_random[[i]] <- data_variables[[i]][isel, ]
  }

  # Obtenemos biomasa relativa
  for (i in 1:k) {
    data_random[[i]]$bio_relativa <- data_random[[i]]$biomasa * q_random
  }

  # Banco de datos final
  data_random_final <- data.frame()

  for (i in 1:k) {
    data_random_final <- rbind(data_random_final, data_random[[i]])
  }

  # Anyadir el tiempo (10 anyos)
  data_random_final$tiempo <- rep(1:k, each = m)

  return(data_random_final)
}

data_random_final <- muestreo_random(m, q_random)

## Muestreo preferencial ---------------------------
muestreo_preferencial <- function(m, q_pref) {

  # Transformar la variable respuesta biomasa a la escala de 0 a 1
  biomasa_trans <- list()
  for (i in 1:k) {
    biomasa_trans[[i]] <- (data_variables[[i]]$biomasa - min(data_variables[[i]]$biomasa)) / (max(data_variables[[i]]$biomasa) - min(data_variables[[i]]$biomasa))
    #biomasa_trans[[i]] <- ifelse(a < 0.1, 0, a)
  }

  # Seleccionar puntos de muestreo con prob (mas biomasa mas probable selecionar el punto)
  isel_pre <- list()
  for (i in 1:k) {
    isel_pre[[i]] <- sample(1:n, m, prob = biomasa_trans[[i]])
  }

  data_pref <- list()
  for (i in 1:k) {
    data_pref[[i]] <- data_variables[[i]][isel_pre[[i]], ]
  }

  # Generar banco de datos final para un muestreo preferencial
  data_pref_final <- data.frame()

  for (i in 1:k) {
    data_pref_final <- rbind(data_pref_final, data_pref[[i]])
  }
  
  # Generar la variable esfuerzo de pesca (tiempo pescado)
  esfuerzo <- rnorm(m * k, 40, 7)
  
  # id para que sepamos el orden original 
  id <- 1:length(data_pref_final$biomasa)

  data_pref_final$id <- id

  # Ordenar los valores de biomasa de menor a mayor
  data_pref_final1 <- data_pref_final[order(data_pref_final$biomasa), ]
  
  # Ordenar los valores de esfuerzo de menos a mayor
  data_pref_final1$esfuerzo <- sort(esfuerzo)
  
  # Voler a ordenar por el id 
  data_pref_final <- data_pref_final1[order(data_pref_final1$id),]

  # Eliminar id
  data_pref_final$id <- NULL
  
  # Capturas
  data_pref_final$biomasa_r <- data_pref_final$biomasa * q_pref

  # Generar capturas por unidad de esfuerzo CPUE = biomasa * q / esfuerzo
  data_pref_final$CPUE <- data_pref_final$biomasa_r / data_pref_final$esfuerzo

  # Anyadir el tiempo
  data_pref_final$tiempo <- rep(1:k, each = m)

  return(data_pref_final)
  return(biomasa_trans)
}

data_pref_final <- muestreo_preferencial(m, q_pref)



# RData --------------------------- 
save.image(file = "./simulacion/simulacion_biomasa_prueba.RData")
