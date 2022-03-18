#*********************************
# MODELO GEOSTADISTICO           #
# Índice de biomasa relativa     #
# Muestreo aleatorio             #
#*********************************
# Alba Fuster-Alonso             #
#*********************************

# Cargar datos ---------------------------
load("./A-Simulacion/datos_simulados.RData")

# Paquetes ---------------------------
library(INLA)
library(raster)

# Modificar datos
data_random_final <- data_random_final[, -1] # Quitar la biomasa simulada

# Mesh ---------------------------
loc <- cbind(data_random_final$xcoord, data_random_final$ycoord) # Localizaciones

bound <- inla.nonconvex.hull(loc) # Limite

mesh <- inla.mesh.2d(
  boundary = bound, # Limite
  max.edge = c(0.63, 2.5), # Parametros del mesh
  cutoff = 0.05
)

# Banco de datos prediccion ---------------------------
data_bat <- data.frame(x = loc_xy[, 1], y = loc_xy[, 2], z = variables$x_bat1)

ras_bat <- rasterFromXYZ(data_bat) # Convertir batimetria en raster

bat_pred <- extract(ras_bat, mesh$loc[, c(1, 2)]) # Extraer valores del mesh

data_pred <- data.frame(batimetria = bat_pred, xcoord = mesh$loc[, 1], ycoord = mesh$loc[, 2]) # Banco de datos

data_pred_final <- rbind(
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred,
  data_pred
) # 10 anyos repetir 10 veces

data_pred_final$tiempo <- rep(1:10, each = length(data_pred$batimetria)) # Anyadir tiempo
data_pred_final$bio_relativa <- rep(NA, length(data_pred_final$batimetria)) # Anyadir biomasa

# INLA.GROUP ---------------------------
data_pred_final$batimetria[which(data_pred_final$batimetria > max(data_random_final$batimetria))] <- NA # Mismos limites pred y est
data_pred_final$batimetria[which(data_pred_final$batimetria < min(data_random_final$batimetria))] <- NA

all <- rbind(data_random_final, data_pred_final) # Juntar pred y est
igroupbath <- inla.group(all[, 1], n = 20, idx.only = TRUE) # Funcion inla.group
groupbath <- inla.group(all[, 1], n = 20) # Funcion inla.group

allin <- cbind(all, igroupbath, groupbath) # Juntar

data_random_final <- cbind(
  data_random_final, allin$groupbath[1:length(data_random_final$batimetria)],
  allin$igroupbath[1:length(data_random_final$batimetria)]
) # Anyadir columnas inla.group

data_pred_final <- cbind(
  data_pred_final, allin$groupbath[(length(data_random_final$batimetria) + 1):length(allin$batimetria)],
  allin$igroupbath[(length(data_random_final$batimetria) + 1):length(allin$batimetria)]
) # Anyadir columnas inla.group

names(data_random_final) <- c("batimetria", "xcoord", "ycoord", "bio_relativa", "tiempo", "IgBath", "Igroup")
names(data_pred_final) <- c("batimetria", "xcoord", "ycoord", "tiempo", "bio_relativa", "IgBath", "Igroup")

# SPDE ---------------------------
spde <- inla.spde2.pcmatern(
  mesh = mesh,
  prior.range = c(3, 0.5), # P(range < 3) = 0.5
  prior.sigma = c(1, 0.01)
) # P(sigma > 1) = 0.01

# Matriz de pesos (campo espacial) ---------------------------
iset <- inla.spde.make.index("i",
  n.spde = spde$n.spde,
  n.group = k
)

# Matriz de proyeccion ---------------------------
A_est <- inla.spde.make.A(
  mesh = mesh,
  loc = cbind(data_random_final$xcoord, data_random_final$ycoord), group = data_random_final$tiempo
) # Matriz de proyeccion estimacion

A_pred <- inla.spde.make.A(
  mesh = mesh,
  loc = cbind(data_pred_final$xcoord, data_pred_final$ycoo), group = data_pred_final$tiempo
) # Matriz de proyeccion prediccion

# Stack ---------------------------
## Estimacion ---------------------------
stack_est <- inla.stack(
  data = list(y = data_random_final$bio_relativa, link = 1), # Variable respuesta
  A = list(A_est, 1, 1, 1), # Efectos
  effects = list(iset,
    batimetria = data_random_final$IgBath,
    tiempo = data_random_final$tiempo,
    intercept = rep(1, length(data_random_final$bio_relativa))
  ),
  tag = "est"
)

## Prediccion ---------------------------
stack_pred <- inla.stack(
  data = list(y = data_pred_final$bio_relativa, link = 1), # Variable respuesta NA
  A = list(A_pred, 1, 1, 1), # Efectos
  effects = list(iset,
    batimetria = data_pred_final$IgBath,
    tiempo = data_pred_final$tiempo,
    intercept = rep(1, length(data_pred_final$bio_relativa))
  ),
  tag = "pred"
)

## Juntar ambos stack ---------------------------
stack <- inla.stack(stack_est, stack_pred)

# PC-prior rho ---------------------------
h_spec <- list(rho = list(prior = "pc.cor1", param = c(0, 0.9))) # P(cor > 0 = 0.9)

# Formula ---------------------------
formula <- y ~ -1 + intercept + f(batimetria, model = "rw2") + f(i,
  model = spde, group = i.group,
  control.group = list(model = "ar1", hyper = h_spec)
) + f(tiempo, model = "rw1")

# Ajustar modelo ---------------------------
modelo <- inla(formula,
  data = inla.stack.data(stack),
  family = "gamma",
  control.predictor = list(
    compute = TRUE,
    A = inla.stack.A(stack), link = 1
  ),
  verbose = TRUE,
  control.compute = list(waic = TRUE, cpo = TRUE, dic = TRUE),
  num.threads = 2
)

save.image(file = "modelo_geostadisico_aleatorio.RData")


# Prediccion ---------------------------
index.p <- inla.stack.index(stack, tag = "pred")$data

## Mediana de la distribución predictiva a posteriori ---------------------------
output_median <- exp(modelo$summary.linear.predictor[index.p, "0.5quant"])
bio_median <- output_median

prj <- inla.mesh.projector(mesh,
  xlim = range(loc_xy[, 1]),
  ylim = range(loc_xy[, 2])
)

k <- 10
m_mesh <- mesh$n

m_prj_median <- lapply(1:k, function(j) {
  r_median <- inla.mesh.project(
    prj,
    bio_median[1:m_mesh + (j - 1) * m_mesh]
  )
  return(r_median)
})

## Sd de la distribución predictiva a posteriori ---------------------------
output_sd <- exp(modelo$summary.linear.predictor[index.p, "sd"])
bio_sd <- output_sd

m_prj_sd <- lapply(1:k, function(j) {
  r_sd <- inla.mesh.project(
    prj,
    bio_sd[1:m_mesh + (j - 1) * m_mesh]
  )
  return(r_sd)
})

## q0.025 de la distribución predictiva a posteriori ---------------------------
output_25 <- exp(modelo$summary.linear.predictor[index.p, "0.025quant"])
bio_25 <- output_25

m_prj_25 <- lapply(1:k, function(j) {
  r_25 <- inla.mesh.project(
    prj,
    bio_25[1:m_mesh + (j - 1) * m_mesh]
  )
  return(r_25)
})

## q0.975 de la distribución predictiva a posteriori ---------------------------
output_975 <- exp(modelo$summary.linear.predictor[index.p, "0.975quant"])
bio_975 <- output_975

m_prj_975 <- lapply(1:k, function(j) {
  r_975 <- inla.mesh.project(
    prj,
    bio_975[1:m_mesh + (j - 1) * m_mesh]
  )
  return(r_975)
})

# Guardar modelos ---------------------------
save.image("./B-Modelos/geostadistico_aleatorio.RData")