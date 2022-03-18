#*********************************
# MODELO PREFERENCIAL	         #
# Índice CPUE                    #
# Muestreo preferencial          #
# Patrón puntual marcado         #
#*********************************
# Alba Fuster-Alonso             #
#*********************************

# Paquetes ---------------------------
library(INLA)
library(ggplot2)
library(inlabru)
library(devtools)
library(INLAutils)
library(rgdal)
library(tidyverse)

# Modificación de datos ---------------------------
data_pref_final$CPUE <- data_pref_final$CPUE + 0.1
data_pref_final$batimetria_pp <- data_pref_final$batimetria
data = data_pref_final
spdf = SpatialPointsDataFrame(cbind(data$xcoord,data$ycoord),data[,-c(1,3,4,5,6)])

# Limite ---------------------------
poly1 <- Polygon(cbind(c(0,10,10,0,0),c(0,0,10,10,0)))
primer_poly <- Polygons(list(poly1), ID = "A")
boundary <- SpatialPolygons(list(primer_poly))

# Mesh ---------------------------
mesh <- inla.mesh.2d(
  loc.domain =  cbind(c(0,10,10,0,0),c(0,0,10,10,0)),
  max.edge = c(0.63, 2.5), 
  cutoff = 0.05
)

# Puntos de integración ---------------------------
ips <- ipoints(boundary, mesh, group = "tiempo")

# SPDE ---------------------------
spde <- inla.spde2.pcmatern(mesh,
                              prior.sigma = c(1, 0.01),
                              prior.range = c(3, 0.5)
)

# Componentes ---------------------------
cmp = ~ 
  # Efecto espacial correlado para la CPUE
  spatial(coordinates,model = spde,
                group = tiempo,
                ngroup = k,
                control.group = list(model = "ar1")) +
  # Copiar el efecto espacial correla para el PP con el parámetros de escalado (fixed = FALSE)
  spatialCopy(coordinates, 
              copy = "spatial",
              group = tiempo,
              ngroup = k,
              control.group = list(model = "ar1"),
              fixed = FALSE) +
  # Intercepto del PP
  lgcpIntercept(1) +
  # Intercepto de la CPUE
  Intercept(1) +
  # Efecto lineal batimetria CPUE
  bat(batimetria, model = "linear") +
  # Efecto lineal batimetria PP
  batPP(batimetria_pp, model = "linear") +
  # Tendencia temporal para la CPUE
  temporal(tiempo, model = "rw1") 

lik1 <- like(data = spdf,
             family = "gamma",
             formula = CPUE ~ spatial + Intercept + bat + temporal)
lik2 <- like(data = spdf,
             family = "cp",
             ips = ips,
             domain = list(coordinates = mesh),
             formula = coordinates + tiempo ~ spatialCopy + batPP + lgcpIntercept)

fit <- bru(components = cmp,
           lik1,
           lik2,
           options = list(verbose = TRUE))

# Prediccion ---------------------------
ppxl <- pixels(mesh, mask = boundary)
ppxl_all <- cprod(ppxl, data.frame(tiempo = seq_len(k)))

mark <- predict(fit, ppxl_all, ~ data.frame(tiempo = tiempo, mark = exp(spatial + Intercept + temporal + bat)))

colsc <- function(...) {
  scale_fill_gradientn(colours = viridis(30),
                       values = c(0, 0.15, 1),
                       limits = c(min(mark$median),
                                  max(mark$median)))
}

csc_c <- colsc(mark$median)

pl1 <- ggplot() +
  gg(mark, aes(fill = median)) +
  facet_wrap(~tiempo) +
  coord_equal() + csc_c

pl1

serie_tiempo_pref <- list()
for (i in 1:k) {
  serie_tiempo_pref[[i]] <- median(mark$median[mark$tiempo == i])
}

serie_tiempo_pref <- unlist(serie_tiempo_pref)

esfuerzo <- list()
for (i in 1:k) {
  esfuerzo[[i]] <- median(data_pref_final$esfuerzo[data_pref_final$tiempo ==i])
}
esfuerzo <- unlist(esfuerzo)

serie_tiempo_pref_q <- (serie_tiempo_pref * esfuerzo)/q_pref

plot(serie_tiempo_pref_q, type = "l")
lines(serie_tiempo_biomasa$biomasa)
