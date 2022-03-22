#*********************************
# MODELO PREFERENCIAL	         #
# Índice CPUE                    #
# Muestreo preferencial          #
# Patrón puntual marcado         #
#*********************************
# Alba Fuster-Alonso             #
#*********************************

# Cargar datos ---------------------------
load("./A-Simulacion/datos_simulados.RData")

# Paquetes ---------------------------
library(INLA)
library(ggplot2)
library(inlabru)
library(devtools)
library(INLAutils)
library(rgdal)
library(tidyverse)

# Modificación de datos ---------------------------
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
                              prior.range = c(4, 0.9)
)

# PC-prior rho ---------------------------
prec.prior <- list(prec = list(param = c(0.001, 0.001)))
h_spec <- list(rho = list(prior = "pc.cor1", param = c(0, 0.9))) # P(cor > 0 = 0.9)

# Componentes ---------------------------
cmp = ~ 
  # Efecto espacial correlado para la CPUE
  spatial(coordinates,model = spde,
                group = tiempo,
                ngroup = k,
                control.group = list(model = "ar1", hyper = h_spec)) +
  # Copiar el efecto espacial correla para el PP con el parámetros de escalado (fixed = FALSE)
  spatialCopy(coordinates, 
              copy = "spatial",
              group = tiempo,
              ngroup = k,
              control.group = list(model = "ar1", hyper = h_spec),
              fixed = FALSE) +
  # Intercepto del PP
  lgcpIntercept(1) +
  # Intercepto de la CPUE
  Intercept(1) +
  # Tendencia temporal para la CPUE
  temporal(tiempo, model = "rw1", hyper = prec.prior) 

lik1 <- like(data = spdf,
             family = "gamma",
             formula = CPUE ~ spatial + Intercept + temporal)
lik2 <- like(data = spdf,
             family = "cp",
             ips = ips,
             domain = list(coordinates = mesh),
             formula = coordinates + tiempo ~ spatialCopy + lgcpIntercept)

fit <- bru(components = cmp,
           lik1,
           lik2,
           options = list(verbose = TRUE))

# Prediccion ---------------------------
ppxl <- pixels(mesh, mask = boundary)
ppxl_all <- cprod(ppxl, data.frame(tiempo = seq_len(k)))

mark <- predict(fit, ppxl_all, ~ data.frame(tiempo = tiempo, mark = exp(spatial + Intercept + temporal)))

# Guardar modelos ---------------------------
save.image("./B-Modelos/geostadistico_preferencial_PP.RData")
