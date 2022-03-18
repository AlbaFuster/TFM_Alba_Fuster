#*********************************
# SERIES PREDICHAS               #
# Índice de biomasa relativa     #
# Índices CPUE                   #
#*********************************
# Alba Fuster-Alonso             #
#*********************************

# INDICES DE BIOMASA RELATIVA ---------------------------
## Frecuentista ---------------------------
### GLM ---------------------------
glm_f_bio_r <- list()
for (i in 1:k) {
  glm_f_bio_r[[i]] <- median(data_pred_glm$respuesta[data_pred_glm$tiempo == i])
}

glm_f_bio_r <- unlist(glm_f_bio_r)

### GAM ---------------------------
gam_f_bio_r <- list()
for (i in 1:k) {
  gam_f_bio_r[[i]] <- median(data_pred_gam$respuesta[data_pred_gam$tiempo == i])
}

gam_f_bio_r <- unlist(gam_f_bio_r)

## Bayesiano ---------------------------
### GLM --------------------------- 
glm_b_bio_r <- list()
for (i in 1:k) {
  glm_b_bio_r[[i]] <- median(serie_indice_bio_relativa$X50.[serie_indice_bio_relativa$tiempo == i])
}

glm_b_bio_r <- unlist(glm_b_bio_r)

### GAM ---------------------------
gam_b_bio_r <- list()
for (i in 1:k) {
  gam_b_bio_r[[i]] <- median(modelo_gam_r_R2_pred$mu[modelo_gam_r_R2_pred$tiempo == i])
}

gam_b_bio_r <- unlist(gam_b_bio_r)

### Geostadistico ---------------------------
geo_b_bio_r <- list()
for (i in 1:k) {
  geo_b_bio_r[[i]] <- median(m_prj_median[[i]])
}

geo_b_bio_r <- unlist(geo_b_bio_r)

# INDICES CPUE
## Frecuentista ---------------------------
### GLM ---------------------------
glm_f_CPUE <- list()
for (i in 1:k) {
  glm_f_CPUE[[i]] <- median(data_pred_glm$respuesta[data_pred_glm$tiempo == i])
}

glm_f_CPUE <- unlist(glm_f_CPUE)

### GAM ---------------------------
gam_f_CPUE <- list()
for (i in 1:k) {
  gam_f_CPUE[[i]] <- median(data_pred_gam$respuesta[data_pred_gam$tiempo == i])
}

gam_f_CPUE <- unlist(gam_f_CPUE)

## Bayesiano ---------------------------
### GLM --------------------------- 
glm_b_CPUE <- list()
for (i in 1:k) {
  glm_b_CPUE[[i]] <- median(serie_indice_bio_relativa$X50.[serie_indice_bio_relativa$tiempo == i])
}

glm_b_CPUE <- unlist(glm_b_CPUE)

### GAM ---------------------------
gam_b_CPUE <- list()
for (i in 1:k) {
  gam_b_CPUE[[i]] <- median(data_gam_r_R2_pred$mu[data_gam_r_R2_pred$tiempo == i])
}

gam_b_CPUE <- unlist(gam_b_CPUE)

### Geostadistico ---------------------------
geo_b_CPUE <- list()
for (i in 1:k) {
  geo_b_CPUE[[i]] <- median(m_prj_median[[i]])
}

geo_b_CPUE <- unlist(geo_b_CPUE)

### Geostadistico preferencial ---------------------------
geo_pref_b_CPUE <- list()
for (i in 1:k) {
  geo_pref_b_CPUE[[i]] <- median(mark$median[mark$tiempo == i])
}

geo_pref_b_CPUE <- unlist(geo_pref_b_CPUE)


      