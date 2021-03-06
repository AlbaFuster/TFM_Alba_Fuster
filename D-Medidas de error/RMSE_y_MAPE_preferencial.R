#*********************************
# MEDIDAS DE ERROR               #
# RMSE y MAPE                    #
# Indice CPUE                    #
# Muestro preferencial           #
#*********************************
# Alba Fuster-Alonso             #
#*********************************

# Paquetes ---------------------------
library(Metrics)
library(MLmetrics)

# Mediana del esfuerzo para cada anyo ---------------------------
esfuerzo_por_anyo <- list()
for (i in 1:k) {
  esfuerzo_por_anyo[[i]] <- median(data_pref_final$esfuerzo[data_pref_final$tiempo == i])
}
esfuerzo_por_anyo <- unlist(esfuerzo_por_anyo)

# Despejar el índice CPUE para conseguir la Biomasa ---------------------------
glm_f_bio <- (glm_f_CPUE * esfuerzo_por_anyo)/q_pref # GLM frecuentista
gam_f_bio <- (gam_f_CPUE * esfuerzo_por_anyo)/q_pref # GAM frecuentista
glm_b_bio <- (glm_b_CPUE * esfuerzo_por_anyo)/q_pref # GLM bayesiano
gam_b_bio <- (gam_b_CPUE * esfuerzo_por_anyo)/q_pref # GAM bayesiano
geo_b_bio <- (geo_b_CPUE * esfuerzo_por_anyo)/q_pref # Geostadistico
geo_pref_b_bio <- (geo_pref_b_CPUE * esfuerzo_por_anyo)/q_pref # Patron puntual marcado

# RMSE ---------------------------
rmse(glm_f_bio, biomasa_simulada)
rmse(gam_f_bio, biomasa_simulada)
rmse(glm_b_bio, biomasa_simulada)
rmse(gam_b_bio, biomasa_simulada)
rmse(geo_b_bio, biomasa_simulada)
rmse(geo_pref_b_bio, biomasa_simulada)

# MAPE ---------------------------
MAPE(glm_f_bio, biomasa_simulada)
MAPE(gam_f_bio, biomasa_simulada)
MAPE(glm_b_bio, biomasa_simulada)
MAPE(gam_b_bio, biomasa_simulada)
MAPE(geo_b_bio, biomasa_simulada)
MAPE(geo_pref_b_bio, biomasa_simulada)
