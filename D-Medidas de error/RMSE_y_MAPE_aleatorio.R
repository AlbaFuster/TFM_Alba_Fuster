#*********************************
# MEDIDAS DE ERROR               #
# RMSE y MAPE                    #
# Indice biomasa relativa        #
# Muestro aleatorio              #
#*********************************
# Alba Fuster-Alonso             #
#*********************************

# Paquetes ---------------------------
library(Metrics)
library(MLmetrics)

# Biomasa simulada ---------------------------
biomasa_simulada <- list()
for (i in 1:k) {
  biomasa_simulada[[i]] <- median(data_variables[[i]]$biomasa)
} 

biomasa_simulada <- unlist(biomasa_simulada)

# Despejar el índice para conseguir la Biomasa ---------------------------
glm_f_bio <- glm_f_bio_r/q_random # GLM frecuentista
gam_f_bio <- gam_f_bio_r/q_random # GAM frecuentista
glm_b_bio <- glm_b_bio_r/q_random # GLM bayesiano
gam_b_bio <- gam_b_bio_r/q_random # GAM bayesiano
geo_b_bio <- geo_b_bio_r/q_random # Geostadistico

# RMSE ---------------------------
rmse(glm_f_bio, biomasa_simulada)
rmse(gam_f_bio, biomasa_simulada)
rmse(glm_b_bio, biomasa_simulada)
rmse(gam_b_bio, biomasa_simulada)
rmse(geo_b_bio, biomasa_simulada)

# MAPE ---------------------------
MAPE(glm_f_bio, biomasa_simulada)
MAPE(gam_f_bio, biomasa_simulada)
MAPE(glm_b_bio, biomasa_simulada)
MAPE(gam_b_bio, biomasa_simulada)
MAPE(geo_b_bio, biomasa_simulada)

