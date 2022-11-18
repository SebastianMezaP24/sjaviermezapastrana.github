########## Encuesta de internet móvil - CRC ####################
##########           2021             ##########

# Limpiar el ambiente de trabajo
rm(list = ls())

# cargando paquetes a utilizar 
library(tidyverse)
library(readxl)
library(survey)
library(dplyr)

# Definiendo el directorio para la base completa en dta
setwd("C://Users/sebas/OneDrive/Alex/Internet/CRC")

# importando base de datos 
EIM_2021 <- read_xlsx("C:\\Users\\sebas\\OneDrive\\Alex\\Internet\\CRC\\Internet_movil.xlsx", sheet = "CC128403_BASE_INTERNET_MOVIL_ET")

# seleccionando variables a utilizar

EIM_2021_def <- EIM_2021[c(2, 4, 9, 11:14, 16:22, 33:37, 79:81, 84:87, 89:96, 107:108, 110:113, 115:118, 120:123,
                           125:128, 130:133, 135:136, 138:143, 148:153, 179:190, 398:399)]

# conviertiendo variables
EIM_2021_def$GENERO <- factor(EIM_2021_def$GENERO)

EIM_2021_def$ESTRATO <- factor(EIM_2021_def$ESTRATO)
EIM_2021_def$REDAD <- factor(EIM_2021_def$REDAD)


# transformando el data frame a enucesta
BD_survey <- svydesign(ids=~1, data=EIM_2021_def, weights=EIM_2021_def$FACTOR)
class(BD_survey)

# Guardar base de indicadores
saveRDS(BD_survey, "BD_survey_indicadores.rds")

#############################################################################################################################################
# Total personas 
Total_personas <- data.frame(svytable(~FACTOR + REGISTRO,
                                      design = BD_survey))
Total_personas_nacional <- sum(Total_personas$Freq)

#############################################################################################################################################
# ¿En qué ciudad vive permanentemente?
P2_ind <- data.frame(svytable(~FACTOR + P2,
                              design = BD_survey))
# Bogotá D.C
P2_1 <- P2_ind %>% filter(P2_ind$P2 == "Bogotá D.C")  # 1.429.751
P2_1 <- sum(P2_1$Freq)
# Medellín
P2_2 <- P2_ind %>% filter(P2_ind$P2 == "Medellín")  # 539.416
P2_2 <- sum(P2_2$Freq)
# Cali
P2_3 <- P2_ind %>% filter(P2_ind$P2 == "Cali")  # 268.538
P2_3 <- sum(P2_3$Freq)
# Barranquilla
P2_4 <- P2_ind %>% filter(P2_ind$P2 == "Barranquilla")  # 237.777
P2_4 <- sum(P2_4$Freq)
# Cartagena
P2_5 <- P2_ind %>% filter(P2_ind$P2 == "Cartagena") # 179.272
P2_5 <- sum(P2_5$Freq)
# Cúcuta
P2_6 <- P2_ind %>% filter(P2_ind$P2 == "Cúcuta") # 144.895
P2_6 <- sum(P2_6$Freq)
# Bucaramanga
P2_7 <- P2_ind %>% filter(P2_ind$P2 == "Bucaramanga") # 158.357
P2_7 <- sum(P2_7$Freq)
# Pereira
P2_8 <- P2_ind %>% filter(P2_ind$P2 == "Pereira") # 96.179
P2_8 <- sum(P2_8$Freq)
# Pasto
P2_9 <- P2_ind %>% filter(P2_ind$P2 == "Pasto")  # 114.068
P2_9 <- sum(P2_9$Freq)
# Manizales
P2_10 <- P2_ind %>% filter(P2_ind$P2 == "Manizales")  # 92.765
P2_10 <- sum(P2_10$Freq)
# Ibagué
P2_11 <- P2_ind %>% filter(P2_ind$P2 == "Ibagué")  # 136.757
P2_11 <- sum(P2_11$Freq)
# Villavicencio
P2_12 <- P2_ind %>% filter(P2_ind$P2 == "Villavicencio")  # 166.527
P2_12 <- sum(P2_12$Freq)
# Montería
P2_13 <- P2_ind %>% filter(P2_ind$P2 == "Montería")  # 74.844
P2_13 <- sum(P2_13$Freq)
# Quibdó
P2_14 <- P2_ind %>% filter(P2_ind$P2 == "Quibdó")  # 22.862
P2_14 <- sum(P2_14$Freq)
# Leticia
P2_15 <- P2_ind %>% filter(P2_ind$P2 == "Leticia")  # 16.509
P2_15 <- sum(P2_15$Freq)
# San Andrés
P2_16 <- P2_ind %>% filter(P2_ind$P2 == "San Andrés") # 10.541
P2_16 <- sum(P2_16$Freq)
# Arauca
P2_17 <- P2_ind %>% filter(P2_ind$P2 == "Arauca")  # 15.805
P2_17 <- sum(P2_17$Freq)
# Florencia
P2_18 <- P2_ind %>% filter(P2_ind$P2 == "Florencia") # 31.811
P2_18 <- sum(P2_18$Freq)
#Yopal
P2_19 <- P2_ind %>% filter(P2_ind$P2 == "Yopal") # 28.919
P2_19 <- sum(P2_19$Freq)
# Otra
P2_20 <- P2_ind %>% filter(P2_ind$P2 == "Otra")
P2_20 <- sum(P2_20$Freq)

# Total P2
P2_nacional <- P2_1 + P2_2 + P2_3 + P2_4 + P2_5 + P2_6 + P2_7 + P2_8 +   # 3.765.603
  P2_9 + P2_10 + P2_11 + P2_12 + P2_13 + P2_14 + P2_15 + P2_16 +
  P2_17 + P2_18 + P2_19 + P2_20

#########################################################################################################################################
# Estrato socieconómico
ESTRATO_ind <- data.frame(svytable(~FACTOR + ESTRATO,
                              design = BD_survey))

# Sin estrato
Sin_estrato_1 <- ESTRATO_ind %>% filter(ESTRATO_ind$ESTRATO == "Sin estrato") # 0
Sin_estrato_1 <- sum(Sin_estrato_1$Freq)
# Estrato 1
Estrato_1 <- ESTRATO_ind %>% filter(ESTRATO_ind$ESTRATO == "Estrato 1")
Estrato_1 <- sum(Estrato_1$Freq)
# Estrato 2
Estrato_2 <- ESTRATO_ind %>% filter(ESTRATO_ind$ESTRATO == "Estrato 2")
Estrato_2 <- sum(Estrato_2$Freq)
# Estrato 3
Estrato_3 <- ESTRATO_ind %>% filter(ESTRATO_ind$ESTRATO == "Estrato 3")
Estrato_3 <- sum(Estrato_3$Freq)
# Estrato 4
Estrato_4 <- ESTRATO_ind %>% filter(ESTRATO_ind$ESTRATO == "Estrato 4")
Estrato_4 <- sum(Estrato_4$Freq)
# Estrato 5
Estrato_5 <- ESTRATO_ind %>% filter(ESTRATO_ind$ESTRATO == "Estrato 5")
Estrato_5 <- sum(Estrato_5$Freq)
# Estrato 6
Estrato_6 <- ESTRATO_ind %>% filter(ESTRATO_ind$ESTRATO == "Estrato 6")
Estrato_6 <- sum(Estrato_6$Freq)
# No responde
No_responde_998_ <- ESTRATO_ind %>% filter(ESTRATO_ind$ESTRATO == "No responde")
No_responde_998_ <- sum(No_responde_998_$Freq)
# No sabe
No_sabe_999_ <- ESTRATO_ind %>% filter(ESTRATO_ind$ESTRATO == "No sabe")
No_sabe_999_ <- sum(No_sabe_999_$Freq)

Total_estrato_nacional <- Sin_estrato_1 + Estrato_1 + Estrato_2 + Estrato_3 +
  Estrato_4 + Estrato_5 + Estrato_6 
###############################################################################################################################################################
# GENERO
GENERO_ind <- data.frame(svytable(~FACTOR + GENERO,
                                   design = BD_survey))

# Femenino
Femenino_nacional <- GENERO_ind %>% filter(GENERO_ind$GENERO == "Femenino")
Femenino_nacional <- sum(Femenino_nacional$Freq)
# Masculino
Masculino_nacional <- GENERO_ind %>% filter(GENERO_ind$GENERO == "Masculino")
Masculino_nacional <- sum(Masculino_nacional$Freq)

# Total GENERO nacional
Total_genero_nacional <- Femenino_nacional + Masculino_nacional

########################################################################################################################################################
# Rangos de edad
REDAD_ind <- data.frame(svytable(~FACTOR + REDAD,
                                 design = BD_survey))
# Menos de 18 años
Menos_de_18_años <- REDAD_ind %>% filter(REDAD_ind$REDAD == "Menos de 18 años")
Menos_de_18_años <- sum(Menos_de_18_años$Freq)

# Entre 18 y 24 años
Entre_18_y_24 <- REDAD_ind %>% filter(REDAD_ind$REDAD == "Entre 25 y 34 años")
Entre_18_y_24 <- sum(Entre_18_y_24$Freq)

# Entre 25 y 34 años
Entre_25_y_34 <- REDAD_ind %>% filter(REDAD_ind$REDAD == "Entre 25 y 34 años")
Entre_25_y_34 <- sum(Entre_25_y_34$Freq)

# Entre 35 y 44 años
Entre_35_y_44 <- REDAD_ind %>% filter(REDAD_ind$REDAD == "Entre 35 y 44 años")
Entre_35_y_44 <- sum(Entre_35_y_44$Freq)

# # Entre 45 y 54 años
Entre_45_y_54 <- REDAD_ind %>% filter(REDAD_ind$REDAD == "Entre 45 y 54 años")
Entre_45_y_54 <- sum(Entre_45_y_54$Freq)

# Entre 55 y 80 años
Entre_55_y_80 <- REDAD_ind %>% filter(REDAD_ind$REDAD == "55 y 80 años")
Entre_55_y_80 <- sum(Entre_55_y_80$Freq)

# Mayores de 80 años
Mayores_de_80 <- REDAD_ind %>% filter(REDAD_ind$REDAD == "Mayores de 80 años")
Mayores_de_80 <- sum(Mayores_de_80$Freq)
#################################################################################################################################################
# Servicios con los que cuenta de manera permanente en el hogar
P4_1_ind <- data.frame(svytable(~FACTOR + P4_1,
                                 design = BD_survey))
# Telefonía movil - celular
Tele_movi_celul <- P4_1_ind %>% filter(P4_1_ind$P4_1 == "Telefonía  móvil / celular")
Tele_movi_celul <- sum(Tele_movi_celul$Freq)
# Telefonía fija en casa - hogar
Tele_fija_hogar <- P4_1_ind %>% filter(P4_1_ind$P4_1 == "Telefonía fija en casa/hogar")
Tele_fija_hogar <- sum(Tele_fija_hogar$Freq)
# Internet móvil- datos en su celular
Inter_mov_celul <- P4_1_ind %>% filter(P4_1_ind$P4_1 == "Internet móvil- datos en su celular")
Inter_mov_celul <- sum(Inter_mov_celul$Freq)
# Internet fijo en su casa/hogar
Inter_fijo_hogar <- P4_1_ind %>% filter(P4_1_ind$P4_1 == "Internet fijo en su casa/hogar")
Inter_fijo_hogar <- sum(Inter_fijo_hogar$Freq)
# Televisión por suscripción- Cable
Telev_suscrip_cable <- P4_1_ind %>% filter(P4_1_ind$P4_1 == "Televisión por suscripción- Cable")
Telev_suscrip_cable <- sum(Telev_suscrip_cable$Freq)
# Televisión Abierta – No es por suscripción y es gratuita
Telev_abier_gratui <- P4_1_ind %>% filter(P4_1_ind$P4_1 == "Televisión Abierta – No es por suscripción y es gratuita")
Telev_abier_gratui <- sum(Telev_abier_gratui$Freq)


# Servicios con los que cuenta de manera permanente en el hogar 2
P4_2_ind <- data.frame(svytable(~FACTOR + P4_2,
                                design = BD_survey))
# Telefonía movil - celular
Tele_movi_celul_2 <- P4_2_ind %>% filter(P4_2_ind$P4_2 == "Telefonía  móvil / celular")
Tele_movi_celul_2 <- sum(Tele_movi_celul_2$Freq)
# Telefonía fija en casa - hogar
Tele_fija_hogar_2 <- P4_2_ind %>% filter(P4_2_ind$P4_2 == "Telefonía fija en casa/hogar")
Tele_fija_hogar_2 <- sum(Tele_fija_hogar_2$Freq)
# Internet móvil- datos en su celular
Inter_mov_celul_2 <- P4_2_ind %>% filter(P4_2_ind$P4_2 == "Internet móvil- datos en su celular")
Inter_mov_celul_2 <- sum(Inter_mov_celul_2$Freq)
# Internet fijo en su casa/hogar
Inter_fijo_hogar_2 <- P4_2_ind %>% filter(P4_2_ind$P4_2 == "Internet fijo en su casa/hogar")
Inter_fijo_hogar_2 <- sum(Inter_fijo_hogar_2$Freq)
# Televisión por suscripción- Cable
Telev_suscrip_cable_2 <- P4_2_ind %>% filter(P4_2_ind$P4_2 == "Televisión por suscripción- Cable")
Telev_suscrip_cable_2 <- sum(Telev_suscrip_cable_2$Freq)
# Televisión Abierta – No es por suscripción y es gratuita
Telev_abier_gratui_2 <- P4_2_ind %>% filter(P4_2_ind$P4_2 == "Televisión Abierta – No es por suscripción y es gratuita")
Telev_abier_gratui_2 <- sum(Telev_abier_gratui_2$Freq)

# Servicios con los que cuenta de manera permanente en el hogar 3
P4_3_ind <- data.frame(svytable(~FACTOR + P4_3,
                                design = BD_survey))
# Telefonía movil - celular
Tele_movi_celul_3 <- P4_3_ind %>% filter(P4_3_ind$P4_3 == "Telefonía  móvil / celular")
Tele_movi_celul_3 <- sum(Tele_movi_celul_3$Freq)
# Telefonía fija en casa - hogar
Tele_fija_hogar_3 <- P4_3_ind %>% filter(P4_3_ind$P4_3 == "Telefonía fija en casa/hogar")
Tele_fija_hogar_3 <- sum(Tele_fija_hogar_3$Freq)
# Internet móvil- datos en su celular
Inter_mov_celul_3 <- P4_3_ind %>% filter(P4_3_ind$P4_3 == "Internet móvil- datos en su celular")
Inter_mov_celul_3 <- sum(Inter_mov_celul_3$Freq)
# Internet fijo en su casa/hogar
Inter_fijo_hogar_3 <- P4_3_ind %>% filter(P4_3_ind$P4_3 == "Internet fijo en su casa/hogar")
Inter_fijo_hogar_3 <- sum(Inter_fijo_hogar_3$Freq)
# Televisión por suscripción- Cable
Telev_suscrip_cable_3 <- P4_3_ind %>% filter(P4_3_ind$P4_3 == "Televisión por suscripción- Cable")
Telev_suscrip_cable_3 <- sum(Telev_suscrip_cable_3$Freq)
# Televisión Abierta – No es por suscripción y es gratuita
Telev_abier_gratui_3 <- P4_3_ind %>% filter(P4_3_ind$P4_3 == "Televisión Abierta – No es por suscripción y es gratuita")
Telev_abier_gratui_3 <- sum(Telev_abier_gratui_3$Freq)


# Servicios con los que cuenta de manera permanente en el hogar 4
P4_4_ind <- data.frame(svytable(~FACTOR + P4_4,
                                design = BD_survey))
# Telefonía movil - celular
Tele_movi_celul_4 <- P4_4_ind %>% filter(P4_4_ind$P4_4 == "Telefonía  móvil / celular")
Tele_movi_celul_4 <- sum(Tele_movi_celul_4$Freq)
# Telefonía fija en casa - hogar
Tele_fija_hogar_4 <- P4_4_ind %>% filter(P4_4_ind$P4_4 == "Telefonía fija en casa/hogar")
Tele_fija_hogar_4 <- sum(Tele_fija_hogar_4$Freq)
# Internet móvil- datos en su celular
Inter_mov_celul_4 <- P4_4_ind %>% filter(P4_4_ind$P4_4 == "Internet móvil- datos en su celular")
Inter_mov_celul_4 <- sum(Inter_mov_celul_4$Freq)
# Internet fijo en su casa/hogar
Inter_fijo_hogar_4 <- P4_4_ind %>% filter(P4_4_ind$P4_4 == "Internet fijo en su casa/hogar")
Inter_fijo_hogar_4 <- sum(Inter_fijo_hogar_4$Freq)
# Televisión por suscripción- Cable
Telev_suscrip_cable_4 <- P4_4_ind %>% filter(P4_4_ind$P4_4 == "Televisión por suscripción- Cable")
Telev_suscrip_cable_4 <- sum(Telev_suscrip_cable_4$Freq)
# Televisión Abierta – No es por suscripción y es gratuita
Telev_abier_gratui_4 <- P4_4_ind %>% filter(P4_4_ind$P4_4 == "Televisión Abierta – No es por suscripción y es gratuita")
Telev_abier_gratui_4 <- sum(Telev_abier_gratui_4$Freq)


# Servicios con los que cuenta de manera permanente en el hogar 5
P4_5_ind <- data.frame(svytable(~FACTOR + P4_5,
                                design = BD_survey))
# Telefonía movil - celular
Tele_movi_celul_5 <- P4_5_ind %>% filter(P4_5_ind$P4_5 == "Telefonía  móvil / celular")
Tele_movi_celul_5 <- sum(Tele_movi_celul_5$Freq)
# Telefonía fija en casa - hogar
Tele_fija_hogar_5 <- P4_5_ind %>% filter(P4_5_ind$P4_5 == "Telefonía fija en casa/hogar")
Tele_fija_hogar_5 <- sum(Tele_fija_hogar_5$Freq)
# Internet móvil- datos en su celular
Inter_mov_celul_5 <- P4_5_ind %>% filter(P4_5_ind$P4_5 == "Internet móvil- datos en su celular")
Inter_mov_celul_5 <- sum(Inter_mov_celul_5$Freq)
# Internet fijo en su casa/hogar
Inter_fijo_hogar_5 <- P4_5_ind %>% filter(P4_5_ind$P4_5 == "Internet fijo en su casa/hogar")
Inter_fijo_hogar_5 <- sum(Inter_fijo_hogar_5$Freq)
# Televisión por suscripción- Cable
Telev_suscrip_cable_5 <- P4_5_ind %>% filter(P4_5_ind$P4_5 == "Televisión por suscripción- Cable")
Telev_suscrip_cable_5 <- sum(Telev_suscrip_cable_5$Freq)
# Televisión Abierta – No es por suscripción y es gratuita
Telev_abier_gratui_5 <- P4_5_ind %>% filter(P4_5_ind$P4_5 == "Televisión Abierta – No es por suscripción y es gratuita")
Telev_abier_gratui_5 <- sum(Telev_abier_gratui_5$Freq)

# Servicios con los que cuenta de manera permanente en el hogar 6
P4_6_ind <- data.frame(svytable(~FACTOR + P4_6,
                                design = BD_survey))
# Telefonía movil - celular
Tele_movi_celul_6 <- P4_6_ind %>% filter(P4_6_ind$P4_6 == "Telefonía  móvil / celular")
Tele_movi_celul_6 <- sum(Tele_movi_celul_6$Freq)
# Telefonía fija en casa - hogar
Tele_fija_hogar_6 <- P4_6_ind %>% filter(P4_6_ind$P4_6 == "Telefonía fija en casa/hogar")
Tele_fija_hogar_6 <- sum(Tele_fija_hogar_6$Freq)
# Internet móvil- datos en su celular
Inter_mov_celul_6 <- P4_6_ind %>% filter(P4_6_ind$P4_6 == "Internet móvil- datos en su celular")
Inter_mov_celul_6 <- sum(Inter_mov_celul_6$Freq)
# Internet fijo en su casa/hogar
Inter_fijo_hogar_6 <- P4_6_ind %>% filter(P4_6_ind$P4_6 == "Internet fijo en su casa/hogar")
Inter_fijo_hogar_6 <- sum(Inter_fijo_hogar_6$Freq)
# Televisión por suscripción- Cable

Telev_suscrip_cable_6 <- P4_6_ind %>% filter(P4_6_ind$P4_6 == "Televisión por suscripción- Cable")
Telev_suscrip_cable_6 <- sum(Telev_suscrip_cable_6$Freq)
# Televisión Abierta – No es por suscripción y es gratuita
Telev_abier_gratui_6 <- P4_6_ind %>% filter(P4_6_ind$P4_6 == "Televisión Abierta – No es por suscripción y es gratuita")
Telev_abier_gratui_6 <- sum(Telev_abier_gratui_6$Freq)

##############################################################################################################################################################################
# En una escala de 1 a 10. ¿qué tanto conoce o entiende de tecnología?
P50_ind <- data.frame(svytable(~FACTOR + P50,
                           design = BD_survey))

# no conozco nada
No_conozco <- P50_ind %>% filter(P50_ind$P50 == "No conozco nada")
No_conozco <- sum(No_conozco$Freq)

# 2
P50_2 <- P50_ind %>% filter(P50_ind$P50 == "2")
P50_2 <- sum(P50_2$Freq)

# 3
P50_3 <- P50_ind %>% filter(P50_ind$P50 == "3")
P50_3 <- sum(P50_3$Freq)

# 4
P50_4 <- P50_ind %>% filter(P50_ind$P50 == "4")
P50_4 <- sum(P50_4$Freq)

# 5
P50_5 <- P50_ind %>% filter(P50_ind$P50 == "5")
P50_5 <- sum(P50_5$Freq)

# 6
P50_6 <- P50_ind %>% filter(P50_ind$P50 == "6")
P50_6 <- sum(P50_6$Freq)

# 7
P50_7 <- P50_ind %>% filter(P50_ind$P50 == "7")
P50_7 <- sum(P50_7$Freq)

# 8
P50_8 <- P50_ind %>% filter(P50_ind$P50 == "8")
P50_8 <- sum(P50_8$Freq)

# 9
P50_9 <- P50_ind %>% filter(P50_ind$P50 == "9")
P50_9 <- sum(P50_9$Freq)

# Conozco mucho
Conozco_mucho <- P50_ind %>% filter(P50_ind$P50 == "Conozco mucho")
Conozco_mucho <- sum(Conozco_mucho$Freq)
#########################################################################################################################################
# En una escala de 1 a 10, ¿qué tanto se informa o se interesa por saber tecnología?
P51_ind <- data.frame(svytable(~FACTOR + P51,
                               design = BD_survey))

# no conozco nada
No_interesado <- P51_ind %>% filter(P51_ind$P51 == "No me interesa nada")
No_interesado <- sum(No_interesado$Freq)

# 2
P51_2 <- P50_ind %>% filter(P51_ind$P51 == "2")
P51_2 <- sum(P51_2$Freq)

# 3
P51_3 <- P51_ind %>% filter(P51_ind$P51 == "3")
P51_3 <- sum(P51_3$Freq)

# 4
P51_4 <- P51_ind %>% filter(P51_ind$P51 == "4")
P51_4 <- sum(P51_4$Freq)

# 5
P51_5 <- P51_ind %>% filter(P51_ind$P51 == "5")
P51_5 <- sum(P51_5$Freq)

# 6
P51_6 <- P51_ind %>% filter(P51_ind$P51 == "6")
P51_6 <- sum(P51_6$Freq)

# 7
P51_7 <- P51_ind %>% filter(P51_ind$P51 == "7")
P51_7 <- sum(P51_7$Freq)

# 8
P51_8 <- P51_ind %>% filter(P51_ind$P51 == "8")
P51_8 <- sum(P51_8$Freq)

# 9
P51_9 <- P51_ind %>% filter(P51_ind$P51 == "9")
P51_9 <- sum(P51_9$Freq)

# Conozco mucho
Me_interesa <- P51_ind %>% filter(P51_ind$P51 == "Me interesa mucho")
Me_interesa <- sum(Me_interesa$Freq)

###############################################################################################################################
# ¿Cuál es el último nivrl educativo que cursó o se encuentra cursando actualmente?
P54_ind <- data.frame(svytable(~FACTOR + P54,
                               design = BD_survey))

# Primaria
Primaria <- P54_ind %>% filter(P54_ind$P54 == "Primaria")
Primaria <- sum(Primaria$Freq)

# Bachillerato
Bachillerato <- P54_ind %>% filter(P54_ind$P54 == "Bachillerato")
Bachillerato <- sum(Bachillerato$Freq)

# Técnico/ tecnológico
Técnico_tecnológico <- P54_ind %>% filter(P54_ind$P54 == "Técnico/ tecnológico")
Técnico_tecnológico <- sum(Técnico_tecnológico$Freq)

# Universitario
Universitario <- P54_ind %>% filter(P54_ind$P54 == "Universitario")
Universitario <- sum(Universitario$Freq)

# Especialización
Especialización <- P54_ind %>% filter(P54_ind$P54 == "Especialización")
Especialización <- sum(Especialización$Freq)

# Maestría
Maestría <- P54_ind %>% filter(P54_ind$P54 == "Maestría")
Maestría <- sum(Maestría$Freq)

# Doctorado
Doctorado <- P54_ind %>% filter(P54_ind$P54 == "Doctorado")
Doctorado <- sum(Doctorado$Freq)

# Ninguno
Ninguno <- P54_ind %>% filter(P54_ind$P54 == "Ninguno")
Ninguno <- sum(Ninguno$Freq)

#################################################################################################################################################
# Si tiene internet móvil, ¿ a qué operador pertenece este servicio?
P306_1_ind <- data.frame(svytable(~FACTOR + P306_1,
                                  design = BD_survey))
# Claro
Claro_1 <- P306_1_ind %>% filter(P306_1_ind$P306_1 == "Claro")
Claro_1 <- sum(Claro_1$Freq)

# Movistar/Telefónica
Movistar_telefónica_1 <- P306_1_ind %>% filter(P306_1_ind$P306_1 == "Movistar/Telefónica")
Movistar_telefónica_1 <- sum(Movistar_telefónica_1$Freq)

# Tigo/Une
Tigo_une_1 <- P306_1_ind %>% filter(P306_1_ind$P306_1 == "Tigo/Une")
Tigo_une_1 <- sum(Tigo_une_1$Freq)

# ETB
ETB_1 <- P306_1_ind %>% filter(P306_1_ind$P306_1 == "ETB")
ETB_1 <- sum(ETB_1$Freq)

# Éxito
Exito_1 <- P306_1_ind %>% filter(P306_1_ind$P306_1 == "Éxito")
Exito_1 <- sum(Exito_1$Freq)

# Flash mobile
Flash_mobile_1 <- P306_1_ind %>% filter(P306_1_ind$P306_1 == "Flash Mobile")
Flash_mobile_1 <- sum(Flash_mobile_1$Freq)

# Virgin mobile
Virgin_mobile_1 <- P306_1_ind %>% filter(P306_1_ind$P306_1 == "Virgin Mobile")
Virgin_mobile_1 <- sum(Virgin_mobile_1$Freq)

# Avantel
Avantel_1 <- P306_1_ind %>% filter(P306_1_ind$P306_1 == "Avantel")
Avantel_1 <- sum(Avantel_1$Freq)

# Wom
WOM_1 <- P306_1_ind %>% filter(P306_1_ind$P306_1 == "WOM")
WOM_1 <- sum(WOM_1$Freq)

# Emcali
Emcali_1 <- P306_1_ind %>% filter(P306_1_ind$P306_1 == "Emcali")
Emcali_1 <- sum(Emcali_1$Freq)

# Otro
Otro_1 <- P306_1_ind %>% filter(P306_1_ind$P306_1 == "Otro")
Otro_1 <- sum(Otro_1$Freq)

# No sabe/no responde
Nosabe_noresponde_1 <- P306_1_ind %>% filter(P306_1_ind$P306_1 == "No sabe / no responde")
Nosabe_noresponde_1 <- sum(Nosabe_noresponde_1$Freq)

##############################################################################################################################
# Si tiene internet móvil, ¿ a qué operador pertenece este servicio? 2
P306_2_ind <- data.frame(svytable(~FACTOR + P306_2,
                                  design = BD_survey))
# Claro
Claro_2 <- P306_2_ind %>% filter(P306_2_ind$P306_2 == "Claro")
Claro_2 <- sum(Claro_2$Freq)

# Movistar/Telefónica
Movistar_telefónica_2 <- P306_2_ind %>% filter(P306_2_ind$P306_2 == "Movistar/Telefónica")
Movistar_telefónica_2 <- sum(Movistar_telefónica_2$Freq)

# Tigo/Une
Tigo_une_2 <- P306_2_ind %>% filter(P306_2_ind$P306_2 == "Tigo/Une")
Tigo_une_2 <- sum(Tigo_une_2$Freq)

# ETB
ETB_2 <- P306_2_ind %>% filter(P306_2_ind$P306_2 == "ETB")
ETB_2 <- sum(ETB_2$Freq)

# Éxito
Exito_2 <- P306_2_ind %>% filter(P306_2_ind$P306_2 == "Éxito")
Exito_2 <- sum(Exito_2$Freq)

# Flash mobile
Flash_mobile_2 <- P306_2_ind %>% filter(P306_2_ind$P306_2 == "Flash Mobile")
Flash_mobile_2 <- sum(Flash_mobile_2$Freq)

# Virgin mobile
Virgin_mobile_2 <- P306_2_ind %>% filter(P306_2_ind$P306_2 == "Virgin Mobile")
Virgin_mobile_2 <- sum(Virgin_mobile_2$Freq)

# Avantel
Avantel_2 <- P306_2_ind %>% filter(P306_2_ind$P306_2 == "Avantel")
Avantel_2 <- sum(Avantel_2$Freq)

# Wom
WOM_2 <- P306_2_ind %>% filter(P306_2_ind$P306_2 == "WOM")
WOM_2 <- sum(WOM_2$Freq)

# Emcali
Emcali_2 <- P306_2_ind %>% filter(P306_2_ind$P306_2 == "Emcali")
Emcali_2 <- sum(Emcali_2$Freq)

# Otro
Otro_2 <- P306_2_ind %>% filter(P306_2_ind$P306_2 == "Otro")
Otro_2 <- sum(Otro_2$Freq)

# No sabe/no responde
Nosabe_noresponde_2 <- P306_2_ind %>% filter(P306_2_ind$P306_2 == "No sabe / no responde")
Nosabe_noresponde_2 <- sum(Nosabe_noresponde_2$Freq)

##############################################################################################################################
# Si tiene internet móvil, ¿ a qué operador pertenece este servicio? 3
P306_3_ind <- data.frame(svytable(~FACTOR + P306_3,
                                  design = BD_survey))
# Claro
Claro_3 <- P306_3_ind %>% filter(P306_3_ind$P306_3 == "Claro")
Claro_3 <- sum(Claro_3$Freq)

# Movistar/Telefónica
Movistar_telefónica_3 <- P306_3_ind %>% filter(P306_3_ind$P306_3 == "Movistar/Telefónica")
Movistar_telefónica_3 <- sum(Movistar_telefónica_3$Freq)

# Tigo/Une
Tigo_une_3 <- P306_3_ind %>% filter(P306_3_ind$P306_3 == "Tigo/Une")
Tigo_une_3 <- sum(Tigo_une_3$Freq)

# ETB
ETB_3 <- P306_3_ind %>% filter(P306_3_ind$P306_3 == "ETB")
ETB_3 <- sum(ETB_3$Freq)

# Éxito
Exito_3 <- P306_3_ind %>% filter(P306_3_ind$P306_3 == "Éxito")
Exito_3 <- sum(Exito_3$Freq)

# Flash mobile
Flash_mobile_3 <- P306_3_ind %>% filter(P306_3_ind$P306_3 == "Flash Mobile")
Flash_mobile_3 <- sum(Flash_mobile_3$Freq)

# Virgin mobile
Virgin_mobile_3 <- P306_3_ind %>% filter(P306_3_ind$P306_3 == "Virgin Mobile")
Virgin_mobile_3 <- sum(Virgin_mobile_3$Freq)

# Avantel
Avantel_3 <- P306_3_ind %>% filter(P306_3_ind$P306_3 == "Avantel")
Avantel_3 <- sum(Avantel_3$Freq)

# Wom
WOM_3 <- P306_3_ind %>% filter(P306_3_ind$P306_3 == "WOM")
WOM_3 <- sum(WOM_3$Freq)

# Emcali
Emcali_3 <- P306_3_ind %>% filter(P306_3_ind$P306_3 == "Emcali")
Emcali_3 <- sum(Emcali_3$Freq)

# Otro
Otro_3 <- P306_3_ind %>% filter(P306_3_ind$P306_3 == "Otro")
Otro_3 <- sum(Otro_3$Freq)

# No sabe/no responde
Nosabe_noresponde_3 <- P306_3_ind %>% filter(P306_3_ind$P306_3 == "No sabe / no responde")
Nosabe_noresponde_3 <- sum(Nosabe_noresponde_3$Freq)

#######################################################################################################################################
# Hablando de su teléfono móvil, ¿cuántos teléfonos tiene en uso?
P306BVAL_ind <- data.frame(svytable(~FACTOR + P306BVAL,
                                    design = BD_survey))
# 1
Cant_1_telef <- P306BVAL_ind %>% filter(P306BVAL_ind$P306BVAL == "1")
Cant_1_telef <- sum(Cant_1_telef$Freq)

# 2 
Cant_2_telef <- P306BVAL_ind %>% filter(P306BVAL_ind$P306BVAL == "2")
Cant_2_telef <- sum(Cant_2_telef$Freq)

# 3
Cant_3_telef <- P306BVAL_ind %>% filter(P306BVAL_ind$P306BVAL == "3")
Cant_3_telef <- sum(Cant_3_telef$Freq)

# 4
Cant_4_telef <- P306BVAL_ind %>% filter(P306BVAL_ind$P306BVAL == "4")
Cant_4_telef <- sum(Cant_4_telef$Freq)

# 5
Cant_5_telef <- P306BVAL_ind %>% filter(P306BVAL_ind$P306BVAL == "5")
Cant_5_telef <- sum(Cant_5_telef$Freq)

# 6
Cant_6_telef <- P306BVAL_ind %>% filter(P306BVAL_ind$P306BVAL == "6")
Cant_6_telef <- sum(Cant_6_telef$Freq)

######################################################################################################################################################
# Tipo de teléfono 1
P306C_1_ind <- data.frame(svytable(~FACTOR + P306C_1,
                                    design = BD_survey))

# Smartphone Inteligente / IPhone
Smartphone_1 <- P306C_1_ind %>% filter(P306C_1_ind$P306C_1 == "Smartphone Inteligente / IPhone")
Smartphone_1 <- sum(Smartphone_1$Freq)

# Clásico (Flecha)
Flecha_1 <- P306C_1_ind %>% filter(P306C_1_ind$P306C_1 == "Clásico (Flecha)")
Flecha_1 <- sum(Flecha_1$Freq)

########################################################################################################################################################
# Tipo de teléfono 2
P306C_2_ind <- data.frame(svytable(~FACTOR + P306C_2,
                                   design = BD_survey))

# Smartphone Inteligente / IPhone
Smartphone_2 <- P306C_2_ind %>% filter(P306C_2_ind$P306C_2 == "Smartphone Inteligente / IPhone")
Smartphone_2 <- sum(Smartphone_2$Freq)

# Clásico (Flecha)
Flecha_2 <- P306C_2_ind %>% filter(P306C_2_ind$P306C_2 == "Clásico (Flecha)")
Flecha_2 <- sum(Flecha_2$Freq)

###########################################################################################################################################################
# Tipo de teléfono 3
P306C_3_ind <- data.frame(svytable(~FACTOR + P306C_3,
                                   design = BD_survey))

# Smartphone Inteligente / IPhone
Smartphone_3 <- P306C_3_ind %>% filter(P306C_3_ind$P306C_3 == "Smartphone Inteligente / IPhone")
Smartphone_3 <- sum(Smartphone_3$Freq)

# Clásico (Flecha)
Flecha_3 <- P306C_3_ind %>% filter(P306C_3_ind$P306C_3 == "Clásico (Flecha)")
Flecha_3 <- sum(Flecha_3$Freq)

############################################################################################################################################################
# Tipo de teléfono 4
P306C_4_ind <- data.frame(svytable(~FACTOR + P306C_4,
                                   design = BD_survey))

# Smartphone Inteligente / IPhone
Smartphone_4 <- P306C_4_ind %>% filter(P306C_4_ind$P306C_4 == "Smartphone Inteligente / IPhone")
Smartphone_4 <- sum(Smartphone_4$Freq)

# Clásico (Flecha)
Flecha_4 <- P306C_4_ind %>% filter(P306C_4_ind$P306C_4 == "Clásico (Flecha)")
Flecha_4 <- sum(Flecha_4$Freq)

############################################################################################################################################################
# Tipo de teléfono 5
P306C_5_ind <- data.frame(svytable(~FACTOR + P306C_5,
                                   design = BD_survey))

# Smartphone Inteligente / IPhone
Smartphone_5 <- P306C_5_ind %>% filter(P306C_5_ind$P306C_5 == "Smartphone Inteligente / IPhone")
Smartphone_5 <- sum(Smartphone_5$Freq)

# Clásico (Flecha)
Flecha_5 <- P306C_5_ind %>% filter(P306C_5_ind$P306C_5 == "Clásico (Flecha)")
Flecha_5 <- sum(Flecha_5$Freq)

##############################################################################################################################################################
# Tipo de teléfono 6
P306C_6_ind <- data.frame(svytable(~FACTOR + P306C_6,
                                   design = BD_survey))

# Smartphone Inteligente / IPhone
Smartphone_6 <- P306C_6_ind %>% filter(P306C_6_ind$P306C_6 == "Smartphone Inteligente / IPhone")
Smartphone_6 <- sum(Smartphone_6$Freq)

# Clásico (Flecha)
Flecha_6 <- P306C_6_ind %>% filter(P306C_6_ind$P306C_6 == "Clásico (Flecha)")
Flecha_6 <- sum(Flecha_6$Freq)

###############################################################################################################################################################
###############################################################################################################################################################
# Telefono 1, ¿qué marca es?
CP310_1_ind <- data.frame(svytable(~FACTOR + CP310_1,
                                   design = BD_survey))
# Samsung
Samsung_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Samsung")
Samsung_1 <- sum(Samsung_1$Freq)

# LG
LG_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "LG")
LG_1 <- sum(LG_1$Freq)

# Sony
Sony_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Sony")
Sony_1 <- sum(Sony_1$Freq)

# Panasonic
Panasonic_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Panasonic")
Panasonic_1 <- sum(Panasonic_1$Freq)

# Kalley
Kalley_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Kalley")
Kalley_1 <- sum(Kalley_1$Freq)

# Challenger
Challenger_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Challenger")
Challenger_1 <- sum(Challenger_1$Freq)

# Daewo
Daewo_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Daewo")
Daewo_1 <- sum(Daewo_1$Freq)

# Caixun
Caixun_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Caixun")
Caixun_1 <- sum(Caixun_1$Freq)

# Hyundai
Hyundai_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Hyundai")
Hyundai_1 <- sum(Hyundai_1$Freq)

# Sharp
Sharp_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Sharp")
Sharp_1 <- sum(Sharp_1$Freq)

# Ssangyong
Ssangyong_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Ssangyong")
Ssangyong_1 <- sum(Ssangyong_1$Freq)

# Toshiba
Toshiba_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Toshiba")
Toshiba_1 <- sum(Toshiba_1$Freq)

# Olimpo
Olimpo_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Olimpo")
Olimpo_1 <- sum(Olimpo_1$Freq)

# Sankey
Sankey_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Sankey")
Sankey_1 <- sum(Sankey_1$Freq)

# Philips
Philips_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Philips")
Philips_1 <- sum(Philips_1$Freq)

# Iván Botero Gómez (IBG)
IBG_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Iván Botero Gómez (IBG)")
IBG_1 <- sum(IBG_1$Freq)

# Vizio
Vizio_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Vizio")
Vizio_1 <- sum(Vizio_1$Freq)

# Smart Visión TV
SmartTV_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Smart Visión TV")
SmartTV_1 <- sum(SmartTV_1$Freq)

# Simply
Simply_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Simply")
Simply_1 <- sum(Simply_1$Freq)

# AOC
AOC_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "AOC")
AOC_1 <- sum(AOC_1$Freq)

# Apple
Apple_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Apple")
Apple_1 <- sum(Apple_1$Freq)

# Huawei (Honor)
Huawei_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Huawei (Honor)")
Huawei_1 <- sum(Huawei_1$Freq)

# Motorola
Motorola_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Motorola")
Motorola_1 <- sum(Motorola_1$Freq)

# Xiaomi
Xiaomi_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Xiaomi")
Xiaomi_1 <- sum(Xiaomi_1$Freq)

# Nokia
Nokia_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Nokia")
Nokia_1 <- sum(Nokia_1$Freq)

# Vivo
Vivo_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Vivo")
Vivo_1 <- sum(Vivo_1$Freq)

# Alcatel
Alcatel_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Alcatel")
Alcatel_1 <- sum(Alcatel_1$Freq)

# Krono
Krono_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Krono")
Krono_1 <- sum(Krono_1$Freq)

# ZTE
ZTE_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "ZTE")
ZTE_1 <- sum(ZTE_1$Freq)

# Tecno Mobile
TecnoMobile_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Tecno Mobile")
TecnoMobile_1 <- sum(TecnoMobile_1$Freq)

# Asus
Asus_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Asus")
Asus_1 <- sum(Asus_1$Freq)

# Blu
Blu_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Blu")
Blu_1 <- sum(Blu_1$Freq)

# Corn
Corn_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Corn")
Corn_1 <- sum(Corn_1$Freq)

# iPro
iPro_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "iPro")
iPro_1 <- sum(iPro_1$Freq)

# Oppo
Oppo_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Oppo")
Oppo_1 <- sum(Oppo_1$Freq)

# Caterpillar
Caterpillar_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Caterpillar")
Caterpillar_1 <- sum(Caterpillar_1$Freq)

# Infinix
Infinix_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Infinix")
Infinix_1 <- sum(Infinix_1$Freq)

# Lenovo
Lenovo_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Lenovo")
Lenovo_1 <- sum(Lenovo_1$Freq)

# Otro
Otro_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "Otro")
Otro_1 <- sum(Otro_1$Freq)

# No sabe / no responde
Nosabe_noresponde_1 <- CP310_1_ind %>% filter(CP310_1_ind$CP310_1 == "No sabe / no responde")
Nosabe_noresponde_1  <- sum(Nosabe_noresponde_1 $Freq)
###########################################################################################################################################################
# Año en que fue adquirido el teléfono 1
P310B_1_ind <- data.frame(svytable(~FACTOR + P310B_1,
                                   design = BD_survey))
# 1999
Año1999_1 <- P310B_1_ind %>% filter(P310B_1_ind$P310B_1 == "99")
Año1999_1 <- sum(Año1999_1$Freq)

# 2000
Año2000_1 <- P310B_1_ind %>% filter(P310B_1_ind$P310B_1 == "2000")
Año2000_1 <- sum(Año2000_1$Freq)

# 2001
Año2001_1 <- P310B_1_ind %>% filter(P310B_1_ind$P310B_1 == "2001")
Año2001_1 <- sum(Año2001_1$Freq)

# 2003
Año2003_1 <- P310B_1_ind %>% filter(P310B_1_ind$P310B_1 == "2003")
Año2003_1 <- sum(Año2003_1$Freq)

# 2005
Año2005_1 <- P310B_1_ind %>% filter(P310B_1_ind$P310B_1 == "2005")
Año2005_1 <- sum(Año2005_1$Freq)

# 2008
Año2008_1 <- P310B_1_ind %>% filter(P310B_1_ind$P310B_1 == "2008")
Año2008_1 <- sum(Año2008_1$Freq)

# 2009
Año2009_1 <- P310B_1_ind %>% filter(P310B_1_ind$P310B_1 == "2009")
Año2009_1 <- sum(Año2009_1$Freq)

# 2010
Año2010_1 <- P310B_1_ind %>% filter(P310B_1_ind$P310B_1 == "2010")
Año2010_1 <- sum(Año2010_1$Freq)

# 2011
Año2011_1 <- P310B_1_ind %>% filter(P310B_1_ind$P310B_1 == "2011")
Año2011_1 <- sum(Año2011_1$Freq)

# 2012
Año2012_1 <- P310B_1_ind %>% filter(P310B_1_ind$P310B_1 == "2012")
Año2012_1 <- sum(Año2012_1$Freq)

# 2013
Año2013_1 <- P310B_1_ind %>% filter(P310B_1_ind$P310B_1 == "2013")
Año2013_1 <- sum(Año2013_1$Freq)

# 2014
Año2014_1 <- P310B_1_ind %>% filter(P310B_1_ind$P310B_1 == "2014")
Año2014_1 <- sum(Año2014_1$Freq)

# 2015
Año2015_1 <- P310B_1_ind %>% filter(P310B_1_ind$P310B_1 == "2015")
Año2015_1 <- sum(Año2015_1$Freq)

# 2016
Año2016_1 <- P310B_1_ind %>% filter(P310B_1_ind$P310B_1 == "2016")
Año2016_1 <- sum(Año2016_1$Freq)

# 2017
Año2017_1 <- P310B_1_ind %>% filter(P310B_1_ind$P310B_1 == "2017")
Año2017_1 <- sum(Año2017_1$Freq)

# 2018
Año2018_1 <- P310B_1_ind %>% filter(P310B_1_ind$P310B_1 == "2018")
Año2018_1 <- sum(Año2018_1$Freq)

# 2019
Año2019_1 <- P310B_1_ind %>% filter(P310B_1_ind$P310B_1 == "2019")
Año2019_1 <- sum(Año2019_1$Freq)

# 2020
Año2020_1 <- P310B_1_ind %>% filter(P310B_1_ind$P310B_1 == "2020")
Año2020_1 <- sum(Año2020_1$Freq)

# 2021
Año2021_1 <- P310B_1_ind %>% filter(P310B_1_ind$P310B_1 == "2021")
Año2021_1 <- sum(Año2021_1$Freq)
################################################################################################################################################
################################################################################################################################################
# Telefono 2, ¿qué marca es?
CP310_2_ind <- data.frame(svytable(~FACTOR + CP310_2,
                                   design = BD_survey))
# Samsung
Samsung_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Samsung")
Samsung_2 <- sum(Samsung_2$Freq)

# LG
LG_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "LG")
LG_2 <- sum(LG_2$Freq)

# Sony
Sony_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Sony")
Sony_2 <- sum(Sony_2$Freq)

# Panasonic
Panasonic_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Panasonic")
Panasonic_2 <- sum(Panasonic_2$Freq)

# Kalley
Kalley_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Kalley")
Kalley_2 <- sum(Kalley_2$Freq)

# Challenger
Challenger_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Challenger")
Challenger_2 <- sum(Challenger_2$Freq)

# Daewo
Daewo_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Daewo")
Daewo_2 <- sum(Daewo_2$Freq)

# Caixun
Caixun_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Caixun")
Caixun_2 <- sum(Caixun_2$Freq)

# Hyundai
Hyundai_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Hyundai")
Hyundai_2 <- sum(Hyundai_2$Freq)

# Sharp
Sharp_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Sharp")
Sharp_2 <- sum(Sharp_2$Freq)

# Ssangyong
Ssangyong_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Ssangyong")
Ssangyong_2 <- sum(Ssangyong_2$Freq)

# Toshiba
Toshiba_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Toshiba")
Toshiba_2 <- sum(Toshiba_2$Freq)

# Olimpo
Olimpo_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Olimpo")
Olimpo_2 <- sum(Olimpo_2$Freq)

# Sankey
Sankey_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Sankey")
Sankey_2 <- sum(Sankey_2$Freq)

# Philips
Philips_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Philips")
Philips_2 <- sum(Philips_2$Freq)

# Iván Botero Gómez (IBG)
IBG_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Iván Botero Gómez (IBG)")
IBG_2 <- sum(IBG_2$Freq)

# Vizio
Vizio_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Vizio")
Vizio_2 <- sum(Vizio_2$Freq)

# Smart Visión TV
SmartTV_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Smart Visión TV")
SmartTV_2 <- sum(SmartTV_2$Freq)

# Simply
Simply_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Simply")
Simply_2 <- sum(Simply_2$Freq)

# AOC
AOC_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "AOC")
AOC_2 <- sum(AOC_2$Freq)

# Apple
Apple_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Apple")
Apple_2 <- sum(Apple_2$Freq)

# Huawei (Honor)
Huawei_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Huawei (Honor)")
Huawei_2 <- sum(Huawei_2$Freq)

# Motorola
Motorola_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Motorola")
Motorola_2 <- sum(Motorola_2$Freq)

# Xiaomi
Xiaomi_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Xiaomi")
Xiaomi_2 <- sum(Xiaomi_2$Freq)

# Nokia
Nokia_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Nokia")
Nokia_2 <- sum(Nokia_2$Freq)

# Vivo
Vivo_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Vivo")
Vivo_2 <- sum(Vivo_2$Freq)

# Alcatel
Alcatel_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Alcatel")
Alcatel_2 <- sum(Alcatel_2$Freq)

# Krono
Krono_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Krono")
Krono_2 <- sum(Krono_2$Freq)

# ZTE
ZTE_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "ZTE")
ZTE_2 <- sum(ZTE_2$Freq)

# Tecno Mobile
TecnoMobile_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Tecno Mobile")
TecnoMobile_2 <- sum(TecnoMobile_2$Freq)

# Asus
Asus_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Asus")
Asus_2 <- sum(Asus_2$Freq)

# Blu
Blu_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Blu")
Blu_2 <- sum(Blu_2$Freq)

# Corn
Corn_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Corn")
Corn_2 <- sum(Corn_2$Freq)

# iPro
iPro_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "iPro")
iPro_2 <- sum(iPro_2$Freq)

# Oppo
Oppo_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Oppo")
Oppo_2 <- sum(Oppo_2$Freq)

# Caterpillar
Caterpillar_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Caterpillar")
Caterpillar_2 <- sum(Caterpillar_2$Freq)

# Infinix
Infinix_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Infinix")
Infinix_2 <- sum(Infinix_2$Freq)

# Lenovo
Lenovo_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Lenovo")
Lenovo_2 <- sum(Lenovo_2$Freq)

# Otro
Otro_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "Otro")
Otro_2 <- sum(Otro_2$Freq)

# No sabe / no responde
Nosabe_noresponde_2 <- CP310_2_ind %>% filter(CP310_2_ind$CP310_2 == "No sabe / no responde")
Nosabe_noresponde_2  <- sum(Nosabe_noresponde_2 $Freq)


###########################################################################################################################################################
# Año en que fue adquirido el teléfono 2
P310B_2_ind <- data.frame(svytable(~FACTOR + P310B_2,
                                   design = BD_survey))
# 1999
Año1999_2 <- P310B_2_ind %>% filter(P310B_2_ind$P310B_2 == "99")
Año1999_2 <- sum(Año1999_2$Freq)

# 2007
Año2007_2 <- P310B_2_ind %>% filter(P310B_2_ind$P310B_2 == "2007")
Año2007_2 <- sum(Año2007_2$Freq)

# 2008
Año2008_2 <- P310B_2_ind %>% filter(P310B_2_ind$P310B_2 == "2008")
Año2008_2 <- sum(Año2008_2$Freq)

# 2010
Año2010_2 <- P310B_2_ind %>% filter(P310B_2_ind$P310B_2 == "2010")
Año2010_2 <- sum(Año2010_2$Freq)

# 2013
Año2013_2 <- P310B_2_ind %>% filter(P310B_2_ind$P310B_2 == "2013")
Año2013_2 <- sum(Año2013_2$Freq)

# 2015
Año2015_2 <- P310B_2_ind %>% filter(P310B_2_ind$P310B_2 == "2015")
Año2015_2 <- sum(Año2015_2$Freq)

# 2016
Año2016_2 <- P310B_2_ind %>% filter(P310B_2_ind$P310B_2 == "2016")
Año2016_2 <- sum(Año2016_2$Freq)

# 2017
Año2017_2 <- P310B_2_ind %>% filter(P310B_2_ind$P310B_2 == "2017")
Año2017_2 <- sum(Año2017_2$Freq)

# 2018
Año2018_2 <- P310B_2_ind %>% filter(P310B_2_ind$P310B_2 == "2018")
Año2018_2 <- sum(Año2018_2$Freq)

# 2019
Año2019_2 <- P310B_2_ind %>% filter(P310B_2_ind$P310B_2 == "2019")
Año2019_2 <- sum(Año2019_2$Freq)

# 2020
Año2020_2 <- P310B_2_ind %>% filter(P310B_2_ind$P310B_2 == "2020")
Año2020_2 <- sum(Año2020_2$Freq)

# 2021
Año2021_2 <- P310B_2_ind %>% filter(P310B_2_ind$P310B_2 == "2021")
Año2021_2 <- sum(Año2021_2$Freq)

#############################################################################################################################################
#############################################################################################################################################
# Telefono 3, ¿qué marca es?
CP310_3_ind <- data.frame(svytable(~FACTOR + CP310_3,
                                   design = BD_survey))
# Samsung
Samsung_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Samsung")
Samsung_3 <- sum(Samsung_3$Freq)

# LG
LG_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "LG")
LG_3 <- sum(LG_3$Freq)

# Sony
Sony_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Sony")
Sony_3 <- sum(Sony_3$Freq)

# Panasonic
Panasonic_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Panasonic")
Panasonic_3 <- sum(Panasonic_3$Freq)

# Kalley
Kalley_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Kalley")
Kalley_3 <- sum(Kalley_3$Freq)

# Challenger
Challenger_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Challenger")
Challenger_3 <- sum(Challenger_3$Freq)

# Daewo
Daewo_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Daewo")
Daewo_3 <- sum(Daewo_3$Freq)

# Caixun
Caixun_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Caixun")
Caixun_3 <- sum(Caixun_3$Freq)

# Hyundai
Hyundai_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Hyundai")
Hyundai_3 <- sum(Hyundai_3$Freq)

# Sharp
Sharp_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Sharp")
Sharp_3 <- sum(Sharp_3$Freq)

# Ssangyong
Ssangyong_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Ssangyong")
Ssangyong_3 <- sum(Ssangyong_3$Freq)

# Toshiba
Toshiba_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Toshiba")
Toshiba_3 <- sum(Toshiba_3$Freq)

# Olimpo
Olimpo_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Olimpo")
Olimpo_3 <- sum(Olimpo_3$Freq)

# Sankey
Sankey_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Sankey")
Sankey_3 <- sum(Sankey_3$Freq)

# Philips
Philips_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Philips")
Philips_3 <- sum(Philips_3$Freq)

# Iván Botero Gómez (IBG)
IBG_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Iván Botero Gómez (IBG)")
IBG_3 <- sum(IBG_3$Freq)

# Vizio
Vizio_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Vizio")
Vizio_3 <- sum(Vizio_3$Freq)

# Smart Visión TV
SmartTV_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Smart Visión TV")
SmartTV_3 <- sum(SmartTV_3$Freq)

# Simply
Simply_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Simply")
Simply_3 <- sum(Simply_3$Freq)

# AOC
AOC_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "AOC")
AOC_3 <- sum(AOC_3$Freq)

# Apple
Apple_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Apple")
Apple_3 <- sum(Apple_3$Freq)

# Huawei (Honor)
Huawei_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Huawei (Honor)")
Huawei_3 <- sum(Huawei_3$Freq)

# Motorola
Motorola_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Motorola")
Motorola_3 <- sum(Motorola_3$Freq)

# Xiaomi
Xiaomi_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Xiaomi")
Xiaomi_3 <- sum(Xiaomi_3$Freq)

# Nokia
Nokia_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Nokia")
Nokia_3 <- sum(Nokia_3$Freq)

# Vivo
Vivo_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Vivo")
Vivo_3 <- sum(Vivo_3$Freq)

# Alcatel
Alcatel_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Alcatel")
Alcatel_3 <- sum(Alcatel_3$Freq)

# Krono
Krono_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Krono")
Krono_3 <- sum(Krono_3$Freq)

# ZTE
ZTE_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "ZTE")
ZTE_3 <- sum(ZTE_3$Freq)

# Tecno Mobile
TecnoMobile_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Tecno Mobile")
TecnoMobile_3 <- sum(TecnoMobile_3$Freq)

# Asus
Asus_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Asus")
Asus_3 <- sum(Asus_3$Freq)

# Blu
Blu_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Blu")
Blu_3 <- sum(Blu_3$Freq)

# Corn
Corn_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Corn")
Corn_3 <- sum(Corn_3$Freq)

# iPro
iPro_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "iPro")
iPro_3 <- sum(iPro_3$Freq)

# Oppo
Oppo_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Oppo")
Oppo_3 <- sum(Oppo_3$Freq)

# Caterpillar
Caterpillar_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Caterpillar")
Caterpillar_3 <- sum(Caterpillar_3$Freq)

# Infinix
Infinix_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Infinix")
Infinix_3 <- sum(Infinix_3$Freq)

# Lenovo
Lenovo_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Lenovo")
Lenovo_3 <- sum(Lenovo_3$Freq)

# Otro
Otro_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "Otro")
Otro_3 <- sum(Otro_3$Freq)

# No sabe / no responde
Nosabe_noresponde_3 <- CP310_3_ind %>% filter(CP310_3_ind$CP310_3 == "No sabe / no responde")
Nosabe_noresponde_3  <- sum(Nosabe_noresponde_3 $Freq)

###########################################################################################################################################################
# Año en que fue adquirido el teléfono 3

P310B_3_ind <- data.frame(svytable(~FACTOR + P310B_3,
                                   design = BD_survey))
# 1999
Año1999_3 <- P310B_3_ind %>% filter(P310B_3_ind$P310B_3 == "99")
Año1999_3 <- sum(Año1999_3$Freq)

# 2000
Año2000_3 <- P310B_3_ind %>% filter(P310B_3_ind$P310B_3 == "2000")
Año2000_3 <- sum(Año2000_3$Freq)

# 2018
Año2018_3 <- P310B_3_ind %>% filter(P310B_3_ind$P310B_3 == "2018")
Año2018_3 <- sum(Año2018_3$Freq)

# 2019
Año2019_3 <- P310B_3_ind %>% filter(P310B_3_ind$P310B_3 == "2019")
Año2019_3 <- sum(Año2019_3$Freq)

# 2020
Año2020_3 <- P310B_3_ind %>% filter(P310B_3_ind$P310B_3 == "2020")
Año2020_3 <- sum(Año2020_3$Freq)

# 2021
Año2021_3 <- P310B_3_ind %>% filter(P310B_3_ind$P310B_3 == "2021")
Año2021_3 <- sum(Año2021_3$Freq)

#############################################################################################################################################
#############################################################################################################################################
# Telefono 4, ¿qué marca es?
CP310_4_ind <- data.frame(svytable(~FACTOR + CP310_4,
                                   design = BD_survey))
# Samsung
Samsung_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Samsung")
Samsung_4 <- sum(Samsung_4$Freq)

# LG
LG_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "LG")
LG_4 <- sum(LG_4$Freq)

# Sony
Sony_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Sony")
Sony_4 <- sum(Sony_4$Freq)

# Panasonic
Panasonic_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Panasonic")
Panasonic_4 <- sum(Panasonic_4$Freq)

# Kalley
Kalley_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Kalley")
Kalley_4 <- sum(Kalley_4$Freq)

# Challenger
Challenger_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Challenger")
Challenger_4 <- sum(Challenger_4$Freq)

# Daewo
Daewo_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Daewo")
Daewo_4 <- sum(Daewo_4$Freq)

# Caixun
Caixun_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Caixun")
Caixun_4 <- sum(Caixun_4$Freq)

# Hyundai
Hyundai_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Hyundai")
Hyundai_4 <- sum(Hyundai_4$Freq)

# Sharp
Sharp_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Sharp")
Sharp_4 <- sum(Sharp_4$Freq)

# Ssangyong
Ssangyong_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Ssangyong")
Ssangyong_4 <- sum(Ssangyong_4$Freq)

# Toshiba
Toshiba_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Toshiba")
Toshiba_4 <- sum(Toshiba_4$Freq)

# Olimpo
Olimpo_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Olimpo")
Olimpo_4 <- sum(Olimpo_4$Freq)

# Sankey
Sankey_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Sankey")
Sankey_4 <- sum(Sankey_4$Freq)

# Philips
Philips_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Philips")
Philips_4 <- sum(Philips_4$Freq)

# Iván Botero Gómez (IBG)
IBG_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Iván Botero Gómez (IBG)")
IBG_4 <- sum(IBG_4$Freq)

# Vizio
Vizio_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Vizio")
Vizio_4 <- sum(Vizio_4$Freq)

# Smart Visión TV
SmartTV_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Smart Visión TV")
SmartTV_4 <- sum(SmartTV_4$Freq)

# Simply
Simply_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Simply")
Simply_4 <- sum(Simply_4$Freq)

# AOC
AOC_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "AOC")
AOC_4 <- sum(AOC_4$Freq)

# Apple
Apple_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Apple")
Apple_4 <- sum(Apple_4$Freq)

# Huawei (Honor)
Huawei_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Huawei (Honor)")
Huawei_4 <- sum(Huawei_4$Freq)

# Motorola
Motorola_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Motorola")
Motorola_4 <- sum(Motorola_4$Freq)

# Xiaomi
Xiaomi_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Xiaomi")
Xiaomi_4 <- sum(Xiaomi_4$Freq)

# Nokia
Nokia_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Nokia")
Nokia_4 <- sum(Nokia_4$Freq)

# Vivo
Vivo_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Vivo")
Vivo_4 <- sum(Vivo_4$Freq)

# Alcatel
Alcatel_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Alcatel")
Alcatel_4 <- sum(Alcatel_4$Freq)

# Krono
Krono_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Krono")
Krono_4 <- sum(Krono_4$Freq)

# ZTE
ZTE_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "ZTE")
ZTE_4 <- sum(ZTE_4$Freq)

# Tecno Mobile
TecnoMobile_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Tecno Mobile")
TecnoMobile_4 <- sum(TecnoMobile_4$Freq)

# Asus
Asus_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Asus")
Asus_4 <- sum(Asus_4$Freq)

# Blu
Blu_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Blu")
Blu_4 <- sum(Blu_4$Freq)

# Corn
Corn_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Corn")
Corn_4 <- sum(Corn_4$Freq)

# iPro
iPro_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "iPro")
iPro_4 <- sum(iPro_4$Freq)

# Oppo
Oppo_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Oppo")
Oppo_4 <- sum(Oppo_4$Freq)

# Caterpillar
Caterpillar_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Caterpillar")
Caterpillar_4 <- sum(Caterpillar_4$Freq)

# Infinix
Infinix_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Infinix")
Infinix_4 <- sum(Infinix_4$Freq)

# Lenovo
Lenovo_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Lenovo")
Lenovo_4 <- sum(Lenovo_4$Freq)

# Otro
Otro_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "Otro")
Otro_4 <- sum(Otro_4$Freq)

# No sabe / no responde
Nosabe_noresponde_4 <- CP310_4_ind %>% filter(CP310_4_ind$CP310_4 == "No sabe / no responde")
Nosabe_noresponde_4  <- sum(Nosabe_noresponde_4 $Freq)

###########################################################################################################################################################
# Año en que fue adquirido el teléfono 4

P310B_4_ind <- data.frame(svytable(~FACTOR + P310B_4,
                                   design = BD_survey))
# 1999
Año1999_4 <- P310B_4_ind %>% filter(P310B_4_ind$P310B_4 == "99")
Año1999_4 <- sum(Año1999_4$Freq)

# 2018
Año2018_4 <- P310B_4_ind %>% filter(P310B_4_ind$P310B_4 == "2018")
Año2018_4 <- sum(Año2018_4$Freq)

# 2019
Año2019_4 <- P310B_4_ind %>% filter(P310B_4_ind$P310B_4 == "2019")
Año2019_4 <- sum(Año2019_4$Freq)

# 2020
Año2020_4 <- P310B_4_ind %>% filter(P310B_4_ind$P310B_4 == "2020")
Año2020_4 <- sum(Año2020_4$Freq)

# 2021
Año2021_4 <- P310B_4_ind %>% filter(P310B_4_ind$P310B_4 == "2021")
Año2021_4 <- sum(Año2021_4$Freq)

#############################################################################################################################################
#############################################################################################################################################
# Telefono 5, ¿qué marca es?
CP310_5_ind <- data.frame(svytable(~FACTOR + CP310_5,
                                   design = BD_survey))
# Samsung
Samsung_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Samsung")
Samsung_5 <- sum(Samsung_5$Freq)

# LG
LG_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "LG")
LG_5 <- sum(LG_5$Freq)

# Sony
Sony_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Sony")
Sony_5 <- sum(Sony_5$Freq)

# Panasonic
Panasonic_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Panasonic")
Panasonic_5 <- sum(Panasonic_5$Freq)

# Kalley
Kalley_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Kalley")
Kalley_5 <- sum(Kalley_5$Freq)

# Challenger
Challenger_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Challenger")
Challenger_5 <- sum(Challenger_5$Freq)

# Daewo
Daewo_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Daewo")
Daewo_5 <- sum(Daewo_5$Freq)

# Caixun
Caixun_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Caixun")
Caixun_5 <- sum(Caixun_5$Freq)

# Hyundai
Hyundai_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Hyundai")
Hyundai_5 <- sum(Hyundai_5$Freq)

# Sharp
Sharp_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Sharp")
Sharp_5 <- sum(Sharp_5$Freq)

# Ssangyong
Ssangyong_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Ssangyong")
Ssangyong_5 <- sum(Ssangyong_5$Freq)

# Toshiba
Toshiba_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Toshiba")
Toshiba_5 <- sum(Toshiba_5$Freq)

# Olimpo
Olimpo_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Olimpo")
Olimpo_5 <- sum(Olimpo_5$Freq)

# Sankey
Sankey_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Sankey")
Sankey_5 <- sum(Sankey_5$Freq)

# Philips
Philips_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Philips")
Philips_5 <- sum(Philips_5$Freq)

# Iván Botero Gómez (IBG)
IBG_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Iván Botero Gómez (IBG)")
IBG_5 <- sum(IBG_5$Freq)

# Vizio
Vizio_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Vizio")
Vizio_5 <- sum(Vizio_5$Freq)

# Smart Visión TV
SmartTV_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Smart Visión TV")
SmartTV_5 <- sum(SmartTV_5$Freq)

# Simply
Simply_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Simply")
Simply_5 <- sum(Simply_5$Freq)

# AOC
AOC_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "AOC")
AOC_5 <- sum(AOC_5$Freq)

# Apple
Apple_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Apple")
Apple_5 <- sum(Apple_5$Freq)

# Huawei (Honor)
Huawei_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Huawei (Honor)")
Huawei_5 <- sum(Huawei_5$Freq)

# Motorola
Motorola_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Motorola")
Motorola_5 <- sum(Motorola_5$Freq)

# Xiaomi
Xiaomi_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Xiaomi")
Xiaomi_5 <- sum(Xiaomi_5$Freq)

# Nokia
Nokia_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Nokia")
Nokia_5 <- sum(Nokia_5$Freq)

# Vivo
Vivo_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Vivo")
Vivo_5 <- sum(Vivo_5$Freq)

# Alcatel
Alcatel_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Alcatel")
Alcatel_5 <- sum(Alcatel_5$Freq)

# Krono
Krono_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Krono")
Krono_5 <- sum(Krono_5$Freq)

# ZTE
ZTE_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "ZTE")
ZTE_5 <- sum(ZTE_5$Freq)

# Tecno Mobile
TecnoMobile_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Tecno Mobile")
TecnoMobile_5 <- sum(TecnoMobile_5$Freq)

# Asus
Asus_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Asus")
Asus_5 <- sum(Asus_5$Freq)

# Blu
Blu_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Blu")
Blu_5 <- sum(Blu_5$Freq)

# Corn
Corn_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Corn")
Corn_5 <- sum(Corn_5$Freq)

# iPro
iPro_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "iPro")
iPro_5 <- sum(iPro_5$Freq)

# Oppo
Oppo_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Oppo")
Oppo_5 <- sum(Oppo_5$Freq)

# Caterpillar
Caterpillar_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Caterpillar")
Caterpillar_5 <- sum(Caterpillar_5$Freq)

# Infinix
Infinix_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Infinix")
Infinix_5 <- sum(Infinix_5$Freq)

# Lenovo
Lenovo_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Lenovo")
Lenovo_5 <- sum(Lenovo_5$Freq)

# Otro
Otro_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "Otro")
Otro_5 <- sum(Otro_5$Freq)

# No sabe / no responde
Nosabe_noresponde_5 <- CP310_5_ind %>% filter(CP310_5_ind$CP310_5 == "No sabe / no responde")
Nosabe_noresponde_5  <- sum(Nosabe_noresponde_5 $Freq)

###########################################################################################################################################################
# Año en que fue adquirido el teléfono 5

P310B_5_ind <- data.frame(svytable(~FACTOR + P310B_5,
                                   design = BD_survey))
# 1999
Año1999_5 <- P310B_5_ind %>% filter(P310B_5_ind$P310B_5 == "99")
Año1999_5 <- sum(Año1999_5$Freq)

#############################################################################################################################################
#############################################################################################################################################
# Telefono 6, ¿qué marca es?
CP310_6_ind <- data.frame(svytable(~FACTOR + CP310_6,
                                   design = BD_survey))
# Samsung
Samsung_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Samsung")
Samsung_6 <- sum(Samsung_6$Freq)

# LG
LG_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "LG")
LG_6 <- sum(LG_6$Freq)

# Sony
Sony_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Sony")
Sony_6 <- sum(Sony_6$Freq)

# Panasonic
Panasonic_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Panasonic")
Panasonic_6 <- sum(Panasonic_6$Freq)

# Kalley
Kalley_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Kalley")
Kalley_6 <- sum(Kalley_6$Freq)

# Challenger
Challenger_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Challenger")
Challenger_6 <- sum(Challenger_6$Freq)

# Daewo
Daewo_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Daewo")
Daewo_6 <- sum(Daewo_6$Freq)

# Caixun
Caixun_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Caixun")
Caixun_6 <- sum(Caixun_6$Freq)

# Hyundai
Hyundai_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Hyundai")
Hyundai_6 <- sum(Hyundai_6$Freq)

# Sharp
Sharp_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Sharp")
Sharp_6 <- sum(Sharp_6$Freq)

# Ssangyong
Ssangyong_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Ssangyong")
Ssangyong_6 <- sum(Ssangyong_6$Freq)

# Toshiba
Toshiba_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Toshiba")
Toshiba_6 <- sum(Toshiba_6$Freq)

# Olimpo
Olimpo_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Olimpo")
Olimpo_6 <- sum(Olimpo_6$Freq)

# Sankey
Sankey_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Sankey")
Sankey_6 <- sum(Sankey_6$Freq)

# Philips
Philips_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Philips")
Philips_6 <- sum(Philips_6$Freq)

# Iván Botero Gómez (IBG)
IBG_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Iván Botero Gómez (IBG)")
IBG_6 <- sum(IBG_6$Freq)

# Vizio
Vizio_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Vizio")
Vizio_6 <- sum(Vizio_6$Freq)

# Smart Visión TV
SmartTV_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Smart Visión TV")
SmartTV_6 <- sum(SmartTV_6$Freq)

# Simply
Simply_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Simply")
Simply_6 <- sum(Simply_6$Freq)

# AOC
AOC_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "AOC")
AOC_6 <- sum(AOC_6$Freq)

# Apple
Apple_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Apple")
Apple_6 <- sum(Apple_6$Freq)

# Huawei (Honor)
Huawei_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Huawei (Honor)")
Huawei_6 <- sum(Huawei_6$Freq)

# Motorola
Motorola_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Motorola")
Motorola_6 <- sum(Motorola_6$Freq)

# Xiaomi
Xiaomi_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Xiaomi")
Xiaomi_6 <- sum(Xiaomi_6$Freq)

# Nokia
Nokia_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Nokia")
Nokia_6 <- sum(Nokia_6$Freq)

# Vivo
Vivo_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Vivo")
Vivo_6 <- sum(Vivo_6$Freq)

# Alcatel
Alcatel_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Alcatel")
Alcatel_6 <- sum(Alcatel_6$Freq)

# Krono
Krono_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Krono")
Krono_6 <- sum(Krono_6$Freq)

# ZTE
ZTE_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "ZTE")
ZTE_6 <- sum(ZTE_6$Freq)

# Tecno Mobile
TecnoMobile_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Tecno Mobile")
TecnoMobile_6 <- sum(TecnoMobile_6$Freq)

# Asus
Asus_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Asus")
Asus_6 <- sum(Asus_6$Freq)

# Blu
Blu_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Blu")
Blu_6 <- sum(Blu_6$Freq)

# Corn
Corn_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Corn")
Corn_6 <- sum(Corn_6$Freq)

# iPro
iPro_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "iPro")
iPro_6 <- sum(iPro_6$Freq)

# Oppo
Oppo_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Oppo")
Oppo_6 <- sum(Oppo_6$Freq)

# Caterpillar
Caterpillar_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Caterpillar")
Caterpillar_6 <- sum(Caterpillar_6$Freq)

# Infinix
Infinix_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Infinix")
Infinix_6 <- sum(Infinix_6$Freq)

# Lenovo
Lenovo_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Lenovo")
Lenovo_6 <- sum(Lenovo_6$Freq)

# Otro
Otro_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "Otro")
Otro_6 <- sum(Otro_6$Freq)

# No sabe / no responde
Nosabe_noresponde_6 <- CP310_6_ind %>% filter(CP310_6_ind$CP310_6 == "No sabe / no responde")
Nosabe_noresponde_6  <- sum(Nosabe_noresponde_6 $Freq)

###########################################################################################################################################################
# Año en que fue adquirido el teléfono 6

P310B_6_ind <- data.frame(svytable(~FACTOR + P310B_6,
                                   design = BD_survey))
# 1999
Año1999_6 <- P310B_6_ind %>% filter(P310B_6_ind$P310B_6 == "99")
Año1999_6 <- sum(Año1999_6$Freq)

############################################################################################################################################################
############################################################################################################################################################
# Con respecto al internet móvil que usted tiene en su smartphone/Iphone que utiliza con mayor frecuencia,
# ¿qué tipo de plan tiene?
P311_ind <- data.frame(svytable(~FACTOR + P311,
                                design = BD_survey))
# PREPAGO 
Prepago <- P311_ind %>% filter(P311_ind$P311 == "PREPAGO (se hace recarga)")
Prepago <- sum(Prepago$Freq)

# POSPAGO
Pospago <- P311_ind %>% filter(P311_ind$P311 == "POS-PAGO (se recibe factura)")
Pospago <- sum(Pospago$Freq)

##############################################################################################################################################################
##############################################################################################################################################################
# De acuerdo al operador de internet móvil que posee, en una escala de 1 a 10 en donde 1 es "Muy malo" y 10 es "Muy bueno",
# ¿cómo califica el servicio que le presta el operador de internet móvil que tiene actualmente?
P319_ind <- data.frame(svytable(~FACTOR + P319,
                                design = BD_survey))
# Muy malo
P319_Muymalo <- P319_ind %>% filter(P319_ind$P319 == "Muy malo") 
P319_Muymalo <- sum(P319_Muymalo$Freq)

# 2
P319_2 <- P319_ind %>% filter(P319_ind$P319 == "2") 
P319_2 <- sum(P319_2$Freq)

# 3
P319_3 <- P319_ind %>% filter(P319_ind$P319 == "3") 
P319_3 <- sum(P319_3$Freq)

# 4
P319_4 <- P319_ind %>% filter(P319_ind$P319 == "4") 
P319_4 <- sum(P319_4$Freq)

# 5
P319_5 <- P319_ind %>% filter(P319_ind$P319 == "5") 
P319_5 <- sum(P319_5$Freq)

# 6
P319_6 <- P319_ind %>% filter(P319_ind$P319 == "6") 
P319_6 <- sum(P319_6$Freq)

# 7
P319_7 <- P319_ind %>% filter(P319_ind$P319 == "7") 
P319_7 <- sum(P319_7$Freq)

# 8
P319_8 <- P319_ind %>% filter(P319_ind$P319 == "8") 
P319_8 <- sum(P319_8$Freq)

# 9
P319_9 <- P319_ind %>% filter(P319_ind$P319 == "9") 
P319_9 <- sum(P319_9$Freq)

# Muy bueno
P319_Muybueno <- P319_ind %>% filter(P319_ind$P319 == "Muy bueno") 
P319_Muybueno <- sum(P319_Muybueno$Freq)

##############################################################################################################################################
# a) La velocidad/calidad en la navegación, es decir, el tiempo que se demoran en cargar las páginas o aplicaciones según el plan contratado
P320_1_ind <- data.frame(svytable(~FACTOR + P320_1,
                                  design = BD_survey))

# Muy malo
P320_1_Muymalo <- P320_1_ind %>% filter(P320_1_ind$P320_1 == "Muy malo") 
P320_1_Muymalo <- sum(P320_1_Muymalo$Freq)

# 2
P320_1_2 <- P320_1_ind %>% filter(P320_1_ind$P320_1 == "2") 
P320_1_2 <- sum(P320_1_2$Freq)

# 3
P320_1_3 <- P320_1_ind %>% filter(P320_1_ind$P320_1 == "3") 
P320_1_3 <- sum(P320_1_3$Freq)

# 4
P320_1_4 <- P320_1_ind %>% filter(P320_1_ind$P320_1 == "4") 
P320_1_4 <- sum(P320_1_4$Freq)

# 5
P320_1_5 <- P320_1_ind %>% filter(P320_1_ind$P320_1 == "5") 
P320_1_5 <- sum(P320_1_5$Freq)

# 6
P320_1_6 <- P320_1_ind %>% filter(P320_1_ind$P320_1 == "6") 
P320_1_6 <- sum(P320_1_6$Freq)

# 7
P320_1_7 <- P320_1_ind %>% filter(P320_1_ind$P320_1 == "7") 
P320_1_7 <- sum(P320_1_7$Freq)

# 8
P320_1_8 <- P320_1_ind %>% filter(P320_1_ind$P320_1 == "8") 
P320_1_8 <- sum(P320_1_8$Freq)

# 9
P320_1_9 <- P320_1_ind %>% filter(P320_1_ind$P320_1 == "9") 
P320_1_9 <- sum(P320_1_9$Freq)

# Muy bueno
P320_1_Muybueno <- P320_1_ind %>% filter(P320_1_ind$P320_1 == "Muy bueno") 
P320_1_Muybueno <- sum(P320_1_Muybueno$Freq)

###################################################################################################################################################
# b) La continuidad de la conexión, es decir, mientras se está navegando no hay caídas ni fallas en la red

P320_2_ind <- data.frame(svytable(~FACTOR + P320_2,
                                  design = BD_survey))

# Muy malo
P320_2_Muymalo <- P320_2_ind %>% filter(P320_2_ind$P320_2 == "Muy malo") 
P320_2_Muymalo <- sum(P320_2_Muymalo$Freq)

# 2
P320_2_2 <- P320_2_ind %>% filter(P320_2_ind$P320_2 == "2") 
P320_2_2 <- sum(P320_2_2$Freq)

# 3
P320_2_3 <- P320_2_ind %>% filter(P320_2_ind$P320_2 == "3") 
P320_2_3 <- sum(P320_2_3$Freq)

# 4
P320_2_4 <- P320_2_ind %>% filter(P320_2_ind$P320_2 == "4") 
P320_2_4 <- sum(P320_2_4$Freq)

# 5
P320_2_5 <- P320_2_ind %>% filter(P320_2_ind$P320_2 == "5") 
P320_2_5 <- sum(P320_2_5$Freq)

# 6
P320_2_6 <- P320_2_ind %>% filter(P320_2_ind$P320_2 == "6") 
P320_2_6 <- sum(P320_2_6$Freq)

# 7
P320_2_7 <- P320_2_ind %>% filter(P320_2_ind$P320_2 == "7") 
P320_2_7 <- sum(P320_2_7$Freq)

# 8
P320_2_8 <- P320_2_ind %>% filter(P320_2_ind$P320_2 == "8") 
P320_2_8 <- sum(P320_2_8$Freq)

# 9
P320_2_9 <- P320_2_ind %>% filter(P320_2_ind$P320_2 == "9") 
P320_2_9 <- sum(P320_2_9$Freq)

# Muy bueno
P320_2_Muybueno <- P320_2_ind %>% filter(P320_2_ind$P320_2 == "Muy bueno") 
P320_2_Muybueno <- sum(P320_2_Muybueno$Freq)

#############################################################################################################################################
# d) La calidad de la señal cuando se hacen llamadas por internet
P320_4_ind <- data.frame(svytable(~FACTOR + P320_4,
                                  design = BD_survey))

# Muy malo
P320_4_Muymalo <- P320_4_ind %>% filter(P320_4_ind$P320_4 == "Muy malo") 
P320_4_Muymalo <- sum(P320_4_Muymalo$Freq)

# 2
P320_4_2 <- P320_4_ind %>% filter(P320_4_ind$P320_4 == "2") 
P320_4_2 <- sum(P320_4_2$Freq)

# 3
P320_4_3 <- P320_4_ind %>% filter(P320_4_ind$P320_4 == "3") 
P320_4_3 <- sum(P320_4_3$Freq)

# 4
P320_4_4 <- P320_4_ind %>% filter(P320_4_ind$P320_4 == "4") 
P320_4_4 <- sum(P320_4_4$Freq)

# 5
P320_4_5 <- P320_4_ind %>% filter(P320_4_ind$P320_4 == "5") 
P320_4_5 <- sum(P320_4_5$Freq)

# 6
P320_4_6 <- P320_4_ind %>% filter(P320_4_ind$P320_4 == "6") 
P320_4_6 <- sum(P320_4_6$Freq)

# 7
P320_4_7 <- P320_4_ind %>% filter(P320_4_ind$P320_4 == "7") 
P320_4_7 <- sum(P320_4_7$Freq)

# 8
P320_4_8 <- P320_4_ind %>% filter(P320_4_ind$P320_4 == "8") 
P320_4_8 <- sum(P320_4_8$Freq)

# 9
P320_4_9 <- P320_4_ind %>% filter(P320_4_ind$P320_4 == "9") 
P320_4_9 <- sum(P320_4_9$Freq)

# Muy bueno
P320_4_Muybueno <- P320_4_ind %>% filter(P320_4_ind$P320_4 == "Muy bueno") 
P320_4_Muybueno <- sum(P320_4_Muybueno$Freq)

#################################################################################################################################################
# e) Poder acceder a mi internet móvil en cualquier lugar
P320_5_ind <- data.frame(svytable(~FACTOR + P320_5,
                                  design = BD_survey))

# Muy malo
P320_5_Muymalo <- P320_5_ind %>% filter(P320_5_ind$P320_5 == "Muy malo") 
P320_5_Muymalo <- sum(P320_5_Muymalo$Freq)

# 2
P320_5_2 <- P320_5_ind %>% filter(P320_5_ind$P320_5 == "2") 
P320_5_2 <- sum(P320_5_2$Freq)

# 3
P320_5_3 <- P320_5_ind %>% filter(P320_5_ind$P320_5 == "3") 
P320_5_3 <- sum(P320_5_3$Freq)

# 4
P320_5_4 <- P320_5_ind %>% filter(P320_5_ind$P320_5 == "4") 
P320_5_4 <- sum(P320_5_4$Freq)

# 5
P320_5_5 <- P320_5_ind %>% filter(P320_5_ind$P320_5 == "5") 
P320_5_5 <- sum(P320_5_5$Freq)

# 6
P320_5_6 <- P320_5_ind %>% filter(P320_5_ind$P320_5 == "6") 
P320_5_6 <- sum(P320_5_6$Freq)

# 7
P320_5_7 <- P320_5_ind %>% filter(P320_5_ind$P320_5 == "7") 
P320_5_7 <- sum(P320_5_7$Freq)

# 8
P320_5_8 <- P320_5_ind %>% filter(P320_5_ind$P320_5 == "8") 
P320_5_8 <- sum(P320_5_8$Freq)

# 9
P320_5_9 <- P320_5_ind %>% filter(P320_5_ind$P320_5 == "9") 
P320_5_9 <- sum(P320_5_9$Freq)

# Muy bueno
P320_5_Muybueno <- P320_5_ind %>% filter(P320_5_ind$P320_5 == "Muy bueno") 
P320_5_Muybueno <- sum(P320_5_2$Freq)

###################################################################################################################################################
# De las siguientes opciones, ¿cuál describe mejor su opinión respecto a la calidad entregada por su operador de internet móvil?
P322_ind <- data.frame(svytable(~FACTOR + P322,
                                design = BD_survey))
# El servicio que me ofrecen es muy costoso  respecto a la calidad
P322_1 <- P322_ind %>% filter(P322_ind$P322 == "El servicio que me ofrecen es muy costoso  respecto a la cal")
P322_1 <- sum(P322_1$Freq)

# El servicio que me ofrecen es algo costoso  respecto a la calidad
P322_2 <- P322_ind %>% filter(P322_ind$P322 == "El servicio que me ofrecen es algo costoso  respecto a la ca")
P322_2 <- sum(P322_2$Freq)

# El servicio que me ofrecen tiene un precio justo respecto a la calidad
P322_3 <- P322_ind %>% filter(P322_ind$P322 == "El servicio que me ofrecen tiene un precio justo respecto a")
P322_3 <- sum(P322_3$Freq)

# El servicio que me ofrecen tiene un costo algo bajo respecto a la calidad
P322_4 <- P322_ind %>% filter(P322_ind$P322 == "El servicio que me ofrecen tiene un costo algo bajo respecto")
P322_4 <- sum(P322_4$Freq)

# El servicio que me ofrecen tiene un costo muy bajo respecto a la calidad
P322_5 <- P322_ind %>% filter(P322_ind$P322 == "El servicio que me ofrecen tiene un costo muy bajo respecto")
P322_5 <- sum(P322_5$Freq)


