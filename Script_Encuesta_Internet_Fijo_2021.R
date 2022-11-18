########## Encuesta de internet fijo - CRC ####################
##########           2021             ##########

# Limpiar el ambiente de trabajo
rm(list = ls())

# cargando paquetes a utilizar 
library(tidyverse)
library(readxl)
library(survey)
library(dplyr)

# Definiendo el directorio para la base completa en dta
setwd("C:/Users/sebas/OneDrive/Alex/Internet/CRC/Internet fijo")

# importando base de datos 
EIF_2021 <- read_xlsx("C:\\Users\\sebas\\OneDrive\\Alex\\Internet\\CRC\\Internet fijo\\Internet_fijo.xlsx", sheet = "CC128403_BASE_INTERNET_FIJO_ETI")

# seleccionando variables a utilizar

EIF_2021_def <- EIF_2021[c(1, 2, 3, 4, 9, 11, 12, 13, 14, 16, 17, 18, 19, 20, 21, 22, 39, 40, 41, 42, 43, 79:84, 
                           87:88, 90:97, 100:103, 108:112, 117:124, 132:133, 135, 143:148, 310)]

# conviertiendo variables
EIF_2021_def$GENERO <- factor(EIF_2021_def$GENERO)

EIF_2021_def$ESTRATO <- factor(EIF_2021_def$ESTRATO)
EIF_2021_def$REDAD <- factor(EIF_2021_def$REDAD)

# transformando el data frame a enucesta
BD_survey <- svydesign(ids=~1, data=EIF_2021_def, weights=EIF_2021_def$FACTOR)
class(BD_survey)

# Guardar base de indicadores
saveRDS(BD_survey, "BD_survey_indicadores.rds")

#############################################################################################################################################
# Total personas 
Total_personas <- data.frame(svytable(~FACTOR + REGISTRO,
                                      design = BD_survey))
Total_personas_nacional <- sum(Total_personas$Freq)
#############################################################################################################################################
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

###################################################################################################################################
# GENERO
GENERO_ind <- data.frame(svytable(~FACTOR + GENERO,
                                  design = BD_survey))
# Femenino
Femenino_nacional <- GENERO_ind %>% filter(GENERO_ind$GENERO == "Femenino")
Femenino_nacional <- sum(Femenino_nacional$Freq)
# Masculino
Masculino_nacional <- GENERO_ind %>% filter(GENERO_ind$GENERO == "Masculino")
Masculino_nacional <- sum(Masculino_nacional$Freq)

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
# Televisión Abierta - No es por suscripción y es gratuita
Telev_abier_gratui <- P4_1_ind %>% filter(P4_1_ind$P4_1 == "Televisión Abierta - No es por suscripción y es gratuita")
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
# Televisión Abierta - No es por suscripción y es gratuita
Telev_abier_gratui_2 <- P4_2_ind %>% filter(P4_2_ind$P4_2 == "Televisión Abierta - No es por suscripción y es gratuita")
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
# Televisión Abierta - No es por suscripción y es gratuita
Telev_abier_gratui_3 <- P4_3_ind %>% filter(P4_3_ind$P4_3 == "Televisión Abierta - No es por suscripción y es gratuita")
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
# Televisión Abierta - No es por suscripción y es gratuita
Telev_abier_gratui_4 <- P4_4_ind %>% filter(P4_4_ind$P4_4 == "Televisión Abierta - No es por suscripción y es gratuita")
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
# Televisión Abierta - No es por suscripción y es gratuita
Telev_abier_gratui_5 <- P4_5_ind %>% filter(P4_5_ind$P4_5 == "Televisión Abierta - No es por suscripción y es gratuita")
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
# Televisión Abierta - No es por suscripción y es gratuita
Telev_abier_gratui_6 <- P4_6_ind %>% filter(P4_6_ind$P4_6 == "Televisión Abierta - No es por suscripción y es gratuita")
Telev_abier_gratui_6 <- sum(Telev_abier_gratui_6$Freq)


########################################################################################################################################
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
################################################################################################################################3
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
######################################################################################################################################
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
############################################################################################################################################3
# ¿Cuál de las siguientes opciones describe mejor su ocupación actual?
P55_ind <- data.frame(svytable(~FACTOR + P55,
                               design = BD_survey))
# Estudiante
Estudiante <- P55_ind %>% filter(P55_ind$P55 == "Estudiante")
Estudiante <- sum(Estudiante$Freq)
# Empleado
Empleado <- P55_ind %>% filter(P55_ind$P55 == "Empleado")
Empleado <- sum(Empleado$Freq)
# Independiente
Independiente <- P55_ind %>% filter(P55_ind$P55 == "Independiente")
Independiente <- sum(Independiente$Freq)
# Empresario
Empresario <- P55_ind %>% filter(P55_ind$P55 == "Empresario")
Empresario <- sum(Empresario$Freq)
# Desempleado
Desempleado <- P55_ind %>% filter(P55_ind$P55 == "Desempleado")
Desempleado <- sum(Desempleado$Freq)
# Incapacitado
Incapacitado <- P55_ind %>% filter(P55_ind$P55 == "Incapacitado")
Incapacitado <- sum(Incapacitado$Freq)
# Ama de casa
Amadecasa <- P55_ind %>% filter(P55_ind$P55 == "Ama de casa")
Amadecasa <- sum(Amadecasa$Freq)
# Pensionado
Pensionado <- P55_ind %>% filter(P55_ind$P55 == "Pensionado")
Pensionado <- sum(Pensionado$Freq)
# Inversionista
Inversionista <- P55_ind %>% filter(P55_ind$P55 == "Inversionista")
Inversionista <- sum(Inversionista$Freq)
# Estudia/trabaja
Estudia_trabaja <- P55_ind %>% filter(P55_ind$P55 == "Estudia/trabaja")
Estudia_trabaja <- sum(Estudia_trabaja$Freq)
#############################################################################################################################
# ¿Si la empresa para la que se realiza este estudio quisiera establecer planes de acción en su beneficio, ¿podría tener autorización para conocer sus respuestas en esta encuesta?
P56_ind <- data.frame(svytable(~FACTOR + P56,
                               design = BD_survey))
# SI
P56_Si <- P56_ind %>% filter(P56_ind$P56 == "SÍ")
P56_Si <- sum(P56_Si$Freq)
# NO
P56_No <- P56_ind %>% filter(P56_ind$P56 == "NO")
P56_No <- sum(P56_No$Freq)
##################################################################################################################################################################################################
# Usted me dice que tiene internet fijo en su casa, ¿a qué operador pertenece este servicio?
P406_ind <- data.frame(svytable(~FACTOR + P406,
                                design = BD_survey))
# Claro
Claro <- P406_ind %>% filter(P406_ind$P406 == "Claro")
claro <- sum(Claro$Freq)
# Movistar/Telefónica
Movistartelefonica <- P406_ind %>% filter(P406_ind$P406 == "Movistar/Telefónica")
Movistartelefonica <- sum(Movistartelefonica$Freq)
# Tigo/Une
Tigo_Une <- P406_ind %>% filter(P406_ind$P406 == "Tigo/Une")
Tigo_Une <- sum(Tigo_Une$Freq)
# ETB
ETB <- P406_ind %>% filter(P406_ind$P406 == "ETB")
ETB <- sum(ETB$Freq)
# Tele Bucaramanga/Movistar
TeleBucaramanga_Movistar <- P406_ind %>% filter(P406_ind$P406 == "Tele Bucaramanga/Movistar")
TeleBucaramanga_Movistar <- sum(TeleBucaramanga_Movistar$Freq)
# Metrotel/Movistar
Metrotel_Movistar <- P406_ind %>% filter(P406_ind$P406 == "Metrotel/Movistar")
Metrotel_Movistar <- sum(Metrotel_Movistar$Freq)
# Sol Cable Visión
SolCableVisión <- P406_ind %>% filter(P406_ind$P406 == "Sol Cable Visión")
SolCableVisión <- sum(SolCableVisión$Freq)
# DIRECTV Colombia
DIRECTVColombia <- P406_ind %>% filter(P406_ind$P406 == "DIRECTV Colombia")
DIRECTVColombia <- sum(DIRECTVColombia$Freq)
# Edatel
Edatel <- P406_ind %>% filter(P406_ind$P406 == "Edatel")
Edatel <- sum(Edatel$Freq)
# Emcali
Emcali <- P406_ind %>% filter(P406_ind$P406 == "Emcali")
Emcali <- sum(Emcali$Freq)
# Cable Éxito
CableÉxito <- P406_ind %>% filter(P406_ind$P406 == "Cable Éxito")
CableÉxito <- sum(CableÉxito$Freq)
# Cabletelco
Cabletelco <- P406_ind %>% filter(P406_ind$P406 == "Cabletelco")
Cabletelco<- sum(Cabletelco$Freq)
# Cablem@s
Cablemos <- P406_ind %>% filter(P406_ind$P406 == "Cablem@s")
Cablemos <- sum(Cablemos$Freq)
# Conexión Digital
ConexiónDigital <- P406_ind %>% filter(P406_ind$P406 == "Conexión Digital")
ConexiónDigital <- sum(ConexiónDigital$Freq)
# HV Multiplay
HVMultiplay <- P406_ind %>% filter(P406_ind$P406 == "HV Multiplay")
HVMultiplay <- sum(HVMultiplay$Freq)
# Servinet
Servinet <- P406_ind %>% filter(P406_ind$P406 == "Servinet")
Servinet <- sum(Servinet$Freq)
# Velonet
Velonet <- P406_ind %>% filter(P406_ind$P406 == "Velonet")
Velonet <- sum(Velonet$Freq)
# Tricom Telecomunicaciones
TricomTelecomunicaciones <- P406_ind %>% filter(P406_ind$P406 == "Tricom Telecomunicaciones")
TricomTelecomunicaciones <- sum(TricomTelecomunicaciones$Freq)
# Colcable
Colcable <- P406_ind %>% filter(P406_ind$P406 == "Colcable")
Colcable <- sum(Colcable$Freq)
# Tv Isla
TvIsla <- P406_ind %>% filter(P406_ind$P406 == "Tv Isla")
TvIsla <- sum(TvIsla$Freq)
# Comunal / de barrio
Comunaldebarrio <- P406_ind %>% filter(P406_ind$P406 == "Comunal / de barrio")
Comunaldebarrio <- sum(Comunaldebarrio$Freq)
# Otro
P406_otro <- P406_ind %>% filter(P406_ind$P406 == "Otro")
P406_otro <- sum(P406_otro$Freq)
# No sabe / no responde
P406_NS <- P406_ind %>% filter(P406_ind$P406 == "No sabe / no responde")
P406_NS <- sum(P406_NS$Freq)
###################################################################################################################################################33
# ¿Usted conoce la velocidad contratada para el internet fijo de su casa?
P408_ind <- data.frame(svytable(~FACTOR + P408,
                                design = BD_survey))
# SÍ
P408_Si <- P408_ind %>% filter(P408_ind$P408 == "SÍ")
P408_Si <- sum(P408_Si$Freq)
# NO
P408_NO <- P408_ind %>% filter(P408_ind$P408 == "NO")
P408_NO <- sum(P408_NO$Freq)
####################################################################################################################################################
# ¿Qué velocidad de internet tiene en su hogar?
P409_ind <- data.frame(svytable(~FACTOR + P409,
                                design = BD_survey))
# 1 Mega
mega1 <- P409_ind %>% filter(P409_ind$P409 == "1 Mega")
mega1 <- sum(mega1$Freq)
# 2 Mega
mega2 <- P409_ind %>% filter(P409_ind$P409 == "2 Megas")
mega2 <- sum(mega2$Freq)
# 3 Mega
mega3 <- P409_ind %>% filter(P409_ind$P409 == "3 Megas")
mega3 <- sum(mega3$Freq)
# 4 Mega
mega4 <- P409_ind %>% filter(P409_ind$P409 == "4 Megas")
mega4 <- sum(mega4$Freq)
# 5 Mega
mega5 <- P409_ind %>% filter(P409_ind$P409 == "5 Megas")
mega5 <- sum(mega5$Freq)
# 6 Mega
mega6 <- P409_ind %>% filter(P409_ind$P409 == "6 Megas")
mega6 <- sum(mega6$Freq)
# 10 Mega
mega10 <- P409_ind %>% filter(P409_ind$P409 == "10 Megas")
mega10 <- sum(mega10$Freq)
# 20 Mega
mega20 <- P409_ind %>% filter(P409_ind$P409 == "20 Megas")
mega20 <- sum(mega20$Freq)
# 50 Mega
mega50 <- P409_ind %>% filter(P409_ind$P409 == "50 Megas")
mega50 <- sum(mega50$Freq)
# Más de 50 Megas
mega50mas <- P409_ind %>% filter(P409_ind$P409 == "Más de 50 Megas")
mega50mas <- sum(mega50mas$Freq)
# 15 Mega
mega15 <- P409_ind %>% filter(P409_ind$P409 == "15 Megas")
mega15 <- sum(mega15$Freq)
# 25 Mega
mega25 <- P409_ind %>% filter(P409_ind$P409 == "25 Megas")
mega25 <- sum(mega25$Freq) 
# 30 Mega
mega30 <- P409_ind %>% filter(P409_ind$P409 == "30 Megas")
mega30 <- sum(mega30$Freq) 
# 40 Mega
mega40 <- P409_ind %>% filter(P409_ind$P409 == "40 megas")
mega40 <- sum(mega40$Freq) 
#################################################################################################################################################
# ¿Cuántas personas, máximo, se llegan a conectar al mismo tiempo al internet fijo en el hogar?
P411_ind <-  data.frame(svytable(~FACTOR + P411,
                                    design = BD_survey))
# 1
P411_1 <- P411_ind %>% filter(P411_ind$P411 == "1")
P411_1 <- sum(P411_1$Freq)
# 2
P411_2 <- P411_ind %>% filter(P411_ind$P411 == "2")
P411_2 <- sum(P411_2$Freq)
# 3
P411_3 <- P411_ind %>% filter(P411_ind$P411 == "3")
P411_3 <- sum(P411_3$Freq)
# 4
P411_4 <- P411_ind %>% filter(P411_ind$P411 == "4")
P411_4 <- sum(P411_4$Freq)
# 5
P411_5 <- P411_ind %>% filter(P411_ind$P411 == "5")
P411_5 <- sum(P411_5$Freq)
# Más de 5
P411_mas5 <- P411_ind %>% filter(P411_ind$P411 == "Más de 5")
P411_mas5 <- sum(P411_mas5$Freq)
##############################################################################################################################################################
#  ¿A través de que dispositivos se conectan al internet fijo los diferentes miembros en el hogar? 1
P412_1_ind <- data.frame(svytable(~FACTOR + P412_1,
                              design = BD_survey))
# celular 
celular_1 <- P412_1_ind %>% filter(P412_1_ind$P412_1 == "Celular")
celular_1 <- sum(celular_1$Freq)
# celular 
Tablet_1 <- P412_1_ind %>% filter(P412_1_ind$P412_1 == "Tablet")
Tablet_1 <- sum(Tablet_1$Freq)
# Computador de mesa
Computadordemesa_1 <- P412_1_ind %>% filter(P412_1_ind$P412_1 == "Computador de mesa")
Computadordemesa_1 <- sum(Computadordemesa_1$Freq)
# Portátil
Portátil_1 <- P412_1_ind %>% filter(P412_1_ind$P412_1 == "Portátil")
Portátil_1 <- sum(Portátil_1$Freq)
# Televisor
Televisor_1 <- P412_1_ind %>% filter(P412_1_ind$P412_1 == "Televisor")
Televisor_1 <- sum(Televisor_1$Freq)
# Consola de video juegos
Consoladevideojuegos_1 <- P412_1_ind %>% filter(P412_1_ind$P412_1 == "Consola de video juegos")
Consoladevideojuegos_1 <- sum(Consoladevideojuegos_1$Freq)
# Smartwatch / reloj inteligente
Smartwatch_relojinteligente_1 <- P412_1_ind %>% filter(P412_1_ind$P412_1 == "Smartwatch / reloj inteligente")
Smartwatch_relojinteligente_1 <- sum(Smartwatch_relojinteligente_1$Freq)
# No sabe / no responde
P412_1_NS <- P412_1_ind %>% filter(P412_1_ind$P412_1 == "No sabe / no responde")
P412_1_NS <- sum(P412_1_NS$Freq)
###############################################################################################################################################################
#  ¿A través de que dispositivos se conectan al internet fijo los diferentes miembros en el hogar? 2
P412_2_ind <- data.frame(svytable(~FACTOR + P412_2,
                                  design = BD_survey))
# celular 
celular_2 <- P412_2_ind %>% filter(P412_2_ind$P412_2 == "Celular")
celular_2 <- sum(celular_2$Freq)
# celular 
Tablet_2 <- P412_2_ind %>% filter(P412_2_ind$P412_2 == "Tablet")
Tablet_2 <- sum(Tablet_2$Freq)
# Computador de mesa
Computadordemesa_2 <- P412_2_ind %>% filter(P412_2_ind$P412_2 == "Computador de mesa")
Computadordemesa_2 <- sum(Computadordemesa_2$Freq)
# Portátil
Portátil_2 <- P412_2_ind %>% filter(P412_2_ind$P412_2 == "Portátil")
Portátil_2 <- sum(Portátil_2$Freq)
# Televisor
Televisor_2 <- P412_2_ind %>% filter(P412_2_ind$P412_2 == "Televisor")
Televisor_2 <- sum(Televisor_2$Freq)
# Consola de video juegos
Consoladevideojuegos_2 <- P412_2_ind %>% filter(P412_2_ind$P412_2 == "Consola de video juegos")
Consoladevideojuegos_2 <- sum(Consoladevideojuegos_2$Freq)
# Smartwatch / reloj inteligente
Smartwatch_relojinteligente_2 <- P412_2_ind %>% filter(P412_2_ind$P412_2 == "Smartwatch / reloj inteligente")
Smartwatch_relojinteligente_2 <- sum(Smartwatch_relojinteligente_2$Freq)
# No sabe / no responde
P412_2_NS <- P412_2_ind %>% filter(P412_2_ind$P412_2 == "No sabe / no responde")
P412_2_NS <- sum(P412_2_NS$Freq)
###############################################################################################################################################################
#  ¿A través de que dispositivos se conectan al internet fijo los diferentes miembros en el hogar? 3
P412_3_ind <- data.frame(svytable(~FACTOR + P412_3,
                                  design = BD_survey))
# celular 
celular_3 <- P412_3_ind %>% filter(P412_3_ind$P412_3 == "Celular")
celular_3 <- sum(celular_3$Freq)
# celular 
Tablet_3 <- P412_3_ind %>% filter(P412_3_ind$P412_3 == "Tablet")
Tablet_3 <- sum(Tablet_3$Freq)
# Computador de mesa
Computadordemesa_3 <- P412_3_ind %>% filter(P412_3_ind$P412_3 == "Computador de mesa")
Computadordemesa_3 <- sum(Computadordemesa_3$Freq)
# Portátil
Portátil_3 <- P412_3_ind %>% filter(P412_3_ind$P412_3 == "Portátil")
Portátil_3 <- sum(Portátil_3$Freq)
# Televisor
Televisor_3 <- P412_3_ind %>% filter(P412_3_ind$P412_3 == "Televisor")
Televisor_3 <- sum(Televisor_3$Freq)
# Consola de video juegos
Consoladevideojuegos_3 <- P412_3_ind %>% filter(P412_3_ind$P412_3 == "Consola de video juegos")
Consoladevideojuegos_3 <- sum(Consoladevideojuegos_3$Freq)
# Smartwatch / reloj inteligente
Smartwatch_relojinteligente_3 <- P412_3_ind %>% filter(P412_3_ind$P412_3 == "Smartwatch / reloj inteligente")
Smartwatch_relojinteligente_3 <- sum(Smartwatch_relojinteligente_3$Freq)
# No sabe / no responde
P412_3_NS <- P412_3_ind %>% filter(P412_3_ind$P412_3 == "No sabe / no responde")
P412_3_NS <- sum(P412_3_NS$Freq)
###############################################################################################################################################################
#  ¿A través de que dispositivos se conectan al internet fijo los diferentes miembros en el hogar? 4
P412_4_ind <- data.frame(svytable(~FACTOR + P412_4,
                                  design = BD_survey))
# celular 
celular_4 <- P412_4_ind %>% filter(P412_4_ind$P412_4 == "Celular")
celular_4 <- sum(celular_4$Freq)
# celular 
Tablet_4 <- P412_4_ind %>% filter(P412_4_ind$P412_4 == "Tablet")
Tablet_4 <- sum(Tablet_4$Freq)
# Computador de mesa
Computadordemesa_4 <- P412_4_ind %>% filter(P412_4_ind$P412_4 == "Computador de mesa")
Computadordemesa_4 <- sum(Computadordemesa_4$Freq)
# Portátil
Portátil_4 <- P412_4_ind %>% filter(P412_4_ind$P412_4 == "Portátil")
Portátil_4 <- sum(Portátil_4$Freq)
# Televisor
Televisor_4 <- P412_4_ind %>% filter(P412_4_ind$P412_4 == "Televisor")
Televisor_4 <- sum(Televisor_4$Freq)
# Consola de video juegos
Consoladevideojuegos_4 <- P412_4_ind %>% filter(P412_4_ind$P412_4 == "Consola de video juegos")
Consoladevideojuegos_4 <- sum(Consoladevideojuegos_4$Freq)
# Smartwatch / reloj inteligente
Smartwatch_relojinteligente_4 <- P412_4_ind %>% filter(P412_4_ind$P412_4 == "Smartwatch / reloj inteligente")
Smartwatch_relojinteligente_4 <- sum(Smartwatch_relojinteligente_4$Freq)
# No sabe / no responde
P412_4_NS <- P412_4_ind %>% filter(P412_4_ind$P412_4 == "No sabe / no responde")
P412_4_NS <- sum(P412_4_NS$Freq)
###############################################################################################################################################################
#  ¿A través de que dispositivos se conectan al internet fijo los diferentes miembros en el hogar? 5
P412_5_ind <- data.frame(svytable(~FACTOR + P412_5,
                                  design = BD_survey))
# celular 
celular_5 <- P412_5_ind %>% filter(P412_5_ind$P412_5 == "Celular")
celular_5 <- sum(celular_5$Freq)
# celular 
Tablet_5 <- P412_5_ind %>% filter(P412_5_ind$P412_5 == "Tablet")
Tablet_5 <- sum(Tablet_5$Freq)
# Computador de mesa
Computadordemesa_5 <- P412_5_ind %>% filter(P412_5_ind$P412_5 == "Computador de mesa")
Computadordemesa_5 <- sum(Computadordemesa_5$Freq)
# Portátil
Portátil_5 <- P412_5_ind %>% filter(P412_5_ind$P412_5 == "Portátil")
Portátil_5 <- sum(Portátil_5$Freq)
# Televisor
Televisor_5 <- P412_5_ind %>% filter(P412_5_ind$P412_5 == "Televisor")
Televisor_5 <- sum(Televisor_5$Freq)
# Consola de video juegos
Consoladevideojuegos_5 <- P412_5_ind %>% filter(P412_5_ind$P412_5 == "Consola de video juegos")
Consoladevideojuegos_5 <- sum(Consoladevideojuegos_5$Freq)
# Smartwatch / reloj inteligente
Smartwatch_relojinteligente_5 <- P412_5_ind %>% filter(P412_5_ind$P412_5 == "Smartwatch / reloj inteligente")
Smartwatch_relojinteligente_5 <- sum(Smartwatch_relojinteligente_5$Freq)
# No sabe / no responde
P412_5_NS <- P412_5_ind %>% filter(P412_5_ind$P412_5 == "No sabe / no responde")
P412_5_NS <- sum(P412_5_NS$Freq)
###############################################################################################################################################################
#  ¿A través de que dispositivos se conectan al internet fijo los diferentes miembros en el hogar? 6
P412_6_ind <- data.frame(svytable(~FACTOR + P412_6,
                                  design = BD_survey))
# celular 
celular_6 <- P412_6_ind %>% filter(P412_6_ind$P412_6 == "Celular")
celular_6 <- sum(celular_6$Freq)
# celular 
Tablet_6 <- P412_6_ind %>% filter(P412_6_ind$P412_6 == "Tablet")
Tablet_6 <- sum(Tablet_6$Freq)
# Computador de mesa
Computadordemesa_6 <- P412_6_ind %>% filter(P412_6_ind$P412_6 == "Computador de mesa")
Computadordemesa_6 <- sum(Computadordemesa_6$Freq)
# Portátil
Portátil_6 <- P412_6_ind %>% filter(P412_6_ind$P412_6 == "Portátil")
Portátil_6 <- sum(Portátil_6$Freq)
# Televisor
Televisor_6 <- P412_6_ind %>% filter(P412_6_ind$P412_6 == "Televisor")
Televisor_6 <- sum(Televisor_6$Freq)
# Consola de video juegos
Consoladevideojuegos_6 <- P412_6_ind %>% filter(P412_6_ind$P412_6 == "Consola de video juegos")
Consoladevideojuegos_6 <- sum(Consoladevideojuegos_6$Freq)
# Smartwatch / reloj inteligente
Smartwatch_relojinteligente_6 <- P412_6_ind %>% filter(P412_6_ind$P412_6 == "Smartwatch / reloj inteligente")
Smartwatch_relojinteligente_6 <- sum(Smartwatch_relojinteligente_6$Freq)
# No sabe / no responde
P412_6_NS <- P412_6_ind %>% filter(P412_6_ind$P412_6 == "No sabe / no responde")
P412_6_NS <- sum(P412_6_NS$Freq)
##############################################################################################################################################
# ¿Con que otros servicios tiene el paquete de internet fijo..con? 1
P414_1_ind <- data.frame(svytable(~FACTOR + P414_1,
                                  design = BD_survey))
# Internet móvil
Internetmovil_1 <- P414_1_ind %>% filter(P414_1_ind$P414_1 == "Internet Móvil")
Internetmovil_1 <- sum(Internetmovil_1$Freq)
# Telefonía Móvil
Telefoniamovil_1 <- P414_1_ind %>% filter(P414_1_ind$P414_1 == "Telefonía Móvil")
Telefoniamovil_1<- sum(Telefoniamovil_1$Freq)
# Telefonía fija
Telefoniafija_1 <- P414_1_ind %>% filter(P414_1_ind$P414_1 == "Telefonía fija")
Telefoniafija_1<- sum(Telefoniafija_1$Freq)
# Televisión cable
Televisioncable_1 <- P414_1_ind %>% filter(P414_1_ind$P414_1 == "Televisión cable")
Televisioncable_1 <- sum(Televisioncable_1$Freq)
##############################################################################################################################################
# ¿Con que otros servicios tiene el paquete de internet fijo..con? 2
P414_2_ind <- data.frame(svytable(~FACTOR + P414_2,
                                  design = BD_survey))
# Internet móvil
Internetmovil_2 <- P414_2_ind %>% filter(P414_2_ind$P414_2 == "Internet Móvil")
Internetmovil_2 <- sum(Internetmovil_2$Freq)
# Telefonía Móvil
Telefoniamovil_2 <- P414_2_ind %>% filter(P414_2_ind$P414_2 == "Telefonía Móvil")
Telefoniamovil_2 <- sum(Telefoniamovil_2$Freq)
# Telefonía fija
Telefoniafija_2 <- P414_2_ind %>% filter(P414_2_ind$P414_2 == "Telefonía fija")
Telefoniafija_2 <- sum(Telefoniafija_2$Freq)
# Televisión cable
Televisioncable_2 <- P414_2_ind %>% filter(P414_2_ind$P414_2 == "Televisión cable")
Televisioncable_2 <- sum(Televisioncable_2$Freq)
##############################################################################################################################################
# ¿Con que otros servicios tiene el paquete de internet fijo..con? 3
P414_3_ind <- data.frame(svytable(~FACTOR + P414_3,
                                  design = BD_survey))
# Internet móvil
Internetmovil_3 <- P414_3_ind %>% filter(P414_3_ind$P414_3 == "Internet Móvil")
Internetmovil_3 <- sum(Internetmovil_3$Freq)
# Telefonía Móvil
Telefoniamovil_3 <- P414_3_ind %>% filter(P414_3_ind$P414_3 == "Telefonía Móvil")
Telefoniamovil_3 <- sum(Telefoniamovil_3$Freq)
# Telefonía fija
Telefoniafija_3 <- P414_3_ind %>% filter(P414_3_ind$P414_3 == "Telefonía fija")
Telefoniafija_3 <- sum(Telefoniafija_3$Freq)
# Televisión cable
Televisioncable_3 <- P414_3_ind %>% filter(P414_3_ind$P414_3 == "Televisión cable")
Televisioncable_3 <- sum(Televisioncable_3$Freq)
##############################################################################################################################################
# ¿Con que otros servicios tiene el paquete de internet fijo..con? 4
P414_4_ind <- data.frame(svytable(~FACTOR + P414_4,
                                  design = BD_survey))
# Internet móvil
Internetmovil_4 <- P414_4_ind %>% filter(P414_4_ind$P414_4 == "Internet Móvil")
Internetmovil_4 <- sum(Internetmovil_4$Freq)
# Telefonía Móvil
Telefoniamovil_4 <- P414_4_ind %>% filter(P414_4_ind$P414_4 == "Telefonía Móvil")
Telefoniamovil_4 <- sum(Telefoniamovil_4$Freq)
# Telefonía fija
Telefoniafija_4 <- P414_4_ind %>% filter(P414_4_ind$P414_4 == "Telefonía fija")
Telefoniafija_4 <- sum(Telefoniafija_4$Freq)
# Televisión cable
Televisioncable_4 <- P414_4_ind %>% filter(P414_4_ind$P414_4 == "Televisión cable")
Televisioncable_4 <- sum(Televisioncable_4$Freq)
##############################################################################################################################################
#  ¿con cuál de estas opciones cuenta su servicio de Internet Fijo?
P417_1_ind <- data.frame(svytable(~FACTOR + P417_1,
                                  design = BD_survey))
# Cable de red (conexión con cable)
Cabledered_1 <- P417_1_ind %>% filter(P417_1_ind$P417_1 == "Cable de red (conexión con cable)")
Cabledered_1 <- sum(Cabledered_1$Freq)
# Wifi ( tecnología inalámbrica)
wifi_1 <- P417_1_ind %>% filter(P417_1_ind$P417_1 == "Wifi ( tecnología inalámbrica)")
wifi_1 <- sum(wifi_1$Freq)
# Puntos de cableado adicionales (Conectar varios dispositivos por cable de red al mismo tiempo)
Puntoscableado_1 <- P417_1_ind %>% filter(P417_1_ind$P417_1 == "Puntos de cableado adicionales (Conectar varios dispositivos por cable de red al mismo tiempo)")
Puntoscableado_1 <- sum(Puntoscableado_1$Freq)
# Conexión de varios dispositivos al tiempo
conexion_1 <- P417_1_ind %>% filter(P417_1_ind$P417_1 == "Conexión de varios dispositivos al tiempo")
conexion_1 <- sum(conexion_1$Freq)
# Repetidor de wifi (amplificador de la señal en casa para que llegue a todos los rincones)
repetidorwifi_1 <- P417_1_ind %>% filter(P417_1_ind$P417_1 == "Repetidor de wifi (amplificador de la señal en casa para que llegue a todos los rincones)")
repetidorwifi_1 <- sum(repetidorwifi_1$Freq)
# Otro
Otro_1 <- P417_1_ind %>% filter(P417_1_ind$P417_1 == "Otro")
Otro_1 <- sum(Otro_1$Freq)
# No sabe / no responde
P417_1_NS <- P417_1_ind %>% filter(P417_1_ind$P417_1 == "No sabe / no responde")
P417_1_NS <- sum(P417_1_NS$Freq)
##############################################################################################################################################
#  ¿con cuál de estas opciones cuenta su servicio de Internet Fijo? 2
P417_2_ind <- data.frame(svytable(~FACTOR + P417_2,
                                  design = BD_survey))
# Cable de red (conexión con cable)
Cabledered_2 <- P417_2_ind %>% filter(P417_2_ind$P417_2 == "Cable de red (conexión con cable)")
Cabledered_2 <- sum(Cabledered_2$Freq)
# Wifi ( tecnología inalámbrica)
wifi_2 <- P417_2_ind %>% filter(P417_2_ind$P417_2 == "Wifi ( tecnología inalámbrica)")
wifi_2 <- sum(wifi_2$Freq)
# Puntos de cableado adicionales (Conectar varios dispositivos por cable de red al mismo tiempo)
Puntoscableado_2 <- P417_2_ind %>% filter(P417_2_ind$P417_2 == "Puntos de cableado adicionales (Conectar varios dispositivos por cable de red al mismo tiempo)")
Puntoscableado_2 <- sum(Puntoscableado_2$Freq)
# Conexión de varios dispositivos al tiempo
conexion_2 <- P417_2_ind %>% filter(P417_2_ind$P417_2 == "Conexión de varios dispositivos al tiempo")
conexion_2 <- sum(conexion_2$Freq)
# Repetidor de wifi (amplificador de la señal en casa para que llegue a todos los rincones)
repetidorwifi_2 <- P417_2_ind %>% filter(P417_2_ind$P417_2 == "Repetidor de wifi (amplificador de la señal en casa para que llegue a todos los rincones)")
repetidorwifi_2 <- sum(repetidorwifi_2$Freq)
# Otro
Otro_2 <- P417_2_ind %>% filter(P417_2_ind$P417_2 == "Otro")
Otro_2 <- sum(Otro_2$Freq)
# No sabe / no responde
P417_2_NS <- P417_2_ind %>% filter(P417_2_ind$P417_2 == "No sabe / no responde")
P417_2_NS <- sum(P417_2_NS$Freq)

############################################################################################################################################################################################33
#  ¿con cuál de estas opciones cuenta su servicio de Internet Fijo? 3
P417_3_ind <- data.frame(svytable(~FACTOR + P417_3,
                                  design = BD_survey))
# Cable de red (conexión con cable)
Cabledered_3 <- P417_3_ind %>% filter(P417_3_ind$P417_3 == "Cable de red (conexión con cable)")
Cabledered_3 <- sum(Cabledered_3$Freq)
# Wifi ( tecnología inalámbrica)
wifi_3 <- P417_3_ind %>% filter(P417_3_ind$P417_3 == "Wifi ( tecnología inalámbrica)")
wifi_3 <- sum(wifi_3$Freq)
# Puntos de cableado adicionales (Conectar varios dispositivos por cable de red al mismo tiempo)
Puntoscableado_3 <- P417_3_ind %>% filter(P417_3_ind$P417_3 == "Puntos de cableado adicionales (Conectar varios dispositivos por cable de red al mismo tiempo)")
Puntoscableado_3 <- sum(Puntoscableado_3$Freq)
# Conexión de varios dispositivos al tiempo
conexion_3 <- P417_3_ind %>% filter(P417_3_ind$P417_3 == "Conexión de varios dispositivos al tiempo")
conexion_3 <- sum(conexion_3$Freq)
# Repetidor de wifi (amplificador de la señal en casa para que llegue a todos los rincones)
repetidorwifi_3 <- P417_3_ind %>% filter(P417_3_ind$P417_3 == "Repetidor de wifi (amplificador de la señal en casa para que llegue a todos los rincones)")
repetidorwifi_3 <- sum(repetidorwifi_3$Freq)
# Otro
Otro_3 <- P417_3_ind %>% filter(P417_3_ind$P417_3 == "Otro")
Otro_3 <- sum(Otro_3$Freq)
# No sabe / no responde
P417_3_NS <- P417_3_ind %>% filter(P417_3_ind$P417_3 == "No sabe / no responde")
P417_3_NS <- sum(P417_3_NS$Freq)
#######################################################################################################################################################################################################3
#  ¿con cuál de estas opciones cuenta su servicio de Internet Fijo? 4
P417_4_ind <- data.frame(svytable(~FACTOR + P417_4,
                                  design = BD_survey))
# Cable de red (conexión con cable)
Cabledered_4 <- P417_4_ind %>% filter(P417_4_ind$P417_4 == "Cable de red (conexión con cable)")
Cabledered_4 <- sum(Cabledered_4$Freq)
# Wifi ( tecnología inalámbrica)
wifi_4 <- P417_4_ind %>% filter(P417_4_ind$P417_4 == "Wifi ( tecnología inalámbrica)")
wifi_4 <- sum(wifi_4$Freq)
# Puntos de cableado adicionales (Conectar varios dispositivos por cable de red al mismo tiempo)
Puntoscableado_4 <- P417_4_ind %>% filter(P417_4_ind$P417_4 == "Puntos de cableado adicionales (Conectar varios dispositivos por cable de red al mismo tiempo)")
Puntoscableado_4 <- sum(Puntoscableado_4$Freq)
# Conexión de varios dispositivos al tiempo
conexion_4 <- P417_4_ind %>% filter(P417_4_ind$P417_4 == "Conexión de varios dispositivos al tiempo")
conexion_4 <- sum(conexion_4$Freq)
# Repetidor de wifi (amplificador de la señal en casa para que llegue a todos los rincones)
repetidorwifi_4 <- P417_4_ind %>% filter(P417_4_ind$P417_4 == "Repetidor de wifi (amplificador de la señal en casa para que llegue a todos los rincones)")
repetidorwifi_4 <- sum(repetidorwifi_4$Freq)
# Otro
Otro_4 <- P417_4_ind %>% filter(P417_4_ind$P417_4 == "Otro")
Otro_4 <- sum(Otro_4$Freq)
# No sabe / no responde
P417_4_NS <- P417_4_ind %>% filter(P417_4_ind$P417_4 == "No sabe / no responde")
P417_4_NS <- sum(P417_4_NS$Freq)
#######################################################################################################################################################################################################3
#  ¿con cuál de estas opciones cuenta su servicio de Internet Fijo? 5
P417_5_ind <- data.frame(svytable(~FACTOR + P417_5,
                                  design = BD_survey))
# Cable de red (conexión con cable)
Cabledered_5 <- P417_5_ind %>% filter(P417_5_ind$P417_5 == "Cable de red (conexión con cable)")
Cabledered_5 <- sum(Cabledered_5$Freq)
# Wifi ( tecnología inalámbrica)
wifi_5 <- P417_5_ind %>% filter(P417_5_ind$P417_5 == "Wifi ( tecnología inalámbrica)")
wifi_5 <- sum(wifi_5$Freq)
# Puntos de cableado adicionales (Conectar varios dispositivos por cable de red al mismo tiempo)
Puntoscableado_5 <- P417_5_ind %>% filter(P417_5_ind$P417_5 == "Puntos de cableado adicionales (Conectar varios dispositivos por cable de red al mismo tiempo)")
Puntoscableado_5 <- sum(Puntoscableado_5$Freq)
# Conexión de varios dispositivos al tiempo
conexion_5 <- P417_5_ind %>% filter(P417_5_ind$P417_5 == "Conexión de varios dispositivos al tiempo")
conexion_5 <- sum(conexion_5$Freq)
# Repetidor de wifi (amplificador de la señal en casa para que llegue a todos los rincones)
repetidorwifi_5 <- P417_5_ind %>% filter(P417_5_ind$P417_5 == "Repetidor de wifi (amplificador de la señal en casa para que llegue a todos los rincones)")
repetidorwifi_5 <- sum(repetidorwifi_5$Freq)
# Otro
Otro_5 <- P417_5_ind %>% filter(P417_5_ind$P417_5 == "Otro")
Otro_5 <- sum(Otro_5$Freq)
# No sabe / no responde
P417_5_NS <- P417_5_ind %>% filter(P417_5_ind$P417_5 == "No sabe / no responde")
P417_5_NS <- sum(P417_5_NS$Freq)
#######################################################################################################################################################################################################3
#  ¿Cómo califica el servicio que le presta el operador de internet Fijo que tiene actualmente?
P419_ind <- data.frame(svytable(~FACTOR + P419,
                                  design = BD_survey))
# Muy malo
P419_muymalo <- P419_ind %>% filter(P419_ind$P419 == "Muy malo")
P419_muymalo <- sum(P419_muymalo$Freq)
# 2
P419_2 <- P419_ind %>% filter(P419_ind$P419 == "2")
P419_2 <- sum(P419_2$Freq)
# 3
P419_3 <- P419_ind %>% filter(P419_ind$P419 == "3")
P419_3 <- sum(P419_3$Freq)
# 4
P419_4 <- P419_ind %>% filter(P419_ind$P419 == "4")
P419_4 <- sum(P419_4$Freq)

# 5
P419_5 <- P419_ind %>% filter(P419_ind$P419 == "5")
P419_5 <- sum(P419_5$Freq)

# 6
P419_6 <- P419_ind %>% filter(P419_ind$P419 == "6")
P419_6 <- sum(P419_6$Freq)

# 7
P419_7 <- P419_ind %>% filter(P419_ind$P419 == "7")
P419_7 <- sum(P419_7$Freq)

# 8
P419_8 <- P419_ind %>% filter(P419_ind$P419 == "8")
P419_8 <- sum(P419_8$Freq)

# 9
P419_9 <- P419_ind %>% filter(P419_ind$P419 == "9")
P419_9 <- sum(P419_9$Freq)

# Muy bueno
P419_muybueno <- P419_ind %>% filter(P419_ind$P419 == "Muy bueno")
P419_muybueno <- sum(P419_muybueno$Freq)
##########################################################################################################################################
#  La disponibilidad de la red para acceder a internet, es decir, hay señal todo el tiempo.
P420_1_ind <- data.frame(svytable(~FACTOR + P420_1,
                                design = BD_survey))
# Muy malo
P420_1_muymalo <- P420_1_ind %>% filter(P420_1_ind$P420_1 == "Muy malo")
P420_1_muymalo <- sum(P420_1_muymalo$Freq)
# 2
P420_1_2 <- P420_1_ind %>% filter(P420_1_ind$P420_1 == "2")
P420_1_2 <- sum(P420_1_2$Freq)
# 3
P420_1_3 <- P420_1_ind %>% filter(P420_1_ind$P420_1 == "3")
P420_1_3 <- sum(P420_1_3$Freq)
# 4
P420_1_4 <- P420_1_ind %>% filter(P420_1_ind$P420_1 == "4")
P420_1_4 <- sum(P420_1_4$Freq)

# 5
P420_1_5 <- P420_1_ind %>% filter(P420_1_ind$P420_1 == "5")
P420_1_5 <- sum(P420_1_5$Freq)

# 6
P420_1_6 <- P420_1_ind %>% filter(P420_1_ind$P420_1 == "6")
P420_1_6 <- sum(P420_1_6$Freq)

# 7
P420_1_7 <- P420_1_ind %>% filter(P420_1_ind$P420_1 == "7")
P420_1_7 <- sum(P420_1_7$Freq)

# 8
P420_1_8 <- P420_1_ind %>% filter(P420_1_ind$P420_1 == "8")
P420_1_8 <- sum(P420_1_8$Freq)

# 9
P420_1_9 <- P420_1_ind %>% filter(P420_1_ind$P420_1 == "9")
P420_1_9 <- sum(P420_1_9$Freq)

# Muy bueno
P420_1_muybueno <- P420_1_ind %>% filter(P420_1_ind$P420_1 == "Muy bueno")
P420_1_muybueno <- sum(P420_1_muybueno$Freq)

# NS/NR
P420_1_NS_NR <- P420_1_ind %>% filter(P420_1_ind$P420_1 == "NS/NR")
P420_1_NS_NR <- sum(P420_1_NS_NR$Freq)
###########################################################################################################################################
# La velocidad/calidad en la navegación, es decir el tiempo que se demoran en cargar las páginas o aplicaciones de acuerdo a lo contratado.
P420_2_ind <- data.frame(svytable(~FACTOR + P420_2,
                                  design = BD_survey))
# Muy malo
P420_2_muymalo <- P420_2_ind %>% filter(P420_2_ind$P420_2 == "Muy malo")
P420_2_muymalo <- sum(P420_2_muymalo$Freq)
# 2
P420_2_2 <- P420_2_ind %>% filter(P420_2_ind$P420_2 == "2")
P420_2_2 <- sum(P420_2_2$Freq)
# 3
P420_2_3 <- P420_2_ind %>% filter(P420_2_ind$P420_2 == "3")
P420_2_3 <- sum(P420_2_3$Freq)
# 4
P420_2_4 <- P420_2_ind %>% filter(P420_2_ind$P420_2 == "4")
P420_2_4 <- sum(P420_2_4$Freq)

# 5
P420_2_5 <- P420_2_ind %>% filter(P420_2_ind$P420_2 == "5")
P420_2_5 <- sum(P420_2_5$Freq)

# 6
P420_2_6 <- P420_2_ind %>% filter(P420_2_ind$P420_2 == "6")
P420_2_6 <- sum(P420_2_6$Freq)

# 7
P420_2_7 <- P420_2_ind %>% filter(P420_2_ind$P420_2 == "7")
P420_2_7 <- sum(P420_2_7$Freq)

# 8
P420_2_8 <- P420_2_ind %>% filter(P420_2_ind$P420_2 == "8")
P420_2_8 <- sum(P420_2_8$Freq)

# 9
P420_2_9 <- P420_2_ind %>% filter(P420_2_ind$P420_2 == "9")
P420_2_9 <- sum(P420_2_9$Freq)

# Muy bueno
P420_2_muybueno <- P420_2_ind %>% filter(P420_2_ind$P420_2 == "Muy bueno")
P420_2_muybueno <- sum(P420_2_muybueno$Freq)

# NS/NR
P420_2_NS_NR <- P420_2_ind %>% filter(P420_2_ind$P420_2 == "NS/NR")
P420_2_NS_NR <- sum(P420_2_NS_NR$Freq)
###########################################################################################################################################
# La continuidad de la conexión, es decir mientras se está navegando no hay caídas ni fallas en la red
P420_3_ind <- data.frame(svytable(~FACTOR + P420_3,
                                  design = BD_survey))
# Muy malo
P420_3_muymalo <- P420_3_ind %>% filter(P420_3_ind$P420_3 == "Muy malo")
P420_3_muymalo <- sum(P420_3_muymalo$Freq)
# 2
P420_3_2 <- P420_3_ind %>% filter(P420_3_ind$P420_3 == "2")
P420_3_2 <- sum(P420_3_2$Freq)
# 3
P420_3_3 <- P420_3_ind %>% filter(P420_3_ind$P420_3 == "3")
P420_3_3 <- sum(P420_3_3$Freq)
# 4
P420_3_4 <- P420_3_ind %>% filter(P420_3_ind$P420_3 == "4")
P420_3_4 <- sum(P420_3_4$Freq)

# 5
P420_3_5 <- P420_3_ind %>% filter(P420_3_ind$P420_3 == "5")
P420_3_5 <- sum(P420_3_5$Freq)

# 6
P420_3_6 <- P420_3_ind %>% filter(P420_3_ind$P420_3 == "6")
P420_3_6 <- sum(P420_3_6$Freq)

# 7
P420_3_7 <- P420_3_ind %>% filter(P420_3_ind$P420_3 == "7")
P420_3_7 <- sum(P420_3_7$Freq)

# 8
P420_3_8 <- P420_3_ind %>% filter(P420_3_ind$P420_3 == "8")
P420_3_8 <- sum(P420_3_8$Freq)

# 9
P420_3_9 <- P420_3_ind %>% filter(P420_3_ind$P420_3 == "9")
P420_3_9 <- sum(P420_3_9$Freq)

# Muy bueno
P420_3_muybueno <- P420_3_ind %>% filter(P420_3_ind$P420_3 == "Muy bueno")
P420_3_muybueno <- sum(P420_3_muybueno$Freq)

# NS/NR
P420_3_NS_NR <- P420_3_ind %>% filter(P420_3_ind$P420_3 == "NS/NR")
P420_3_NS_NR <- sum(P420_3_NS_NR$Freq)
###########################################################################################################################################
#  La disponibilidad de la señal cuando están todos en casa conectados
P420_4_ind <- data.frame(svytable(~FACTOR + P420_4,
                                  design = BD_survey))
# Muy malo
P420_4_muymalo <- P420_4_ind %>% filter(P420_4_ind$P420_4 == "Muy malo")
P420_4_muymalo <- sum(P420_4_muymalo$Freq)
# 2
P420_4_2 <- P420_4_ind %>% filter(P420_4_ind$P420_4 == "2")
P420_4_2 <- sum(P420_4_2$Freq)
# 3
P420_4_3 <- P420_4_ind %>% filter(P420_4_ind$P420_4 == "3")
P420_4_3 <- sum(P420_4_3$Freq)
# 4
P420_4_4 <- P420_4_ind %>% filter(P420_4_ind$P420_4 == "4")
P420_4_4 <- sum(P420_4_4$Freq)

# 5
P420_4_5 <- P420_4_ind %>% filter(P420_4_ind$P420_4 == "5")
P420_4_5 <- sum(P420_4_5$Freq)

# 6
P420_4_6 <- P420_4_ind %>% filter(P420_4_ind$P420_4 == "6")
P420_4_6 <- sum(P420_4_6$Freq)

# 7
P420_4_7 <- P420_4_ind %>% filter(P420_4_ind$P420_4 == "7")
P420_4_7 <- sum(P420_4_7$Freq)

# 8
P420_4_8 <- P420_4_ind %>% filter(P420_4_ind$P420_4 == "8")
P420_4_8 <- sum(P420_4_8$Freq)

# 9
P420_4_9 <- P420_4_ind %>% filter(P420_4_ind$P420_4 == "9")
P420_4_9 <- sum(P420_4_9$Freq)

# Muy bueno
P420_4_muybueno <- P420_4_ind %>% filter(P420_4_ind$P420_4 == "Muy bueno")
P420_4_muybueno <- sum(P420_4_muybueno$Freq)

# NS/NR
P420_4_NS_NR <- P420_4_ind %>% filter(P420_4_ind$P420_4 == "NS/NR")
P420_4_NS_NR <- sum(P420_4_NS_NR$Freq)
###########################################################################################################################################
#  La calidad de la señal se mantiene igual en todos los espacios de la casa
P420_5_ind <- data.frame(svytable(~FACTOR + P420_5,
                                  design = BD_survey))
# Muy malo
P420_5_muymalo <- P420_5_ind %>% filter(P420_5_ind$P420_5 == "Muy malo")
P420_5_muymalo <- sum(P420_5_muymalo$Freq)
# 2
P420_5_2 <- P420_5_ind %>% filter(P420_5_ind$P420_5 == "2")
P420_5_2 <- sum(P420_5_2$Freq)
# 3
P420_5_3 <- P420_5_ind %>% filter(P420_5_ind$P420_5 == "3")
P420_5_3 <- sum(P420_5_3$Freq)
# 4
P420_5_4 <- P420_5_ind %>% filter(P420_5_ind$P420_5 == "4")
P420_5_4 <- sum(P420_5_4$Freq)

# 5
P420_5_5 <- P420_5_ind %>% filter(P420_5_ind$P420_5 == "5")
P420_5_5 <- sum(P420_5_5$Freq)

# 6
P420_5_6 <- P420_5_ind %>% filter(P420_5_ind$P420_5 == "6")
P420_5_6 <- sum(P420_5_6$Freq)

# 7
P420_5_7 <- P420_5_ind %>% filter(P420_5_ind$P420_5 == "7")
P420_5_7 <- sum(P420_5_7$Freq)

# 8
P420_5_8 <- P420_5_ind %>% filter(P420_5_ind$P420_5 == "8")
P420_5_8 <- sum(P420_5_8$Freq)

# 9
P420_5_9 <- P420_5_ind %>% filter(P420_5_ind$P420_5 == "9")
P420_5_9 <- sum(P420_5_9$Freq)

# Muy bueno
P420_5_muybueno <- P420_5_ind %>% filter(P420_5_ind$P420_5 == "Muy bueno")
P420_5_muybueno <- sum(P420_5_muybueno$Freq)

# NS/NR
P420_5_NS_NR <- P420_5_ind %>% filter(P420_5_ind$P420_5 == "NS/NR")
P420_5_NS_NR <- sum(P420_5_NS_NR$Freq)
###########################################################################################################################################
#   La velocidad/calidad en la navegación, cuando están todos en casa conectados
P420_6_ind <- data.frame(svytable(~FACTOR + P420_6,
                                  design = BD_survey))
# Muy malo
P420_6_muymalo <- P420_6_ind %>% filter(P420_6_ind$P420_6 == "Muy malo")
P420_6_muymalo <- sum(P420_6_muymalo$Freq)
# 2
P420_6_2 <- P420_6_ind %>% filter(P420_6_ind$P420_6 == "2")
P420_6_2 <- sum(P420_6_2$Freq)
# 3
P420_6_3 <- P420_6_ind %>% filter(P420_6_ind$P420_6 == "3")
P420_6_3 <- sum(P420_6_3$Freq)
# 4
P420_6_4 <- P420_6_ind %>% filter(P420_6_ind$P420_6 == "4")
P420_6_4 <- sum(P420_6_4$Freq)

# 5
P420_6_5 <- P420_6_ind %>% filter(P420_6_ind$P420_6 == "5")
P420_6_5 <- sum(P420_6_5$Freq)

# 6
P420_6_6 <- P420_6_ind %>% filter(P420_6_ind$P420_6 == "6")
P420_6_6 <- sum(P420_6_6$Freq)

# 7
P420_6_7 <- P420_6_ind %>% filter(P420_6_ind$P420_6 == "7")
P420_6_7 <- sum(P420_6_7$Freq)

# 8
P420_6_8 <- P420_6_ind %>% filter(P420_6_ind$P420_6 == "8")
P420_6_8 <- sum(P420_6_8$Freq)

# 9
P420_6_9 <- P420_6_ind %>% filter(P420_6_ind$P420_6 == "9")
P420_6_9 <- sum(P420_6_9$Freq)

# Muy bueno
P420_6_muybueno <- P420_6_ind %>% filter(P420_6_ind$P420_6 == "Muy bueno")
P420_6_muybueno <- sum(P420_6_muybueno$Freq)

# NS/NR
P420_6_NS_NR <- P420_6_ind %>% filter(P420_6_ind$P420_6 == "NS/NR")
P420_6_NS_NR <- sum(P420_6_NS_NR$Freq)
###########################################################################################################################################
#   Acceso a promociones exclusivas que premian mi fidelidad
P420_7_ind <- data.frame(svytable(~FACTOR + P420_7,
                                  design = BD_survey))
# Muy malo
P420_7_muymalo <- P420_7_ind %>% filter(P420_7_ind$P420_7 == "Muy malo")
P420_7_muymalo <- sum(P420_7_muymalo$Freq)
# 2
P420_7_2 <- P420_7_ind %>% filter(P420_7_ind$P420_7 == "2")
P420_7_2 <- sum(P420_7_2$Freq)
# 3
P420_7_3 <- P420_7_ind %>% filter(P420_7_ind$P420_7 == "3")
P420_7_3 <- sum(P420_7_3$Freq)
# 4
P420_7_4 <- P420_7_ind %>% filter(P420_7_ind$P420_7 == "4")
P420_7_4 <- sum(P420_7_4$Freq)

# 5
P420_7_5 <- P420_7_ind %>% filter(P420_7_ind$P420_7 == "5")
P420_7_5 <- sum(P420_7_5$Freq)

# 6
P420_7_6 <- P420_7_ind %>% filter(P420_7_ind$P420_7 == "6")
P420_7_6 <- sum(P420_7_6$Freq)

# 7
P420_7_7 <- P420_7_ind %>% filter(P420_7_ind$P420_7 == "7")
P420_7_7 <- sum(P420_7_7$Freq)

# 8
P420_7_8 <- P420_7_ind %>% filter(P420_7_ind$P420_7 == "8")
P420_7_8 <- sum(P420_7_8$Freq)

# 9
P420_7_9 <- P420_7_ind %>% filter(P420_7_ind$P420_7 == "9")
P420_7_9 <- sum(P420_7_9$Freq)

# Muy bueno
P420_7_muybueno <- P420_7_ind %>% filter(P420_7_ind$P420_7 == "Muy bueno")
P420_7_muybueno <- sum(P420_7_muybueno$Freq)

# NS/NR
P420_7_NS_NR <- P420_7_ind %>% filter(P420_7_ind$P420_7 == "NS/NR")
P420_7_NS_NR <- sum(P420_7_NS_NR$Freq)
###################################################################################################################################################
# De las siguientes opciones que le voy a leer ¿cuál describe mejor su opinión respecto a la calidad entregada por su operador de Internet fijo? 
P422_ind <- data.frame(svytable(~FACTOR + P422,
                                design = BD_survey))
# El servicio que me ofrecen es muy costoso  respecto a la calidad
P422_1 <- P422_ind %>% filter(P422_ind$P422 == "El servicio que me ofrecen es muy costoso  respecto a la calidad entregada")
P422_1 <- sum(P422_1$Freq)

# El servicio que me ofrecen es algo costoso  respecto a la calidad
P422_2 <- P422_ind %>% filter(P422_ind$P422 == "El servicio que me ofrecen es algo costoso  respecto a la calidad entregada")
P422_2 <- sum(P422_2$Freq)

# El servicio que me ofrecen tiene un precio justo respecto a la calidad
P422_3 <- P422_ind %>% filter(P422_ind$P422 == "El servicio que me ofrecen tiene un precio justo respecto a la calidad entregada")
P422_3 <- sum(P422_3$Freq)

# El servicio que me ofrecen tiene un costo algo bajo respecto a la calidad
P422_4 <- P422_ind %>% filter(P422_ind$P422 == "El servicio que me ofrecen tiene un costo algo bajo respecto a la calidad entregada")
P422_4 <- sum(P422_4$Freq)

# El servicio que me ofrecen tiene un costo muy bajo respecto a la calidad
P422_5 <- P422_ind %>% filter(P422_ind$P422 == "El servicio que me ofrecen tiene un costo muy bajo respecto a la calidad entregada")
P422_5 <- sum(P422_5$Freq)

