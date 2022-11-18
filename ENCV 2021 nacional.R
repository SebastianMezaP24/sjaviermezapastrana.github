################### ENCUESTA NACIONAL DE CALIDAD DE VIDA 2021 ##########################################
###################  ENFASIS EN LA ENCUESTA DE SALUD          ##########################################
###################         TOTAL NACIONAL                    ##########################################
########################################################################################################

# Limpiar el ambiente de trabajo
rm(list = ls())

# Paquetes a usar
install.packages("tidyverse") # importar, transformar, visualizar, modelar y comunicar información para procesar bases de datos
install.packages("haven") # para importar bases en dta
install.packages("readxl") # para manejar bases de excel y csv
install.packages("xlsx")
install.packages("openxlsx")
install.packages("writexl")
library(tidyverse)
library(haven)
library(readxl) 
library(survey) # para procesar encuestas realizadas mediante tecnicas de muestreo de poblaciones finitas (estructura con estratos y conglomerados)
library(xlsx)
library(openxlsx)
library(writexl)

# Verificar el directorio de trabajo
getwd()

# Definiendo el directorio para la base completa en dta
setwd("C://Users/sebas/OneDrive/Alex/Encuesta Nacional de Calidad de Vida 2021")


# 7.1. Importando las bases de personas
perchogar <- read_dta("Caracteristicas y composicion del hogar.dta")
perftraba <- read_dta("Trabajo infantil.dta")
pertic <- read_dta("Tecnologias de información y comunicación.dta")
persalud <- read_dta("Salud.dta")
peratenint <- read_dta("Atencion integral de los niños y niñas menores de 5 años.dta")
pereducacion <- read_dta("Educación.dta")
perftrab <- read_dta("Fuerza de trabajo.dta")

# 7.1.1. Unir las bases de personas
BD_personas <- merge(perchogar,
                     merge(persalud, 
                           merge(pertic, 
                                 merge(pereducacion,
                                       merge(perftrab,
                                             merge(perftraba, peratenint,
                                                   all = TRUE),
                                             all = TRUE),
                                       all = TRUE),
                                 all = T),
                           all = T),
                     all = T)

colnames(perchogar)
colnames(persalud)

sum(BD_personas$fex_c) # 51.224.060 personas en Colombia

# renombramos las variables secuencia_encuesta y secuencia_p
colnames(BD_personas)[2:3] <- c("secuencia_p","secuencia_encuesta")


# el proceso se puede hacer de otra manera pero hay que hacer el merge uno por uno
# ahora vamos a remover las bases de personas para liberar un poco de la memoria en el disco
remove(perchogar, peratenint, pereducacion, perftrab, perftraba, persalud, pertic)

# 7.2. Importando las bases de hogares
hogserhogar <- read_dta("Servicios del hogar.dta")
hogtenviv <- read_dta("Tenencia y financiacion de la vivienda que ocupa el hogar.dta")
hogcondvida <- read_dta("Condiciones de vida del hogar y tenencia de bienes.dta")
hoggastohog <- read_dta("Gastos de los hogares (lugares de compra).dta")

# 7.2.1. Unir las bases de hogares
BD_hogares <- merge(hogserhogar,
                    merge(hogtenviv,
                          merge(hogcondvida, hoggastohog,
                                all = T),
                          all = T),
                    all = T)

sum(BD_hogares$fex_c) # 17.068.100 hogares en Colombia

# ahora vamos a remover las bases de hogares para liberar un poco de la memoria en el disco
remove(hogcondvida, hoggastohog, hogserhogar, hogtenviv)

# 7.3. Importando la base de vivienda
BD_viviendas <- read_dta("Datos de la vivienda.dta")
sum(BD_viviendas$fex_c) # 16.924.234 hogares en Colombia

# 7.4. Unir la base completa
# Forma incorrecta: Uniendo las bases de personas, hogares y viviendas
BD_ecv_2021 <- merge(BD_personas,
                     merge(BD_hogares, BD_viviendas,
                           all = T),
                     all = T)

# Forma correcta: Primero se debe eliminar las columnas que no son necesarias
BD_viviendas2 <- BD_viviendas[c(-2, -3, -4)]

summary(BD_hogares$secuencia_encuesta)
summary(BD_hogares$secuencia_p)

BD_hogares2 <- BD_hogares[c(-3, -4)]


BD_ecv_2021 <- merge(BD_personas,
                       merge(BD_hogares2, BD_viviendas2,
                             all = T),
                       all = T)

# 7.6. Renombrar variables a minusculas
names(BD_ecv_2021) <- tolower(names(BD_ecv_2021))

# 7.6. Guardar la base de datos
write.csv(BD_ecv_2020_1, "ecv_2020r.csv")

write_dta(BD_ecv_2020_1, "ecv_2020r.dta")

saveRDS(BD_ecv_2021, "ecv_2021r.rds")

# 8. Construcción de indicadores
rm(list = ls())
# setwd("G:/Unidades compartidas/Docente/Universidad del Atlántico/2021-1/Diplomado/ECV/Data 2020/dta")

BD_ecv_2021 <- readRDS("ecv_2021r.rds")
colnames(BD_ecv_2021)
BD_ecv_ind <- BD_ecv_2021[c(-9, -10, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25, -26, -27, -28, -29,
                            -30, -31, -32, -34, -35, -37, -38, -39, -40, -41, -42, -43, -44, -45, -46, -47, -48, -49, -50, -51,-52,
                            -196, -197, -198, -199, -200, -201, -202, -203, -204, -205, -216, -217, -218, -219, -220, -221, -222, -223, -224, 
                            -225, -226, -227, -228, -229, -230, -231, -232, -233, -234, -235, -236, -237, -238, -239, -240,-246, -247, 
                            -248, -249, -250, -251, -252, -253, -254, -255, -256, -257, -258, -259, -260, -261, -262, -263, -264, -265, -270,
                            -273, -274, -278, -279, -363, -366, -367, -368, -369, -370, -371, -372, -373, -374, -375, -376, -377, -378, -379, -380,
                            -381, -382, -383, -384, -385, -386, -387, -388, -389, -390, -391, -392, -393, -394, -395, -396, -397, -398, -399, -400, 
                            -401, -402, -403, -404, -405, -406, -407, -408, -409, -410, -411, -412, -413, -414, -415, -416, -417, -418, -419, -420,
                            -421, -422, -423, -424, -425, -426, -427, -428, -429, -430, -431, -432, -433, -434, -435, -436, -437, -438, -439, -440,
                            -441, -442, -443, -444, -445, -446, -447, -448, -449, -450, -451, -452, -453, -459, -462, -466, -467, -468, -469, -470,
                            -471, -472, -473, -474, -475, -476, -477, -478, -479, -480, -481, -482, -483, -484, -485, -486, -487, -488, -489, -490,
                            -491, -492, -493, -494, -495, -496, -497, -498, -499, -500, -501, -502, -503, -504, -505, -506, -507, -508, -509, -510,
                            -511, -512, -513, -514, -515, -516, -517, -518, -519, -520, -521, -522, -523, -524, -525, -526, -528, -529, -530, -531,
                            -533, -535, -537, -538, -539, -547, -548, -549, -550, -551, -552, -553, -554, -555, -556, -557, -558, -559, -560,
                            -561, -562, -563, -564, -565, -566, -567, -568, -569, -560, -561, -562, -563, -564, -565, -566, -567, -568, -569, -570,
                            -575, -579, -580, -581, -582, -583, -585, -586, -587, -588, -589, -590, -591, -592, -593, -594, -595, -596, -597,
                            -598, -599, -600, -601, -602, -605, -606, -607, -609, -610, -611, -612, -613, -614, -615, -616, -617, -618, -619, -620,
                            -621, -622, -623, -624, -625, -626, -627, -628, -629, -630, -631, -632, -633, -634, -635, -636, -637, -638, -639, -640,
                            -641, -643, -644, -645, -646, -647, -649, -651, -652, -654, -655, -656, -657, -658, -659, -660, -661, -662, -663,
                            -681, -683, -689, -691, -693, -694, -698, -699, -707, -708, -709, -710, -711, -712, -713, -714, -715, -716, -717, -718,
                            -719, -720, -721, -722, -723, -724, -725, -726, -727, -728, -729, -730, -731, -732, -733, -734, -735, -736, -737,
                            -738, -739, -740, -741, -742, -743, -744, -745, -746, -747, -748, -749, -750, -751, -752, -753, -754, -755, -756, -757, 
                            -758, -759, -770, -771, -772, -773, -774, -775, -776, -777, -778, -779, -780, -781, -782, -783, -784, -785, -786, -787,
                            -788, -789, -790, -791, -792, -793, -794, -795, -796, -797, -798, -799, -800, -801, -802, -803, -809, -810, -811, -823, -824, 
                            -825, -826, -827, -828, -829, -830, -831, -832, -833, -834, -835, -836, -837, -838, -839, -840, -841, -842, -843, -844, 
                            -845, -846)]
str(BD_ecv_ind)

# importando la base de DIVIPOLA
divipola <- read.csv2("DIVIPOLA_Departamentos.csv")

# eliminar filas
#divipola <- head(divipola, -10)
#divipola <- tail(divipola, -5)

# renombrar variables de interes
colnames(divipola)[1:2] <- c("codigo", "departamento")

# Quitar variables
divipola <- divipola[c(1:2)]

# Quitar duplicados a la divipola
divipola2 <- divipola %>% distinct()

# Verificar en que formato esta la variable codigo
str(divipola2$codigo)
divipola2$codigo <- as.numeric(divipola2$codigo)

# generar una nueva variable
BD_ecv_ind$codigo <- BD_ecv_ind$p1_departamento
str(BD_ecv_ind$codigo)
BD_ecv_ind$codigo <- as.numeric(BD_ecv_ind$codigo)

# uniendo las bases con la divipola
BD_ecv_ind <- merge(BD_ecv_ind, divipola2, by = "codigo", all.x = TRUE)

# Etiquetando las variables
BD_ecv_ind$sexo <-factor(BD_ecv_ind$p6020, levels = c("1", "2"), labels = c("Hombre", "Mujer"))
BD_ecv_ind$codigo <- factor(BD_ecv_ind$codigo)

# guardando la base de indicadores
saveRDS(BD_ecv_ind, "BD_ecv_ind_2021.rds")

# limpiamos el ambiente de trabajo 
rm(list = ls())

# llamamos la base de indicadores
BD_ecv_ind <- readRDS("BD_ecv_ind_2021.rds")

# Modificando el diseño del data.frame a encuesta

BD_survey <- svydesign(ids=~1, data=BD_ecv_ind, weights=BD_ecv_ind$fex_c)

class(BD_survey)
####################################################################################################################################################
# DATOS TOTAL VIVIENDA, HOGARES Y PERSONAS A NIVEL NACIONAL 
###################################################################################################################################################

############# Frecuencias poblacionales ##################
tabsexo <- as.data.frame(svytable(~p6020, BD_survey))
tabsexo
sum(tabsexo$Freq)
prop.table(svytotal(~sexo, BD_survey))
tabsexofreq <- as.data.frame(round(prop.table(svytable(~sexo, BD_survey)),3)*100)
str(tabsexofreq)
sum(tabsexofreq$Freq)

summary(BD_survey)

# media
svymean(~sexo, BD_survey)
svymean(~sexo+cant_personas_hogar, BD_survey)
svymean(~interaction(sexo, p6051), BD_survey)

# total
svytotal(~sexo, BD_survey)
svytotal(~sexo+departamento, BD_survey)

######################################################################################################
##  Total personas a nivel colombia
Total_personas <- data.frame(svytable(~fex_c + codigo,
                                 design = BD_survey))
Total_Personas1 <- sum(Total_personas$Freq)

########################################################################################################
# Total hogares nacional colombia
Total_hogares <- data.frame(svytable(~fex_c+secuencia_p+codigo,
                                     design = BD_survey))
Total_hogares_1 <- Total_hogares %>% filter(Total_hogares$secuencia_p == 1)

Total_hogares_2 <- sum(Total_hogares_1$Freq)

#########################################################################################################
# Total viviendas colombia
BD_ecv_ind_viv <- BD_ecv_ind %>% filter(BD_ecv_ind$secuencia_encuesta == 1 & BD_ecv_ind$secuencia_p == 1)

BD_survey_viv <- svydesign(ids=~1, data=BD_ecv_ind_viv, weights=BD_ecv_ind_viv$fex_c)

Total_viviendas <- data.frame(svytable(~fex_c+codigo,
                          design = BD_survey_viv)) 
Total_viviendas_2 <- sum(Total_viviendas$Freq)

##########################################################################################################
# Total personas barranquilla
Total_personas_barranquilla <- Total_personas %>% filter(Total_personas$codigo == 8)
Total_personas_barranquilla_1 <- sum(Total_personas_barranquilla$Freq)


# Total hogares barranquilla
THbarr <- Thogares %>% filter(Thogares$codigo == 8)
TH_barr <- sum(THbarr$Freq)


# Total viviendas barranquilla
TVbarr <- TV %>% filter(TV$codigo == 8)
TV_barr <- sum(TVbarr$Freq)


###################################################################################################################################################
######################################### DATOS SALUD ############################################################################################

############################################# Nivel nacional ##########################################
############################################ PARTE 1         ##########################################

# 1. p6090 = ¿ ... está afiliado/a,  (cotizante o es beneficiario/a) a alguna entidad de seguridad social en salud? (Entidad promotora de salud [EPS] o entidad promotora de salud subsidiada [EPS-S] )
p6090_ind <- data.frame(svytable(~fex_c + p6090,
                             design = BD_survey))
# Sí 
p6090_1 <- p6090_ind %>% filter(p6090_ind$p6090 == 1)  # 47.654.668
p6090_1 <- sum(p6090_1$Freq)

# No
p6090_2 <- p6090_ind %>% filter(p6090_ind$p6090 == 2)
p6090_2 <- sum(p6090_2$Freq)                            # 3.340.072

# No sabe / no informa
p6090_9 <- p6090_ind %>% filter(p6090_ind$p6090 == 9)   # 228.668
p6090_9 <- sum(p6090_9$Freq)

# Total p6090
p6090_total <- p6090_1 + p6090_2 + p6090_9   # 51.224.060
#######################################################################################################################################################################################################

# 2. p768 = ¿Por qué razón principal no está afiliado/a de una entidad de seguridad social en salud? (Entidad promotora de salud [EPS] o entidad promotora de salud subsidiada [EPS-S] 
p768_ind <- data.frame(svytable(~fex_c + p768,
                                design = BD_survey)) 
# Por falta de dinero
p768_1 <- p768_ind %>% filter(p768_ind$p768 == 1)
p768_1 <- sum(p768_1$Freq)

# Muchos trámites
p768_2 <- p768_ind %>% filter(p768_ind$p768 == 2)
p768_2 <- sum(p768_2$Freq)
  
# No le interesa o descuido
p768_3 <- p768_ind %>% filter(p768_ind$p768 == 3)
p768_3 <- sum(p768_3$Freq)

# No sabe que debe afiliarse 
p768_4 <- p768_ind %>% filter(p768_ind$p768 == 4)
p768_4 <- sum(p768_4$Freq)

# No esta vinculado/a laboralmente a una empresa o entidad (Usted o la persona de la que es beneificiario/a)
p768_5 <- p768_ind %>% filter(p768_ind$p768 == 5)
p768_5 <- sum(p768_5$Freq)

# Esta en tramite de afiliacion
p768_6 <- p768_ind %>% filter(p768_ind$p768 == 6)
p768_6 <- sum(p768_6$Freq)

# Problemas con el sisben (no lo han visitado/a, afiliado/a en otro municipio, lo/a desvincularon, le asignaron puntaje alto)
p768_7 <- p768_ind %>% filter(p768_ind$p768 == 7)
p768_7 <- sum(p768_7$Freq)

# Otra razon
p768_8 <- p768_ind %>% filter(p768_ind$p768 == 8)
p768_8 <- sum(p768_8$Freq)

# Total p768
p768_total <- p768_1 + p768_2 + p768_3 + p768_4 + p768_5 + p768_6 + p768_7 + p768_8  # 3.340.722
########################################################################################################################################################

# 3. ¿A cuál de los siguientes regímenes de seguridad social en salud está afiliado/a?
p6100_ind <- data.frame(svytable(~fex_c + p6100,
                                 design = BD_survey))
# Contributivo (EPS)
p6100_1 <- p6100_ind %>% filter(p6100_ind$p6100 == 1)
p6100_1 <- sum(p6100_1$Freq)

# Especial (Fuerzas Armadas, Ecopetrol, universidades públicas, magisterio)
p6100_2 <- p6100_ind %>% filter(p6100_ind$p6100 == 2)
p6100_2 <- sum(p6100_2$Freq)

# Subsidiado (EPS-S)
p6100_3 <- p6100_ind %>% filter(p6100_ind$p6100 == 3)
p6100_3 <- sum(p6100_3$Freq)

# No sabe, no informa
p6100_9 <- p6100_ind %>% filter(p6100_ind$p6100 ==9)
p6100_9 <- sum(p6100_9$Freq)

# Total p6100
p6100_total <- p6100_1 + p6100_2 + p6100_3 + p6100_9  # 47.654.668
####################################################################################################################################################

# 4. ¿Quién paga mensualmente por la afiliación de ...?
p6115_ind <- data.frame(svytable(~fex_c + p6115,
                                 design = BD_survey))

# Paga una parte y otra la empresa o patron
p6115_1 <- p6115_ind %>% filter(p6115_ind$p6115 == 1)
p6115_1 <- sum(p6115_1$Freq)

# Le descuentan de la pension
p6115_2 <- p6115_ind %>% filter(p6115_ind$p6115 == 2)
p6115_2 <- sum(p6115_2$Freq)

# Paga la totalidad de la afiliacion
p6115_3 <- p6115_ind %>% filter(p6115_ind$p6115 == 3)
p6115_3 <- sum(p6115_3$Freq)

# Paga completamente la empresa o patron donde trabaja o trabajó
p6115_4 <- p6115_ind %>% filter(p6115_ind$p6115 == 4)
p6115_4 <- sum(p6115_4$Freq)

# No paga, es beneficiario/a
p6115_5 <- p6115_ind %>% filter(p6115_ind$p6115 == 5)
p6115_5 <- sum(p6115_5$Freq)

# Total p6115
p6115_total <- p6115_1 + p6115_2 + p6115_3 + p6115_4 + p6115_5  # 22.150.639
#####################################################################################################################################################

# 5. ¿De quién es beneficiario/a ...?
p5669_ind <- data.frame(svytable(~fex_c + p5669,
                                 design = BD_survey))

# De una persona de este hogar
p5669_1 <- p5669_ind %>% filter(p5669_ind$p5669 == 1)
p5669_1 <- sum(p5669_1$Freq)

# De una persona de otro hogar
p5669_2 <- p5669_ind %>% filter(p5669_ind$p5669 == 2)
p5669_2 <- sum(p5669_2$Freq)

# Total p5669
p5669_total <- p5669_1 + p5669_2  # 10.083.933
######################################################################################################################################################

# Numero Orden
p5669s1 <- table(BD_ecv_ind$p5669s1)

# 1 <- 16.854
# 2 <- 6.431
# 3 <- 907
# 4 <- 400
# 5 <- 155
# 6 <- 64
# 7 <- 19
# 8 <- 7
# 9 <- 2
# 10 <- 2
# 14 <- 2
# 17<- 1
# -1 <- 1
# -2 <- 7
# -3 <- 1
# -4 <- 2
#######################################################################################################################################################

# 6. ¿Cuánto paga o cuánto le descuentan mensualmente a ... para estar cubierto/a por una entidad de seguridad social en salud? 
p8551 <- table(BD_ecv_ind$p8551)
#######################################################################################################################################################

# 7. En general, considera que la calidad del servicio de su EPS o de la entidad de seguridad social en salud en la cual ... está afiliado/a es

p6181_ind <- data.frame(svytable(~fex_c + p6181,
                                 design = BD_survey))

# Muy buena
p6181_1 <- p6181_ind %>% filter(p6181_ind$p6181 == 1)
p6181_1 <- sum(p6181_1$Freq)                             # 5.521.958

# Buena
p6181_2 <- p6181_ind %>% filter(p6181_ind$p6181 == 2)
p6181_2 <- sum(p6181_2$Freq)                            # 35.891.943

# Mala
p6181_3 <- p6181_ind %>% filter(p6181_ind$p6181 == 3)
p6181_3 <- sum(p6181_3$Freq)                            # 4.343.912

# Muy mala
p6181_4 <- p6181_ind %>% filter(p6181_ind$p6181 == 4)
p6181_4 <- sum(p6181_4$Freq)                            # 842.775

# No sabe
p6181_9 <- p6181_ind %>% filter(p6181_ind$p6181 == 9)
p6181_9 <- sum(p6181_9$Freq)                            # 892.227

# total p6181
p6181_total <- p6181_1 + p6181_2 + p6181_3 + p6181_4 + p6181_9
########################################################################################################################################################

# 8. ¿Cuál es el aspecto que más influye en su percepción sobre la calidad del servicio de su EPS o de la entidad de seguridad social en salud en la cual se encuentra afiliado/a? 

p798_ind <- data.frame(svytable(~fex_c + p798,
                                design = BD_survey))

# Tramites excesivos o dispendiosos
p798_1 <- p798_ind %>% filter(p798_ind$p798 == 1)
p798_1 <- sum(p798_1$Freq)

# Mala atencion del personal administrativo o asistencial (medicos, enfermeras, etc)
p798_2 <- p798_ind %>% filter(p798_ind$p798 == 2)
p798_2 <- sum(p798_2$Freq)

# Falta de capacidad, conocimientos o habilidad del personal asistencial
p798_3 <-p798_ind %>% filter(p798_ind$p798 == 3)
p798_3 <- sum(p798_3$Freq)

# Condiciones deficientes de infraestructura, dotación, mobiliario o accesibilidad para población en condición de discapacidad
p798_4 <- p798_ind %>% filter(p798_ind$p798 == 4)
p798_4 <- sum(p798_4$Freq)

# Demora en la asignacion de citas
p798_5 <- p798_ind %>% filter(p798_ind$p798 == 5)
p798_5 <- sum(p798_5$Freq)

# Demora en la atencion por parte del personal medico
p798_6 <- p798_ind %>% filter(p798_ind$p798 == 6)
p798_6 <- sum(p798_6$Freq)

# Problemas relacionados con los medicamentos
p798_7 <- p798_ind %>% filter(p798_ind$p798 == 7)
p798_7 <- sum(p798_7$Freq)

# Otro
p798_8 <- p798_ind %>% filter(p798_ind$p798 == 8)
p798_8 <- sum(p798_8$Freq)

# Total p798
p798_total <- p798_1 + p798_2 + p798_3 + p798_4 + p798_5 + p798_6 + p798_7 + p798_8  # 5.186.688
##############################################################################################################################################################

# 9. ¿Cuáles de los siguientes planes o seguros VOLUNTARIOS de salud tiene_________? Medicina prepagada

p799s2_ind <- data.frame(svytable(~fex_c + p799s2,
                                  design = BD_survey)) 
# Si
p799s2_1 <- p799s2_ind %>% filter(p799s2_ind$p799s2 == 1)
p799s2_1 <- sum(p799s2_1$Freq)

# No
p799s2_2 <- p799s2_ind %>% filter(p799s2_ind$p799s2 == 2)
p799s2_2 <- sum(p799s2_2$Freq)                               # 49.586.947

# total p799s2
p799s2_total <- p799s2_1 + p799s2_2
##############################################################################################################################################################

# 9.2 Plan complementario de salud con una EPS
p799s3_ind <- data.frame(svytable(~fex_c + p799s3,
                                design = BD_survey))
# Si 
p799s3_1 <- p799s3_ind %>% filter(p799s3_ind$p799s3 == 1)
p799s3_1 <- sum(p799s3_1$Freq)

# No
p799s3_2 <- p799s3_ind %>% filter(p799s3_ind$p799s3 == 2)
p799s3_2 <- sum(p799s3_2$Freq)                             # 49.918.373

# total p799s3
p799s3_total <- p799s3_1 + p799s3_2
##############################################################################################################################################################

# 9.3 Poliza de hospitalizacion o cirugia
p799s1_ind <- data.frame(svytable(~fex_c + p799s1,
                                  design = BD_survey))
# Si 
p799s1_1 <- p799s1_ind %>% filter(p799s1_ind$p799s1 == 1)
p799s1_1 <- sum(p799s1_1$Freq)

# No
p799s1_2 <- p799s1_ind %>% filter(p799s1_ind$p799s1 == 2)
p799s1_2 <- sum(p799s1_2$Freq)                              # 50.998.175  

# total p799s1
p799s1_total <- p799s1_1 + p799s1_2 
###############################################################################################################################################################

# 9.4 Seguros medicos estudiantiles
p799s4_ind <- data.frame(svytable(~fex_c + p799s4,
                                  design = BD_survey))
# Si 
p799s4_1 <- p799s4_ind %>% filter(p799s4_ind$p799s4 == 1)
p799s4_1 <- sum(p799s4_1$Freq)

# No
p799s4_2 <- p799s4_ind %>% filter(p799s4_ind$p799s4 == 2)
p799s4_2 <- sum(p799s4_2$Freq)                              # 50.959.603

# total p799s4
p799s4_total <- p799s4_1 + p799s4_2 
##############################################################################################################################################################

# 9.5 Otro (ambulancia, asistencia medica domiciliaria, etc)
p799s5_ind <- data.frame(svytable(~fex_c + p799s5,
                                  design = BD_survey))
# Si 
p799s5_1 <- p799s5_ind %>% filter(p799s5_ind$p799s5 == 1)
p799s5_1 <- sum(p799s5_1$Freq)

# No
p799s5_2 <- p799s5_ind %>% filter(p799s5_ind$p799s5 == 2)
p799s5_2 <- sum(p799s5_2$Freq)                              # 50.851.187

# total p799s5
p799s5_total <- p799s5_1 + p799s5_2 
################################################################################################################################################################

# 10. ¿Cuánto paga o  le descuentan mensualmente a ________ por concepto de estos planes o seguros voluntarios de salud?
p3176 <- table(BD_ecv_ind$p3176)
################################################################################################################################################################

# 11. El estado de salud de ... en general, es:
p6127_ind <- data.frame(svytable(~fex_c + p6127,
                                 design = BD_survey))
# Muy bueno
p6127_1 <- p6127_ind %>% filter(p6127_ind$p6127 == 1)
p6127_1 <- sum(p6127_1$Freq)                             # 9.728.470

# Bueno
p6127_2 <- p6127_ind %>% filter(p6127_ind$p6127 == 2)
p6127_2 <- sum(p6127_2$Freq)                             # 35.022.390

# Regular
p6127_3 <- p6127_ind %>% filter(p6127_ind$p6127 == 3)
p6127_3 <- sum(p6127_3$Freq)                            # 6.011.597

# Malo
p6127_4 <- p6127_ind %>% filter(p6127_ind$p6127 == 4)
p6127_4 <- sum(p6127_4$Freq)                            # 461.600

# Total p6127
p6127_total <- p6127_1 + p6127_2 + p6127_3 + p6127_4    # 51.224.060
###############################################################################################################################################################

# 12.1 ¿A....le han diagnosticado alguna enfermedad crónica? (enfermedad de larga duración y prolongados tratamientos como: 
#    enfermedades cardiovasculares-hipertensión, asma, bronquitis crónica, gastritis, lupus, cáncer, gota, leucemia, diabetes, etc.).

p1930_ind <- data.frame(svytable(~fex_c + p1930,
                                 design = BD_survey))
# Si
p1930_1 <- p1930_ind %>% filter(p1930_ind$p1930 == 1)
p1930_1 <- sum(p1930_1$Freq)                            # 7.902.500

# No
p1930_2 <- p1930_ind %>% filter(p1930_ind$p1930 == 2)
p1930_2 <- sum(p1930_2$Freq)                            # 43.321.560

# total p1930
p1930_total <- p1930_1 + p1930_2
################################################################################################################################################################

# 12.2 ¿Recibe o recibió tratamiento formulado por el médico?
p1930s1_ind <- data.frame(svytable(~fex_c + p1930s1,
                                 design = BD_survey))

# Si
p1930s1_1 <- p1930s1_ind %>% filter(p1930s1_ind$p1930s1 == 1)
p1930s1_1 <- sum(p1930s1_1$Freq)                                   # 7.054.214

# No
p1930s1_2 <- p1930s1_ind %>% filter(p1930s1_ind$p1930s1 == 2)
p1930s1_2 <- sum(p1930s1_2$Freq)                                  # 848.285

# total p1930s1
p1930s1_total <- p1930s1_1 + p1930s1_2
###############################################################################################################################################################
# 13.1 Dada su condición física y mental, y sin ningún tipo de ayuda, ¿ ... puede: Oir la voz o los sonidos?
p1906s1_ind <- data.frame(svytable(~fex_c + p1906s1,
                                 design = BD_survey))

# No puede hacerlo
p1906s1_1 <- p1906s1_ind %>% filter(p1906s1_ind$p1906s1 == 1)
p1906s1_1 <- sum(p1906s1_1$Freq)                                # 64.930

# Si, con mucha dificultad
p1906s1_2 <- p1906s1_ind %>% filter(p1906s1_ind$p1906s1 == 2)
p1906s1_2 <- sum(p1906s1_2$Freq)                                # 394.853

# Si, con alguna dificultad
p1906s1_3 <- p1906s1_ind %>% filter(p1906s1_ind$p1906s1 == 3)
p1906s1_3 <- sum(p1906s1_3$Freq)                                # 1.294.267

# Sin dificultad
p1906s1_4 <- p1906s1_ind %>% filter(p1906s1_ind$p1906s1 == 4)
p1906s1_4 <- sum(p1906s1_4$Freq)                                # 49.470.008

# total p1906s1
p1906s1_total <- p1906s1_1 + p1906s1_2 + p1906s1_3 + p1906s1_4  # 51.224.060
################################################################################################################################################################

# 13.2 Hablar o conversar
p1906s2_ind <- data.frame(svytable(~fex_c + p1906s2,
                                   design = BD_survey))
# No puede hacerlo
p1906s2_1 <- p1906s2_ind %>% filter(p1906s2_ind$p1906s2 == 1)
p1906s2_1 <- sum(p1906s2_1$Freq)                                # 241.458

# Si, con mucha dificultad
p1906s2_2 <- p1906s2_ind %>% filter(p1906s2_ind$p1906s2 == 2)
p1906s2_2 <- sum(p1906s2_2$Freq)                                # 257.339

# Si, con alguna dificultad
p1906s2_3 <- p1906s2_ind %>% filter(p1906s2_ind$p1906s2 == 3)
p1906s2_3 <- sum(p1906s2_3$Freq)                               # 716.621

# Sin dificultad
p1906s2_4 <- p1906s2_ind %>% filter(p1906s2_ind$p1906s2 == 4)
p1906s2_4 <- sum(p1906s2_4$Freq)                              # 50.008.640

# total p1906s2
p1906s2_total <- p1906s2_1 + p1906s2_2 + p1906s2_3 + p1906s2_4  # 51.224.060
################################################################################################################################################################

# 13.3 Ver de verca, de lejos o alrededor
p1906s3_ind <- data.frame(svytable(~fex_c + p1906s3,
                                   design = BD_survey))
# No puede hacerlo
p1906s3_1 <- p1906s3_ind %>% filter(p1906s3_ind$p1906s3 == 1)
p1906s3_1 <- sum(p1906s3_1$Freq)                               # 60.483

# Si, con mucha dificultad
p1906s3_2 <- p1906s3_ind %>% filter(p1906s3_ind$p1906s3 == 2)
p1906s3_2 <- sum(p1906s3_2$Freq)                               # 1.510.469

# Si, con alguna dificultad
p1906s3_3 <- p1906s3_ind %>% filter(p1906s3_ind$p1906s3 == 3)
p1906s3_3 <- sum(p1906s3_3$Freq)                               # 7.705.725

# Sin dificultad
p1906s3_4 <- p1906s3_ind %>% filter(p1906s3_ind$p1906s3 == 4)
p1906s3_4 <- sum(p1906s3_4$Freq)                              # 41.947.381

# Total p1906s3
p1906s3_total <- p1906s3_1 + p1906s3_2 + p1906s3_3 + p1906s3_4  # 51.224.060
################################################################################################################################################################

# 13.4 Mover el cuerpo, caminar o subir y bajar escaleras?
p1906s4_ind <- data.frame(svytable(~fex_c + p1906s4,
                                   design = BD_survey))
# No puede hacerlo
p1906s4_1 <- p1906s4_ind %>% filter(p1906s4_ind$p1906s4 == 1)
p1906s4_1 <- sum(p1906s4_1$Freq)                               # 261.957

# Si, con mucha dificultad
p1906s4_2 <- p1906s4_ind %>% filter(p1906s4_ind$p1906s4 == 2)
p1906s4_2 <- sum(p1906s4_2$Freq)                              # 831.341

# Si, con alguna dificultad
p1906s4_3 <- p1906s4_ind %>% filter(p1906s4_ind$p1906s4 == 3)
p1906s4_3 <- sum(p1906s4_3$Freq)                              # 2.423.154

# Sin dificultad
p1906s4_4 <- p1906s4_ind %>% filter(p1906s4_ind$p1906s4 == 4)
p1906s4_4 <- sum(p1906s4_4$Freq)                               # 47.707.606

# total p1906s4
p1906s4_total <- p1906s4_1 + p1906s4_2 + p1906s4_3 + p1906s4_4   # 51.224.060
#############################################################################################################################################################

# 13.5 Agarrar o mover objetos con las manos?
p1906s5_ind <- data.frame(svytable(~fex_c + p1906s5,
                                   design = BD_survey))

# No puede hacerlo 
p1906s5_1 <- p1906s5_ind %>% filter(p1906s5_ind$p1906s5 == 1)
p1906s5_1 <- sum(p1906s5_1$Freq)                                # 112.440

# Si, con mucha dificultad
p1906s5_2 <- p1906s5_ind %>% filter(p1906s5_ind$p1906s5 == 2)
p1906s5_2 <- sum(p1906s5_2$Freq)                               # 341.544

# Si, con alguna dificultad
p1906s5_3 <- p1906s5_ind %>% filter(p1906s5_ind$p1906s5 == 3)
p1906s5_3 <- sum(p1906s5_3$Freq)                               # 1.204.316

# Sin dificultad
p1906s5_4 <- p1906s5_ind %>% filter(p1906s5_ind$p1906s5 == 4)
p1906s5_4 <- sum(p1906s5_4$Freq)                              # 49.565.758

#  total p1906s5
p1906s5_total <- p1906s5_1 + p1906s5_2 + p1906s5_3 + p1906s5_4   # 51.224.060
##############################################################################################################################################################

# 13.6 Entender, aprender, recordar o tomar decisiones por sí mismo/a?
p1906s6_ind <- data.frame(svytable(~fex_c + p1906s6, 
                                   design = BD_survey))

# No puede hacerlo
p1906s6_1 <- p1906s6_ind %>% filter(p1906s6_ind$p1906s6 == 1)
p1906s6_1 <- sum(p1906s6_1$Freq)                              # 324.313

# Si, con mucha dificultad
p1906s6_2 <- p1906s6_ind %>% filter(p1906s6_ind$p1906s6 == 2)
p1906s6_2 <- sum(p1906s6_2$Freq)                               # 301.788

# Si, con alguna dificultad
p1906s6_3 <- p1906s6_ind %>% filter(p1906s6_ind$p1906s6 == 3)
p1906s6_3 <- sum(p1906s6_3$Freq)                                # 890.502

# Sin dificultad
p1906s6_4 <- p1906s6_ind %>% filter(p1906s6_ind$p1906s6 == 4)
p1906s6_4 <- sum(p1906s6_4$Freq)                               # 49.707.455

# total p1906s6
p1906s6_total <- p1906s6_1 + p1906s6_2 + p1906s6_3 + p1906s6_4
###############################################################################################################################################################

# 13.7 Comer, vestirse o bañarse por sí mismo/a?
p1906s7_ind <- data.frame(svytable(~fex_c + p1906s7,
                                   design = BD_survey))

# No puede hacerlo 
p1906s7_1 <- p1906s7_ind %>% filter(p1906s7_ind$p1906s7 ==1)
p1906s7_1 <- sum(p1906s7_1$Freq)                               # 377.034

# Si, con mucha dificultad
p1906s7_2 <- p1906s7_ind %>% filter(p1906s7_ind$p1906s7 == 2)
p1906s7_2 <- sum(p1906s7_2$Freq)                               # 205.757

# Si, con alguna dificultad
p1906s7_3 <- p1906s7_ind %>% filter(p1906s7_ind$p1906s7 == 3)
p1906s7_3 <- sum(p1906s7_3$Freq)                               # 764.642

# Sin dificultad
p1906s7_4 <- p1906s7_ind %>% filter(p1906s7_ind$p1906s7 == 4)
p1906s7_4 <- sum(p1906s7_4$Freq)                               # 49.876.625

# total p1906s7
p1906s7_total <- p1906s7_1 + p1906s7_2 + p1906s7_3 + p1906s7_4
###########################################################################################################################################################

# 13.8 Relacionarse o interactuar con las demás personas?
p1906s8_ind <- data.frame(svytable(~fex_c + p1906s8,
                                   design = BD_survey))

# No puede hacerlo 
p1906s8_1 <- p1906s8_ind %>% filter(p1906s8_ind$p1906s8 == 1)
p1906s8_1 <- sum(p1906s8_1$Freq)                               # 210.839

# Si, con mucha dificultad
p1906s8_2 <- p1906s8_ind %>% filter(p1906s8_ind$p1906s8 == 2)
p1906s8_2 <- sum(p1906s8_2$Freq)                               # 207.426

# Si, con alguna dificultad
p1906s8_3 <- p1906s8_ind %>% filter(p1906s8_ind$p1906s8 == 3)
p1906s8_3 <- sum(p1906s8_3$Freq)                               # 571.846

# Sin dificultad
p1906s8_4 <- p1906s8_ind %>% filter(p1906s8_ind$p1906s8 == 4)  
p1906s8_4 <- sum(p1906s8_4$Freq)                               # 50.233.957

# total p1906s8
p1906s8_total <- p1906s8_1 + p1906s8_2 + p1906s8_3 + p1906s8_4
#############################################################################################################################################################

# 14.1 ¿Esta dificultad de ... fue ocasionada: ¿Esta dificultad (Oír la voz o los sonidos) de ... fue ocasionada
p1908s1_ind <- data.frame(svytable(~fex_c + p1908s1,
                                   design = BD_survey))
# Porque nació así
p1908s1_1 <- p1908s1_ind %>% filter(p1908s1_ind$p1908s1 == 1)
p1908s1_1 <- sum(p1908s1_1$Freq)                                 # 81.880

# Por enfermedad
p1908s1_2 <- p1908s1_ind %>% filter(p1908s1_ind$p1908s1 == 2)
p1908s1_2 <- sum(p1908s1_2$Freq)                                 # 120.070

# Por accidente laboral o enfermedad profesional
p1908s1_3 <- p1908s1_ind %>% filter(p1908s1_ind$p1908s1 == 3)
p1908s1_3 <- sum(p1908s1_3$Freq)                                 # 20.859

# Por otro tipo de accidente
p1908s1_4 <- p1908s1_ind %>% filter(p1908s1_ind$p1908s1 == 4)
p1908s1_4 <- sum(p1908s1_4$Freq)                                # 21.337

# Por edad avanzada
p1908s1_5 <- p1908s1_ind %>% filter(p1908s1_ind$p1908s1 == 5)
p1908s1_5 <- sum(p1908s1_5$Freq)                                # 189.089

# Por el conflicto armado
p1908s1_6 <- p1908s1_ind %>% filter(p1908s1_ind$p1908s1 == 6)
p1908s1_6 <- sum(p1908s1_6$Freq)                               # 3.716

# Por violencia NO asociada al conflicto armado
p1908s1_7 <- p1908s1_ind %>% filter(p1908s1_ind$p1908s1 == 7)
p1908s1_7 <- sum(p1908s1_7$Freq)                               # 3.196

# Por otra causa
p1908s1_8 <- p1908s1_ind %>% filter(p1908s1_ind$p1908s1 == 8)
p1908s1_8 <- sum(p1908s1_8$Freq)                               # 10.715

# No sabe
p1908s1_9 <- p1908s1_ind %>% filter(p1908s1_ind$p1908s1 == 9)
p1908s1_9 <- sum(p1908s1_9$Freq)                               # 8.918

# total 1908s1
p1908s1_total <- p1908s1_1 + p1908s1_2 + p1908s1_3 + p1908s1_4 + p1908s1_5 + p1908s1_6 + p1908s1_7 + p1908s1_8 + p1908s1_9   # 459.783
###########################################################################################################################################

# 14.2 ¿Esta dificultad (Hablar o conversar) de ... fue ocasionada:
p1908s2_ind <- data.frame(svytable(~fex_c + p1908s2,
                                   design = BD_survey))
# Porque nació así
p1908s2_1 <- p1908s2_ind %>% filter(p1908s2_ind$p1908s2 == 1)
p1908s2_1 <- sum(p1908s2_1$Freq)                                 # 167.274

# Por enfermedad
p1908s2_2 <- p1908s2_ind %>% filter(p1908s2_ind$p1908s2 == 2)
p1908s2_2 <- sum(p1908s2_2$Freq)                                 # 143.084

# Por accidente laboral o enfermedad profesional
p1908s2_3 <- p1908s2_ind %>% filter(p1908s2_ind$p1908s2 == 3)
p1908s2_3 <- sum(p1908s2_3$Freq)                                 # 2.768

# Por otro tipo de accidente
p1908s2_4 <- p1908s2_ind %>% filter(p1908s2_ind$p1908s2 == 4)
p1908s2_4 <- sum(p1908s2_4$Freq)                                # 9.081

# Por edad avanzada
p1908s2_5 <- p1908s2_ind %>% filter(p1908s2_ind$p1908s2 == 5)
p1908s2_5 <- sum(p1908s2_5$Freq)                                # 38.205

# Por el conflicto armado
p1908s2_6 <- p1908s2_ind %>% filter(p1908s2_ind$p1908s2 == 6)
p1908s2_6 <- sum(p1908s2_6$Freq)                               # 3.716

# Por violencia NO asociada al conflicto armado
p1908s2_7 <- p1908s2_ind %>% filter(p1908s2_ind$p1908s2 == 7)
p1908s2_7 <- sum(p1908s2_7$Freq)                               # 3.678

# Por otra causa
p1908s2_8 <- p1908s2_ind %>% filter(p1908s2_ind$p1908s2 == 8)
p1908s2_8 <- sum(p1908s2_8$Freq)                               # 90.537

# No sabe
p1908s2_9 <- p1908s2_ind %>% filter(p1908s2_ind$p1908s2 == 9)
p1908s2_9 <- sum(p1908s2_9$Freq)                               # 44.167

# total 1908s1
p1908s2_total <- p1908s2_1 + p1908s2_2 + p1908s2_3 + p1908s2_4 + p1908s2_5 + p1908s2_6 + p1908s2_7 + p1908s2_8 + p1908s2_9   # 498.798
#####################################################################################################################################################

# 14.3 ¿Esta dificultad (Ver de cerca, de lejos o alrededor) de ... fue ocasionada:
p1908s3_ind <- data.frame(svytable(~fex_c + p1908s3,
                                   design = BD_survey))
# Porque nació así
p1908s3_1 <- p1908s3_ind %>% filter(p1908s3_ind$p1908s3 == 1)
p1908s3_1 <- sum(p1908s3_1$Freq)                                 # 196.590

# Por enfermedad
p1908s3_2 <- p1908s3_ind %>% filter(p1908s3_ind$p1908s3 == 2)
p1908s3_2 <- sum(p1908s3_2$Freq)                                 # 652.913

# Por accidente laboral o enfermedad profesional
p1908s3_3 <- p1908s3_ind %>% filter(p1908s3_ind$p1908s3 == 3)
p1908s3_3 <- sum(p1908s3_3$Freq)                                 # 29.999

# Por otro tipo de accidente
p1908s3_4 <- p1908s3_ind %>% filter(p1908s3_ind$p1908s3 == 4)
p1908s3_4 <- sum(p1908s3_4$Freq)                                # 33.839

# Por edad avanzada
p1908s3_5 <- p1908s3_ind %>% filter(p1908s3_ind$p1908s3 == 5)
p1908s3_5 <- sum(p1908s3_5$Freq)                                # 568.106

# Por el conflicto armado
p1908s3_6 <- p1908s3_ind %>% filter(p1908s3_ind$p1908s3 == 6)
p1908s3_6 <- sum(p1908s3_6$Freq)                               # 1.518

# Por violencia NO asociada al conflicto armado
p1908s3_7 <- p1908s3_ind %>% filter(p1908s3_ind$p1908s3 == 7)
p1908s3_7 <- sum(p1908s3_7$Freq)                               # 2.739

# Por otra causa
p1908s3_8 <- p1908s3_ind %>% filter(p1908s3_ind$p1908s3 == 8)
p1908s3_8 <- sum(p1908s3_8$Freq)                               # 41.505

# No sabe
p1908s3_9 <- p1908s3_ind %>% filter(p1908s3_ind$p1908s3 == 9)
p1908s3_9 <- sum(p1908s3_9$Freq)                               # 43.738

# total 1908s1
p1908s3_total <- p1908s3_1 + p1908s3_2 + p1908s3_3 + p1908s3_4 + p1908s3_5 + p1908s3_6 + p1908s3_7 + p1908s3_8 + p1908s3_9   # 1.570.952
###################################################################################################################################################################3

# 14. 4 ¿Esta dificultad (Mover el cuerpo, caminar o subir y bajar escaleras) de ... fue ocasionada:
p1908s4_ind <- data.frame(svytable(~fex_c + p1908s4,
                                   design = BD_survey))
# Porque nació así
p1908s4_1 <- p1908s4_ind %>% filter(p1908s4_ind$p1908s4 == 1)
p1908s4_1 <- sum(p1908s4_1$Freq)                                 # 77.018

# Por enfermedad
p1908s4_2 <- p1908s4_ind %>% filter(p1908s4_ind$p1908s4 == 2)
p1908s4_2 <- sum(p1908s4_2$Freq)                                 # 494.919

# Por accidente laboral o enfermedad profesional
p1908s4_3 <- p1908s4_ind %>% filter(p1908s4_ind$p1908s4 == 3)
p1908s4_3 <- sum(p1908s4_3$Freq)                                 # 42.026

# Por otro tipo de accidente
p1908s4_4 <- p1908s4_ind %>% filter(p1908s4_ind$p1908s4 == 4)
p1908s4_4 <- sum(p1908s4_4$Freq)                                # 115.369

# Por edad avanzada
p1908s4_5 <- p1908s4_ind %>% filter(p1908s4_ind$p1908s4 == 5)
p1908s4_5 <- sum(p1908s4_5$Freq)                                # 239.833

# Por el conflicto armado
p1908s4_6 <- p1908s4_ind %>% filter(p1908s4_ind$p1908s4 == 6)
p1908s4_6 <- sum(p1908s4_6$Freq)                               # 5.294

# Por violencia NO asociada al conflicto armado
p1908s4_7 <- p1908s4_ind %>% filter(p1908s4_ind$p1908s4 == 7)
p1908s4_7 <- sum(p1908s4_7$Freq)                               # 6.077

# Por otra causa
p1908s4_8 <- p1908s4_ind %>% filter(p1908s4_ind$p1908s4 == 8)
p1908s4_8 <- sum(p1908s4_8$Freq)                               # 80.451

# No sabe
p1908s4_9 <- p1908s4_ind %>% filter(p1908s4_ind$p1908s4 == 9)
p1908s4_9 <- sum(p1908s4_9$Freq)                               # 32.306

# total 1908s1
p1908s4_total <- p1908s4_1 + p1908s4_2 + p1908s4_3 + p1908s4_4 + p1908s4_5 + p1908s4_6 + p1908s4_7 + p1908s4_8 + p1908s4_9   # 1.093.298
############################################################################################################################################################3

# 14.5 ¿Esta dificultad (Agarrar o mover objetos con las manos) de ... fue ocasionada:
p1908s5_ind <- data.frame(svytable(~fex_c + p1908s5,
                                   design = BD_survey))
# Porque nació así
p1908s5_1 <- p1908s5_ind %>% filter(p1908s5_ind$p1908s5 == 1)
p1908s5_1 <- sum(p1908s5_1$Freq)                                 # 48.466

# Por enfermedad
p1908s5_2 <- p1908s5_ind %>% filter(p1908s5_ind$p1908s5 == 2)
p1908s5_2 <- sum(p1908s5_2$Freq)                                 # 217.013

# Por accidente laboral o enfermedad profesional
p1908s5_3 <- p1908s5_ind %>% filter(p1908s5_ind$p1908s5 == 3)
p1908s5_3 <- sum(p1908s5_3$Freq)                                 # 24.418

# Por otro tipo de accidente
p1908s5_4 <- p1908s5_ind %>% filter(p1908s5_ind$p1908s5 == 4)
p1908s5_4 <- sum(p1908s5_4$Freq)                                # 46.664

# Por edad avanzada
p1908s5_5 <- p1908s5_ind %>% filter(p1908s5_ind$p1908s5 == 5)
p1908s5_5 <- sum(p1908s5_5$Freq)                                # 69.480

# Por el conflicto armado
p1908s5_6 <- p1908s5_ind %>% filter(p1908s5_ind$p1908s5 == 6)
p1908s5_6 <- sum(p1908s5_6$Freq)                               # 1.004

# Por violencia NO asociada al conflicto armado
p1908s5_7 <- p1908s5_ind %>% filter(p1908s5_ind$p1908s5 == 7)
p1908s5_7 <- sum(p1908s5_7$Freq)                               # 3.654

# Por otra causa
p1908s5_8 <- p1908s5_ind %>% filter(p1908s5_ind$p1908s5 == 8)
p1908s5_8 <- sum(p1908s5_8$Freq)                               # 33.318

# No sabe
p1908s5_9 <- p1908s5_ind %>% filter(p1908s5_ind$p1908s5 == 9)
p1908s5_9 <- sum(p1908s5_9$Freq)                               # 9.963

# total 1908s1
p1908s5_total <- p1908s5_1 + p1908s5_2 + p1908s5_3 + p1908s5_4 + p1908s5_5 + p1908s5_6 + p1908s5_7 + p1908s5_8 + p1908s5_9   # 453.984
#############################################################################################################################################################

# 14.6 ¿Esta dificultad (Entender, aprender, recordar o tomar decisiones por sí mismo/a) de ... fue ocasionada:
p1908s6_ind <- data.frame(svytable(~fex_c + p1908s6,
                                   design = BD_survey))
# Porque nació así
p1908s6_1 <- p1908s6_ind %>% filter(p1908s6_ind$p1908s6 == 1)
p1908s6_1 <- sum(p1908s6_1$Freq)                                 # 180.947

# Por enfermedad
p1908s6_2 <- p1908s6_ind %>% filter(p1908s6_ind$p1908s6 == 2)
p1908s6_2 <- sum(p1908s6_2$Freq)                                 # 180.746

# Por accidente laboral o enfermedad profesional
p1908s6_3 <- p1908s6_ind %>% filter(p1908s6_ind$p1908s6 == 3)
p1908s6_3 <- sum(p1908s6_3$Freq)                                 # 3.667

# Por otro tipo de accidente
p1908s6_4 <- p1908s6_ind %>% filter(p1908s6_ind$p1908s6 == 4)
p1908s6_4 <- sum(p1908s6_4$Freq)                                # 7.068

# Por edad avanzada
p1908s6_5 <- p1908s6_ind %>% filter(p1908s6_ind$p1908s6 == 5)
p1908s6_5 <- sum(p1908s6_5$Freq)                                # 70.598

# Por el conflicto armado
p1908s6_6 <- p1908s6_ind %>% filter(p1908s6_ind$p1908s6 == 6)
p1908s6_6 <- sum(p1908s6_6$Freq)                               # 67

# Por violencia NO asociada al conflicto armado
p1908s6_7 <- p1908s6_ind %>% filter(p1908s6_ind$p1908s6 == 7)
p1908s6_7 <- sum(p1908s6_7$Freq)                               # 3.718

# Por otra causa
p1908s6_8 <- p1908s6_ind %>% filter(p1908s6_ind$p1908s6 == 8)
p1908s6_8 <- sum(p1908s6_8$Freq)                               # 125.438

# No sabe
p1908s6_9 <- p1908s6_ind %>% filter(p1908s6_ind$p1908s6 == 9)
p1908s6_9 <- sum(p1908s6_9$Freq)                               # 53.847

# total 1908s1
p1908s6_total <- p1908s6_1 + p1908s6_2 + p1908s6_3 + p1908s6_4 + p1908s6_5 + p1908s6_6 + p1908s6_7 + p1908s6_8 + p1908s6_9   # 626.102
##############################################################################################################################################################

# 14.7 ¿Esta dificultad (Comer, vestirse o bañarse por sí mismo/a) de ... fue ocasionada:
p1908s7_ind <- data.frame(svytable(~fex_c + p1908s7,
                                   design = BD_survey))
# Porque nació así
p1908s7_1 <- p1908s7_ind %>% filter(p1908s7_ind$p1908s7 == 1)
p1908s7_1 <- sum(p1908s7_1$Freq)                                 # 72.800

# Por enfermedad
p1908s7_2 <- p1908s7_ind %>% filter(p1908s7_ind$p1908s7 == 2)
p1908s7_2 <- sum(p1908s7_2$Freq)                                 # 181.615

# Por accidente laboral o enfermedad profesional
p1908s7_3 <- p1908s7_ind %>% filter(p1908s7_ind$p1908s7 == 3)
p1908s7_3 <- sum(p1908s7_3$Freq)                                 # 9.181

# Por otro tipo de accidente
p1908s7_4 <- p1908s7_ind %>% filter(p1908s7_ind$p1908s7 == 4)
p1908s7_4 <- sum(p1908s7_4$Freq)                                # 24.485

# Por edad avanzada
p1908s7_5 <- p1908s7_ind %>% filter(p1908s7_ind$p1908s7 == 5)
p1908s7_5 <- sum(p1908s7_5$Freq)                                # 60.454

# Por el conflicto armado
p1908s7_6 <- p1908s7_ind %>% filter(p1908s7_ind$p1908s7 == 6)
p1908s7_6 <- sum(p1908s7_6$Freq)                               # 313

# Por violencia NO asociada al conflicto armado
p1908s7_7 <- p1908s7_ind %>% filter(p1908s7_ind$p1908s7 == 7)
p1908s7_7 <- sum(p1908s7_7$Freq)                               # 3.539

# Por otra causa
p1908s7_8 <- p1908s7_ind %>% filter(p1908s7_ind$p1908s7 == 8)
p1908s7_8 <- sum(p1908s7_8$Freq)                               # 170.171

# No sabe
p1908s7_9 <- p1908s7_ind %>% filter(p1908s7_ind$p1908s7 == 9)
p1908s7_9 <- sum(p1908s7_9$Freq)                               # 60.230

# total 1908s1
p1908s7_total <- p1908s7_1 + p1908s7_2 + p1908s7_3 + p1908s7_4 + p1908s7_5 + p1908s7_6 + p1908s7_7 + p1908s7_8 + p1908s7_9   # 582.792
##############################################################################################################################################################

# 14.8 ¿Esta dificultad (Relacionarse o interactuar con las demás personas?) de ... fue ocasionada:
p1908s8_ind <- data.frame(svytable(~fex_c + p1908s8,
                                   design = BD_survey))
# Porque nació así
p1908s8_1 <- p1908s8_ind %>% filter(p1908s8_ind$p1908s8 == 1)
p1908s8_1 <- sum(p1908s8_1$Freq)                                 # 122.240

# Por enfermedad
p1908s8_2 <- p1908s8_ind %>% filter(p1908s8_ind$p1908s8 == 2)
p1908s8_2 <- sum(p1908s8_2$Freq)                                 # 141.262

# Por accidente laboral o enfermedad profesional
p1908s8_3 <- p1908s8_ind %>% filter(p1908s8_ind$p1908s8 == 3)
p1908s8_3 <- sum(p1908s8_3$Freq)                                 # 2.177

# Por otro tipo de accidente
p1908s8_4 <- p1908s8_ind %>% filter(p1908s8_ind$p1908s8 == 4)
p1908s8_4 <- sum(p1908s8_4$Freq)                                # 3.826

# Por edad avanzada
p1908s8_5 <- p1908s8_ind %>% filter(p1908s8_ind$p1908s8 == 5)
p1908s8_5 <- sum(p1908s8_5$Freq)                                # 30.457

# Por el conflicto armado
p1908s8_6 <- p1908s8_ind %>% filter(p1908s8_ind$p1908s8 == 6)
p1908s8_6 <- sum(p1908s8_6$Freq)                               # 120

# Por violencia NO asociada al conflicto armado
p1908s8_7 <- p1908s8_ind %>% filter(p1908s8_ind$p1908s8 == 7)
p1908s8_7 <- sum(p1908s8_7$Freq)                               # 3.589

# Por otra causa
p1908s8_8 <- p1908s8_ind %>% filter(p1908s8_ind$p1908s8 == 8)
p1908s8_8 <- sum(p1908s8_8$Freq)                               # 81.753

# No sabe
p1908s8_9 <- p1908s8_ind %>% filter(p1908s8_ind$p1908s8 == 9)
p1908s8_9 <- sum(p1908s8_9$Freq)                               # 32.828

# total 1908s1
p1908s8_total <- p1908s8_1 + p1908s8_2 + p1908s8_3 + p1908s8_4 + p1908s8_5 + p1908s8_6 + p1908s8_7 + p1908s8_8 + p1908s8_9   # 418.255
#########################################################################################################################################################################################

# 15.1 ¿Para estas dificultades ____ utiliza de manera permanente: Gafas, lentes de contacto, lentes intraoculares, programa computacional adaptado, regleta Braille, perro guía, otros?
p1909s1_ind <- data.frame(svytable(~fex_c + p1909s1,
                                   design = BD_survey))

# Si
p1909s1_1 <- p1909s1_ind %>% filter(p1909s1_ind$p1909s1 == 1)
p1909s1_1 <- sum(p1909s1_1$Freq)                                # 1.530.735

# No
p1909s1_2 <- p1909s1_ind %>% filter(p1909s1_ind$p1909s1 == 2)
p1909s1_2 <- sum(p1909s1_2$Freq)                                # 1.622.963

# total p1909s1
p1909s1_total <- p1909s1_1 + p1909s1_2                        # 3.153.698
###########################################################################################################################################################################################

# 15.2 Bastón, silla de ruedas, muletas, caminador?
p1909s2_ind <- data.frame(svytable(~fex_c + p1909s2,
                                   design = BD_survey))

# Si
p1909s2_1 <- p1909s2_ind %>% filter(p1909s2_ind$p1909s2 == 1)
p1909s2_1 <- sum(p1909s2_1$Freq)                                # 628.000

# No
p1909s2_2 <- p1909s2_ind %>% filter(p1909s2_ind$p1909s2 == 2)
p1909s2_2 <- sum(p1909s2_2$Freq)                                # 2.525.697

# total p1909s2
p1909s2_total <- p1909s2_1 + p1909s2_2                    # 3.153.698
###########################################################################################################################################################################################

# 15.3 Audífonos medicados, implantes cocleares, otros?
p1909s3_ind <- data.frame(svytable(~fex_c + p1909s3,
                                   design = BD_survey))

# Si
p1909s3_1 <- p1909s3_ind %>% filter(p1909s3_ind$p1909s3 == 1)
p1909s3_1 <- sum(p1909s3_1$Freq)                                # 129.765

# No
p1909s3_2 <- p1909s3_ind %>% filter(p1909s3_ind$p1909s3 == 2)
p1909s3_2 <- sum(p1909s3_2$Freq)                                # 3.023.933

# total p1909s3
p1909s3_total <- p1909s3_1 + p1909s3_2                    # 3.153.698
###########################################################################################################################################################################################

# 15.4 Ayuda de otras personas?
p1909s4_ind <- data.frame(svytable(~fex_c + p1909s4,
                                   design = BD_survey))

# Si
p1909s4_1 <- p1909s4_ind %>% filter(p1909s4_ind$p1909s4 == 1)
p1909s4_1 <- sum(p1909s4_1$Freq)                                # 811.336

# No
p1909s4_2 <- p1909s4_ind %>% filter(p1909s4_ind$p1909s4 == 2)
p1909s4_2 <- sum(p1909s4_2$Freq)                                # 2.342.361

# total p1909s4
p1909s4_total <- p1909s4_1 + p1909s4_2                    # 3.153.698
#####################################################################################################################################################################

# 15.5 Medicamentos o terapias?
p1909s5_ind <- data.frame(svytable(~fex_c + p1909s5,
                                   design = BD_survey))

# Si
p1909s5_1 <- p1909s5_ind %>% filter(p1909s5_ind$p1909s5 == 1)
p1909s5_1 <- sum(p1909s5_1$Freq)                                # 822.865

# No
p1909s5_2 <- p1909s5_ind %>% filter(p1909s5_ind$p1909s5 == 2)
p1909s5_2 <- sum(p1909s5_2$Freq)                                # 2.330.833

# total p1909s5
p1909s5_total <- p1909s5_1 + p1909s5_2                    # 3.153.698
############################################################################################################################################################################

# 15.6 Prácticas de medicina ancestral?
p1909s6_ind <- data.frame(svytable(~fex_c + p1909s6,
                                   design = BD_survey))

# Si
p1909s6_1 <- p1909s6_ind %>% filter(p1909s6_ind$p1909s6 == 1)
p1909s6_1 <- sum(p1909s6_1$Freq)                                # 20.007

# No
p1909s6_2 <- p1909s6_ind %>% filter(p1909s6_ind$p1909s6 == 2)
p1909s6_2 <- sum(p1909s6_2$Freq)                                # 3.133.690

# total p1909s6
p1909s6_total <- p1909s6_1 + p1909s6_2                    # 3.153.698
#############################################################################################################################################################################

# 16. ¿Quién se ocupa principalmente del cuidado de ...?
p6126_ind <- data.frame(svytable(~fex_c + p6126,
                                 design = BD_survey))
# Una persona del hogar
p6126_1 <- p6126_ind %>% filter(p6126_ind$p6126 == 1)
p6126_1 <- sum(p6126_1$Freq)                            # 1.295.861

# Una persona de otro hogar no remunerada
p6126_2 <- p6126_ind %>% filter(p6126_ind$p6126 == 2)
p6126_2 <- sum(p6126_2$Freq)                            # 159.271

# Una persona de otro hogar remunerada
p6126_3 <- p6126_ind %>% filter(p6126_ind$p6126 == 3)
p6126_3 <- sum(p6126_3$Freq)                            # 55.173

# Permanece solo/a
p6126_4 <- p6126_ind %>% filter(p6126_ind$p6126 == 4)
p6126_4 <- sum(p6126_4$Freq)                            # 148.162

# No requiere cuidado
p6126_5 <- p6126_ind %>% filter(p6126_ind$p6126 == 5)
p6126_5 <- sum(p6126_5$Freq)                            # 1.495.230

# total p6126
p6126_total <- p6126_1 + p6126_2 + p6126_3 + p6126_4 + p6126_5
##############################################################################################################################################################################

# 16.1 Numero Orden
p6126s1_ind <- data.frame(svytable(~fex_c + p6126s1,
                                   design = BD_survey))
p6126s1_1 <- p6126s1_ind %>% filter(p6126s1_ind$p6126s1 == 1)
p6126s1_1 <- sum(p6126s1_1$Freq)                                  # 574.240

p6126s1_2 <- p6126s1_ind %>% filter(p6126s1_ind$p6126s1 == 2)
p6126s1_2 <- sum(p6126s1_2$Freq)                                  # 550.177

p6126s1_3 <- p6126s1_ind %>% filter(p6126s1_ind$p6126s1 == 3)
p6126s1_3 <- sum(p6126s1_3$Freq)                                  # 101.327

p6126s1_4 <- p6126s1_ind %>% filter(p6126s1_ind$p6126s1 == 4)
p6126s1_4 <- sum(p6126s1_4$Freq)                                  # 34.562

p6126s1_5 <- p6126s1_ind %>% filter(p6126s1_ind$p6126s1 == 5)
p6126s1_5 <- sum(p6126s1_5$Freq)                                  # 20.526

p6126s1_6 <- p6126s1_ind %>% filter(p6126s1_ind$p6126s1 == 6)
p6126s1_6 <- sum(p6126s1_6$Freq)                                  # 10.521

p6126s1_7 <- p6126s1_ind %>% filter(p6126s1_ind$p6126s1 == 7)
p6126s1_7 <- sum(p6126s1_7$Freq)                                  # 2.783

p6126s1_8 <- p6126s1_ind %>% filter(p6126s1_ind$p6126s1 == 8)
p6126s1_8 <- sum(p6126s1_8$Freq)                                  # 210

p6126s1_9 <- p6126s1_ind %>% filter(p6126s1_ind$p6126s1 == 9)
p6126s1_9 <- sum(p6126s1_9$Freq)                                  # 1.072

p6126s1_11 <- p6126s1_ind %>% filter(p6126s1_ind$p6126s1 == 11)   
p6126s1_11 <- sum(p6126s1_11$Freq)                                # 2,65

p6126s1_13 <- p6126s1_ind %>% filter(p6126s1_ind$p6126s1 == 13)   
p6126s1_13 <- sum(p6126s1_13$Freq)                                # 222

p6126s1_total <- p6126s1_1 + p6126s1_2 + p6126s1_3 + p6126s1_4 + p6126s1_5 + p6126s1_6 + p6126s1_7 + p6126s1_8 + p6126s1_9 + p6126s1_11 + p6126s1_13  # 1.295.646

#############################################################################################################################################################################
# 16.2 Sexo
p6126s2_ind <- data.frame(svytable(~fex_c + p6126s2,
                                   design = BD_survey))
# Hombre
p6126s2_1 <- p6126s2_ind %>% filter(p6126s2_ind$p6126s2 == 1)
p6126s2_1 <- sum(p6126s2_1$Freq)                                  # 27.594

# Mujer
p6126s2_2 <- p6126s2_ind %>% filter(p6126s2_ind$p6126s2 == 2)
p6126s2_2 <- sum(p6126s2_2$Freq)                                 # 186.850

# total p6126s2
p6126s2_total <- p6126s2_1 + p6126s2_2
#############################################################################################################################################################################
# 16.3 ¿Esta persona tuvo que dejar de trabajar para dedicarse al cuidado de...?
p6126s3_ind <- data.frame(svytable(~fex_c + p6126s3,
                                   design = BD_survey))

# Si 
p6126s3_1 <- p6126s3_ind %>% filter(p6126s3_ind$p6126s3 == 1)
p6126s3_1 <- sum(p6126s3_1$Freq)                               # 394.654

# No
p6126s3_2 <- p6126s3_ind %>% filter(p6126s3_ind$p6126s3 == 2)
p6126s3_2 <- sum(p6126s3_2$Freq)                               # 901.206

# total p6126s3
p6126s3_total <- p6126s3_1 + p6126s3_2
##############################################################################################################################################################################
# 17. En los últimos 30 días, ... ¿tuvo alguna enfermedad, accidente , problema odontológico o algún otro problema de salud que no haya implicado hospitalización?
p5665_ind <- data.frame(svytable(~fex_c + p5665,
                                 design = BD_survey))

# Si
p5665_1 <- p5665_ind %>% filter(p5665_ind$p5665 == 1)
p5665_1 <- sum(p5665_1$Freq)                            # 1.747.297
 
# No
p5665_2 <- p5665_ind %>% filter(p5665_ind$p5665 == 2)
p5665_2 <- sum(p5665_2$Freq)                           # 49.476.762

# total p5665
p5665_total <- p5665_1 + p5665_2
###############################################################################################################################################################################
# 18. Por ese problema de salud, ¿durante cuántos días en total dejó ... de realizar sus actividades normales? Número de dias
p6134_ind <- data.frame(svytable(~fex_c + p6134,
                                 design = BD_survey))
# 0 dias
p6134_0 <- p6134_ind %>% filter(p6134_ind$p6134 == 0)     # 448.995
p6134_0 <- sum(p6134_0$Freq)
# 1 dias
p6134_1 <- p6134_ind %>% filter(p6134_ind$p6134 == 1)     # 222.564
p6134_1 <- sum(p6134_1$Freq)
# 2 dias
p6134_2 <- p6134_ind %>% filter(p6134_ind$p6134 == 2)     # 224.850
p6134_2 <- sum(p6134_2$Freq)
# 3 dias
p6134_3 <- p6134_ind %>% filter(p6134_ind$p6134 == 3)     # 174.547
p6134_3 <- sum(p6134_3$Freq)
# 4 dias
p6134_4 <- p6134_ind %>% filter(p6134_ind$p6134 == 4)     # 69.171
p6134_4 <- sum(p6134_4$Freq)
# 5 dias
p6134_5 <- p6134_ind %>% filter(p6134_ind$p6134 == 5)     # 96.025
p6134_5 <- sum(p6134_5$Freq)
# 6 dias
p6134_6 <- p6134_ind %>% filter(p6134_ind$p6134 == 6)     # 23.981
p6134_6 <- sum(p6134_6$Freq)
# 7 dias
p6134_7 <- p6134_ind %>% filter(p6134_ind$p6134 == 7)     # 80.060
p6134_7 <- sum(p6134_7$Freq)
# 8 dias
p6134_8 <- p6134_ind %>% filter(p6134_ind$p6134 == 8)     # 93.623
p6134_8 <- sum(p6134_8$Freq)
# 9 dias
p6134_9 <- p6134_ind %>% filter(p6134_ind$p6134 == 9)     # 1.353
p6134_9 <- sum(p6134_9$Freq)
# 10 dias
p6134_10 <- p6134_ind %>% filter(p6134_ind$p6134 == 10)     # 50.409
p6134_10 <- sum(p6134_10$Freq)
# 11 dias
p6134_11 <- p6134_ind %>% filter(p6134_ind$p6134 == 11)     # 189
p6134_11 <- sum(p6134_11$Freq)
# 12 dias
p6134_12 <- p6134_ind %>% filter(p6134_ind$p6134 == 12)     # 4.917
p6134_12 <- sum(p6134_12$Freq)
# 13 dias
p6134_13 <- p6134_ind %>% filter(p6134_ind$p6134 == 13)     # 157
p6134_13 <- sum(p6134_13$Freq)
# 14 dias
p6134_14 <- p6134_ind %>% filter(p6134_ind$p6134 == 14)     # 13.412
p6134_14 <- sum(p6134_14$Freq)
# 15 dias
p6134_15 <- p6134_ind %>% filter(p6134_ind$p6134 == 15)     # 89.320
p6134_15 <- sum(p6134_15$Freq)
# 16 dias
p6134_16 <- p6134_ind %>% filter(p6134_ind$p6134 == 16)     # 206
p6134_16 <- sum(p6134_16$Freq)
# 17 dias
p6134_17 <- p6134_ind %>% filter(p6134_ind$p6134 == 17)     # 3.004
p6134_17 <- sum(p6134_17$Freq)
# 18 dias
p6134_18 <- p6134_ind %>% filter(p6134_ind$p6134 == 18)     # 1.525
p6134_18 <- sum(p6134_18$Freq)
# 19 dias
p6134_19 <- p6134_ind %>% filter(p6134_ind$p6134 == 19)     # 209
p6134_19 <- sum(p6134_19$Freq)
# 20 dias
p6134_20 <- p6134_ind %>% filter(p6134_ind$p6134 == 20)     # 29.542
p6134_20 <- sum(p6134_20$Freq)
# 21 dias
p6134_21 <- p6134_ind %>% filter(p6134_ind$p6134 == 21)     # 3.020
p6134_21 <- sum(p6134_21$Freq)
# 22 dias
p6134_22 <- p6134_ind %>% filter(p6134_ind$p6134 == 22)     # 3.059
p6134_22 <- sum(p6134_22$Freq)
# 23 dias
p6134_23 <- p6134_ind %>% filter(p6134_ind$p6134 == 23)     # 209
p6134_23 <- sum(p6134_23$Freq)
# 24 dias
p6134_24 <- p6134_ind %>% filter(p6134_ind$p6134 == 24)     # 1.093
p6134_24 <- sum(p6134_24$Freq)
# 25 dias
p6134_25 <- p6134_ind %>% filter(p6134_ind$p6134 == 25)     # 2.051
p6134_25 <- sum(p6134_25$Freq)
# 26 dias
p6134_26 <- p6134_ind %>% filter(p6134_ind$p6134 == 26)     # 700
p6134_26 <- sum(p6134_26$Freq)
# 27 dias
p6134_27 <- p6134_ind %>% filter(p6134_ind$p6134 == 27)     # 205
p6134_27 <- sum(p6134_27$Freq)
# 28 dias
p6134_28 <- p6134_ind %>% filter(p6134_ind$p6134 == 28)     # 341
p6134_28 <- sum(p6134_28$Freq)
#29 dias
p6134_29 <- p6134_ind %>% filter(p6134_ind$p6134 == 29)     # 171
p6134_29 <- sum(p6134_29$Freq)
# 30 dias
p6134_30 <- p6134_ind %>% filter(p6134_ind$p6134 == 30)     # 62.955
p6134_30 <- sum(p6134_30$Freq)
# 31 dias
p6134_31 <- p6134_ind %>% filter(p6134_ind$p6134 == 31)     # 156
p6134_31 <- sum(p6134_31$Freq)
# 35 dias
p6134_35 <- p6134_ind %>% filter(p6134_ind$p6134 == 35)     # 564
p6134_35 <- sum(p6134_35$Freq)
# 36 dias
p6134_36 <- p6134_ind %>% filter(p6134_ind$p6134 == 36)     # 2.001
p6134_36 <- sum(p6134_36$Freq)
# 37 dias
p6134_37 <- p6134_ind %>% filter(p6134_ind$p6134 == 37)     # 225
p6134_37 <- sum(p6134_37$Freq)
# 38 dias
p6134_38 <- p6134_ind %>% filter(p6134_ind$p6134 == 38)     # 23
p6134_38 <- sum(p6134_38$Freq)
# 40 dias
p6134_40 <- p6134_ind %>% filter(p6134_ind$p6134 == 40)     # 1.942
p6134_40 <- sum(p6134_40$Freq)
# 45 dias
p6134_45 <- p6134_ind %>% filter(p6134_ind$p6134 == 45)     # 6.126
p6134_45 <- sum(p6134_45$Freq)
# 50 dias
p6134_50 <- p6134_ind %>% filter(p6134_ind$p6134 == 50)     # 291
p6134_50 <- sum(p6134_50$Freq)
# 60 dias
p6134_60 <- p6134_ind %>% filter(p6134_ind$p6134 == 60)     # 16.127
p6134_60 <- sum(p6134_60$Freq)
# 64 dias
p6134_64 <- p6134_ind %>% filter(p6134_ind$p6134 == 64)     # 19
p6134_64 <- sum(p6134_64$Freq)
# 68 dias
p6134_68 <- p6134_ind %>% filter(p6134_ind$p6134 == 68)     # 918
p6134_68 <- sum(p6134_68$Freq)
# 72 dias
p6134_72 <- p6134_ind %>% filter(p6134_ind$p6134 == 72)     # 241
p6134_72 <- sum(p6134_72$Freq)
# 75 dias
p6134_75 <- p6134_ind %>% filter(p6134_ind$p6134 == 75)     # 2.675
p6134_75 <- sum(p6134_75$Freq)
# 80 dias
p6134_80 <- p6134_ind %>% filter(p6134_ind$p6134 == 80)     # 269
p6134_80 <- sum(p6134_80$Freq)
# 90 dias
p6134_90 <- p6134_ind %>% filter(p6134_ind$p6134 == 90)     # 6.197
p6134_90 <- sum(p6134_90$Freq)
# 98 dias
p6134_98 <- p6134_ind %>% filter(p6134_ind$p6134 == 98)     # 224
p6134_98 <- sum(p6134_98$Freq)
# 99 dias
p6134_99 <- p6134_ind %>% filter(p6134_ind$p6134 == 99)     # 1.085
p6134_99 <- sum(p6134_99$Freq)
# 100 dias
p6134_100 <- p6134_ind %>% filter(p6134_ind$p6134 == 100)     # 291
p6134_100 <- sum(p6134_100$Freq)
# 120 dias
p6134_120 <- p6134_ind %>% filter(p6134_ind$p6134 == 120)     # 6.036
p6134_120 <- sum(p6134_120$Freq)

# p6134 total
p6134_total <- p6134_0+p6134_1+p6134_2+p6134_3+p6134_4+p6134_5+p6134_6+p6134_7+p6134_8+p6134_9+p6134_10+p6134_11+
  p6134_12+p6134_13+p6134_14+p6134_15+p6134_16+p6134_17+p6134_18+p6134_19+p6134_20+p6134_21+p6134_22+p6134_23+
  p6134_24+p6134_25+p6134_26+p6134_27+p6134_28+p6134_29+p6134_30+p6134_31+p6134_35+p6134_36+p6134_37+p6134_38+
  p6134_40+p6134_45+p6134_50+p6134_60+p6134_64+p6134_68+p6134_72+p6134_75+p6134_80+p6134_90+p6134_98+p6134_99+p6134_100+p6134_120  # 1.747.297
#################################################################################################################################################################
# 19. Para tratar ese problema de salud, ¿que hizo principalmente ...?:
p8563_ind <- data.frame(svytable(~fex_c + p8563, 
                                 design = BD_survey))
# Acudió a Ia entidad de seguridad social en salud de la cual es afiliado/a
p8563_1 <- p8563_ind %>% filter(p8563_ind$p8563 == 1)     # 1.128.872
p8563_1 <- sum(p8563_1$Freq)
# Acudió de forma particular  a un médico general, especialista, odontólogo, terapeuta o profesional de la salud
p8563_2 <- p8563_ind %>% filter(p8563_ind$p8563 == 2)     # 187.631
p8563_2 <- sum(p8563_2$Freq)
# Acudió a un boticario, farmaceuta, droguista
p8563_3 <- p8563_ind %>% filter(p8563_ind$p8563 == 3)     # 72.711
p8563_3 <- sum(p8563_3$Freq)
# Consultó a un empírico, curandero, yerbatero, comadrona
p8563_4 <- p8563_ind %>% filter(p8563_ind$p8563 == 4)     # 5.313
p8563_4 <- sum(p8563_4$Freq)
# Asistió a terapias alternativas (acupuntura, esencias florales, musicoterapias, homeópata etc.)
p8563_5 <- p8563_ind %>% filter(p8563_ind$p8563 == 5)     # 2.727
p8563_5 <- sum(p8563_5$Freq)
# Usó remedios caseros 
p8563_6 <- p8563_ind %>% filter(p8563_ind$p8563 == 6)     # 173.571
p8563_6 <- sum(p8563_6$Freq)
# Se autorecetó
p8563_7 <- p8563_ind %>% filter(p8563_ind$p8563 == 7)     # 123.691
p8563_7 <- sum(p8563_7$Freq)
# Nada
p8563_8 <- p8563_ind %>% filter(p8563_ind$p8563 == 8)     # 52.778
p8563_8 <- sum(p8563_8$Freq)

# total p8563
p8563_total <- p8563_1+p8563_2+p8563_3+p8563_4+p8563_5+p8563_6+p8563_7+p8563_8     # 1.747.297
################################################################################################################################################################################
# 20. ¿Acudió al servicio de urgencias en la institución prestadora de servicios (hospital o clínica) pública o privada?
p1092_ind <- data.frame(svytable(~fex_c + p1092,
                                 design = BD_survey))
# Si
p1092_1 <- p1092_ind %>% filter(p1092_ind$p1092 == 1)  # 668.261
p1092_1 <- sum(p1092_1$Freq)
#No
p1092_2 <- p1092_ind %>% filter(p1092_ind$p1092 == 2)  # 460.611
p1092_2 <- sum(p1092_2$Freq)

# total p1092
p1092_total <- p1092_1 + p1092_2 
###################################################################################################################################################################################
# 21. ¿A ... le brindaron asistencia médica en el servicio de urgencias para solucionar el problema de salud?
p8573_ind <- data.frame(svytable(~fex_c + p8573,
                                 design = BD_survey))
# Si
p8573_1 <- p8573_ind %>% filter(p8573_ind$p8573 == 1)  # 641.745
p8573_1 <- sum(p8573_1$Freq)
#No
p8573_2 <- p8573_ind %>% filter(p8573_ind$p8573 == 2)  # 26.515
p8573_2 <- sum(p8573_2$Freq)

# p8573
p8573_total <- p8573_1 + p8573_2
##############################################################################################################################################################################3
# 22. ¿Cuál fue la razón principal por la que ... no recibió atención médica en el servicio de urgencias?
p8575_ind <- data.frame(svytable(~fex_c + p8575,
                                 design = BD_survey))
# El caso era leve
p8575_1 <- p8575_ind %>% filter(p8575_ind$p8575 == 1)  # 13.543
p8575_1 <- sum(p8575_1$Freq)
# Esperó demasiado tiempo y no lo/la atendieron
p8575_2 <- p8575_ind %>% filter(p8575_ind$p8575 == 2) # 4.229
p8575_2 <- sum(p8575_2$Freq)
# Indicaron que allí no lo/la podían atender porque no estaba afiliado/a a alguna entidad que tuviera contrato con ellos
p8575_3 <- p8575_ind %>% filter(p8575_ind$p8575 == 3)  # 1.161
p8575_3 <- sum(p8575_3$Freq)
# No tenía identificación y por eso lo/la rechazaron
p8575_4 <- p8575_ind %>% filter(p8575_ind$p8575 == 4)  # 196
p8575_4 <- sum(p8575_4$Freq)
# Indicaron que debían remitirlo/la a otra institución prestadora de servicios que tuviera los servicios requeridos para atenderlo/la
p8575_5 <- p8575_ind %>% filter(p8575_ind$p8575 == 5)  # 4.965
p8575_5 <- sum(p8575_5$Freq)
# No le dieron informacion
p8575_6 <- p8575_ind %>% filter(p8575_ind$p8575 == 6) # 2.038
p8575_6 <- sum(p8575_6$Freq)
# No sabe/no responde
p8575_9 <- p8575_ind %>% filter(p8575_ind$p8575 == 9)  # 380
p8575_9 <- sum(p8575_9$Freq)

# total p8575
p8575_total <- p8575_1+p8575_2+p8575_3+p8575_4+p8575_5+p8575_6+p8575_9  # 26.515
##################################################################################################################################################################
# 23. ¿Cuánto tiempo transcurrió entre el momento de llegar al servicio de urgencias y el momento de ser atendido/a por personal médico?
p8577_ind <- data.frame(svytable(~fex_c + p8577,
                                 design = BD_survey))
# Lo atendieron inmediatamente
p8577_1 <- p8577_ind %>% filter(p8577_ind$p8577 == 1)   # 223.498
p8577_1 <- sum(p8577_1$Freq)
# En maximo 30 minutos
p8577_2 <- p8577_ind %>% filter(p8577_ind$p8577 == 2)   # 151.202
p8577_2 <- sum(p8577_2$Freq)
# Entre 31 minutos y una hora
p8577_3 <- p8577_ind %>% filter(p8577_ind$p8577 == 3)   # 84.852
p8577_3 <- sum(p8577_3$Freq)
# Mas de una hora hasta dos horas
p8577_4 <- p8577_ind %>% filter(p8577_ind$p8577 == 4)   # 75.840
p8577_4 <- sum(p8577_4$Freq)
# Mas de dos horas
p8577_5 <- p8577_ind %>% filter(p8577_ind$p8577 == 5)   # 106.352
p8577_5 <- sum(p8577_5$Freq)

# total p8577
p8577_total <- p8577_1+p8577_2+p8577_3+p8577_4+p8577_5   # 641.745
####################################################################################################################################################################
# 24. En el servicio de urgencias fue atendido/a por:
p770_ind <- data.frame(svytable(~fex_c + p770,
                                design = BD_survey))
# Medico/a general
p770_1 <- p770_ind %>% filter(p770_ind$p770 == 1)  # 535.006
p770_1 <- sum(p770_1$Freq)
# Odontologo/a
p770_2 <- p770_ind %>% filter(p770_ind$p770 == 2)  # 27.464
p770_2 <- sum(p770_2$Freq)
# Especialista
p770_3 <- p770_ind %>% filter(p770_ind$p770 == 3)  # 79.273
p770_3 <- sum(p770_3$Freq)

# total p770
p770_total <- p770_1+p770_2+p770_3  # 641.745
#####################################################################################################################################################################
# 25. ¿Cuál fue la razón principal por la que ... no solicitó o no recibió atención médica?
p6153_ind <- data.frame(svytable(~fex_c + p6153,
                                 design = BD_survey))
# El caso era leve
p6153_1 <- p6153_ind %>% filter(p6153_ind$p6153 == 1)  # 259.267
p6153_1 <- sum(p6153_1$Freq)
# No tuvo tiempo
p6153_2 <- p6153_ind %>% filter(p6153_ind$p6153 == 2)  # 18.685
p6153_2 <- sum(p6153_2$Freq)
# El centro de atención queda lejos
p6153_3 <- p6153_ind %>% filter(p6153_ind$p6153 == 3)  # 17.632
p6153_3 <- sum(p6153_3$Freq)
# Falta de dinero
p6153_4 <- p6153_ind %>% filter(p6153_ind$p6153 == 4)  # 23.136
p6153_4 <- sum(p6153_4$Freq)
# Mal servicio o cita distanciada en el tiempo
p6153_5 <- p6153_ind %>% filter(p6153_ind$p6153 == 5)  # 22.811
p6153_5 <- sum(p6153_5$Freq)
# No lo/la atendieron
p6153_6 <- p6153_ind %>% filter(p6153_ind$p6153 == 6)  # 8.190
p6153_6 <- sum(p6153_6$Freq)
# No confia en los medicos
p6153_7 <- p6153_ind %>% filter(p6153_ind$p6153 == 7)  # 10.808
p6153_7 <- sum(p6153_7$Freq)
# Consulto antes y no le resolvieron el problema
p6153_8 <- p6153_ind %>% filter(p6153_ind$p6153 == 8)  # 7.366
p6153_8 <- sum(p6153_8$Freq)
# Muchos tramites para la cita
p6153_9 <- p6153_ind %>% filter(p6153_ind$p6153 == 9)  # 15.161
p6153_9 <- sum(p6153_9$Freq)
# No le cubrian o no le autorizaron la atencion
p6153_10 <- p6153_ind %>% filter(p6153_ind$p6153 == 10)  # 5.477
p6153_10 <- sum(p6153_10$Freq)
# Le hacen esperar mucho para atenderlo/la
p6153_11 <- p6153_ind %>% filter(p6153_ind$p6153 == 11)  # 14.279
p6153_11 <- sum(p6153_11$Freq)
# Dificultad para viajar
p6153_12 <- p6153_ind %>% filter(p6153_ind$p6153 == 12)  # 4.774
p6153_12 <- sum(p6153_12$Freq)
# otro
p6153_13 <- p6153_ind %>% filter(p6153_ind$p6153 == 13)  # 23.200
p6153_13 <- sum(p6153_13$Freq)

# total p6153
p6153_total <- p6153_1+p6153_2+p6153_3+p6153_4+p6153_5+p6153_6+p6153_7+p6153_8+p6153_9+p6153_10+p6153_11+p6153_12+p6153_13  # 430.793
###############################################################################################################################################################
# 26. ¿Cuántos días transcurrieron entre el momento de pedir la cita y el momento de la consulta con el medico/a general u odontólogo/a?
p6199_ind <- data.frame(svytable(~fex_c + p6199,
                                 design = BD_survey))
# Medico/a general
p6199_1 <- p6199_ind %>% filter(p6199_ind$p6199 == 1)  # 445.947
p6199_1 <- sum(p6199_1$Freq)
# Odontologo/a
p6199_2 <- p6199_ind %>% filter(p6199_ind$p6199 == 2)  # 106.069
p6199_2 <- sum(p6199_2$Freq)
# Acudio directo al especialista
p6199_3 <- p6199_ind %>% filter(p6199_ind$p6199 == 3)  # 96.225
p6199_3 <- sum(p6199_3$Freq)

# total p6199
p6199_total <- p6199_1 +p6199_2 + p6199_3
################################################################################################################################################################
# 26.1 Numero de dias
p6199s1_ind <- data.frame(svytable(~fex_c + p6199s1,
                                   design = BD_survey))
# 0 dias
p6199s1_0 <- p6199s1_ind %>% filter(p6199s1_ind$p6199s1 == 0)  # 188.847
p6199s1_0 <- sum(p6199s1_0$Freq)
# 1 dias
p6199s1_1 <- p6199s1_ind %>% filter(p6199s1_ind$p6199s1 == 1)  # 95.355
p6199s1_1 <- sum(p6199s1_1$Freq)
# 2 dias
p6199s1_2 <- p6199s1_ind %>% filter(p6199s1_ind$p6199s1 == 2)  # 53.732
p6199s1_2 <- sum(p6199s1_2$Freq)
# 3 dias
p6199s1_3 <- p6199s1_ind %>% filter(p6199s1_ind$p6199s1 == 3)  # 45.512
p6199s1_3 <- sum(p6199s1_3$Freq)
# 4 dias
p6199s1_4 <- p6199s1_ind %>% filter(p6199s1_ind$p6199s1 == 4)  # 15.734
p6199s1_4 <- sum(p6199s1_4$Freq)
# 5 dias
p6199s1_5 <- p6199s1_ind %>% filter(p6199s1_ind$p6199s1 == 5)  # 22.587
p6199s1_5 <- sum(p6199s1_5$Freq)
# 6 dias
p6199s1_6 <- p6199s1_ind %>% filter(p6199s1_ind$p6199s1 == 6)  # 2.989
p6199s1_6 <- sum(p6199s1_6$Freq)
# 7 dias
p6199s1_7 <- p6199s1_ind %>% filter(p6199s1_ind$p6199s1 == 7)  # 10.369
p6199s1_7 <- sum(p6199s1_7$Freq)
# 8 dias
p6199s1_8 <- p6199s1_ind %>% filter(p6199s1_ind$p6199s1 == 8)  # 39.203
p6199s1_8 <- sum(p6199s1_8$Freq)
# 9 dias
p6199s1_9 <- p6199s1_ind %>% filter(p6199s1_ind$p6199s1 == 9)  # 933
p6199s1_9 <- sum(p6199s1_9$Freq)
# 10 dias
p6199s1_10 <- p6199s1_ind %>% filter(p6199s1_ind$p6199s1 == 10)  # 10.268
p6199s1_10 <- sum(p6199s1_10$Freq)
# 12 dias
p6199s1_12 <- p6199s1_ind %>% filter(p6199s1_ind$p6199s1 == 12)  # 3.413
p6199s1_12 <- sum(p6199s1_12$Freq)
# 14 dias
p6199s1_14 <- p6199s1_ind %>% filter(p6199s1_ind$p6199s1 == 14)  # 188
p6199s1_14 <- sum(p6199s1_14$Freq)
# 15 dias
p6199s1_15 <- p6199s1_ind %>% filter(p6199s1_ind$p6199s1 == 15)  # 30.295
p6199s1_15 <- sum(p6199s1_15$Freq)
# 20 dias
p6199s1_20 <- p6199s1_ind %>% filter(p6199s1_ind$p6199s1 == 20)  # 9.913
p6199s1_20 <- sum(p6199s1_20$Freq)
# 21 dias
p6199s1_21 <- p6199s1_ind %>% filter(p6199s1_ind$p6199s1 == 21)  # 314
p6199s1_21 <- sum(p6199s1_21$Freq)
# 23 dias
p6199s1_23 <- p6199s1_ind %>% filter(p6199s1_ind$p6199s1 == 23)  # 1.324
p6199s1_23 <- sum(p6199s1_23$Freq)
# 25 dias
p6199s1_25 <- p6199s1_ind %>% filter(p6199s1_ind$p6199s1 == 25)  # 141
p6199s1_25 <- sum(p6199s1_25$Freq)
# 30 dias
p6199s1_30 <- p6199s1_ind %>% filter(p6199s1_ind$p6199s1 == 30)  # 20.889
p6199s1_30 <- sum(p6199s1_30$Freq)

# total p6199s1
p6199s1_total <- p6199s1_0+p6199s1_1+p6199s1_2+p6199s1_3+p6199s1_4+p6199s1_5+p6199s1_6+p6199s1_7+p6199s1_8+p6199s1_9+p6199s1_10+  # 552.016
  p6199s1_12+p6199s1_14+p6199s1_15+p6199s1_20+p6199s1_21+p6199s1_23+p6199s1_25+p6199s1_30 
##################################################################################################################################################################
# 27. ¿fue remitido(a) a especialista?
p6145_ind <- data.frame(svytable(~fex_c + p6145,
                                 design = BD_survey))
# Si
p6145_1 <- p6145_ind %>% filter(p6145_ind$p6145 == 1)  # 435.938
p6145_1 <- sum(p6145_1$Freq)
# No
p6145_2 <- p6145_ind %>% filter(p6145_ind$p6145 == 2)  # 678.550
p6145_2 <- sum(p6145_2$Freq)

# total p6145
p6145_total <- p6145_1+p6145_2  # 1.114.488
##################################################################################################################################################################
# 28. En general, considera que la calidad de la prestación del servicio de salud (medicina general, medicina especializada, odontología, etc.) fue:
p8554_ind <- data.frame(svytable(~fex_c + p8554,
                                 design = BD_survey))
# Muy buena
p8554_1 <- p8554_ind %>% filter(p8554_ind$p8554 == 1)  # 239.533
p8554_1 <- sum(p8554_1$Freq)
# Buena
p8554_2 <- p8554_ind %>% filter(p8554_ind$p8554 == 2)  # 899.439
p8554_2 <- sum(p8554_2$Freq)
# Mala
p8554_3 <- p8554_ind %>% filter(p8554_ind$p8554 == 3)  # 125.048
p8554_3 <- sum(p8554_3$Freq)
# Muy mala
p8554_4 <- p8554_ind %>% filter(p8554_ind$p8554 == 4)  # 25.966
p8554_4 <- sum(p8554_4$Freq)

# total p8554
p8554_total <- p8554_1+p8554_2+p8554_3+p8554_4  # 1.289.988
###################################################################################################################################################################
# 29. ¿Cuál es el aspecto que más influyó en su percepción sobre la calidad de la prestación del servicio?
p801_ind <- data.frame(svytable(~fex_c + p801,
                                design = BD_survey))
# Trámites excesivos o dispendiosos
p801_1 <- p801_ind %>% filter(p801_ind$p801 == 1)  # 18.371
p801_1 <- sum(p801_1$Freq)
# Mala atención del personal administrativo o asistencial (médicos, enfermeras, etc.)
p801_2 <- p801_ind %>% filter(p801_ind$p801 == 2)  # 44.532
p801_2 <- sum(p801_2$Freq)
# Falta de capacidad, conocimientos o habilidad del personal asistencial
p801_3 <- p801_ind %>% filter(p801_ind$p801 == 3)  # 11.337
p801_3 <- sum(p801_3$Freq)
# Condiciones deficientes de infraestructura, dotación, mobiliario o accesibilidad para población en condición de discapacidad
p801_4 <- p801_ind %>% filter(p801_ind$p801 == 4)  # 1.678
p801_4 <- sum(p801_4$Freq)
# Demora en la asignación de citas
p801_5 <- p801_ind %>% filter(p801_ind$p801 == 5)  # 27.835
p801_5 <- sum(p801_5$Freq)
# Demora en la atención por parte del personal médico
p801_6 <- p801_ind %>% filter(p801_ind$p801 == 6)  # 36.312
p801_6 <- sum(p801_6$Freq)
# Problemas relacionados con los medicamentos
p801_7 <- p801_ind %>% filter(p801_ind$p801 == 7)  # 6.765
p801_7 <- sum(p801_7$Freq)
# Otro
p801_8 <- p801_ind %>% filter(p801_ind$p801 == 8)  # 4.181
p801_8 <- sum(p801_8$Freq)

# total p801
p801_total <- p801_1+p801_2+p801_3+p801_4+p801_5+p801_6+p801_7+p801_8  # 151.014
##########################################################################################################################################################################
# 30. ¿Cuáles de las siguientes fuentes utilizó ... para cubrir los costos de atención en salud en los últimos 30 días? (incluya consulta médica , exámenes y medicamentos)
# p8556 

# 30.1 EPS o entidad de seguridad social en salud  en la cual está afiliado/a
p8556s1_ind <- data.frame(svytable(~fex_c + p8556s1,
                                   design = BD_survey))
p8556s1_1 <- p8556s1_ind %>% filter(p8556s1_ind$p8556s1 == 1) # 992.892
p8556s1_1 <- sum(p8556s1_1$Freq)
# 30.2 Plan o seguro voluntario (Seguro médico, plan complementario o medicina prepagada)
p8556s2_ind <- data.frame(svytable(~fex_c + p8556s2,
                                   design = BD_survey))
p8556s2_2 <- p8556s2_ind %>% filter(p8556s2_ind$p8556s2 == 1) # 88.170
p8556s2_2 <- sum(p8556s2_2$Freq)
# 30.3 Seguro obligatorio de accidentes de tránsito (SOAT)
p8556s4_ind <- data.frame(svytable(~fex_c + p8556s4,
                                   design = BD_survey))
p8556s4_3 <- p8556s4_ind %>% filter(p8556s4_ind$p8556s4 == 1) # 18.364
p8556s4_3 <- sum(p8556s4_3$Freq)
# 30.4 Secretaria de salud o la alcaldía
p8556s5_ind <- data.frame(svytable(~fex_c + p8556s5,
                                   design = BD_survey))
p8556s5_4 <- p8556s5_ind %>% filter(p8556s5_ind$p8556s5 == 1) # 6.195
p8556s5_4 <- sum(p8556s5_4$Freq)
# 30.5 Recursos propios o familiares
p8556s6_ind <- data.frame(svytable(~fex_c + p8556s6,
                                   design = BD_survey))
p8556s6_5 <- p8556s6_ind %>% filter(p8556s6_ind$p8556s6 == 1) # 218.488
p8556s6_5 <- sum(p8556s6_5$Freq)
# 30.6 Recursos de otras personas 
p8556s9_ind <- data.frame(svytable(~fex_c + p8556s9,
                                   design = BD_survey))
p8556s9_6 <- p8556s9_ind %>% filter(p8556s9_ind$p8556s9 == 1) # 13.270
p8556s9_6 <- sum(p8556s9_6$Freq)
# 30.7 No se requirio pago
p8556s10_ind <- data.frame(svytable(~fex_c + p8556s10,
                                   design = BD_survey))
p8556s10_7 <- p8556s10_ind %>% filter(p8556s10_ind$p8556s10 == 1) # 33.040
p8556s10_7 <- sum(p8556s10_7$Freq)

# total p8556
p8556_total <- p8556s1_1 + p8556s2_2 + p8556s4_3 + p8556s5_4 + p8556s6_5 + p8556s9_6 + p8556s10_7
#######################################################################################################################################################
# 31. Por esta enfermedad , ¿a ... le formularon medicamentos?
p6147_ind <- data.frame(svytable(~fex_c + p6147,
                                 design = BD_survey))
# Si
p6147_1 <- p6147_ind %>% filter(p6147_ind$p6147 == 1)  # 1.062.164
p6147_1 <- sum(p6147_1$Freq)
# No
p6147_2 <- p6147_ind %>% filter(p6147_ind$p6147 == 2)  # 227.824
p6147_2 <- sum(p6147_2$Freq)

# total 6147
p6147_total <- p6147_1+p6147_2  # 1.289.988
#########################################################################################################################################################
# 32. ¿Estos medicamentos o remedios le fueron entregados a ... por cuenta de la institución a la cual está afiliado(a)?
p6148_ind <- data.frame(svytable(~fex_c + p6148,
                                 design = BD_survey))
# Si, todos
p6148_1 <- p6148_ind %>% filter(p6148_ind$p6148 == 1)  # 654.081
p6148_1 <- sum(p6148_1$Freq)
# Si, algunos
p6148_2 <- p6148_ind %>% filter(p6148_ind$p6148 == 2)  # 150.248
p6148_2 <- sum(p6148_2$Freq)
# No
p6148_3 <- p6148_ind %>% filter(p6148_ind$p6148 == 3)  # 257.834
p6148_3 <- sum(p6148_3$Freq)
# total 6147
p6148_total <- p6148_1+p6148_2+p6148_3  # 1.062.164
###########################################################################################################################################################
# 33. ¿Por qué razón no le fueron entregados los medicamentos  (todos o algunos)?
p6149_ind <- data.frame(svytable(~fex_c + p6149,
                                 design = BD_survey))
# No están incluidos en el plan de beneficios en salud o POS o no le autorizaron
p6149_1 <- p6149_ind %>% filter(p6149_ind$p6149 == 1)  # 145.918
p6149_1 <- sum(p6149_1$Freq)
# No había los medicamentos recetados
p6149_2 <- p6149_ind %>% filter(p6149_ind$p6149 == 2)  # 65.402
p6149_2 <- sum(p6149_2$Freq)
# No había la cantidad requerida
p6149_3 <- p6149_ind %>% filter(p6149_ind$p6149 == 3)  # 16.343
p6149_3 <- sum(p6149_3$Freq)
# Por errores o deficiencias en la expedición de la fórmula medica
p6149_4 <- p6149_ind %>% filter(p6149_ind$p6149 == 4)  # 4.175
p6149_4 <- sum(p6149_4$Freq)
# No hizo las gestiones para reclamarlos
p6149_5 <- p6149_ind %>% filter(p6149_ind$p6149 == 5)  # 62.811
p6149_5 <- sum(p6149_5$Freq)
# No tenia dinero
p6149_6 <- p6149_ind %>% filter(p6149_ind$p6149 == 6)  # 3.120
p6149_6 <- sum(p6149_6$Freq)
# Acudió a medico particular
p6149_7 <- p6149_ind %>% filter(p6149_ind$p6149 == 7)  # 99.120
p6149_7 <- sum(p6149_7$Freq)
# Otra
p6149_8 <- p6149_ind %>% filter(p6149_ind$p6149 == 8)  # 11.190
p6149_8 <- sum(p6149_8$Freq)

# total p6149
p6149_total <- p6149_1+p6149_2+p6149_3+p6149_4+p6149_5+p6149_6+p6149_7+p6149_8  # 408.083
############################################################################################################################################################
# 34.  Durante los últimos 30 días ....realizó pagos por:  (No incluya gastos reportados en hospitalización)
# 34.1 Consulta medica general o con especialista ?
p3178_ind <- data.frame(svytable(~fex_c + p3178,
                                 design = BD_survey))
# Si
p3178_1 <- p3178_ind %>% filter(p3178_ind$p3178 == 1)  # 3.473.363
p3178_1 <- sum(p3178_1$Freq)
# No
p3178_2 <- p3178_ind %>% filter(p3178_ind$p3178 == 2)  # 47.750.696
p3178_2 <- sum(p3178_2$Freq)

# total p3178
p3178_total <- p3178_1+p3178_2  # 51.224.060
#############################################################################################################################################################
# 34.1.1 A traves de EPS
p3178s1_ind <- data.frame(svytable(~fex_c + p3178s1,
                                   design = BD_survey))
p3178s1 <- p3178s1_ind %>% filter(p3178s1_ind$p3178s1 == 1)  # 2.657.242
p3178s1 <- sum(p3178s1$Freq)
# 34.1.1.A1 Valor a traves de EPS
p3178s1a1 <- table(BD_ecv_ind$p3178s1a1)
##############################################################################################################################################################
# 34.1.2 Medico particular
p3178s2_ind <- data.frame(svytable(~fex_c + p3178s2,
                                   design = BD_survey))
p3178s2 <- p3178s2_ind %>% filter(p3178s2_ind$p3178s2 == 1)  # 480.700
p3178s2 <- sum(p3178s2$Freq)
# 34.1.2.A1 Valor Médico particular
p3178s2a1 <- table(BD_ecv_ind$p3178s2a1)
##############################################################################################################################################################
# 34.1.3 Plan voluntario (seguro médico, plan complementario o medicina prepagada)
p3178s3_ind <- data.frame(svytable(~fex_c + p3178s3,
                                   design = BD_survey))
p3178s3 <- p3178s3_ind %>% filter(p3178s3_ind$p3178s3 == 1)  # 364.612
p3178s3 <- sum(p3178s3$Freq)
# 34.1.2 A1 Valor Plan voluntario (seguro médico, plan complementario o medicina prepagada) 
p3178s3a1 <- table(BD_ecv_ind$p3178s3a1)
#############################################################################################################################################################
# 34.2 Consulta o tratamiento odontologico?
p3179_ind <- data.frame(svytable(~fex_c + p3179,
                                 design = BD_survey))
# Si
p3179_1 <- p3179_ind %>% filter(p3179_ind$p3179 == 1)  # 1.231.712
p3179_1 <- sum(p3179_1$Freq)
# No
p3179_2 <- p3179_ind %>% filter(p3179_ind$p3179 == 2)  # 49.992.347
p3179_2 <- sum(p3179_2$Freq)

# total p3179
p3179_total <- p3179_1+p3179_2  # 51.224.060
##############################################################################################################################################################
# 34.2.1 A traves de EPS
p3179s1_ind <- data.frame(svytable(~fex_c + p3179s1,
                                   design = BD_survey))
p3179s1 <- p3179s1_ind %>% filter(p3179s1_ind$p3179s1 == 1)  # 524.796
p3179s1 <- sum(p3179s1$Freq)
# 34.2.1.A1 Valor a traves de EPS
p3179s1a1 <- table(BD_ecv_ind$p3179s1a1)
##############################################################################################################################################################
# 34.2.2 Odontologo particular
p3179s2_ind <- data.frame(svytable(~fex_c + p3179s2,
                                   design = BD_survey))
p3179s2 <- p3179s2_ind %>% filter(p3179s2_ind$p3179s2 == 1)  # 629.571
p3179s2 <- sum(p3179s2$Freq)
# 34.2.2.A1 Valor odontologo particular
p3179s2a1 <- table(BD_ecv_ind$p3179s2a1)
###############################################################################################################################################################
# 34.2.3 Plan voluntario (seguro médico, plan complementario o medicina prepagada)
p3179s3_ind <- data.frame(svytable(~fex_c + p3179s3,
                                   design = BD_survey))
p3179s3 <- p3179s3_ind %>% filter(p3179s3_ind$p3179s3 == 1)  # 82.093
p3179s3 <- sum(p3179s3$Freq)
# 34.2.3 A1 Valor Plan voluntario (seguro médico, plan complementario o medicina prepagada) 
p3179s3a1 <- table(BD_ecv_ind$p3179s3a1)
###############################################################################################################################################################
# 34.3 Vacunas
p3181_ind <- data.frame(svytable(~fex_c + p3181, 
                                 design = BD_survey))
# Si
p3181_1 <- p3181_ind %>% filter(p3181_ind$p3181 == 1)  # 330.701
p3181_1 <- sum(p3181_1$Freq)

# No
p3181_2 <- p3181_ind %>% filter(p3181_ind$p3181 == 2) # 50.893.358
p3181_2 <- sum(p3181_2$Freq)

# total p3181 
p3181_total <- p3181_1+p3181_2  # 51.224.060
# 34.3.1 Valor vacuna
p3181s1_ind <- data.frame(svytable(~fex_c + p3181s1,
                                   design = BD_survey))
p3181s1 <- table(BD_ecv_ind$p3181s1)
#################################################################################################################################################################
# 34.4 Formulas medicas o compra de medicamentos consumidos ocasional o regularmente
p3182_ind <- data.frame(svytable(~fex_c + p3182,
                                 design = BD_survey))
# Si
p3182_1 <- p3182_ind %>% filter(p3182_ind$p3182 == 1) # 3.351.729
p3182_1 <- sum(p3182_1$Freq)
# No
p3182_2 <- p3182_ind %>% filter(p3182_ind$p3182 == 2)  # 47.872.330
p3182_2 <- sum(p3182_2$Freq)

# total p3182
p3182_total <- p3182_1+p3182_2   # 51.224.060

# 34.4.1 Valor
p3182s1_ind <- data.frame(svytable(~fex_c + p3182s1_ind,
                                   design = BD_survey))
p3182s1 <- table(BD_ecv_ind$p3182s1)
####################################################################################################################################################################
# 34.5 Laboratorio clinico, RX, examenes de diagnostico?
p3183_ind <- data.frame(svytable(~fex_c + p3183,
                                 design = BD_survey))
# Si
p3183_1 <- p3183_ind %>% filter(p3183_ind$p3183 == 1)  # 1.041.283
p3183_1 <- sum(p3183_1$Freq)
# No
p3183_2 <- p3183_ind %>% filter(p3183_ind$p3183 == 2)  # 50.182.776
p3183_2 <- sum(p3183_2$Freq)

# total p3183
p3183_total <- p3183_1+p3183_2  # 51.224.060

# 34.5.1 Valor
p3183s1_ind <- data.frame(svytable(~fex_c + p3183s1_ind,
                                   design = BD_survey))
p3183s1 <- table(BD_ecv_ind$p3183s1)
#######################################################################################################################################################################
# 34.6 Rehabilitacion o terapias medicas ?
p3184_ind <- data.frame(svytable(~fex_c + p3184,
                                 design = BD_survey))
# Si
p3184_1 <- p3184_ind %>% filter(p3184_ind$p3184 == 1)  # 229.624
p3184_1 <- sum(p3184_1$Freq)
# No
p3184_2 <- p3184_ind %>% filter(p3184_ind$p3184 == 2)  # 50.994.435
p3184_2 <- sum(p3184_2$Freq)

# total p3184
p3184_total <- p3184_1+p3184_2  # 51.224.060

# 34.6.1 Valor
p3184s1_ind <- data.frame(svytable(~fex_c + p3184s1,
                                   design = BD_survey))
p3184s1 <- table(BD_ecv_ind$p3184s1)
########################################################################################################################################################################
# 34.7 Terapias alternativas ? (homeopatia, acupuntura, esencias florales, musicoterapia)
p3185_ind <- data.frame(svytable(~fex_c + p3185,
                                 design = BD_survey))
# Si
p3185_1 <- p3185_ind %>% filter(p3185_ind$p3185 == 1)  # 75.960
p3185_1 <- sum(p3185_1$Freq)
# No
p3185_2 <- p3185_ind %>% filter(p3185_ind$p3185 == 2)  # 51.148.099
p3185_2 <- sum(p3185_2$Freq)

# total p3185
p3185_total <- p3185_1+p3185_2  # 51.224.060

# 34.7.1 Valor
p3185s1_ind <- data.frame(svytable(~fex_c + p3185s1,
                                   design = BD_survey))
p3185s1 <- table(BD_ecv_ind$p3185s1)
##########################################################################################################################################################################
# 34.8 Transporte para ir al sitio de atencion medica y regresar
p3186_ind <- data.frame(svytable(~fex_c + p3186,
                                 design = BD_survey))
# Si
p3186_1 <- p3186_ind %>% filter(p3186_ind$p3186 == 1)  # 2.989.198
p3186_1 <- sum(p3186_1$Freq)
# No
p3186_2 <- p3186_ind %>% filter(p3186_ind$p3186 == 2)  # 48.234.861
p3186_2 <- sum(p3186_2$Freq)

# total p3186
p3186_total <- p3186_1+p3186_2  # 51.224.060

# 34.7.1 Valor
p3186s1_ind <- data.frame(svytable(~fex_c + p3186s1,
                                   design = BD_survey))
p3186s1 <- table(BD_ecv_ind$p3186s1)
###########################################################################################################################################################################
# 35. Durante los ÚLTIMOS DOCE MESES ¿Realizó pagos por: 
# 35.1  Lentes, audífonos o aparatos ortopédicos (muletas, sillas de ruedas, elementos para terapias, etc.)
p3187s1_ind <- data.frame(svytable(~fex_c + p3187s1,
                                 design = BD_survey))
# Si
p3187s1_1 <- p3187s1_ind %>% filter(p3187s1_ind$p3187s1 == 1)  # 1.974.715
p3187s1_1 <- sum(p3187s1_1$Freq)
# No
p3187s1_2 <- p3187s1_ind %>% filter(p3187s1_ind$p3187s1 == 2)  # 49.249.344
p3187s1_2 <- sum(p3187s1_2$Freq)

# total p3187s1
p3187s1_total <- p3187s1_1+p3187s1_2  # 51.224.060

# 35.2. Valor Lentes, audífonos o aparatos ortopédicos (muletas, sillas de ruedas, elementos para terapias, etc.)
p3187s2_ind <- data.frame(svytable(~fex_c + p3187s2,
                                   design = BD_survey))
p3187s2 <- table(BD_ecv_ind$p3187s2)
############################################################################################################################################################################
# 35.2 Cirugías o procedimientos ambulatorios? 
p3188_ind <- data.frame(svytable(~fex_c + p3188,
                                   design = BD_survey))
# Si
p3188_1 <- p3188_ind %>% filter(p3188_ind$p3188 == 1)  # 438.655
p3188_1 <- sum(p3188_1$Freq)
# No
p3188_2 <- p3188_ind %>% filter(p3188_ind$p3188 == 2)  # 50.785.404
p3188_2 <- sum(p3188_2$Freq)

# total p3188
p3188_total <- p3188_1+p3188_2  # 51.224.060
#############################################################################################################################################################################
# 35.2.1 A traves de EPS
p3188s1_ind <- data.frame(svytable(~fex_c + p3188s1,
                                   design = BD_survey))
p3188s1 <- p3188s1_ind %>% filter(p3188s1_ind$p3188s1 == 1)   # 308.589
p3188s1 <- sum(p3188s1$Freq)

# 35.2.1 A1 Valor a traves de EPS
p3188s1a1_ind <- data.frame(svytable(~fex_c + p3188s1a1,
                                     design = BD_survey))
p3188s1a1 <- table(BD_ecv_ind$p3188s1a1)
#############################################################################################################################################################################
# 35.2.2 Medico particular
p3188s2_ind <- data.frame(svytable(~fex_c + p3188s2,
                                   design = BD_survey))
p3188s2 <- p3188s2_ind %>% filter(p3188s2_ind$p3188s2 == 1)   # 92.039
p3188s2 <- sum(p3188s2$Freq)

# 35.2.2 A1 Valor medico particular
p3188s2a1_ind <- data.frame(svytable(~fex_c + p3188s2a1,
                                     design = BD_survey))
p3188s2a1 <- table(BD_ecv_ind$p3188s2a1)
###############################################################################################################################################################################
# 35.2.3 Plan voluntario (seguro médico, plan complementario o medicina prepagada) 
p3188s3_ind <- data.frame(svytable(~fex_c + p3188s3,
                                   design = BD_survey))
p3188s3 <- p3188s3_ind %>% filter(p3188s3_ind$p3188s3 == 1)   # 46.433
p3188s3 <- sum(p3188s3$Freq)

# 35.2.3 A1  Valor Valor Plan voluntario (seguro médico, plan complementario o medicina prepagada) 
p3188s3a1_ind <- data.frame(svytable(~fex_c + p3188s3a1,
                                     design = BD_survey))
p3188s3a1 <- table(BD_ecv_ind$p3188s3a1)
################################################################################################################################################################################
# 36. ¿Actualmente _____ fuma (cigarrillo, tabaco, vapeador o cigarrillo electrónico)?
# 36.1 Cigarrillo, tabaco:
p3008s1_ind <- data.frame(svytable(~fex_c + p3008s1,
                                   design = BD_survey))
# Si 
p3008s1_1 <- p3008s1_ind %>% filter(p3008s1_ind$p3008s1 == 1)  # 2.383.536
p3008s1_1 <- sum(p3008s1_1$Freq)
# No
p3008s1_2 <- p3008s1_ind %>% filter(p3008s1_ind$p3008s1 == 2)  # 41.049.011
p3008s1_2 <- sum(p3008s1_2$Freq)

# total p3008s1
p3008s1_total <- p3008s1_1+p3008s1_2  # 43.432.547
#####################################################################################################################################################################################
# 36.2 Frecuencia cigarrillo, tabaco:
p3008s1a1_ind <- data.frame(svytable(~fex_c + p3008s1a1,
                                   design = BD_survey))
# Diariamente 
p3008s1a1_1 <- p3008s1a1_ind %>% filter(p3008s1a1_ind$p3008s1a1 == 1)  # 1.442.725
p3008s1a1_1 <- sum(p3008s1a1_1$Freq)
# Algunos dias a la semana
p3008s1a1_2 <- p3008s1a1_ind %>% filter(p3008s1a1_ind$p3008s1a1 == 2)  # 651.584
p3008s1a1_2 <- sum(p3008s1a1_2$Freq)
# Menos de una vez por semana
p3008s1a1_3 <- p3008s1a1_ind %>% filter(p3008s1a1_ind$p3008s1a1 == 3)  # 289.226
p3008s1a1_3 <- sum(p3008s1a1_3$Freq)
# total p3008s1a1
p3008s1a1_total <- p3008s1a1_1+p3008s1a1_2+p3008s1a1_3  # 2.383.536
#################################################################################################################################################################################
# 36.3 ¿Cuantos cigarrillos al dia?
p3008s1a2_ind <- data.frame(svytable(~fex_c + p3008s1a2,
                                     design = BD_survey))
p3008s1a2 <- table(BD_ecv_ind$p3008s1a2)
###################################################################################################################################################################################
# 36.4 Vapeador o cigarrillo electronico
p3008s2_ind <- data.frame(svytable(~fex_c + p3008s2,
                                   design = BD_survey))
# Si 
p3008s2_1 <- p3008s2_ind %>% filter(p3008s2_ind$p3008s2 == 1)  # 87.578
p3008s2_1 <- sum(p3008s2_1$Freq)
# No
p3008s2_2 <- p3008s2_ind %>% filter(p3008s2_ind$p3008s2 == 2)  # 43.344.968
p3008s2_2 <- sum(p3008s2_2$Freq)

# total p3008s1
p3008s2_total <- p3008s2_1+p3008s2_2  # 43.432.547
###################################################################################################################################################################################
# 37. ¿ ......  consume bebidas azucaradas (gaseosas, refrescos, bebidas de  jugos de frutas procesadas, té endulzado, refrescos en polvo)?
p1707_ind <- data.frame(svytable(~fex_c + p1707,
                                 design = BD_survey))
# Si
p1707_1 <- p1707_ind %>% filter(p1707_ind$p1707 == 1)  # 30.787.997
p1707_1 <- sum(p1707_1$Freq)
# No
p1707_2 <- p1707_ind %>% filter(p1707_ind$p1707 == 2)  # 19.067.852
p1707_2 <- sum(p1707_2$Freq)

# total p1707
p1707_total <- p1707_1+p1707_2  # 49.855.850
#####################################################################################################################################################################################
# 37.1 Con que frecuencia consume las bebidas  azucaradas:
p1707s1_ind <- data.frame(svytable(~fex_c + p1707s1,
                                 design = BD_survey))
# Todos los dias de la semana (dos o mas veces al dia)
p1707s1_1 <- p1707s1_ind %>% filter(p1707s1_ind$p1707s1 == 1)  # 2.505.255
p1707s1_1 <- sum(p1707s1_1$Freq)
# Todos los dias de la semana (una vez al dia)
p1707s1_2 <- p1707s1_ind %>% filter(p1707s1_ind$p1707s1 == 2)  # 3.189.128
p1707s1_2 <- sum(p1707s1_2$Freq)
# Cuatro a seis veces a la semana
p1707s1_3 <- p1707s1_ind %>% filter(p1707s1_ind$p1707s1 == 3)  # 2.628.549
p1707s1_3 <- sum(p1707s1_3$Freq)
# Dos o tres veces a la semana
p1707s1_4 <- p1707s1_ind %>% filter(p1707s1_ind$p1707s1 == 4)  # 9.686.285
p1707s1_4 <- sum(p1707s1_4$Freq)
# Una vez a la semana
p1707s1_5 <- p1707s1_ind %>% filter(p1707s1_ind$p1707s1 == 5)  # 8.220.918
p1707s1_5 <- sum(p1707s1_5$Freq)
# Menos de una vez por semana
p1707s1_6 <- p1707s1_ind %>% filter(p1707s1_ind$p1707s1 == 6)  # 4.557.860
p1707s1_6 <- sum(p1707s1_6$Freq)

# total p1707s1
p1707s1_total <- p1707s1_1+p1707s1_2+p1707s1_3+p1707s1_4+p1707s1_5+p1707s1_6  # 30.787.997
########################################################################################################################################################################
# 38. ¿ ____  consume alimentos de paquete (papas, plátanos, chitos, paquete mixto, rosquitas, chicharrones o similares)?
p3003_ind <- data.frame(svytable(~fex_c + p3003,
                                 design = BD_survey))
# Si
p3003_1 <- p3003_ind %>% filter(p3003_ind$p3003 == 1)  # 24.581.997
p3003_1 <- sum(p3003_1$Freq)
# No
p3003_2 <- p3003_ind %>% filter(p3003_ind$p3003 == 2)  # 25.273.852
p3003_2 <- sum(p3003_2$Freq)

# total p3003
p3003_total <- p3003_1+p3003_2  # 49.855.850
#########################################################################################################################################################################
# 38.1 Con que frecuencia consume alimentos de paquete
p3003s1_ind <- data.frame(svytable(~fex_c + p3003s1,
                                   design = BD_survey))
# Todos los dias de la semana (dos o mas veces al dia)
p3003s1_1 <- p3003s1_ind %>% filter(p3003s1_ind$p3003s1 == 1)  # 939.190
p3003s1_1 <- sum(p3003s1_1$Freq)
# Todos los dias de la semana (una vez al dia)
p3003s1_2 <- p3003s1_ind %>% filter(p3003s1_ind$p3003s1 == 2)  # 1.841.252
p3003s1_2 <- sum(p3003s1_2$Freq)
# Cuatro a seis veces a la semana
p3003s1_3 <- p3003s1_ind %>% filter(p3003s1_ind$p3003s1 == 3)  # 1.965.457
p3003s1_3 <- sum(p3003s1_3$Freq)
# Dos o tres veces a la semana
p3003s1_4 <- p3003s1_ind %>% filter(p3003s1_ind$p3003s1 == 4)  # 7.480.343
p3003s1_4 <- sum(p3003s1_4$Freq)
# Una vez a la semana
p3003s1_5 <- p3003s1_ind %>% filter(p3003s1_ind$p3003s1 == 5)  # 7.481.018
p3003s1_5 <- sum(p3003s1_5$Freq)
# Menos de una vez por semana
p3003s1_6 <- p3003s1_ind %>% filter(p3003s1_ind$p3003s1 == 6)  # 4.874.735
p3003s1_6 <- sum(p3003s1_6$Freq)

# total p3003s1
p3003s1_total <- p3003s1_1+p3003s1_2+p3003s1_3+p3003s1_4+p3003s1_5+p3003s1_6  # 24.581.997
##################################################################################################################################################################################
# 39. ¿Durante los últimos 12 meses ____ tuvo que ser hospitalizado/a?
p6133_ind <- data.frame(svytable(~fex_c + p6133,
                                 design = BD_survey))
# Si
p6133_1 <- p6133_ind %>% filter(p6133_ind$p6133 == 1)  # 1.902.902
p6133_1 <- sum(p6133_1$Freq)
# No
p6133_2 <- p6133_ind %>% filter(p6133_ind$p6133 == 2)  # 49.321.157
p6133_2 <- sum(p6133_2$Freq)

# total p6133
p6133_total <- p6133_1+p6133_2  # 51.224.060
####################################################################################################################################################################################
# 40. ¿Cuáles de las siguientes fuentes se utilizaron para cubrir los costos de esta hospitalización (incluya consulta médica, exámenes y medicamentos)?
# 40.1 EPS o entidad de seguridad social en la cual está afiliado/a 
p8560s1_ind <- data.frame(svytable(~fex_c + p8560s1,
                                   design = BD_survey))
# Si
p8560s1_1 <- p8560s1_ind %>% filter(p8560s1_ind$p8560s1 == 1)  # 1.687.713
p8560s1_1 <- sum(p8560s1_1$Freq)
# No
p8560s1_2 <- p8560s1_ind %>% filter(p8560s1_ind$p8560s1 == 2)  # 215.188
p8560s1_2 <- sum(p8560s1_2$Freq)

# total p8560s1
p8560s1_total <- p8560s1_1+p8560s1_2  # 1.902.902
###############################################################################################################################################################################################
# 40.2 Plan o seguro voluntario (seguro médico, plan complementario o medicina prepagada)
p8560s2_ind <- data.frame(svytable(~fex_c + p8560s2,
                                   design = BD_survey))
# Si
p8560s2_1 <- p8560s2_ind %>% filter(p8560s2_ind$p8560s2 == 1)  # 100.026
p8560s2_1 <- sum(p8560s2_1$Freq)
# No
p8560s2_2 <- p8560s2_ind %>% filter(p8560s2_ind$p8560s2 == 2)  # 1.802.875
p8560s2_2 <- sum(p8560s2_2$Freq)

# total p8560s2
p8560s2_total <- p8560s2_1+p8560s2_2  # 1.902.902
##################################################################################################################################################################################
# 40.3 Seguro obligatorio de accidentes de tránsito (SOAT)
p8560s3_ind <- data.frame(svytable(~fex_c + p8560s3,
                                   design = BD_survey))
# Si
p8560s3_1 <- p8560s3_ind %>% filter(p8560s3_ind$p8560s3 == 1)  # 53.984
p8560s3_1 <- sum(p8560s3_1$Freq)
# No
p8560s3_2 <- p8560s3_ind %>% filter(p8560s3_ind$p8560s3 == 2)  # 1.848.917
p8560s3_2 <- sum(p8560s3_2$Freq)

# total p8560s3
p8560s3_total <- p8560s3_1+p8560s3_2  # 1.902.902
#######################################################################################################################################################################################
# 40.4 Secretaria de salud o la alcaldía
p8560s4_ind <- data.frame(svytable(~fex_c + p8560s4,
                                   design = BD_survey))
# Si
p8560s4_1 <- p8560s4_ind %>% filter(p8560s4_ind$p8560s4 == 1)  # 32.243
p8560s4_1 <- sum(p8560s4_1$Freq)
# No
p8560s4_2 <- p8560s4_ind %>% filter(p8560s4_ind$p8560s4 == 2)  # 1.870.658
p8560s4_2 <- sum(p8560s4_2$Freq)

# total p8560s4
p8560s4_total <- p8560s4_1+p8560s4_2  # 1.902.902
########################################################################################################################################################################################
# 40.5 Recursos propios o familiares
p8560s5_ind <- data.frame(svytable(~fex_c + p8560s5,
                                   design = BD_survey))
# Si
p8560s5_1 <- p8560s5_ind %>% filter(p8560s5_ind$p8560s5 == 1)  # 125.886
p8560s5_1 <- sum(p8560s5_1$Freq)
# No
p8560s5_2 <- p8560s5_ind %>% filter(p8560s5_ind$p8560s5 == 2)  # 1.777.015
p8560s5_2 <- sum(p8560s5_2$Freq)

# total p8560s5
p8560s5_total <- p8560s5_1+p8560s5_2  # 1.902.902
##########################################################################################################################################################################################
# 41. ¿Cuánto pagó en total ____ por esta hospitalización?
# 41.1 A traves de EPS
p3189s1_ind <- data.frame(svytable(~fex_c + p3189s1,
                                   design = BD_survey))
p3189s1 <- p3189s1_ind %>% filter(p3189s1_ind$p3189s1 == 1)   # 1.762.048
p3189s1 <- sum(p3189s1$Freq)
# 41.1 A1 Valor a traves de EPS
p3189s1a1_ind <- data.frame(svytable(~fex_c + p3189s1a1,
                                     design = BD_survey))
p3189s1a1 <- table(BD_ecv_ind$p3189s1a1)

###########################################################################################################################################################################################
# 41.2 Servicio particular o plan voluntario (seguro médico, plan complementario o medicina prepagada)
p3189s2_ind <- data.frame(svytable(~fex_c + p3189s2,
                                   design = BD_survey))
p3189s2 <- p3189s2_ind %>% filter(p3189s2_ind$p3189s2 == 1)   # 155.134
p3189s2 <- sum(p3189s2$Freq)

# 45.2.1 A1 Valor servicio particular o plan voluntario
p3189s2a1_ind <- data.frame(svytable(~fex_c + p3189s2a1,
                                     design = BD_survey))
p3189s2a1 <- table(BD_ecv_ind$p3189s2a1)
#############################################################################################################################################################################################
# 42. Considera que la calidad del servicio en esta hospitalizacion fue:
p8561_ind <- data.frame(svytable(~fex_c + p8561,
                                 design = BD_survey))
# Muy buena
p8561_1 <- p8561_ind %>% filter(p8561_ind$p8561 == 1)  # 474.216
p8561_1 <- sum(p8561_1$Freq)
# Buena
p8561_2 <- p8561_ind %>% filter(p8561_ind$p8561 == 2)  # 1.284.721
p8561_2 <- sum(p8561_2$Freq)
# Mala
p8561_3 <- p8561_ind %>% filter(p8561_ind$p8561 == 3)  # 123.312
p8561_3 <- sum(p8561_3$Freq)
# Muy mala
p8561_4 <- p8561_ind %>% filter(p8561_ind$p8561 == 4)  # 20.651
p8561_4 <- sum(p8561_4$Freq)

# total p8561 
p8561_total <- p8561_1+p8561_2+p8561_3+p8561_4  # 1.902.902
#################################################################################################################################################################################################
# 43. ¿... ha estado embarazada?
p3335_ind <- data.frame(svytable(~fex_c + p3335,
                                 design = BD_survey))
# Si
p3335_1 <- p3335_ind %>% filter(p3335_ind$p3335 == 1)  # 14.942.200
p3335_1 <- sum(p3335_1$Freq)
# No
p3335_2 <- p3335_ind %>% filter(p3335_ind$p3335 == 2)  # 7.493.849
p3335_2 <- sum(p3335_2$Freq)

# total p3335
p3335_total <- p3335_1+p3335_2  # 22.436.050
##################################################################################################################################################################################################
# 43.a ¿Cuantos hijos nacidos vivos ha tenido?
p3335s1_ind <- data.frame(svytable(~fex_c + p3335s1,
                                   design = BD_survey))
p3335s1 <- table(BD_ecv_ind$p3335s1)
# 43.b ¿A que edad tuvo su primer hijo?
p3335s1a1_ind <- data.frame(svytable(~fex_c + p3335s1a1,
                                   design = BD_survey))
p3335s1a1 <- table(BD_ecv_ind$p3335s1a1)
####################################################################################################################################################################################################
# 44. ¿... está embarazada actualmente?
p8584_ind <- data.frame(svytable(~fex_c + p8584,
                                 design = BD_survey))
# Si
p8584_1 <- p8584_ind %>% filter(p8584_ind$p8584 == 1)  # 238.315
p8584_1 <- sum(p8584_1$Freq)
# No
p8584_2 <- p8584_ind %>% filter(p8584_ind$p8584 == 2)  # 8.662.504
p8584_2 <- sum(p8584_2$Freq)

# total p8584
p8584_total <- p8584_1+p8584_2   # 8.900.820
######################################################################################################################################################################################################
# 45. ¿Asiste a control prenatal?
p5694_ind <- data.frame(svytable(~fex_c + p5694,
                                 design = BD_survey))
# Si
p5694_1 <- p5694_ind %>% filter(p5694_ind$p5694 == 1)  # 217.904
p5694_1 <- sum(p5694_1$Freq)
# No
p5694_2 <- p5694_ind %>% filter(p5694_ind$p5694 == 2)  # 20.411
p5694_2 <- sum(p5694_2$Freq)

# total p5694
p5694_total <- p5694_1+p5694_2  # 238.315
####################################################################################################################################################################################################
# 46. ¿... tiene el esquema completo de vacunación, según su edad?
p5452_ind <- data.frame(svytable(~fex_c + p5452,
                                 design = BD_survey))
# Si 
p5452_1 <- p5452_ind %>% filter(p5452_ind$p5452 == 1)  # 4.059.601
p5452_1 <- sum(p5452_1$Freq)
# No
p5452_2 <- p5452_ind %>% filter(p5452_ind$p5452 == 2)  # 445.353
p5452_2 <- sum(p5452_2$Freq)

# p5452 total 
p5452_total <- p5452_1+p5452_2   # 4.504.955
#######################################################################################################################################################################################################
# 47. ¿Llevan a ... a control de crecimiento y desarrollo?
p6161_ind <- data.frame(svytable(~fex_c + p6161,
                                 design = BD_survey))
# Si 
p6161_1 <- p6161_ind %>% filter(p6161_ind$p6161 == 1)  # 3.851.943
p6161_1 <- sum(p6161_1$Freq)
# No
p6161_2 <- p6161_ind %>% filter(p6161_ind$p6161 == 2)  # 653.012
p6161_2 <- sum(p6161_2$Freq)

# p6161 total 
p6161_total <- p6161_1+p6161_2   # 4.504.955

# 47.1 Cuántas veces lo llevaron durante los ÚLTIMOS 12 MESES
p6161s1_ind <- data.frame(svytable(~fex_c + p6161s1,
                                   design = BD_survey))
p6161s1 <- table(BD_ecv_ind$p6161s1)
###########################################################################################################################################################################################################
# 48. ¿Cuál fue la principal razón para no llevar a ... a un control de crecimiento y desarrollo?
p1089_ind <- data.frame(svytable(~fex_c + p1089,
                                 design = BD_survey))
# No pensó que fuera necesario llevarlo/a a consulta
p1089_1 <- p1089_ind %>% filter(p1089_ind$p1089 == 1)  # 57.792
p1089_1 <- sum(p1089_1$Freq)
# La consulta es muy cara, no tiene plata
p1089_2 <- p1089_ind %>% filter(p1089_ind$p1089 == 2)  # 19.817
p1089_2 <- sum(p1089_2$Freq)
# El lugar donde lo atienden queda muy lejos / no hay servicio cerca
p1089_3 <- p1089_ind %>% filter(p1089_ind$p1089 == 3)  # 80.369
p1089_3 <- sum(p1089_3$Freq)
# No pudo dejar el trabajo/no tuvo tiempo
p1089_4 <- p1089_ind %>% filter(p1089_ind$p1089 == 4)  # 18.091
p1089_4 <- sum(p1089_4$Freq)
# No está afiliado/a a EPS o a régimen subsidiado
p1089_5 <- p1089_ind %>% filter(p1089_ind$p1089 == 5)  # 191.713
p1089_5 <- sum(p1089_5$Freq)
# No consiguió cita cercana en el tiempo o lo atienden muy mal
p1089_6 <- p1089_ind %>% filter(p1089_ind$p1089 == 6)  # 72.642
p1089_6 <- sum(p1089_6$Freq)
# Los trámites en la EPS/IPS son muy complicados
p1089_7 <- p1089_ind %>% filter(p1089_ind$p1089 == 7)  # 33.315
p1089_7 <- sum(p1089_7$Freq)
# Considera que no está en edad o es recién nacido/a
p1089_8 <- p1089_ind %>% filter(p1089_ind$p1089 == 8)  # 60.294
p1089_8 <- sum(p1089_8$Freq)
# No tiene registro civil de nacimiento
p1089_9 <- p1089_ind %>% filter(p1089_ind$p1089 == 9)  # 20.786
p1089_9 <- sum(p1089_9$Freq)
# Cambio de EPS o de municipio
p1089_10 <- p1089_ind %>% filter(p1089_ind$p1089 == 10)  # 24.358
p1089_10 <- sum(p1089_10$Freq)
# Otra
p1089_11 <- p1089_ind %>% filter(p1089_ind$p1089 == 11)  # 73.829
p1089_11 <- sum(p1089_11$Freq)

# total p1089
p1089_total <- p1089_1+p1089_2+p1089_3+p1089_4+p1089_5+p1089_6+p1089_7+p1089_8+p1089_9+p1089_10+p1089_11   # 653.012


#############################################################################################################################################
#############################################################################################################################################
########################################## NIVEL NACIONAL DESGLOSADO POR AREA ###############################################################
########################################## PARTE 2                            ###############################################################
#############################################################################################################################################
########################################## CABECERA                           ###############################################################
# 1. p6090 = ¿ ... está afiliado/a,  (cotizante o es beneficiario/a) a alguna entidad de seguridad social en salud? (Entidad promotora de salud [EPS] o entidad promotora de salud subsidiada [EPS-S] )
p6090_ind_cab <- data.frame(svytable(~fex_c + p6090 + clase,
                                 design = BD_survey))
# Sí 
p6090_1_cab <- p6090_ind_cab %>% filter(p6090_ind_cab$p6090 == 1 & p6090_ind_cab$clase == 1)  # 36.198.054
p6090_1_cab <- sum(p6090_1_cab$Freq)

# No
p6090_2_cab <- p6090_ind_cab %>% filter(p6090_ind_cab$p6090 == 2 & p6090_ind_cab$clase == 1)  # 2.685.076
p6090_2_cab <- sum(p6090_2_cab$Freq)                            

# No sabe / no informa
p6090_9_cab <- p6090_ind_cab %>% filter(p6090_ind_cab$p6090 == 9 & p6090_ind_cab$clase == 1)   # 185.474
p6090_9_cab <- sum(p6090_9_cab$Freq)

# Total p6090
p6090_total_cab <- p6090_1_cab + p6090_2_cab + p6090_9_cab   # 39.068.605
#######################################################################################################################################################################################################

# 2. p768 = ¿Por qué razón principal no está afiliado/a de una entidad de seguridad social en salud? (Entidad promotora de salud [EPS] o entidad promotora de salud subsidiada [EPS-S] 
p768_ind_cab <- data.frame(svytable(~fex_c + p768 + clase,
                                design = BD_survey)) 
# Por falta de dinero
p768_1_cab <- p768_ind_cab %>% filter(p768_ind_cab$p768 == 1 & p768_ind_cab$clase == 1 ) # 204.502
p768_1_cab <- sum(p768_1_cab$Freq)

# Muchos trámites
p768_2_cab <- p768_ind_cab %>% filter(p768_ind_cab$p768 == 2 & p768_ind_cab$clase == 1)  # 434.790
p768_2_cab <- sum(p768_2_cab$Freq)

# No le interesa o descuido
p768_3_cab <- p768_ind_cab %>% filter(p768_ind_cab$p768 == 3 & p768_ind_cab$clase == 1)  # 150.325
p768_3_cab <- sum(p768_3_cab$Freq)

# No sabe que debe afiliarse 
p768_4_cab <- p768_ind_cab %>% filter(p768_ind_cab$p768 == 4 & p768_ind_cab$clase == 1)  # 55.310
p768_4_cab <- sum(p768_4_cab$Freq)

# No esta vinculado/a laboralmente a una empresa o entidad (Usted o la persona de la que es beneificiario/a)
p768_5_cab <- p768_ind_cab %>% filter(p768_ind_cab$p768 == 5 & p768_ind_cab$clase == 1)  # 255.930
p768_5_cab <- sum(p768_5_cab$Freq)

# Esta en tramite de afiliacion
p768_6_cab <- p768_ind_cab %>% filter(p768_ind_cab$p768 == 6 & p768_ind_cab$clase == 1)  # 454.999
p768_6_cab <- sum(p768_6_cab$Freq)

# Problemas con el sisben (no lo han visitado/a, afiliado/a en otro municipio, lo/a desvincularon, le asignaron puntaje alto)
p768_7_cab <- p768_ind_cab %>% filter(p768_ind_cab$p768 == 7 & p768_ind_cab$clase == 1)  # 399.746
p768_7_cab <- sum(p768_7_cab$Freq)

# Otra razon
p768_8_cab <- p768_ind_cab %>% filter(p768_ind_cab$p768 == 8 & p768_ind_cab$clase == 1)  # 729.470
p768_8_cab <- sum(p768_8_cab$Freq)

# Total p768
p768_total_cab <- p768_1_cab + p768_2_cab + p768_3_cab + p768_4_cab + p768_5_cab + p768_6_cab + p768_7_cab + p768_8_cab  # 2.685.076
########################################################################################################################################################

# 3. ¿A cuál de los siguientes regímenes de seguridad social en salud está afiliado/a?
p6100_ind_cab <- data.frame(svytable(~fex_c + p6100 + clase,
                                 design = BD_survey))
# Contributivo (EPS)
p6100_1_cab <- p6100_ind_cab %>% filter(p6100_ind_cab$p6100 == 1 & p6100_ind_cab$clase == 1) # 19.351.790
p6100_1_cab <- sum(p6100_1_cab$Freq)

# Especial (Fuerzas Armadas, Ecopetrol, universidades públicas, magisterio)
p6100_2_cab <- p6100_ind_cab %>% filter(p6100_ind_cab$p6100 == 2 & p6100_ind_cab$clase == 1) # 936.649
p6100_2_cab <- sum(p6100_2_cab$Freq)

# Subsidiado (EPS-S)
p6100_3_cab <- p6100_ind_cab %>% filter(p6100_ind_cab$p6100 == 3 & p6100_ind_cab$clase == 1) # 15.782.439
p6100_3_cab <- sum(p6100_3_cab$Freq)

# No sabe, no informa
p6100_9_cab <- p6100_ind_cab %>% filter(p6100_ind_cab$p6100 ==9 & p6100_ind_cab$clase == 1) # 127.174
p6100_9_cab <- sum(p6100_9_cab$Freq)

# Total p6100
p6100_total_cab <- p6100_1_cab + p6100_2_cab + p6100_3_cab + p6100_9_cab  # 36.198.054
####################################################################################################################################################

# 4. ¿Quién paga mensualmente por la afiliación de ...?
p6115_ind_cab <- data.frame(svytable(~fex_c + p6115 + clase,
                                 design = BD_survey))

# Paga una parte y otra la empresa o patron
p6115_1_cab <- p6115_ind_cab %>% filter(p6115_ind_cab$p6115 == 1 & p6115_ind_cab$clase == 1) # 6.212.965
p6115_1_cab <- sum(p6115_1_cab$Freq)

# Le descuentan de la pension
p6115_2_cab <- p6115_ind_cab %>% filter(p6115_ind_cab$p6115 == 2 & p6115_ind_cab$clase == 1) # 1.814.128
p6115_2_cab <- sum(p6115_2_cab$Freq)

# Paga la totalidad de la afiliacion
p6115_3_cab <- p6115_ind_cab %>% filter(p6115_ind_cab$p6115 == 3 & p6115_ind_cab$clase == 1) # 2.525.898
p6115_3_cab <- sum(p6115_3_cab$Freq)

# Paga completamente la empresa o patron donde trabaja o trabajó
p6115_4_cab <- p6115_ind_cab %>% filter(p6115_ind_cab$p6115 == 4 & p6115_ind_cab$clase == 1) # 610.609
p6115_4_cab <- sum(p6115_4_cab$Freq)

# No paga, es beneficiario/a
p6115_5_cab <- p6115_ind_cab %>% filter(p6115_ind_cab$p6115 == 5 & p6115_ind_cab$clase == 1) # 9.124.838
p6115_5_cab <- sum(p6115_5_cab$Freq)

# Total p6115
p6115_total_cab <- p6115_1_cab + p6115_2_cab + p6115_3_cab + p6115_4_cab + p6115_5_cab  # 20.288.440
#####################################################################################################################################################

# 5. ¿De quién es beneficiario/a ...?
p5669_ind_cab <- data.frame(svytable(~fex_c + p5669 + clase,
                                 design = BD_survey))

# De una persona de este hogar
p5669_1_cab <- p5669_ind_cab %>% filter(p5669_ind_cab$p5669 == 1 & p5669_ind_cab$clase == 1) # 7.390.702
p5669_1_cab <- sum(p5669_1_cab$Freq)

# De una persona de otro hogar
p5669_2_cab <- p5669_ind_cab %>% filter(p5669_ind_cab$p5669 == 2 & p5669_ind_cab$clase == 1) # 1.734.136
p5669_2_cab <- sum(p5669_2_cab$Freq)

# Total p5669
p5669_total_cab <- p5669_1_cab + p5669_2_cab  # 9.124.838
######################################################################################################################################################

# Numero Orden
#p5669s1_ind_cab <- data.frame(svytable(~fex_c + p5669s1 + clase,
                                       #design = BD_survey))
#p5669s1_cab <- p5669s1_ind_cab %>% filter(p5669s1_ind_cab$clase == 1)


# 1 <- 16.854
# 2 <- 6.431
# 3 <- 907
# 4 <- 400
# 5 <- 155
# 6 <- 64
# 7 <- 19
# 8 <- 7
# 9 <- 2
# 10 <- 2
# 14 <- 2
# 17<- 1
# -1 <- 1
# -2 <- 7
# -3 <- 1
# -4 <- 2
#######################################################################################################################################################

# 6. ¿Cuánto paga o cuánto le descuentan mensualmente a ... para estar cubierto/a por una entidad de seguridad social en salud? 
#p8551 <- table(BD_ecv_ind$p8551)
#######################################################################################################################################################

# 7. En general, considera que la calidad del servicio de su EPS o de la entidad de seguridad social en salud en la cual ... está afiliado/a es

p6181_ind_cab <- data.frame(svytable(~fex_c + p6181 + clase,
                                 design = BD_survey))

# Muy buena
p6181_1_cab <- p6181_ind_cab %>% filter(p6181_ind_cab$p6181 == 1 & p6181_ind_cab$clase == 1)
p6181_1_cab <- sum(p6181_1_cab$Freq)                             # 4.760.351

# Buena
p6181_2_cab <- p6181_ind_cab %>% filter(p6181_ind_cab$p6181 == 2 & p6181_ind_cab$clase == 1)
p6181_2_cab <- sum(p6181_2_cab$Freq)                            # 26.809.768

# Mala
p6181_3_cab <- p6181_ind_cab %>% filter(p6181_ind_cab$p6181 == 3 & p6181_ind_cab$clase == 1)
p6181_3_cab <- sum(p6181_3_cab$Freq)                            # 3.216.178

# Muy mala
p6181_4_cab <- p6181_ind_cab %>% filter(p6181_ind_cab$p6181 == 4 & p6181_ind_cab$clase == 1)
p6181_4_cab <- sum(p6181_4_cab$Freq)                            # 653.001

# No sabe
p6181_9_cab <- p6181_ind_cab %>% filter(p6181_ind_cab$p6181 == 9 & p6181_ind_cab$clase == 1)
p6181_9_cab <- sum(p6181_9_cab$Freq)                            # 631.579

# total p6181
p6181_total_cab <- p6181_1_cab + p6181_2_cab + p6181_3_cab + p6181_4_cab + p6181_9_cab
########################################################################################################################################################

# 8. ¿Cuál es el aspecto que más influye en su percepción sobre la calidad del servicio de su EPS o de la entidad de seguridad social en salud en la cual se encuentra afiliado/a? 

p798_ind_cab <- data.frame(svytable(~fex_c + p798 + clase,
                                design = BD_survey))

# Tramites excesivos o dispendiosos
p798_1_cab <- p798_ind_cab %>% filter(p798_ind_cab$p798 == 1 & p798_ind_cab$clase == 1)  # 999.491
p798_1_cab <- sum(p798_1_cab$Freq)

# Mala atencion del personal administrativo o asistencial (medicos, enfermeras, etc)
p798_2_cab <- p798_ind_cab %>% filter(p798_ind_cab$p798 == 2 & p798_ind_cab$clase == 1)  # 510.123
p798_2_cab <- sum(p798_2_cab$Freq)

# Falta de capacidad, conocimientos o habilidad del personal asistencial
p798_3_cab <- p798_ind_cab %>% filter(p798_ind_cab$p798 == 3 & p798_ind_cab$clase == 1)  # 90.574
p798_3_cab <- sum(p798_3_cab$Freq)

# Condiciones deficientes de infraestructura, dotación, mobiliario o accesibilidad para población en condición de discapacidad
p798_4_cab <- p798_ind_cab %>% filter(p798_ind_cab$p798 == 4 & p798_ind_cab$clase == 1)  # 39.096
p798_4_cab <- sum(p798_4_cab$Freq)

# Demora en la asignacion de citas
p798_5_cab <- p798_ind_cab %>% filter(p798_ind_cab$p798 == 5 & p798_ind_cab$clase == 1)  # 1.834.950
p798_5_cab <- sum(p798_5_cab$Freq)

# Demora en la atencion por parte del personal medico
p798_6_cab <- p798_ind_cab %>% filter(p798_ind_cab$p798 == 6 & p798_ind_cab$clase == 1)  # 242.322
p798_6_cab <- sum(p798_6_cab$Freq)

# Problemas relacionados con los medicamentos
p798_7_cab <- p798_ind_cab %>% filter(p798_ind_cab$p798 == 7 & p798_ind_cab$clase == 1)  # 137.595
p798_7_cab <- sum(p798_7_cab$Freq)

# Otro
p798_8_cab <- p798_ind_cab %>% filter(p798_ind_cab$p798 == 8 & p798_ind_cab$clase == 1)  # 15.025
p798_8_cab <- sum(p798_8_cab$Freq)

# Total p798
p798_total_cab <- p798_1_cab + p798_2_cab + p798_3_cab + p798_4_cab + p798_5_cab + p798_6_cab + p798_7_cab + p798_8_cab  # 3.869.179
##############################################################################################################################################################

# 9. ¿Cuáles de los siguientes planes o seguros VOLUNTARIOS de salud tiene_________? Medicina prepagada

p799s2_ind_cab <- data.frame(svytable(~fex_c + p799s2 + clase,
                                  design = BD_survey)) 
# Si
p799s2_1_cab <- p799s2_ind_cab %>% filter(p799s2_ind_cab$p799s2 == 1 & p799s2_ind_cab$clase == 1) # 1.612.601
p799s2_1_cab <- sum(p799s2_1_cab$Freq)

# No
p799s2_2_cab <- p799s2_ind_cab %>% filter(p799s2_ind_cab$p799s2 == 2 & p799s2_ind_cab$clase == 1) # 37.456.003
p799s2_2_cab <- sum(p799s2_2_cab$Freq)     

# total p799s2
p799s2_total_cab <- p799s2_1_cab + p799s2_2_cab
##############################################################################################################################################################

# 9.2 Plan complementario de salud con una EPS
p799s3_ind_cab <- data.frame(svytable(~fex_c + p799s3 + clase,
                                  design = BD_survey))
# Si 
p799s3_1_cab <- p799s3_ind_cab %>% filter(p799s3_ind_cab$p799s3 == 1 & p799s3_ind_cab$clase == 1) # 1.271.847
p799s3_1_cab <- sum(p799s3_1_cab$Freq)

# No
p799s3_2_cab <- p799s3_ind_cab %>% filter(p799s3_ind_cab$p799s3 == 2 & p799s3_ind_cab$clase == 1) # 37.796.757
p799s3_2_cab <- sum(p799s3_2_cab$Freq)   

# total p799s3
p799s3_total_cab <- p799s3_1_cab + p799s3_2_cab
##############################################################################################################################################################

# 9.3 Poliza de hospitalizacion o cirugia
p799s1_ind_cab <- data.frame(svytable(~fex_c + p799s1 + clase,
                                  design = BD_survey))
# Si 
p799s1_1_cab <- p799s1_ind_cab %>% filter(p799s1_ind_cab$p799s1 == 1 & p799s1_ind_cab$clase == 1) # 222.884
p799s1_1_cab <- sum(p799s1_1_cab$Freq)

# No
p799s1_2_cab <- p799s1_ind_cab %>% filter(p799s1_ind_cab$p799s1 == 2 & p799s1_ind_cab$clase == 1) # 38.845.720
p799s1_2_cab <- sum(p799s1_2_cab$Freq) 

# total p799s1
p799s1_total_cab <- p799s1_1_cab + p799s1_2_cab
###############################################################################################################################################################

# 9.4 Seguros medicos estudiantiles
p799s4_ind_cab <- data.frame(svytable(~fex_c + p799s4 + clase,
                                  design = BD_survey))
# Si 
p799s4_1_cab <- p799s4_ind_cab %>% filter(p799s4_ind_cab$p799s4 == 1 & p799s4_ind_cab$clase == 1) # 207.740
p799s4_1_cab <- sum(p799s4_1_cab$Freq)

# No
p799s4_2_cab <- p799s4_ind_cab %>% filter(p799s4_ind_cab$p799s4 == 2 & p799s4_ind_cab$clase == 1) # 38.860.864
p799s4_2_cab <- sum(p799s4_2_cab$Freq) 

# total p799s4
p799s4_total_cab <- p799s4_1_cab + p799s4_2_cab
##############################################################################################################################################################

# 9.5 Otro (ambulancia, asistencia medica domiciliaria, etc)
p799s5_ind_cab <- data.frame(svytable(~fex_c + p799s5 + clase,
                                  design = BD_survey))
# Si 
p799s5_1_cab <- p799s5_ind_cab %>% filter(p799s5_ind_cab$p799s5 == 1 & p799s5_ind_cab$clase == 1) # 369.011
p799s5_1_cab <- sum(p799s5_1_cab$Freq)

# No
p799s5_2_cab <- p799s5_ind_cab %>% filter(p799s5_ind_cab$p799s5 == 2 & p799s5_ind_cab$clase == 1) # 38.699.593
p799s5_2_cab <- sum(p799s5_2_cab$Freq) 

# total p799s5
p799s5_total_cab <- p799s5_1_cab + p799s5_2_cab
################################################################################################################################################################

# 10. ¿Cuánto paga o  le descuentan mensualmente a ________ por concepto de estos planes o seguros voluntarios de salud?
#p3176 <- table(BD_ecv_ind$p3176)
################################################################################################################################################################

# 11. El estado de salud de ... en general, es:
p6127_ind_cab <- data.frame(svytable(~fex_c + p6127 + clase,
                                 design = BD_survey))
# Muy bueno
p6127_1_cab <- p6127_ind_cab %>% filter(p6127_ind_cab$p6127 == 1 & p6127_ind_cab$clase == 1)
p6127_1_cab <- sum(p6127_1_cab$Freq)                             # 7.948.033

# Bueno
p6127_2_cab <- p6127_ind_cab %>% filter(p6127_ind_cab$p6127 == 2 & p6127_ind_cab$clase == 1)
p6127_2_cab <- sum(p6127_2_cab$Freq)                             # 26.200.078

# Regular
p6127_3_cab <- p6127_ind_cab %>% filter(p6127_ind_cab$p6127 == 3 & p6127_ind_cab$clase == 1)
p6127_3_cab <- sum(p6127_3_cab$Freq)                            # 4.569.673

# Malo
p6127_4_cab <- p6127_ind_cab %>% filter(p6127_ind_cab$p6127 == 4 & p6127_ind_cab$clase == 1)
p6127_4_cab <- sum(p6127_4_cab$Freq)                            # 350.820

# Total p6127
p6127_total_cab <- p6127_1_cab + p6127_2_cab + p6127_3_cab + p6127_4_cab    # 39.068.605
###############################################################################################################################################################

# 12.1 ¿A....le han diagnosticado alguna enfermedad crónica? (enfermedad de larga duración y prolongados tratamientos como: 
#    enfermedades cardiovasculares-hipertensión, asma, bronquitis crónica, gastritis, lupus, cáncer, gota, leucemia, diabetes, etc.).

p1930_ind_cab <- data.frame(svytable(~fex_c + p1930 + clase,
                                 design = BD_survey))
# Si
p1930_1_cab <- p1930_ind_cab %>% filter(p1930_ind_cab$p1930 == 1 & p1930_ind_cab$clase == 1)
p1930_1_cab <- sum(p1930_1_cab$Freq)                            # 6.436.021

# No
p1930_2_cab <- p1930_ind_cab %>% filter(p1930_ind_cab$p1930 == 2  & p1930_ind_cab$clase == 1)
p1930_2_cab <- sum(p1930_2_cab$Freq)                            # 32.632.583

# total p1930
p1930_total_cab <- p1930_1_cab + p1930_2_cab  # 39.068.605
################################################################################################################################################################

# 12.2 ¿Recibe o recibió tratamiento formulado por el médico?
p1930s1_ind_cab <- data.frame(svytable(~fex_c + p1930s1 + clase,
                                   design = BD_survey))

# Si
p1930s1_1_cab <- p1930s1_ind_cab %>% filter(p1930s1_ind_cab$p1930s1 == 1 & p1930s1_ind_cab$clase == 1)
p1930s1_1_cab <- sum(p1930s1_1_cab$Freq)                                   # 5.789.286

# No
p1930s1_2_cab <- p1930s1_ind_cab %>% filter(p1930s1_ind_cab$p1930s1 == 2  & p1930s1_ind_cab$clase == 1)
p1930s1_2_cab <- sum(p1930s1_2_cab$Freq)                                  # 646.734

# total p1930s1
p1930s1_total_cab <- p1930s1_1_cab + p1930s1_2_cab  # 6.436.021
###############################################################################################################################################################
# 13.1 Dada su condición física y mental, y sin ningún tipo de ayuda, ¿ ... puede: Oir la voz o los sonidos?
p1906s1_ind_cab <- data.frame(svytable(~fex_c + p1906s1 + clase,
                                   design = BD_survey))

# No puede hacerlo
p1906s1_1_cab <- p1906s1_ind_cab %>% filter(p1906s1_ind_cab$p1906s1 == 1 & p1906s1_ind_cab$clase == 1)
p1906s1_1_cab <- sum(p1906s1_1_cab$Freq)                                # 46.999

# Si, con mucha dificultad
p1906s1_2_cab <- p1906s1_ind_cab %>% filter(p1906s1_ind_cab$p1906s1 == 2 & p1906s1_ind_cab$clase == 1)
p1906s1_2_cab <- sum(p1906s1_2_cab$Freq)                                # 300.424

# Si, con alguna dificultad
p1906s1_3_cab <- p1906s1_ind_cab %>% filter(p1906s1_ind_cab$p1906s1 == 3 & p1906s1_ind_cab$clase == 1)
p1906s1_3_cab <- sum(p1906s1_3_cab$Freq)                                # 993.257

# Sin dificultad
p1906s1_4_cab <- p1906s1_ind_cab %>% filter(p1906s1_ind_cab$p1906s1 == 4 & p1906s1_ind_cab$clase == 1)
p1906s1_4_cab <- sum(p1906s1_4_cab$Freq)                                # 37.727.923

# total p1906s1
p1906s1_total_cab <- p1906s1_1_cab + p1906s1_2_cab + p1906s1_3_cab + p1906s1_4_cab  # 39.068.605
################################################################################################################################################################

# 13.2 Hablar o conversar
p1906s2_ind_cab <- data.frame(svytable(~fex_c + p1906s2 + clase,
                                   design = BD_survey))
# No puede hacerlo
p1906s2_1_cab <- p1906s2_ind_cab %>% filter(p1906s2_ind_cab$p1906s2 == 1 & p1906s2_ind_cab$clase == 1)
p1906s2_1_cab <- sum(p1906s2_1_cab$Freq)                                # 164.803

# Si, con mucha dificultad
p1906s2_2_cab <- p1906s2_ind_cab %>% filter(p1906s2_ind_cab$p1906s2 == 2 & p1906s2_ind_cab$clase == 1)
p1906s2_2_cab <- sum(p1906s2_2_cab$Freq)                                # 200.978

# Si, con alguna dificultad
p1906s2_3_cab <- p1906s2_ind_cab %>% filter(p1906s2_ind_cab$p1906s2 == 3 & p1906s2_ind_cab$clase == 1)
p1906s2_3_cab <- sum(p1906s2_3_cab$Freq)                               # 540.821

# Sin dificultad
p1906s2_4_cab <- p1906s2_ind_cab %>% filter(p1906s2_ind_cab$p1906s2 == 4 & p1906s2_ind_cab$clase == 1)
p1906s2_4_cab <- sum(p1906s2_4_cab$Freq)                              # 38.162.001

# total p1906s2
p1906s2_total_cab <- p1906s2_1_cab + p1906s2_2_cab + p1906s2_3_cab + p1906s2_4_cab  # 39.068.605 
################################################################################################################################################################

# 13.3 Ver de verca, de lejos o alrededor
p1906s3_ind_cab <- data.frame(svytable(~fex_c + p1906s3 + clase,
                                   design = BD_survey))
# No puede hacerlo
p1906s3_1_cab <- p1906s3_ind_cab %>% filter(p1906s3_ind_cab$p1906s3 == 1 & p1906s3_ind_cab$clase == 1)
p1906s3_1_cab <- sum(p1906s3_1_cab$Freq)                               # 44.926

# Si, con mucha dificultad
p1906s3_2_cab <- p1906s3_ind_cab %>% filter(p1906s3_ind_cab$p1906s3 == 2 & p1906s3_ind_cab$clase == 1)
p1906s3_2_cab <- sum(p1906s3_2_cab$Freq)                               # 1.228.273

# Si, con alguna dificultad
p1906s3_3_cab <- p1906s3_ind_cab %>% filter(p1906s3_ind_cab$p1906s3 == 3 & p1906s3_ind_cab$clase == 1)
p1906s3_3_cab <- sum(p1906s3_3_cab$Freq)                               # 6.421.120

# Sin dificultad
p1906s3_4_cab <- p1906s3_ind_cab %>% filter(p1906s3_ind_cab$p1906s3 == 4 & p1906s3_ind_cab$clase == 1)
p1906s3_4_cab <- sum(p1906s3_4_cab$Freq)                              # 31.374.284

# Total p1906s3
p1906s3_total_cab <- p1906s3_1_cab + p1906s3_2_cab + p1906s3_3_cab + p1906s3_4_cab  # 39.068.605
################################################################################################################################################################

# 13.4 Mover el cuerpo, caminar o subir y bajar escaleras?
p1906s4_ind_cab <- data.frame(svytable(~fex_c + p1906s4 + clase,
                                   design = BD_survey))
# No puede hacerlo
p1906s4_1_cab <- p1906s4_ind_cab %>% filter(p1906s4_ind_cab$p1906s4 == 1 & p1906s4_ind_cab$clase == 1)
p1906s4_1_cab <- sum(p1906s4_1_cab$Freq)                               # 191.841

# Si, con mucha dificultad
p1906s4_2_cab <- p1906s4_ind_cab %>% filter(p1906s4_ind_cab$p1906s4 == 2 & p1906s4_ind_cab$clase == 1)
p1906s4_2_cab <- sum(p1906s4_2_cab$Freq)                              # 644.176

# Si, con alguna dificultad
p1906s4_3_cab <- p1906s4_ind_cab %>% filter(p1906s4_ind_cab$p1906s4 == 3 & p1906s4_ind_cab$clase == 1)
p1906s4_3_cab <- sum(p1906s4_3_cab$Freq)                              # 1.843.478

# Sin dificultad
p1906s4_4_cab <- p1906s4_ind_cab %>% filter(p1906s4_ind_cab$p1906s4 == 4 & p1906s4_ind_cab$clase == 1)
p1906s4_4_cab <- sum(p1906s4_4_cab$Freq)                               # 36.389.108

# total p1906s4
p1906s4_total_cab <- p1906s4_1_cab + p1906s4_2_cab + p1906s4_3_cab + p1906s4_4_cab   # 39.068.605
#############################################################################################################################################################

# 13.5 Agarrar o mover objetos con las manos?
p1906s5_ind_cab <- data.frame(svytable(~fex_c + p1906s5 + clase,
                                   design = BD_survey))

# No puede hacerlo 
p1906s5_1_cab <- p1906s5_ind_cab %>% filter(p1906s5_ind_cab$p1906s5 == 1 & p1906s5_ind_cab$clase == 1)
p1906s5_1_cab <- sum(p1906s5_1_cab$Freq)                                # 84.951

# Si, con mucha dificultad
p1906s5_2_cab <- p1906s5_ind_cab %>% filter(p1906s5_ind_cab$p1906s5 == 2  & p1906s5_ind_cab$clase == 1)
p1906s5_2_cab <- sum(p1906s5_2_cab$Freq)                               # 267.246

# Si, con alguna dificultad
p1906s5_3_cab <- p1906s5_ind_cab %>% filter(p1906s5_ind_cab$p1906s5 == 3  & p1906s5_ind_cab$clase == 1)
p1906s5_3_cab <- sum(p1906s5_3_cab$Freq)                               # 942.223

# Sin dificultad
p1906s5_4_cab <- p1906s5_ind_cab %>% filter(p1906s5_ind_cab$p1906s5 == 4  & p1906s5_ind_cab$clase == 1)
p1906s5_4_cab <- sum(p1906s5_4_cab$Freq)                              # 37.774.183

#  total p1906s5
p1906s5_total_cab <- p1906s5_1_cab + p1906s5_2_cab + p1906s5_3_cab + p1906s5_4_cab   # 39.068.605
##############################################################################################################################################################

# 13.6 Entender, aprender, recordar o tomar decisiones por sí mismo/a?
p1906s6_ind_cab <- data.frame(svytable(~fex_c + p1906s6 + clase, 
                                   design = BD_survey))

# No puede hacerlo
p1906s6_1_cab <- p1906s6_ind_cab %>% filter(p1906s6_ind_cab$p1906s6 == 1 & p1906s6_ind_cab$clase == 1)
p1906s6_1_cab <- sum(p1906s6_1_cab$Freq)                              # 234.297

# Si, con mucha dificultad
p1906s6_2_cab <- p1906s6_ind_cab %>% filter(p1906s6_ind_cab$p1906s6 == 2 & p1906s6_ind_cab$clase == 1)
p1906s6_2_cab <- sum(p1906s6_2_cab$Freq)                               # 234.030

# Si, con alguna dificultad
p1906s6_3_cab <- p1906s6_ind_cab %>% filter(p1906s6_ind_cab$p1906s6 == 3 & p1906s6_ind_cab$clase == 1)
p1906s6_3_cab <- sum(p1906s6_3_cab$Freq)                                # 682.966

# Sin dificultad
p1906s6_4_cab <- p1906s6_ind_cab %>% filter(p1906s6_ind_cab$p1906s6 == 4 & p1906s6_ind_cab$clase == 1)
p1906s6_4_cab <- sum(p1906s6_4_cab$Freq)                               # 37.917.310

# total p1906s6
p1906s6_total_cab <- p1906s6_1_cab + p1906s6_2_cab + p1906s6_3_cab + p1906s6_4_cab  # 39.068.605
###############################################################################################################################################################

# 13.7 Comer, vestirse o bañarse por sí mismo/a?
p1906s7_ind_cab <- data.frame(svytable(~fex_c + p1906s7 + clase,
                                   design = BD_survey))

# No puede hacerlo 
p1906s7_1_cab <- p1906s7_ind_cab %>% filter(p1906s7_ind_cab$p1906s7 ==1 & p1906s7_ind_cab$clase == 1)
p1906s7_1_cab <- sum(p1906s7_1_cab$Freq)                               # 274.045

# Si, con mucha dificultad
p1906s7_2_cab <- p1906s7_ind_cab %>% filter(p1906s7_ind_cab$p1906s7 == 2 & p1906s7_ind_cab$clase == 1)
p1906s7_2_cab <- sum(p1906s7_2_cab$Freq)                               # 164.761

# Si, con alguna dificultad
p1906s7_3_cab <- p1906s7_ind_cab %>% filter(p1906s7_ind_cab$p1906s7 == 3 & p1906s7_ind_cab$clase == 1)
p1906s7_3_cab <- sum(p1906s7_3_cab$Freq)                               # 598.750

# Sin dificultad
p1906s7_4_cab <- p1906s7_ind_cab %>% filter(p1906s7_ind_cab$p1906s7 == 4 & p1906s7_ind_cab$clase == 1)
p1906s7_4_cab <- sum(p1906s7_4_cab$Freq)                               # 38.031.048

# total p1906s7
p1906s7_total_cab <- p1906s7_1_cab + p1906s7_2_cab + p1906s7_3_cab + p1906s7_4_cab  # 39.068.605
###########################################################################################################################################################

# 13.8 Relacionarse o interactuar con las demás personas?
p1906s8_ind_cab <- data.frame(svytable(~fex_c + p1906s8 + clase,
                                   design = BD_survey))

# No puede hacerlo 
p1906s8_1_cab <- p1906s8_ind_cab %>% filter(p1906s8_ind_cab$p1906s8 == 1 & p1906s8_ind_cab$clase == 1)
p1906s8_1_cab <- sum(p1906s8_1_cab$Freq)                               # 147.579

# Si, con mucha dificultad
p1906s8_2_cab <- p1906s8_ind_cab %>% filter(p1906s8_ind_cab$p1906s8 == 2 & p1906s8_ind_cab$clase == 1)
p1906s8_2_cab <- sum(p1906s8_2_cab$Freq)                               # 165.645

# Si, con alguna dificultad
p1906s8_3_cab <- p1906s8_ind_cab %>% filter(p1906s8_ind_cab$p1906s8 == 3 & p1906s8_ind_cab$clase == 1)
p1906s8_3_cab <- sum(p1906s8_3_cab$Freq)                               # 440.559

# Sin dificultad
p1906s8_4_cab <- p1906s8_ind_cab %>% filter(p1906s8_ind_cab$p1906s8 == 4 & p1906s8_ind_cab$clase == 1)  
p1906s8_4_cab <- sum(p1906s8_4_cab$Freq)                               # 38.314.820

# total p1906s8
p1906s8_total_cab <- p1906s8_1_cab + p1906s8_2_cab + p1906s8_3_cab+ p1906s8_4_cab  # 39.068.605
#############################################################################################################################################################

# 14.1 ¿Esta dificultad de ... fue ocasionada: ¿Esta dificultad (Oír la voz o los sonidos) de ... fue ocasionada
p1908s1_ind_cab <- data.frame(svytable(~fex_c + p1908s1 + clase,
                                   design = BD_survey))
# Porque nació así
p1908s1_1_cab <- p1908s1_ind_cab %>% filter(p1908s1_ind_cab$p1908s1 == 1 & p1908s1_ind_cab$clase == 1)
p1908s1_1_cab <- sum(p1908s1_1_cab$Freq)                                 # 61.645

# Por enfermedad
p1908s1_2_cab <- p1908s1_ind_cab %>% filter(p1908s1_ind_cab$p1908s1 == 2 & p1908s1_ind_cab$clase == 1)
p1908s1_2_cab <- sum(p1908s1_2_cab$Freq)                                 # 92.201

# Por accidente laboral o enfermedad profesional
p1908s1_3_cab <- p1908s1_ind_cab %>% filter(p1908s1_ind_cab$p1908s1 == 3 & p1908s1_ind_cab$clase == 1)
p1908s1_3_cab <- sum(p1908s1_3_cab$Freq)                                 # 16.727

# Por otro tipo de accidente
p1908s1_4_cab <- p1908s1_ind_cab %>% filter(p1908s1_ind_cab$p1908s1 == 4 & p1908s1_ind_cab$clase == 1)
p1908s1_4_cab <- sum(p1908s1_4_cab$Freq)                                # 17.210

# Por edad avanzada
p1908s1_5_cab <- p1908s1_ind_cab %>% filter(p1908s1_ind_cab$p1908s1 == 5 & p1908s1_ind_cab$clase == 1)
p1908s1_5_cab <- sum(p1908s1_5_cab$Freq)                                # 139.455

# Por el conflicto armado
p1908s1_6_cab <- p1908s1_ind_cab %>% filter(p1908s1_ind_cab$p1908s1 == 6 & p1908s1_ind_cab$clase == 1)
p1908s1_6_cab <- sum(p1908s1_6_cab$Freq)                               # 3.113

# Por violencia NO asociada al conflicto armado
p1908s1_7_cab <- p1908s1_ind_cab %>% filter(p1908s1_ind_cab$p1908s1 == 7 & p1908s1_ind_cab$clase == 1)
p1908s1_7_cab <- sum(p1908s1_7_cab$Freq)                               # 3.084

# Por otra causa
p1908s1_8_cab <- p1908s1_ind_cab %>% filter(p1908s1_ind_cab$p1908s1 == 8 & p1908s1_ind_cab$clase == 1)
p1908s1_8_cab <- sum(p1908s1_8_cab$Freq)                               # 7.390

# No sabe
p1908s1_9_cab <- p1908s1_ind_cab %>% filter(p1908s1_ind_cab$p1908s1 == 9 & p1908s1_ind_cab$clase == 1)
p1908s1_9_cab <- sum(p1908s1_9_cab$Freq)                               # 6.594

# total 1908s1
p1908s1_total_cab <- p1908s1_1_cab + p1908s1_2_cab + p1908s1_3_cab + p1908s1_4_cab + p1908s1_5_cab + p1908s1_6_cab + p1908s1_7_cab + p1908s1_8_cab + p1908s1_9_cab   # 347.423
###########################################################################################################################################

# 14.2 ¿Esta dificultad (Hablar o conversar) de ... fue ocasionada:
p1908s2_ind_cab <- data.frame(svytable(~fex_c + p1908s2 + clase,
                                   design = BD_survey))
# Porque nació así
p1908s2_1_cab <- p1908s2_ind_cab %>% filter(p1908s2_ind_cab$p1908s2 == 1 & p1908s2_ind_cab$clase == 1)
p1908s2_1_cab <- sum(p1908s2_1_cab$Freq)                                 # 128.873

# Por enfermedad
p1908s2_2_cab <- p1908s2_ind_cab %>% filter(p1908s2_ind_cab$p1908s2 == 2 & p1908s2_ind_cab$clase == 1)
p1908s2_2_cab <- sum(p1908s2_2_cab$Freq)                                 # 117.819

# Por accidente laboral o enfermedad profesional
p1908s2_3_cab <- p1908s2_ind_cab %>% filter(p1908s2_ind_cab$p1908s2 == 3 & p1908s2_ind_cab$clase == 1)
p1908s2_3_cab <- sum(p1908s2_3_cab$Freq)                                 # 1.812

# Por otro tipo de accidente
p1908s2_4_cab <- p1908s2_ind_cab %>% filter(p1908s2_ind_cab$p1908s2 == 4 & p1908s2_ind_cab$clase == 1)
p1908s2_4_cab <- sum(p1908s2_4_cab$Freq)                                # 8.188

# Por edad avanzada
p1908s2_5_cab <- p1908s2_ind_cab %>% filter(p1908s2_ind_cab$p1908s2 == 5 & p1908s2_ind_cab$clase == 1)
p1908s2_5_cab <- sum(p1908s2_5_cab$Freq)                                # 27.989

# Por el conflicto armado
p1908s2_6_cab <- p1908s2_ind_cab %>% filter(p1908s2_ind_cab$p1908s2 == 6 & p1908s2_ind_cab$clase == 1)
p1908s2_6_cab <- sum(p1908s2_6_cab$Freq)                               # 0

# Por violencia NO asociada al conflicto armado
p1908s2_7_cab <- p1908s2_ind_cab %>% filter(p1908s2_ind_cab$p1908s2 == 7 & p1908s2_ind_cab$clase == 1)
p1908s2_7_cab <- sum(p1908s2_7_cab$Freq)                               # 3.630

# Por otra causa
p1908s2_8_cab <- p1908s2_ind_cab %>% filter(p1908s2_ind_cab$p1908s2 == 8 & p1908s2_ind_cab$clase == 1)
p1908s2_8_cab <- sum(p1908s2_8_cab$Freq)                               # 50.512

# No sabe
p1908s2_9_cab <- p1908s2_ind_cab %>% filter(p1908s2_ind_cab$p1908s2 == 9 & p1908s2_ind_cab$clase == 1)
p1908s2_9_cab <- sum(p1908s2_9_cab$Freq)                               # 26.955

# total 1908s1
p1908s2_total_cab <- p1908s2_1_cab + p1908s2_2_cab + p1908s2_3_cab + p1908s2_4_cab + p1908s2_5_cab + p1908s2_6_cab + p1908s2_7_cab +
  p1908s2_8_cab + p1908s2_9_cab   # 365.782
#####################################################################################################################################################

# 14.3 ¿Esta dificultad (Ver de cerca, de lejos o alrededor) de ... fue ocasionada:
p1908s3_ind_cab <- data.frame(svytable(~fex_c + p1908s3 + clase,
                                   design = BD_survey))
# Porque nació así
p1908s3_1_cab <- p1908s3_ind_cab %>% filter(p1908s3_ind_cab$p1908s3 == 1 & p1908s3_ind_cab$clase == 1)
p1908s3_1_cab <- sum(p1908s3_1_cab$Freq)                                 # 171.457

# Por enfermedad
p1908s3_2_cab <- p1908s3_ind_cab %>% filter(p1908s3_ind_cab$p1908s3 == 2 & p1908s3_ind_cab$clase == 1)
p1908s3_2_cab <- sum(p1908s3_2_cab$Freq)                                 # 537.370

# Por accidente laboral o enfermedad profesional
p1908s3_3_cab <- p1908s3_ind_cab %>% filter(p1908s3_ind_cab$p1908s3 == 3 & p1908s3_ind_cab$clase == 1)
p1908s3_3_cab <- sum(p1908s3_3_cab$Freq)                                 # 23.062

# Por otro tipo de accidente
p1908s3_4_cab <- p1908s3_ind_cab %>% filter(p1908s3_ind_cab$p1908s3 == 4 & p1908s3_ind_cab$clase == 1)
p1908s3_4_cab <- sum(p1908s3_4_cab$Freq)                                # 25.221

# Por edad avanzada
p1908s3_5_cab <- p1908s3_ind_cab %>% filter(p1908s3_ind_cab$p1908s3 == 5 & p1908s3_ind_cab$clase == 1)
p1908s3_5_cab <- sum(p1908s3_5_cab$Freq)                                # 447.024

# Por el conflicto armado
p1908s3_6_cab <- p1908s3_ind_cab %>% filter(p1908s3_ind_cab$p1908s3 == 6 & p1908s3_ind_cab$clase == 1)
p1908s3_6_cab <- sum(p1908s3_6_cab$Freq)                               # 680

# Por violencia NO asociada al conflicto armado
p1908s3_7_cab <- p1908s3_ind_cab %>% filter(p1908s3_ind_cab$p1908s3 == 7 & p1908s3_ind_cab$clase == 1)
p1908s3_7_cab <- sum(p1908s3_7_cab$Freq)                               # 2.247

# Por otra causa
p1908s3_8_cab <- p1908s3_ind_cab %>% filter(p1908s3_ind_cab$p1908s3 == 8 & p1908s3_ind_cab$clase == 1)
p1908s3_8_cab <- sum(p1908s3_8_cab$Freq)                               # 30.275

# No sabe
p1908s3_9_cab <- p1908s3_ind_cab %>% filter(p1908s3_ind_cab$p1908s3 == 9 & p1908s3_ind_cab$clase == 1)
p1908s3_9_cab <- sum(p1908s3_9_cab$Freq)                               # 35.859

# total 1908s1
p1908s3_total_cab <- p1908s3_1_cab + p1908s3_2_cab + p1908s3_3_cab + p1908s3_4_cab + p1908s3_5_cab + p1908s3_6_cab +
  p1908s3_7_cab + p1908s3_8_cab + p1908s3_9_cab   # 1.273.199
###################################################################################################################################################################3

# 14. 4 ¿Esta dificultad (Mover el cuerpo, caminar o subir y bajar escaleras) de ... fue ocasionada:
p1908s4_ind_cab <- data.frame(svytable(~fex_c + p1908s4 + clase,
                                   design = BD_survey))
# Porque nació así
p1908s4_1_cab <- p1908s4_ind_cab %>% filter(p1908s4_ind_cab$p1908s4 == 1 & p1908s4_ind_cab$clase == 1)
p1908s4_1_cab <- sum(p1908s4_1_cab$Freq)                                 # 55.332

# Por enfermedad
p1908s4_2_cab <- p1908s4_ind_cab %>% filter(p1908s4_ind_cab$p1908s4 == 2 & p1908s4_ind_cab$clase == 1)
p1908s4_2_cab <- sum(p1908s4_2_cab$Freq)                                 # 394.668

# Por accidente laboral o enfermedad profesional
p1908s4_3_cab <- p1908s4_ind_cab %>% filter(p1908s4_ind_cab$p1908s4 == 3 & p1908s4_ind_cab$clase == 1)
p1908s4_3_cab <- sum(p1908s4_3_cab$Freq)                                 # 32.604

# Por otro tipo de accidente
p1908s4_4_cab <- p1908s4_ind_cab %>% filter(p1908s4_ind_cab$p1908s4 == 4 & p1908s4_ind_cab$clase == 1)
p1908s4_4_cab <- sum(p1908s4_4_cab$Freq)                                # 89.407

# Por edad avanzada
p1908s4_5_cab <- p1908s4_ind_cab %>% filter(p1908s4_ind_cab$p1908s4 == 5 & p1908s4_ind_cab$clase == 1)
p1908s4_5_cab <- sum(p1908s4_5_cab$Freq)                                # 186.349

# Por el conflicto armado
p1908s4_6_cab <- p1908s4_ind_cab %>% filter(p1908s4_ind_cab$p1908s4 == 6 & p1908s4_ind_cab$clase == 1)
p1908s4_6_cab <- sum(p1908s4_6_cab$Freq)                               # 3.975

# Por violencia NO asociada al conflicto armado
p1908s4_7_cab <- p1908s4_ind_cab %>% filter(p1908s4_ind_cab$p1908s4 == 7 & p1908s4_ind_cab$clase == 1)
p1908s4_7_cab <- sum(p1908s4_7_cab$Freq)                               # 4.040

# Por otra causa
p1908s4_8_cab <- p1908s4_ind_cab %>% filter(p1908s4_ind_cab$p1908s4 == 8 & p1908s4_ind_cab$clase == 1)
p1908s4_8_cab <- sum(p1908s4_8_cab$Freq)                               # 49.450

# No sabe
p1908s4_9_cab <- p1908s4_ind_cab %>% filter(p1908s4_ind_cab$p1908s4 == 9 & p1908s4_ind_cab$clase == 1)
p1908s4_9_cab <- sum(p1908s4_9_cab$Freq)                               # 20.188

# total 1908s1 cab
p1908s4_total_cab <- p1908s4_1_cab + p1908s4_2_cab + p1908s4_3_cab + p1908s4_4_cab + p1908s4_5_cab + p1908s4_6_cab +
  p1908s4_7_cab + p1908s4_8_cab + p1908s4_9_cab   # 836.017
############################################################################################################################################################3

# 14.5 ¿Esta dificultad (Agarrar o mover objetos con las manos) de ... fue ocasionada:
p1908s5_ind_cab <- data.frame(svytable(~fex_c + p1908s5 + clase,
                                   design = BD_survey))
# Porque nació así
p1908s5_1_cab <- p1908s5_ind_cab %>% filter(p1908s5_ind_cab$p1908s5 == 1 & p1908s5_ind_cab$clase == 1)
p1908s5_1_cab <- sum(p1908s5_1_cab$Freq)                                 # 37.893

# Por enfermedad
p1908s5_2_cab <- p1908s5_ind_cab %>% filter(p1908s5_ind_cab$p1908s5 == 2 & p1908s5_ind_cab$clase == 1)
p1908s5_2_cab <- sum(p1908s5_2_cab$Freq)                                 # 174.758

# Por accidente laboral o enfermedad profesional
p1908s5_3_cab <- p1908s5_ind_cab %>% filter(p1908s5_ind_cab$p1908s5 == 3 & p1908s5_ind_cab$clase == 1)
p1908s5_3_cab <- sum(p1908s5_3_cab$Freq)                                 # 19.191

# Por otro tipo de accidente
p1908s5_4_cab <- p1908s5_ind_cab %>% filter(p1908s5_ind_cab$p1908s5 == 4 & p1908s5_ind_cab$clase == 1)
p1908s5_4_cab <- sum(p1908s5_4_cab$Freq)                                # 36.925

# Por edad avanzada
p1908s5_5_cab <- p1908s5_ind_cab %>% filter(p1908s5_ind_cab$p1908s5 == 5 & p1908s5_ind_cab$clase == 1)
p1908s5_5_cab <- sum(p1908s5_5_cab$Freq)                                # 55.863

# Por el conflicto armado
p1908s5_6_cab <- p1908s5_ind_cab %>% filter(p1908s5_ind_cab$p1908s5 == 6 & p1908s5_ind_cab$clase == 1)
p1908s5_6_cab <- sum(p1908s5_6_cab$Freq)                               # 552

# Por violencia NO asociada al conflicto armado
p1908s5_7_cab <- p1908s5_ind_cab %>% filter(p1908s5_ind_cab$p1908s5 == 7 & p1908s5_ind_cab$clase == 1)
p1908s5_7_cab <- sum(p1908s5_7_cab$Freq)                               # 3.203

# Por otra causa
p1908s5_8_cab <- p1908s5_ind_cab %>% filter(p1908s5_ind_cab$p1908s5 == 8 & p1908s5_ind_cab$clase == 1)
p1908s5_8_cab <- sum(p1908s5_8_cab$Freq)                               # 17.737

# No sabe
p1908s5_9_cab <- p1908s5_ind_cab %>% filter(p1908s5_ind_cab$p1908s5 == 9 & p1908s5_ind_cab$clase == 1)
p1908s5_9_cab <- sum(p1908s5_9_cab$Freq)                               # 6.071

# total 1908s1
p1908s5_total_cab <- p1908s5_1_cab + p1908s5_2_cab + p1908s5_3_cab + p1908s5_4_cab + p1908s5_5_cab + p1908s5_6_cab +
  p1908s5_7_cab + p1908s5_8_cab + p1908s5_9_cab   # 352.197
#############################################################################################################################################################

# 14.6 ¿Esta dificultad (Entender, aprender, recordar o tomar decisiones por sí mismo/a) de ... fue ocasionada:
p1908s6_ind_cab <- data.frame(svytable(~fex_c + p1908s6 + clase,
                                   design = BD_survey))
# Porque nació así
p1908s6_1_cab <- p1908s6_ind_cab %>% filter(p1908s6_ind_cab$p1908s6 == 1 & p1908s6_ind_cab$clase == 1)
p1908s6_1_cab <- sum(p1908s6_1_cab$Freq)                                 # 143.059

# Por enfermedad
p1908s6_2_cab <- p1908s6_ind_cab %>% filter(p1908s6_ind_cab$p1908s6 == 2 & p1908s6_ind_cab$clase == 1)
p1908s6_2_cab <- sum(p1908s6_2_cab$Freq)                                 # 144.252

# Por accidente laboral o enfermedad profesional
p1908s6_3_cab <- p1908s6_ind_cab %>% filter(p1908s6_ind_cab$p1908s6 == 3 & p1908s6_ind_cab$clase == 1)
p1908s6_3_cab <- sum(p1908s6_3_cab$Freq)                                 # 3.479

# Por otro tipo de accidente
p1908s6_4_cab <- p1908s6_ind_cab %>% filter(p1908s6_ind_cab$p1908s6 == 4 & p1908s6_ind_cab$clase == 1)
p1908s6_4_cab <- sum(p1908s6_4_cab$Freq)                                # 4.653

# Por edad avanzada
p1908s6_5_cab <- p1908s6_ind_cab %>% filter(p1908s6_ind_cab$p1908s6 == 5 & p1908s6_ind_cab$clase == 1)
p1908s6_5_cab <- sum(p1908s6_5_cab$Freq)                                # 56.987

# Por el conflicto armado
p1908s6_6_cab <- p1908s6_ind_cab %>% filter(p1908s6_ind_cab$p1908s6 == 6 & p1908s6_ind_cab$clase == 1)
p1908s6_6_cab <- sum(p1908s6_6_cab$Freq)                               # 0

# Por violencia NO asociada al conflicto armado
p1908s6_7_cab <- p1908s6_ind_cab %>% filter(p1908s6_ind_cab$p1908s6 == 7 & p1908s6_ind_cab$clase == 1)
p1908s6_7_cab <- sum(p1908s6_7_cab$Freq)                               # 3.311

# Por otra causa
p1908s6_8_cab <- p1908s6_ind_cab %>% filter(p1908s6_ind_cab$p1908s6 == 8 & p1908s6_ind_cab$clase == 1)
p1908s6_8_cab <- sum(p1908s6_8_cab$Freq)                               # 79.061

# No sabe
p1908s6_9_cab <- p1908s6_ind_cab %>% filter(p1908s6_ind_cab$p1908s6 == 9 & p1908s6_ind_cab$clase == 1)
p1908s6_9_cab <- sum(p1908s6_9_cab$Freq)                               # 33.522

# total 1908s1
p1908s6_total_cab <- p1908s6_1_cab + p1908s6_2_cab + p1908s6_3_cab + p1908s6_4_cab + p1908s6_5_cab + p1908s6_6_cab +
  p1908s6_7_cab + p1908s6_8_cab + p1908s6_9_cab   # 468.327
##############################################################################################################################################################

# 14.7 ¿Esta dificultad (Comer, vestirse o bañarse por sí mismo/a) de ... fue ocasionada:
p1908s7_ind_cab <- data.frame(svytable(~fex_c + p1908s7 + clase,
                                   design = BD_survey))
# Porque nació así
p1908s7_1_cab <- p1908s7_ind_cab %>% filter(p1908s7_ind_cab$p1908s7 == 1 & p1908s7_ind_cab$clase == 1)
p1908s7_1_cab <- sum(p1908s7_1_cab$Freq)                                 # 58.950

# Por enfermedad
p1908s7_2_cab <- p1908s7_ind_cab %>% filter(p1908s7_ind_cab$p1908s7 == 2 & p1908s7_ind_cab$clase == 1)
p1908s7_2_cab <- sum(p1908s7_2_cab$Freq)                                 # 148.370

# Por accidente laboral o enfermedad profesional
p1908s7_3_cab <- p1908s7_ind_cab %>% filter(p1908s7_ind_cab$p1908s7 == 3 & p1908s7_ind_cab$clase == 1)
p1908s7_3_cab <- sum(p1908s7_3_cab$Freq)                                 # 7.219

# Por otro tipo de accidente
p1908s7_4_cab <- p1908s7_ind_cab %>% filter(p1908s7_ind_cab$p1908s7 == 4 & p1908s7_ind_cab$clase == 1)
p1908s7_4_cab <- sum(p1908s7_4_cab$Freq)                                # 20.719

# Por edad avanzada
p1908s7_5_cab <- p1908s7_ind_cab %>% filter(p1908s7_ind_cab$p1908s7 == 5 & p1908s7_ind_cab$clase == 1)
p1908s7_5_cab <- sum(p1908s7_5_cab$Freq)                                # 48.247

# Por el conflicto armado
p1908s7_6_cab <- p1908s7_ind_cab %>% filter(p1908s7_ind_cab$p1908s7 == 6 & p1908s7_ind_cab$clase == 1)
p1908s7_6_cab <- sum(p1908s7_6_cab$Freq)                               # 0

# Por violencia NO asociada al conflicto armado
p1908s7_7_cab <- p1908s7_ind_cab %>% filter(p1908s7_ind_cab$p1908s7 == 7 & p1908s7_ind_cab$clase == 1)
p1908s7_7_cab <- sum(p1908s7_7_cab$Freq)                               # 3.258

# Por otra causa
p1908s7_8_cab <- p1908s7_ind_cab %>% filter(p1908s7_ind_cab$p1908s7 == 8 & p1908s7_ind_cab$clase == 1)
p1908s7_8_cab <- sum(p1908s7_8_cab$Freq)                               # 115.083

# No sabe
p1908s7_9_cab <- p1908s7_ind_cab %>% filter(p1908s7_ind_cab$p1908s7 == 9 & p1908s7_ind_cab$clase == 1)
p1908s7_9_cab <- sum(p1908s7_9_cab$Freq)                               # 36.956

# total 1908s1
p1908s7_total_cab <- p1908s7_1_cab + p1908s7_2_cab + p1908s7_3_cab + p1908s7_4_cab + p1908s7_5_cab + p1908s7_6_cab +
  p1908s7_7_cab + p1908s7_8_cab + p1908s7_9_cab   # 438.806
##############################################################################################################################################################

# 14.8 ¿Esta dificultad (Relacionarse o interactuar con las demás personas?) de ... fue ocasionada:
p1908s8_ind_cab <- data.frame(svytable(~fex_c + p1908s8 + clase,
                                   design = BD_survey))
# Porque nació así
p1908s8_1_cab <- p1908s8_ind_cab %>% filter(p1908s8_ind_cab$p1908s8 == 1 & p1908s8_ind_cab$clase == 1)
p1908s8_1_cab <- sum(p1908s8_1_cab$Freq)                                 # 96.697

# Por enfermedad
p1908s8_2_cab <- p1908s8_ind_cab %>% filter(p1908s8_ind_cab$p1908s8 == 2 & p1908s8_ind_cab$clase == 1)
p1908s8_2_cab <- sum(p1908s8_2_cab$Freq)                                 # 115.511

# Por accidente laboral o enfermedad profesional
p1908s8_3_cab <- p1908s8_ind_cab %>% filter(p1908s8_ind_cab$p1908s8 == 3 & p1908s8_ind_cab$clase == 1)
p1908s8_3_cab <- sum(p1908s8_3_cab$Freq)                                 # 1.812

# Por otro tipo de accidente
p1908s8_4_cab <- p1908s8_ind_cab %>% filter(p1908s8_ind_cab$p1908s8 == 4 & p1908s8_ind_cab$clase == 1)
p1908s8_4_cab <- sum(p1908s8_4_cab$Freq)                                # 3.261

# Por edad avanzada
p1908s8_5_cab <- p1908s8_ind_cab %>% filter(p1908s8_ind_cab$p1908s8 == 5 & p1908s8_ind_cab$clase == 1)
p1908s8_5_cab <- sum(p1908s8_5_cab$Freq)                                # 23.327

# Por el conflicto armado
p1908s8_6_cab <- p1908s8_ind_cab %>% filter(p1908s8_ind_cab$p1908s8 == 6 & p1908s8_ind_cab$clase == 1)
p1908s8_6_cab <- sum(p1908s8_6_cab$Freq)                               # 99

# Por violencia NO asociada al conflicto armado
p1908s8_7_cab <- p1908s8_ind_cab %>% filter(p1908s8_ind_cab$p1908s8 == 7 & p1908s8_ind_cab$clase == 1)
p1908s8_7_cab <- sum(p1908s8_7_cab$Freq)                               # 3.168

# Por otra causa
p1908s8_8_cab <- p1908s8_ind_cab %>% filter(p1908s8_ind_cab$p1908s8 == 8 & p1908s8_ind_cab$clase == 1)
p1908s8_8_cab <- sum(p1908s8_8_cab$Freq)                               # 47.961

# No sabe
p1908s8_9_cab <- p1908s8_ind_cab %>% filter(p1908s8_ind_cab$p1908s8 == 9 & p1908s8_ind_cab$clase == 1)
p1908s8_9_cab <- sum(p1908s8_9_cab$Freq)                               # 21.386

# total 1908s1
p1908s8_total_cab <- p1908s8_1_cab + p1908s8_2_cab + p1908s8_3_cab + p1908s8_4_cab + p1908s8_5_cab + p1908s8_6_cab +
  p1908s8_7_cab + p1908s8_8_cab + p1908s8_9_cab   # 313.225
#########################################################################################################################################################################################

# 15.1 ¿Para estas dificultades ____ utiliza de manera permanente: Gafas, lentes de contacto, lentes intraoculares, programa computacional adaptado, regleta Braille, perro guía, otros?
p1909s1_ind_cab <- data.frame(svytable(~fex_c + p1909s1 + clase,
                                   design = BD_survey))

# Si
p1909s1_1_cab <- p1909s1_ind_cab %>% filter(p1909s1_ind_cab$p1909s1 == 1 & p1909s1_ind_cab$clase == 1)
p1909s1_1_cab <- sum(p1909s1_1_cab$Freq)                                # 1.319.503

# No
p1909s1_2_cab <- p1909s1_ind_cab %>% filter(p1909s1_ind_cab$p1909s1 == 2 & p1909s1_ind_cab$clase == 1)
p1909s1_2_cab <- sum(p1909s1_2_cab$Freq)                                # 1.156.824

# total p1909s1
p1909s1_total_cab <- p1909s1_1_cab + p1909s1_2_cab                        # 2.476.328
###########################################################################################################################################################################################

# 15.2 Bastón, silla de ruedas, muletas, caminador?
p1909s2_ind_cab <- data.frame(svytable(~fex_c + p1909s2 + clase,
                                   design = BD_survey))

# Si
p1909s2_1_cab <- p1909s2_ind_cab %>% filter(p1909s2_ind_cab$p1909s2 == 1 & p1909s2_ind_cab$clase == 1)
p1909s2_1_cab <- sum(p1909s2_1_cab$Freq)                                # 497.756

# No
p1909s2_2_cab <- p1909s2_ind_cab %>% filter(p1909s2_ind_cab$p1909s2 == 2 & p1909s2_ind_cab$clase == 1)
p1909s2_2_cab <- sum(p1909s2_2_cab$Freq)                                # 1.978.572

# total p1909s2
p1909s2_total_cab <- p1909s2_1_cab + p1909s2_2_cab                    # 2.476.328
###########################################################################################################################################################################################

# 15.3 Audífonos medicados, implantes cocleares, otros?
p1909s3_ind_cab <- data.frame(svytable(~fex_c + p1909s3 + clase,
                                   design = BD_survey))

# Si
p1909s3_1_cab <- p1909s3_ind_cab %>% filter(p1909s3_ind_cab$p1909s3 == 1 & p1909s3_ind_cab$clase == 1)
p1909s3_1_cab <- sum(p1909s3_1_cab$Freq)                                # 115.456

# No
p1909s3_2_cab <- p1909s3_ind_cab %>% filter(p1909s3_ind_cab$p1909s3 == 2 & p1909s3_ind_cab$clase == 1)
p1909s3_2_cab <- sum(p1909s3_2_cab$Freq)                                # 2.360.872

# total p1909s3
p1909s3_total_cab <- p1909s3_1_cab + p1909s3_2_cab                    # 2.476.328
###########################################################################################################################################################################################

# 15.4 Ayuda de otras personas?
p1909s4_ind_cab <- data.frame(svytable(~fex_c + p1909s4 + clase,
                                   design = BD_survey))

# Si
p1909s4_1_cab <- p1909s4_ind_cab %>% filter(p1909s4_ind_cab$p1909s4 == 1 & p1909s4_ind_cab$clase == 1)
p1909s4_1_cab <- sum(p1909s4_1_cab$Freq)                                # 658.627

# No
p1909s4_2_cab <- p1909s4_ind_cab %>% filter(p1909s4_ind_cab$p1909s4 == 2 & p1909s4_ind_cab$clase == 1)
p1909s4_2_cab <- sum(p1909s4_2_cab$Freq)                                # 1.817.701

# total p1909s4
p1909s4_total_cab <- p1909s4_1_cab + p1909s4_2_cab                    # 2.476.328
#####################################################################################################################################################################

# 15.5 Medicamentos o terapias?
p1909s5_ind_cab <- data.frame(svytable(~fex_c + p1909s5 + clase,
                                   design = BD_survey))

# Si
p1909s5_1_cab <- p1909s5_ind_cab %>% filter(p1909s5_ind_cab$p1909s5 == 1 & p1909s5_ind_cab$clase == 1)
p1909s5_1_cab <- sum(p1909s5_1_cab$Freq)                                # 696.613

# No
p1909s5_2_cab <- p1909s5_ind_cab %>% filter(p1909s5_ind_cab$p1909s5 == 2 & p1909s5_ind_cab$clase == 1)
p1909s5_2_cab <- sum(p1909s5_2_cab$Freq)                                # 1.779.715

# total p1909s5
p1909s5_total_cab <- p1909s5_1_cab + p1909s5_2_cab                    # 2.476.328
############################################################################################################################################################################

# 15.6 Prácticas de medicina ancestral?
p1909s6_ind_cab <- data.frame(svytable(~fex_c + p1909s6 + clase,
                                   design = BD_survey))

# Si
p1909s6_1_cab <- p1909s6_ind_cab %>% filter(p1909s6_ind_cab$p1909s6 == 1 & p1909s6_ind_cab$clase == 1)
p1909s6_1_cab <- sum(p1909s6_1_cab$Freq)                                # 14.842

# No
p1909s6_2_cab <- p1909s6_ind_cab %>% filter(p1909s6_ind_cab$p1909s6 == 2 & p1909s6_ind_cab$clase == 1)
p1909s6_2_cab <- sum(p1909s6_2_cab$Freq)                                # 2.461.485

# total p1909s6
p1909s6_total_cab <- p1909s6_1_cab + p1909s6_2_cab                    # 2.476.328
#############################################################################################################################################################################

# 16. ¿Quién se ocupa principalmente del cuidado de ...?
p6126_ind_cab <- data.frame(svytable(~fex_c + p6126 + clase,
                                 design = BD_survey))
# Una persona del hogar
p6126_1_cab <- p6126_ind_cab %>% filter(p6126_ind_cab$p6126 == 1 & p6126_ind_cab$clase == 1)
p6126_1_cab <- sum(p6126_1_cab$Freq)                            # 999.354

# Una persona de otro hogar no remunerada
p6126_2_cab <- p6126_ind_cab %>% filter(p6126_ind_cab$p6126 == 2 & p6126_ind_cab$clase == 1)
p6126_2_cab <- sum(p6126_2_cab$Freq)                            # 122.006

# Una persona de otro hogar remunerada
p6126_3_cab <- p6126_ind_cab %>% filter(p6126_ind_cab$p6126 == 3 & p6126_ind_cab$clase == 1)
p6126_3_cab <- sum(p6126_3_cab$Freq)                            # 49.717

# Permanece solo/a
p6126_4_cab <- p6126_ind_cab %>% filter(p6126_ind_cab$p6126 == 4 & p6126_ind_cab$clase == 1)
p6126_4_cab <- sum(p6126_4_cab$Freq)                            # 110.974

# No requiere cuidado
p6126_5_cab <- p6126_ind_cab %>% filter(p6126_ind_cab$p6126 == 5 & p6126_ind_cab$clase == 1)
p6126_5_cab <- sum(p6126_5_cab$Freq)                            # 1.194.275

# total p6126
p6126_total_cab <- p6126_1_cab + p6126_2_cab + p6126_3_cab + p6126_4_cab + p6126_5_cab  # 2.476.328
##############################################################################################################################################################################

# 16.1 Numero Orden
p6126s1_ind_cab <- data.frame(svytable(~fex_c + p6126s1 + clase,
                                   design = BD_survey))
p6126s1_1_cab <- p6126s1_ind_cab %>% filter(p6126s1_ind_cab$p6126s1 == 1 & p6126s1_ind_cab$clase == 1)
p6126s1_1_cab <- sum(p6126s1_1_cab$Freq)                                  # 462.510

p6126s1_2_cab <- p6126s1_ind_cab %>% filter(p6126s1_ind_cab$p6126s1 == 2 & p6126s1_ind_cab$clase == 1)
p6126s1_2_cab <- sum(p6126s1_2_cab$Freq)                                  # 395.417

p6126s1_3_cab <- p6126s1_ind_cab %>% filter(p6126s1_ind_cab$p6126s1 == 3 & p6126s1_ind_cab$clase == 1)
p6126s1_3_cab <- sum(p6126s1_3_cab$Freq)                                  # 84.945

p6126s1_4_cab <- p6126s1_ind_cab %>% filter(p6126s1_ind_cab$p6126s1 == 4 & p6126s1_ind_cab$clase == 1)
p6126s1_4_cab <- sum(p6126s1_4_cab$Freq)                                  # 27.988

p6126s1_5_cab <- p6126s1_ind_cab %>% filter(p6126s1_ind_cab$p6126s1 == 5 & p6126s1_ind_cab$clase == 1)
p6126s1_5_cab <- sum(p6126s1_5_cab$Freq)                                  # 16.377

p6126s1_6_cab <- p6126s1_ind_cab %>% filter(p6126s1_ind_cab$p6126s1 == 6 & p6126s1_ind_cab$clase == 1)
p6126s1_6_cab <- sum(p6126s1_6_cab$Freq)                                  # 7.876

p6126s1_7_cab <- p6126s1_ind_cab %>% filter(p6126s1_ind_cab$p6126s1 == 7 & p6126s1_ind_cab$clase == 1)
p6126s1_7_cab <- sum(p6126s1_7_cab$Freq)                                  # 2.690

p6126s1_8_cab <- p6126s1_ind_cab %>% filter(p6126s1_ind_cab$p6126s1 == 8 & p6126s1_ind_cab$clase == 1)
p6126s1_8_cab <- sum(p6126s1_8_cab$Freq)                                  # 49

p6126s1_9_cab <- p6126s1_ind_cab %>% filter(p6126s1_ind_cab$p6126s1 == 9 & p6126s1_ind_cab$clase == 1)
p6126s1_9_cab <- sum(p6126s1_9_cab$Freq)                                  # 1.072

p6126s1_11_cab <- p6126s1_ind_cab %>% filter(p6126s1_ind_cab$p6126s1 == 11 & p6126s1_ind_cab$clase == 1)   
p6126s1_11_cab <- sum(p6126s1_11_cab$Freq)                                # 2,65

p6126s1_13_cab <- p6126s1_ind_cab %>% filter(p6126s1_ind_cab$p6126s1 == 13 & p6126s1_ind_cab$clase == 1)   
p6126s1_13_cab <- sum(p6126s1_13_cab$Freq)                                # 208

p6126s1_total_cab <- p6126s1_1_cab + p6126s1_2_cab + p6126s1_3_cab + p6126s1_4_cab + p6126s1_5_cab + p6126s1_6_cab +
  p6126s1_7_cab + p6126s1_8_cab + p6126s1_9_cab + p6126s1_11_cab + p6126s1_13_cab  # 999.139

#############################################################################################################################################################################
# 16.2 Sexo
p6126s2_ind_cab <- data.frame(svytable(~fex_c + p6126s2 + clase,
                                   design = BD_survey))
# Hombre
p6126s2_1_cab <- p6126s2_ind_cab %>% filter(p6126s2_ind_cab$p6126s2 == 1 & p6126s2_ind_cab$clase == 1)
p6126s2_1_cab <- sum(p6126s2_1_cab$Freq)                                  # 19.958

# Mujer
p6126s2_2_cab <- p6126s2_ind_cab %>% filter(p6126s2_ind_cab$p6126s2 == 2 & p6126s2_ind_cab$clase == 1)
p6126s2_2_cab <- sum(p6126s2_2_cab$Freq)                                 # 151.766

# total p6126s2
p6126s2_total_cab <- p6126s2_1_cab + p6126s2_2_cab  # 171.724
#############################################################################################################################################################################
# 16.3 ¿Esta persona tuvo que dejar de trabajar para dedicarse al cuidado de...?
p6126s3_ind_cab <- data.frame(svytable(~fex_c + p6126s3 + clase,
                                   design = BD_survey))

# Si 
p6126s3_1_cab <- p6126s3_ind_cab %>% filter(p6126s3_ind_cab$p6126s3 == 1 & p6126s3_ind_cab$clase == 1)
p6126s3_1_cab <- sum(p6126s3_1_cab$Freq)                               # 320.570

# No
p6126s3_2_cab <- p6126s3_ind_cab %>% filter(p6126s3_ind_cab$p6126s3 == 2 & p6126s3_ind_cab$clase == 1)
p6126s3_2_cab <- sum(p6126s3_2_cab$Freq)                               # 678.784

# total p6126s3
p6126s3_total_cab <- p6126s3_1_cab + p6126s3_2_cab  # 999.354
##############################################################################################################################################################################
# 17. En los últimos 30 días, ... ¿tuvo alguna enfermedad, accidente , problema odontológico o algún otro problema de salud que no haya implicado hospitalización?
p5665_ind_cab <- data.frame(svytable(~fex_c + p5665 + clase,
                                 design = BD_survey))

# Si
p5665_1_cab <- p5665_ind_cab %>% filter(p5665_ind_cab$p5665 == 1 & p5665_ind_cab$clase == 1)
p5665_1_cab <- sum(p5665_1_cab$Freq)                            # 1.440.555

# No
p5665_2_cab <- p5665_ind_cab %>% filter(p5665_ind_cab$p5665 == 2 & p5665_ind_cab$clase == 1)
p5665_2_cab <- sum(p5665_2_cab$Freq)                           # 37.628.049

# total p5665
p5665_total_cab <- p5665_1_cab + p5665_2_cab  # 39.068.605
###############################################################################################################################################################################
# 18. Por ese problema de salud, ¿durante cuántos días en total dejó ... de realizar sus actividades normales? Número de dias
p6134_ind_cab <- data.frame(svytable(~fex_c + p6134 + clase,
                                 design = BD_survey))
# 0 dias
p6134_0_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 0 & p6134_ind_cab$clase == 1)     
p6134_0_cab <- sum(p6134_0_cab$Freq)
# 1 dias
p6134_1_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 1 & p6134_ind_cab$clase == 1)     
p6134_1_cab <- sum(p6134_1_cab$Freq)
# 2 dias
p6134_2_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 2 & p6134_ind_cab$clase == 1)     
p6134_2_cab <- sum(p6134_2_cab$Freq)
# 3 dias
p6134_3_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 3 & p6134_ind_cab$clase == 1)     
p6134_3_cab <- sum(p6134_3_cab$Freq)
# 4 dias
p6134_4_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 4 & p6134_ind_cab$clase == 1)     
p6134_4_cab <- sum(p6134_4_cab$Freq)
# 5 dias
p6134_5_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 5 & p6134_ind_cab$clase == 1)     
p6134_5_cab <- sum(p6134_5_cab$Freq)
# 6 dias
p6134_6_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 6 & p6134_ind_cab$clase == 1)     
p6134_6_cab <- sum(p6134_6_cab$Freq)
# 7 dias
p6134_7_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 7 & p6134_ind_cab$clase == 1)     
p6134_7_cab <- sum(p6134_7_cab$Freq)
# 8 dias
p6134_8_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 8 & p6134_ind_cab$clase == 1)     
p6134_8_cab <- sum(p6134_8_cab$Freq)
# 9 dias
p6134_9_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 9 & p6134_ind_cab$clase == 1)     
p6134_9_cab <- sum(p6134_9_cab$Freq)
# 10 dias
p6134_10_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 10 & p6134_ind_cab$clase == 1)     
p6134_10_cab <- sum(p6134_10_cab$Freq)
# 11 dias
p6134_11_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 11 & p6134_ind_cab$clase == 1)     
p6134_11_cab <- sum(p6134_11_cab$Freq)
# 12 dias
p6134_12_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 12 & p6134_ind_cab$clase == 1)     
p6134_12_cab <- sum(p6134_12_cab$Freq)
# 13 dias
p6134_13_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 13 & p6134_ind_cab$clase == 1)     
p6134_13_cab <- sum(p6134_13_cab$Freq)
# 14 dias
p6134_14_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 14 & p6134_ind_cab$clase == 1)     
p6134_14_cab <- sum(p6134_14_cab$Freq)
# 15 dias
p6134_15_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 15 & p6134_ind_cab$clase == 1)     
p6134_15_cab <- sum(p6134_15_cab$Freq)
# 16 dias
p6134_16_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 16 & p6134_ind_cab$clase == 1)     
p6134_16_cab <- sum(p6134_16_cab$Freq)
# 17 dias
p6134_17_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 17 & p6134_ind_cab$clase == 1)     
p6134_17_cab <- sum(p6134_17_cab$Freq)
# 18 dias
p6134_18_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 18 & p6134_ind_cab$clase == 1)     
p6134_18_cab <- sum(p6134_18_cab$Freq)
# 19 dias
p6134_19_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 19 & p6134_ind_cab$clase == 1)     
p6134_19_cab <- sum(p6134_19_cab$Freq)
# 20 dias
p6134_20_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 20 & p6134_ind_cab$clase == 1)     
p6134_20_cab <- sum(p6134_20_cab$Freq)
# 21 dias
p6134_21_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 21 & p6134_ind_cab$clase == 1)     
p6134_21_cab <- sum(p6134_21_cab$Freq)
# 22 dias
p6134_22_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 22 & p6134_ind_cab$clase == 1)     
p6134_22_cab <- sum(p6134_22_cab$Freq)
# 23 dias
p6134_23_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 23 & p6134_ind_cab$clase == 1)     
p6134_23_cab <- sum(p6134_23_cab$Freq)
# 24 dias
p6134_24_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 24 & p6134_ind_cab$clase == 1)     
p6134_24_cab <- sum(p6134_24_cab$Freq)
# 25 dias
p6134_25_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 25 & p6134_ind_cab$clase == 1)     
p6134_25_cab <- sum(p6134_25_cab$Freq)
# 26 dias
p6134_26_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 26 & p6134_ind_cab$clase == 1)     
p6134_26_cab <- sum(p6134_26_cab$Freq)
# 27 dias
p6134_27_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 27 & p6134_ind_cab$clase == 1)     
p6134_27_cab <- sum(p6134_27_cab$Freq)
# 28 dias
p6134_28_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 28 & p6134_ind_cab$clase == 1)     
p6134_28_cab <- sum(p6134_28_cab$Freq)
#29 dias
p6134_29_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 29 & p6134_ind_cab$clase == 1)     
p6134_29_cab <- sum(p6134_29_cab$Freq)
# 30 dias
p6134_30_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 30 & p6134_ind_cab$clase == 1)     
p6134_30_cab <- sum(p6134_30_cab$Freq)
# 31 dias
p6134_31_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 31 & p6134_ind_cab$clase == 1)     
p6134_31_cab <- sum(p6134_31_cab$Freq)
# 35 dias
p6134_35_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 35 & p6134_ind_cab$clase == 1)     
p6134_35_cab <- sum(p6134_35_cab$Freq)
# 36 dias
p6134_36_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 36 & p6134_ind_cab$clase == 1)     
p6134_36_cab <- sum(p6134_36_cab$Freq)
# 37 dias
p6134_37_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 37 & p6134_ind_cab$clase == 1)     
p6134_37_cab <- sum(p6134_37_cab$Freq)
# 38 dias
p6134_38_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 38 & p6134_ind_cab$clase == 1)     
p6134_38_cab <- sum(p6134_38_cab$Freq)
# 40 dias
p6134_40_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 40 & p6134_ind_cab$clase == 1)     
p6134_40_cab <- sum(p6134_40_cab$Freq)
# 45 dias
p6134_45_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 45 & p6134_ind_cab$clase == 1)     
p6134_45_cab <- sum(p6134_45_cab$Freq)
# 50 dias
p6134_50_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 50 & p6134_ind_cab$clase == 1)     
p6134_50_cab <- sum(p6134_50_cab$Freq)
# 60 dias
p6134_60_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 60 & p6134_ind_cab$clase == 1)     
p6134_60_cab <- sum(p6134_60_cab$Freq)
# 64 dias
p6134_64_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 64 & p6134_ind_cab$clase == 1)     
p6134_64_cab <- sum(p6134_64_cab$Freq)
# 68 dias
p6134_68_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 68 & p6134_ind_cab$clase == 1)     
p6134_68_cab <- sum(p6134_68_cab$Freq)
# 72 dias
p6134_72_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 72 & p6134_ind_cab$clase == 1)     
p6134_72_cab <- sum(p6134_72_cab$Freq)
# 75 dias
p6134_75_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 75 & p6134_ind_cab$clase == 1)     
p6134_75_cab <- sum(p6134_75_cab$Freq)
# 80 dias
p6134_80_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 80 & p6134_ind_cab$clase == 1)     
p6134_80_cab <- sum(p6134_80_cab$Freq)
# 90 dias
p6134_90_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 90 & p6134_ind_cab$clase == 1)     
p6134_90_cab <- sum(p6134_90_cab$Freq)
# 98 dias
p6134_98_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 98 & p6134_ind_cab$clase == 1)     
p6134_98_cab <- sum(p6134_98_cab$Freq)
# 99 dias
p6134_99_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 99 & p6134_ind_cab$clase == 1)     
p6134_99_cab <- sum(p6134_99_cab$Freq)
# 100 dias
p6134_100_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 100 & p6134_ind_cab$clase == 1)     
p6134_100_cab <- sum(p6134_100_cab$Freq)
# 120 dias
p6134_120_cab <- p6134_ind_cab %>% filter(p6134_ind_cab$p6134 == 120 & p6134_ind_cab$clase == 1)     
p6134_120_cab <- sum(p6134_120_cab$Freq)

# p6134 total
p6134_total_cab <- p6134_0_cab+p6134_1_cab+p6134_2_cab+p6134_3_cab+p6134_4_cab+p6134_5_cab+p6134_6_cab+p6134_7_cab+
  p6134_8_cab+p6134_9_cab+p6134_10_cab+p6134_11_cab+p6134_12_cab+p6134_13_cab+p6134_14_cab+p6134_15_cab+p6134_16_cab+
  p6134_17_cab+p6134_18_cab+p6134_19_cab+p6134_20_cab+p6134_21_cab+p6134_22_cab+p6134_23_cab+p6134_24_cab+p6134_25_cab+
  p6134_26_cab+p6134_27_cab+p6134_28_cab+p6134_29_cab+p6134_30_cab+p6134_31_cab+p6134_35_cab+p6134_36_cab+p6134_37_cab+
  p6134_38_cab+p6134_40_cab+p6134_45_cab+p6134_50_cab+p6134_60_cab+p6134_64_cab+p6134_68_cab+p6134_72_cab+p6134_75_cab+
  p6134_80_cab+p6134_90_cab+p6134_98_cab+p6134_99_cab+p6134_100_cab+p6134_120_cab
# 1.440.555
#################################################################################################################################################################
# 19. Para tratar ese problema de salud, ¿que hizo principalmente ...?:
p8563_ind_cab <- data.frame(svytable(~fex_c + p8563 + clase, 
                                 design = BD_survey))
# Acudió a Ia entidad de seguridad social en salud de la cual es afiliado/a
p8563_1_cab <- p8563_ind_cab %>% filter(p8563_ind_cab$p8563 == 1 & p8563_ind_cab$clase == 1)     # 940.480
p8563_1_cab <- sum(p8563_1_cab$Freq)
# Acudió de forma particular  a un médico general, especialista, odontólogo, terapeuta o profesional de la salud
p8563_2_cab <- p8563_ind_cab %>% filter(p8563_ind_cab$p8563 == 2 & p8563_ind_cab$clase == 1)     # 163.148
p8563_2_cab <- sum(p8563_2_cab$Freq)
# Acudió a un boticario, farmaceuta, droguista
p8563_3_cab <- p8563_ind_cab %>% filter(p8563_ind_cab$p8563 == 3 & p8563_ind_cab$clase == 1)     # 61.190
p8563_3_cab <- sum(p8563_3_cab$Freq)
# Consultó a un empírico, curandero, yerbatero, comadrona
p8563_4_cab <- p8563_ind_cab %>% filter(p8563_ind_cab$p8563 == 4 & p8563_ind_cab$clase == 1)     # 1.373
p8563_4_cab <- sum(p8563_4_cab$Freq)
# Asistió a terapias alternativas (acupuntura, esencias florales, musicoterapias, homeópata etc.)
p8563_5_cab <- p8563_ind_cab %>% filter(p8563_ind_cab$p8563 == 5 & p8563_ind_cab$clase == 1)     # 2.362
p8563_5_cab <- sum(p8563_5_cab$Freq)
# Usó remedios caseros 
p8563_6_cab <- p8563_ind_cab %>% filter(p8563_ind_cab$p8563 == 6 & p8563_ind_cab$clase == 1)     # 128.047
p8563_6_cab <- sum(p8563_6_cab$Freq)
# Se autorecetó
p8563_7_cab <- p8563_ind_cab %>% filter(p8563_ind_cab$p8563 == 7 & p8563_ind_cab$clase == 1)     # 95.876
p8563_7_cab <- sum(p8563_7_cab$Freq)
# Nada
p8563_8_cab <- p8563_ind_cab %>% filter(p8563_ind_cab$p8563 == 8 & p8563_ind_cab$clase == 1)     # 48.076
p8563_8_cab <- sum(p8563_8_cab$Freq)

# total p8563
p8563_total_cab <- p8563_1_cab+p8563_2_cab+p8563_3_cab+p8563_4_cab+p8563_5_cab+p8563_6_cab+p8563_7_cab+p8563_8_cab     # 1.440.555
################################################################################################################################################################################
# 20. ¿Acudió al servicio de urgencias en la institución prestadora de servicios (hospital o clínica) pública o privada?
p1092_ind_cab <- data.frame(svytable(~fex_c + p1092 + clase,
                                 design = BD_survey))
# Si
p1092_1_cab <- p1092_ind_cab %>% filter(p1092_ind_cab$p1092 == 1 & p1092_ind_cab$clase == 1)  # 554.266
p1092_1_cab <- sum(p1092_1_cab$Freq)
#No
p1092_2_cab <- p1092_ind_cab %>% filter(p1092_ind_cab$p1092 == 2 & p1092_ind_cab$clase == 1)  # 386.214
p1092_2_cab <- sum(p1092_2_cab$Freq)

# total p1092
p1092_total_cab <- p1092_1_cab + p1092_2_cab  # 940.480
###################################################################################################################################################################################
# 21. ¿A ... le brindaron asistencia médica en el servicio de urgencias para solucionar el problema de salud?
p8573_ind_cab <- data.frame(svytable(~fex_c + p8573 + clase,
                                 design = BD_survey))
# Si
p8573_1_cab <- p8573_ind_cab %>% filter(p8573_ind_cab$p8573 == 1 & p8573_ind_cab$clase == 1)  # 532.546
p8573_1_cab <- sum(p8573_1_cab$Freq)
#No
p8573_2_cab <- p8573_ind_cab %>% filter(p8573_ind_cab$p8573 == 2 & p8573_ind_cab$clase == 1)  # 21.719
p8573_2_cab <- sum(p8573_2_cab$Freq)

# total p8573 
p8573_total_cab <- p8573_1_cab + p8573_2_cab  # 554.266
##############################################################################################################################################################################3
# 22. ¿Cuál fue la razón principal por la que ... no recibió atención médica en el servicio de urgencias?
p8575_ind_cab <- data.frame(svytable(~fex_c + p8575 + clase,
                                 design = BD_survey))
# El caso era leve
p8575_1_cab <- p8575_ind_cab %>% filter(p8575_ind_cab$p8575 == 1 & p8575_ind_cab$clase == 1)  # 12.412
p8575_1_cab <- sum(p8575_1_cab$Freq)
# Esperó demasiado tiempo y no lo/la atendieron
p8575_2_cab <- p8575_ind_cab %>% filter(p8575_ind_cab$p8575 == 2 & p8575_ind_cab$clase == 1) # 2.098
p8575_2_cab <- sum(p8575_2_cab$Freq)
# Indicaron que allí no lo/la podían atender porque no estaba afiliado/a a alguna entidad que tuviera contrato con ellos
p8575_3_cab <- p8575_ind_cab %>% filter(p8575_ind_cab$p8575 == 3 & p8575_ind_cab$clase == 1)  # 960
p8575_3_cab <- sum(p8575_3_cab$Freq)
# No tenía identificación y por eso lo/la rechazaron
p8575_4_cab <- p8575_ind_cab %>% filter(p8575_ind_cab$p8575 == 4 & p8575_ind_cab$clase == 1)  # 196
p8575_4_cab <- sum(p8575_4_cab$Freq)
# Indicaron que debían remitirlo/la a otra institución prestadora de servicios que tuviera los servicios requeridos para atenderlo/la
p8575_5_cab <- p8575_ind_cab %>% filter(p8575_ind_cab$p8575 == 5 & p8575_ind_cab$clase == 1)  # 3.764
p8575_5_cab <- sum(p8575_5_cab$Freq)
# No le dieron informacion
p8575_6_cab <- p8575_ind_cab %>% filter(p8575_ind_cab$p8575 == 6 & p8575_ind_cab$clase == 1) # 1.993
p8575_6_cab <- sum(p8575_6_cab$Freq)
# No sabe/no responde
p8575_9_cab <- p8575_ind_cab %>% filter(p8575_ind_cab$p8575 == 9 & p8575_ind_cab$clase == 1)  # 293
p8575_9_cab <- sum(p8575_9_cab$Freq)

# total p8575
p8575_total_cab <- p8575_1_cab+p8575_2_cab+p8575_3_cab+p8575_4_cab+p8575_5_cab+p8575_6_cab+p8575_9_cab  # 21.719
##################################################################################################################################################################
# 23. ¿Cuánto tiempo transcurrió entre el momento de llegar al servicio de urgencias y el momento de ser atendido/a por personal médico?
p8577_ind_cab <- data.frame(svytable(~fex_c + p8577 + clase,
                                 design = BD_survey))
# Lo atendieron inmediatamente
p8577_1_cab <- p8577_ind_cab %>% filter(p8577_ind_cab$p8577 == 1 & p8577_ind_cab$clase == 1)   # 178.222
p8577_1_cab <- sum(p8577_1_cab$Freq)
# En maximo 30 minutos
p8577_2_cab <- p8577_ind_cab %>% filter(p8577_ind_cab$p8577 == 2 & p8577_ind_cab$clase == 1)   # 126.125
p8577_2_cab <- sum(p8577_2_cab$Freq)
# Entre 31 minutos y una hora
p8577_3_cab <- p8577_ind_cab %>% filter(p8577_ind_cab$p8577 == 3 & p8577_ind_cab$clase == 1)   # 68.603
p8577_3_cab <- sum(p8577_3_cab$Freq)
# Mas de una hora hasta dos horas
p8577_4_cab <- p8577_ind_cab %>% filter(p8577_ind_cab$p8577 == 4 & p8577_ind_cab$clase == 1)   # 63.237
p8577_4_cab <- sum(p8577_4_cab$Freq)
# Mas de dos horas
p8577_5_cab <- p8577_ind_cab %>% filter(p8577_ind_cab$p8577 == 5 & p8577_ind_cab$clase == 1)   # 96.357
p8577_5_cab <- sum(p8577_5_cab$Freq)

# total p8577
p8577_total_cab <- p8577_1_cab+p8577_2_cab+p8577_3_cab+p8577_4_cab+p8577_5_cab   # 532.546
####################################################################################################################################################################
# 24. En el servicio de urgencias fue atendido/a por:
p770_ind_cab <- data.frame(svytable(~fex_c + p770 + clase,
                                design = BD_survey))
# Medico/a general
p770_1_cab <- p770_ind_cab %>% filter(p770_ind_cab$p770 == 1 & p770_ind_cab$clase == 1)  # 437.246
p770_1_cab <- sum(p770_1_cab$Freq)
# Odontologo/a
p770_2_cab <- p770_ind_cab %>% filter(p770_ind_cab$p770 == 2 & p770_ind_cab$clase == 1)  # 25.132
p770_2_cab <- sum(p770_2_cab$Freq)
# Especialista
p770_3_cab <- p770_ind_cab %>% filter(p770_ind_cab$p770 == 3 & p770_ind_cab$clase == 1)  # 70.167
p770_3_cab <- sum(p770_3_cab$Freq)

# total p770
p770_total_cab <- p770_1_cab+p770_2_cab+p770_3_cab  # 532.546
#####################################################################################################################################################################
# 25. ¿Cuál fue la razón principal por la que ... no solicitó o no recibió atención médica?
p6153_ind_cab <- data.frame(svytable(~fex_c + p6153 + clase,
                                 design = BD_survey))
# El caso era leve
p6153_1_cab <- p6153_ind_cab %>% filter(p6153_ind_cab$p6153 == 1 & p6153_ind_cab$clase == 1)  # 213.904
p6153_1_cab <- sum(p6153_1_cab$Freq)
# No tuvo tiempo
p6153_2_cab <- p6153_ind_cab %>% filter(p6153_ind_cab$p6153 == 2 & p6153_ind_cab$clase == 1)  # 15.681
p6153_2_cab <- sum(p6153_2_cab$Freq)
# El centro de atención queda lejos
p6153_3_cab <- p6153_ind_cab %>% filter(p6153_ind_cab$p6153 == 3 & p6153_ind_cab$clase == 1)  # 1.079
p6153_3_cab <- sum(p6153_3_cab$Freq)
# Falta de dinero
p6153_4_cab <- p6153_ind_cab %>% filter(p6153_ind_cab$p6153 == 4 & p6153_ind_cab$clase == 1)  # 17.186
p6153_4_cab <- sum(p6153_4_cab$Freq)
# Mal servicio o cita distanciada en el tiempo
p6153_5_cab <- p6153_ind_cab %>% filter(p6153_ind_cab$p6153 == 5 & p6153_ind_cab$clase == 1)  # 17.851
p6153_5_cab <- sum(p6153_5_cab$Freq)
# No lo/la atendieron
p6153_6_cab <- p6153_ind_cab %>% filter(p6153_ind_cab$p6153 == 6 & p6153_ind_cab$clase == 1)  # 6.884
p6153_6_cab <- sum(p6153_6_cab$Freq)
# No confia en los medicos
p6153_7_cab <- p6153_ind_cab %>% filter(p6153_ind_cab$p6153 == 7 & p6153_ind_cab$clase == 1)  # 8.031
p6153_7_cab <- sum(p6153_7_cab$Freq)
# Consulto antes y no le resolvieron el problema
p6153_8_cab <- p6153_ind_cab %>% filter(p6153_ind_cab$p6153 == 8 & p6153_ind_cab$clase == 1)  # 5.021
p6153_8_cab <- sum(p6153_8_cab$Freq)
# Muchos tramites para la cita
p6153_9_cab <- p6153_ind_cab %>% filter(p6153_ind_cab$p6153 == 9 & p6153_ind_cab$clase == 1)  # 13.368
p6153_9_cab <- sum(p6153_9_cab$Freq)
# No le cubrian o no le autorizaron la atencion
p6153_10_cab <- p6153_ind_cab %>% filter(p6153_ind_cab$p6153 == 10 & p6153_ind_cab$clase == 1)  # 4.370
p6153_10_cab <- sum(p6153_10_cab$Freq)
# Le hacen esperar mucho para atenderlo/la
p6153_11_cab <- p6153_ind_cab %>% filter(p6153_ind_cab$p6153 == 11 & p6153_ind_cab$clase == 1)  # 11.576
p6153_11_cab <- sum(p6153_11_cab$Freq)
# Dificultad para viajar
p6153_12_cab <- p6153_ind_cab %>% filter(p6153_ind_cab$p6153 == 12 & p6153_ind_cab$clase == 1)  # 776
p6153_12_cab <- sum(p6153_12_cab$Freq)
# otro
p6153_13_cab <- p6153_ind_cab %>% filter(p6153_ind_cab$p6153 == 13 & p6153_ind_cab$clase == 1)  # 21.194
p6153_13_cab <- sum(p6153_13_cab$Freq)

# total p6153
p6153_total_cab <- p6153_1_cab+p6153_2_cab+p6153_3_cab+p6153_4_cab+p6153_5_cab+p6153_6_cab+p6153_7_cab+p6153_8_cab+
  p6153_9_cab+p6153_10_cab+p6153_11_cab+p6153_12_cab+p6153_13_cab  # 336.926
###############################################################################################################################################################
# 26. ¿Cuántos días transcurrieron entre el momento de pedir la cita y el momento de la consulta con el medico/a general u odontólogo/a?
p6199_ind_cab <- data.frame(svytable(~fex_c + p6199 + clase,
                                 design = BD_survey))
# Medico/a general
p6199_1_cab <- p6199_ind_cab %>% filter(p6199_ind_cab$p6199 == 1 & p6199_ind_cab$clase == 1)  # 369.281
p6199_1_cab <- sum(p6199_1_cab$Freq)
# Odontologo/a
p6199_2_cab <- p6199_ind_cab %>% filter(p6199_ind_cab$p6199 == 2 & p6199_ind_cab$clase == 1)  # 95.334
p6199_2_cab <- sum(p6199_2_cab$Freq)
# Acudio directo al especialista
p6199_3_cab <- p6199_ind_cab %>% filter(p6199_ind_cab$p6199 == 3 & p6199_ind_cab$clase == 1)  # 84.746
p6199_3_cab <- sum(p6199_3_cab$Freq)

# total p6199 
p6199_total_cab <- p6199_1_cab+p6199_2_cab+p6199_3_cab  # 549.362
################################################################################################################################################################
# 26.1 Numero de dias
p6199s1_ind_cab <- data.frame(svytable(~fex_c + p6199s1 + clase,
                                   design = BD_survey))
# 0 dias
p6199s1_0_cab <- p6199s1_ind_cab %>% filter(p6199s1_ind_cab$p6199s1 == 0 & p6199s1_ind_cab$clase == 1)  
p6199s1_0_cab <- sum(p6199s1_0_cab$Freq)
# 1 dias
p6199s1_1_cab <- p6199s1_ind_cab %>% filter(p6199s1_ind_cab$p6199s1 == 1 & p6199s1_ind_cab$clase == 1)  
p6199s1_1_cab <- sum(p6199s1_1_cab$Freq)
# 2 dias
p6199s1_2_cab <- p6199s1_ind_cab %>% filter(p6199s1_ind_cab$p6199s1 == 2 & p6199s1_ind_cab$clase == 1)  
p6199s1_2_cab <- sum(p6199s1_2_cab$Freq)
# 3 dias
p6199s1_3_cab <- p6199s1_ind_cab %>% filter(p6199s1_ind_cab$p6199s1 == 3 & p6199s1_ind_cab$clase == 1)  
p6199s1_3_cab <- sum(p6199s1_3_cab$Freq)
# 4 dias
p6199s1_4_cab <- p6199s1_ind_cab %>% filter(p6199s1_ind_cab$p6199s1 == 4 & p6199s1_ind_cab$clase == 1)  
p6199s1_4_cab <- sum(p6199s1_4_cab$Freq)
# 5 dias
p6199s1_5_cab <- p6199s1_ind_cab %>% filter(p6199s1_ind_cab$p6199s1 == 5 & p6199s1_ind_cab$clase == 1)  
p6199s1_5_cab <- sum(p6199s1_5_cab$Freq)
# 6 dias
p6199s1_6_cab <- p6199s1_ind_cab %>% filter(p6199s1_ind_cab$p6199s1 == 6 & p6199s1_ind_cab$clase == 1)  
p6199s1_6_cab <- sum(p6199s1_6_cab$Freq)
# 7 dias
p6199s1_7_cab <- p6199s1_ind_cab %>% filter(p6199s1_ind_cab$p6199s1 == 7 & p6199s1_ind_cab$clase == 1)  
p6199s1_7_cab <- sum(p6199s1_7_cab$Freq)
# 8 dias
p6199s1_8_cab <- p6199s1_ind_cab %>% filter(p6199s1_ind_cab$p6199s1 == 8 & p6199s1_ind_cab$clase == 1)  
p6199s1_8_cab <- sum(p6199s1_8_cab$Freq)
# 9 dias
p6199s1_9_cab <- p6199s1_ind_cab %>% filter(p6199s1_ind_cab$p6199s1 == 9 & p6199s1_ind_cab$clase == 1)  
p6199s1_9_cab <- sum(p6199s1_9_cab$Freq)
# 10 dias
p6199s1_10_cab <- p6199s1_ind_cab %>% filter(p6199s1_ind_cab$p6199s1 == 10 & p6199s1_ind_cab$clase == 1)  
p6199s1_10_cab <- sum(p6199s1_10_cab$Freq)
# 12 dias
p6199s1_12_cab <- p6199s1_ind_cab %>% filter(p6199s1_ind_cab$p6199s1 == 12 & p6199s1_ind_cab$clase == 1)  
p6199s1_12_cab <- sum(p6199s1_12_cab$Freq)
# 14 dias
p6199s1_14_cab <- p6199s1_ind_cab %>% filter(p6199s1_ind_cab$p6199s1 == 14 & p6199s1_ind_cab$clase == 1)  
p6199s1_14_cab <- sum(p6199s1_14_cab$Freq)
# 15 dias
p6199s1_15_cab <- p6199s1_ind_cab %>% filter(p6199s1_ind_cab$p6199s1 == 15 & p6199s1_ind_cab$clase == 1)  
p6199s1_15_cab <- sum(p6199s1_15_cab$Freq)
# 20 dias
p6199s1_20_cab <- p6199s1_ind_cab %>% filter(p6199s1_ind_cab$p6199s1 == 20 & p6199s1_ind_cab$clase == 1)  
p6199s1_20_cab <- sum(p6199s1_20_cab$Freq)
# 21 dias
p6199s1_21_cab <- p6199s1_ind_cab %>% filter(p6199s1_ind_cab$p6199s1 == 21 & p6199s1_ind_cab$clase == 1)
p6199s1_21_cab <- sum(p6199s1_21_cab$Freq)
# 23 dias
p6199s1_23_cab <- p6199s1_ind_cab %>% filter(p6199s1_ind_cab$p6199s1 == 23 & p6199s1_ind_cab$clase == 1)  
p6199s1_23_cab <- sum(p6199s1_23_cab$Freq)
# 25 dias
p6199s1_25_cab <- p6199s1_ind_cab %>% filter(p6199s1_ind_cab$p6199s1 == 25 & p6199s1_ind_cab$clase == 1)  
p6199s1_25_cab <- sum(p6199s1_25_cab$Freq)
# 30 dias
p6199s1_30_cab <- p6199s1_ind_cab %>% filter(p6199s1_ind_cab$p6199s1 == 30 & p6199s1_ind_cab$clase == 1)  
p6199s1_30_cab <- sum(p6199s1_30_cab$Freq)

# total p6199s1
p6199s1_total_cab <- p6199s1_0_cab+p6199s1_1_cab+p6199s1_2_cab+p6199s1_3_cab+p6199s1_4_cab+p6199s1_5_cab+p6199s1_6_cab+
  p6199s1_7_cab+p6199s1_8_cab+p6199s1_9_cab+p6199s1_10_cab+p6199s1_12_cab+p6199s1_14_cab+p6199s1_15_cab+p6199s1_20_cab+
  p6199s1_21_cab+p6199s1_23_cab+p6199s1_25_cab+p6199s1_30_cab
                                                               # 464.616
##################################################################################################################################################################
# 27. ¿fue remitido(a) a especialista?
p6145_ind_cab <- data.frame(svytable(~fex_c + p6145 + clase,
                                 design = BD_survey))
# Si
p6145_1_cab <- p6145_ind_cab %>% filter(p6145_ind_cab$p6145 == 1 & p6145_ind_cab$clase == 1)  # 372.126
p6145_1_cab <- sum(p6145_1_cab$Freq)
# No
p6145_2_cab <- p6145_ind_cab %>% filter(p6145_ind_cab$p6145 == 2 & p6145_ind_cab$clase == 1)  # 554.868
p6145_2_cab <- sum(p6145_2_cab$Freq)

# total p6145
p6145_total_cab <- p6145_1_cab+p6145_2_cab  # 926.995
##################################################################################################################################################################
# 28. En general, considera que la calidad de la prestación del servicio de salud (medicina general, medicina especializada, odontología, etc.) fue:
p8554_ind_cab <- data.frame(svytable(~fex_c + p8554 + clase,
                                 design = BD_survey))
# Muy buena
p8554_1_cab <- p8554_ind_cab %>% filter(p8554_ind_cab$p8554 == 1 & p8554_ind_cab$clase == 1)  # 216.723
p8554_1_cab <- sum(p8554_1_cab$Freq)
# Buena
p8554_2_cab <- p8554_ind_cab %>% filter(p8554_ind_cab$p8554 == 2 & p8554_ind_cab$clase == 1)  # 736.842
p8554_2_cab <- sum(p8554_2_cab$Freq)
# Mala
p8554_3_cab <- p8554_ind_cab %>% filter(p8554_ind_cab$p8554 == 3 & p8554_ind_cab$clase == 1)  # 105.294
p8554_3_cab <- sum(p8554_3_cab$Freq)
# Muy mala
p8554_4_cab <- p8554_ind_cab %>% filter(p8554_ind_cab$p8554 == 4 & p8554_ind_cab$clase == 1)  # 25.966
p8554_4_cab <- sum(p8554_4_cab$Freq)

# total p8554
p8554_total_cab <- p8554_1_cab+p8554_2_cab+p8554_3_cab+p8554_4_cab  # 1.289.988
###################################################################################################################################################################
# 29. ¿Cuál es el aspecto que más influyó en su percepción sobre la calidad de la prestación del servicio?
p801_ind_cab <- data.frame(svytable(~fex_c + p801 + clase,
                                design = BD_survey))
# Trámites excesivos o dispendiosos
p801_1_cab <- p801_ind_cab %>% filter(p801_ind_cab$p801 == 1 & p801_ind_cab$clase == 1)  # 15.283
p801_1_cab <- sum(p801_1_cab$Freq)
# Mala atención del personal administrativo o asistencial (médicos, enfermeras, etc.)
p801_2_cab <- p801_ind_cab %>% filter(p801_ind_cab$p801 == 2 & p801_ind_cab$clase == 1)  # 38.582
p801_2_cab <- sum(p801_2_cab$Freq)
# Falta de capacidad, conocimientos o habilidad del personal asistencial
p801_3_cab <- p801_ind_cab %>% filter(p801_ind_cab$p801 == 3 & p801_ind_cab$clase == 1)  # 9.405
p801_3_cab <- sum(p801_3_cab$Freq)
# Condiciones deficientes de infraestructura, dotación, mobiliario o accesibilidad para población en condición de discapacidad
p801_4_cab <- p801_ind_cab %>% filter(p801_ind_cab$p801 == 4 & p801_ind_cab$clase == 1)  # 1.173
p801_4_cab <- sum(p801_4_cab$Freq)
# Demora en la asignación de citas
p801_5_cab <- p801_ind_cab %>% filter(p801_ind_cab$p801 == 5 & p801_ind_cab$clase == 1)  # 24.652
p801_5_cab <- sum(p801_5_cab$Freq)
# Demora en la atención por parte del personal médico
p801_6_cab <- p801_ind_cab %>% filter(p801_ind_cab$p801 == 6 & p801_ind_cab$clase == 1)  # 31.277
p801_6_cab <- sum(p801_6_cab$Freq)
# Problemas relacionados con los medicamentos
p801_7_cab <- p801_ind_cab %>% filter(p801_ind_cab$p801 == 7 & p801_ind_cab$clase == 1)  # 4.486
p801_7_cab <- sum(p801_7_cab$Freq)
# Otro
p801_8_cab <- p801_ind_cab %>% filter(p801_ind_cab$p801 == 8 & p801_ind_cab$clase == 1)  # 3.482
p801_8_cab <- sum(p801_8_cab$Freq)

# total p801
p801_total_cab <- p801_1_cab+p801_2_cab+p801_3_cab+p801_4_cab+p801_5_cab+p801_6_cab+p801_7_cab+p801_8_cab  # 128.343
##########################################################################################################################################################################
# 30. ¿Cuáles de las siguientes fuentes utilizó ... para cubrir los costos de atención en salud en los últimos 30 días? (incluya consulta médica , exámenes y medicamentos)
# p8556 

# 30.1 EPS o entidad de seguridad social en salud  en la cual está afiliado/a
p8556s1_ind_cab <- data.frame(svytable(~fex_c + p8556s1 + clase,
                                   design = BD_survey))
p8556s1_1_cab <- p8556s1_ind_cab %>% filter(p8556s1_ind_cab$p8556s1 == 1 & p8556s1_ind_cab$clase == 1) # 816.522
p8556s1_1_cab <- sum(p8556s1_1_cab$Freq)
# 30.2 Plan o seguro voluntario (Seguro médico, plan complementario o medicina prepagada)
p8556s2_ind_cab <- data.frame(svytable(~fex_c + p8556s2 + clase,
                                   design = BD_survey))
p8556s2_2_cab <- p8556s2_ind_cab %>% filter(p8556s2_ind_cab$p8556s2 == 1 & p8556s2_ind_cab$clase == 1) # 87.174
p8556s2_2_cab <- sum(p8556s2_2_cab$Freq)
# 30.3 Seguro obligatorio de accidentes de tránsito (SOAT)
p8556s4_ind_cab <- data.frame(svytable(~fex_c + p8556s4 + clase,
                                   design = BD_survey))
p8556s4_3_cab <- p8556s4_ind_cab %>% filter(p8556s4_ind_cab$p8556s4 == 1 & p8556s4_ind_cab$clase == 1) # 15.492
p8556s4_3_cab <- sum(p8556s4_3_cab$Freq)
# 30.4 Secretaria de salud o la alcaldía
p8556s5_ind_cab <- data.frame(svytable(~fex_c + p8556s5 + clase,
                                   design = BD_survey))
p8556s5_4_cab <- p8556s5_ind_cab %>% filter(p8556s5_ind_cab$p8556s5 == 1 & p8556s5_ind_cab$clase == 1) # 5.662
p8556s5_4_cab <- sum(p8556s5_4_cab$Freq)
# 30.5 Recursos propios o familiares
p8556s6_ind_cab <- data.frame(svytable(~fex_c + p8556s6 + clase,
                                   design = BD_survey))
p8556s6_5_cab <- p8556s6_ind_cab %>% filter(p8556s6_ind_cab$p8556s6 == 1 & p8556s6_ind_cab$clase == 1) # 188.067
p8556s6_5_cab <- sum(p8556s6_5_cab$Freq)
# 30.6 Recursos de otras personas 
p8556s9_ind_cab <- data.frame(svytable(~fex_c + p8556s9 + clase,
                                   design = BD_survey))
p8556s9_6_cab <- p8556s9_ind_cab %>% filter(p8556s9_ind_cab$p8556s9 == 1 & p8556s9_ind_cab$clase == 1) # 10.872
p8556s9_6_cab <- sum(p8556s9_6_cab$Freq)
# 30.7 No se requirio pago
p8556s10_ind_cab <- data.frame(svytable(~fex_c + p8556s10 + clase,
                                    design = BD_survey))
p8556s10_7_cab <- p8556s10_ind_cab %>% filter(p8556s10_ind_cab$p8556s10 == 1 & p8556s10_ind_cab$clase == 1) # 28.801
p8556s10_7_cab <- sum(p8556s10_7_cab$Freq)

# total p8556
p8556_total_cab <- p8556s1_1_cab + p8556s2_2_cab + p8556s4_3_cab + p8556s5_4_cab + p8556s6_5_cab + p8556s9_6_cab + p8556s10_7_cab
#######################################################################################################################################################
# 31. Por esta enfermedad , ¿a ... le formularon medicamentos?
p6147_ind_cab <- data.frame(svytable(~fex_c + p6147 + clase,
                                 design = BD_survey))
# Si
p6147_1_cab <- p6147_ind_cab %>% filter(p6147_ind_cab$p6147 == 1 & p6147_ind_cab$clase == 1)  # 889.650
p6147_1_cab <- sum(p6147_1_cab$Freq)
# No
p6147_2_cab <- p6147_ind_cab %>% filter(p6147_ind_cab$p6147 == 2 & p6147_ind_cab$clase == 1)  # 192.258
p6147_2_cab <- sum(p6147_2_cab$Freq)

# total 6147
p6147_total_cab <- p6147_1_cab+p6147_2_cab  # 1.081.909
#########################################################################################################################################################
# 32. ¿Estos medicamentos o remedios le fueron entregados a ... por cuenta de la institución a la cual está afiliado(a)?
p6148_ind_cab <- data.frame(svytable(~fex_c + p6148 + clase,
                                 design = BD_survey))
# Si, todos
p6148_1_cab <- p6148_ind_cab %>% filter(p6148_ind_cab$p6148 == 1 & p6148_ind_cab$clase == 1)  # 547.752
p6148_1_cab <- sum(p6148_1_cab$Freq)
# Si, algunos
p6148_2_cab <- p6148_ind_cab %>% filter(p6148_ind_cab$p6148 == 2 & p6148_ind_cab$clase == 1)  # 121.387
p6148_2_cab <- sum(p6148_2_cab$Freq)
# No
p6148_3_cab <- p6148_ind_cab %>% filter(p6148_ind_cab$p6148 == 3 & p6148_ind_cab$clase == 1)  # 220.510
p6148_3_cab <- sum(p6148_3_cab$Freq)
# total 6147
p6148_total_cab <- p6148_1_cab+p6148_2_cab+p6148_3_cab  # 889.650
###########################################################################################################################################################
# 33. ¿Por qué razón no le fueron entregados los medicamentos  (todos o algunos)?
p6149_ind_cab <- data.frame(svytable(~fex_c + p6149 + clase,
                                 design = BD_survey))
# No están incluidos en el plan de beneficios en salud o POS o no le autorizaron
p6149_1_cab <- p6149_ind_cab %>% filter(p6149_ind_cab$p6149 == 1 & p6149_ind_cab$clase == 1)  # 125.724
p6149_1_cab <- sum(p6149_1_cab$Freq)
# No había los medicamentos recetados
p6149_2_cab <- p6149_ind_cab %>% filter(p6149_ind_cab$p6149 == 2 & p6149_ind_cab$clase == 1)  # 49.676
p6149_2_cab <- sum(p6149_2_cab$Freq)
# No había la cantidad requerida
p6149_3_cab <- p6149_ind_cab %>% filter(p6149_ind_cab$p6149 == 3 & p6149_ind_cab$clase == 1)  # 10.584
p6149_3_cab <- sum(p6149_3_cab$Freq)
# Por errores o deficiencias en la expedición de la fórmula medica
p6149_4_cab <- p6149_ind_cab %>% filter(p6149_ind_cab$p6149 == 4 & p6149_ind_cab$clase == 1)  # 3.048
p6149_4_cab <- sum(p6149_4_cab$Freq)
# No hizo las gestiones para reclamarlos
p6149_5_cab <- p6149_ind_cab %>% filter(p6149_ind_cab$p6149 == 5 & p6149_ind_cab$clase == 1)  # 57.664
p6149_5_cab <- sum(p6149_5_cab$Freq)
# No tenia dinero
p6149_6_cab <- p6149_ind_cab %>% filter(p6149_ind_cab$p6149 == 6 & p6149_ind_cab$clase == 1)  # 2.151
p6149_6_cab <- sum(p6149_6_cab$Freq)
# Acudió a medico particular
p6149_7_cab <- p6149_ind_cab %>% filter(p6149_ind_cab$p6149 == 7 & p6149_ind_cab$clase == 1)  # 83.081
p6149_7_cab <- sum(p6149_7_cab$Freq)
# Otra
p6149_8_cab <- p6149_ind_cab %>% filter(p6149_ind_cab$p6149 == 8 & p6149_ind_cab$clase == 1)  # 9.965
p6149_8_cab <- sum(p6149_8_cab$Freq)

# total p6149
p6149_total_cab <- p6149_1_cab+p6149_2_cab+p6149_3_cab+p6149_4_cab+p6149_5_cab+p6149_6_cab+p6149_7_cab+p6149_8_cab  # 341.897
############################################################################################################################################################
# 34.  Durante los últimos 30 días ....realizó pagos por:  (No incluya gastos reportados en hospitalización)
# 34.1 Consulta medica general o con especialista ?
p3178_ind_cab <- data.frame(svytable(~fex_c + p3178 + clase,
                                 design = BD_survey))
# Si
p3178_1_cab <- p3178_ind_cab %>% filter(p3178_ind_cab$p3178 == 1 & p3178_ind_cab$clase == 1)  # 3.169.949
p3178_1_cab <- sum(p3178_1_cab$Freq)
# No
p3178_2_cab <- p3178_ind_cab %>% filter(p3178_ind_cab$p3178 == 2 & p3178_ind_cab$clase == 1)  # 35.898.655
p3178_2_cab <- sum(p3178_2_cab$Freq)

# total p3178
p3178_total_cab <- p3178_1_cab+p3178_2_cab  # 39.068.605
#############################################################################################################################################################
# 34.1.1 A traves de EPS
p3178s1_ind_cab <- data.frame(svytable(~fex_c + p3178s1 + clase,
                                   design = BD_survey))
p3178s1_cab <- p3178s1_ind_cab %>% filter(p3178s1_ind_cab$p3178s1 == 1 & p3178s1_ind_cab$clase == 1)  # 2.462.322
p3178s1_cab <- sum(p3178s1_cab$Freq)
# 34.1.1.A1 Valor a traves de EPS
#p3178s1a1_cab <- table(BD_ecv_ind$p3178s1a1)
##############################################################################################################################################################
# 34.1.2 Medico particular
p3178s2_ind_cab <- data.frame(svytable(~fex_c + p3178s2 + clase,
                                   design = BD_survey))
p3178s2_cab <- p3178s2_ind_cab %>% filter(p3178s2_ind_cab$p3178s2 == 1 & p3178s2_ind_cab$clase == 1)  # 374.129
p3178s2_cab <- sum(p3178s2_cab$Freq)
# 34.1.2.A1 Valor Médico particular
#p3178s2a1_cab <- table(BD_ecv_ind$p3178s2a1)
##############################################################################################################################################################
# 34.1.3 Plan voluntario (seguro médico, plan complementario o medicina prepagada)
p3178s3_ind_cab <- data.frame(svytable(~fex_c + p3178s3 + clase,
                                   design = BD_survey))
p3178s3_cab <- p3178s3_ind_cab %>% filter(p3178s3_ind_cab$p3178s3 == 1 & p3178s3_ind_cab$clase == 1)  # 361.850
p3178s3_cab <- sum(p3178s3_cab$Freq)
# 34.1.2 A1 Valor Plan voluntario (seguro médico, plan complementario o medicina prepagada) 
#p3178s3a1_cab <- table(BD_ecv_ind$p3178s3a1)
#############################################################################################################################################################
# 34.2 Consulta o tratamiento odontologico?
p3179_ind_cab <- data.frame(svytable(~fex_c + p3179 + clase,
                                 design = BD_survey))
# Si
p3179_1_cab <- p3179_ind_cab %>% filter(p3179_ind_cab$p3179 == 1 & p3179_ind_cab$clase == 1)  # 1.135.904
p3179_1_cab <- sum(p3179_1_cab$Freq)
# No
p3179_2_cab <- p3179_ind_cab %>% filter(p3179_ind_cab$p3179 == 2 & p3179_ind_cab$clase == 1)  # 37.932.700
p3179_2_cab <- sum(p3179_2_cab$Freq)

# total p3179
p3179_total_cab <- p3179_1_cab+p3179_2_cab  # 39.068.605
##############################################################################################################################################################
# 34.2.1 A traves de EPS
p3179s1_ind_cab <- data.frame(svytable(~fex_c + p3179s1 + clase,
                                   design = BD_survey))
p3179s1_cab <- p3179s1_ind_cab %>% filter(p3179s1_ind_cab$p3179s1 == 1 & p3179s1_ind_cab$clase == 1)  # 490.716
p3179s1_cab <- sum(p3179s1_cab$Freq)
# 34.2.1.A1 Valor a traves de EPS
#p3179s1a1_cab <- table(BD_ecv_ind$p3179s1a1)
##############################################################################################################################################################
# 34.2.2 Odontologo particular
p3179s2_ind_cab <- data.frame(svytable(~fex_c + p3179s2 + clase,
                                   design = BD_survey))
p3179s2_cab <- p3179s2_ind_cab %>% filter(p3179s2_ind_cab$p3179s2 == 1 & p3179s2_ind_cab$clase == 1)  # 568.694
p3179s2_cab <- sum(p3179s2_cab$Freq)
# 34.2.2.A1 Valor odontologo particular
#p3179s2a1_cab <- table(BD_ecv_ind$p3179s2a1)
###############################################################################################################################################################
# 34.2.3 Plan voluntario (seguro médico, plan complementario o medicina prepagada)
p3179s3_ind_cab <- data.frame(svytable(~fex_c + p3179s3 + clase,
                                   design = BD_survey))
p3179s3_cab <- p3179s3_ind_cab %>% filter(p3179s3_ind_cab$p3179s3 == 1 & p3179s3_ind_cab$clase == 1)  # 81.220
p3179s3_cab <- sum(p3179s3_cab$Freq)
# 34.2.3 A1 Valor Plan voluntario (seguro médico, plan complementario o medicina prepagada) 
#p3179s3a1_cab <- summary(BD_ecv_ind$p3179s3a1)
###############################################################################################################################################################
# 34.3 Vacunas
p3181_ind_cab <- data.frame(svytable(~fex_c + p3181 + clase, 
                                 design = BD_survey))
# Si
p3181_1_cab <- p3181_ind_cab %>% filter(p3181_ind_cab$p3181 == 1 & p3181_ind_cab$clase == 1)  # 279.829
p3181_1_cab <- sum(p3181_1_cab$Freq)

# No
p3181_2_cab <- p3181_ind_cab %>% filter(p3181_ind_cab$p3181 == 2 & p3181_ind_cab$clase == 1) # 38.788.775
p3181_2_cab <- sum(p3181_2_cab$Freq)

# total p3181 
p3181_total_cab <- p3181_1_cab+p3181_2_cab  # 39.068.605
# 34.3.1 Valor vacuna
#p3181s1_ind_cab <- data.frame(svytable(~fex_c + p3181s1 + clase,
                                   #design = BD_survey))
#p3181s1_cab <- summary(p3181s1_ind_cab$p3181s1)
#################################################################################################################################################################
# 34.4 Formulas medicas o compra de medicamentos consumidos ocasional o regularmente
p3182_ind_cab <- data.frame(svytable(~fex_c + p3182 + clase,
                                 design = BD_survey))
# Si
p3182_1_cab <- p3182_ind_cab %>% filter(p3182_ind_cab$p3182 == 1 & p3182_ind_cab$clase == 1) # 2.885.358
p3182_1_cab <- sum(p3182_1_cab$Freq)
# No
p3182_2_cab <- p3182_ind_cab %>% filter(p3182_ind_cab$p3182 == 2 & p3182_ind_cab$clase == 1)  # 36.183.246
p3182_2_cab <- sum(p3182_2_cab$Freq)

# total p3182
p3182_total_cab <- p3182_1_cab+p3182_2_cab   # 39.068.605

# 34.4.1 Valor
#p3182s1_ind_cab <- data.frame(svytable(~fex_c + p3182s1_ind + clase,
                                   #design = BD_survey))
#p3182s1_cab <- table(BD_ecv_ind$p3182s1)
####################################################################################################################################################################
# 34.5 Laboratorio clinico, RX, examenes de diagnostico?
p3183_ind_cab <- data.frame(svytable(~fex_c + p3183 + clase,
                                 design = BD_survey))
# Si
p3183_1_cab <- p3183_ind_cab %>% filter(p3183_ind_cab$p3183 == 1 & p3183_ind_cab$clase == 1)  # 962.324
p3183_1_cab <- sum(p3183_1_cab$Freq)
# No
p3183_2_cab <- p3183_ind_cab %>% filter(p3183_ind_cab$p3183 == 2 & p3183_ind_cab$clase == 1)  # 38.106.280
p3183_2_cab <- sum(p3183_2_cab$Freq)

# total p3183
p3183_total_cab <- p3183_1_cab+p3183_2_cab  # 39.068.605

# 34.5.1 Valor
#p3183s1_ind_cab <- data.frame(svytable(~fex_c + p3183s1_ind + clase,
                                   #design = BD_survey))
#p3183s1_cab <- table(BD_ecv_ind$p3183s1)
#######################################################################################################################################################################
# 34.6 Rehabilitacion o terapias medicas ?
p3184_ind_cab <- data.frame(svytable(~fex_c + p3184 + clase,
                                 design = BD_survey))
# Si
p3184_1_cab <- p3184_ind_cab %>% filter(p3184_ind_cab$p3184 == 1 & p3184_ind_cab$clase == 1)  # 214.157
p3184_1_cab <- sum(p3184_1_cab$Freq)
# No
p3184_2_cab <- p3184_ind_cab %>% filter(p3184_ind_cab$p3184 == 2 & p3184_ind_cab$clase == 1)  # 38.854.447
p3184_2_cab <- sum(p3184_2_cab$Freq)

# total p3184
p3184_total_cab <- p3184_1_cab+p3184_2_cab  # 39.068.605

# 34.6.1 Valor
#p3184s1_ind_cab <- data.frame(svytable(~fex_c + p3184s1,
                                   #design = BD_survey))
#p3184s1_cab <- table(BD_ecv_ind$p3184s1)
########################################################################################################################################################################
# 34.7 Terapias alternativas ? (homeopatia, acupuntura, esencias florales, musicoterapia)
p3185_ind_cab <- data.frame(svytable(~fex_c + p3185 + clase,
                                 design = BD_survey))
# Si
p3185_1_cab <- p3185_ind_cab %>% filter(p3185_ind_cab$p3185 == 1 & p3185_ind_cab$clase == 1)  # 64.133
p3185_1_cab <- sum(p3185_1_cab$Freq)
# No
p3185_2_cab <- p3185_ind_cab %>% filter(p3185_ind_cab$p3185 == 2 & p3185_ind_cab$clase == 1)  # 39.004.471
p3185_2_cab <- sum(p3185_2_cab$Freq)

# total p3185
p3185_total_cab <- p3185_1_cab+p3185_2_cab  # 39.068.605

# 34.7.1 Valor
#p3185s1_ind_cab <- data.frame(svytable(~fex_c + p3185s1 + clase,
                                   #design = BD_survey))
#p3185s1_cab <- table(BD_ecv_ind$p3185s1)
##########################################################################################################################################################################
# 34.8 Transporte para ir al sitio de atencion medica y regresar
p3186_ind_cab <- data.frame(svytable(~fex_c + p3186 + clase,
                                 design = BD_survey))
# Si
p3186_1_cab <- p3186_ind_cab %>% filter(p3186_ind_cab$p3186 == 1 & p3186_ind_cab$clase == 1)  # 2.587.719
p3186_1_cab <- sum(p3186_1_cab$Freq)
# No
p3186_2_cab <- p3186_ind_cab %>% filter(p3186_ind_cab$p3186 == 2 & p3186_ind_cab$clase == 1)  # 36.480.885
p3186_2_cab <- sum(p3186_2_cab$Freq)

# total p3186
p3186_total_cab <- p3186_1_cab+p3186_2_cab  # 39.068.605

# 34.7.1 Valor
#p3186s1_ind_cab <- data.frame(svytable(~fex_c + p3186s1 + clase,
                                   #design = BD_survey))
#p3186s1_cab <- table(BD_ecv_ind$p3186s1)
###########################################################################################################################################################################
# 35. Durante los ÚLTIMOS DOCE MESES ¿Realizó pagos por: 
# 35.1  Lentes, audífonos o aparatos ortopédicos (muletas, sillas de ruedas, elementos para terapias, etc.)
p3187s1_ind_cab <- data.frame(svytable(~fex_c + p3187s1 + clase,
                                   design = BD_survey))
# Si
p3187s1_1_cab <- p3187s1_ind_cab %>% filter(p3187s1_ind_cab$p3187s1 == 1 & p3187s1_ind_cab$clase == 1)  # 1.845.552
p3187s1_1_cab <- sum(p3187s1_1_cab$Freq)
# No
p3187s1_2_cab <- p3187s1_ind_cab %>% filter(p3187s1_ind_cab$p3187s1 == 2 & p3187s1_ind_cab$clase == 1)  # 37.223.052
p3187s1_2_cab <- sum(p3187s1_2_cab$Freq)

# total p3187s1
p3187s1_total_cab <- p3187s1_1_cab+p3187s1_2_cab  # 39.068.605

# 35.2. Valor Lentes, audífonos o aparatos ortopédicos (muletas, sillas de ruedas, elementos para terapias, etc.)
#p3187s2_ind_cab <- data.frame(svytable(~fex_c + p3187s2 + clase,
                                   #design = BD_survey))
#p3187s2_cab <- table(BD_ecv_ind$p3187s2)
############################################################################################################################################################################
# 35.2 Cirugías o procedimientos ambulatorios? 
p3188_ind_cab <- data.frame(svytable(~fex_c + p3188 + clase,
                                 design = BD_survey))
# Si
p3188_1_cab <- p3188_ind_cab %>% filter(p3188_ind_cab$p3188 == 1 & p3188_ind_cab$clase == 1)  # 401.410
p3188_1_cab <- sum(p3188_1_cab$Freq)
# No
p3188_2_cab <- p3188_ind_cab %>% filter(p3188_ind_cab$p3188 == 2 & p3188_ind_cab$clase == 1)  # 38.667.194
p3188_2_cab <- sum(p3188_2_cab$Freq)

# total p3188
p3188_total_cab <- p3188_1_cab+p3188_2_cab  # 39.068.605
#############################################################################################################################################################################
# 35.2.1 A traves de EPS
p3188s1_ind_cab <- data.frame(svytable(~fex_c + p3188s1 + clase,
                                   design = BD_survey))
p3188s1_cab <- p3188s1_ind_cab %>% filter(p3188s1_ind_cab$p3188s1 == 1 & p3188s1_ind_cab$clase == 1)   # 279.004
p3188s1_cab <- sum(p3188s1_cab$Freq)

# 35.2.1 A1 Valor a traves de EPS
#p3188s1a1_ind_cab <- data.frame(svytable(~fex_c + p3188s1a1 + clase,
                                     #design = BD_survey))
#p3188s1a1_cab <- table(BD_ecv_ind$p3188s1a1)
#############################################################################################################################################################################
# 35.2.2 Medico particular
p3188s2_ind_cab <- data.frame(svytable(~fex_c + p3188s2 + clase,
                                   design = BD_survey))
p3188s2_cab <- p3188s2_ind_cab %>% filter(p3188s2_ind_cab$p3188s2 == 1 & p3188s2_ind_cab$clase == 1)   # 85.457
p3188s2_cab <- sum(p3188s2_cab$Freq)

# 35.2.2 A1 Valor medico particular
#p3188s2a1_ind_cab <- data.frame(svytable(~fex_c + p3188s2a1 + clase,
                                     #design = BD_survey))
#p3188s2a1_cab <- table(BD_ecv_ind$p3188s2a1)
###############################################################################################################################################################################
# 35.2.3 Plan voluntario (seguro médico, plan complementario o medicina prepagada) 
p3188s3_ind_cab <- data.frame(svytable(~fex_c + p3188s3 + clase,
                                   design = BD_survey))
p3188s3_cab <- p3188s3_ind_cab %>% filter(p3188s3_ind_cab$p3188s3 == 1 & p3188s3_ind_cab$clase == 1)   # 45.300
p3188s3_cab <- sum(p3188s3_cab$Freq)

# 35.2.3 A1  Valor Valor Plan voluntario (seguro médico, plan complementario o medicina prepagada) 
#p3188s3a1_ind_cab <- data.frame(svytable(~fex_c + p3188s3a1 + clase,
                                     #design = BD_survey))
#p3188s3a1_cab <- table(BD_ecv_ind$p3188s3a1)
################################################################################################################################################################################
# 36. ¿Actualmente _____ fuma (cigarrillo, tabaco, vapeador o cigarrillo electrónico)?
# 36.1 Cigarrillo, tabaco:
p3008s1_ind_cab <- data.frame(svytable(~fex_c + p3008s1 + clase,
                                   design = BD_survey))
# Si 
p3008s1_1_cab <- p3008s1_ind_cab %>% filter(p3008s1_ind_cab$p3008s1 == 1 & p3008s1_ind_cab$clase == 1)  # 1.957.838
p3008s1_1_cab <- sum(p3008s1_1_cab$Freq)
# No
p3008s1_2_cab <- p3008s1_ind_cab %>% filter(p3008s1_ind_cab$p3008s1 == 2 & p3008s1_ind_cab$clase == 1)  # 31.611.553
p3008s1_2_cab <- sum(p3008s1_2_cab$Freq)

# total p3008s1
p3008s1_total_cab <- p3008s1_1_cab+p3008s1_2_cab  # 33.569.391
#####################################################################################################################################################################################
# 36.2 Frecuencia cigarrillo, tabaco:
p3008s1a1_ind_cab <- data.frame(svytable(~fex_c + p3008s1a1 + clase,
                                     design = BD_survey))
# Diariamente 
p3008s1a1_1_cab <- p3008s1a1_ind_cab %>% filter(p3008s1a1_ind_cab$p3008s1a1 == 1 & p3008s1a1_ind_cab$clase == 1)  # 1.191.560
p3008s1a1_1_cab <- sum(p3008s1a1_1_cab$Freq)
# Algunos dias a la semana
p3008s1a1_2_cab <- p3008s1a1_ind_cab %>% filter(p3008s1a1_ind_cab$p3008s1a1 == 2 & p3008s1a1_ind_cab$clase == 1)  # 521.681
p3008s1a1_2_cab <- sum(p3008s1a1_2_cab$Freq)
# Menos de una vez por semana
p3008s1a1_3_cab <- p3008s1a1_ind_cab %>% filter(p3008s1a1_ind_cab$p3008s1a1 == 3 & p3008s1a1_ind_cab$clase == 1)  # 244.596
p3008s1a1_3_cab <- sum(p3008s1a1_3_cab$Freq)
# total p3008s1a1
p3008s1a1_total_cab <- p3008s1a1_1_cab+p3008s1a1_2_cab+p3008s1a1_3_cab  # 1.957.838
#################################################################################################################################################################################
# 36.3 ¿Cuantos cigarrillos al dia?
#p3008s1a2_ind_cab <- data.frame(svytable(~fex_c + p3008s1a2 + clase,
                                     #design = BD_survey))
#p3008s1a2_cab <- table(BD_ecv_ind$p3008s1a2)
###################################################################################################################################################################################
# 36.4 Vapeador o cigarrillo electronico
p3008s2_ind_cab <- data.frame(svytable(~fex_c + p3008s2 + clase,
                                   design = BD_survey))
# Si 
p3008s2_1_cab <- p3008s2_ind_cab %>% filter(p3008s2_ind_cab$p3008s2 == 1 & p3008s2_ind_cab$clase == 1)  # 82.943
p3008s2_1_cab <- sum(p3008s2_1_cab$Freq)
# No
p3008s2_2_cab <- p3008s2_ind_cab %>% filter(p3008s2_ind_cab$p3008s2 == 2 & p3008s2_ind_cab$clase == 1)  # 33.486.447
p3008s2_2_cab <- sum(p3008s2_2_cab$Freq)

# total p3008s1
p3008s2_total_cab <- p3008s2_1_cab+p3008s2_2_cab  # 33.569.391
###################################################################################################################################################################################
# 37. ¿ ......  consume bebidas azucaradas (gaseosas, refrescos, bebidas de  jugos de frutas procesadas, té endulzado, refrescos en polvo)?
p1707_ind_cab <- data.frame(svytable(~fex_c + p1707 + clase,
                                 design = BD_survey))
# Si
p1707_1_cab <- p1707_ind_cab %>% filter(p1707_ind_cab$p1707 == 1 & p1707_ind_cab$clase == 1)  # 23.730.629
p1707_1_cab <- sum(p1707_1_cab$Freq)
# No
p1707_2_cab <- p1707_ind_cab %>% filter(p1707_ind_cab$p1707 == 2 & p1707_ind_cab$clase == 1)  # 14.396.233
p1707_2_cab <- sum(p1707_2_cab$Freq)

# total p1707
p1707_total_cab <- p1707_1_cab+p1707_2_cab  # 38.126.862
#####################################################################################################################################################################################
# 37.1 Con que frecuencia consume las bebidas azucaradas:
p1707s1_ind_cab <- data.frame(svytable(~fex_c + p1707s1 + clase,
                                   design = BD_survey))
# Todos los dias de la semana (dos o mas veces al dia)
p1707s1_1_cab <- p1707s1_ind_cab %>% filter(p1707s1_ind_cab$p1707s1 == 1 & p1707s1_ind_cab$clase == 1)  # 1.998.276
p1707s1_1_cab <- sum(p1707s1_1_cab$Freq)
# Todos los dias de la semana (una vez al dia)
p1707s1_2_cab <- p1707s1_ind_cab %>% filter(p1707s1_ind_cab$p1707s1 == 2 & p1707s1_ind_cab$clase == 1)  # 2.642.830
p1707s1_2_cab <- sum(p1707s1_2_cab$Freq)
# Cuatro a seis veces a la semana
p1707s1_3_cab <- p1707s1_ind_cab %>% filter(p1707s1_ind_cab$p1707s1 == 3 & p1707s1_ind_cab$clase == 1)  # 2.112.003
p1707s1_3_cab <- sum(p1707s1_3_cab$Freq)
# Dos o tres veces a la semana
p1707s1_4_cab <- p1707s1_ind_cab %>% filter(p1707s1_ind_cab$p1707s1 == 4 & p1707s1_ind_cab$clase == 1)  # 7.708.717
p1707s1_4_cab <- sum(p1707s1_4_cab$Freq)
# Una vez a la semana
p1707s1_5_cab <- p1707s1_ind_cab %>% filter(p1707s1_ind_cab$p1707s1 == 5 & p1707s1_ind_cab$clase == 1)  # 6.139.353
p1707s1_5_cab <- sum(p1707s1_5_cab$Freq)
# Menos de una vez por semana
p1707s1_6_cab <- p1707s1_ind_cab %>% filter(p1707s1_ind_cab$p1707s1 == 6 & p1707s1_ind_cab$clase == 1)  # 3.129.448
p1707s1_6_cab <- sum(p1707s1_6_cab$Freq)

# total p1707s1
p1707s1_total_cab <- p1707s1_1_cab+p1707s1_2_cab+p1707s1_3_cab+p1707s1_4_cab+p1707s1_5_cab+p1707s1_6_cab  # 23.730.629
########################################################################################################################################################################
# 38. ¿ ____  consume alimentos de paquete (papas, plátanos, chitos, paquete mixto, rosquitas, chicharrones o similares)?
p3003_ind_cab <- data.frame(svytable(~fex_c + p3003 + clase,
                                 design = BD_survey))
# Si
p3003_1_cab <- p3003_ind_cab %>% filter(p3003_ind_cab$p3003 == 1 & p3003_ind_cab$clase == 1)  # 19.382.921
p3003_1_cab <- sum(p3003_1_cab$Freq)
# No
p3003_2_cab <- p3003_ind_cab %>% filter(p3003_ind_cab$p3003 == 2 & p3003_ind_cab$clase == 1)  # 18.743.941
p3003_2_cab <- sum(p3003_2_cab$Freq)

# total p3003
p3003_total_cab <- p3003_1_cab+p3003_2_cab  # 38.126.862
#########################################################################################################################################################################
# 38.1 Con que frecuencia consume alimentos de paquete
p3003s1_ind_cab <- data.frame(svytable(~fex_c + p3003s1 + clase,
                                   design = BD_survey))
# Todos los dias de la semana (dos o mas veces al dia)
p3003s1_1_cab <- p3003s1_ind_cab %>% filter(p3003s1_ind_cab$p3003s1 == 1 & p3003s1_ind_cab$clase == 1)  # 772.644
p3003s1_1_cab <- sum(p3003s1_1_cab$Freq)
# Todos los dias de la semana (una vez al dia)
p3003s1_2_cab <- p3003s1_ind_cab %>% filter(p3003s1_ind_cab$p3003s1 == 2 & p3003s1_ind_cab$clase == 1)  # 1.547.378
p3003s1_2_cab <- sum(p3003s1_2_cab$Freq)
# Cuatro a seis veces a la semana
p3003s1_3_cab <- p3003s1_ind_cab %>% filter(p3003s1_ind_cab$p3003s1 == 3 & p3003s1_ind_cab$clase == 1)  # 1.590.644
p3003s1_3_cab <- sum(p3003s1_3_cab$Freq)
# Dos o tres veces a la semana
p3003s1_4_cab <- p3003s1_ind_cab %>% filter(p3003s1_ind_cab$p3003s1 == 4 & p3003s1_ind_cab$clase == 1)  # 6.099.625
p3003s1_4_cab <- sum(p3003s1_4_cab$Freq)
# Una vez a la semana
p3003s1_5_cab <- p3003s1_ind_cab %>% filter(p3003s1_ind_cab$p3003s1 == 5 & p3003s1_ind_cab$clase == 1)  # 5.839.326
p3003s1_5_cab <- sum(p3003s1_5_cab$Freq)
# Menos de una vez por semana
p3003s1_6_cab <- p3003s1_ind_cab %>% filter(p3003s1_ind_cab$p3003s1 == 6 & p3003s1_ind_cab$clase == 1)  # 3.533.302
p3003s1_6_cab <- sum(p3003s1_6_cab$Freq)

# total p3003s1
p3003s1_total_cab <- p3003s1_1_cab+p3003s1_2_cab+p3003s1_3_cab+p3003s1_4_cab+p3003s1_5_cab+p3003s1_6_cab  # 19.382.921
##################################################################################################################################################################################
# 39. ¿Durante los últimos 12 meses ____ tuvo que ser hospitalizado/a?
p6133_ind_cab <- data.frame(svytable(~fex_c + p6133 + clase,
                                 design = BD_survey))
# Si
p6133_1_cab <- p6133_ind_cab %>% filter(p6133_ind_cab$p6133 == 1 & p6133_ind_cab$clase == 1)  # 1.516.726
p6133_1_cab <- sum(p6133_1_cab$Freq)
# No
p6133_2_cab <- p6133_ind_cab %>% filter(p6133_ind_cab$p6133 == 2 & p6133_ind_cab$clase == 1)  # 37.551.878
p6133_2_cab <- sum(p6133_2_cab$Freq)

# total p6133
p6133_total_cab <- p6133_1_cab+p6133_2_cab  # 39.068.605
####################################################################################################################################################################################
# 40. ¿Cuáles de las siguientes fuentes se utilizaron para cubrir los costos de esta hospitalización (incluya consulta médica, exámenes y medicamentos)?
# 40.1 EPS o entidad de seguridad social en la cual está afiliado/a 
p8560s1_ind_cab <- data.frame(svytable(~fex_c + p8560s1 + clase,
                                   design = BD_survey))
# Si
p8560s1_1_cab <- p8560s1_ind_cab %>% filter(p8560s1_ind_cab$p8560s1 == 1 & p8560s1_ind_cab$clase == 1)  # 1.328.576
p8560s1_1_cab <- sum(p8560s1_1_cab$Freq)
# No
p8560s1_2_cab <- p8560s1_ind_cab %>% filter(p8560s1_ind_cab$p8560s1 == 2 & p8560s1_ind_cab$clase == 1)  # 188.149
p8560s1_2_cab <- sum(p8560s1_2_cab$Freq)

# total p8560s1
p8560s1_total_cab <- p8560s1_1_cab+p8560s1_2_cab  # 1.516.726
###############################################################################################################################################################################################
# 40.2 Plan o seguro voluntario (seguro médico, plan complementario o medicina prepagada)
p8560s2_ind_cab <- data.frame(svytable(~fex_c + p8560s2 + clase,
                                   design = BD_survey))
# Si
p8560s2_1_cab <- p8560s2_ind_cab %>% filter(p8560s2_ind_cab$p8560s2 == 1 & p8560s2_ind_cab$clase == 1)  # 97.201
p8560s2_1_cab <- sum(p8560s2_1_cab$Freq)
# No
p8560s2_2_cab <- p8560s2_ind_cab %>% filter(p8560s2_ind_cab$p8560s2 == 2 & p8560s2_ind_cab$clase == 1)  # 1.419.524
p8560s2_2_cab <- sum(p8560s2_2_cab$Freq)

# total p8560s2
p8560s2_total_cab <- p8560s2_1_cab+p8560s2_2_cab  # 1.516.726
##################################################################################################################################################################################
# 40.3 Seguro obligatorio de accidentes de tránsito (SOAT)
p8560s3_ind_cab <- data.frame(svytable(~fex_c + p8560s3 + clase,
                                   design = BD_survey))
# Si
p8560s3_1_cab <- p8560s3_ind_cab %>% filter(p8560s3_ind_cab$p8560s3 == 1 & p8560s3_ind_cab$clase == 1)  # 42.745
p8560s3_1_cab <- sum(p8560s3_1_cab$Freq)
# No
p8560s3_2_cab <- p8560s3_ind_cab %>% filter(p8560s3_ind_cab$p8560s3 == 2 & p8560s3_ind_cab$clase == 1)  # 1.473.981
p8560s3_2_cab <- sum(p8560s3_2_cab$Freq)

# total p8560s3
p8560s3_total_cab <- p8560s3_1_cab+p8560s3_2_cab  # 1.516.726
#######################################################################################################################################################################################
# 40.4 Secretaria de salud o la alcaldía
p8560s4_ind_cab <- data.frame(svytable(~fex_c + p8560s4 + clase,
                                   design = BD_survey))
# Si
p8560s4_1_cab <- p8560s4_ind_cab %>% filter(p8560s4_ind_cab$p8560s4 == 1 & p8560s4_ind_cab$clase == 1)  # 26.853
p8560s4_1_cab <- sum(p8560s4_1_cab$Freq)
# No
p8560s4_2_cab <- p8560s4_ind_cab %>% filter(p8560s4_ind_cab$p8560s4 == 2 & p8560s4_ind_cab$clase == 1)  # 1.489.872
p8560s4_2_cab <- sum(p8560s4_2_cab$Freq)

# total p8560s4
p8560s4_total_cab <- p8560s4_1_cab+p8560s4_2_cab  # 1.516.726
########################################################################################################################################################################################
# 40.5 Recursos propios o familiares
p8560s5_ind_cab <- data.frame(svytable(~fex_c + p8560s5 + clase,
                                   design = BD_survey))
# Si
p8560s5_1_cab <- p8560s5_ind_cab %>% filter(p8560s5_ind_cab$p8560s5 == 1 & p8560s5_ind_cab$clase == 1)  # 100.776
p8560s5_1_cab <- sum(p8560s5_1_cab$Freq)
# No
p8560s5_2_cab <- p8560s5_ind_cab %>% filter(p8560s5_ind_cab$p8560s5 == 2 & p8560s5_ind_cab$clase == 1)  # 1.415.949
p8560s5_2_cab <- sum(p8560s5_2_cab$Freq)

# total p8560s5
p8560s5_total_cab <- p8560s5_1_cab+p8560s5_2_cab  # 1.516.726
##########################################################################################################################################################################################
# 41. ¿Cuánto pagó en total ____ por esta hospitalización?
# 41.1 A traves de EPS
p3189s1_ind_cab <- data.frame(svytable(~fex_c + p3189s1 + clase,
                                   design = BD_survey))
p3189s1_cab <- p3189s1_ind_cab %>% filter(p3189s1_ind_cab$p3189s1 == 1 & p3189s1_ind_cab$clase == 1)   # 1.389.167
p3189s1_cab <- sum(p3189s1_cab$Freq)
# 41.1 A1 Valor a traves de EPS
#p3189s1a1_ind_cab <- data.frame(svytable(~fex_c + p3189s1a1 + fecha,
                                     #design = BD_survey))
#p3189s1a1_cab <- table(BD_ecv_ind$p3189s1a1)

###########################################################################################################################################################################################
# 41.2 Servicio particular o plan voluntario (seguro médico, plan complementario o medicina prepagada)
p3189s2_ind_cab <- data.frame(svytable(~fex_c + p3189s2 + clase,
                                   design = BD_survey))
p3189s2_cab <- p3189s2_ind_cab %>% filter(p3189s2_ind_cab$p3189s2 == 1 & p3189s2_ind_cab$clase == 1)   # 141.061
p3189s2_cab <- sum(p3189s2_cab$Freq)

# 45.2.1 A1 Valor servicio particular o plan voluntario
#p3189s2a1_ind_cab <- data.frame(svytable(~fex_c + p3189s2a1 + clase,
                                     #design = BD_survey))
#p3189s2a1_cab <- table(BD_ecv_ind$p3189s2a1)
#############################################################################################################################################################################################
# 42. Considera que la calidad del servicio en esta hospitalizacion fue:
p8561_ind_cab <- data.frame(svytable(~fex_c + p8561 + clase,
                                 design = BD_survey))
# Muy buena
p8561_1_cab <- p8561_ind_cab %>% filter(p8561_ind_cab$p8561 == 1 & p8561_ind_cab$clase == 1)  # 410.673
p8561_1_cab <- sum(p8561_1_cab$Freq)
# Buena
p8561_2_cab <- p8561_ind_cab %>% filter(p8561_ind_cab$p8561 == 2 & p8561_ind_cab$clase == 1)  # 989.894
p8561_2_cab <- sum(p8561_2_cab$Freq)
# Mala
p8561_3_cab <- p8561_ind_cab %>% filter(p8561_ind_cab$p8561 == 3 & p8561_ind_cab$clase == 1)  # 99.211
p8561_3_cab <- sum(p8561_3_cab$Freq)
# Muy mala
p8561_4_cab <- p8561_ind_cab %>% filter(p8561_ind_cab$p8561 == 4 & p8561_ind_cab$clase == 1)  # 16.946
p8561_4_cab <- sum(p8561_4_cab$Freq)

# total p8561 
p8561_total_cab <- p8561_1_cab+p8561_2_cab+p8561_3_cab+p8561_4_cab  # 1.516.726
#################################################################################################################################################################################################
# 43. ¿... ha estado embarazada?
p3335_ind_cab <- data.frame(svytable(~fex_c + p3335 + clase,
                                 design = BD_survey))
# Si
p3335_1_cab <- p3335_ind_cab %>% filter(p3335_ind_cab$p3335 == 1 & p3335_ind_cab$clase == 1)  # 11.558.294
p3335_1_cab <- sum(p3335_1_cab$Freq)
# No
p3335_2_cab <- p3335_ind_cab %>% filter(p3335_ind_cab$p3335 == 2 & p3335_ind_cab$clase == 1)  # 6.139.230
p3335_2_cab <- sum(p3335_2_cab$Freq)

# total p3335
p3335_total_cab <- p3335_1_cab+p3335_2_cab  # 17.697.525
##################################################################################################################################################################################################
# 43.a ¿Cuantos hijos nacidos vivos ha tenido?
#p3335s1_ind_cab <- data.frame(svytable(~fex_c + p3335s1 + clase,
                                   #design = BD_survey))
#p3335s1_cab <- table(p3335s1_ind_cab$p3335s1)
# 43.b ¿A que edad tuvo su primer hijo?
#p3335s1a1_ind_cab <- data.frame(svytable(~fex_c + p3335s1a1 + clase,
                                     #design = BD_survey))
#p3335s1a1_cab <- table(BD_ecv_ind$p3335s1a1)
####################################################################################################################################################################################################
# 44. ¿... está embarazada actualmente?
p8584_ind_cab <- data.frame(svytable(~fex_c + p8584 + clase,
                                 design = BD_survey))
# Si
p8584_1_cab <- p8584_ind_cab %>% filter(p8584_ind_cab$p8584 == 1 & p8584_ind_cab$clase == 1)  # 165.869
p8584_1_cab <- sum(p8584_1_cab$Freq)
# No
p8584_2_cab <- p8584_ind_cab %>% filter(p8584_ind_cab$p8584 == 2 & p8584_ind_cab$clase == 1)  # 6.565.134
p8584_2_cab <- sum(p8584_2_cab$Freq)

# total p8584
p8584_total_cab <- p8584_1_cab+p8584_2_cab   # 6.731.003
######################################################################################################################################################################################################
# 45. ¿Asiste a control prenatal?
p5694_ind_cab <- data.frame(svytable(~fex_c + p5694 + clase,
                                 design = BD_survey))
# Si
p5694_1_cab <- p5694_ind_cab %>% filter(p5694_ind_cab$p5694 == 1 & p5694_ind_cab$clase == 1)  # 154.103
p5694_1_cab <- sum(p5694_1_cab$Freq)
# No
p5694_2_cab <- p5694_ind_cab %>% filter(p5694_ind_cab$p5694 == 2 & p5694_ind_cab$clase == 1)  # 11.765
p5694_2_cab <- sum(p5694_2_cab$Freq)

# total p5694
p5694_total_cab <- p5694_1_cab+p5694_2_cab  # 165.869
####################################################################################################################################################################################################
# 46. ¿... tiene el esquema completo de vacunación, según su edad?
p5452_ind_cab <- data.frame(svytable(~fex_c + p5452 + clase,
                                 design = BD_survey))
# Si 
p5452_1_cab <- p5452_ind_cab %>% filter(p5452_ind_cab$p5452 == 1 & p5452_ind_cab$clase == 1)  # 2.833.754
p5452_1_cab <- sum(p5452_1_cab$Freq)
# No
p5452_2_cab <- p5452_ind_cab %>% filter(p5452_ind_cab$p5452 == 2 & p5452_ind_cab$clase == 1)  # 326.512
p5452_2_cab <- sum(p5452_2_cab$Freq)

# p5452 total 
p5452_total_cab <- p5452_1_cab+p5452_2_cab   # 3.160.267
#######################################################################################################################################################################################################
# 47. ¿Llevan a ... a control de crecimiento y desarrollo?
p6161_ind_cab <- data.frame(svytable(~fex_c + p6161 + clase,
                                 design = BD_survey))
# Si 
p6161_1_cab <- p6161_ind_cab %>% filter(p6161_ind_cab$p6161 == 1 & p6161_ind_cab$clase == 1)  # 2.701.036
p6161_1_cab <- sum(p6161_1_cab$Freq)
# No
p6161_2_cab <- p6161_ind_cab %>% filter(p6161_ind_cab$p6161 == 2 & p6161_ind_cab$clase == 1)  # 459.230
p6161_2_cab <- sum(p6161_2_cab$Freq)

# p6161 total 
p6161_total_cab <- p6161_1_cab+p6161_2_cab   # 3.160.267

# 47.1 Cuántas veces lo llevaron durante los ÚLTIMOS 12 MESES
#p6161s1_ind_cab <- data.frame(svytable(~fex_c + p6161s1 + clase,
                                   #design = BD_survey))
#p6161s1_cab <- table(BD_ecv_ind$p6161s1)
###########################################################################################################################################################################################################
# 48. ¿Cuál fue la principal razón para no llevar a ... a un control de crecimiento y desarrollo?
p1089_ind_cab <- data.frame(svytable(~fex_c + p1089 + clase,
                                 design = BD_survey))
# No pensó que fuera necesario llevarlo/a a consulta
p1089_1_cab <- p1089_ind_cab %>% filter(p1089_ind_cab$p1089 == 1 & p1089_ind_cab$clase == 1)  # 37.421
p1089_1_cab <- sum(p1089_1_cab$Freq)
# La consulta es muy cara, no tiene plata
p1089_2_cab <- p1089_ind_cab %>% filter(p1089_ind_cab$p1089 == 2 & p1089_ind_cab$clase == 1)  # 13.691
p1089_2_cab <- sum(p1089_2_cab$Freq)
# El lugar donde lo atienden queda muy lejos / no hay servicio cerca
p1089_3_cab <- p1089_ind_cab %>% filter(p1089_ind_cab$p1089 == 3 & p1089_ind_cab$clase == 1)  # 23.288
p1089_3_cab <- sum(p1089_3_cab$Freq)
# No pudo dejar el trabajo/no tuvo tiempo
p1089_4_cab <- p1089_ind_cab %>% filter(p1089_ind_cab$p1089 == 4 & p1089_ind_cab$clase == 1)  # 16.065
p1089_4_cab <- sum(p1089_4_cab$Freq)
# No está afiliado/a a EPS o a régimen subsidiado
p1089_5_cab <- p1089_ind_cab %>% filter(p1089_ind_cab$p1089 == 5 & p1089_ind_cab$clase == 1)  # 147.188
p1089_5_cab <- sum(p1089_5_cab$Freq)
# No consiguió cita cercana en el tiempo o lo atienden muy mal
p1089_6_cab <- p1089_ind_cab %>% filter(p1089_ind_cab$p1089 == 6 & p1089_ind_cab$clase == 1)  # 60.331
p1089_6_cab <- sum(p1089_6_cab$Freq)
# Los trámites en la EPS/IPS son muy complicados
p1089_7_cab <- p1089_ind_cab %>% filter(p1089_ind_cab$p1089 == 7 & p1089_ind_cab$clase == 1)  # 26.507
p1089_7_cab <- sum(p1089_7_cab$Freq)
# Considera que no está en edad o es recién nacido/a
p1089_8_cab <- p1089_ind_cab %>% filter(p1089_ind_cab$p1089 == 8 & p1089_ind_cab$clase == 1)  # 41.680
p1089_8_cab <- sum(p1089_8_cab$Freq)
# No tiene registro civil de nacimiento
p1089_9_cab <- p1089_ind_cab %>% filter(p1089_ind_cab$p1089 == 9 & p1089_ind_cab$clase == 1)  # 13.399
p1089_9_cab <- sum(p1089_9_cab$Freq)
# Cambio de EPS o de municipio
p1089_10_cab <- p1089_ind_cab %>% filter(p1089_ind_cab$p1089 == 10 & p1089_ind_cab$clase == 1)  # 18.302
p1089_10_cab <- sum(p1089_10_cab$Freq)
# Otra
p1089_11_cab <- p1089_ind_cab %>% filter(p1089_ind_cab$p1089 == 11 & p1089_ind_cab$clase == 1)  # 61.352
p1089_11_cab <- sum(p1089_11_cab$Freq)

# total p1089
p1089_total_cab <- p1089_1_cab+p1089_2_cab+p1089_3_cab+p1089_4_cab+p1089_5_cab+p1089_6_cab+p1089_7_cab+
  p1089_8_cab+p1089_9_cab+p1089_10_cab+p1089_11_cab   # 459.230



#############################################################################################################################################
#############################################################################################################################################
########################################## NIVEL NACIONAL DESGLOSADO POR AREA         #######################################################
########################################## PARTE 3                                    #######################################################
#############################################################################################################################################
########################################## CENTROS POBLADOS, INSPECCIÓN DE POLICÍA O  #######################################################
########################################## CORREGIMIENTOS - ÁREA RURAL DISPERSA      ########################################################
# 1. p6090 = ¿ ... está afiliado/a,  (cotizante o es beneficiario/a) a alguna entidad de seguridad social en salud? (Entidad promotora de salud [EPS] o entidad promotora de salud subsidiada [EPS-S] )
p6090_ind_rural <- data.frame(svytable(~fex_c + p6090 + clase,
                                       design = BD_survey))
# Sí 
p6090_1_rural <- p6090_ind_rural %>% filter(p6090_ind_rural$p6090 == 1 & p6090_ind_rural$clase == 2)  # 36.198.054
p6090_1_rural <- sum(p6090_1_rural$Freq)

# No
p6090_2_rural <- p6090_ind_rural %>% filter(p6090_ind_rural$p6090 == 2 & p6090_ind_rural$clase == 2)  # 2.685.076
p6090_2_rural <- sum(p6090_2_rural$Freq)                            

# No sabe / no informa
p6090_9_rural <- p6090_ind_rural %>% filter(p6090_ind_rural$p6090 == 9 & p6090_ind_rural$clase == 2)   # 185.474
p6090_9_rural <- sum(p6090_9_rural$Freq)

# Total p6090
p6090_total_rural <- p6090_1_rural + p6090_2_rural + p6090_9_rural   # 39.068.605
#######################################################################################################################################################################################################

# 2. p768 = ¿Por qué razón principal no está afiliado/a de una entidad de seguridad social en salud? (Entidad promotora de salud [EPS] o entidad promotora de salud subsidiada [EPS-S] 
p768_ind_rural <- data.frame(svytable(~fex_c + p768 + clase,
                                      design = BD_survey)) 
# Por falta de dinero
p768_1_rural <- p768_ind_rural %>% filter(p768_ind_rural$p768 == 1 & p768_ind_rural$clase == 2 ) # 204.502
p768_1_rural <- sum(p768_1_rural$Freq)

# Muchos trámites
p768_2_rural <- p768_ind_rural %>% filter(p768_ind_rural$p768 == 2 & p768_ind_rural$clase == 2)  # 434.790
p768_2_rural <- sum(p768_2_rural$Freq)

# No le interesa o descuido
p768_3_rural <- p768_ind_rural %>% filter(p768_ind_rural$p768 == 3 & p768_ind_rural$clase == 2)  # 150.325
p768_3_rural <- sum(p768_3_rural$Freq)

# No sabe que debe afiliarse 
p768_4_rural <- p768_ind_rural %>% filter(p768_ind_rural$p768 == 4 & p768_ind_rural$clase == 2)  # 55.310
p768_4_rural <- sum(p768_4_rural$Freq)

# No esta vinculado/a laboralmente a una empresa o entidad (Usted o la persona de la que es beneificiario/a)
p768_5_rural <- p768_ind_rural %>% filter(p768_ind_rural$p768 == 5 & p768_ind_rural$clase == 2)  # 255.930
p768_5_rural <- sum(p768_5_rural$Freq)

# Esta en tramite de afiliacion
p768_6_rural <- p768_ind_rural %>% filter(p768_ind_rural$p768 == 6 & p768_ind_rural$clase == 2)  # 454.999
p768_6_rural <- sum(p768_6_rural$Freq)

# Problemas con el sisben (no lo han visitado/a, afiliado/a en otro municipio, lo/a desvincularon, le asignaron puntaje alto)
p768_7_rural <- p768_ind_rural %>% filter(p768_ind_rural$p768 == 7 & p768_ind_rural$clase == 2)  # 399.746
p768_7_rural <- sum(p768_7_rural$Freq)

# Otra razon
p768_8_rural <- p768_ind_rural %>% filter(p768_ind_rural$p768 == 8 & p768_ind_rural$clase == 2)  # 729.470
p768_8_rural <- sum(p768_8_rural$Freq)

# Total p768
p768_total_rural <- p768_1_rural + p768_2_rural + p768_3_rural + p768_4_rural + p768_5_rural + p768_6_rural + p768_7_rural + p768_8_rural  # 2.685.076
########################################################################################################################################################

# 3. ¿A cuál de los siguientes regímenes de seguridad social en salud está afiliado/a?
p6100_ind_rural <- data.frame(svytable(~fex_c + p6100 + clase,
                                       design = BD_survey))
# Contributivo (EPS)
p6100_1_rural <- p6100_ind_rural %>% filter(p6100_ind_rural$p6100 == 1 & p6100_ind_rural$clase == 2) # 19.351.790
p6100_1_rural <- sum(p6100_1_rural$Freq)

# Especial (Fuerzas Armadas, Ecopetrol, universidades públicas, magisterio)
p6100_2_rural <- p6100_ind_rural %>% filter(p6100_ind_rural$p6100 == 2 & p6100_ind_rural$clase == 2) # 936.649
p6100_2_rural <- sum(p6100_2_rural$Freq)

# Subsidiado (EPS-S)
p6100_3_rural <- p6100_ind_rural %>% filter(p6100_ind_rural$p6100 == 3 & p6100_ind_rural$clase == 2) # 15.782.439
p6100_3_rural <- sum(p6100_3_rural$Freq)

# No sabe, no informa
p6100_9_rural <- p6100_ind_rural %>% filter(p6100_ind_rural$p6100 ==9 & p6100_ind_rural$clase == 2) # 127.174
p6100_9_rural <- sum(p6100_9_rural$Freq)

# Total p6100
p6100_total_rural <- p6100_1_rural + p6100_2_rural + p6100_3_rural + p6100_9_rural  # 36.198.054
####################################################################################################################################################

# 4. ¿Quién paga mensualmente por la afiliación de ...?
p6115_ind_rural <- data.frame(svytable(~fex_c + p6115 + clase,
                                       design = BD_survey))

# Paga una parte y otra la empresa o patron
p6115_1_rural <- p6115_ind_rural %>% filter(p6115_ind_rural$p6115 == 1 & p6115_ind_rural$clase == 2) # 6.212.965
p6115_1_rural <- sum(p6115_1_rural$Freq)

# Le descuentan de la pension
p6115_2_rural <- p6115_ind_rural %>% filter(p6115_ind_rural$p6115 == 2 & p6115_ind_rural$clase == 2) # 1.814.128
p6115_2_rural <- sum(p6115_2_rural$Freq)

# Paga la totalidad de la afiliacion
p6115_3_rural <- p6115_ind_rural %>% filter(p6115_ind_rural$p6115 == 3 & p6115_ind_rural$clase == 2) # 2.525.898
p6115_3_rural <- sum(p6115_3_rural$Freq)

# Paga completamente la empresa o patron donde trabaja o trabajó
p6115_4_rural <- p6115_ind_rural %>% filter(p6115_ind_rural$p6115 == 4 & p6115_ind_rural$clase == 2) # 610.609
p6115_4_rural <- sum(p6115_4_rural$Freq)

# No paga, es beneficiario/a
p6115_5_rural <- p6115_ind_rural %>% filter(p6115_ind_rural$p6115 == 5 & p6115_ind_rural$clase == 2) # 9.124.838
p6115_5_rural <- sum(p6115_5_rural$Freq)

# Total p6115
p6115_total_rural <- p6115_1_rural + p6115_2_rural + p6115_3_rural + p6115_4_rural + p6115_5_rural  # 20.288.440
#####################################################################################################################################################

# 5. ¿De quién es beneficiario/a ...?
p5669_ind_rural <- data.frame(svytable(~fex_c + p5669 + clase,
                                       design = BD_survey))

# De una persona de este hogar
p5669_1_rural <- p5669_ind_rural %>% filter(p5669_ind_rural$p5669 == 1 & p5669_ind_rural$clase == 2) # 7.390.702
p5669_1_rural <- sum(p5669_1_rural$Freq)

# De una persona de otro hogar
p5669_2_rural <- p5669_ind_rural %>% filter(p5669_ind_rural$p5669 == 2 & p5669_ind_rural$clase == 2) # 1.734.136
p5669_2_rural <- sum(p5669_2_rural$Freq)

# Total p5669
p5669_total_rural <- p5669_1_rural + p5669_2_rural  # 9.124.838
######################################################################################################################################################

# Numero Orden
#p5669s1_ind_rural <- data.frame(svytable(~fex_c + p5669s1 + clase,
#design = BD_survey))
#p5669s1_rural <- p5669s1_ind_rural %>% filter(p5669s1_ind_rural$clase == 2)


# 1 <- 16.854
# 2 <- 6.431
# 3 <- 907
# 4 <- 400
# 5 <- 155
# 6 <- 64
# 7 <- 19
# 8 <- 7
# 9 <- 2
# 10 <- 2
# 14 <- 2
# 17<- 1
# -1 <- 1
# -2 <- 7
# -3 <- 1
# -4 <- 2
#######################################################################################################################################################

# 6. ¿Cuánto paga o cuánto le descuentan mensualmente a ... para estar cubierto/a por una entidad de seguridad social en salud? 
#p8551 <- table(BD_ecv_ind$p8551)
#######################################################################################################################################################

# 7. En general, considera que la calidad del servicio de su EPS o de la entidad de seguridad social en salud en la cual ... está afiliado/a es

p6181_ind_rural <- data.frame(svytable(~fex_c + p6181 + clase,
                                       design = BD_survey))

# Muy buena
p6181_1_rural <- p6181_ind_rural %>% filter(p6181_ind_rural$p6181 == 1 & p6181_ind_rural$clase == 2)
p6181_1_rural <- sum(p6181_1_rural$Freq)                             # 4.760.351

# Buena
p6181_2_rural <- p6181_ind_rural %>% filter(p6181_ind_rural$p6181 == 2 & p6181_ind_rural$clase == 2)
p6181_2_rural <- sum(p6181_2_rural$Freq)                            # 26.809.768

# Mala
p6181_3_rural <- p6181_ind_rural %>% filter(p6181_ind_rural$p6181 == 3 & p6181_ind_rural$clase == 2)
p6181_3_rural <- sum(p6181_3_rural$Freq)                            # 3.216.178

# Muy mala
p6181_4_rural <- p6181_ind_rural %>% filter(p6181_ind_rural$p6181 == 4 & p6181_ind_rural$clase == 2)
p6181_4_rural <- sum(p6181_4_rural$Freq)                            # 653.001

# No sabe
p6181_9_rural <- p6181_ind_rural %>% filter(p6181_ind_rural$p6181 == 9 & p6181_ind_rural$clase == 2)
p6181_9_rural <- sum(p6181_9_rural$Freq)                            # 631.579

# total p6181
p6181_total_rural <- p6181_1_rural + p6181_2_rural + p6181_3_rural + p6181_4_rural + p6181_9_rural
########################################################################################################################################################

# 8. ¿Cuál es el aspecto que más influye en su percepción sobre la calidad del servicio de su EPS o de la entidad de seguridad social en salud en la cual se encuentra afiliado/a? 

p798_ind_rural <- data.frame(svytable(~fex_c + p798 + clase,
                                      design = BD_survey))

# Tramites excesivos o dispendiosos
p798_1_rural <- p798_ind_rural %>% filter(p798_ind_rural$p798 == 1 & p798_ind_rural$clase == 2)  # 999.491
p798_1_rural <- sum(p798_1_rural$Freq)

# Mala atencion del personal administrativo o asistencial (medicos, enfermeras, etc)
p798_2_rural <- p798_ind_rural %>% filter(p798_ind_rural$p798 == 2 & p798_ind_rural$clase == 2)  # 510.123
p798_2_rural <- sum(p798_2_rural$Freq)

# Falta de capacidad, conocimientos o habilidad del personal asistencial
p798_3_rural <- p798_ind_rural %>% filter(p798_ind_rural$p798 == 3 & p798_ind_rural$clase == 2)  # 90.574
p798_3_rural <- sum(p798_3_rural$Freq)

# Condiciones deficientes de infraestructura, dotación, mobiliario o accesibilidad para población en condición de discapacidad
p798_4_rural <- p798_ind_rural %>% filter(p798_ind_rural$p798 == 4 & p798_ind_rural$clase == 2)  # 39.096
p798_4_rural <- sum(p798_4_rural$Freq)

# Demora en la asignacion de citas
p798_5_rural <- p798_ind_rural %>% filter(p798_ind_rural$p798 == 5 & p798_ind_rural$clase == 2)  # 1.834.950
p798_5_rural <- sum(p798_5_rural$Freq)

# Demora en la atencion por parte del personal medico
p798_6_rural <- p798_ind_rural %>% filter(p798_ind_rural$p798 == 6 & p798_ind_rural$clase == 2)  # 242.322
p798_6_rural <- sum(p798_6_rural$Freq)

# Problemas relacionados con los medicamentos
p798_7_rural <- p798_ind_rural %>% filter(p798_ind_rural$p798 == 7 & p798_ind_rural$clase == 2)  # 137.595
p798_7_rural <- sum(p798_7_rural$Freq)

# Otro
p798_8_rural <- p798_ind_rural %>% filter(p798_ind_rural$p798 == 8 & p798_ind_rural$clase == 2)  # 15.025
p798_8_rural <- sum(p798_8_rural$Freq)

# Total p798
p798_total_rural <- p798_1_rural + p798_2_rural + p798_3_rural + p798_4_rural + p798_5_rural + p798_6_rural + p798_7_rural + p798_8_rural  # 3.869.179
##############################################################################################################################################################

# 9. ¿Cuáles de los siguientes planes o seguros VOLUNTARIOS de salud tiene_________? Medicina prepagada

p799s2_ind_rural <- data.frame(svytable(~fex_c + p799s2 + clase,
                                        design = BD_survey)) 
# Si
p799s2_1_rural <- p799s2_ind_rural %>% filter(p799s2_ind_rural$p799s2 == 1 & p799s2_ind_rural$clase == 2) # 1.612.601
p799s2_1_rural <- sum(p799s2_1_rural$Freq)

# No
p799s2_2_rural <- p799s2_ind_rural %>% filter(p799s2_ind_rural$p799s2 == 2 & p799s2_ind_rural$clase == 2) # 37.456.003
p799s2_2_rural <- sum(p799s2_2_rural$Freq)   

# total p799s2
p799s2_total_rural <- p799s2_1_rural + p799s2_2_rural
##############################################################################################################################################################

# 9.2 Plan complementario de salud con una EPS
p799s3_ind_rural <- data.frame(svytable(~fex_c + p799s3 + clase,
                                        design = BD_survey))
# Si 
p799s3_1_rural <- p799s3_ind_rural %>% filter(p799s3_ind_rural$p799s3 == 1 & p799s3_ind_rural$clase == 2) # 1.271.847
p799s3_1_rural <- sum(p799s3_1_rural$Freq)

# No
p799s3_2_rural <- p799s3_ind_rural %>% filter(p799s3_ind_rural$p799s3 == 2 & p799s3_ind_rural$clase == 2) # 37.796.757
p799s3_2_rural <- sum(p799s3_2_rural$Freq)  

# p799s3
p799s3_total_rural <- p799s3_1_rural + p799s3_2_rural
##############################################################################################################################################################

# 9.3 Poliza de hospitalizacion o cirugia
p799s1_ind_rural <- data.frame(svytable(~fex_c + p799s1 + clase,
                                        design = BD_survey))
# Si 
p799s1_1_rural <- p799s1_ind_rural %>% filter(p799s1_ind_rural$p799s1 == 1 & p799s1_ind_rural$clase == 2) # 222.884
p799s1_1_rural <- sum(p799s1_1_rural$Freq)

# No
p799s1_2_rural <- p799s1_ind_rural %>% filter(p799s1_ind_rural$p799s1 == 2 & p799s1_ind_rural$clase == 2) # 38.845.720
p799s1_2_rural <- sum(p799s1_2_rural$Freq)   

# total p799s1
p799s1_total_rural <- p799s1_1_rural + p799s1_2_rural
###############################################################################################################################################################

# 9.4 Seguros medicos estudiantiles
p799s4_ind_rural <- data.frame(svytable(~fex_c + p799s4 + clase,
                                        design = BD_survey))
# Si 
p799s4_1_rural <- p799s4_ind_rural %>% filter(p799s4_ind_rural$p799s4 == 1 & p799s4_ind_rural$clase == 2) # 207.740
p799s4_1_rural <- sum(p799s4_1_rural$Freq)

# No
p799s4_2_rural <- p799s4_ind_rural %>% filter(p799s4_ind_rural$p799s4 == 2 & p799s4_ind_rural$clase == 2) # 38.860.864
p799s4_2_rural <- sum(p799s4_2_rural$Freq) 

# total p799s4
p799s4_total_rural <- p799s4_1_rural + p799s4_2_rural
##############################################################################################################################################################

# 9.5 Otro (ambulancia, asistencia medica domiciliaria, etc)
p799s5_ind_rural <- data.frame(svytable(~fex_c + p799s5 + clase,
                                        design = BD_survey))
# Si 
p799s5_1_rural <- p799s5_ind_rural %>% filter(p799s5_ind_rural$p799s5 == 1 & p799s5_ind_rural$clase == 2) # 369.011
p799s5_1_rural <- sum(p799s5_1_rural$Freq)

# No
p799s5_2_rural <- p799s5_ind_rural %>% filter(p799s5_ind_rural$p799s5 == 2 & p799s5_ind_rural$clase == 2) # 38.699.593
p799s5_2_rural <- sum(p799s5_2_rural$Freq) 

# total p799s5
p799s5_total_rural <- p799s5_1_rural + p799s5_2_rural
################################################################################################################################################################

# 10. ¿Cuánto paga o  le descuentan mensualmente a ________ por concepto de estos planes o seguros voluntarios de salud?
#p3176 <- table(BD_ecv_ind$p3176)
################################################################################################################################################################

# 11. El estado de salud de ... en general, es:
p6127_ind_rural <- data.frame(svytable(~fex_c + p6127 + clase,
                                       design = BD_survey))
# Muy bueno
p6127_1_rural <- p6127_ind_rural %>% filter(p6127_ind_rural$p6127 == 1 & p6127_ind_rural$clase == 2)
p6127_1_rural <- sum(p6127_1_rural$Freq)                             # 7.948.033

# Bueno
p6127_2_rural <- p6127_ind_rural %>% filter(p6127_ind_rural$p6127 == 2 & p6127_ind_rural$clase == 2)
p6127_2_rural <- sum(p6127_2_rural$Freq)                             # 26.200.078

# Regular
p6127_3_rural <- p6127_ind_rural %>% filter(p6127_ind_rural$p6127 == 3 & p6127_ind_rural$clase == 2)
p6127_3_rural <- sum(p6127_3_rural$Freq)                            # 4.569.673

# Malo
p6127_4_rural <- p6127_ind_rural %>% filter(p6127_ind_rural$p6127 == 4 & p6127_ind_rural$clase == 2)
p6127_4_rural <- sum(p6127_4_rural$Freq)                            # 350.820

# Total p6127
p6127_total_rural <- p6127_1_rural + p6127_2_rural + p6127_3_rural + p6127_4_rural    # 39.068.605
###############################################################################################################################################################

# 12.1 ¿A....le han diagnosticado alguna enfermedad crónica? (enfermedad de larga duración y prolongados tratamientos como: 
#    enfermedades cardiovasculares-hipertensión, asma, bronquitis crónica, gastritis, lupus, cáncer, gota, leucemia, diabetes, etc.).

p1930_ind_rural <- data.frame(svytable(~fex_c + p1930 + clase,
                                       design = BD_survey))
# Si
p1930_1_rural <- p1930_ind_rural %>% filter(p1930_ind_rural$p1930 == 1 & p1930_ind_rural$clase == 2)
p1930_1_rural <- sum(p1930_1_rural$Freq)                            # 6.436.021

# No
p1930_2_rural <- p1930_ind_rural %>% filter(p1930_ind_rural$p1930 == 2  & p1930_ind_rural$clase == 2)
p1930_2_rural <- sum(p1930_2_rural$Freq)                            # 32.632.583

# total p1930
p1930_total_rural <- p1930_1_rural + p1930_2_rural  # 39.068.605
################################################################################################################################################################

# 12.2 ¿Recibe o recibió tratamiento formulado por el médico?
p1930s1_ind_rural <- data.frame(svytable(~fex_c + p1930s1 + clase,
                                         design = BD_survey))

# Si
p1930s1_1_rural <- p1930s1_ind_rural %>% filter(p1930s1_ind_rural$p1930s1 == 1 & p1930s1_ind_rural$clase == 2)
p1930s1_1_rural <- sum(p1930s1_1_rural$Freq)                                   # 5.789.286

# No
p1930s1_2_rural <- p1930s1_ind_rural %>% filter(p1930s1_ind_rural$p1930s1 == 2  & p1930s1_ind_rural$clase == 2)
p1930s1_2_rural <- sum(p1930s1_2_rural$Freq)                                  # 646.734

# total p1930s1
p1930s1_total_rural <- p1930s1_1_rural + p1930s1_2_rural  # 6.436.021
###############################################################################################################################################################
# 13.1 Dada su condición física y mental, y sin ningún tipo de ayuda, ¿ ... puede: Oir la voz o los sonidos?
p1906s1_ind_rural <- data.frame(svytable(~fex_c + p1906s1 + clase,
                                         design = BD_survey))

# No puede hacerlo
p1906s1_1_rural <- p1906s1_ind_rural %>% filter(p1906s1_ind_rural$p1906s1 == 1 & p1906s1_ind_rural$clase == 2)
p1906s1_1_rural <- sum(p1906s1_1_rural$Freq)                                # 46.999

# Si, con mucha dificultad
p1906s1_2_rural <- p1906s1_ind_rural %>% filter(p1906s1_ind_rural$p1906s1 == 2 & p1906s1_ind_rural$clase == 2)
p1906s1_2_rural <- sum(p1906s1_2_rural$Freq)                                # 300.424

# Si, con alguna dificultad
p1906s1_3_rural <- p1906s1_ind_rural %>% filter(p1906s1_ind_rural$p1906s1 == 3 & p1906s1_ind_rural$clase == 2)
p1906s1_3_rural <- sum(p1906s1_3_rural$Freq)                                # 993.257

# Sin dificultad
p1906s1_4_rural <- p1906s1_ind_rural %>% filter(p1906s1_ind_rural$p1906s1 == 4 & p1906s1_ind_rural$clase == 2)
p1906s1_4_rural <- sum(p1906s1_4_rural$Freq)                                # 37.727.923

# total p1906s1
p1906s1_total_rural <- p1906s1_1_rural + p1906s1_2_rural + p1906s1_3_rural + p1906s1_4_rural  # 39.068.605
################################################################################################################################################################

# 13.2 Hablar o conversar
p1906s2_ind_rural <- data.frame(svytable(~fex_c + p1906s2 + clase,
                                         design = BD_survey))
# No puede hacerlo
p1906s2_1_rural <- p1906s2_ind_rural %>% filter(p1906s2_ind_rural$p1906s2 == 1 & p1906s2_ind_rural$clase == 2)
p1906s2_1_rural <- sum(p1906s2_1_rural$Freq)                                # 164.803

# Si, con mucha dificultad
p1906s2_2_rural <- p1906s2_ind_rural %>% filter(p1906s2_ind_rural$p1906s2 == 2 & p1906s2_ind_rural$clase == 2)
p1906s2_2_rural <- sum(p1906s2_2_rural$Freq)                                # 200.978

# Si, con alguna dificultad
p1906s2_3_rural <- p1906s2_ind_rural %>% filter(p1906s2_ind_rural$p1906s2 == 3 & p1906s2_ind_rural$clase == 2)
p1906s2_3_rural <- sum(p1906s2_3_rural$Freq)                               # 540.821

# Sin dificultad
p1906s2_4_rural <- p1906s2_ind_rural %>% filter(p1906s2_ind_rural$p1906s2 == 4 & p1906s2_ind_rural$clase == 2)
p1906s2_4_rural <- sum(p1906s2_4_rural$Freq)                              # 38.162.001

# total p1906s2
p1906s2_total_rural <- p1906s2_1_rural + p1906s2_2_rural + p1906s2_3_rural + p1906s2_4_rural  # 39.068.605 
################################################################################################################################################################

# 13.3 Ver de verca, de lejos o alrededor
p1906s3_ind_rural <- data.frame(svytable(~fex_c + p1906s3 + clase,
                                         design = BD_survey))
# No puede hacerlo
p1906s3_1_rural <- p1906s3_ind_rural %>% filter(p1906s3_ind_rural$p1906s3 == 1 & p1906s3_ind_rural$clase == 2)
p1906s3_1_rural <- sum(p1906s3_1_rural$Freq)                               # 44.926

# Si, con mucha dificultad
p1906s3_2_rural <- p1906s3_ind_rural %>% filter(p1906s3_ind_rural$p1906s3 == 2 & p1906s3_ind_rural$clase == 2)
p1906s3_2_rural <- sum(p1906s3_2_rural$Freq)                               # 1.228.273

# Si, con alguna dificultad
p1906s3_3_rural <- p1906s3_ind_rural %>% filter(p1906s3_ind_rural$p1906s3 == 3 & p1906s3_ind_rural$clase == 2)
p1906s3_3_rural <- sum(p1906s3_3_rural$Freq)                               # 6.421.120

# Sin dificultad
p1906s3_4_rural <- p1906s3_ind_rural %>% filter(p1906s3_ind_rural$p1906s3 == 4 & p1906s3_ind_rural$clase == 2)
p1906s3_4_rural <- sum(p1906s3_4_rural$Freq)                              # 31.374.284

# Total p1906s3
p1906s3_total_rural <- p1906s3_1_rural + p1906s3_2_rural + p1906s3_3_rural + p1906s3_4_rural  # 39.068.605
################################################################################################################################################################

# 13.4 Mover el cuerpo, caminar o subir y bajar escaleras?
p1906s4_ind_rural <- data.frame(svytable(~fex_c + p1906s4 + clase,
                                         design = BD_survey))
# No puede hacerlo
p1906s4_1_rural <- p1906s4_ind_rural %>% filter(p1906s4_ind_rural$p1906s4 == 1 & p1906s4_ind_rural$clase == 2)
p1906s4_1_rural <- sum(p1906s4_1_rural$Freq)                               # 191.841

# Si, con mucha dificultad
p1906s4_2_rural <- p1906s4_ind_rural %>% filter(p1906s4_ind_rural$p1906s4 == 2 & p1906s4_ind_rural$clase == 2)
p1906s4_2_rural <- sum(p1906s4_2_rural$Freq)                              # 644.176

# Si, con alguna dificultad
p1906s4_3_rural <- p1906s4_ind_rural %>% filter(p1906s4_ind_rural$p1906s4 == 3 & p1906s4_ind_rural$clase == 2)
p1906s4_3_rural <- sum(p1906s4_3_rural$Freq)                              # 1.843.478

# Sin dificultad
p1906s4_4_rural <- p1906s4_ind_rural %>% filter(p1906s4_ind_rural$p1906s4 == 4 & p1906s4_ind_rural$clase == 2)
p1906s4_4_rural <- sum(p1906s4_4_rural$Freq)                               # 36.389.108

# total p1906s4
p1906s4_total_rural <- p1906s4_1_rural + p1906s4_2_rural + p1906s4_3_rural + p1906s4_4_rural   # 39.068.605
#############################################################################################################################################################

# 13.5 Agarrar o mover objetos con las manos?
p1906s5_ind_rural <- data.frame(svytable(~fex_c + p1906s5 + clase,
                                         design = BD_survey))

# No puede hacerlo 
p1906s5_1_rural <- p1906s5_ind_rural %>% filter(p1906s5_ind_rural$p1906s5 == 1 & p1906s5_ind_rural$clase == 2)
p1906s5_1_rural <- sum(p1906s5_1_rural$Freq)                                # 84.951

# Si, con mucha dificultad
p1906s5_2_rural <- p1906s5_ind_rural %>% filter(p1906s5_ind_rural$p1906s5 == 2  & p1906s5_ind_rural$clase == 2)
p1906s5_2_rural <- sum(p1906s5_2_rural$Freq)                               # 267.246

# Si, con alguna dificultad
p1906s5_3_rural <- p1906s5_ind_rural %>% filter(p1906s5_ind_rural$p1906s5 == 3  & p1906s5_ind_rural$clase == 2)
p1906s5_3_rural <- sum(p1906s5_3_rural$Freq)                               # 942.223

# Sin dificultad
p1906s5_4_rural <- p1906s5_ind_rural %>% filter(p1906s5_ind_rural$p1906s5 == 4  & p1906s5_ind_rural$clase == 2)
p1906s5_4_rural <- sum(p1906s5_4_rural$Freq)                              # 37.774.183

#  total p1906s5
p1906s5_total_rural <- p1906s5_1_rural + p1906s5_2_rural + p1906s5_3_rural + p1906s5_4_rural   # 39.068.605
##############################################################################################################################################################

# 13.6 Entender, aprender, recordar o tomar decisiones por sí mismo/a?
p1906s6_ind_rural <- data.frame(svytable(~fex_c + p1906s6 + clase, 
                                         design = BD_survey))

# No puede hacerlo
p1906s6_1_rural <- p1906s6_ind_rural %>% filter(p1906s6_ind_rural$p1906s6 == 1 & p1906s6_ind_rural$clase == 2)
p1906s6_1_rural <- sum(p1906s6_1_rural$Freq)                              # 234.297

# Si, con mucha dificultad
p1906s6_2_rural <- p1906s6_ind_rural %>% filter(p1906s6_ind_rural$p1906s6 == 2 & p1906s6_ind_rural$clase == 2)
p1906s6_2_rural <- sum(p1906s6_2_rural$Freq)                               # 234.030

# Si, con alguna dificultad
p1906s6_3_rural <- p1906s6_ind_rural %>% filter(p1906s6_ind_rural$p1906s6 == 3 & p1906s6_ind_rural$clase == 2)
p1906s6_3_rural <- sum(p1906s6_3_rural$Freq)                                # 682.966

# Sin dificultad
p1906s6_4_rural <- p1906s6_ind_rural %>% filter(p1906s6_ind_rural$p1906s6 == 4 & p1906s6_ind_rural$clase == 2)
p1906s6_4_rural <- sum(p1906s6_4_rural$Freq)                               # 37.917.310

# total p1906s6
p1906s6_total_rural <- p1906s6_1_rural + p1906s6_2_rural + p1906s6_3_rural + p1906s6_4_rural  # 39.068.605
###############################################################################################################################################################

# 13.7 Comer, vestirse o bañarse por sí mismo/a?
p1906s7_ind_rural <- data.frame(svytable(~fex_c + p1906s7 + clase,
                                         design = BD_survey))

# No puede hacerlo 
p1906s7_1_rural <- p1906s7_ind_rural %>% filter(p1906s7_ind_rural$p1906s7 ==1 & p1906s7_ind_rural$clase == 2)
p1906s7_1_rural <- sum(p1906s7_1_rural$Freq)                               # 274.045

# Si, con mucha dificultad
p1906s7_2_rural <- p1906s7_ind_rural %>% filter(p1906s7_ind_rural$p1906s7 == 2 & p1906s7_ind_rural$clase == 2)
p1906s7_2_rural <- sum(p1906s7_2_rural$Freq)                               # 164.761

# Si, con alguna dificultad
p1906s7_3_rural <- p1906s7_ind_rural %>% filter(p1906s7_ind_rural$p1906s7 == 3 & p1906s7_ind_rural$clase == 2)
p1906s7_3_rural <- sum(p1906s7_3_rural$Freq)                               # 598.750

# Sin dificultad
p1906s7_4_rural <- p1906s7_ind_rural %>% filter(p1906s7_ind_rural$p1906s7 == 4 & p1906s7_ind_rural$clase == 2)
p1906s7_4_rural <- sum(p1906s7_4_rural$Freq)                               # 38.031.048

# total p1906s7
p1906s7_total_rural <- p1906s7_1_rural + p1906s7_2_rural + p1906s7_3_rural + p1906s7_4_rural  # 39.068.605
###########################################################################################################################################################

# 13.8 Relacionarse o interactuar con las demás personas?
p1906s8_ind_rural <- data.frame(svytable(~fex_c + p1906s8 + clase,
                                         design = BD_survey))

# No puede hacerlo 
p1906s8_1_rural <- p1906s8_ind_rural %>% filter(p1906s8_ind_rural$p1906s8 == 1 & p1906s8_ind_rural$clase == 2)
p1906s8_1_rural <- sum(p1906s8_1_rural$Freq)                               # 147.579

# Si, con mucha dificultad
p1906s8_2_rural <- p1906s8_ind_rural %>% filter(p1906s8_ind_rural$p1906s8 == 2 & p1906s8_ind_rural$clase == 2)
p1906s8_2_rural <- sum(p1906s8_2_rural$Freq)                               # 165.645

# Si, con alguna dificultad
p1906s8_3_rural <- p1906s8_ind_rural %>% filter(p1906s8_ind_rural$p1906s8 == 3 & p1906s8_ind_rural$clase == 2)
p1906s8_3_rural <- sum(p1906s8_3_rural$Freq)                               # 440.559

# Sin dificultad
p1906s8_4_rural <- p1906s8_ind_rural %>% filter(p1906s8_ind_rural$p1906s8 == 4 & p1906s8_ind_rural$clase == 2)  
p1906s8_4_rural <- sum(p1906s8_4_rural$Freq)                               # 38.314.820

# total p1906s8
p1906s8_total_rural <- p1906s8_1_rural + p1906s8_2_rural + p1906s8_3_rural+ p1906s8_4_rural  # 39.068.605
#############################################################################################################################################################

# 14.1 ¿Esta dificultad de ... fue ocasionada: ¿Esta dificultad (Oír la voz o los sonidos) de ... fue ocasionada
p1908s1_ind_rural <- data.frame(svytable(~fex_c + p1908s1 + clase,
                                         design = BD_survey))
# Porque nació así
p1908s1_1_rural <- p1908s1_ind_rural %>% filter(p1908s1_ind_rural$p1908s1 == 1 & p1908s1_ind_rural$clase == 2)
p1908s1_1_rural <- sum(p1908s1_1_rural$Freq)                                 # 61.645

# Por enfermedad
p1908s1_2_rural <- p1908s1_ind_rural %>% filter(p1908s1_ind_rural$p1908s1 == 2 & p1908s1_ind_rural$clase == 2)
p1908s1_2_rural <- sum(p1908s1_2_rural$Freq)                                 # 92.201

# Por accidente laboral o enfermedad profesional
p1908s1_3_rural <- p1908s1_ind_rural %>% filter(p1908s1_ind_rural$p1908s1 == 3 & p1908s1_ind_rural$clase == 2)
p1908s1_3_rural <- sum(p1908s1_3_rural$Freq)                                 # 16.727

# Por otro tipo de accidente
p1908s1_4_rural <- p1908s1_ind_rural %>% filter(p1908s1_ind_rural$p1908s1 == 4 & p1908s1_ind_rural$clase == 2)
p1908s1_4_rural <- sum(p1908s1_4_rural$Freq)                                # 17.210

# Por edad avanzada
p1908s1_5_rural <- p1908s1_ind_rural %>% filter(p1908s1_ind_rural$p1908s1 == 5 & p1908s1_ind_rural$clase == 2)
p1908s1_5_rural <- sum(p1908s1_5_rural$Freq)                                # 139.455

# Por el conflicto armado
p1908s1_6_rural <- p1908s1_ind_rural %>% filter(p1908s1_ind_rural$p1908s1 == 6 & p1908s1_ind_rural$clase == 2)
p1908s1_6_rural <- sum(p1908s1_6_rural$Freq)                               # 3.113

# Por violencia NO asociada al conflicto armado
p1908s1_7_rural <- p1908s1_ind_rural %>% filter(p1908s1_ind_rural$p1908s1 == 7 & p1908s1_ind_rural$clase == 2)
p1908s1_7_rural <- sum(p1908s1_7_rural$Freq)                               # 3.084

# Por otra causa
p1908s1_8_rural <- p1908s1_ind_rural %>% filter(p1908s1_ind_rural$p1908s1 == 8 & p1908s1_ind_rural$clase == 2)
p1908s1_8_rural <- sum(p1908s1_8_rural$Freq)                               # 7.390

# No sabe
p1908s1_9_rural <- p1908s1_ind_rural %>% filter(p1908s1_ind_rural$p1908s1 == 9 & p1908s1_ind_rural$clase == 2)
p1908s1_9_rural <- sum(p1908s1_9_rural$Freq)                               # 6.594

# total 1908s1
p1908s1_total_rural <- p1908s1_1_rural + p1908s1_2_rural + p1908s1_3_rural + p1908s1_4_rural + p1908s1_5_rural + p1908s1_6_rural + p1908s1_7_rural + p1908s1_8_rural + p1908s1_9_rural   # 347.423
###########################################################################################################################################

# 14.2 ¿Esta dificultad (Hablar o conversar) de ... fue ocasionada:
p1908s2_ind_rural <- data.frame(svytable(~fex_c + p1908s2 + clase,
                                         design = BD_survey))
# Porque nació así
p1908s2_1_rural <- p1908s2_ind_rural %>% filter(p1908s2_ind_rural$p1908s2 == 1 & p1908s2_ind_rural$clase == 2)
p1908s2_1_rural <- sum(p1908s2_1_rural$Freq)                                 # 128.873

# Por enfermedad
p1908s2_2_rural <- p1908s2_ind_rural %>% filter(p1908s2_ind_rural$p1908s2 == 2 & p1908s2_ind_rural$clase == 2)
p1908s2_2_rural <- sum(p1908s2_2_rural$Freq)                                 # 117.819

# Por accidente laboral o enfermedad profesional
p1908s2_3_rural <- p1908s2_ind_rural %>% filter(p1908s2_ind_rural$p1908s2 == 3 & p1908s2_ind_rural$clase == 2)
p1908s2_3_rural <- sum(p1908s2_3_rural$Freq)                                 # 1.812

# Por otro tipo de accidente
p1908s2_4_rural <- p1908s2_ind_rural %>% filter(p1908s2_ind_rural$p1908s2 == 4 & p1908s2_ind_rural$clase == 2)
p1908s2_4_rural <- sum(p1908s2_4_rural$Freq)                                # 8.188

# Por edad avanzada
p1908s2_5_rural <- p1908s2_ind_rural %>% filter(p1908s2_ind_rural$p1908s2 == 5 & p1908s2_ind_rural$clase == 2)
p1908s2_5_rural <- sum(p1908s2_5_rural$Freq)                                # 27.989

# Por el conflicto armado
p1908s2_6_rural <- p1908s2_ind_rural %>% filter(p1908s2_ind_rural$p1908s2 == 6 & p1908s2_ind_rural$clase == 2)
p1908s2_6_rural <- sum(p1908s2_6_rural$Freq)                               # 0

# Por violencia NO asociada al conflicto armado
p1908s2_7_rural <- p1908s2_ind_rural %>% filter(p1908s2_ind_rural$p1908s2 == 7 & p1908s2_ind_rural$clase == 2)
p1908s2_7_rural <- sum(p1908s2_7_rural$Freq)                               # 3.630

# Por otra causa
p1908s2_8_rural <- p1908s2_ind_rural %>% filter(p1908s2_ind_rural$p1908s2 == 8 & p1908s2_ind_rural$clase == 2)
p1908s2_8_rural <- sum(p1908s2_8_rural$Freq)                               # 50.512

# No sabe
p1908s2_9_rural <- p1908s2_ind_rural %>% filter(p1908s2_ind_rural$p1908s2 == 9 & p1908s2_ind_rural$clase == 2)
p1908s2_9_rural <- sum(p1908s2_9_rural$Freq)                               # 26.955

# total 1908s1
p1908s2_total_rural <- p1908s2_1_rural + p1908s2_2_rural + p1908s2_3_rural + p1908s2_4_rural + p1908s2_5_rural + p1908s2_6_rural + p1908s2_7_rural +
  p1908s2_8_rural + p1908s2_9_rural   # 365.782
#####################################################################################################################################################

# 14.3 ¿Esta dificultad (Ver de cerca, de lejos o alrededor) de ... fue ocasionada:
p1908s3_ind_rural <- data.frame(svytable(~fex_c + p1908s3 + clase,
                                         design = BD_survey))
# Porque nació así
p1908s3_1_rural <- p1908s3_ind_rural %>% filter(p1908s3_ind_rural$p1908s3 == 1 & p1908s3_ind_rural$clase == 2)
p1908s3_1_rural <- sum(p1908s3_1_rural$Freq)                                 # 171.457

# Por enfermedad
p1908s3_2_rural <- p1908s3_ind_rural %>% filter(p1908s3_ind_rural$p1908s3 == 2 & p1908s3_ind_rural$clase == 2)
p1908s3_2_rural <- sum(p1908s3_2_rural$Freq)                                 # 537.370

# Por accidente laboral o enfermedad profesional
p1908s3_3_rural <- p1908s3_ind_rural %>% filter(p1908s3_ind_rural$p1908s3 == 3 & p1908s3_ind_rural$clase == 2)
p1908s3_3_rural <- sum(p1908s3_3_rural$Freq)                                 # 23.062

# Por otro tipo de accidente
p1908s3_4_rural <- p1908s3_ind_rural %>% filter(p1908s3_ind_rural$p1908s3 == 4 & p1908s3_ind_rural$clase == 2)
p1908s3_4_rural <- sum(p1908s3_4_rural$Freq)                                # 25.221

# Por edad avanzada
p1908s3_5_rural <- p1908s3_ind_rural %>% filter(p1908s3_ind_rural$p1908s3 == 5 & p1908s3_ind_rural$clase == 2)
p1908s3_5_rural <- sum(p1908s3_5_rural$Freq)                                # 447.024

# Por el conflicto armado
p1908s3_6_rural <- p1908s3_ind_rural %>% filter(p1908s3_ind_rural$p1908s3 == 6 & p1908s3_ind_rural$clase == 2)
p1908s3_6_rural <- sum(p1908s3_6_rural$Freq)                               # 680

# Por violencia NO asociada al conflicto armado
p1908s3_7_rural <- p1908s3_ind_rural %>% filter(p1908s3_ind_rural$p1908s3 == 7 & p1908s3_ind_rural$clase == 2)
p1908s3_7_rural <- sum(p1908s3_7_rural$Freq)                               # 2.247

# Por otra causa
p1908s3_8_rural <- p1908s3_ind_rural %>% filter(p1908s3_ind_rural$p1908s3 == 8 & p1908s3_ind_rural$clase == 2)
p1908s3_8_rural <- sum(p1908s3_8_rural$Freq)                               # 30.275

# No sabe
p1908s3_9_rural <- p1908s3_ind_rural %>% filter(p1908s3_ind_rural$p1908s3 == 9 & p1908s3_ind_rural$clase == 2)
p1908s3_9_rural <- sum(p1908s3_9_rural$Freq)                               # 35.859

# total 1908s1
p1908s3_total_rural <- p1908s3_1_rural + p1908s3_2_rural + p1908s3_3_rural + p1908s3_4_rural + p1908s3_5_rural + p1908s3_6_rural +
  p1908s3_7_rural + p1908s3_8_rural + p1908s3_9_rural   # 1.273.199
###################################################################################################################################################################3

# 14. 4 ¿Esta dificultad (Mover el cuerpo, caminar o subir y bajar escaleras) de ... fue ocasionada:
p1908s4_ind_rural <- data.frame(svytable(~fex_c + p1908s4 + clase,
                                         design = BD_survey))
# Porque nació así
p1908s4_1_rural <- p1908s4_ind_rural %>% filter(p1908s4_ind_rural$p1908s4 == 1 & p1908s4_ind_rural$clase == 2)
p1908s4_1_rural <- sum(p1908s4_1_rural$Freq)                                 # 55.332

# Por enfermedad
p1908s4_2_rural <- p1908s4_ind_rural %>% filter(p1908s4_ind_rural$p1908s4 == 2 & p1908s4_ind_rural$clase == 2)
p1908s4_2_rural <- sum(p1908s4_2_rural$Freq)                                 # 394.668

# Por accidente laboral o enfermedad profesional
p1908s4_3_rural <- p1908s4_ind_rural %>% filter(p1908s4_ind_rural$p1908s4 == 3 & p1908s4_ind_rural$clase == 2)
p1908s4_3_rural <- sum(p1908s4_3_rural$Freq)                                 # 32.604

# Por otro tipo de accidente
p1908s4_4_rural <- p1908s4_ind_rural %>% filter(p1908s4_ind_rural$p1908s4 == 4 & p1908s4_ind_rural$clase == 2)
p1908s4_4_rural <- sum(p1908s4_4_rural$Freq)                                # 89.407

# Por edad avanzada
p1908s4_5_rural <- p1908s4_ind_rural %>% filter(p1908s4_ind_rural$p1908s4 == 5 & p1908s4_ind_rural$clase == 2)
p1908s4_5_rural <- sum(p1908s4_5_rural$Freq)                                # 186.349

# Por el conflicto armado
p1908s4_6_rural <- p1908s4_ind_rural %>% filter(p1908s4_ind_rural$p1908s4 == 6 & p1908s4_ind_rural$clase == 2)
p1908s4_6_rural <- sum(p1908s4_6_rural$Freq)                               # 3.975

# Por violencia NO asociada al conflicto armado
p1908s4_7_rural <- p1908s4_ind_rural %>% filter(p1908s4_ind_rural$p1908s4 == 7 & p1908s4_ind_rural$clase == 2)
p1908s4_7_rural <- sum(p1908s4_7_rural$Freq)                               # 4.040

# Por otra causa
p1908s4_8_rural <- p1908s4_ind_rural %>% filter(p1908s4_ind_rural$p1908s4 == 8 & p1908s4_ind_rural$clase == 2)
p1908s4_8_rural <- sum(p1908s4_8_rural$Freq)                               # 49.450

# No sabe
p1908s4_9_rural <- p1908s4_ind_rural %>% filter(p1908s4_ind_rural$p1908s4 == 9 & p1908s4_ind_rural$clase == 2)
p1908s4_9_rural <- sum(p1908s4_9_rural$Freq)                               # 20.188

# total 1908s1 cab
p1908s4_total_rural <- p1908s4_1_rural + p1908s4_2_rural + p1908s4_3_rural + p1908s4_4_rural + p1908s4_5_rural + p1908s4_6_rural +
  p1908s4_7_rural + p1908s4_8_rural + p1908s4_9_rural   # 836.017
############################################################################################################################################################3

# 14.5 ¿Esta dificultad (Agarrar o mover objetos con las manos) de ... fue ocasionada:
p1908s5_ind_rural <- data.frame(svytable(~fex_c + p1908s5 + clase,
                                         design = BD_survey))
# Porque nació así
p1908s5_1_rural <- p1908s5_ind_rural %>% filter(p1908s5_ind_rural$p1908s5 == 1 & p1908s5_ind_rural$clase == 2)
p1908s5_1_rural <- sum(p1908s5_1_rural$Freq)                                 # 37.893

# Por enfermedad
p1908s5_2_rural <- p1908s5_ind_rural %>% filter(p1908s5_ind_rural$p1908s5 == 2 & p1908s5_ind_rural$clase == 2)
p1908s5_2_rural <- sum(p1908s5_2_rural$Freq)                                 # 174.758

# Por accidente laboral o enfermedad profesional
p1908s5_3_rural <- p1908s5_ind_rural %>% filter(p1908s5_ind_rural$p1908s5 == 3 & p1908s5_ind_rural$clase == 2)
p1908s5_3_rural <- sum(p1908s5_3_rural$Freq)                                 # 19.191

# Por otro tipo de accidente
p1908s5_4_rural <- p1908s5_ind_rural %>% filter(p1908s5_ind_rural$p1908s5 == 4 & p1908s5_ind_rural$clase == 2)
p1908s5_4_rural <- sum(p1908s5_4_rural$Freq)                                # 36.925

# Por edad avanzada
p1908s5_5_rural <- p1908s5_ind_rural %>% filter(p1908s5_ind_rural$p1908s5 == 5 & p1908s5_ind_rural$clase == 2)
p1908s5_5_rural <- sum(p1908s5_5_rural$Freq)                                # 55.863

# Por el conflicto armado
p1908s5_6_rural <- p1908s5_ind_rural %>% filter(p1908s5_ind_rural$p1908s5 == 6 & p1908s5_ind_rural$clase == 2)
p1908s5_6_rural <- sum(p1908s5_6_rural$Freq)                               # 552

# Por violencia NO asociada al conflicto armado
p1908s5_7_rural <- p1908s5_ind_rural %>% filter(p1908s5_ind_rural$p1908s5 == 7 & p1908s5_ind_rural$clase == 2)
p1908s5_7_rural <- sum(p1908s5_7_rural$Freq)                               # 3.203

# Por otra causa
p1908s5_8_rural <- p1908s5_ind_rural %>% filter(p1908s5_ind_rural$p1908s5 == 8 & p1908s5_ind_rural$clase == 2)
p1908s5_8_rural <- sum(p1908s5_8_rural$Freq)                               # 17.737

# No sabe
p1908s5_9_rural <- p1908s5_ind_rural %>% filter(p1908s5_ind_rural$p1908s5 == 9 & p1908s5_ind_rural$clase == 2)
p1908s5_9_rural <- sum(p1908s5_9_rural$Freq)                               # 6.071

# total 1908s1
p1908s5_total_rural <- p1908s5_1_rural + p1908s5_2_rural + p1908s5_3_rural + p1908s5_4_rural + p1908s5_5_rural + p1908s5_6_rural +
  p1908s5_7_rural + p1908s5_8_rural + p1908s5_9_rural   # 352.197
#############################################################################################################################################################

# 14.6 ¿Esta dificultad (Entender, aprender, recordar o tomar decisiones por sí mismo/a) de ... fue ocasionada:
p1908s6_ind_rural <- data.frame(svytable(~fex_c + p1908s6 + clase,
                                         design = BD_survey))
# Porque nació así
p1908s6_1_rural <- p1908s6_ind_rural %>% filter(p1908s6_ind_rural$p1908s6 == 1 & p1908s6_ind_rural$clase == 2)
p1908s6_1_rural <- sum(p1908s6_1_rural$Freq)                                 # 143.059

# Por enfermedad
p1908s6_2_rural <- p1908s6_ind_rural %>% filter(p1908s6_ind_rural$p1908s6 == 2 & p1908s6_ind_rural$clase == 2)
p1908s6_2_rural <- sum(p1908s6_2_rural$Freq)                                 # 144.252

# Por accidente laboral o enfermedad profesional
p1908s6_3_rural <- p1908s6_ind_rural %>% filter(p1908s6_ind_rural$p1908s6 == 3 & p1908s6_ind_rural$clase == 2)
p1908s6_3_rural <- sum(p1908s6_3_rural$Freq)                                 # 3.479

# Por otro tipo de accidente
p1908s6_4_rural <- p1908s6_ind_rural %>% filter(p1908s6_ind_rural$p1908s6 == 4 & p1908s6_ind_rural$clase == 2)
p1908s6_4_rural <- sum(p1908s6_4_rural$Freq)                                # 4.653

# Por edad avanzada
p1908s6_5_rural <- p1908s6_ind_rural %>% filter(p1908s6_ind_rural$p1908s6 == 5 & p1908s6_ind_rural$clase == 2)
p1908s6_5_rural <- sum(p1908s6_5_rural$Freq)                                # 56.987

# Por el conflicto armado
p1908s6_6_rural <- p1908s6_ind_rural %>% filter(p1908s6_ind_rural$p1908s6 == 6 & p1908s6_ind_rural$clase == 2)
p1908s6_6_rural <- sum(p1908s6_6_rural$Freq)                               # 0

# Por violencia NO asociada al conflicto armado
p1908s6_7_rural <- p1908s6_ind_rural %>% filter(p1908s6_ind_rural$p1908s6 == 7 & p1908s6_ind_rural$clase == 2)
p1908s6_7_rural <- sum(p1908s6_7_rural$Freq)                               # 3.311

# Por otra causa
p1908s6_8_rural <- p1908s6_ind_rural %>% filter(p1908s6_ind_rural$p1908s6 == 8 & p1908s6_ind_rural$clase == 2)
p1908s6_8_rural <- sum(p1908s6_8_rural$Freq)                               # 79.061

# No sabe
p1908s6_9_rural <- p1908s6_ind_rural %>% filter(p1908s6_ind_rural$p1908s6 == 9 & p1908s6_ind_rural$clase == 2)
p1908s6_9_rural <- sum(p1908s6_9_rural$Freq)                               # 33.522

# total 1908s1
p1908s6_total_rural <- p1908s6_1_rural + p1908s6_2_rural + p1908s6_3_rural + p1908s6_4_rural + p1908s6_5_rural + p1908s6_6_rural +
  p1908s6_7_rural + p1908s6_8_rural + p1908s6_9_rural   # 468.327
##############################################################################################################################################################

# 14.7 ¿Esta dificultad (Comer, vestirse o bañarse por sí mismo/a) de ... fue ocasionada:
p1908s7_ind_rural <- data.frame(svytable(~fex_c + p1908s7 + clase,
                                         design = BD_survey))
# Porque nació así
p1908s7_1_rural <- p1908s7_ind_rural %>% filter(p1908s7_ind_rural$p1908s7 == 1 & p1908s7_ind_rural$clase == 2)
p1908s7_1_rural <- sum(p1908s7_1_rural$Freq)                                 # 58.950

# Por enfermedad
p1908s7_2_rural <- p1908s7_ind_rural %>% filter(p1908s7_ind_rural$p1908s7 == 2 & p1908s7_ind_rural$clase == 2)
p1908s7_2_rural <- sum(p1908s7_2_rural$Freq)                                 # 148.370

# Por accidente laboral o enfermedad profesional
p1908s7_3_rural <- p1908s7_ind_rural %>% filter(p1908s7_ind_rural$p1908s7 == 3 & p1908s7_ind_rural$clase == 2)
p1908s7_3_rural <- sum(p1908s7_3_rural$Freq)                                 # 7.219

# Por otro tipo de accidente
p1908s7_4_rural <- p1908s7_ind_rural %>% filter(p1908s7_ind_rural$p1908s7 == 4 & p1908s7_ind_rural$clase == 2)
p1908s7_4_rural <- sum(p1908s7_4_rural$Freq)                                # 20.719

# Por edad avanzada
p1908s7_5_rural <- p1908s7_ind_rural %>% filter(p1908s7_ind_rural$p1908s7 == 5 & p1908s7_ind_rural$clase == 2)
p1908s7_5_rural <- sum(p1908s7_5_rural$Freq)                                # 48.247

# Por el conflicto armado
p1908s7_6_rural <- p1908s7_ind_rural %>% filter(p1908s7_ind_rural$p1908s7 == 6 & p1908s7_ind_rural$clase == 2)
p1908s7_6_rural <- sum(p1908s7_6_rural$Freq)                               # 0

# Por violencia NO asociada al conflicto armado
p1908s7_7_rural <- p1908s7_ind_rural %>% filter(p1908s7_ind_rural$p1908s7 == 7 & p1908s7_ind_rural$clase == 2)
p1908s7_7_rural <- sum(p1908s7_7_rural$Freq)                               # 3.258

# Por otra causa
p1908s7_8_rural <- p1908s7_ind_rural %>% filter(p1908s7_ind_rural$p1908s7 == 8 & p1908s7_ind_rural$clase == 2)
p1908s7_8_rural <- sum(p1908s7_8_rural$Freq)                               # 115.083

# No sabe
p1908s7_9_rural <- p1908s7_ind_rural %>% filter(p1908s7_ind_rural$p1908s7 == 9 & p1908s7_ind_rural$clase == 2)
p1908s7_9_rural <- sum(p1908s7_9_rural$Freq)                               # 36.956

# total 1908s1
p1908s7_total_rural <- p1908s7_1_rural + p1908s7_2_rural + p1908s7_3_rural + p1908s7_4_rural + p1908s7_5_rural + p1908s7_6_rural +
  p1908s7_7_rural + p1908s7_8_rural + p1908s7_9_rural   # 438.806
##############################################################################################################################################################

# 14.8 ¿Esta dificultad (Relacionarse o interactuar con las demás personas?) de ... fue ocasionada:
p1908s8_ind_rural <- data.frame(svytable(~fex_c + p1908s8 + clase,
                                         design = BD_survey))
# Porque nació así
p1908s8_1_rural <- p1908s8_ind_rural %>% filter(p1908s8_ind_rural$p1908s8 == 1 & p1908s8_ind_rural$clase == 2)
p1908s8_1_rural <- sum(p1908s8_1_rural$Freq)                                 # 96.697

# Por enfermedad
p1908s8_2_rural <- p1908s8_ind_rural %>% filter(p1908s8_ind_rural$p1908s8 == 2 & p1908s8_ind_rural$clase == 2)
p1908s8_2_rural <- sum(p1908s8_2_rural$Freq)                                 # 115.511

# Por accidente laboral o enfermedad profesional
p1908s8_3_rural <- p1908s8_ind_rural %>% filter(p1908s8_ind_rural$p1908s8 == 3 & p1908s8_ind_rural$clase == 2)
p1908s8_3_rural <- sum(p1908s8_3_rural$Freq)                                 # 1.812

# Por otro tipo de accidente
p1908s8_4_rural <- p1908s8_ind_rural %>% filter(p1908s8_ind_rural$p1908s8 == 4 & p1908s8_ind_rural$clase == 2)
p1908s8_4_rural <- sum(p1908s8_4_rural$Freq)                                # 3.261

# Por edad avanzada
p1908s8_5_rural <- p1908s8_ind_rural %>% filter(p1908s8_ind_rural$p1908s8 == 5 & p1908s8_ind_rural$clase == 2)
p1908s8_5_rural <- sum(p1908s8_5_rural$Freq)                                # 23.327

# Por el conflicto armado
p1908s8_6_rural <- p1908s8_ind_rural %>% filter(p1908s8_ind_rural$p1908s8 == 6 & p1908s8_ind_rural$clase == 2)
p1908s8_6_rural <- sum(p1908s8_6_rural$Freq)                               # 99

# Por violencia NO asociada al conflicto armado
p1908s8_7_rural <- p1908s8_ind_rural %>% filter(p1908s8_ind_rural$p1908s8 == 7 & p1908s8_ind_rural$clase == 2)
p1908s8_7_rural <- sum(p1908s8_7_rural$Freq)                               # 3.168

# Por otra causa
p1908s8_8_rural <- p1908s8_ind_rural %>% filter(p1908s8_ind_rural$p1908s8 == 8 & p1908s8_ind_rural$clase == 2)
p1908s8_8_rural <- sum(p1908s8_8_rural$Freq)                               # 47.961

# No sabe
p1908s8_9_rural <- p1908s8_ind_rural %>% filter(p1908s8_ind_rural$p1908s8 == 9 & p1908s8_ind_rural$clase == 2)
p1908s8_9_rural <- sum(p1908s8_9_rural$Freq)                               # 21.386

# total 1908s1
p1908s8_total_rural <- p1908s8_1_rural + p1908s8_2_rural + p1908s8_3_rural + p1908s8_4_rural + p1908s8_5_rural + p1908s8_6_rural +
  p1908s8_7_rural + p1908s8_8_rural + p1908s8_9_rural   # 313.225
#########################################################################################################################################################################################

# 15.1 ¿Para estas dificultades ____ utiliza de manera permanente: Gafas, lentes de contacto, lentes intraoculares, programa computacional adaptado, regleta Braille, perro guía, otros?
p1909s1_ind_rural <- data.frame(svytable(~fex_c + p1909s1 + clase,
                                         design = BD_survey))

# Si
p1909s1_1_rural <- p1909s1_ind_rural %>% filter(p1909s1_ind_rural$p1909s1 == 1 & p1909s1_ind_rural$clase == 2)
p1909s1_1_rural <- sum(p1909s1_1_rural$Freq)                                # 1.319.503

# No
p1909s1_2_rural <- p1909s1_ind_rural %>% filter(p1909s1_ind_rural$p1909s1 == 2 & p1909s1_ind_rural$clase == 2)
p1909s1_2_rural <- sum(p1909s1_2_rural$Freq)                                # 1.156.824

# total p1909s1
p1909s1_total_rural <- p1909s1_1_rural + p1909s1_2_rural                        # 2.476.328
###########################################################################################################################################################################################

# 15.2 Bastón, silla de ruedas, muletas, caminador?
p1909s2_ind_rural <- data.frame(svytable(~fex_c + p1909s2 + clase,
                                         design = BD_survey))

# Si
p1909s2_1_rural <- p1909s2_ind_rural %>% filter(p1909s2_ind_rural$p1909s2 == 1 & p1909s2_ind_rural$clase == 2)
p1909s2_1_rural <- sum(p1909s2_1_rural$Freq)                                # 497.756

# No
p1909s2_2_rural <- p1909s2_ind_rural %>% filter(p1909s2_ind_rural$p1909s2 == 2 & p1909s2_ind_rural$clase == 2)
p1909s2_2_rural <- sum(p1909s2_2_rural$Freq)                                # 1.978.572

# total p1909s2
p1909s2_total_rural <- p1909s2_1_rural + p1909s2_2_rural                    # 2.476.328
###########################################################################################################################################################################################

# 15.3 Audífonos medicados, implantes cocleares, otros?
p1909s3_ind_rural <- data.frame(svytable(~fex_c + p1909s3 + clase,
                                         design = BD_survey))

# Si
p1909s3_1_rural <- p1909s3_ind_rural %>% filter(p1909s3_ind_rural$p1909s3 == 1 & p1909s3_ind_rural$clase == 2)
p1909s3_1_rural <- sum(p1909s3_1_rural$Freq)                                # 115.456

# No
p1909s3_2_rural <- p1909s3_ind_rural %>% filter(p1909s3_ind_rural$p1909s3 == 2 & p1909s3_ind_rural$clase == 2)
p1909s3_2_rural <- sum(p1909s3_2_rural$Freq)                                # 2.360.872

# total p1909s3
p1909s3_total_rural <- p1909s3_1_rural + p1909s3_2_rural                    # 2.476.328
###########################################################################################################################################################################################

# 15.4 Ayuda de otras personas?
p1909s4_ind_rural <- data.frame(svytable(~fex_c + p1909s4 + clase,
                                         design = BD_survey))

# Si
p1909s4_1_rural <- p1909s4_ind_rural %>% filter(p1909s4_ind_rural$p1909s4 == 1 & p1909s4_ind_rural$clase == 2)
p1909s4_1_rural <- sum(p1909s4_1_rural$Freq)                                # 658.627

# No
p1909s4_2_rural <- p1909s4_ind_rural %>% filter(p1909s4_ind_rural$p1909s4 == 2 & p1909s4_ind_rural$clase == 2)
p1909s4_2_rural <- sum(p1909s4_2_rural$Freq)                                # 1.817.701

# total p1909s4
p1909s4_total_rural <- p1909s4_1_rural + p1909s4_2_rural                    # 2.476.328
#####################################################################################################################################################################

# 15.5 Medicamentos o terapias?
p1909s5_ind_rural <- data.frame(svytable(~fex_c + p1909s5 + clase,
                                         design = BD_survey))

# Si
p1909s5_1_rural <- p1909s5_ind_rural %>% filter(p1909s5_ind_rural$p1909s5 == 1 & p1909s5_ind_rural$clase == 2)
p1909s5_1_rural <- sum(p1909s5_1_rural$Freq)                                # 696.613

# No
p1909s5_2_rural <- p1909s5_ind_rural %>% filter(p1909s5_ind_rural$p1909s5 == 2 & p1909s5_ind_rural$clase == 2)
p1909s5_2_rural <- sum(p1909s5_2_rural$Freq)                                # 1.779.715

# total p1909s5
p1909s5_total_rural <- p1909s5_1_rural + p1909s5_2_rural                    # 2.476.328
############################################################################################################################################################################

# 15.6 Prácticas de medicina ancestral?
p1909s6_ind_rural <- data.frame(svytable(~fex_c + p1909s6 + clase,
                                         design = BD_survey))

# Si
p1909s6_1_rural <- p1909s6_ind_rural %>% filter(p1909s6_ind_rural$p1909s6 == 1 & p1909s6_ind_rural$clase == 2)
p1909s6_1_rural <- sum(p1909s6_1_rural$Freq)                                # 14.842

# No
p1909s6_2_rural <- p1909s6_ind_rural %>% filter(p1909s6_ind_rural$p1909s6 == 2 & p1909s6_ind_rural$clase == 2)
p1909s6_2_rural <- sum(p1909s6_2_rural$Freq)                                # 2.461.485

# total p1909s6
p1909s6_total_rural <- p1909s6_1_rural + p1909s6_2_rural                    # 2.476.328
#############################################################################################################################################################################

# 16. ¿Quién se ocupa principalmente del cuidado de ...?
p6126_ind_rural <- data.frame(svytable(~fex_c + p6126 + clase,
                                       design = BD_survey))
# Una persona del hogar
p6126_1_rural <- p6126_ind_rural %>% filter(p6126_ind_rural$p6126 == 1 & p6126_ind_rural$clase == 2)
p6126_1_rural <- sum(p6126_1_rural$Freq)                            # 999.354

# Una persona de otro hogar no remunerada
p6126_2_rural <- p6126_ind_rural %>% filter(p6126_ind_rural$p6126 == 2 & p6126_ind_rural$clase == 2)
p6126_2_rural <- sum(p6126_2_rural$Freq)                            # 122.006

# Una persona de otro hogar remunerada
p6126_3_rural <- p6126_ind_rural %>% filter(p6126_ind_rural$p6126 == 3 & p6126_ind_rural$clase == 2)
p6126_3_rural <- sum(p6126_3_rural$Freq)                            # 49.717

# Permanece solo/a
p6126_4_rural <- p6126_ind_rural %>% filter(p6126_ind_rural$p6126 == 4 & p6126_ind_rural$clase == 2)
p6126_4_rural <- sum(p6126_4_rural$Freq)                            # 110.974

# No requiere cuidado
p6126_5_rural <- p6126_ind_rural %>% filter(p6126_ind_rural$p6126 == 5 & p6126_ind_rural$clase == 2)
p6126_5_rural <- sum(p6126_5_rural$Freq)                            # 1.194.275

# total p6126
p6126_total_rural <- p6126_1_rural + p6126_2_rural + p6126_3_rural + p6126_4_rural + p6126_5_rural  # 2.476.328
##############################################################################################################################################################################

# 16.1 Numero Orden
p6126s1_ind_rural <- data.frame(svytable(~fex_c + p6126s1 + clase,
                                         design = BD_survey))
p6126s1_1_rural <- p6126s1_ind_rural %>% filter(p6126s1_ind_rural$p6126s1 == 1 & p6126s1_ind_rural$clase == 2)
p6126s1_1_rural <- sum(p6126s1_1_rural$Freq)                                  # 462.510

p6126s1_2_rural <- p6126s1_ind_rural %>% filter(p6126s1_ind_rural$p6126s1 == 2 & p6126s1_ind_rural$clase == 2)
p6126s1_2_rural <- sum(p6126s1_2_rural$Freq)                                  # 395.417

p6126s1_3_rural <- p6126s1_ind_rural %>% filter(p6126s1_ind_rural$p6126s1 == 3 & p6126s1_ind_rural$clase == 2)
p6126s1_3_rural <- sum(p6126s1_3_rural$Freq)                                  # 84.945

p6126s1_4_rural <- p6126s1_ind_rural %>% filter(p6126s1_ind_rural$p6126s1 == 4 & p6126s1_ind_rural$clase == 2)
p6126s1_4_rural <- sum(p6126s1_4_rural$Freq)                                  # 27.988

p6126s1_5_rural <- p6126s1_ind_rural %>% filter(p6126s1_ind_rural$p6126s1 == 5 & p6126s1_ind_rural$clase == 2)
p6126s1_5_rural <- sum(p6126s1_5_rural$Freq)                                  # 16.377

p6126s1_6_rural <- p6126s1_ind_rural %>% filter(p6126s1_ind_rural$p6126s1 == 6 & p6126s1_ind_rural$clase == 2)
p6126s1_6_rural <- sum(p6126s1_6_rural$Freq)                                  # 7.876

p6126s1_7_rural <- p6126s1_ind_rural %>% filter(p6126s1_ind_rural$p6126s1 == 7 & p6126s1_ind_rural$clase == 2)
p6126s1_7_rural <- sum(p6126s1_7_rural$Freq)                                  # 2.690

p6126s1_8_rural <- p6126s1_ind_rural %>% filter(p6126s1_ind_rural$p6126s1 == 8 & p6126s1_ind_rural$clase == 2)
p6126s1_8_rural <- sum(p6126s1_8_rural$Freq)                                  # 49

p6126s1_9_rural <- p6126s1_ind_rural %>% filter(p6126s1_ind_rural$p6126s1 == 9 & p6126s1_ind_rural$clase == 2)
p6126s1_9_rural <- sum(p6126s1_9_rural$Freq)                                  # 1.072

p6126s1_11_rural <- p6126s1_ind_rural %>% filter(p6126s1_ind_rural$p6126s1 == 11 & p6126s1_ind_rural$clase == 2)   
p6126s1_11_rural <- sum(p6126s1_11_rural$Freq)                                # 2,65

p6126s1_13_rural <- p6126s1_ind_rural %>% filter(p6126s1_ind_rural$p6126s1 == 13 & p6126s1_ind_rural$clase == 2)   
p6126s1_13_rural <- sum(p6126s1_13_rural$Freq)                                # 208

p6126s1_total_rural <- p6126s1_1_rural + p6126s1_2_rural + p6126s1_3_rural + p6126s1_4_rural + p6126s1_5_rural + p6126s1_6_rural +
  p6126s1_7_rural + p6126s1_8_rural + p6126s1_9_rural + p6126s1_11_rural + p6126s1_13_rural  # 999.139

#############################################################################################################################################################################
# 16.2 Sexo
p6126s2_ind_rural <- data.frame(svytable(~fex_c + p6126s2 + clase,
                                         design = BD_survey))
# Hombre
p6126s2_1_rural <- p6126s2_ind_rural %>% filter(p6126s2_ind_rural$p6126s2 == 1 & p6126s2_ind_rural$clase == 2)
p6126s2_1_rural <- sum(p6126s2_1_rural$Freq)                                  # 19.958

# Mujer
p6126s2_2_rural <- p6126s2_ind_rural %>% filter(p6126s2_ind_rural$p6126s2 == 2 & p6126s2_ind_rural$clase == 2)
p6126s2_2_rural <- sum(p6126s2_2_rural$Freq)                                 # 151.766

# total p6126s2
p6126s2_total_rural <- p6126s2_1_rural + p6126s2_2_rural  # 171.724
#############################################################################################################################################################################
# 16.3 ¿Esta persona tuvo que dejar de trabajar para dedicarse al cuidado de...?
p6126s3_ind_rural <- data.frame(svytable(~fex_c + p6126s3 + clase,
                                         design = BD_survey))

# Si 
p6126s3_1_rural <- p6126s3_ind_rural %>% filter(p6126s3_ind_rural$p6126s3 == 1 & p6126s3_ind_rural$clase == 2)
p6126s3_1_rural <- sum(p6126s3_1_rural$Freq)                               # 320.570

# No
p6126s3_2_rural <- p6126s3_ind_rural %>% filter(p6126s3_ind_rural$p6126s3 == 2 & p6126s3_ind_rural$clase == 2)
p6126s3_2_rural <- sum(p6126s3_2_rural$Freq)                               # 678.784

# total p6126s3
p6126s3_total_rural <- p6126s3_1_rural + p6126s3_2_rural  # 999.354
##############################################################################################################################################################################
# 17. En los últimos 30 días, ... ¿tuvo alguna enfermedad, accidente , problema odontológico o algún otro problema de salud que no haya implicado hospitalización?
p5665_ind_rural <- data.frame(svytable(~fex_c + p5665 + clase,
                                       design = BD_survey))

# Si
p5665_1_rural <- p5665_ind_rural %>% filter(p5665_ind_rural$p5665 == 1 & p5665_ind_rural$clase == 2)
p5665_1_rural <- sum(p5665_1_rural$Freq)                            # 1.440.555

# No
p5665_2_rural <- p5665_ind_rural %>% filter(p5665_ind_rural$p5665 == 2 & p5665_ind_rural$clase == 2)
p5665_2_rural <- sum(p5665_2_rural$Freq)                           # 37.628.049

# total p5665
p5665_total_rural <- p5665_1_rural + p5665_2_rural  # 39.068.605
###############################################################################################################################################################################
# 18. Por ese problema de salud, ¿durante cuántos días en total dejó ... de realizar sus actividades normales? Número de dias
p6134_ind_rural <- data.frame(svytable(~fex_c + p6134 + clase,
                                       design = BD_survey))
# 0 dias
p6134_0_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 0 & p6134_ind_rural$clase == 2)     
p6134_0_rural <- sum(p6134_0_rural$Freq)
# 1 dias
p6134_1_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 1 & p6134_ind_rural$clase == 2)     
p6134_1_rural <- sum(p6134_1_rural$Freq)
# 2 dias
p6134_2_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 2 & p6134_ind_rural$clase == 2)     
p6134_2_rural <- sum(p6134_2_rural$Freq)
# 3 dias
p6134_3_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 3 & p6134_ind_rural$clase == 2)     
p6134_3_rural <- sum(p6134_3_rural$Freq)
# 4 dias
p6134_4_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 4 & p6134_ind_rural$clase == 2)     
p6134_4_rural <- sum(p6134_4_rural$Freq)
# 5 dias
p6134_5_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 5 & p6134_ind_rural$clase == 2)     
p6134_5_rural <- sum(p6134_5_rural$Freq)
# 6 dias
p6134_6_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 6 & p6134_ind_rural$clase == 2)     
p6134_6_rural <- sum(p6134_6_rural$Freq)
# 7 dias
p6134_7_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 7 & p6134_ind_rural$clase == 2)     
p6134_7_rural <- sum(p6134_7_rural$Freq)
# 8 dias
p6134_8_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 8 & p6134_ind_rural$clase == 2)     
p6134_8_rural <- sum(p6134_8_rural$Freq)
# 9 dias
p6134_9_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 9 & p6134_ind_rural$clase == 2)     
p6134_9_rural <- sum(p6134_9_rural$Freq)
# 10 dias
p6134_10_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 10 & p6134_ind_rural$clase == 2)     
p6134_10_rural <- sum(p6134_10_rural$Freq)
# 11 dias
p6134_11_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 11 & p6134_ind_rural$clase == 2)     
p6134_11_rural <- sum(p6134_11_rural$Freq)
# 12 dias
p6134_12_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 12 & p6134_ind_rural$clase == 2)     
p6134_12_rural <- sum(p6134_12_rural$Freq)
# 13 dias
p6134_13_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 13 & p6134_ind_rural$clase == 2)     
p6134_13_rural <- sum(p6134_13_rural$Freq)
# 14 dias
p6134_14_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 14 & p6134_ind_rural$clase == 2)     
p6134_14_rural <- sum(p6134_14_rural$Freq)
# 15 dias
p6134_15_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 15 & p6134_ind_rural$clase == 2)     
p6134_15_rural <- sum(p6134_15_rural$Freq)
# 16 dias
p6134_16_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 16 & p6134_ind_rural$clase == 2)     
p6134_16_rural <- sum(p6134_16_rural$Freq)
# 17 dias
p6134_17_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 17 & p6134_ind_rural$clase == 2)     
p6134_17_rural <- sum(p6134_17_rural$Freq)
# 18 dias
p6134_18_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 18 & p6134_ind_rural$clase == 2)     
p6134_18_rural <- sum(p6134_18_rural$Freq)
# 19 dias
p6134_19_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 19 & p6134_ind_rural$clase == 2)     
p6134_19_rural <- sum(p6134_19_rural$Freq)
# 20 dias
p6134_20_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 20 & p6134_ind_rural$clase == 2)     
p6134_20_rural <- sum(p6134_20_rural$Freq)
# 21 dias
p6134_21_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 21 & p6134_ind_rural$clase == 2)     
p6134_21_rural <- sum(p6134_21_rural$Freq)
# 22 dias
p6134_22_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 22 & p6134_ind_rural$clase == 2)     
p6134_22_rural <- sum(p6134_22_rural$Freq)
# 23 dias
p6134_23_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 23 & p6134_ind_rural$clase == 2)     
p6134_23_rural <- sum(p6134_23_rural$Freq)
# 24 dias
p6134_24_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 24 & p6134_ind_rural$clase == 2)     
p6134_24_rural <- sum(p6134_24_rural$Freq)
# 25 dias
p6134_25_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 25 & p6134_ind_rural$clase == 2)     
p6134_25_rural <- sum(p6134_25_rural$Freq)
# 26 dias
p6134_26_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 26 & p6134_ind_rural$clase == 2)     
p6134_26_rural <- sum(p6134_26_rural$Freq)
# 27 dias
p6134_27_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 27 & p6134_ind_rural$clase == 2)     
p6134_27_rural <- sum(p6134_27_rural$Freq)
# 28 dias
p6134_28_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 28 & p6134_ind_rural$clase == 2)     
p6134_28_rural <- sum(p6134_28_rural$Freq)
#29 dias
p6134_29_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 29 & p6134_ind_rural$clase == 2)     
p6134_29_rural <- sum(p6134_29_rural$Freq)
# 30 dias
p6134_30_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 30 & p6134_ind_rural$clase == 2)     
p6134_30_rural <- sum(p6134_30_rural$Freq)
# 31 dias
p6134_31_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 31 & p6134_ind_rural$clase == 2)     
p6134_31_rural <- sum(p6134_31_rural$Freq)
# 35 dias
p6134_35_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 35 & p6134_ind_rural$clase == 2)     
p6134_35_rural <- sum(p6134_35_rural$Freq)
# 36 dias
p6134_36_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 36 & p6134_ind_rural$clase == 2)     
p6134_36_rural <- sum(p6134_36_rural$Freq)
# 37 dias
p6134_37_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 37 & p6134_ind_rural$clase == 2)     
p6134_37_rural <- sum(p6134_37_rural$Freq)
# 38 dias
p6134_38_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 38 & p6134_ind_rural$clase == 2)     
p6134_38_rural <- sum(p6134_38_rural$Freq)
# 40 dias
p6134_40_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 40 & p6134_ind_rural$clase == 2)     
p6134_40_rural <- sum(p6134_40_rural$Freq)
# 45 dias
p6134_45_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 45 & p6134_ind_rural$clase == 2)     
p6134_45_rural <- sum(p6134_45_rural$Freq)
# 50 dias
p6134_50_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 50 & p6134_ind_rural$clase == 2)     
p6134_50_rural <- sum(p6134_50_rural$Freq)
# 60 dias
p6134_60_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 60 & p6134_ind_rural$clase == 2)     
p6134_60_rural <- sum(p6134_60_rural$Freq)
# 64 dias
p6134_64_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 64 & p6134_ind_rural$clase == 2)     
p6134_64_rural <- sum(p6134_64_rural$Freq)
# 68 dias
p6134_68_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 68 & p6134_ind_rural$clase == 2)     
p6134_68_rural <- sum(p6134_68_rural$Freq)
# 72 dias
p6134_72_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 72 & p6134_ind_rural$clase == 2)     
p6134_72_rural <- sum(p6134_72_rural$Freq)
# 75 dias
p6134_75_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 75 & p6134_ind_rural$clase == 2)     
p6134_75_rural <- sum(p6134_75_rural$Freq)
# 80 dias
p6134_80_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 80 & p6134_ind_rural$clase == 2)     
p6134_80_rural <- sum(p6134_80_rural$Freq)
# 90 dias
p6134_90_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 90 & p6134_ind_rural$clase == 2)     
p6134_90_rural <- sum(p6134_90_rural$Freq)
# 98 dias
p6134_98_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 98 & p6134_ind_rural$clase == 2)     
p6134_98_rural <- sum(p6134_98_rural$Freq)
# 99 dias
p6134_99_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 99 & p6134_ind_rural$clase == 2)     
p6134_99_rural <- sum(p6134_99_rural$Freq)
# 100 dias
p6134_100_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 100 & p6134_ind_rural$clase == 2)     
p6134_100_rural <- sum(p6134_100_rural$Freq)
# 120 dias
p6134_120_rural <- p6134_ind_rural %>% filter(p6134_ind_rural$p6134 == 120 & p6134_ind_rural$clase == 2)     
p6134_120_rural <- sum(p6134_120_rural$Freq)

# p6134 total
p6134_total_rural <- p6134_0_rural+p6134_1_rural+p6134_2_rural+p6134_3_rural+p6134_4_rural+p6134_5_rural+p6134_6_rural+p6134_7_rural+
  p6134_8_rural+p6134_9_rural+p6134_10_rural+p6134_11_rural+p6134_12_rural+p6134_13_rural+p6134_14_rural+p6134_15_rural+p6134_16_rural+
  p6134_17_rural+p6134_18_rural+p6134_19_rural+p6134_20_rural+p6134_21_rural+p6134_22_rural+p6134_23_rural+p6134_24_rural+p6134_25_rural+
  p6134_26_rural+p6134_27_rural+p6134_28_rural+p6134_29_rural+p6134_30_rural+p6134_31_rural+p6134_35_rural+p6134_36_rural+p6134_37_rural+
  p6134_38_rural+p6134_40_rural+p6134_45_rural+p6134_50_rural+p6134_60_rural+p6134_64_rural+p6134_68_rural+p6134_72_rural+p6134_75_rural+
  p6134_80_rural+p6134_90_rural+p6134_98_rural+p6134_99_rural+p6134_100_rural+p6134_120_rural
# 1.440.555
#################################################################################################################################################################
# 19. Para tratar ese problema de salud, ¿que hizo principalmente ...?:
p8563_ind_rural <- data.frame(svytable(~fex_c + p8563 + clase, 
                                       design = BD_survey))
# Acudió a Ia entidad de seguridad social en salud de la cual es afiliado/a
p8563_1_rural <- p8563_ind_rural %>% filter(p8563_ind_rural$p8563 == 1 & p8563_ind_rural$clase == 2)     # 940.480
p8563_1_rural <- sum(p8563_1_rural$Freq)
# Acudió de forma particular  a un médico general, especialista, odontólogo, terapeuta o profesional de la salud
p8563_2_rural <- p8563_ind_rural %>% filter(p8563_ind_rural$p8563 == 2 & p8563_ind_rural$clase == 2)     # 163.148
p8563_2_rural <- sum(p8563_2_rural$Freq)
# Acudió a un boticario, farmaceuta, droguista
p8563_3_rural <- p8563_ind_rural %>% filter(p8563_ind_rural$p8563 == 3 & p8563_ind_rural$clase == 2)     # 61.190
p8563_3_rural <- sum(p8563_3_rural$Freq)
# Consultó a un empírico, curandero, yerbatero, comadrona
p8563_4_rural <- p8563_ind_rural %>% filter(p8563_ind_rural$p8563 == 4 & p8563_ind_rural$clase == 2)     # 1.373
p8563_4_rural <- sum(p8563_4_rural$Freq)
# Asistió a terapias alternativas (acupuntura, esencias florales, musicoterapias, homeópata etc.)
p8563_5_rural <- p8563_ind_rural %>% filter(p8563_ind_rural$p8563 == 5 & p8563_ind_rural$clase == 2)     # 2.362
p8563_5_rural <- sum(p8563_5_rural$Freq)
# Usó remedios caseros 
p8563_6_rural <- p8563_ind_rural %>% filter(p8563_ind_rural$p8563 == 6 & p8563_ind_rural$clase == 2)     # 128.047
p8563_6_rural <- sum(p8563_6_rural$Freq)
# Se autorecetó
p8563_7_rural <- p8563_ind_rural %>% filter(p8563_ind_rural$p8563 == 7 & p8563_ind_rural$clase == 2)     # 95.876
p8563_7_rural <- sum(p8563_7_rural$Freq)
# Nada
p8563_8_rural <- p8563_ind_rural %>% filter(p8563_ind_rural$p8563 == 8 & p8563_ind_rural$clase == 2)     # 48.076
p8563_8_rural <- sum(p8563_8_rural$Freq)

# total p8563
p8563_total_rural <- p8563_1_rural+p8563_2_rural+p8563_3_rural+p8563_4_rural+p8563_5_rural+p8563_6_rural+p8563_7_rural+p8563_8_rural     # 1.440.555
################################################################################################################################################################################
# 20. ¿Acudió al servicio de urgencias en la institución prestadora de servicios (hospital o clínica) pública o privada?
p1092_ind_rural <- data.frame(svytable(~fex_c + p1092 + clase,
                                       design = BD_survey))
# Si
p1092_1_rural <- p1092_ind_rural %>% filter(p1092_ind_rural$p1092 == 1 & p1092_ind_rural$clase == 2)  # 554.266
p1092_1_rural <- sum(p1092_1_rural$Freq)
#No
p1092_2_rural <- p1092_ind_rural %>% filter(p1092_ind_rural$p1092 == 2 & p1092_ind_rural$clase == 2)  # 386.214
p1092_2_rural <- sum(p1092_2_rural$Freq)

# total p1092
p1092_total_rural <- p1092_1_rural + p1092_2_rural  # 940.480
###################################################################################################################################################################################
# 21. ¿A ... le brindaron asistencia médica en el servicio de urgencias para solucionar el problema de salud?
p8573_ind_rural <- data.frame(svytable(~fex_c + p8573 + clase,
                                       design = BD_survey))
# Si
p8573_1_rural <- p8573_ind_rural %>% filter(p8573_ind_rural$p8573 == 1 & p8573_ind_rural$clase == 2)  # 532.546
p8573_1_rural <- sum(p8573_1_rural$Freq)
#No
p8573_2_rural <- p8573_ind_rural %>% filter(p8573_ind_rural$p8573 == 2 & p8573_ind_rural$clase == 2)  # 21.719
p8573_2_rural <- sum(p8573_2_rural$Freq)

# total p8573 
p8573_total_rural <- p8573_1_rural + p8573_2_rural  # 554.266
##############################################################################################################################################################################3
# 22. ¿Cuál fue la razón principal por la que ... no recibió atención médica en el servicio de urgencias?
p8575_ind_rural <- data.frame(svytable(~fex_c + p8575 + clase,
                                       design = BD_survey))
# El caso era leve
p8575_1_rural <- p8575_ind_rural %>% filter(p8575_ind_rural$p8575 == 1 & p8575_ind_rural$clase == 2)  # 12.412
p8575_1_rural <- sum(p8575_1_rural$Freq)
# Esperó demasiado tiempo y no lo/la atendieron
p8575_2_rural <- p8575_ind_rural %>% filter(p8575_ind_rural$p8575 == 2 & p8575_ind_rural$clase == 2) # 2.098
p8575_2_rural <- sum(p8575_2_rural$Freq)
# Indicaron que allí no lo/la podían atender porque no estaba afiliado/a a alguna entidad que tuviera contrato con ellos
p8575_3_rural <- p8575_ind_rural %>% filter(p8575_ind_rural$p8575 == 3 & p8575_ind_rural$clase == 2)  # 960
p8575_3_rural <- sum(p8575_3_rural$Freq)
# No tenía identificación y por eso lo/la rechazaron
p8575_4_rural <- p8575_ind_rural %>% filter(p8575_ind_rural$p8575 == 4 & p8575_ind_rural$clase == 2)  # 196
p8575_4_rural <- sum(p8575_4_rural$Freq)
# Indicaron que debían remitirlo/la a otra institución prestadora de servicios que tuviera los servicios requeridos para atenderlo/la
p8575_5_rural <- p8575_ind_rural %>% filter(p8575_ind_rural$p8575 == 5 & p8575_ind_rural$clase == 2)  # 3.764
p8575_5_rural <- sum(p8575_5_rural$Freq)
# No le dieron informacion
p8575_6_rural <- p8575_ind_rural %>% filter(p8575_ind_rural$p8575 == 6 & p8575_ind_rural$clase == 2) # 1.993
p8575_6_rural <- sum(p8575_6_rural$Freq)
# No sabe/no responde
p8575_9_rural <- p8575_ind_rural %>% filter(p8575_ind_rural$p8575 == 9 & p8575_ind_rural$clase == 2)  # 293
p8575_9_rural <- sum(p8575_9_rural$Freq)

# total p8575
p8575_total_rural <- p8575_1_rural+p8575_2_rural+p8575_3_rural+p8575_4_rural+p8575_5_rural+p8575_6_rural+p8575_9_rural  # 21.719
##################################################################################################################################################################
# 23. ¿Cuánto tiempo transcurrió entre el momento de llegar al servicio de urgencias y el momento de ser atendido/a por personal médico?
p8577_ind_rural <- data.frame(svytable(~fex_c + p8577 + clase,
                                       design = BD_survey))
# Lo atendieron inmediatamente
p8577_1_rural <- p8577_ind_rural %>% filter(p8577_ind_rural$p8577 == 1 & p8577_ind_rural$clase == 2)   # 178.222
p8577_1_rural <- sum(p8577_1_rural$Freq)
# En maximo 30 minutos
p8577_2_rural <- p8577_ind_rural %>% filter(p8577_ind_rural$p8577 == 2 & p8577_ind_rural$clase == 2)   # 126.125
p8577_2_rural <- sum(p8577_2_rural$Freq)
# Entre 31 minutos y una hora
p8577_3_rural <- p8577_ind_rural %>% filter(p8577_ind_rural$p8577 == 3 & p8577_ind_rural$clase == 2)   # 68.603
p8577_3_rural <- sum(p8577_3_rural$Freq)
# Mas de una hora hasta dos horas
p8577_4_rural <- p8577_ind_rural %>% filter(p8577_ind_rural$p8577 == 4 & p8577_ind_rural$clase == 2)   # 63.237
p8577_4_rural <- sum(p8577_4_rural$Freq)
# Mas de dos horas
p8577_5_rural <- p8577_ind_rural %>% filter(p8577_ind_rural$p8577 == 5 & p8577_ind_rural$clase == 2)   # 96.357
p8577_5_rural <- sum(p8577_5_rural$Freq)

# total p8577
p8577_total_rural <- p8577_1_rural+p8577_2_rural+p8577_3_rural+p8577_4_rural+p8577_5_rural   # 532.546
####################################################################################################################################################################
# 24. En el servicio de urgencias fue atendido/a por:
p770_ind_rural <- data.frame(svytable(~fex_c + p770 + clase,
                                      design = BD_survey))
# Medico/a general
p770_1_rural <- p770_ind_rural %>% filter(p770_ind_rural$p770 == 1 & p770_ind_rural$clase == 2)  # 437.246
p770_1_rural <- sum(p770_1_rural$Freq)
# Odontologo/a
p770_2_rural <- p770_ind_rural %>% filter(p770_ind_rural$p770 == 2 & p770_ind_rural$clase == 2)  # 25.132
p770_2_rural <- sum(p770_2_rural$Freq)
# Especialista
p770_3_rural <- p770_ind_rural %>% filter(p770_ind_rural$p770 == 3 & p770_ind_rural$clase == 2)  # 70.167
p770_3_rural <- sum(p770_3_rural$Freq)

# total p770
p770_total_rural <- p770_1_rural+p770_2_rural+p770_3_rural  # 532.546
#####################################################################################################################################################################
# 25. ¿Cuál fue la razón principal por la que ... no solicitó o no recibió atención médica?
p6153_ind_rural <- data.frame(svytable(~fex_c + p6153 + clase,
                                       design = BD_survey))
# El caso era leve
p6153_1_rural <- p6153_ind_rural %>% filter(p6153_ind_rural$p6153 == 1 & p6153_ind_rural$clase == 2)  # 213.904
p6153_1_rural <- sum(p6153_1_rural$Freq)
# No tuvo tiempo
p6153_2_rural <- p6153_ind_rural %>% filter(p6153_ind_rural$p6153 == 2 & p6153_ind_rural$clase == 2)  # 15.681
p6153_2_rural <- sum(p6153_2_rural$Freq)
# El centro de atención queda lejos
p6153_3_rural <- p6153_ind_rural %>% filter(p6153_ind_rural$p6153 == 3 & p6153_ind_rural$clase == 2)  # 1.079
p6153_3_rural <- sum(p6153_3_rural$Freq)
# Falta de dinero
p6153_4_rural <- p6153_ind_rural %>% filter(p6153_ind_rural$p6153 == 4 & p6153_ind_rural$clase == 2)  # 17.186
p6153_4_rural <- sum(p6153_4_rural$Freq)
# Mal servicio o cita distanciada en el tiempo
p6153_5_rural <- p6153_ind_rural %>% filter(p6153_ind_rural$p6153 == 5 & p6153_ind_rural$clase == 2)  # 17.851
p6153_5_rural <- sum(p6153_5_rural$Freq)
# No lo/la atendieron
p6153_6_rural <- p6153_ind_rural %>% filter(p6153_ind_rural$p6153 == 6 & p6153_ind_rural$clase == 2)  # 6.884
p6153_6_rural <- sum(p6153_6_rural$Freq)
# No confia en los medicos
p6153_7_rural <- p6153_ind_rural %>% filter(p6153_ind_rural$p6153 == 7 & p6153_ind_rural$clase == 2)  # 8.031
p6153_7_rural <- sum(p6153_7_rural$Freq)
# Consulto antes y no le resolvieron el problema
p6153_8_rural <- p6153_ind_rural %>% filter(p6153_ind_rural$p6153 == 8 & p6153_ind_rural$clase == 2)  # 5.021
p6153_8_rural <- sum(p6153_8_rural$Freq)
# Muchos tramites para la cita
p6153_9_rural <- p6153_ind_rural %>% filter(p6153_ind_rural$p6153 == 9 & p6153_ind_rural$clase == 2)  # 13.368
p6153_9_rural <- sum(p6153_9_rural$Freq)
# No le cubrian o no le autorizaron la atencion
p6153_10_rural <- p6153_ind_rural %>% filter(p6153_ind_rural$p6153 == 10 & p6153_ind_rural$clase == 2)  # 4.370
p6153_10_rural <- sum(p6153_10_rural$Freq)
# Le hacen esperar mucho para atenderlo/la
p6153_11_rural <- p6153_ind_rural %>% filter(p6153_ind_rural$p6153 == 11 & p6153_ind_rural$clase == 2)  # 11.576
p6153_11_rural <- sum(p6153_11_rural$Freq)
# Dificultad para viajar
p6153_12_rural <- p6153_ind_rural %>% filter(p6153_ind_rural$p6153 == 12 & p6153_ind_rural$clase == 2)  # 776
p6153_12_rural <- sum(p6153_12_rural$Freq)
# otro
p6153_13_rural <- p6153_ind_rural %>% filter(p6153_ind_rural$p6153 == 13 & p6153_ind_rural$clase == 2)  # 21.194
p6153_13_rural <- sum(p6153_13_rural$Freq)

# total p6153
p6153_total_rural <- p6153_1_rural+p6153_2_rural+p6153_3_rural+p6153_4_rural+p6153_5_rural+p6153_6_rural+p6153_7_rural+p6153_8_rural+
  p6153_9_rural+p6153_10_rural+p6153_11_rural+p6153_12_rural+p6153_13_rural  # 336.926
###############################################################################################################################################################
# 26. ¿Cuántos días transcurrieron entre el momento de pedir la cita y el momento de la consulta con el medico/a general u odontólogo/a?
p6199_ind_rural <- data.frame(svytable(~fex_c + p6199 + clase,
                                       design = BD_survey))
# Medico/a general
p6199_1_rural <- p6199_ind_rural %>% filter(p6199_ind_rural$p6199 == 1 & p6199_ind_rural$clase == 2)  # 369.281
p6199_1_rural <- sum(p6199_1_rural$Freq)
# Odontologo/a
p6199_2_rural <- p6199_ind_rural %>% filter(p6199_ind_rural$p6199 == 2 & p6199_ind_rural$clase == 2)  # 95.334
p6199_2_rural <- sum(p6199_2_rural$Freq)
# Acudio directo al especialista
p6199_3_rural <- p6199_ind_rural %>% filter(p6199_ind_rural$p6199 == 3 & p6199_ind_rural$clase == 2)  # 84.746
p6199_3_rural <- sum(p6199_3_rural$Freq)

# total p6199 
p6199_total_rural <- p6199_1_rural+p6199_2_rural+p6199_3_rural  # 549.362
################################################################################################################################################################
# 26.1 Numero de dias
p6199s1_ind_rural <- data.frame(svytable(~fex_c + p6199s1 + clase,
                                         design = BD_survey))
# 0 dias
p6199s1_0_rural <- p6199s1_ind_rural %>% filter(p6199s1_ind_rural$p6199s1 == 0 & p6199s1_ind_rural$clase == 2)  
p6199s1_0_rural <- sum(p6199s1_0_rural$Freq)
# 1 dias
p6199s1_1_rural <- p6199s1_ind_rural %>% filter(p6199s1_ind_rural$p6199s1 == 1 & p6199s1_ind_rural$clase == 2)  
p6199s1_1_rural <- sum(p6199s1_1_rural$Freq)
# 2 dias
p6199s1_2_rural <- p6199s1_ind_rural %>% filter(p6199s1_ind_rural$p6199s1 == 2 & p6199s1_ind_rural$clase == 2)  
p6199s1_2_rural <- sum(p6199s1_2_rural$Freq)
# 3 dias
p6199s1_3_rural <- p6199s1_ind_rural %>% filter(p6199s1_ind_rural$p6199s1 == 3 & p6199s1_ind_rural$clase == 2)  
p6199s1_3_rural <- sum(p6199s1_3_rural$Freq)
# 4 dias
p6199s1_4_rural <- p6199s1_ind_rural %>% filter(p6199s1_ind_rural$p6199s1 == 4 & p6199s1_ind_rural$clase == 2)  
p6199s1_4_rural <- sum(p6199s1_4_rural$Freq)
# 5 dias
p6199s1_5_rural <- p6199s1_ind_rural %>% filter(p6199s1_ind_rural$p6199s1 == 5 & p6199s1_ind_rural$clase == 2)  
p6199s1_5_rural <- sum(p6199s1_5_rural$Freq)
# 6 dias
p6199s1_6_rural <- p6199s1_ind_rural %>% filter(p6199s1_ind_rural$p6199s1 == 6 & p6199s1_ind_rural$clase == 2)  
p6199s1_6_rural <- sum(p6199s1_6_rural$Freq)
# 7 dias
p6199s1_7_rural <- p6199s1_ind_rural %>% filter(p6199s1_ind_rural$p6199s1 == 7 & p6199s1_ind_rural$clase == 2)  
p6199s1_7_rural <- sum(p6199s1_7_rural$Freq)
# 8 dias
p6199s1_8_rural <- p6199s1_ind_rural %>% filter(p6199s1_ind_rural$p6199s1 == 8 & p6199s1_ind_rural$clase == 2)  
p6199s1_8_rural <- sum(p6199s1_8_rural$Freq)
# 9 dias
p6199s1_9_rural <- p6199s1_ind_rural %>% filter(p6199s1_ind_rural$p6199s1 == 9 & p6199s1_ind_rural$clase == 2)  
p6199s1_9_rural <- sum(p6199s1_9_rural$Freq)
# 10 dias
p6199s1_10_rural <- p6199s1_ind_rural %>% filter(p6199s1_ind_rural$p6199s1 == 10 & p6199s1_ind_rural$clase == 2)  
p6199s1_10_rural <- sum(p6199s1_10_rural$Freq)
# 12 dias
p6199s1_12_rural <- p6199s1_ind_rural %>% filter(p6199s1_ind_rural$p6199s1 == 12 & p6199s1_ind_rural$clase == 2)  
p6199s1_12_rural <- sum(p6199s1_12_rural$Freq)
# 14 dias
p6199s1_14_rural <- p6199s1_ind_rural %>% filter(p6199s1_ind_rural$p6199s1 == 14 & p6199s1_ind_rural$clase == 2)  
p6199s1_14_rural <- sum(p6199s1_14_rural$Freq)
# 15 dias
p6199s1_15_rural <- p6199s1_ind_rural %>% filter(p6199s1_ind_rural$p6199s1 == 15 & p6199s1_ind_rural$clase == 2)  
p6199s1_15_rural <- sum(p6199s1_15_rural$Freq)
# 20 dias
p6199s1_20_rural <- p6199s1_ind_rural %>% filter(p6199s1_ind_rural$p6199s1 == 20 & p6199s1_ind_rural$clase == 2)  
p6199s1_20_rural <- sum(p6199s1_20_rural$Freq)
# 21 dias
p6199s1_21_rural <- p6199s1_ind_rural %>% filter(p6199s1_ind_rural$p6199s1 == 21 & p6199s1_ind_rural$clase == 2)
p6199s1_21_rural <- sum(p6199s1_21_rural$Freq)
# 23 dias
p6199s1_23_rural <- p6199s1_ind_rural %>% filter(p6199s1_ind_rural$p6199s1 == 23 & p6199s1_ind_rural$clase == 2)  
p6199s1_23_rural <- sum(p6199s1_23_rural$Freq)
# 25 dias
p6199s1_25_rural <- p6199s1_ind_rural %>% filter(p6199s1_ind_rural$p6199s1 == 25 & p6199s1_ind_rural$clase == 2)  
p6199s1_25_rural <- sum(p6199s1_25_rural$Freq)
# 30 dias
p6199s1_30_rural <- p6199s1_ind_rural %>% filter(p6199s1_ind_rural$p6199s1 == 30 & p6199s1_ind_rural$clase == 2)  
p6199s1_30_rural <- sum(p6199s1_30_rural$Freq)

# total p6199s1
p6199s1_total_rural <- p6199s1_0_rural+p6199s1_1_rural+p6199s1_2_rural+p6199s1_3_rural+p6199s1_4_rural+p6199s1_5_rural+p6199s1_6_rural+
  p6199s1_7_rural+p6199s1_8_rural+p6199s1_9_rural+p6199s1_10_rural+p6199s1_12_rural+p6199s1_14_rural+p6199s1_15_rural+p6199s1_20_rural+
  p6199s1_21_rural+p6199s1_23_rural+p6199s1_25_rural+p6199s1_30_rural
# 464.616
##################################################################################################################################################################
# 27. ¿fue remitido(a) a especialista?
p6145_ind_rural <- data.frame(svytable(~fex_c + p6145 + clase,
                                       design = BD_survey))
# Si
p6145_1_rural <- p6145_ind_rural %>% filter(p6145_ind_rural$p6145 == 1 & p6145_ind_rural$clase == 2)  # 372.126
p6145_1_rural <- sum(p6145_1_rural$Freq)
# No
p6145_2_rural <- p6145_ind_rural %>% filter(p6145_ind_rural$p6145 == 2 & p6145_ind_rural$clase == 2)  # 554.868
p6145_2_rural <- sum(p6145_2_rural$Freq)

# total p6145
p6145_total_rural <- p6145_1_rural+p6145_2_rural  # 926.995
##################################################################################################################################################################
# 28. En general, considera que la calidad de la prestación del servicio de salud (medicina general, medicina especializada, odontología, etc.) fue:
p8554_ind_rural <- data.frame(svytable(~fex_c + p8554 + clase,
                                       design = BD_survey))
# Muy buena
p8554_1_rural <- p8554_ind_rural %>% filter(p8554_ind_rural$p8554 == 1 & p8554_ind_rural$clase == 2)  # 216.723
p8554_1_rural <- sum(p8554_1_rural$Freq)
# Buena
p8554_2_rural <- p8554_ind_rural %>% filter(p8554_ind_rural$p8554 == 2 & p8554_ind_rural$clase == 2)  # 736.842
p8554_2_rural <- sum(p8554_2_rural$Freq)
# Mala
p8554_3_rural <- p8554_ind_rural %>% filter(p8554_ind_rural$p8554 == 3 & p8554_ind_rural$clase == 2)  # 105.294
p8554_3_rural <- sum(p8554_3_rural$Freq)
# Muy mala
p8554_4_rural <- p8554_ind_rural %>% filter(p8554_ind_rural$p8554 == 4 & p8554_ind_rural$clase == 2)  # 25.966
p8554_4_rural <- sum(p8554_4_rural$Freq)

# total p8554
p8554_total_rural <- p8554_1_rural+p8554_2_rural+p8554_3_rural+p8554_4_rural  # 1.289.988
###################################################################################################################################################################
# 29. ¿Cuál es el aspecto que más influyó en su percepción sobre la calidad de la prestación del servicio?
p801_ind_rural <- data.frame(svytable(~fex_c + p801 + clase,
                                      design = BD_survey))
# Trámites excesivos o dispendiosos
p801_1_rural <- p801_ind_rural %>% filter(p801_ind_rural$p801 == 1 & p801_ind_rural$clase == 2)  # 15.283
p801_1_rural <- sum(p801_1_rural$Freq)
# Mala atención del personal administrativo o asistencial (médicos, enfermeras, etc.)
p801_2_rural <- p801_ind_rural %>% filter(p801_ind_rural$p801 == 2 & p801_ind_rural$clase == 2)  # 38.582
p801_2_rural <- sum(p801_2_rural$Freq)
# Falta de capacidad, conocimientos o habilidad del personal asistencial
p801_3_rural <- p801_ind_rural %>% filter(p801_ind_rural$p801 == 3 & p801_ind_rural$clase == 2)  # 9.405
p801_3_rural <- sum(p801_3_rural$Freq)
# Condiciones deficientes de infraestructura, dotación, mobiliario o accesibilidad para población en condición de discapacidad
p801_4_rural <- p801_ind_rural %>% filter(p801_ind_rural$p801 == 4 & p801_ind_rural$clase == 2)  # 1.173
p801_4_rural <- sum(p801_4_rural$Freq)
# Demora en la asignación de citas
p801_5_rural <- p801_ind_rural %>% filter(p801_ind_rural$p801 == 5 & p801_ind_rural$clase == 2)  # 24.652
p801_5_rural <- sum(p801_5_rural$Freq)
# Demora en la atención por parte del personal médico
p801_6_rural <- p801_ind_rural %>% filter(p801_ind_rural$p801 == 6 & p801_ind_rural$clase == 2)  # 31.277
p801_6_rural <- sum(p801_6_rural$Freq)
# Problemas relacionados con los medicamentos
p801_7_rural <- p801_ind_rural %>% filter(p801_ind_rural$p801 == 7 & p801_ind_rural$clase == 2)  # 4.486
p801_7_rural <- sum(p801_7_rural$Freq)
# Otro
p801_8_rural <- p801_ind_rural %>% filter(p801_ind_rural$p801 == 8 & p801_ind_rural$clase == 2)  # 3.482
p801_8_rural <- sum(p801_8_rural$Freq)

# total p801
p801_total_rural <- p801_1_rural+p801_2_rural+p801_3_rural+p801_4_rural+p801_5_rural+p801_6_rural+p801_7_rural+p801_8_rural  # 128.343
##########################################################################################################################################################################
# 30. ¿Cuáles de las siguientes fuentes utilizó ... para cubrir los costos de atención en salud en los últimos 30 días? (incluya consulta médica , exámenes y medicamentos)
# p8556 

# 30.1 EPS o entidad de seguridad social en salud  en la cual está afiliado/a
p8556s1_ind_rural <- data.frame(svytable(~fex_c + p8556s1 + clase,
                                         design = BD_survey))
p8556s1_1_rural <- p8556s1_ind_rural %>% filter(p8556s1_ind_rural$p8556s1 == 1 & p8556s1_ind_rural$clase == 2) # 816.522
p8556s1_1_rural <- sum(p8556s1_1_rural$Freq)
# 30.2 Plan o seguro voluntario (Seguro médico, plan complementario o medicina prepagada)
p8556s2_ind_rural <- data.frame(svytable(~fex_c + p8556s2 + clase,
                                         design = BD_survey))
p8556s2_2_rural <- p8556s2_ind_rural %>% filter(p8556s2_ind_rural$p8556s2 == 1 & p8556s2_ind_rural$clase == 2) # 87.174
p8556s2_2_rural <- sum(p8556s2_2_rural$Freq)
# 30.3 Seguro obligatorio de accidentes de tránsito (SOAT)
p8556s4_ind_rural <- data.frame(svytable(~fex_c + p8556s4 + clase,
                                         design = BD_survey))
p8556s4_3_rural <- p8556s4_ind_rural %>% filter(p8556s4_ind_rural$p8556s4 == 1 & p8556s4_ind_rural$clase == 2) # 15.492
p8556s4_3_rural <- sum(p8556s4_3_rural$Freq)
# 30.4 Secretaria de salud o la alcaldía
p8556s5_ind_rural <- data.frame(svytable(~fex_c + p8556s5 + clase,
                                         design = BD_survey))
p8556s5_4_rural <- p8556s5_ind_rural %>% filter(p8556s5_ind_rural$p8556s5 == 1 & p8556s5_ind_rural$clase == 2) # 5.662
p8556s5_4_rural <- sum(p8556s5_4_rural$Freq)
# 30.5 Recursos propios o familiares
p8556s6_ind_rural <- data.frame(svytable(~fex_c + p8556s6 + clase,
                                         design = BD_survey))
p8556s6_5_rural <- p8556s6_ind_rural %>% filter(p8556s6_ind_rural$p8556s6 == 1 & p8556s6_ind_rural$clase == 2) # 188.067
p8556s6_5_rural <- sum(p8556s6_5_rural$Freq)
# 30.6 Recursos de otras personas 
p8556s9_ind_rural <- data.frame(svytable(~fex_c + p8556s9 + clase,
                                         design = BD_survey))
p8556s9_6_rural <- p8556s9_ind_rural %>% filter(p8556s9_ind_rural$p8556s9 == 1 & p8556s9_ind_rural$clase == 2) # 10.872
p8556s9_6_rural <- sum(p8556s9_6_rural$Freq)
# 30.7 No se requirio pago
p8556s10_ind_rural <- data.frame(svytable(~fex_c + p8556s10 + clase,
                                          design = BD_survey))
p8556s10_7_rural <- p8556s10_ind_rural %>% filter(p8556s10_ind_rural$p8556s10 == 1 & p8556s10_ind_rural$clase == 2) # 28.801
p8556s10_7_rural <- sum(p8556s10_7_rural$Freq)

# total p8556
p8556_total_rural <- p8556s1_1_rural + p8556s2_2_rural + p8556s4_3_rural + p8556s5_4_rural + p8556s6_5_rural+ p8556s9_6_rural + p8556s10_7_rural 

#######################################################################################################################################################
# 31. Por esta enfermedad , ¿a ... le formularon medicamentos?
p6147_ind_rural <- data.frame(svytable(~fex_c + p6147 + clase,
                                       design = BD_survey))
# Si
p6147_1_rural <- p6147_ind_rural %>% filter(p6147_ind_rural$p6147 == 1 & p6147_ind_rural$clase == 2)  # 889.650
p6147_1_rural <- sum(p6147_1_rural$Freq)
# No
p6147_2_rural <- p6147_ind_rural %>% filter(p6147_ind_rural$p6147 == 2 & p6147_ind_rural$clase == 2)  # 192.258
p6147_2_rural <- sum(p6147_2_rural$Freq)

# total 6147
p6147_total_rural <- p6147_1_rural+p6147_2_rural  # 1.081.909
#########################################################################################################################################################
# 32. ¿Estos medicamentos o remedios le fueron entregados a ... por cuenta de la institución a la cual está afiliado(a)?
p6148_ind_rural <- data.frame(svytable(~fex_c + p6148 + clase,
                                       design = BD_survey))
# Si, todos
p6148_1_rural <- p6148_ind_rural %>% filter(p6148_ind_rural$p6148 == 1 & p6148_ind_rural$clase == 2)  # 547.752
p6148_1_rural <- sum(p6148_1_rural$Freq)
# Si, algunos
p6148_2_rural <- p6148_ind_rural %>% filter(p6148_ind_rural$p6148 == 2 & p6148_ind_rural$clase == 2)  # 121.387
p6148_2_rural <- sum(p6148_2_rural$Freq)
# No
p6148_3_rural <- p6148_ind_rural %>% filter(p6148_ind_rural$p6148 == 3 & p6148_ind_rural$clase == 2)  # 220.510
p6148_3_rural <- sum(p6148_3_rural$Freq)
# total 6147
p6148_total_rural <- p6148_1_rural+p6148_2_rural+p6148_3_rural  # 889.650
###########################################################################################################################################################
# 33. ¿Por qué razón no le fueron entregados los medicamentos  (todos o algunos)?
p6149_ind_rural <- data.frame(svytable(~fex_c + p6149 + clase,
                                       design = BD_survey))
# No están incluidos en el plan de beneficios en salud o POS o no le autorizaron
p6149_1_rural <- p6149_ind_rural %>% filter(p6149_ind_rural$p6149 == 1 & p6149_ind_rural$clase == 2)  # 125.724
p6149_1_rural <- sum(p6149_1_rural$Freq)
# No había los medicamentos recetados
p6149_2_rural <- p6149_ind_rural %>% filter(p6149_ind_rural$p6149 == 2 & p6149_ind_rural$clase == 2)  # 49.676
p6149_2_rural <- sum(p6149_2_rural$Freq)
# No había la cantidad requerida
p6149_3_rural <- p6149_ind_rural %>% filter(p6149_ind_rural$p6149 == 3 & p6149_ind_rural$clase == 2)  # 10.584
p6149_3_rural <- sum(p6149_3_rural$Freq)
# Por errores o deficiencias en la expedición de la fórmula medica
p6149_4_rural <- p6149_ind_rural %>% filter(p6149_ind_rural$p6149 == 4 & p6149_ind_rural$clase == 2)  # 3.048
p6149_4_rural <- sum(p6149_4_rural$Freq)
# No hizo las gestiones para reclamarlos
p6149_5_rural <- p6149_ind_rural %>% filter(p6149_ind_rural$p6149 == 5 & p6149_ind_rural$clase == 2)  # 57.664
p6149_5_rural <- sum(p6149_5_rural$Freq)
# No tenia dinero
p6149_6_rural <- p6149_ind_rural %>% filter(p6149_ind_rural$p6149 == 6 & p6149_ind_rural$clase == 2)  # 2.151
p6149_6_rural <- sum(p6149_6_rural$Freq)
# Acudió a medico particular
p6149_7_rural <- p6149_ind_rural %>% filter(p6149_ind_rural$p6149 == 7 & p6149_ind_rural$clase == 2)  # 83.081
p6149_7_rural <- sum(p6149_7_rural$Freq)
# Otra
p6149_8_rural <- p6149_ind_rural %>% filter(p6149_ind_rural$p6149 == 8 & p6149_ind_rural$clase == 2)  # 9.965
p6149_8_rural <- sum(p6149_8_rural$Freq)

# total p6149
p6149_total_rural <- p6149_1_rural+p6149_2_rural+p6149_3_rural+p6149_4_rural+p6149_5_rural+p6149_6_rural+p6149_7_rural+p6149_8_rural  # 341.897
############################################################################################################################################################
# 34.  Durante los últimos 30 días ....realizó pagos por:  (No incluya gastos reportados en hospitalización)
# 34.1 Consulta medica general o con especialista ?
p3178_ind_rural <- data.frame(svytable(~fex_c + p3178 + clase,
                                       design = BD_survey))
# Si
p3178_1_rural <- p3178_ind_rural %>% filter(p3178_ind_rural$p3178 == 1 & p3178_ind_rural$clase == 2)  # 3.169.949
p3178_1_rural <- sum(p3178_1_rural$Freq)
# No
p3178_2_rural <- p3178_ind_rural %>% filter(p3178_ind_rural$p3178 == 2 & p3178_ind_rural$clase == 2)  # 35.898.655
p3178_2_rural <- sum(p3178_2_rural$Freq)

# total p3178
p3178_total_rural <- p3178_1_rural+p3178_2_rural  # 39.068.605
#############################################################################################################################################################
# 34.1.1 A traves de EPS
p3178s1_ind_rural <- data.frame(svytable(~fex_c + p3178s1 + clase,
                                         design = BD_survey))
p3178s1_rural <- p3178s1_ind_rural %>% filter(p3178s1_ind_rural$p3178s1 == 1 & p3178s1_ind_rural$clase == 2)  # 2.462.322
p3178s1_rural <- sum(p3178s1_rural$Freq)
# 34.1.1.A1 Valor a traves de EPS
#p3178s1a1_rural <- table(BD_ecv_ind$p3178s1a1)
##############################################################################################################################################################
# 34.1.2 Medico particular
p3178s2_ind_rural <- data.frame(svytable(~fex_c + p3178s2 + clase,
                                         design = BD_survey))
p3178s2_rural <- p3178s2_ind_rural %>% filter(p3178s2_ind_rural$p3178s2 == 1 & p3178s2_ind_rural$clase == 2)  # 374.129
p3178s2_rural <- sum(p3178s2_rural$Freq)
# 34.1.2.A1 Valor Médico particular
#p3178s2a1_rural <- table(BD_ecv_ind$p3178s2a1)
##############################################################################################################################################################
# 34.1.3 Plan voluntario (seguro médico, plan complementario o medicina prepagada)
p3178s3_ind_rural <- data.frame(svytable(~fex_c + p3178s3 + clase,
                                         design = BD_survey))
p3178s3_rural <- p3178s3_ind_rural %>% filter(p3178s3_ind_rural$p3178s3 == 1 & p3178s3_ind_rural$clase == 2)  # 361.850
p3178s3_rural <- sum(p3178s3_rural$Freq)
# 34.1.2 A1 Valor Plan voluntario (seguro médico, plan complementario o medicina prepagada) 
#p3178s3a1_rural <- table(BD_ecv_ind$p3178s3a1)
#############################################################################################################################################################
# 34.2 Consulta o tratamiento odontologico?
p3179_ind_rural <- data.frame(svytable(~fex_c + p3179 + clase,
                                       design = BD_survey))
# Si
p3179_1_rural <- p3179_ind_rural %>% filter(p3179_ind_rural$p3179 == 1 & p3179_ind_rural$clase == 2)  # 1.135.904
p3179_1_rural <- sum(p3179_1_rural$Freq)
# No
p3179_2_rural <- p3179_ind_rural %>% filter(p3179_ind_rural$p3179 == 2 & p3179_ind_rural$clase == 2)  # 37.932.700
p3179_2_rural <- sum(p3179_2_rural$Freq)

# total p3179
p3179_total_rural <- p3179_1_rural+p3179_2_rural  # 39.068.605
##############################################################################################################################################################
# 34.2.1 A traves de EPS
p3179s1_ind_rural <- data.frame(svytable(~fex_c + p3179s1 + clase,
                                         design = BD_survey))
p3179s1_rural <- p3179s1_ind_rural %>% filter(p3179s1_ind_rural$p3179s1 == 1 & p3179s1_ind_rural$clase == 2)  # 490.716
p3179s1_rural <- sum(p3179s1_rural$Freq)
# 34.2.1.A1 Valor a traves de EPS
#p3179s1a1_rural <- table(BD_ecv_ind$p3179s1a1)
##############################################################################################################################################################
# 34.2.2 Odontologo particular
p3179s2_ind_rural <- data.frame(svytable(~fex_c + p3179s2 + clase,
                                         design = BD_survey))
p3179s2_rural <- p3179s2_ind_rural %>% filter(p3179s2_ind_rural$p3179s2 == 1 & p3179s2_ind_rural$clase == 2)  # 568.694
p3179s2_rural <- sum(p3179s2_rural$Freq)
# 34.2.2.A1 Valor odontologo particular
#p3179s2a1_rural <- table(BD_ecv_ind$p3179s2a1)
###############################################################################################################################################################
# 34.2.3 Plan voluntario (seguro médico, plan complementario o medicina prepagada)
p3179s3_ind_rural <- data.frame(svytable(~fex_c + p3179s3 + clase,
                                         design = BD_survey))
p3179s3_rural <- p3179s3_ind_rural %>% filter(p3179s3_ind_rural$p3179s3 == 1 & p3179s3_ind_rural$clase == 2)  # 81.220
p3179s3_rural <- sum(p3179s3_rural$Freq)
# 34.2.3 A1 Valor Plan voluntario (seguro médico, plan complementario o medicina prepagada) 
#p3179s3a1_rural <- summary(BD_ecv_ind$p3179s3a1)
###############################################################################################################################################################
# 34.3 Vacunas
p3181_ind_rural <- data.frame(svytable(~fex_c + p3181 + clase, 
                                       design = BD_survey))
# Si
p3181_1_rural <- p3181_ind_rural %>% filter(p3181_ind_rural$p3181 == 1 & p3181_ind_rural$clase == 2)  # 279.829
p3181_1_rural <- sum(p3181_1_rural$Freq)

# No
p3181_2_rural <- p3181_ind_rural %>% filter(p3181_ind_rural$p3181 == 2 & p3181_ind_rural$clase == 2) # 38.788.775
p3181_2_rural <- sum(p3181_2_rural$Freq)

# total p3181 
p3181_total_rural <- p3181_1_rural+p3181_2_rural  # 39.068.605
# 34.3.1 Valor vacuna
#p3181s1_ind_rural <- data.frame(svytable(~fex_c + p3181s1 + clase,
#design = BD_survey))
#p3181s1_rural <- summary(p3181s1_ind_rural$p3181s1)
#################################################################################################################################################################
# 34.4 Formulas medicas o compra de medicamentos consumidos ocasional o regularmente
p3182_ind_rural <- data.frame(svytable(~fex_c + p3182 + clase,
                                       design = BD_survey))
# Si
p3182_1_rural <- p3182_ind_rural %>% filter(p3182_ind_rural$p3182 == 1 & p3182_ind_rural$clase == 2) # 2.885.358
p3182_1_rural <- sum(p3182_1_rural$Freq)
# No
p3182_2_rural <- p3182_ind_rural %>% filter(p3182_ind_rural$p3182 == 2 & p3182_ind_rural$clase == 2)  # 36.183.246
p3182_2_rural <- sum(p3182_2_rural$Freq)

# total p3182
p3182_total_rural <- p3182_1_rural+p3182_2_rural   # 39.068.605

# 34.4.1 Valor
#p3182s1_ind_rural <- data.frame(svytable(~fex_c + p3182s1_ind + clase,
#design = BD_survey))
#p3182s1_rural <- table(BD_ecv_ind$p3182s1)
####################################################################################################################################################################
# 34.5 Laboratorio clinico, RX, examenes de diagnostico?
p3183_ind_rural <- data.frame(svytable(~fex_c + p3183 + clase,
                                       design = BD_survey))
# Si
p3183_1_rural <- p3183_ind_rural %>% filter(p3183_ind_rural$p3183 == 1 & p3183_ind_rural$clase == 2)  # 962.324
p3183_1_rural <- sum(p3183_1_rural$Freq)
# No
p3183_2_rural <- p3183_ind_rural %>% filter(p3183_ind_rural$p3183 == 2 & p3183_ind_rural$clase == 2)  # 38.106.280
p3183_2_rural <- sum(p3183_2_rural$Freq)

# total p3183
p3183_total_rural <- p3183_1_rural+p3183_2_rural  # 39.068.605

# 34.5.1 Valor
#p3183s1_ind_rural <- data.frame(svytable(~fex_c + p3183s1_ind + clase,
#design = BD_survey))
#p3183s1_rural <- table(BD_ecv_ind$p3183s1)
#######################################################################################################################################################################
# 34.6 Rehabilitacion o terapias medicas ?
p3184_ind_rural <- data.frame(svytable(~fex_c + p3184 + clase,
                                       design = BD_survey))
# Si
p3184_1_rural <- p3184_ind_rural %>% filter(p3184_ind_rural$p3184 == 1 & p3184_ind_rural$clase == 2)  # 214.157
p3184_1_rural <- sum(p3184_1_rural$Freq)
# No
p3184_2_rural <- p3184_ind_rural %>% filter(p3184_ind_rural$p3184 == 2 & p3184_ind_rural$clase == 2)  # 38.854.447
p3184_2_rural <- sum(p3184_2_rural$Freq)

# total p3184
p3184_total_rural <- p3184_1_rural+p3184_2_rural  # 39.068.605

# 34.6.1 Valor
#p3184s1_ind_rural <- data.frame(svytable(~fex_c + p3184s1,
#design = BD_survey))
#p3184s1_rural <- table(BD_ecv_ind$p3184s1)
########################################################################################################################################################################
# 34.7 Terapias alternativas ? (homeopatia, acupuntura, esencias florales, musicoterapia)
p3185_ind_rural <- data.frame(svytable(~fex_c + p3185 + clase,
                                       design = BD_survey))
# Si
p3185_1_rural <- p3185_ind_rural %>% filter(p3185_ind_rural$p3185 == 1 & p3185_ind_rural$clase == 2)  # 64.133
p3185_1_rural <- sum(p3185_1_rural$Freq)
# No
p3185_2_rural <- p3185_ind_rural %>% filter(p3185_ind_rural$p3185 == 2 & p3185_ind_rural$clase == 2)  # 39.004.471
p3185_2_rural <- sum(p3185_2_rural$Freq)

# total p3185
p3185_total_rural <- p3185_1_rural+p3185_2_rural  # 39.068.605

# 34.7.1 Valor
#p3185s1_ind_rural <- data.frame(svytable(~fex_c + p3185s1 + clase,
#design = BD_survey))
#p3185s1_rural <- table(BD_ecv_ind$p3185s1)
##########################################################################################################################################################################
# 34.8 Transporte para ir al sitio de atencion medica y regresar
p3186_ind_rural <- data.frame(svytable(~fex_c + p3186 + clase,
                                       design = BD_survey))
# Si
p3186_1_rural <- p3186_ind_rural %>% filter(p3186_ind_rural$p3186 == 1 & p3186_ind_rural$clase == 2)  # 2.587.719
p3186_1_rural <- sum(p3186_1_rural$Freq)
# No
p3186_2_rural <- p3186_ind_rural %>% filter(p3186_ind_rural$p3186 == 2 & p3186_ind_rural$clase == 2)  # 36.480.885
p3186_2_rural <- sum(p3186_2_rural$Freq)

# total p3186
p3186_total_rural <- p3186_1_rural+p3186_2_rural  # 39.068.605

# 34.7.1 Valor
#p3186s1_ind_rural <- data.frame(svytable(~fex_c + p3186s1 + clase,
#design = BD_survey))
#p3186s1_rural <- table(BD_ecv_ind$p3186s1)
###########################################################################################################################################################################
# 35. Durante los ÚLTIMOS DOCE MESES ¿Realizó pagos por: 
# 35.1  Lentes, audífonos o aparatos ortopédicos (muletas, sillas de ruedas, elementos para terapias, etc.)
p3187s1_ind_rural <- data.frame(svytable(~fex_c + p3187s1 + clase,
                                         design = BD_survey))
# Si
p3187s1_1_rural <- p3187s1_ind_rural %>% filter(p3187s1_ind_rural$p3187s1 == 1 & p3187s1_ind_rural$clase == 2)  # 1.845.552
p3187s1_1_rural <- sum(p3187s1_1_rural$Freq)
# No
p3187s1_2_rural <- p3187s1_ind_rural %>% filter(p3187s1_ind_rural$p3187s1 == 2 & p3187s1_ind_rural$clase == 2)  # 37.223.052
p3187s1_2_rural <- sum(p3187s1_2_rural$Freq)

# total p3187s1
p3187s1_total_rural <- p3187s1_1_rural+p3187s1_2_rural  # 39.068.605

# 35.2. Valor Lentes, audífonos o aparatos ortopédicos (muletas, sillas de ruedas, elementos para terapias, etc.)
#p3187s2_ind_rural <- data.frame(svytable(~fex_c + p3187s2 + clase,
#design = BD_survey))
#p3187s2_rural <- table(BD_ecv_ind$p3187s2)
############################################################################################################################################################################
# 35.2 Cirugías o procedimientos ambulatorios? 
p3188_ind_rural <- data.frame(svytable(~fex_c + p3188 + clase,
                                       design = BD_survey))
# Si
p3188_1_rural <- p3188_ind_rural %>% filter(p3188_ind_rural$p3188 == 1 & p3188_ind_rural$clase == 2)  # 401.410
p3188_1_rural <- sum(p3188_1_rural$Freq)
# No
p3188_2_rural <- p3188_ind_rural %>% filter(p3188_ind_rural$p3188 == 2 & p3188_ind_rural$clase == 2)  # 38.667.194
p3188_2_rural <- sum(p3188_2_rural$Freq)

# total p3188
p3188_total_rural <- p3188_1_rural+p3188_2_rural  # 39.068.605
#############################################################################################################################################################################
# 35.2.1 A traves de EPS
p3188s1_ind_rural <- data.frame(svytable(~fex_c + p3188s1 + clase,
                                         design = BD_survey))
p3188s1_rural <- p3188s1_ind_rural %>% filter(p3188s1_ind_rural$p3188s1 == 1 & p3188s1_ind_rural$clase == 2)   # 279.004
p3188s1_rural <- sum(p3188s1_rural$Freq)

# 35.2.1 A1 Valor a traves de EPS
#p3188s1a1_ind_rural <- data.frame(svytable(~fex_c + p3188s1a1 + clase,
#design = BD_survey))
#p3188s1a1_rural <- table(BD_ecv_ind$p3188s1a1)
#############################################################################################################################################################################
# 35.2.2 Medico particular
p3188s2_ind_rural <- data.frame(svytable(~fex_c + p3188s2 + clase,
                                         design = BD_survey))
p3188s2_rural <- p3188s2_ind_rural %>% filter(p3188s2_ind_rural$p3188s2 == 1 & p3188s2_ind_rural$clase == 2)   # 85.457
p3188s2_rural <- sum(p3188s2_rural$Freq)

# 35.2.2 A1 Valor medico particular
#p3188s2a1_ind_rural <- data.frame(svytable(~fex_c + p3188s2a1 + clase,
#design = BD_survey))
#p3188s2a1_rural <- table(BD_ecv_ind$p3188s2a1)
###############################################################################################################################################################################
# 35.2.3 Plan voluntario (seguro médico, plan complementario o medicina prepagada) 
p3188s3_ind_rural <- data.frame(svytable(~fex_c + p3188s3 + clase,
                                         design = BD_survey))
p3188s3_rural <- p3188s3_ind_rural %>% filter(p3188s3_ind_rural$p3188s3 == 1 & p3188s3_ind_rural$clase == 2)   # 45.300
p3188s3_rural <- sum(p3188s3_rural$Freq)

# 35.2.3 A1  Valor Valor Plan voluntario (seguro médico, plan complementario o medicina prepagada) 
#p3188s3a1_ind_rural <- data.frame(svytable(~fex_c + p3188s3a1 + clase,
#design = BD_survey))
#p3188s3a1_rural <- table(BD_ecv_ind$p3188s3a1)
################################################################################################################################################################################
# 36. ¿Actualmente _____ fuma (cigarrillo, tabaco, vapeador o cigarrillo electrónico)?
# 36.1 Cigarrillo, tabaco:
p3008s1_ind_rural <- data.frame(svytable(~fex_c + p3008s1 + clase,
                                         design = BD_survey))
# Si 
p3008s1_1_rural <- p3008s1_ind_rural %>% filter(p3008s1_ind_rural$p3008s1 == 1 & p3008s1_ind_rural$clase == 2)  # 1.957.838
p3008s1_1_rural <- sum(p3008s1_1_rural$Freq)
# No
p3008s1_2_rural <- p3008s1_ind_rural %>% filter(p3008s1_ind_rural$p3008s1 == 2 & p3008s1_ind_rural$clase == 2)  # 31.611.553
p3008s1_2_rural <- sum(p3008s1_2_rural$Freq)

# total p3008s1
p3008s1_total_rural <- p3008s1_1_rural+p3008s1_2_rural  # 33.569.391
#####################################################################################################################################################################################
# 36.2 Frecuencia cigarrillo, tabaco:
p3008s1a1_ind_rural <- data.frame(svytable(~fex_c + p3008s1a1 + clase,
                                           design = BD_survey))
# Diariamente 
p3008s1a1_1_rural <- p3008s1a1_ind_rural %>% filter(p3008s1a1_ind_rural$p3008s1a1 == 1 & p3008s1a1_ind_rural$clase == 2)  # 1.191.560
p3008s1a1_1_rural <- sum(p3008s1a1_1_rural$Freq)
# Algunos dias a la semana
p3008s1a1_2_rural <- p3008s1a1_ind_rural %>% filter(p3008s1a1_ind_rural$p3008s1a1 == 2 & p3008s1a1_ind_rural$clase == 2)  # 521.681
p3008s1a1_2_rural <- sum(p3008s1a1_2_rural$Freq)
# Menos de una vez por semana
p3008s1a1_3_rural <- p3008s1a1_ind_rural %>% filter(p3008s1a1_ind_rural$p3008s1a1 == 3 & p3008s1a1_ind_rural$clase == 2)  # 244.596
p3008s1a1_3_rural <- sum(p3008s1a1_3_rural$Freq)
# total p3008s1a1
p3008s1a1_total_rural <- p3008s1a1_1_rural+p3008s1a1_2_rural+p3008s1a1_3_rural  # 1.957.838
#################################################################################################################################################################################
# 36.3 ¿Cuantos cigarrillos al dia?
#p3008s1a2_ind_rural <- data.frame(svytable(~fex_c + p3008s1a2 + clase,
#design = BD_survey))
#p3008s1a2_rural <- table(BD_ecv_ind$p3008s1a2)
###################################################################################################################################################################################
# 36.4 Vapeador o cigarrillo electronico
p3008s2_ind_rural <- data.frame(svytable(~fex_c + p3008s2 + clase,
                                         design = BD_survey))
# Si 
p3008s2_1_rural <- p3008s2_ind_rural %>% filter(p3008s2_ind_rural$p3008s2 == 1 & p3008s2_ind_rural$clase == 2)  # 82.943
p3008s2_1_rural <- sum(p3008s2_1_rural$Freq)
# No
p3008s2_2_rural <- p3008s2_ind_rural %>% filter(p3008s2_ind_rural$p3008s2 == 2 & p3008s2_ind_rural$clase == 2)  # 33.486.447
p3008s2_2_rural <- sum(p3008s2_2_rural$Freq)

# total p3008s1
p3008s2_total_rural <- p3008s2_1_rural+p3008s2_2_rural  # 33.569.391
###################################################################################################################################################################################
# 37. ¿ ......  consume bebidas azucaradas (gaseosas, refrescos, bebidas de  jugos de frutas procesadas, té endulzado, refrescos en polvo)?
p1707_ind_rural <- data.frame(svytable(~fex_c + p1707 + clase,
                                       design = BD_survey))
# Si
p1707_1_rural <- p1707_ind_rural %>% filter(p1707_ind_rural$p1707 == 1 & p1707_ind_rural$clase == 2)  # 23.730.629
p1707_1_rural <- sum(p1707_1_rural$Freq)
# No
p1707_2_rural <- p1707_ind_rural %>% filter(p1707_ind_rural$p1707 == 2 & p1707_ind_rural$clase == 2)  # 14.396.233
p1707_2_rural <- sum(p1707_2_rural$Freq)

# total p1707
p1707_total_rural <- p1707_1_rural+p1707_2_rural  # 38.126.862
#####################################################################################################################################################################################
# 37.1 Con que frecuencia consume las bebidas azucaradas:
p1707s1_ind_rural <- data.frame(svytable(~fex_c + p1707s1 + clase,
                                         design = BD_survey))
# Todos los dias de la semana (dos o mas veces al dia)
p1707s1_1_rural <- p1707s1_ind_rural %>% filter(p1707s1_ind_rural$p1707s1 == 1 & p1707s1_ind_rural$clase == 2)  # 1.998.276
p1707s1_1_rural <- sum(p1707s1_1_rural$Freq)
# Todos los dias de la semana (una vez al dia)
p1707s1_2_rural <- p1707s1_ind_rural %>% filter(p1707s1_ind_rural$p1707s1 == 2 & p1707s1_ind_rural$clase == 2)  # 2.642.830
p1707s1_2_rural <- sum(p1707s1_2_rural$Freq)
# Cuatro a seis veces a la semana
p1707s1_3_rural <- p1707s1_ind_rural %>% filter(p1707s1_ind_rural$p1707s1 == 3 & p1707s1_ind_rural$clase == 2)  # 2.112.003
p1707s1_3_rural <- sum(p1707s1_3_rural$Freq)
# Dos o tres veces a la semana
p1707s1_4_rural <- p1707s1_ind_rural %>% filter(p1707s1_ind_rural$p1707s1 == 4 & p1707s1_ind_rural$clase == 2)  # 7.708.717
p1707s1_4_rural <- sum(p1707s1_4_rural$Freq)
# Una vez a la semana
p1707s1_5_rural <- p1707s1_ind_rural %>% filter(p1707s1_ind_rural$p1707s1 == 5 & p1707s1_ind_rural$clase == 2)  # 6.139.353
p1707s1_5_rural <- sum(p1707s1_5_rural$Freq)
# Menos de una vez por semana
p1707s1_6_rural <- p1707s1_ind_rural %>% filter(p1707s1_ind_rural$p1707s1 == 6 & p1707s1_ind_rural$clase == 2)  # 3.129.448
p1707s1_6_rural <- sum(p1707s1_6_rural$Freq)

# total p1707s1
p1707s1_total_rural <- p1707s1_1_rural+p1707s1_2_rural+p1707s1_3_rural+p1707s1_4_rural+p1707s1_5_rural+p1707s1_6_rural  # 23.730.629
########################################################################################################################################################################
# 38. ¿ ____  consume alimentos de paquete (papas, plátanos, chitos, paquete mixto, rosquitas, chicharrones o similares)?
p3003_ind_rural <- data.frame(svytable(~fex_c + p3003 + clase,
                                       design = BD_survey))
# Si
p3003_1_rural <- p3003_ind_rural %>% filter(p3003_ind_rural$p3003 == 1 & p3003_ind_rural$clase == 2)  # 19.382.921
p3003_1_rural <- sum(p3003_1_rural$Freq)
# No
p3003_2_rural <- p3003_ind_rural %>% filter(p3003_ind_rural$p3003 == 2 & p3003_ind_rural$clase == 2)  # 18.743.941
p3003_2_rural <- sum(p3003_2_rural$Freq)

# total p3003
p3003_total_rural <- p3003_1_rural+p3003_2_rural  # 38.126.862
#########################################################################################################################################################################
# 38.1 Con que frecuencia consume alimentos de paquete
p3003s1_ind_rural <- data.frame(svytable(~fex_c + p3003s1 + clase,
                                         design = BD_survey))
# Todos los dias de la semana (dos o mas veces al dia)
p3003s1_1_rural <- p3003s1_ind_rural %>% filter(p3003s1_ind_rural$p3003s1 == 1 & p3003s1_ind_rural$clase == 2)  # 772.644
p3003s1_1_rural <- sum(p3003s1_1_rural$Freq)
# Todos los dias de la semana (una vez al dia)
p3003s1_2_rural <- p3003s1_ind_rural %>% filter(p3003s1_ind_rural$p3003s1 == 2 & p3003s1_ind_rural$clase == 2)  # 1.547.378
p3003s1_2_rural <- sum(p3003s1_2_rural$Freq)
# Cuatro a seis veces a la semana
p3003s1_3_rural <- p3003s1_ind_rural %>% filter(p3003s1_ind_rural$p3003s1 == 3 & p3003s1_ind_rural$clase == 2)  # 1.590.644
p3003s1_3_rural <- sum(p3003s1_3_rural$Freq)
# Dos o tres veces a la semana
p3003s1_4_rural <- p3003s1_ind_rural %>% filter(p3003s1_ind_rural$p3003s1 == 4 & p3003s1_ind_rural$clase == 2)  # 6.099.625
p3003s1_4_rural <- sum(p3003s1_4_rural$Freq)
# Una vez a la semana
p3003s1_5_rural <- p3003s1_ind_rural %>% filter(p3003s1_ind_rural$p3003s1 == 5 & p3003s1_ind_rural$clase == 2)  # 5.839.326
p3003s1_5_rural <- sum(p3003s1_5_rural$Freq)
# Menos de una vez por semana
p3003s1_6_rural <- p3003s1_ind_rural %>% filter(p3003s1_ind_rural$p3003s1 == 6 & p3003s1_ind_rural$clase == 2)  # 3.533.302
p3003s1_6_rural <- sum(p3003s1_6_rural$Freq)

# total p3003s1
p3003s1_total_rural <- p3003s1_1_rural+p3003s1_2_rural+p3003s1_3_rural+p3003s1_4_rural+p3003s1_5_rural+p3003s1_6_rural  # 19.382.921
##################################################################################################################################################################################
# 39. ¿Durante los últimos 12 meses ____ tuvo que ser hospitalizado/a?
p6133_ind_rural <- data.frame(svytable(~fex_c + p6133 + clase,
                                       design = BD_survey))
# Si
p6133_1_rural <- p6133_ind_rural %>% filter(p6133_ind_rural$p6133 == 1 & p6133_ind_rural$clase == 2)  # 1.516.726
p6133_1_rural <- sum(p6133_1_rural$Freq)
# No
p6133_2_rural <- p6133_ind_rural %>% filter(p6133_ind_rural$p6133 == 2 & p6133_ind_rural$clase == 2)  # 37.551.878
p6133_2_rural <- sum(p6133_2_rural$Freq)

# total p6133
p6133_total_rural <- p6133_1_rural+p6133_2_rural  # 39.068.605
####################################################################################################################################################################################
# 40. ¿Cuáles de las siguientes fuentes se utilizaron para cubrir los costos de esta hospitalización (incluya consulta médica, exámenes y medicamentos)?
# 40.1 EPS o entidad de seguridad social en la cual está afiliado/a 
p8560s1_ind_rural <- data.frame(svytable(~fex_c + p8560s1 + clase,
                                         design = BD_survey))
# Si
p8560s1_1_rural <- p8560s1_ind_rural %>% filter(p8560s1_ind_rural$p8560s1 == 1 & p8560s1_ind_rural$clase == 2)  # 1.328.576
p8560s1_1_rural <- sum(p8560s1_1_rural$Freq)
# No
p8560s1_2_rural <- p8560s1_ind_rural %>% filter(p8560s1_ind_rural$p8560s1 == 2 & p8560s1_ind_rural$clase == 2)  # 188.149
p8560s1_2_rural <- sum(p8560s1_2_rural$Freq)

# total p8560s1
p8560s1_total_rural <- p8560s1_1_rural+p8560s1_2_rural  # 1.516.726
###############################################################################################################################################################################################
# 40.2 Plan o seguro voluntario (seguro médico, plan complementario o medicina prepagada)
p8560s2_ind_rural <- data.frame(svytable(~fex_c + p8560s2 + clase,
                                         design = BD_survey))
# Si
p8560s2_1_rural <- p8560s2_ind_rural %>% filter(p8560s2_ind_rural$p8560s2 == 1 & p8560s2_ind_rural$clase == 2)  # 97.201
p8560s2_1_rural <- sum(p8560s2_1_rural$Freq)
# No
p8560s2_2_rural <- p8560s2_ind_rural %>% filter(p8560s2_ind_rural$p8560s2 == 2 & p8560s2_ind_rural$clase == 2)  # 1.419.524
p8560s2_2_rural <- sum(p8560s2_2_rural$Freq)

# total p8560s2
p8560s2_total_rural <- p8560s2_1_rural+p8560s2_2_rural  # 1.516.726
##################################################################################################################################################################################
# 40.3 Seguro obligatorio de accidentes de tránsito (SOAT)
p8560s3_ind_rural <- data.frame(svytable(~fex_c + p8560s3 + clase,
                                         design = BD_survey))
# Si
p8560s3_1_rural <- p8560s3_ind_rural %>% filter(p8560s3_ind_rural$p8560s3 == 1 & p8560s3_ind_rural$clase == 2)  # 42.745
p8560s3_1_rural <- sum(p8560s3_1_rural$Freq)
# No
p8560s3_2_rural <- p8560s3_ind_rural %>% filter(p8560s3_ind_rural$p8560s3 == 2 & p8560s3_ind_rural$clase == 2)  # 1.473.981
p8560s3_2_rural <- sum(p8560s3_2_rural$Freq)

# total p8560s3
p8560s3_total_rural <- p8560s3_1_rural+p8560s3_2_rural  # 1.516.726
#######################################################################################################################################################################################
# 40.4 Secretaria de salud o la alcaldía
p8560s4_ind_rural <- data.frame(svytable(~fex_c + p8560s4 + clase,
                                         design = BD_survey))
# Si
p8560s4_1_rural <- p8560s4_ind_rural %>% filter(p8560s4_ind_rural$p8560s4 == 1 & p8560s4_ind_rural$clase == 2)  # 26.853
p8560s4_1_rural <- sum(p8560s4_1_rural$Freq)
# No
p8560s4_2_rural <- p8560s4_ind_rural %>% filter(p8560s4_ind_rural$p8560s4 == 2 & p8560s4_ind_rural$clase == 2)  # 1.489.872
p8560s4_2_rural <- sum(p8560s4_2_rural$Freq)

# total p8560s4
p8560s4_total_rural <- p8560s4_1_rural+p8560s4_2_rural  # 1.516.726
########################################################################################################################################################################################
# 40.5 Recursos propios o familiares
p8560s5_ind_rural <- data.frame(svytable(~fex_c + p8560s5 + clase,
                                         design = BD_survey))
# Si
p8560s5_1_rural <- p8560s5_ind_rural %>% filter(p8560s5_ind_rural$p8560s5 == 1 & p8560s5_ind_rural$clase == 2)  # 100.776
p8560s5_1_rural <- sum(p8560s5_1_rural$Freq)
# No
p8560s5_2_rural <- p8560s5_ind_rural %>% filter(p8560s5_ind_rural$p8560s5 == 2 & p8560s5_ind_rural$clase == 2)  # 1.415.949
p8560s5_2_rural <- sum(p8560s5_2_rural$Freq)

# total p8560s5
p8560s5_total_rural <- p8560s5_1_rural+p8560s5_2_rural  # 1.516.726
##########################################################################################################################################################################################
# 41. ¿Cuánto pagó en total ____ por esta hospitalización?
# 41.1 A traves de EPS
p3189s1_ind_rural <- data.frame(svytable(~fex_c + p3189s1 + clase,
                                         design = BD_survey))
p3189s1_rural <- p3189s1_ind_rural %>% filter(p3189s1_ind_rural$p3189s1 == 1 & p3189s1_ind_rural$clase == 2)   # 1.389.167
p3189s1_rural <- sum(p3189s1_rural$Freq)
# 41.1 A1 Valor a traves de EPS
#p3189s1a1_ind_rural <- data.frame(svytable(~fex_c + p3189s1a1 + fecha,
#design = BD_survey))
#p3189s1a1_rural <- table(BD_ecv_ind$p3189s1a1)

###########################################################################################################################################################################################
# 41.2 Servicio particular o plan voluntario (seguro médico, plan complementario o medicina prepagada)
p3189s2_ind_rural <- data.frame(svytable(~fex_c + p3189s2 + clase,
                                         design = BD_survey))
p3189s2_rural <- p3189s2_ind_rural %>% filter(p3189s2_ind_rural$p3189s2 == 1 & p3189s2_ind_rural$clase == 2)   # 141.061
p3189s2_rural <- sum(p3189s2_rural$Freq)

# 45.2.1 A1 Valor servicio particular o plan voluntario
#p3189s2a1_ind_rural <- data.frame(svytable(~fex_c + p3189s2a1 + clase,
#design = BD_survey))
#p3189s2a1_rural <- table(BD_ecv_ind$p3189s2a1)
#############################################################################################################################################################################################
# 42. Considera que la calidad del servicio en esta hospitalizacion fue:
p8561_ind_rural <- data.frame(svytable(~fex_c + p8561 + clase,
                                       design = BD_survey))
# Muy buena
p8561_1_rural <- p8561_ind_rural %>% filter(p8561_ind_rural$p8561 == 1 & p8561_ind_rural$clase == 2)  # 410.673
p8561_1_rural <- sum(p8561_1_rural$Freq)
# Buena
p8561_2_rural <- p8561_ind_rural %>% filter(p8561_ind_rural$p8561 == 2 & p8561_ind_rural$clase == 2)  # 989.894
p8561_2_rural <- sum(p8561_2_rural$Freq)
# Mala
p8561_3_rural <- p8561_ind_rural %>% filter(p8561_ind_rural$p8561 == 3 & p8561_ind_rural$clase == 2)  # 99.211
p8561_3_rural <- sum(p8561_3_rural$Freq)
# Muy mala
p8561_4_rural <- p8561_ind_rural %>% filter(p8561_ind_rural$p8561 == 4 & p8561_ind_rural$clase == 2)  # 16.946
p8561_4_rural <- sum(p8561_4_rural$Freq)

# total p8561 
p8561_total_rural <- p8561_1_rural+p8561_2_rural+p8561_3_rural+p8561_4_rural  # 1.516.726
#################################################################################################################################################################################################
# 43. ¿... ha estado embarazada?
p3335_ind_rural <- data.frame(svytable(~fex_c + p3335 + clase,
                                       design = BD_survey))
# Si
p3335_1_rural <- p3335_ind_rural %>% filter(p3335_ind_rural$p3335 == 1 & p3335_ind_rural$clase == 2)  # 11.558.294
p3335_1_rural <- sum(p3335_1_rural$Freq)
# No
p3335_2_rural <- p3335_ind_rural %>% filter(p3335_ind_rural$p3335 == 2 & p3335_ind_rural$clase == 2)  # 6.139.230
p3335_2_rural <- sum(p3335_2_rural$Freq)

# total p3335
p3335_total_rural <- p3335_1_rural+p3335_2_rural  # 17.697.525
##################################################################################################################################################################################################
# 43.a ¿Cuantos hijos nacidos vivos ha tenido?
#p3335s1_ind_rural <- data.frame(svytable(~fex_c + p3335s1 + clase,
#design = BD_survey))
#p3335s1_rural <- table(p3335s1_ind_rural$p3335s1)
# 43.b ¿A que edad tuvo su primer hijo?
#p3335s1a1_ind_rural <- data.frame(svytable(~fex_c + p3335s1a1 + clase,
#design = BD_survey))
#p3335s1a1_rural <- table(BD_ecv_ind$p3335s1a1)
####################################################################################################################################################################################################
# 44. ¿... está embarazada actualmente?
p8584_ind_rural <- data.frame(svytable(~fex_c + p8584 + clase,
                                       design = BD_survey))
# Si
p8584_1_rural <- p8584_ind_rural %>% filter(p8584_ind_rural$p8584 == 1 & p8584_ind_rural$clase == 2)  # 165.869
p8584_1_rural <- sum(p8584_1_rural$Freq)
# No
p8584_2_rural <- p8584_ind_rural %>% filter(p8584_ind_rural$p8584 == 2 & p8584_ind_rural$clase == 2)  # 6.565.134
p8584_2_rural <- sum(p8584_2_rural$Freq)

# total p8584
p8584_total_rural <- p8584_1_rural+p8584_2_rural   # 6.731.003
######################################################################################################################################################################################################
# 45. ¿Asiste a control prenatal?
p5694_ind_rural <- data.frame(svytable(~fex_c + p5694 + clase,
                                       design = BD_survey))
# Si
p5694_1_rural <- p5694_ind_rural %>% filter(p5694_ind_rural$p5694 == 1 & p5694_ind_rural$clase == 2)  # 154.103
p5694_1_rural <- sum(p5694_1_rural$Freq)
# No
p5694_2_rural <- p5694_ind_rural %>% filter(p5694_ind_rural$p5694 == 2 & p5694_ind_rural$clase == 2)  # 11.765
p5694_2_rural <- sum(p5694_2_rural$Freq)

# total p5694
p5694_total_rural <- p5694_1_rural+p5694_2_rural  # 165.869
####################################################################################################################################################################################################
# 46. ¿... tiene el esquema completo de vacunación, según su edad?
p5452_ind_rural <- data.frame(svytable(~fex_c + p5452 + clase,
                                       design = BD_survey))
# Si 
p5452_1_rural <- p5452_ind_rural %>% filter(p5452_ind_rural$p5452 == 1 & p5452_ind_rural$clase == 2)  # 2.833.754
p5452_1_rural <- sum(p5452_1_rural$Freq)
# No
p5452_2_rural <- p5452_ind_rural %>% filter(p5452_ind_rural$p5452 == 2 & p5452_ind_rural$clase == 2)  # 326.512
p5452_2_rural <- sum(p5452_2_rural$Freq)

# p5452 total 
p5452_total_rural <- p5452_1_rural+p5452_2_rural   # 3.160.267
#######################################################################################################################################################################################################
# 47. ¿Llevan a ... a control de crecimiento y desarrollo?
p6161_ind_rural <- data.frame(svytable(~fex_c + p6161 + clase,
                                       design = BD_survey))
# Si 
p6161_1_rural <- p6161_ind_rural %>% filter(p6161_ind_rural$p6161 == 1 & p6161_ind_rural$clase == 2)  # 2.701.036
p6161_1_rural <- sum(p6161_1_rural$Freq)
# No
p6161_2_rural <- p6161_ind_rural %>% filter(p6161_ind_rural$p6161 == 2 & p6161_ind_rural$clase == 2)  # 459.230
p6161_2_rural <- sum(p6161_2_rural$Freq)

# p6161 total 
p6161_total_rural <- p6161_1_rural+p6161_2_rural   # 3.160.267

# 47.1 Cuántas veces lo llevaron durante los ÚLTIMOS 12 MESES
#p6161s1_ind_rural <- data.frame(svytable(~fex_c + p6161s1 + clase,
#design = BD_survey))
#p6161s1_rural <- table(BD_ecv_ind$p6161s1)
###########################################################################################################################################################################################################
# 48. ¿Cuál fue la principal razón para no llevar a ... a un control de crecimiento y desarrollo?
p1089_ind_rural <- data.frame(svytable(~fex_c + p1089 + clase,
                                       design = BD_survey))
# No pensó que fuera necesario llevarlo/a a consulta
p1089_1_rural <- p1089_ind_rural %>% filter(p1089_ind_rural$p1089 == 1 & p1089_ind_rural$clase == 2)  # 37.421
p1089_1_rural <- sum(p1089_1_rural$Freq)
# La consulta es muy cara, no tiene plata
p1089_2_rural <- p1089_ind_rural %>% filter(p1089_ind_rural$p1089 == 2 & p1089_ind_rural$clase == 2)  # 13.691
p1089_2_rural <- sum(p1089_2_rural$Freq)
# El lugar donde lo atienden queda muy lejos / no hay servicio cerca
p1089_3_rural <- p1089_ind_rural %>% filter(p1089_ind_rural$p1089 == 3 & p1089_ind_rural$clase == 2)  # 23.288
p1089_3_rural <- sum(p1089_3_rural$Freq)
# No pudo dejar el trabajo/no tuvo tiempo
p1089_4_rural <- p1089_ind_rural %>% filter(p1089_ind_rural$p1089 == 4 & p1089_ind_rural$clase == 2)  # 16.065
p1089_4_rural <- sum(p1089_4_rural$Freq)
# No está afiliado/a a EPS o a régimen subsidiado
p1089_5_rural <- p1089_ind_rural %>% filter(p1089_ind_rural$p1089 == 5 & p1089_ind_rural$clase == 2)  # 147.188
p1089_5_rural <- sum(p1089_5_rural$Freq)
# No consiguió cita cercana en el tiempo o lo atienden muy mal
p1089_6_rural <- p1089_ind_rural %>% filter(p1089_ind_rural$p1089 == 6 & p1089_ind_rural$clase == 2)  # 60.331
p1089_6_rural <- sum(p1089_6_rural$Freq)
# Los trámites en la EPS/IPS son muy complicados
p1089_7_rural <- p1089_ind_rural %>% filter(p1089_ind_rural$p1089 == 7 & p1089_ind_rural$clase == 2)  # 26.507
p1089_7_rural <- sum(p1089_7_rural$Freq)
# Considera que no está en edad o es recién nacido/a
p1089_8_rural <- p1089_ind_rural %>% filter(p1089_ind_rural$p1089 == 8 & p1089_ind_rural$clase == 2)  # 41.680
p1089_8_rural <- sum(p1089_8_rural$Freq)
# No tiene registro civil de nacimiento
p1089_9_rural <- p1089_ind_rural %>% filter(p1089_ind_rural$p1089 == 9 & p1089_ind_rural$clase == 2)  # 13.399
p1089_9_rural <- sum(p1089_9_rural$Freq)
# Cambio de EPS o de municipio
p1089_10_rural <- p1089_ind_rural %>% filter(p1089_ind_rural$p1089 == 10 & p1089_ind_rural$clase == 2)  # 18.302
p1089_10_rural <- sum(p1089_10_rural$Freq)
# Otra
p1089_11_rural <- p1089_ind_rural %>% filter(p1089_ind_rural$p1089 == 11 & p1089_ind_rural$clase == 2)  # 61.352
p1089_11_rural <- sum(p1089_11_rural$Freq)

# total p1089
p1089_total_rural <- p1089_1_rural+p1089_2_rural+p1089_3_rural+p1089_4_rural+p1089_5_rural+p1089_6_rural+p1089_7_rural+
  p1089_8_rural+p1089_9_rural+p1089_10_rural+p1089_11_rural   # 459.230

##################################################################################################################################################
################ CUADROS DEL TOTAL NACIONAL PARA CABECERA Y CENTROS POBLADOS #####################################################################
#p6090
Total_nacional_p6090 <- matrix(c(p6090_1, p6090_2, p6090_9, p6090_total), ncol = 1 , nrow = 4 )
colnames(Total_nacional_p6090) <- c("Total nacional ")
rownames(Total_nacional_p6090) <- c("Si", "No", "No sabe / no informa", "Total p6090")
Total_nacional_p6090

Total_cab_p6090 <- matrix(c(p6090_1_cab, p6090_2_cab, p6090_9_cab,p6090_total_cab), ncol = 1, nrow = 4)
colnames(Total_cab_p6090) <- c("Total cabecera")
rownames(Total_cab_p6090) <- c("Si", "No", "No sabe / no informa", "Total p6090")
Total_cab_p6090

Total_rural_p6090 <- matrix(c(p6090_1_rural, p6090_2_rural, p6090_9_rural, p6090_total_rural), ncol = 1, nrow = 4)
colnames(Total_rural_p6090) <- c("Total rural")
rownames(Total_rural_p6090) <- c("Si", "No", "No sabe / no informa", "Total p6090")
Total_rural_p6090

Indicador_p6090 <- cbind(Total_nacional_p6090, Total_cab_p6090, Total_rural_p6090)
Indicador_p6090

# p768
Total_nacional_p768 <- matrix(c(p768_1, p768_2, p768_3, p768_4, p768_5, p768_6, p768_7, p768_8, p768_total), ncol = 1, nrow = 9)
colnames(Total_nacional_p768) <- c("Total nacional")
rownames(Total_nacional_p768) <- c("Por falta de dinero", "Muchos Trámites", "No le interesa o descuido", "No sabe que debe afiliarse",
                                   "No está vinculado/a laboralmente a una empresa o entidad (Usted o la persona de la que es beneficiario/a)",
                                   " Está en trámite de afiliación", "Problemas con el Sisben (no lo/a han visitado, afiliado/a en otro municipio, lo/a desvincularon, le asignaron puntaje alto)",
                                   "Otra razón", "Total p768")
Total_nacional_p768

Total_cab_p768 <- matrix(c(p768_1_cab, p768_2_cab, p768_3_cab, p768_4_cab, p768_5_cab, p768_6_cab, p768_7_cab, p768_8_cab,
                           p768_total_cab), ncol = 1, nrow = 9)
colnames(Total_cab_p768) <- c("Total cabecera")
rownames(Total_cab_p768) <- c("Por falta de dinero", "Muchos Trámites", "No le interesa o descuido", "No sabe que debe afiliarse",
                                   "No está vinculado/a laboralmente a una empresa o entidad (Usted o la persona de la que es beneficiario/a)",
                                   " Está en trámite de afiliación", "Problemas con el Sisben (no lo/a han visitado, afiliado/a en otro municipio, lo/a desvincularon, le asignaron puntaje alto)",
                                   "Otra razón", "Total p768")
Total_cab_p768

Total_rural_p768 <- matrix(c(p768_1_rural, p768_2_rural, p768_3_rural, p768_4_rural, p768_5_rural, p768_6_rural, p768_7_rural, p768_8_rural,
                             p768_total_rural), ncol = 1, nrow = 9)
colnames(Total_rural_p768) <- c("Total rural")
rownames(Total_rural_p768) <- c("Por falta de dinero", "Muchos Trámites", "No le interesa o descuido", "No sabe que debe afiliarse",
                                "No está vinculado/a laboralmente a una empresa o entidad (Usted o la persona de la que es beneficiario/a)",
                                " Está en trámite de afiliación", "Problemas con el Sisben (no lo/a han visitado, afiliado/a en otro municipio, lo/a desvincularon, le asignaron puntaje alto)",
                                "Otra razón", "Total p768")
Total_rural_p768

Indicador_p768 <- cbind(Total_nacional_p768, Total_cab_p768, Total_rural_p768)
Indicador_p768

# p6100
Total_nacional_p6100 <- matrix(c(p6100_1, p6100_2, p6100_3, p6100_9, p6100_total), ncol = 1, nrow = 5)
colnames(Total_nacional_p6100) <- c("Total nacional")
rownames(Total_nacional_p6100) <- c("Contributivo (EPS)", "Especial (Fuerzas Armadas, Ecopetrol, universidades públicas, magisterio)",
                                   "Subsidiado (EPS-S )", "No sabe / no informa", "Total p6100")
Total_nacional_p6100

Total_cab_p6100 <- matrix(c(p6100_1_cab, p6100_2_cab, p6100_3_cab, p6100_9_cab, p6100_total_cab), ncol = 1, nrow = 5)
colnames(Total_cab_p6100) <- c("Total cabecera")
rownames(Total_cab_p6100) <- c("Contributivo (EPS)", "Especial (Fuerzas Armadas, Ecopetrol, universidades públicas, magisterio)",
                               "Subsidiado (EPS-S )", "No sabe / no informa", "Total p6100")
Total_cab_p6100

Total_rural_p6100 <- matrix(c(p6100_1_rural, p6100_2_rural, p6100_3_rural, p6100_9_rural, p6100_total_rural), ncol = 1, nrow = 5)
colnames(Total_rural_p6100) <- c("Total rural")
rownames(Total_rural_p6100) <- c("Contributivo (EPS)", "Especial (Fuerzas Armadas, Ecopetrol, universidades públicas, magisterio)",
                                 "Subsidiado (EPS-S )", "No sabe / no informa", "Total p6100")
Total_rural_p6100

Indicador_p6100 <- cbind(Total_nacional_p6100, Total_cab_p6100, Total_rural_p6100)
Indicador_p6100


# p6115
Total_nacional_p6115 <- matrix(c(p6115_1, p6115_2, p6115_3, p6115_4, p6115_5, p6115_total), ncol = 1, nrow = 6)
colnames(Total_nacional_p6115) <- c("Total nacional")
rownames(Total_nacional_p6115) <- c("Paga una parte y otra la empresa o patrón ", "Le descuentan de la pensión",
                                    "Paga la totalidad de la afiliación", "Paga completamente la empresa o patrón donde trabaja o trabajó",
                                    "No paga, es beneficiario/a","Total p6115")
Total_nacional_p6115

Total_cab_p6115 <- matrix(c(p6115_1_cab, p6115_2_cab, p6115_3_cab, p6115_4_cab, p6115_5_cab, p6115_total_cab), ncol = 1, nrow = 6)
colnames(Total_cab_p6115) <- c("Total cabecera")
rownames(Total_cab_p6115) <- c("Paga una parte y otra la empresa o patrón ", "Le descuentan de la pensión",
                               "Paga la totalidad de la afiliación", "Paga completamente la empresa o patrón donde trabaja o trabajó",
                               "No paga, es beneficiario/a","Total p6115")
Total_cab_p6115

Total_rural_p6115 <- matrix(c(p6115_1_rural, p6115_2_rural, p6115_3_rural, p6115_4_rural, p6115_5_rural, p6115_total_rural), ncol = 1, nrow = 6)
colnames(Total_rural_p6115) <- c("Total rural")
rownames(Total_rural_p6115) <- c("Paga una parte y otra la empresa o patrón ", "Le descuentan de la pensión",
                                 "Paga la totalidad de la afiliación", "Paga completamente la empresa o patrón donde trabaja o trabajó",
                                 "No paga, es beneficiario/a","Total p6115")
Total_rural_p6115

Indicador_p6115 <- cbind(Total_nacional_p6115, Total_cab_p6115, Total_rural_p6115)
Indicador_p6115

# p5669
Total_nacional_p5669 <- matrix(c(p5669_1, p5669_2, p5669_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p5669) <- c("Total nacional")
rownames(Total_nacional_p5669) <- c("De una persona de este hogar", "De una persona de otro hogar", "Total p5669")
Total_nacional_p5669

Total_cab_p5669 <- matrix(c(p5669_1_cab, p5669_2_cab, p5669_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p5669) <- c("Total cabecera")
rownames(Total_cab_p5669) <- c("De una persona de este hogar", "De una persona de otro hogar", "Total p5669")
Total_cab_p5669

Total_rural_p5669 <- matrix(c(p5669_1_rural, p5669_2_rural, p5669_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p5669) <- c("Total rural")
rownames(Total_rural_p5669) <- c("De una persona de este hogar", "De una persona de otro hogar", "Total p5669")
Total_rural_p5669

Indicador_p5669 <- cbind(Total_nacional_p5669, Total_cab_p5669, Total_rural_p5669)
Indicador_p5669

# p6181
Total_nacional_p6181 <-matrix(c(p6181_1, p6181_2, p6181_3, p6181_4, p6181_9, p6181_total), ncol = 1, nrow = 6)
colnames(Total_nacional_p6181) <- c("Total nacional")
rownames(Total_nacional_p6181) <- c("Muy buena", "Buena", "Mala", "Muy mala", "No sabe", "Total p6181")
Total_nacional_p6181

Total_cab_p6181 <- matrix(c(p6181_1_cab, p6181_2_cab, p6181_3_cab, p6181_4_cab, p6181_9_cab, p6181_total_cab), ncol = 1, nrow = 6)
colnames(Total_cab_p6181) <- c("Total cabecera")
rownames(Total_cab_p6181) <- c("Muy buena", "Buena", "Mala", "Muy mala", "No sabe", "Total p6181")
Total_cab_p6181

Total_rural_p6181 <- matrix(c(p6181_1_rural, p6181_2_rural, p6181_3_rural, p6181_4_rural, p6181_9_rural, p6181_total_rural), ncol = 1, nrow = 6)
colnames(Total_rural_p6181) <- c("Total rural")
rownames(Total_rural_p6181) <- c("Muy buena", "Buena", "Mala", "Muy mala", "No sabe", "Total p6181")
Total_rural_p6181

Indicador_p6181 <- cbind(Total_nacional_p6181, Total_cab_p6181, Total_rural_p6181)
Indicador_p6181

# 798
Total_nacional_p798 <-matrix(c(p798_1, p798_2, p798_3, p798_4, p798_5, p798_6, p798_7, p798_8, p798_total), ncol = 1, nrow = 9)
colnames(Total_nacional_p798) <- c("Total nacional")
rownames(Total_nacional_p798) <- c("Trámites excesivos o dispendiosos", "Mala atención del personal administrativo o asistencial (médicos, enfermeras, etc.)",
                                   "Falta de capacidad, conocimientos o habilidad del personal asistencial", 
                                   "Condiciones deficientes de infraestructura, dotación, mobiliario o accesibilidad para población en condición de discapacidad",
                                  "Demora en la asignación de citas", "Demora en la atención por parte del personal médico",
                                  "Problemas relacionados con los medicamentos", "Otro", "Total p798")
Total_nacional_p798

Total_cab_p798 <- matrix(c(p798_1_cab, p798_2_cab, p798_3_cab, p798_4_cab, p798_5_cab, p798_6_cab, p798_7_cab, p798_8_cab, p798_total_cab), ncol = 1, nrow = 9)
colnames(Total_cab_p798) <- c("Total cabecera")
rownames(Total_cab_p798) <- c("Trámites excesivos o dispendiosos", "Mala atención del personal administrativo o asistencial (médicos, enfermeras, etc.)",
                              "Falta de capacidad, conocimientos o habilidad del personal asistencial", 
                              "Condiciones deficientes de infraestructura, dotación, mobiliario o accesibilidad para población en condición de discapacidad",
                              "Demora en la asignación de citas", "Demora en la atención por parte del personal médico",
                              "Problemas relacionados con los medicamentos", "Otro", "Total p798")
Total_cab_p798

Total_rural_p798 <- matrix(c(p798_1_rural, p798_2_rural, p798_3_rural, p798_4_rural, p798_5_rural, p798_6_rural, p798_7_rural, p798_8_rural, 
                             p798_total_rural), ncol = 1, nrow = 9)
colnames(Total_rural_p798) <- c("Total rural")
rownames(Total_rural_p798) <- c("Trámites excesivos o dispendiosos", "Mala atención del personal administrativo o asistencial (médicos, enfermeras, etc.)",
                                "Falta de capacidad, conocimientos o habilidad del personal asistencial", 
                                "Condiciones deficientes de infraestructura, dotación, mobiliario o accesibilidad para población en condición de discapacidad",
                                "Demora en la asignación de citas", "Demora en la atención por parte del personal médico",
                                "Problemas relacionados con los medicamentos", "Otro", "Total p798")
Total_rural_p798

Indicador_p798 <- cbind(Total_nacional_p798, Total_cab_p798, Total_rural_p798)
Indicador_p798


# p799s2
Total_nacional_p799s2 <-matrix(c(p799s2_1, p799s2_2, p799s2_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p799s2) <- c("Total nacional")
rownames(Total_nacional_p799s2) <- c("Si", "No", "Total p799s2")
Total_nacional_p799s2

Total_cab_p799s2 <- matrix(c(p799s2_1_cab, p799s2_2_cab, p799s2_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p799s2) <- c("Total cabecera")
rownames(Total_cab_p799s2) <- c("Si", "No", "Total p799s2")
Total_cab_p799s2

Total_rural_p799s2 <- matrix(c(p799s2_1_rural, p799s2_2_rural, p799s2_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p799s2) <- c("Total rural")
rownames(Total_rural_p799s2) <- c("Si", "No", "Total p799s2")
Total_rural_p799s2

Indicador_p799s2 <- cbind(Total_nacional_p799s2, Total_cab_p799s2, Total_rural_p799s2)
Indicador_p799s2

# p799s3
Total_nacional_p799s3 <-matrix(c(p799s3_1, p799s3_2, p799s3_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p799s3) <- c("Total nacional")
rownames(Total_nacional_p799s3) <- c("Si", "No", "Total p799s3")
Total_nacional_p799s3

Total_cab_p799s3 <- matrix(c(p799s3_1_cab, p799s3_2_cab, p799s3_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p799s3) <- c("Total cabecera")
rownames(Total_cab_p799s3) <- c("Si", "No", "Total p799s3")
Total_cab_p799s3

Total_rural_p799s3 <- matrix(c(p799s3_1_rural, p799s3_2_rural, p799s3_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p799s3) <- c("Total rural")
rownames(Total_rural_p799s3) <- c("Si", "No", "Total p799s3")
Total_rural_p799s3

Indicador_p799s3 <- cbind(Total_nacional_p799s3, Total_cab_p799s3, Total_rural_p799s3)
Indicador_p799s3

# p799s1
Total_nacional_p799s1 <-matrix(c(p799s1_1, p799s1_2, p799s1_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p799s1) <- c("Total nacional")
rownames(Total_nacional_p799s1) <- c("Si", "No", "Total p799s1")
Total_nacional_p799s1

Total_cab_p799s1 <- matrix(c(p799s1_1_cab, p799s1_2_cab, p799s1_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p799s1) <- c("Total cabecera")
rownames(Total_cab_p799s1) <- c("Si", "No", "Total p799s1")
Total_cab_p799s1

Total_rural_p799s1 <- matrix(c(p799s1_1_rural, p799s1_2_rural, p799s1_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p799s1) <- c("Total rural")
rownames(Total_rural_p799s1) <- c("Si", "No", "Total p799s1")
Total_rural_p799s1

Indicador_p799s1 <- cbind(Total_nacional_p799s1, Total_cab_p799s1, Total_rural_p799s1)
Indicador_p799s1

# p799s4
Total_nacional_p799s4 <-matrix(c(p799s4_1, p799s4_2, p799s4_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p799s4) <- c("Total nacional")
rownames(Total_nacional_p799s4) <- c("Si", "No", "Total p799s4")
Total_nacional_p799s4

Total_cab_p799s4 <- matrix(c(p799s4_1_cab, p799s4_2_cab, p799s4_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p799s4) <- c("Total cabecera")
rownames(Total_cab_p799s4) <- c("Si", "No", "Total p799s4")
Total_cab_p799s4

Total_rural_p799s4 <- matrix(c(p799s4_1_rural, p799s4_2_rural, p799s4_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p799s4) <- c("Total rural")
rownames(Total_rural_p799s4) <- c("Si", "No", "Total p799s4")
Total_rural_p799s4

Indicador_p799s4 <- cbind(Total_nacional_p799s4, Total_cab_p799s4, Total_rural_p799s4)
Indicador_p799s4

# p799s5
Total_nacional_p799s5 <-matrix(c(p799s5_1, p799s5_2, p799s5_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p799s5) <- c("Total nacional")
rownames(Total_nacional_p799s5) <- c("Si", "No", "Total p799s5")
Total_nacional_p799s5

Total_cab_p799s5 <- matrix(c(p799s5_1_cab, p799s5_2_cab, p799s5_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p799s5) <- c("Total cabecera")
rownames(Total_cab_p799s5) <- c("Si", "No", "Total p799s5")
Total_cab_p799s5

Total_rural_p799s5 <- matrix(c(p799s5_1_rural, p799s5_2_rural, p799s5_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p799s5) <- c("Total rural")
rownames(Total_rural_p799s5) <- c("Si", "No", "Total p799s5")
Total_rural_p799s5

Indicador_p799s5 <- cbind(Total_nacional_p799s5, Total_cab_p799s5, Total_rural_p799s5)
Indicador_p799s5

# p6127
Total_nacional_p6127 <-matrix(c(p6127_1, p6127_2, p6127_3, p6127_4, p6127_total), ncol = 1, nrow = 5)
colnames(Total_nacional_p6127) <- c("Total nacional")
rownames(Total_nacional_p6127) <- c("Muy bueno", "Bueno", "Regular", "Malo","Total p6127")
Total_nacional_p6127

Total_cab_p6127 <- matrix(c(p6127_1_cab, p6127_2_cab,  p6127_3_cab, p6127_4_cab, p6127_total_cab), ncol = 1, nrow = 5)
colnames(Total_cab_p6127) <- c("Total cabecera")
rownames(Total_cab_p6127) <- c("Muy bueno", "Bueno", "Regular", "Malo","Total p6127")
Total_cab_p6127

Total_rural_p6127 <- matrix(c(p6127_1_rural, p6127_2_rural,  p6127_3_rural, p6127_4_rural, p6127_total_rural), ncol = 1, nrow = 5)
colnames(Total_rural_p6127) <- c("Total rural")
rownames(Total_rural_p6127) <- c("Muy bueno", "Bueno", "Regular", "Malo","Total p6127")
Total_rural_p6127

Indicador_p6127 <- cbind(Total_nacional_p6127, Total_cab_p6127, Total_rural_p6127)
Indicador_p6127

# p1930
Total_nacional_p1930 <-matrix(c(p1930_1, p1930_2, p1930_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p1930) <- c("Total nacional")
rownames(Total_nacional_p1930) <- c("Si", "No", "Total p1930")
Total_nacional_p1930

Total_cab_p1930 <- matrix(c(p1930_1_cab, p1930_2_cab, p1930_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p1930) <- c("Total cabecera")
rownames(Total_cab_p1930) <- c("Si", "No", "Total p1930")
Total_cab_p1930

Total_rural_p1930 <- matrix(c(p1930_1_rural, p1930_2_rural, p1930_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p1930) <- c("Total rural")
rownames(Total_rural_p1930) <- c("Si", "No", "Total p1930")
Total_rural_p1930

Indicador_p1930 <- cbind(Total_nacional_p1930, Total_cab_p1930, Total_rural_p1930)
Indicador_p1930
 
# p1930s1
Total_nacional_p1930s1 <-matrix(c(p1930s1_1, p1930s1_2, p1930s1_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p1930s1) <- c("Total nacional")
rownames(Total_nacional_p1930s1) <- c("Si", "No", "Total p1930s1")
Total_nacional_p1930s1

Total_cab_p1930s1 <- matrix(c(p1930s1_1_cab, p1930s1_2_cab, p1930s1_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p1930s1) <- c("Total cabecera")
rownames(Total_cab_p1930s1) <- c("Si", "No", "Total p1930s1")
Total_cab_p1930s1

Total_rural_p1930s1 <- matrix(c(p1930s1_1_rural, p1930s1_2_rural, p1930s1_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p1930s1) <- c("Total rural")
rownames(Total_rural_p1930s1) <- c("Si", "No", "Total p1930s1")
Total_rural_p1930s1

Indicador_p1930s1 <- cbind(Total_nacional_p1930s1, Total_cab_p1930s1, Total_rural_p1930s1)
Indicador_p1930s1

# p1906s1
Total_nacional_p1906s1 <-matrix(c(p1906s1_1, p1906s1_2, p1906s1_3, p1906s1_4, p1906s1_total), ncol = 1, nrow = 5)
colnames(Total_nacional_p1906s1) <- c("Total nacional")
rownames(Total_nacional_p1906s1) <- c("No puede hacerlo", "Sí, con mucha dificultad", "Sí, con alguna dificultad","Sin dificultad", 
                                      "Total p1906s1")
Total_nacional_p1906s1

Total_cab_p1906s1 <- matrix(c(p1906s1_1_cab, p1906s1_2_cab, p1906s1_3_cab, p1906s1_4_cab, p1906s1_total_cab), ncol = 1, nrow = 5)
colnames(Total_cab_p1906s1) <- c("Total cabecera")
rownames(Total_cab_p1906s1) <- c("No puede hacerlo", "Sí, con mucha dificultad", "Sí, con alguna dificultad","Sin dificultad", 
                                 "Total p1906s1")
Total_cab_p1906s1

Total_rural_p1906s1 <- matrix(c(p1906s1_1_rural, p1906s1_2_rural, p1906s1_3_rural, p1906s1_4_rural, p1906s1_total_rural), ncol = 1, nrow = 5)
colnames(Total_rural_p1906s1) <- c("Total rural")
rownames(Total_rural_p1906s1) <- c("No puede hacerlo", "Sí, con mucha dificultad", "Sí, con alguna dificultad","Sin dificultad", 
                                   "Total p1906s1")
Total_rural_p1906s1

Indicador_p1906s1 <- cbind(Total_nacional_p1906s1, Total_cab_p1906s1, Total_rural_p1906s1)
Indicador_p1906s1

# p1906s2
Total_nacional_p1906s2 <-matrix(c(p1906s2_1, p1906s2_2, p1906s2_3, p1906s2_4, p1906s2_total), ncol = 1, nrow = 5)
colnames(Total_nacional_p1906s2) <- c("Total nacional")
rownames(Total_nacional_p1906s2) <- c("No puede hacerlo", "Sí, con mucha dificultad", "Sí, con alguna dificultad","Sin dificultad", 
                                      "Total p1906s2")
Total_nacional_p1906s2

Total_cab_p1906s2 <- matrix(c(p1906s2_1_cab, p1906s2_2_cab, p1906s2_3_cab, p1906s2_4_cab, p1906s2_total_cab), ncol = 1, nrow = 5)
colnames(Total_cab_p1906s2) <- c("Total cabecera")
rownames(Total_cab_p1906s2) <- c("No puede hacerlo", "Sí, con mucha dificultad", "Sí, con alguna dificultad","Sin dificultad", 
                                 "Total p1906s2")
Total_cab_p1906s2

Total_rural_p1906s2 <- matrix(c(p1906s2_1_rural, p1906s2_2_rural, p1906s2_3_rural, p1906s2_4_rural, p1906s2_total_rural), ncol = 1, nrow = 5)
colnames(Total_rural_p1906s2) <- c("Total rural")
rownames(Total_rural_p1906s2) <- c("No puede hacerlo", "Sí, con mucha dificultad", "Sí, con alguna dificultad","Sin dificultad", 
                                   "Total p1906s2")
Total_rural_p1906s2

Indicador_p1906s2 <- cbind(Total_nacional_p1906s2, Total_cab_p1906s2, Total_rural_p1906s2)
Indicador_p1906s2

# p1906s3
Total_nacional_p1906s3 <-matrix(c(p1906s3_1, p1906s3_2, p1906s3_3, p1906s3_4, p1906s3_total), ncol = 1, nrow = 5)
colnames(Total_nacional_p1906s3) <- c("Total nacional")
rownames(Total_nacional_p1906s3) <- c("No puede hacerlo", "Sí, con mucha dificultad", "Sí, con alguna dificultad","Sin dificultad", 
                                      "Total p1906s3")
Total_nacional_p1906s3

Total_cab_p1906s3 <- matrix(c(p1906s3_1_cab, p1906s3_2_cab, p1906s3_3_cab, p1906s3_4_cab, p1906s3_total_cab), ncol = 1, nrow = 5)
colnames(Total_cab_p1906s3) <- c("Total cabecera")
rownames(Total_cab_p1906s3) <- c("No puede hacerlo", "Sí, con mucha dificultad", "Sí, con alguna dificultad","Sin dificultad", 
                                 "Total p1906s3")
Total_cab_p1906s3

Total_rural_p1906s3 <- matrix(c(p1906s3_1_rural, p1906s3_2_rural, p1906s3_3_rural, p1906s3_4_rural, p1906s3_total_rural), ncol = 1, nrow = 5)
colnames(Total_rural_p1906s3) <- c("Total rural")
rownames(Total_rural_p1906s3) <- c("No puede hacerlo", "Sí, con mucha dificultad", "Sí, con alguna dificultad","Sin dificultad", 
                                   "Total p1906s3")
Total_rural_p1906s3

Indicador_p1906s3 <- cbind(Total_nacional_p1906s3, Total_cab_p1906s3, Total_rural_p1906s3)
Indicador_p1906s3

# p1906s4
Total_nacional_p1906s4 <-matrix(c(p1906s4_1, p1906s4_2, p1906s4_3, p1906s4_4, p1906s4_total), ncol = 1, nrow = 5)
colnames(Total_nacional_p1906s4) <- c("Total nacional")
rownames(Total_nacional_p1906s4) <- c("No puede hacerlo", "Sí, con mucha dificultad", "Sí, con alguna dificultad","Sin dificultad", 
                                      "Total p1906s4")
Total_nacional_p1906s4

Total_cab_p1906s4 <- matrix(c(p1906s4_1_cab, p1906s4_2_cab, p1906s4_3_cab, p1906s4_4_cab, p1906s4_total_cab), ncol = 1, nrow = 5)
colnames(Total_cab_p1906s4) <- c("Total cabecera")
rownames(Total_cab_p1906s4) <- c("No puede hacerlo", "Sí, con mucha dificultad", "Sí, con alguna dificultad","Sin dificultad", 
                                 "Total p1906s4")
Total_cab_p1906s4

Total_rural_p1906s4 <- matrix(c(p1906s4_1_rural, p1906s4_2_rural, p1906s4_3_rural, p1906s4_4_rural, p1906s4_total_rural), ncol = 1, nrow = 5)
colnames(Total_rural_p1906s4) <- c("Total rural")
rownames(Total_rural_p1906s4) <- c("No puede hacerlo", "Sí, con mucha dificultad", "Sí, con alguna dificultad","Sin dificultad", 
                                   "Total p1906s4")
Total_rural_p1906s4

Indicador_p1906s4 <- cbind(Total_nacional_p1906s4, Total_cab_p1906s4, Total_rural_p1906s4)
Indicador_p1906s4

# p1906s5
Total_nacional_p1906s5 <-matrix(c(p1906s5_1, p1906s5_2, p1906s5_3, p1906s5_4, p1906s5_total), ncol = 1, nrow = 5)
colnames(Total_nacional_p1906s5) <- c("Total nacional")
rownames(Total_nacional_p1906s5) <- c("No puede hacerlo", "Sí, con mucha dificultad", "Sí, con alguna dificultad","Sin dificultad", 
                                      "Total p1906s5")
Total_nacional_p1906s5

Total_cab_p1906s5 <- matrix(c(p1906s5_1_cab, p1906s5_2_cab, p1906s5_3_cab, p1906s5_4_cab, p1906s5_total_cab), ncol = 1, nrow = 5)
colnames(Total_cab_p1906s5) <- c("Total cabecera")
rownames(Total_cab_p1906s5) <- c("No puede hacerlo", "Sí, con mucha dificultad", "Sí, con alguna dificultad","Sin dificultad", 
                                 "Total p1906s5")
Total_cab_p1906s5

Total_rural_p1906s5 <- matrix(c(p1906s5_1_rural, p1906s5_2_rural, p1906s5_3_rural, p1906s5_4_rural, p1906s5_total_rural), ncol = 1, nrow = 5)
colnames(Total_rural_p1906s5) <- c("Total rural")
rownames(Total_rural_p1906s5) <- c("No puede hacerlo", "Sí, con mucha dificultad", "Sí, con alguna dificultad","Sin dificultad", 
                                   "Total p1906s5")
Total_rural_p1906s5

Indicador_p1906s5 <- cbind(Total_nacional_p1906s5, Total_cab_p1906s5, Total_rural_p1906s5)
Indicador_p1906s5

# p1906s6
Total_nacional_p1906s6 <-matrix(c(p1906s6_1, p1906s6_2, p1906s6_3, p1906s6_4, p1906s6_total), ncol = 1, nrow = 5)
colnames(Total_nacional_p1906s6) <- c("Total nacional")
rownames(Total_nacional_p1906s6) <- c("No puede hacerlo", "Sí, con mucha dificultad", "Sí, con alguna dificultad","Sin dificultad", 
                                      "Total p1906s6")
Total_nacional_p1906s6

Total_cab_p1906s6 <- matrix(c(p1906s6_1_cab, p1906s6_2_cab, p1906s6_3_cab, p1906s6_4_cab, p1906s6_total_cab), ncol = 1, nrow = 5)
colnames(Total_cab_p1906s6) <- c("Total cabecera")
rownames(Total_cab_p1906s6) <- c("No puede hacerlo", "Sí, con mucha dificultad", "Sí, con alguna dificultad","Sin dificultad", 
                                 "Total p1906s6")
Total_cab_p1906s6

Total_rural_p1906s6 <- matrix(c(p1906s6_1_rural, p1906s6_2_rural, p1906s6_3_rural, p1906s6_4_rural, p1906s6_total_rural), ncol = 1, nrow = 5)
colnames(Total_rural_p1906s6) <- c("Total rural")
rownames(Total_rural_p1906s6) <- c("No puede hacerlo", "Sí, con mucha dificultad", "Sí, con alguna dificultad","Sin dificultad", 
                                   "Total p1906s6")
Total_rural_p1906s6

Indicador_p1906s6 <- cbind(Total_nacional_p1906s6, Total_cab_p1906s6, Total_rural_p1906s6)
Indicador_p1906s6

# p1906s7
Total_nacional_p1906s7 <-matrix(c(p1906s7_1, p1906s7_2, p1906s7_3, p1906s7_4, p1906s7_total), ncol = 1, nrow = 5)
colnames(Total_nacional_p1906s7) <- c("Total nacional")
rownames(Total_nacional_p1906s7) <- c("No puede hacerlo", "Sí, con mucha dificultad", "Sí, con alguna dificultad","Sin dificultad", 
                                      "Total p1906s7")
Total_nacional_p1906s7

Total_cab_p1906s7 <- matrix(c(p1906s7_1_cab, p1906s7_2_cab, p1906s7_3_cab, p1906s7_4_cab, p1906s7_total_cab), ncol = 1, nrow = 5)
colnames(Total_cab_p1906s7) <- c("Total cabecera")
rownames(Total_cab_p1906s7) <- c("No puede hacerlo", "Sí, con mucha dificultad", "Sí, con alguna dificultad","Sin dificultad", 
                                 "Total p1906s7")
Total_cab_p1906s7

Total_rural_p1906s7 <- matrix(c(p1906s7_1_rural, p1906s7_2_rural, p1906s7_3_rural, p1906s7_4_rural, p1906s7_total_rural), ncol = 1, nrow = 5)
colnames(Total_rural_p1906s7) <- c("Total rural")
rownames(Total_rural_p1906s7) <- c("No puede hacerlo", "Sí, con mucha dificultad", "Sí, con alguna dificultad","Sin dificultad", 
                                   "Total p1906s7")
Total_rural_p1906s7

Indicador_p1906s7 <- cbind(Total_nacional_p1906s7, Total_cab_p1906s7, Total_rural_p1906s7)
Indicador_p1906s7

# p1906s8
Total_nacional_p1906s8 <-matrix(c(p1906s8_1, p1906s8_2, p1906s8_3, p1906s8_4, p1906s8_total), ncol = 1, nrow = 5)
colnames(Total_nacional_p1906s8) <- c("Total nacional")
rownames(Total_nacional_p1906s8) <- c("No puede hacerlo", "Sí, con mucha dificultad", "Sí, con alguna dificultad","Sin dificultad", 
                                      "Total p1906s8")
Total_nacional_p1906s8

Total_cab_p1906s8 <- matrix(c(p1906s8_1_cab, p1906s8_2_cab, p1906s8_3_cab, p1906s8_4_cab, p1906s8_total_cab), ncol = 1, nrow = 5)
colnames(Total_cab_p1906s8) <- c("Total cabecera")
rownames(Total_cab_p1906s8) <- c("No puede hacerlo", "Sí, con mucha dificultad", "Sí, con alguna dificultad","Sin dificultad", 
                                 "Total p1906s8")
Total_cab_p1906s8

Total_rural_p1906s8 <- matrix(c(p1906s8_1_rural, p1906s8_2_rural, p1906s8_3_rural, p1906s8_4_rural, p1906s8_total_rural), ncol = 1, nrow = 5)
colnames(Total_rural_p1906s8) <- c("Total rural")
rownames(Total_rural_p1906s8) <- c("No puede hacerlo", "Sí, con mucha dificultad", "Sí, con alguna dificultad","Sin dificultad", 
                                   "Total p1906s8")
Total_rural_p1906s8

Indicador_p1906s8 <- cbind(Total_nacional_p1906s8, Total_cab_p1906s8, Total_rural_p1906s8)
Indicador_p1906s8

# 1908s1 
Total_nacional_p1908s1 <-matrix(c(p1908s1_1, p1908s1_2, p1908s1_3, p1908s1_4, p1908s1_5, p1908s1_6, p1908s1_7,
                                  p1908s1_8, p1908s1_9, p1908s1_total), ncol = 1, nrow = 10)
colnames(Total_nacional_p1908s1) <- c("Total nacional")
rownames(Total_nacional_p1908s1) <- c("Porque nació así", "Por enfermedad", "Por accidente laboral o enfermedad profesional", 
                                      "Por otro tipo de accidente", "Por edad avanzada", "Por el conflicto armado",
                                      "Por violencia NO asociada al conflicto armado", "Por otra causa", "No sabe",
                                      "Total p1908s1")
Total_nacional_p1908s1

Total_cab_p1908s1 <- matrix(c(p1908s1_1_cab, p1908s1_2_cab, p1908s1_3_cab, p1908s1_4_cab, p1908s1_5_cab, p1908s1_6_cab, p1908s1_7_cab,
                              p1908s1_8_cab, p1908s1_9_cab, p1908s1_total_cab), ncol = 1, nrow = 10)
colnames(Total_cab_p1908s1) <- c("Total cabecera")
rownames(Total_cab_p1908s1) <- c("Porque nació así", "Por enfermedad", "Por accidente laboral o enfermedad profesional", 
                                 "Por otro tipo de accidente", "Por edad avanzada", "Por el conflicto armado",
                                 "Por violencia NO asociada al conflicto armado", "Por otra causa", "No sabe",
                                 "Total p1908s1")
Total_cab_p1908s1

Total_rural_p1908s1 <- matrix(c(p1908s1_1_rural, p1908s1_2_rural, p1908s1_3_rural, p1908s1_4_rural, p1908s1_5_rural, p1908s1_6_rural, p1908s1_7_rural,
                                p1908s1_8_rural, p1908s1_9_rural, p1908s1_total_rural), ncol = 1, nrow = 10)
colnames(Total_rural_p1908s1) <- c("Total rural")
rownames(Total_rural_p1908s1) <- c("Porque nació así", "Por enfermedad", "Por accidente laboral o enfermedad profesional", 
                                   "Por otro tipo de accidente", "Por edad avanzada", "Por el conflicto armado",
                                   "Por violencia NO asociada al conflicto armado", "Por otra causa", "No sabe",
                                   "Total p1908s1")
Total_rural_p1908s1

Indicador_p1908s1 <- cbind(Total_nacional_p1908s1, Total_cab_p1908s1, Total_rural_p1908s1)
Indicador_p1908s1

# 1908s2
Total_nacional_p1908s2 <-matrix(c(p1908s2_1, p1908s2_2, p1908s2_3, p1908s2_4, p1908s2_5, p1908s2_6, p1908s2_7,
                                  p1908s2_8, p1908s2_9, p1908s2_total), ncol = 1, nrow = 10)
colnames(Total_nacional_p1908s2) <- c("Total nacional")
rownames(Total_nacional_p1908s2) <- c("Porque nació así", "Por enfermedad", "Por accidente laboral o enfermedad profesional", 
                                      "Por otro tipo de accidente", "Por edad avanzada", "Por el conflicto armado",
                                      "Por violencia NO asociada al conflicto armado", "Por otra causa", "No sabe",
                                      "Total p1908s2")
Total_nacional_p1908s2

Total_cab_p1908s2 <- matrix(c(p1908s2_1_cab, p1908s2_2_cab, p1908s2_3_cab, p1908s2_4_cab, p1908s2_5_cab, p1908s2_6_cab, p1908s2_7_cab,
                              p1908s2_8_cab, p1908s2_9_cab, p1908s2_total_cab), ncol = 1, nrow = 10)
colnames(Total_cab_p1908s2) <- c("Total cabecera")
rownames(Total_cab_p1908s2) <- c("Porque nació así", "Por enfermedad", "Por accidente laboral o enfermedad profesional", 
                                 "Por otro tipo de accidente", "Por edad avanzada", "Por el conflicto armado",
                                 "Por violencia NO asociada al conflicto armado", "Por otra causa", "No sabe",
                                 "Total p1908s2")
Total_cab_p1908s2

Total_rural_p1908s2 <- matrix(c(p1908s2_1_rural, p1908s2_2_rural, p1908s2_3_rural, p1908s2_4_rural, p1908s2_5_rural, p1908s2_6_rural, p1908s2_7_rural,
                                p1908s2_8_rural, p1908s2_9_rural, p1908s2_total_rural), ncol = 1, nrow = 10)
colnames(Total_rural_p1908s2) <- c("Total rural")
rownames(Total_rural_p1908s2) <- c("Porque nació así", "Por enfermedad", "Por accidente laboral o enfermedad profesional", 
                                   "Por otro tipo de accidente", "Por edad avanzada", "Por el conflicto armado",
                                   "Por violencia NO asociada al conflicto armado", "Por otra causa", "No sabe",
                                   "Total p1908s2")
Total_rural_p1908s2

Indicador_p1908s2 <- cbind(Total_nacional_p1908s2, Total_cab_p1908s2, Total_rural_p1908s2)
Indicador_p1908s2

# p1908s3
Total_nacional_p1908s3 <-matrix(c(p1908s3_1, p1908s3_2, p1908s3_3, p1908s3_4, p1908s3_5, p1908s3_6, p1908s3_7,
                                  p1908s3_8, p1908s3_9, p1908s3_total), ncol = 1, nrow = 10)
colnames(Total_nacional_p1908s3) <- c("Total nacional")
rownames(Total_nacional_p1908s3) <- c("Porque nació así", "Por enfermedad", "Por accidente laboral o enfermedad profesional", 
                                      "Por otro tipo de accidente", "Por edad avanzada", "Por el conflicto armado",
                                      "Por violencia NO asociada al conflicto armado", "Por otra causa", "No sabe",
                                      "Total p1908s3")
Total_nacional_p1908s3

Total_cab_p1908s3 <- matrix(c(p1908s3_1_cab, p1908s3_2_cab, p1908s3_3_cab, p1908s3_4_cab, p1908s3_5_cab, p1908s3_6_cab, p1908s3_7_cab,
                              p1908s3_8_cab, p1908s3_9_cab, p1908s3_total_cab), ncol = 1, nrow = 10)
colnames(Total_cab_p1908s3) <- c("Total cabecera")
rownames(Total_cab_p1908s3) <- c("Porque nació así", "Por enfermedad", "Por accidente laboral o enfermedad profesional", 
                                 "Por otro tipo de accidente", "Por edad avanzada", "Por el conflicto armado",
                                 "Por violencia NO asociada al conflicto armado", "Por otra causa", "No sabe",
                                 "Total p1908s3")
Total_cab_p1908s3

Total_rural_p1908s3 <- matrix(c(p1908s3_1_rural, p1908s3_2_rural, p1908s3_3_rural, p1908s3_4_rural, p1908s3_5_rural, p1908s3_6_rural, p1908s3_7_rural,
                                p1908s3_8_rural, p1908s3_9_rural, p1908s3_total_rural), ncol = 1, nrow = 10)
colnames(Total_rural_p1908s3) <- c("Total rural")
rownames(Total_rural_p1908s3) <- c("Porque nació así", "Por enfermedad", "Por accidente laboral o enfermedad profesional", 
                                   "Por otro tipo de accidente", "Por edad avanzada", "Por el conflicto armado",
                                   "Por violencia NO asociada al conflicto armado", "Por otra causa", "No sabe",
                                   "Total p1908s3")
Total_rural_p1908s3

Indicador_p1908s3 <- cbind(Total_nacional_p1908s3, Total_cab_p1908s3, Total_rural_p1908s3)
Indicador_p1908s3

# p1908s4
Total_nacional_p1908s4 <-matrix(c(p1908s4_1, p1908s4_2, p1908s4_3, p1908s4_4, p1908s4_5, p1908s4_6, p1908s4_7,
                                  p1908s4_8, p1908s4_9, p1908s4_total), ncol = 1, nrow = 10)
colnames(Total_nacional_p1908s4) <- c("Total nacional")
rownames(Total_nacional_p1908s4) <- c("Porque nació así", "Por enfermedad", "Por accidente laboral o enfermedad profesional", 
                                      "Por otro tipo de accidente", "Por edad avanzada", "Por el conflicto armado",
                                      "Por violencia NO asociada al conflicto armado", "Por otra causa", "No sabe",
                                      "Total p1908s4")
Total_nacional_p1908s4

Total_cab_p1908s4 <- matrix(c(p1908s4_1_cab, p1908s4_2_cab, p1908s4_3_cab, p1908s4_4_cab, p1908s4_5_cab, p1908s4_6_cab, p1908s4_7_cab,
                              p1908s4_8_cab, p1908s4_9_cab, p1908s4_total_cab), ncol = 1, nrow = 10)
colnames(Total_cab_p1908s4) <- c("Total cabecera")
rownames(Total_cab_p1908s4) <- c("Porque nació así", "Por enfermedad", "Por accidente laboral o enfermedad profesional", 
                                 "Por otro tipo de accidente", "Por edad avanzada", "Por el conflicto armado",
                                 "Por violencia NO asociada al conflicto armado", "Por otra causa", "No sabe",
                                 "Total p1908s4")
Total_cab_p1908s4

Total_rural_p1908s4 <- matrix(c(p1908s4_1_rural, p1908s4_2_rural, p1908s4_3_rural, p1908s4_4_rural, p1908s4_5_rural, p1908s4_6_rural, p1908s4_7_rural,
                                p1908s4_8_rural, p1908s4_9_rural, p1908s4_total_rural), ncol = 1, nrow = 10)
colnames(Total_rural_p1908s4) <- c("Total rural")
rownames(Total_rural_p1908s4) <- c("Porque nació así", "Por enfermedad", "Por accidente laboral o enfermedad profesional", 
                                   "Por otro tipo de accidente", "Por edad avanzada", "Por el conflicto armado",
                                   "Por violencia NO asociada al conflicto armado", "Por otra causa", "No sabe",
                                   "Total p1908s4")
Total_rural_p1908s4

Indicador_p1908s4 <- cbind(Total_nacional_p1908s4, Total_cab_p1908s4, Total_rural_p1908s4)
Indicador_p1908s4

# p1908s5
Total_nacional_p1908s5 <-matrix(c(p1908s5_1, p1908s5_2, p1908s5_3, p1908s5_4, p1908s5_5, p1908s5_6, p1908s5_7,
                                  p1908s5_8, p1908s5_9, p1908s5_total), ncol = 1, nrow = 10)
colnames(Total_nacional_p1908s5) <- c("Total nacional")
rownames(Total_nacional_p1908s5) <- c("Porque nació así", "Por enfermedad", "Por accidente laboral o enfermedad profesional", 
                                      "Por otro tipo de accidente", "Por edad avanzada", "Por el conflicto armado",
                                      "Por violencia NO asociada al conflicto armado", "Por otra causa", "No sabe",
                                      "Total p1908s5")
Total_nacional_p1908s5

Total_cab_p1908s5 <- matrix(c(p1908s5_1_cab, p1908s5_2_cab, p1908s5_3_cab, p1908s5_4_cab, p1908s5_5_cab, p1908s5_6_cab, p1908s5_7_cab,
                              p1908s5_8_cab, p1908s5_9_cab, p1908s5_total_cab), ncol = 1, nrow = 10)
colnames(Total_cab_p1908s5) <- c("Total cabecera")
rownames(Total_cab_p1908s5) <- c("Porque nació así", "Por enfermedad", "Por accidente laboral o enfermedad profesional", 
                                 "Por otro tipo de accidente", "Por edad avanzada", "Por el conflicto armado",
                                 "Por violencia NO asociada al conflicto armado", "Por otra causa", "No sabe",
                                 "Total p1908s5")
Total_cab_p1908s5

Total_rural_p1908s5 <- matrix(c(p1908s5_1_rural, p1908s5_2_rural, p1908s5_3_rural, p1908s5_4_rural, p1908s5_5_rural, p1908s5_6_rural, p1908s5_7_rural,
                                p1908s5_8_rural, p1908s5_9_rural, p1908s5_total_rural), ncol = 1, nrow = 10)
colnames(Total_rural_p1908s5) <- c("Total rural")
rownames(Total_rural_p1908s5) <- c("Porque nació así", "Por enfermedad", "Por accidente laboral o enfermedad profesional", 
                                   "Por otro tipo de accidente", "Por edad avanzada", "Por el conflicto armado",
                                   "Por violencia NO asociada al conflicto armado", "Por otra causa", "No sabe",
                                   "Total p1908s5")
Total_rural_p1908s5

Indicador_p1908s5 <- cbind(Total_nacional_p1908s5, Total_cab_p1908s5, Total_rural_p1908s5)
Indicador_p1908s5

# p1908s6
Total_nacional_p1908s6 <-matrix(c(p1908s6_1, p1908s6_2, p1908s6_3, p1908s6_4, p1908s6_5, p1908s6_6, p1908s6_7,
                                  p1908s6_8, p1908s6_9, p1908s6_total), ncol = 1, nrow = 10)
colnames(Total_nacional_p1908s6) <- c("Total nacional")
rownames(Total_nacional_p1908s6) <- c("Porque nació así", "Por enfermedad", "Por accidente laboral o enfermedad profesional", 
                                      "Por otro tipo de accidente", "Por edad avanzada", "Por el conflicto armado",
                                      "Por violencia NO asociada al conflicto armado", "Por otra causa", "No sabe",
                                      "Total p1908s6")
Total_nacional_p1908s6

Total_cab_p1908s6 <- matrix(c(p1908s6_1_cab, p1908s6_2_cab, p1908s6_3_cab, p1908s6_4_cab, p1908s6_5_cab, p1908s6_6_cab, p1908s6_7_cab,
                              p1908s6_8_cab, p1908s6_9_cab, p1908s6_total_cab), ncol = 1, nrow = 10)
colnames(Total_cab_p1908s6) <- c("Total cabecera")
rownames(Total_cab_p1908s6) <- c("Porque nació así", "Por enfermedad", "Por accidente laboral o enfermedad profesional", 
                                 "Por otro tipo de accidente", "Por edad avanzada", "Por el conflicto armado",
                                 "Por violencia NO asociada al conflicto armado", "Por otra causa", "No sabe",
                                 "Total p1908s6")
Total_cab_p1908s6

Total_rural_p1908s6 <- matrix(c(p1908s6_1_rural, p1908s6_2_rural, p1908s6_3_rural, p1908s6_4_rural, p1908s6_5_rural, p1908s6_6_rural, p1908s6_7_rural,
                                p1908s6_8_rural, p1908s6_9_rural, p1908s6_total_rural), ncol = 1, nrow = 10)
colnames(Total_rural_p1908s6) <- c("Total rural")
rownames(Total_rural_p1908s6) <- c("Porque nació así", "Por enfermedad", "Por accidente laboral o enfermedad profesional", 
                                   "Por otro tipo de accidente", "Por edad avanzada", "Por el conflicto armado",
                                   "Por violencia NO asociada al conflicto armado", "Por otra causa", "No sabe",
                                   "Total p1908s6")
Total_rural_p1908s6

Indicador_p1908s6 <- cbind(Total_nacional_p1908s6, Total_cab_p1908s6, Total_rural_p1908s6)
Indicador_p1908s6

# p1908s7
Total_nacional_p1908s7 <-matrix(c(p1908s7_1, p1908s7_2, p1908s7_3, p1908s7_4, p1908s7_5, p1908s7_6, p1908s7_7,
                                  p1908s7_8, p1908s7_9, p1908s7_total), ncol = 1, nrow = 10)
colnames(Total_nacional_p1908s7) <- c("Total nacional")
rownames(Total_nacional_p1908s7) <- c("Porque nació así", "Por enfermedad", "Por accidente laboral o enfermedad profesional", 
                                      "Por otro tipo de accidente", "Por edad avanzada", "Por el conflicto armado",
                                      "Por violencia NO asociada al conflicto armado", "Por otra causa", "No sabe",
                                      "Total p1908s7")
Total_nacional_p1908s7

Total_cab_p1908s7 <- matrix(c(p1908s7_1_cab, p1908s7_2_cab, p1908s7_3_cab, p1908s7_4_cab, p1908s7_5_cab, p1908s7_6_cab, p1908s7_7_cab,
                              p1908s7_8_cab, p1908s7_9_cab, p1908s7_total_cab), ncol = 1, nrow = 10)
colnames(Total_cab_p1908s7) <- c("Total cabecera")
rownames(Total_cab_p1908s7) <- c("Porque nació así", "Por enfermedad", "Por accidente laboral o enfermedad profesional", 
                                 "Por otro tipo de accidente", "Por edad avanzada", "Por el conflicto armado",
                                 "Por violencia NO asociada al conflicto armado", "Por otra causa", "No sabe",
                                 "Total p1908s7")
Total_cab_p1908s7

Total_rural_p1908s7 <- matrix(c(p1908s7_1_rural, p1908s7_2_rural, p1908s7_3_rural, p1908s7_4_rural, p1908s7_5_rural, p1908s7_6_rural, p1908s7_7_rural,
                                p1908s7_8_rural, p1908s7_9_rural, p1908s7_total_rural), ncol = 1, nrow = 10)
colnames(Total_rural_p1908s7) <- c("Total rural")
rownames(Total_rural_p1908s7) <- c("Porque nació así", "Por enfermedad", "Por accidente laboral o enfermedad profesional", 
                                   "Por otro tipo de accidente", "Por edad avanzada", "Por el conflicto armado",
                                   "Por violencia NO asociada al conflicto armado", "Por otra causa", "No sabe",
                                   "Total p1908s7")
Total_rural_p1908s7

Indicador_p1908s7 <- cbind(Total_nacional_p1908s7, Total_cab_p1908s7, Total_rural_p1908s7)
Indicador_p1908s7

# p1908s8
Total_nacional_p1908s8 <-matrix(c(p1908s8_1, p1908s8_2, p1908s8_3, p1908s8_4, p1908s8_5, p1908s8_6, p1908s8_7,
                                  p1908s8_8, p1908s8_9, p1908s8_total), ncol = 1, nrow = 10)
colnames(Total_nacional_p1908s8) <- c("Total nacional")
rownames(Total_nacional_p1908s8) <- c("Porque nació así", "Por enfermedad", "Por accidente laboral o enfermedad profesional", 
                                      "Por otro tipo de accidente", "Por edad avanzada", "Por el conflicto armado",
                                      "Por violencia NO asociada al conflicto armado", "Por otra causa", "No sabe",
                                      "Total p1908s8")
Total_nacional_p1908s8

Total_cab_p1908s8 <- matrix(c(p1908s8_1_cab, p1908s8_2_cab, p1908s8_3_cab, p1908s8_4_cab, p1908s8_5_cab, p1908s8_6_cab, p1908s8_7_cab,
                              p1908s8_8_cab, p1908s8_9_cab, p1908s8_total_cab), ncol = 1, nrow = 10)
colnames(Total_cab_p1908s8) <- c("Total cabecera")
rownames(Total_cab_p1908s8) <- c("Porque nació así", "Por enfermedad", "Por accidente laboral o enfermedad profesional", 
                                 "Por otro tipo de accidente", "Por edad avanzada", "Por el conflicto armado",
                                 "Por violencia NO asociada al conflicto armado", "Por otra causa", "No sabe",
                                 "Total p1908s8")
Total_cab_p1908s8

Total_rural_p1908s8 <- matrix(c(p1908s8_1_rural, p1908s8_2_rural, p1908s8_3_rural, p1908s8_4_rural, p1908s8_5_rural, p1908s8_6_rural, p1908s8_7_rural,
                                p1908s8_8_rural, p1908s8_9_rural, p1908s8_total_rural), ncol = 1, nrow = 10)
colnames(Total_rural_p1908s8) <- c("Total rural")
rownames(Total_rural_p1908s8) <- c("Porque nació así", "Por enfermedad", "Por accidente laboral o enfermedad profesional", 
                                   "Por otro tipo de accidente", "Por edad avanzada", "Por el conflicto armado",
                                   "Por violencia NO asociada al conflicto armado", "Por otra causa", "No sabe",
                                   "Total p1908s8")
Total_rural_p1908s8

Indicador_p1908s8 <- cbind(Total_nacional_p1908s8, Total_cab_p1908s8, Total_rural_p1908s8)
Indicador_p1908s8

# p1909s1
Total_nacional_p1909s1 <-matrix(c(p1909s1_1, p1909s1_2, p1909s1_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p1909s1) <- c("Total nacional")
rownames(Total_nacional_p1909s1) <- c("Si", "No", "Total p1909s1")
Total_nacional_p1909s1

Total_cab_p1909s1 <- matrix(c(p1909s1_1_cab, p1909s1_2_cab, p1909s1_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p1909s1) <- c("Total cabecera")
rownames(Total_cab_p1909s1) <- c("Si", "No", "Total p1909s1")
Total_cab_p1909s1

Total_rural_p1909s1 <- matrix(c(p1909s1_1_rural, p1909s1_2_rural, p1909s1_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p1909s1) <- c("Total rural")
rownames(Total_rural_p1909s1) <- c("Si", "No", "Total p1909s1")
Total_rural_p1909s1

Indicador_p1909s1 <- cbind(Total_nacional_p1909s1, Total_cab_p1909s1, Total_rural_p1909s1)
Indicador_p1909s1

# p1909s2
Total_nacional_p1909s2 <-matrix(c(p1909s2_1, p1909s2_2, p1909s2_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p1909s2) <- c("Total nacional")
rownames(Total_nacional_p1909s2) <- c("Si", "No", "Total p1909s2")
Total_nacional_p1909s2

Total_cab_p1909s2 <- matrix(c(p1909s2_1_cab, p1909s2_2_cab, p1909s2_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p1909s2) <- c("Total cabecera")
rownames(Total_cab_p1909s2) <- c("Si", "No", "Total p1909s2")
Total_cab_p1909s2

Total_rural_p1909s2 <- matrix(c(p1909s2_1_rural, p1909s2_2_rural, p1909s2_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p1909s2) <- c("Total rural")
rownames(Total_rural_p1909s2) <- c("Si", "No", "Total p1909s2")
Total_rural_p1909s2

Indicador_p1909s2 <- cbind(Total_nacional_p1909s2, Total_cab_p1909s2, Total_rural_p1909s2)
Indicador_p1909s2

# p1909s3
Total_nacional_p1909s3 <-matrix(c(p1909s3_1, p1909s3_2, p1909s3_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p1909s3) <- c("Total nacional")
rownames(Total_nacional_p1909s3) <- c("Si", "No", "Total p1909s3")
Total_nacional_p1909s3

Total_cab_p1909s3 <- matrix(c(p1909s3_1_cab, p1909s3_2_cab, p1909s3_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p1909s3) <- c("Total cabecera")
rownames(Total_cab_p1909s3) <- c("Si", "No", "Total p1909s3")
Total_cab_p1909s3

Total_rural_p1909s3 <- matrix(c(p1909s3_1_rural, p1909s3_2_rural, p1909s3_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p1909s3) <- c("Total rural")
rownames(Total_rural_p1909s3) <- c("Si", "No", "Total p1909s3")
Total_rural_p1909s3

Indicador_p1909s3 <- cbind(Total_nacional_p1909s3, Total_cab_p1909s3, Total_rural_p1909s3)
Indicador_p1909s3

# p1909s4
Total_nacional_p1909s4 <-matrix(c(p1909s4_1, p1909s4_2, p1909s4_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p1909s4) <- c("Total nacional")
rownames(Total_nacional_p1909s4) <- c("Si", "No", "Total p1909s4")
Total_nacional_p1909s4

Total_cab_p1909s4 <- matrix(c(p1909s4_1_cab, p1909s4_2_cab, p1909s4_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p1909s4) <- c("Total cabecera")
rownames(Total_cab_p1909s4) <- c("Si", "No", "Total p1909s4")
Total_cab_p1909s4

Total_rural_p1909s4 <- matrix(c(p1909s4_1_rural, p1909s4_2_rural, p1909s4_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p1909s4) <- c("Total rural")
rownames(Total_rural_p1909s4) <- c("Si", "No", "Total p1909s4")
Total_rural_p1909s4

Indicador_p1909s4 <- cbind(Total_nacional_p1909s4, Total_cab_p1909s4, Total_rural_p1909s4)
Indicador_p1909s4

# p1909s5
Total_nacional_p1909s5 <-matrix(c(p1909s5_1, p1909s5_2, p1909s5_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p1909s5) <- c("Total nacional")
rownames(Total_nacional_p1909s5) <- c("Si", "No", "Total p1909s5")
Total_nacional_p1909s5

Total_cab_p1909s5 <- matrix(c(p1909s5_1_cab, p1909s5_2_cab, p1909s5_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p1909s5) <- c("Total cabecera")
rownames(Total_cab_p1909s5) <- c("Si", "No", "Total p1909s5")
Total_cab_p1909s5

Total_rural_p1909s5 <- matrix(c(p1909s5_1_rural, p1909s5_2_rural, p1909s5_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p1909s5) <- c("Total rural")
rownames(Total_rural_p1909s5) <- c("Si", "No", "Total p1909s5")
Total_rural_p1909s5

Indicador_p1909s5 <- cbind(Total_nacional_p1909s5, Total_cab_p1909s5, Total_rural_p1909s5)
Indicador_p1909s5

# p1909s6
Total_nacional_p1909s6 <-matrix(c(p1909s6_1, p1909s6_2, p1909s6_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p1909s6) <- c("Total nacional")
rownames(Total_nacional_p1909s6) <- c("Si", "No", "Total p1909s6")
Total_nacional_p1909s6

Total_cab_p1909s6 <- matrix(c(p1909s6_1_cab, p1909s6_2_cab, p1909s6_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p1909s6) <- c("Total cabecera")
rownames(Total_cab_p1909s6) <- c("Si", "No", "Total p1909s6")
Total_cab_p1909s6

Total_rural_p1909s6 <- matrix(c(p1909s6_1_rural, p1909s6_2_rural, p1909s6_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p1909s6) <- c("Total rural")
rownames(Total_rural_p1909s6) <- c("Si", "No", "Total p1909s6")
Total_rural_p1909s6

Indicador_p1909s6 <- cbind(Total_nacional_p1909s6, Total_cab_p1909s6, Total_rural_p1909s6)
Indicador_p1909s6

# p6126
Total_nacional_p6126 <-matrix(c(p6126_1, p6126_2, p6126_3, p6126_4, p6126_5, p6126_total), ncol = 1, nrow = 6)
colnames(Total_nacional_p6126) <- c("Total nacional")
rownames(Total_nacional_p6126) <- c( "Una persona del hogar","Una persona de otro hogar no remunerada",
                                     "Una persona de otro hogar remunerada","Permanece solo/a","No requiere cuidado","Total p6126")
Total_nacional_p6126

Total_cab_p6126 <- matrix(c(p6126_1_cab, p6126_2_cab, p6126_3_cab, p6126_4_cab, p6126_5_cab, p6126_total_cab), ncol = 1, nrow = 6)
colnames(Total_cab_p6126) <- c("Total cabecera")
rownames(Total_cab_p6126) <- c("Una persona del hogar","Una persona de otro hogar no remunerada",
                               "Una persona de otro hogar remunerada","Permanece solo/a","No requiere cuidado","Total p6126")
Total_cab_p6126

Total_rural_p6126 <- matrix(c(p6126_1_rural, p6126_2_rural, p6126_3_rural, p6126_4_rural, p6126_5_rural, p6126_total_rural), ncol = 1, nrow = 6)
colnames(Total_rural_p6126) <- c("Total rural")
rownames(Total_rural_p6126) <- c("Una persona del hogar","Una persona de otro hogar no remunerada",
                                 "Una persona de otro hogar remunerada","Permanece solo/a","No requiere cuidado","Total p6126")
Total_rural_p6126

Indicador_p6126 <- cbind(Total_nacional_p6126, Total_cab_p6126, Total_rural_p6126)
Indicador_p6126

# p6126s2
Total_nacional_p6126s2 <-matrix(c(p6126s2_1, p6126s2_2, p6126s2_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p6126s2) <- c("Total nacional")
rownames(Total_nacional_p6126s2) <- c("Hombre", "Mujer", "Total p6126s2")
Total_nacional_p6126s2

Total_cab_p6126s2 <- matrix(c(p6126s2_1_cab, p6126s2_2_cab, p6126s2_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p6126s2) <- c("Total cabecera")
rownames(Total_cab_p6126s2) <- c("Hombre", "Mujer", "Total p6126s2")
Total_cab_p6126s2

Total_rural_p6126s2 <- matrix(c(p6126s2_1_rural, p6126s2_2_rural, p6126s2_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p6126s2) <- c("Total rural")
rownames(Total_rural_p6126s2) <- c("Hombre", "Mujer", "Total p6126s2")
Total_rural_p6126s2

Indicador_p6126s2 <- cbind(Total_nacional_p6126s2, Total_cab_p6126s2, Total_rural_p6126s2)
Indicador_p6126s2


# p6126s3
Total_nacional_p6126s3 <-matrix(c(p6126s3_1, p6126s3_2, p6126s3_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p6126s3) <- c("Total nacional")
rownames(Total_nacional_p6126s3) <- c("Si", "No", "Total p6126s3")
Total_nacional_p6126s3

Total_cab_p6126s3 <- matrix(c(p6126s3_1_cab, p6126s3_2_cab, p6126s3_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p6126s3) <- c("Total cabecera")
rownames(Total_cab_p6126s3) <- c("Si", "No", "Total p6126s3")
Total_cab_p6126s3

Total_rural_p6126s3 <- matrix(c(p6126s3_1_rural, p6126s3_2_rural, p6126s3_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p6126s3) <- c("Total rural")
rownames(Total_rural_p6126s3) <- c("Si", "No", "Total p6126s3")
Total_rural_p6126s3

Indicador_p6126s3 <- cbind(Total_nacional_p6126s3, Total_cab_p6126s3, Total_rural_p6126s3)
Indicador_p6126s3

# p5665
Total_nacional_p5665 <-matrix(c(p5665_1, p5665_2, p5665_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p5665) <- c("Total nacional")
rownames(Total_nacional_p5665) <- c("Si", "No", "Total p5665")
Total_nacional_p5665

Total_cab_p5665 <- matrix(c(p5665_1_cab, p5665_2_cab, p5665_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p5665) <- c("Total cabecera")
rownames(Total_cab_p5665) <- c("Si", "No", "Total p5665")
Total_cab_p5665

Total_rural_p5665 <- matrix(c(p5665_1_rural, p5665_2_rural, p5665_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p5665) <- c("Total rural")
rownames(Total_rural_p5665) <- c("Si", "No", "Total p5665")
Total_rural_p5665

Indicador_p5665 <- cbind(Total_nacional_p5665, Total_cab_p5665, Total_rural_p5665)
Indicador_p5665

# p8563
Total_nacional_p8563 <-matrix(c(p8563_1, p8563_2, p8563_3, p8563_4, p8563_5, p8563_6, p8563_7, p8563_8, p8563_total), ncol = 1, nrow = 9)
colnames(Total_nacional_p8563) <- c("Total nacional")
rownames(Total_nacional_p8563) <- c("Acudió a Ia entidad de seguridad social en salud de la cual es afiliado/a",
                                    "Acudió de forma particular  a un médico general, especialista, odontólogo, terapeuta o profesional de la salud",
                                    "Acudió a un boticario, farmaceuta, droguista", "Consultó a un empírico, curandero, yerbatero, comadrona",
                                    "Asistió a terapias alternativas (acupuntura, esencias florales, musicoterapias, homeópata etc.)",
                                   "Usó remedios caseros ", "Se autorecetó", "Nada", "Total p8563")
Total_nacional_p8563

Total_cab_p8563 <- matrix(c(p8563_1_cab, p8563_2_cab, p8563_3_cab, p8563_4_cab, p8563_5_cab, p8563_6_cab, p8563_7_cab, 
                            p8563_8_cab, p8563_total_cab), ncol = 1, nrow = 9)
colnames(Total_cab_p8563) <- c("Total cabecera")
rownames(Total_cab_p8563) <- c("Acudió a Ia entidad de seguridad social en salud de la cual es afiliado/a",
                               "Acudió de forma particular  a un médico general, especialista, odontólogo, terapeuta o profesional de la salud",
                               "Acudió a un boticario, farmaceuta, droguista", "Consultó a un empírico, curandero, yerbatero, comadrona",
                               "Asistió a terapias alternativas (acupuntura, esencias florales, musicoterapias, homeópata etc.)",
                               "Usó remedios caseros ", "Se autorecetó", "Nada", "Total p8563")
Total_cab_p8563

Total_rural_p8563 <- matrix(c(p8563_1_rural, p8563_2_rural, p8563_3_rural, p8563_4_rural, p8563_5_rural, p8563_6_rural, p8563_7_rural, 
                              p8563_8_rural, p8563_total_rural), ncol = 1, nrow = 9)
colnames(Total_rural_p8563) <- c("Total rural")
rownames(Total_rural_p8563) <- c("Acudió a Ia entidad de seguridad social en salud de la cual es afiliado/a",
                                 "Acudió de forma particular  a un médico general, especialista, odontólogo, terapeuta o profesional de la salud",
                                 "Acudió a un boticario, farmaceuta, droguista", "Consultó a un empírico, curandero, yerbatero, comadrona",
                                 "Asistió a terapias alternativas (acupuntura, esencias florales, musicoterapias, homeópata etc.)",
                                 "Usó remedios caseros ", "Se autorecetó", "Nada", "Total p8563")
Total_rural_p8563

Indicador_p8563 <- cbind(Total_nacional_p8563, Total_cab_p8563, Total_rural_p8563)
Indicador_p8563

# p1092
Total_nacional_p1092 <-matrix(c(p1092_1, p1092_2, p1092_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p1092) <- c("Total nacional")
rownames(Total_nacional_p1092) <- c("Si", "No", "Total p1092")
Total_nacional_p1092

Total_cab_p1092 <- matrix(c(p1092_1_cab, p1092_2_cab, p1092_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p1092) <- c("Total cabecera")
rownames(Total_cab_p1092) <- c("Si", "No", "Total p1092")
Total_cab_p1092

Total_rural_p1092 <- matrix(c(p1092_1_rural, p1092_2_rural, p1092_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p1092) <- c("Total rural")
rownames(Total_rural_p1092) <- c("Si", "No", "Total p1092")
Total_rural_p1092

Indicador_p1092 <- cbind(Total_nacional_p1092, Total_cab_p1092, Total_rural_p1092)
Indicador_p1092

# p8573
Total_nacional_p8573 <-matrix(c(p8573_1, p8573_2, p8573_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p8573) <- c("Total nacional")
rownames(Total_nacional_p8573) <- c("Si", "No", "Total p8573")
Total_nacional_p8573

Total_cab_p8573 <- matrix(c(p8573_1_cab, p8573_2_cab, p8573_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p8573) <- c("Total cabecera")
rownames(Total_cab_p8573) <- c("Si", "No", "Total p8573")
Total_cab_p8573

Total_rural_p8573 <- matrix(c(p8573_1_rural, p8573_2_rural, p8573_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p8573) <- c("Total rural")
rownames(Total_rural_p8573) <- c("Si", "No", "Total p8573")
Total_rural_p8573

Indicador_p8573 <- cbind(Total_nacional_p8573, Total_cab_p8573, Total_rural_p8573)
Indicador_p8573

# p8575
Total_nacional_p8575 <-matrix(c(p8575_1, p8575_2, p8575_3, p8575_4, p8575_5, p8575_6, p8575_9, p8575_total), ncol = 1, nrow = 8)
colnames(Total_nacional_p8575) <- c("Total nacional")
rownames(Total_nacional_p8575) <- c("El caso era leve",
                                    "Esperó demasiado tiempo y no lo/la atendieron",
                                    "Indicaron que allí no lo/la podían atender porque no estaba afiliado/a a alguna entidad que tuviera contrato con ellos",
                                    "No tenía identificación y por eso lo/la rechazaron",
                                    "Indicaron que debían remitirlo/la a otra institución prestadora de servicios que tuviera los servicios requeridos para atenderlo/la",
                                    "No le dieron información ", "No sabe/no responde.", "Total p8575")
Total_nacional_p8575

Total_cab_p8575 <- matrix(c(p8575_1_cab, p8575_2_cab, p8575_3_cab, p8575_4_cab, p8575_5_cab, p8575_6_cab, p8575_9_cab, p8575_total_cab), ncol = 1, nrow = 8)
colnames(Total_cab_p8575) <- c("Total cabecera")
rownames(Total_cab_p8575) <- c("El caso era leve",
                               "Esperó demasiado tiempo y no lo/la atendieron",
                               "Indicaron que allí no lo/la podían atender porque no estaba afiliado/a a alguna entidad que tuviera contrato con ellos",
                               "No tenía identificación y por eso lo/la rechazaron",
                               "Indicaron que debían remitirlo/la a otra institución prestadora de servicios que tuviera los servicios requeridos para atenderlo/la",
                               "No le dieron información ", "No sabe/no responde.", "Total p8575")
Total_cab_p8575

Total_rural_p8575 <- matrix(c(p8575_1_rural, p8575_2_rural, p8575_3_rural, p8575_4_rural, p8575_5_rural, p8575_6_rural, p8575_9_rural, 
                              p8575_total_rural), ncol = 1, nrow = 8)
colnames(Total_rural_p8575) <- c("Total rural")
rownames(Total_rural_p8575) <- c("El caso era leve",
                                 "Esperó demasiado tiempo y no lo/la atendieron",
                                 "Indicaron que allí no lo/la podían atender porque no estaba afiliado/a a alguna entidad que tuviera contrato con ellos",
                                 "No tenía identificación y por eso lo/la rechazaron",
                                 "Indicaron que debían remitirlo/la a otra institución prestadora de servicios que tuviera los servicios requeridos para atenderlo/la",
                                 "No le dieron información ", "No sabe/no responde.", "Total p8575")
Total_rural_p8575

Indicador_p8575 <- cbind(Total_nacional_p8575, Total_cab_p8575, Total_rural_p8575)
Indicador_p8575

# p8577
Total_nacional_p8577 <-matrix(c(p8577_1, p8577_2, p8577_3, p8577_4, p8577_5, p8577_total), ncol = 1, nrow = 6)
colnames(Total_nacional_p8577) <- c("Total nacional")
rownames(Total_nacional_p8577) <- c("Lo atendieron inmediatamente", "En máximo 30 minutos", "Entre 31 minutos y una hora",
                                    "Más de una hora hasta dos horas", "Más de horas", "Total p8577")
Total_nacional_p8577

Total_cab_p8577 <- matrix(c(p8577_1_cab, p8577_2_cab, p8577_3_cab, p8577_4_cab, p8577_5_cab, p8577_total_cab), ncol = 1, nrow = 6)
colnames(Total_cab_p8577) <- c("Total cabecera")
rownames(Total_cab_p8577) <- c("Lo atendieron inmediatamente", "En máximo 30 minutos", "Entre 31 minutos y una hora",
                               "Más de una hora hasta dos horas", "Más de horas", "Total p8577")
Total_cab_p8577

Total_rural_p8577 <- matrix(c(p8577_1_rural, p8577_2_rural, p8577_3_rural, p8577_4_rural, p8577_5_rural, p8577_total_rural), ncol = 1, nrow = 6)
colnames(Total_rural_p8577) <- c("Total rural")
rownames(Total_rural_p8577) <- c("Lo atendieron inmediatamente", "En máximo 30 minutos", "Entre 31 minutos y una hora",
                                 "Más de una hora hasta dos horas", "Más de horas", "Total p8577")
Total_rural_p8577

Indicador_p8577 <- cbind(Total_nacional_p8577, Total_cab_p8577, Total_rural_p8577)
Indicador_p8577

# p770
Total_nacional_p770 <-matrix(c(p770_1, p770_2, p770_3, p770_total), ncol = 1, nrow = 4)
colnames(Total_nacional_p770) <- c("Total nacional")
rownames(Total_nacional_p770) <- c("Médico/a general", "Odontólogo/a", "Especialista", "Total p770")
Total_nacional_p770

Total_cab_p770 <- matrix(c(p770_1_cab, p770_2_cab, p770_3_cab, p770_total_cab), ncol = 1, nrow = 4)
colnames(Total_cab_p770) <- c("Total cabecera")
rownames(Total_cab_p770) <- c("Médico/a general", "Odontólogo/a", "Especialista", "Total p770")
Total_cab_p770

Total_rural_p770 <- matrix(c(p770_1_rural, p770_2_rural, p770_3_rural, p770_total_rural), ncol = 1, nrow = 4)
colnames(Total_rural_p770) <- c("Total rural")
rownames(Total_rural_p770) <- c("Médico/a general", "Odontólogo/a", "Especialista", "Total p770")
Total_rural_p770

Indicador_p770 <- cbind(Total_nacional_p770, Total_cab_p770, Total_rural_p770)
Indicador_p770

# p6153
Total_nacional_p6153 <-matrix(c(p6153_1, p6153_2, p6153_3, p6153_4, p6153_5, p6153_6, p6153_7, p6153_8, p6153_9,
                                p6153_10, p6153_11, p6153_12, p6153_13, p6153_total), ncol = 1, nrow = 14)
colnames(Total_nacional_p6153) <- c("Total nacional")
rownames(Total_nacional_p6153) <- c("El caso era leve", "No tuvo tiempo", "El centro de atención queda lejos",
                                    "Falta de dinero", " Mal servicio o cita distanciada en el tiempo",
                                    "No lo/la atendieron", "No confía en los médicos", "Consulto antes y no le resolvieron el problema",
                                    "Muchos trámites para la cita", "No le cubrían o no le autorizaron la atención",
                                    "Le hacen esperar mucho para atenderlo/la", "Dificultad para viajar", "Otro", "Total p6153")
Total_nacional_p6153

Total_cab_p6153 <- matrix(c(p6153_1_cab, p6153_2_cab, p6153_3_cab, p6153_4_cab, p6153_5_cab, p6153_6_cab, p6153_7_cab, p6153_8_cab, p6153_9_cab,
                            p6153_10_cab, p6153_11_cab, p6153_12_cab, p6153_13_cab, p6153_total_cab), ncol = 1, nrow = 14)
colnames(Total_cab_p6153) <- c("Total cabecera")
rownames(Total_cab_p6153) <- c("El caso era leve", "No tuvo tiempo", "El centro de atención queda lejos",
                               "Falta de dinero", " Mal servicio o cita distanciada en el tiempo",
                               "No lo/la atendieron", "No confía en los médicos", "Consulto antes y no le resolvieron el problema",
                               "Muchos trámites para la cita", "No le cubrían o no le autorizaron la atención",
                               "Le hacen esperar mucho para atenderlo/la", "Dificultad para viajar", "Otro", "Total p6153")
Total_cab_p6153

Total_rural_p6153 <- matrix(c(p6153_1_rural, p6153_2_rural, p6153_3_rural, p6153_4_rural, p6153_5_rural, p6153_6_rural, p6153_7_rural, p6153_8_rural,
                              p6153_9_rural, p6153_10_rural, p6153_11_rural, p6153_12_rural, p6153_13_rural, 
                              p6153_total_rural), ncol = 1, nrow = 14)
colnames(Total_rural_p6153) <- c("Total rural")
rownames(Total_rural_p6153) <- c("El caso era leve", "No tuvo tiempo", "El centro de atención queda lejos",
                                 "Falta de dinero", " Mal servicio o cita distanciada en el tiempo",
                                 "No lo/la atendieron", "No confía en los médicos", "Consulto antes y no le resolvieron el problema",
                                 "Muchos trámites para la cita", "No le cubrían o no le autorizaron la atención",
                                 "Le hacen esperar mucho para atenderlo/la", "Dificultad para viajar", "Otro", "Total p6153")
Total_rural_p6153

Indicador_p6153 <- cbind(Total_nacional_p6153, Total_cab_p6153, Total_rural_p6153)
Indicador_p6153

# p6199
Total_nacional_p6199 <-matrix(c(p6199_1, p6199_2, p6199_3, p6199_total), ncol = 1, nrow = 4)
colnames(Total_nacional_p6199) <- c("Total nacional")
rownames(Total_nacional_p6199) <- c("Médico/a general", "Odontólogo/a", "Acudió directo al especialista", "Total p6199")
Total_nacional_p6199

Total_cab_p6199 <- matrix(c(p6199_1_cab, p6199_2_cab, p6199_3_cab, p6199_total_cab), ncol = 1, nrow = 4)
colnames(Total_cab_p6199) <- c("Total cabecera")
rownames(Total_cab_p6199) <- c("Médico/a general", "Odontólogo/a", "Acudió directo al especialista", "Total p6199")
Total_cab_p6199

Total_rural_p6199 <- matrix(c(p6199_1_rural, p6199_2_rural, p6199_3_rural, p6199_total_rural), ncol = 1, nrow = 4)
colnames(Total_rural_p6199) <- c("Total rural")
rownames(Total_rural_p6199) <- c("Médico/a general", "Odontólogo/a", "Acudió directo al especialista", "Total p6199")
Total_rural_p6199

Indicador_p6199 <- cbind(Total_nacional_p6199, Total_cab_p6199, Total_rural_p6199)
Indicador_p6199

# p6199s1 
Total_nacional_p6199s1 <-matrix(c(p6199s1_0, p6199s1_1, p6199s1_2, p6199s1_3, p6199s1_4, p6199s1_5, p6199s1_6, p6199s1_7,
                                  p6199s1_8,  p6199s1_9,  p6199s1_10,  p6199s1_12,  p6199s1_14,  p6199s1_15,
                                  p6199s1_20,  p6199s1_21,  p6199s1_23,  p6199s1_25,  p6199s1_30, p6199s1_total), ncol = 1, nrow = 20)
colnames(Total_nacional_p6199s1) <- c("Total nacional")
rownames(Total_nacional_p6199s1) <- c("0 días", "1 días", "2 días", "3 días", "4 días", "5 días", "6 días", "7 días",
                                      "8 días", "9 días", "10 días", "12 días", "14 días", "15 días",
                                      "20 días", "21 días", "23 días", "25 días", "30 días", "Total p6199s1")
Total_nacional_p6199s1

Total_cab_p6199s1 <- matrix(c(p6199s1_0_cab, p6199s1_1_cab, p6199s1_2_cab, p6199s1_3_cab, p6199s1_4_cab, p6199s1_5_cab, p6199s1_6_cab, p6199s1_7_cab,
                              p6199s1_8_cab,  p6199s1_9_cab,  p6199s1_10_cab,  p6199s1_12_cab,  p6199s1_14_cab,  p6199s1_15_cab,
                              p6199s1_20_cab,  p6199s1_21_cab,  p6199s1_23_cab,  p6199s1_25_cab,  p6199s1_30_cab, p6199s1_total_cab), ncol = 1, nrow = 20)
colnames(Total_cab_p6199s1) <- c("Total cabecera")
rownames(Total_cab_p6199s1) <- c("0 días", "1 días", "2 días", "3 días", "4 días", "5 días", "6 días", "7 días",
                                 "8 días", "9 días", "10 días", "12 días", "14 días", "15 días",
                                 "20 días", "21 días", "23 días", "25 días", "30 días", "Total p6199s1")
Total_cab_p6199s1

Total_rural_p6199s1 <- matrix(c(p6199s1_0_rural, p6199s1_1_rural, p6199s1_2_rural, p6199s1_3_rural, p6199s1_4_rural, p6199s1_5_rural, p6199s1_6_rural,
                                p6199s1_7_rural, p6199s1_8_rural,  p6199s1_9_rural,  p6199s1_10_rural,  p6199s1_12_rural,  p6199s1_14_rural,  p6199s1_15_rural,
                                p6199s1_20_rural,  p6199s1_21_rural,  p6199s1_23_rural,  p6199s1_25_rural,  p6199s1_30_rural, p6199s1_total_rural), ncol = 1, nrow = 20)
colnames(Total_rural_p6199s1) <- c("Total rural")
rownames(Total_rural_p6199s1) <- c( "0 días", "1 días", "2 días", "3 días", "4 días", "5 días", "6 días", "7 días",
                                    "8 días", "9 días", "10 días", "12 días", "14 días", "15 días",
                                    "20 días", "21 días", "23 días", "25 días", "30 días", "Total p6199s1")
Total_rural_p6199s1

Indicador_p6199s1 <- cbind(Total_nacional_p6199s1, Total_cab_p6199s1, Total_rural_p6199s1)
Indicador_p6199s1

# p6145
Total_nacional_p6145 <-matrix(c(p6145_1, p6145_2, p6145_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p6145) <- c("Total nacional")
rownames(Total_nacional_p6145) <- c("Sí", "No", "Total p6145")
Total_nacional_p6145

Total_cab_p6145 <- matrix(c(p6145_1_cab, p6145_2_cab, p6145_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p6145) <- c("Total cabecera")
rownames(Total_cab_p6145) <- c("Sí", "No", "Total p6145")
Total_cab_p6145

Total_rural_p6145 <- matrix(c(p6145_1_rural, p6145_2_rural, p6145_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p6145) <- c("Total rural")
rownames(Total_rural_p6145) <- c("Sí", "No", "Total p6145")
Total_rural_p6145

Indicador_p6145 <- cbind(Total_nacional_p6145, Total_cab_p6145, Total_rural_p6145)
Indicador_p6145

# p8554
Total_nacional_p8554 <-matrix(c(p8554_1, p8554_2, p8554_3, p8554_4, p8554_total), ncol = 1, nrow = 5)
colnames(Total_nacional_p8554) <- c("Total nacional")
rownames(Total_nacional_p8554) <- c("Muy buena ", "Buena", "Mala", "Muy mala", "Total p8554")
Total_nacional_p8554

Total_cab_p8554 <- matrix(c(p8554_1_cab, p8554_2_cab, p8554_3_cab, p8554_4_cab, p8554_total_cab), ncol = 1, nrow = 5)
colnames(Total_cab_p8554) <- c("Total cabecera")
rownames(Total_cab_p8554) <- c("Muy buena ", "Buena", "Mala", "Muy mala", "Total p8554")
Total_cab_p8554

Total_rural_p8554 <- matrix(c(p8554_1_rural, p8554_2_rural, p8554_3_rural, p8554_4_rural, p8554_total_rural), ncol = 1, nrow = 5)
colnames(Total_rural_p8554) <- c("Total rural")
rownames(Total_rural_p8554) <- c("Muy buena ", "Buena", "Mala", "Muy mala", "Total p8554")
Total_rural_p8554

Indicador_p8554 <- cbind(Total_nacional_p8554, Total_cab_p8554, Total_rural_p8554)
Indicador_p8554

# p801
Total_nacional_p801 <-matrix(c(p801_1, p801_2, p801_3, p801_4, p801_5, p801_6, p801_7, p801_8, p801_total), ncol = 1, nrow = 9)
colnames(Total_nacional_p801) <- c("Total nacional")
rownames(Total_nacional_p801) <- c("Trámites excesivos o dispendiosos",
                                   "Mala atención del personal administrativo o asistencial (médicos, enfermeras, etc.)",
                                   "Falta de capacidad, conocimientos o habilidad del personal asistencial", 
                                   "Condiciones deficientes de infraestructura, dotación, mobiliario o accesibilidad para población en condición de discapacidad",
                                   "Demora en la asignación de citas",
                                   "Demora en la atención por parte del personal médico", "Problemas relacionados con los medicamentos",
                                   "Otro", "Total p801")
Total_nacional_p801

Total_cab_p801 <- matrix(c(p801_1_cab, p801_2_cab, p801_3_cab, p801_4_cab, p801_5_cab, p801_6_cab, p801_7_cab, 
                           p801_8_cab, p801_total_cab), ncol = 1, nrow = 9)
colnames(Total_cab_p801) <- c("Total cabecera")
rownames(Total_cab_p801) <- c("Trámites excesivos o dispendiosos",
                              "Mala atención del personal administrativo o asistencial (médicos, enfermeras, etc.)",
                              "Falta de capacidad, conocimientos o habilidad del personal asistencial", 
                              "Condiciones deficientes de infraestructura, dotación, mobiliario o accesibilidad para población en condición de discapacidad",
                              "Demora en la asignación de citas",
                              "Demora en la atención por parte del personal médico", "Problemas relacionados con los medicamentos",
                              "Otro", "Total p801")
Total_cab_p801

Total_rural_p801 <- matrix(c(p801_1_rural, p801_2_rural, p801_3_rural, p801_4_rural, p801_5_rural, p801_6_rural, p801_7_rural, 
                             p801_8_rural, p801_total_rural), ncol = 1, nrow = 9)
colnames(Total_rural_p801) <- c("Total rural")
rownames(Total_rural_p801) <- c("Trámites excesivos o dispendiosos",
                                "Mala atención del personal administrativo o asistencial (médicos, enfermeras, etc.)",
                                "Falta de capacidad, conocimientos o habilidad del personal asistencial", 
                                "Condiciones deficientes de infraestructura, dotación, mobiliario o accesibilidad para población en condición de discapacidad",
                                "Demora en la asignación de citas",
                                "Demora en la atención por parte del personal médico", "Problemas relacionados con los medicamentos",
                                "Otro", "Total p801")
Total_rural_p801

Indicador_p801 <- cbind(Total_nacional_p801, Total_cab_p801, Total_rural_p801)
Indicador_p801

# p8556 - p8556s1 - p8556s2 - p8556s4 - p8556s5 - p8556s6 - p8556s9 - p8556s10 
Total_nacional_p8556s <-matrix(c(p8556s1_1, p8556s2_2, p8556s4_3, p8556s5_4, p8556s6_5, p8556s9_6, p8556s10_7, p8556_total), ncol = 1, nrow = 8)
colnames(Total_nacional_p8556s) <- c("Total nacional")
rownames(Total_nacional_p8556s) <- c("EPS o entidad de seguridad social en salud  en la cual está afiliado/a",
                                     "Plan o seguro voluntario (Seguro médico, plan complementario o medicina prepagada)",
                                     "Seguro obligatorio de accidentes de tránsito (SOAT)", "Secretaria de salud o la alcaldía",
                                     "Recursos propios o familiares",
                                     "Recursos de otras personas ", "No se requirió pago", "Total p8556s")
Total_nacional_p8556s

Total_cab_p8556s <- matrix(c(p8556s1_1_cab, p8556s2_2_cab, p8556s4_3_cab, p8556s5_4_cab, p8556s6_5_cab, p8556s9_6_cab, p8556s10_7_cab,
                             p8556_total_cab), ncol = 1, nrow = 8)
colnames(Total_cab_p8556s) <- c("Total cabecera")
rownames(Total_cab_p8556s) <- c("EPS o entidad de seguridad social en salud  en la cual está afiliado/a",
                                "Plan o seguro voluntario (Seguro médico, plan complementario o medicina prepagada)",
                                "Seguro obligatorio de accidentes de tránsito (SOAT)", "Secretaria de salud o la alcaldía",
                                "Recursos propios o familiares",
                                "Recursos de otras personas ", "No se requirió pago", "Total p8556s")
Total_cab_p8556s

Total_rural_p8556s <- matrix(c(p8556s1_1_rural, p8556s2_2_rural, p8556s4_3_rural, p8556s5_4_rural, p8556s6_5_rural,
                               p8556s9_6_rural, p8556s10_7_rural, p8556_total_rural), ncol = 1, nrow = 8)
colnames(Total_rural_p8556s) <- c("Total rural")
rownames(Total_rural_p8556s) <- c("EPS o entidad de seguridad social en salud  en la cual está afiliado/a",
                                  "Plan o seguro voluntario (Seguro médico, plan complementario o medicina prepagada)",
                                  "Seguro obligatorio de accidentes de tránsito (SOAT)", "Secretaria de salud o la alcaldía",
                                  "Recursos propios o familiares",
                                  "Recursos de otras personas ", "No se requirió pago", "Total p8556s")
Total_rural_p8556s

Indicador_p8556s <- cbind(Total_nacional_p8556s, Total_cab_p8556s, Total_rural_p8556s)
Indicador_p8556s

# p6147
Total_nacional_p6147 <-matrix(c(p6147_1, p6147_2, p6147_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p6147) <- c("Total nacional")
rownames(Total_nacional_p6147) <- c("Sí", "No", "Total p6147")
Total_nacional_p6147

Total_cab_p6147 <- matrix(c(p6147_1_cab, p6147_2_cab, p6147_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p6147) <- c("Total cabecera")
rownames(Total_cab_p6147) <- c("Sí", "No", "Total p6147")
Total_cab_p6147

Total_rural_p6147 <- matrix(c(p6147_1_rural, p6147_2_rural, p6147_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p6147) <- c("Total rural")
rownames(Total_rural_p6147) <- c("Sí", "No", "Total p6147")
Total_rural_p6147

Indicador_p6147 <- cbind(Total_nacional_p6147, Total_cab_p6147, Total_rural_p6147)
Indicador_p6147

# p6148
Total_nacional_p6148 <-matrix(c(p6148_1, p6148_2, p6148_3, p6148_total), ncol = 1, nrow = 4)
colnames(Total_nacional_p6148) <- c("Total nacional")
rownames(Total_nacional_p6148) <- c("Sí, todos", "Sí, algunos", "No", "Total p6148")
Total_nacional_p6148

Total_cab_p6148 <- matrix(c(p6148_1_cab, p6148_2_cab, p6148_3_cab, p6148_total_cab), ncol = 1, nrow = 4)
colnames(Total_cab_p6148) <- c("Total cabecera")
rownames(Total_cab_p6148) <- c("Sí, todos", "Sí, algunos", "No", "Total p6148")
Total_cab_p6148

Total_rural_p6148 <- matrix(c(p6148_1_rural, p6148_2_rural, p6148_3_rural, p6148_total_rural), ncol = 1, nrow = 4)
colnames(Total_rural_p6148) <- c("Total rural")
rownames(Total_rural_p6148) <- c("Sí, todos", "Sí, algunos", "No", "Total p6148")
Total_rural_p6148

Indicador_p6148 <- cbind(Total_nacional_p6148, Total_cab_p6148, Total_rural_p6148)
Indicador_p6148

# p6149
Total_nacional_p6149 <-matrix(c(p6149_1, p6149_2, p6149_3, p6149_4, p6149_5, p6149_6, p6149_7, p6149_8, p6149_total), ncol = 1, nrow = 9)
colnames(Total_nacional_p6149) <- c("Total nacional")
rownames(Total_nacional_p6149) <- c("No están incluidos en el plan de beneficios en salud o POS o no le autorizaron",
                                    "No había los medicamentos recetados",
                                    "No había la cantidad requerida",
                                    "Por errores o deficiencias en la expedición de la fórmula medica",
                                    "No hizo las gestiones para reclamarlos", "No tenía dinero", "Acudió a médico particular ", "Otra", "Total p6149")
Total_nacional_p6149

Total_cab_p6149 <- matrix(c(p6149_1_cab, p6149_2_cab, p6149_3_cab, p6149_4_cab, p6149_5_cab, p6149_6_cab, p6149_7_cab, 
                            p6149_8_cab, p6149_total_cab), ncol = 1, nrow = 9)
colnames(Total_cab_p6149) <- c("Total cabecera")
rownames(Total_cab_p6149) <- c("No están incluidos en el plan de beneficios en salud o POS o no le autorizaron",
                               "No había los medicamentos recetados",
                               "No había la cantidad requerida",
                               "Por errores o deficiencias en la expedición de la fórmula medica",
                               "No hizo las gestiones para reclamarlos", "No tenía dinero", "Acudió a médico particular ", "Otra", "Total p6149")
Total_cab_p6149

Total_rural_p6149 <- matrix(c(p6149_1_rural, p6149_2_rural, p6149_3_rural, p6149_4_rural, p6149_5_rural, p6149_6_rural, p6149_7_rural, 
                              p6149_8_rural, p6149_total_rural), ncol = 1, nrow = 9)
colnames(Total_rural_p6149) <- c("Total rural")
rownames(Total_rural_p6149) <- c("No están incluidos en el plan de beneficios en salud o POS o no le autorizaron",
                                 "No había los medicamentos recetados",
                                 "No había la cantidad requerida",
                                 "Por errores o deficiencias en la expedición de la fórmula medica",
                                 "No hizo las gestiones para reclamarlos", "No tenía dinero", "Acudió a médico particular ", "Otra", "Total p6149")
Total_rural_p6149

Indicador_p6149 <- cbind(Total_nacional_p6149, Total_cab_p6149, Total_rural_p6149)
Indicador_p6149

# p3178
Total_nacional_p3178 <-matrix(c(p3178_1, p3178_2, p3178_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p3178) <- c("Total nacional")
rownames(Total_nacional_p3178) <- c("Sí", "No", "Total p3178")
Total_nacional_p3178

Total_cab_p3178 <- matrix(c(p3178_1_cab, p3178_2_cab, p3178_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p3178) <- c("Total cabecera")
rownames(Total_cab_p3178) <- c("Sí", "No", "Total p3178")
Total_cab_p3178

Total_rural_p3178 <- matrix(c(p3178_1_rural, p3178_2_rural, p3178_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p3178) <- c("Total rural")
rownames(Total_rural_p3178) <- c("Sí", "No", "Total p3178")
Total_rural_p3178

Indicador_p3178 <- cbind(Total_nacional_p3178, Total_cab_p3178, Total_rural_p3178)
Indicador_p3178

# p3178s1
Total_nacional_p3178s1 <-matrix(c(p3178s1), ncol = 1, nrow = 1)
colnames(Total_nacional_p3178s1) <- c("Total nacional")
rownames(Total_nacional_p3178s1) <- c("A través de EPS")
Total_nacional_p3178s1

Total_cab_p3178s1 <- matrix(c(p3178s1_cab), ncol = 1, nrow = 1)
colnames(Total_cab_p3178s1) <- c("Total cabecera")
rownames(Total_cab_p3178s1) <- c("A través de EPS")
Total_cab_p3178s1

Total_rural_p3178s1 <- matrix(c(p3178s1_rural), ncol = 1, nrow = 1)
colnames(Total_rural_p3178s1) <- c("Total rural")
rownames(Total_rural_p3178s1) <- c("A través de EPS")
Total_rural_p3178s1

Indicador_p3178s1 <- cbind(Total_nacional_p3178s1, Total_cab_p3178s1, Total_rural_p3178s1)
Indicador_p3178s1

# p3178s2
Total_nacional_p3178s2 <-matrix(c(p3178s2), ncol = 1, nrow = 1)
colnames(Total_nacional_p3178s2) <- c("Total nacional")
rownames(Total_nacional_p3178s2) <- c("Médico particular")
Total_nacional_p3178s2

Total_cab_p3178s2 <- matrix(c(p3178s2_cab), ncol = 1, nrow = 1)
colnames(Total_cab_p3178s2) <- c("Total cabecera")
rownames(Total_cab_p3178s2) <- c("Médico particular")
Total_cab_p3178s2

Total_rural_p3178s2 <- matrix(c(p3178s2_rural), ncol = 1, nrow = 1)
colnames(Total_rural_p3178s2) <- c("Total rural")
rownames(Total_rural_p3178s2) <- c("Médico particular")
Total_rural_p3178s2

Indicador_p3178s2 <- cbind(Total_nacional_p3178s2, Total_cab_p3178s2, Total_rural_p3178s2)
Indicador_p3178s2

# p3178s3
Total_nacional_p3178s3 <-matrix(c(p3178s3), ncol = 1, nrow = 1)
colnames(Total_nacional_p3178s3) <- c("Total nacional")
rownames(Total_nacional_p3178s3) <- c("Plan voluntario (seguro médico, plan complementario o medicina prepagada)")
Total_nacional_p3178s3

Total_cab_p3178s3 <- matrix(c(p3178s3_cab), ncol = 1, nrow = 1)
colnames(Total_cab_p3178s3) <- c("Total cabecera")
rownames(Total_cab_p3178s3) <- c("Plan voluntario (seguro médico, plan complementario o medicina prepagada)")
Total_cab_p3178s3

Total_rural_p3178s3 <- matrix(c(p3178s3_rural), ncol = 1, nrow = 1)
colnames(Total_rural_p3178s3) <- c("Total rural")
rownames(Total_rural_p3178s3) <- c("Plan voluntario (seguro médico, plan complementario o medicina prepagada)")
Total_rural_p3178s3

Indicador_p3178s3 <- cbind(Total_nacional_p3178s3, Total_cab_p3178s3, Total_rural_p3178s3)
Indicador_p3178s3

# p3179 
Total_nacional_p3179 <-matrix(c(p3179_1, p3179_2, p3179_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p3179) <- c("Total nacional")
rownames(Total_nacional_p3179) <- c("Sí", "No", "Total p3179")
Total_nacional_p3179

Total_cab_p3179 <- matrix(c(p3179_1_cab, p3179_2_cab, p3179_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p3179) <- c("Total cabecera")
rownames(Total_cab_p3179) <- c("Sí", "No", "Total p3179")
Total_cab_p3179

Total_rural_p3179 <- matrix(c(p3179_1_rural, p3179_2_rural, p3179_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p3179) <- c("Total rural")
rownames(Total_rural_p3179) <- c("Sí", "No", "Total p3179")
Total_rural_p3179

Indicador_p3179 <- cbind(Total_nacional_p3179, Total_cab_p3179, Total_rural_p3179)
Indicador_p3179

# p3179s1
Total_nacional_p3179s1 <-matrix(c(p3179s1), ncol = 1, nrow = 1)
colnames(Total_nacional_p3179s1) <- c("Total nacional")
rownames(Total_nacional_p3179s1) <- c("A través de EPS")
Total_nacional_p3179s1

Total_cab_p3179s1 <- matrix(c(p3179s1_cab), ncol = 1, nrow = 1)
colnames(Total_cab_p3179s1) <- c("Total cabecera")
rownames(Total_cab_p3179s1) <- c("A través de EPS")
Total_cab_p3179s1

Total_rural_p3179s1 <- matrix(c(p3179s1_rural), ncol = 1, nrow = 1)
colnames(Total_rural_p3179s1) <- c("Total rural")
rownames(Total_rural_p3179s1) <- c("A través de EPS")
Total_rural_p3179s1

Indicador_p3179s1 <- cbind(Total_nacional_p3179s1, Total_cab_p3179s1, Total_rural_p3179s1)
Indicador_p3179s1

# p3179s2
Total_nacional_p3179s2 <-matrix(c(p3179s2), ncol = 1, nrow = 1)
colnames(Total_nacional_p3179s2) <- c("Total nacional")
rownames(Total_nacional_p3179s2) <- c("Odontólogo particular")
Total_nacional_p3179s2

Total_cab_p3179s2 <- matrix(c(p3179s2_cab), ncol = 1, nrow = 1)
colnames(Total_cab_p3179s2) <- c("Total cabecera")
rownames(Total_cab_p3179s2) <- c("Odontólogo particular")
Total_cab_p3179s2

Total_rural_p3179s2 <- matrix(c(p3179s2_rural), ncol = 1, nrow = 1)
colnames(Total_rural_p3179s2) <- c("Total rural")
rownames(Total_rural_p3179s2) <- c("Odontólogo particular")
Total_rural_p3179s2

Indicador_p3179s2 <- cbind(Total_nacional_p3179s2, Total_cab_p3179s2, Total_rural_p3179s2)
Indicador_p3179s2

# p3179s3
Total_nacional_p3179s3 <-matrix(c(p3179s3), ncol = 1, nrow = 1)
colnames(Total_nacional_p3179s3) <- c("Total nacional")
rownames(Total_nacional_p3179s3) <- c("Plan voluntario (seguro médico, plan complementario o medicina prepagada)")
Total_nacional_p3179s3

Total_cab_p3179s3 <- matrix(c(p3179s3_cab), ncol = 1, nrow = 1)
colnames(Total_cab_p3179s3) <- c("Total cabecera")
rownames(Total_cab_p3179s3) <- c("Plan voluntario (seguro médico, plan complementario o medicina prepagada)")
Total_cab_p3179s3

Total_rural_p3179s3 <- matrix(c(p3179s3_rural), ncol = 1, nrow = 1)
colnames(Total_rural_p3179s3) <- c("Total rural")
rownames(Total_rural_p3179s3) <- c("Plan voluntario (seguro médico, plan complementario o medicina prepagada)")
Total_rural_p3179s3

Indicador_p3179s3 <- cbind(Total_nacional_p3179s3, Total_cab_p3179s3, Total_rural_p3179s3)
Indicador_p3179s3

# p3181
Total_nacional_p3181 <-matrix(c(p3181_1, p3181_2, p3181_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p3181) <- c("Total nacional")
rownames(Total_nacional_p3181) <- c("Sí", "No", "Total p3181")
Total_nacional_p3181

Total_cab_p3181 <- matrix(c(p3181_1_cab, p3181_2_cab, p3181_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p3181) <- c("Total cabecera")
rownames(Total_cab_p3181) <- c("Sí", "No", "Total p3181")
Total_cab_p3181

Total_rural_p3181 <- matrix(c(p3181_1_rural, p3181_2_rural, p3181_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p3181) <- c("Total rural")
rownames(Total_rural_p3181) <- c("Sí", "No", "Total p3181")
Total_rural_p3181

Indicador_p3181 <- cbind(Total_nacional_p3181, Total_cab_p3181, Total_rural_p3181)
Indicador_p3181

# p3182
Total_nacional_p3182 <-matrix(c(p3182_1, p3182_2, p3182_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p3182) <- c("Total nacional")
rownames(Total_nacional_p3182) <- c("Sí", "No", "Total p3182")
Total_nacional_p3182

Total_cab_p3182 <- matrix(c(p3182_1_cab, p3182_2_cab, p3182_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p3182) <- c("Total cabecera")
rownames(Total_cab_p3182) <- c("Sí", "No", "Total p3182")
Total_cab_p3182

Total_rural_p3182 <- matrix(c(p3182_1_rural, p3182_2_rural, p3182_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p3182) <- c("Total rural")
rownames(Total_rural_p3182) <- c("Sí", "No", "Total p3182")
Total_rural_p3182

Indicador_p3182 <- cbind(Total_nacional_p3182, Total_cab_p3182, Total_rural_p3182)
Indicador_p3182

# p3183
Total_nacional_p3183 <-matrix(c(p3183_1, p3183_2, p3183_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p3183) <- c("Total nacional")
rownames(Total_nacional_p3183) <- c("Sí", "No", "Total p3183")
Total_nacional_p3183

Total_cab_p3183 <- matrix(c(p3183_1_cab, p3183_2_cab, p3183_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p3183) <- c("Total cabecera")
rownames(Total_cab_p3183) <- c("Sí", "No", "Total p3183")
Total_cab_p3183

Total_rural_p3183 <- matrix(c(p3183_1_rural, p3183_2_rural, p3183_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p3183) <- c("Total rural")
rownames(Total_rural_p3183) <- c("Sí", "No", "Total p3183")
Total_rural_p3183

Indicador_p3183 <- cbind(Total_nacional_p3183, Total_cab_p3183, Total_rural_p3183)
Indicador_p3183

# p3184
Total_nacional_p3184 <-matrix(c(p3184_1, p3184_2, p3184_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p3184) <- c("Total nacional")
rownames(Total_nacional_p3184) <- c("Sí", "No", "Total p3184")
Total_nacional_p3184

Total_cab_p3184 <- matrix(c(p3184_1_cab, p3184_2_cab, p3184_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p3184) <- c("Total cabecera")
rownames(Total_cab_p3184) <- c("Sí", "No", "Total p3184")
Total_cab_p3184

Total_rural_p3184 <- matrix(c(p3184_1_rural, p3184_2_rural, p3184_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p3184) <- c("Total rural")
rownames(Total_rural_p3184) <- c("Sí", "No", "Total p3184")
Total_rural_p3184

Indicador_p3184 <- cbind(Total_nacional_p3184, Total_cab_p3184, Total_rural_p3184)
Indicador_p3184

# p3185
Total_nacional_p3185 <-matrix(c(p3185_1, p3185_2, p3185_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p3185) <- c("Total nacional")
rownames(Total_nacional_p3185) <- c("Sí", "No", "Total p3185")
Total_nacional_p3185

Total_cab_p3185 <- matrix(c(p3185_1_cab, p3185_2_cab, p3185_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p3185) <- c("Total cabecera")
rownames(Total_cab_p3185) <- c("Sí", "No", "Total p3185")
Total_cab_p3185

Total_rural_p3185 <- matrix(c(p3185_1_rural, p3185_2_rural, p3185_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p3185) <- c("Total rural")
rownames(Total_rural_p3185) <- c("Sí", "No", "Total p3185")
Total_rural_p3185

Indicador_p3185 <- cbind(Total_nacional_p3185, Total_cab_p3185, Total_rural_p3185)
Indicador_p3185

# p3186
Total_nacional_p3186 <-matrix(c(p3186_1, p3186_2, p3186_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p3186) <- c("Total nacional")
rownames(Total_nacional_p3186) <- c("Sí", "No", "Total p3186")
Total_nacional_p3186

Total_cab_p3186 <- matrix(c(p3186_1_cab, p3186_2_cab, p3186_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p3186) <- c("Total cabecera")
rownames(Total_cab_p3186) <- c("Sí", "No", "Total p3186")
Total_cab_p3186

Total_rural_p3186 <- matrix(c(p3186_1_rural, p3186_2_rural, p3186_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p3186) <- c("Total rural")
rownames(Total_rural_p3186) <- c("Sí", "No", "Total p3186")
Total_rural_p3186

Indicador_p3186 <- cbind(Total_nacional_p3186, Total_cab_p3186, Total_rural_p3186)
Indicador_p3186

# p3187
Total_nacional_p3187s1 <-matrix(c(p3187s1_1, p3187s1_2, p3187s1_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p3187s1) <- c("Total nacional")
rownames(Total_nacional_p3187s1) <- c("Sí", "No", "Total p3187s1")
Total_nacional_p3187s1

Total_cab_p3187s1 <- matrix(c(p3187s1_1_cab, p3187s1_2_cab, p3187s1_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p3187s1) <- c("Total cabecera")
rownames(Total_cab_p3187s1) <- c("Sí", "No", "Total p3187s1")
Total_cab_p3187s1

Total_rural_p3187s1 <- matrix(c(p3187s1_1_rural, p3187s1_2_rural, p3187s1_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p3187s1) <- c("Total rural")
rownames(Total_rural_p3187s1) <- c("Sí", "No", "Total p3187s1")
Total_rural_p3187s1

Indicador_p3187s1 <- cbind(Total_nacional_p3187s1, Total_cab_p3187s1, Total_rural_p3187s1)
Indicador_p3187s1

# p3188
Total_nacional_p3188 <-matrix(c(p3188_1, p3188_2, p3188_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p3188) <- c("Total nacional")
rownames(Total_nacional_p3188) <- c("Sí", "No", "Total p3188")
Total_nacional_p3188

Total_cab_p3188 <- matrix(c(p3188_1_cab, p3188_2_cab, p3188_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p3188) <- c("Total cabecera")
rownames(Total_cab_p3188) <- c("Sí", "No", "Total p3188")
Total_cab_p3188

Total_rural_p3188 <- matrix(c(p3188_1_rural, p3188_2_rural, p3188_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p3188) <- c("Total rural")
rownames(Total_rural_p3188) <- c("Sí", "No", "Total p3188")
Total_rural_p3188

Indicador_p3188 <- cbind(Total_nacional_p3188, Total_cab_p3188, Total_rural_p3188)
Indicador_p3188

# p3188s1
Total_nacional_p3188s1 <-matrix(c(p3188s1), ncol = 1, nrow = 1)
colnames(Total_nacional_p3188s1) <- c("Total nacional")
rownames(Total_nacional_p3188s1) <- c("A través de EPS")
Total_nacional_p3188s1

Total_cab_p3188s1 <- matrix(c(p3188s1_cab), ncol = 1, nrow = 1)
colnames(Total_cab_p3188s1) <- c("Total cabecera")
rownames(Total_cab_p3188s1) <- c("A través de EPS")
Total_cab_p3188s1

Total_rural_p3188s1 <- matrix(c(p3188s1_rural), ncol = 1, nrow = 1)
colnames(Total_rural_p3188s1) <- c("Total rural")
rownames(Total_rural_p3188s1) <- c("A través de EPS")
Total_rural_p3188s1

Indicador_p3188s1 <- cbind(Total_nacional_p3188s1, Total_cab_p3188s1, Total_rural_p3188s1)
Indicador_p3188s1

# p3188s2
Total_nacional_p3188s2 <-matrix(c(p3188s2), ncol = 1, nrow = 1)
colnames(Total_nacional_p3188s2) <- c("Total nacional")
rownames(Total_nacional_p3188s2) <- c("Médico particular")
Total_nacional_p3188s2

Total_cab_p3188s2 <- matrix(c(p3188s2_cab), ncol = 1, nrow = 1)
colnames(Total_cab_p3188s2) <- c("Total cabecera")
rownames(Total_cab_p3188s2) <- c("Médico particular")
Total_cab_p3188s2

Total_rural_p3188s2 <- matrix(c(p3188s2_rural), ncol = 1, nrow = 1)
colnames(Total_rural_p3188s2) <- c("Total rural")
rownames(Total_rural_p3188s2) <- c("Médico particular")
Total_rural_p3188s2

Indicador_p3188s2 <- cbind(Total_nacional_p3188s2, Total_cab_p3188s2, Total_rural_p3188s2)
Indicador_p3188s2

# p3188s3
Total_nacional_p3188s3 <-matrix(c(p3188s3), ncol = 1, nrow = 1)
colnames(Total_nacional_p3188s3) <- c("Total nacional")
rownames(Total_nacional_p3188s3) <- c("Plan voluntario (seguro médico, plan complementario o medicina prepagada) ")
Total_nacional_p3188s3

Total_cab_p3188s3 <- matrix(c(p3188s3_cab), ncol = 1, nrow = 1)
colnames(Total_cab_p3188s3) <- c("Total cabecera")
rownames(Total_cab_p3188s3) <- c("Plan voluntario (seguro médico, plan complementario o medicina prepagada) ")
Total_cab_p3188s3

Total_rural_p3188s3 <- matrix(c(p3188s3_rural), ncol = 1, nrow = 1)
colnames(Total_rural_p3188s3) <- c("Total rural")
rownames(Total_rural_p3188s3) <- c("Plan voluntario (seguro médico, plan complementario o medicina prepagada) ")
Total_rural_p3188s3

Indicador_p3188s3 <- cbind(Total_nacional_p3188s3, Total_cab_p3188s3, Total_rural_p3188s3)
Indicador_p3188s3

# p3008s1
Total_nacional_p3008s1 <-matrix(c(p3008s1_1, p3008s1_2, p3008s1_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p3008s1) <- c("Total nacional")
rownames(Total_nacional_p3008s1) <- c("Sí", "No", "Total p3008s1")
Total_nacional_p3008s1

Total_cab_p3008s1 <- matrix(c(p3008s1_1_cab, p3008s1_2_cab, p3008s1_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p3008s1) <- c("Total cabecera")
rownames(Total_cab_p3008s1) <- c("Sí", "No", "Total p3008s1")
Total_cab_p3008s1

Total_rural_p3008s1 <- matrix(c(p3008s1_1_rural, p3008s1_2_rural, p3008s1_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p3008s1) <- c("Total rural")
rownames(Total_rural_p3008s1) <- c("Sí", "No", "Total p3008s1")
Total_rural_p3008s1

Indicador_p3008s1 <- cbind(Total_nacional_p3008s1, Total_cab_p3008s1, Total_rural_p3008s1)
Indicador_p3008s1

# p3008s1a1
Total_nacional_p3008s1a1 <-matrix(c(p3008s1a1_1, p3008s1a1_2, p3008s1a1_3, p3008s1a1_total), ncol = 1, nrow = 4)
colnames(Total_nacional_p3008s1a1) <- c("Total nacional")
rownames(Total_nacional_p3008s1a1) <- c("Diariamente", "Algunos días de la semana", "Menos de una vez por semana", "Total p3008s1a1")
Total_nacional_p3008s1a1

Total_cab_p3008s1a1 <- matrix(c(p3008s1a1_1_cab, p3008s1a1_2_cab, p3008s1a1_3_cab, p3008s1a1_total_cab), ncol = 1, nrow = 4)
colnames(Total_cab_p3008s1a1) <- c("Total cabecera")
rownames(Total_cab_p3008s1a1) <- c("Diariamente", "Algunos días de la semana", "Menos de una vez por semana", "Total p3008s1a1")
Total_cab_p3008s1a1

Total_rural_p3008s1a1 <- matrix(c(p3008s1a1_1_rural, p3008s1a1_2_rural, p3008s1a1_3_rural, p3008s1a1_total_rural), ncol = 1, nrow = 4)
colnames(Total_rural_p3008s1a1) <- c("Total rural")
rownames(Total_rural_p3008s1a1) <- c("Diariamente", "Algunos días de la semana", "Menos de una vez por semana", "Total p3008s1a1")
Total_rural_p3008s1a1

Indicador_p3008s1a1 <- cbind(Total_nacional_p3008s1a1, Total_cab_p3008s1a1, Total_rural_p3008s1a1)
Indicador_p3008s1a1

# p3008s2
Total_nacional_p3008s2 <-matrix(c(p3008s2_1, p3008s2_2, p3008s2_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p3008s2) <- c("Total nacional")
rownames(Total_nacional_p3008s2) <- c("Sí", "No", "Total p3008s2")
Total_nacional_p3008s2

Total_cab_p3008s2 <- matrix(c(p3008s2_1_cab, p3008s2_2_cab, p3008s2_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p3008s2) <- c("Total cabecera")
rownames(Total_cab_p3008s2) <- c("Sí", "No", "Total p3008s2")
Total_cab_p3008s2

Total_rural_p3008s2 <- matrix(c(p3008s2_1_rural, p3008s2_2_rural, p3008s2_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p3008s2) <- c("Total rural")
rownames(Total_rural_p3008s2) <- c("Sí", "No", "Total p3008s2")
Total_rural_p3008s2

Indicador_p3008s2 <- cbind(Total_nacional_p3008s2, Total_cab_p3008s2, Total_rural_p3008s2)
Indicador_p3008s2

# p1707
Total_nacional_p1707 <-matrix(c(p1707_1, p1707_2, p1707_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p1707) <- c("Total nacional")
rownames(Total_nacional_p1707) <- c("Sí", "No", "Total p1707")
Total_nacional_p1707

Total_cab_p1707 <- matrix(c(p1707_1_cab, p1707_2_cab, p1707_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p1707) <- c("Total cabecera")
rownames(Total_cab_p1707) <- c("Sí", "No", "Total p1707")
Total_cab_p1707

Total_rural_p1707 <- matrix(c(p1707_1_rural, p1707_2_rural, p1707_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p1707) <- c("Total rural")
rownames(Total_rural_p1707) <- c("Sí", "No", "Total p1707")
Total_rural_p1707

Indicador_p1707 <- cbind(Total_nacional_p1707, Total_cab_p1707, Total_rural_p1707)
Indicador_p1707

# p1707s1
Total_nacional_p1707s1 <-matrix(c(p1707s1_1, p1707s1_2, p1707s1_3, p1707s1_4, p1707s1_5, p1707s1_6, p1707s1_total), ncol = 1, nrow = 7)
colnames(Total_nacional_p1707s1) <- c("Total nacional")
rownames(Total_nacional_p1707s1) <- c("Todos los días de la semana (dos o más veces al día)", 
                                      "Todos los días de la semana (una vez al día)",
                                      "Cuatro a seis veces a la semana", 
                                      "Dos o tres veces a la semana", 
                                      "Una vez a la semana", "Menos de una vez por semana", "Total p1707s1")
Total_nacional_p1707s1

Total_cab_p1707s1 <- matrix(c(p1707s1_1_cab, p1707s1_2_cab, p1707s1_3_cab, p1707s1_4_cab, p1707s1_5_cab, p1707s1_6_cab,
                              p1707s1_total_cab), ncol = 1, nrow = 7)
colnames(Total_cab_p1707s1) <- c("Total cabecera")
rownames(Total_cab_p1707s1) <- c("Todos los días de la semana (dos o más veces al día)", 
                                 "Todos los días de la semana (una vez al día)",
                                 "Cuatro a seis veces a la semana", 
                                 "Dos o tres veces a la semana", 
                                 "Una vez a la semana", "Menos de una vez por semana", "Total p1707s1")
Total_cab_p1707s1

Total_rural_p1707s1 <- matrix(c(p1707s1_1_rural, p1707s1_2_rural, p1707s1_3_rural, p1707s1_4_rural,
                                p1707s1_5_rural, p1707s1_6_rural, p1707s1_total_rural), ncol = 1, nrow = 7)
colnames(Total_rural_p1707s1) <- c("Total rural")
rownames(Total_rural_p1707s1) <- c("Todos los días de la semana (dos o más veces al día)", 
                                   "Todos los días de la semana (una vez al día)",
                                   "Cuatro a seis veces a la semana", 
                                   "Dos o tres veces a la semana", 
                                   "Una vez a la semana", "Menos de una vez por semana", "Total p1707s1")
Total_rural_p1707s1

Indicador_p1707s1 <- cbind(Total_nacional_p1707s1, Total_cab_p1707s1, Total_rural_p1707s1)
Indicador_p1707s1

# p3003
Total_nacional_p3003 <-matrix(c(p3003_1, p3003_2, p3003_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p3003) <- c("Total nacional")
rownames(Total_nacional_p3003) <- c("Sí", "No", "Total p3003")
Total_nacional_p3003

Total_cab_p3003 <- matrix(c(p3003_1_cab, p3003_2_cab, p3003_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p3003) <- c("Total cabecera")
rownames(Total_cab_p3003) <- c("Sí", "No", "Total p3003")
Total_cab_p3003

Total_rural_p3003 <- matrix(c(p3003_1_rural, p3003_2_rural, p3003_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p3003) <- c("Total rural")
rownames(Total_rural_p3003) <- c("Sí", "No", "Total p3003")
Total_rural_p3003

Indicador_p3003 <- cbind(Total_nacional_p3003, Total_cab_p3003, Total_rural_p3003)
Indicador_p3003

# p3003s1
Total_nacional_p3003s1 <-matrix(c(p3003s1_1, p3003s1_2, p3003s1_3, p3003s1_4, p3003s1_5, p3003s1_6, p3003s1_total), ncol = 1, nrow = 7)
colnames(Total_nacional_p3003s1) <- c("Total nacional")
rownames(Total_nacional_p3003s1) <- c("Todos los días de la semana (dos o más veces al día)", 
                                      "Todos los días de la semana (una vez al día)",
                                      "Cuatro a seis veces a la semana", 
                                      "Dos o tres veces a la semana", 
                                      "Una vez a la semana", "Menos de una vez por semana", "Total p3003s1")
Total_nacional_p3003s1

Total_cab_p3003s1 <- matrix(c(p3003s1_1_cab, p3003s1_2_cab, p3003s1_3_cab, p3003s1_4_cab, p3003s1_5_cab, p3003s1_6_cab,
                              p3003s1_total_cab), ncol = 1, nrow = 7)
colnames(Total_cab_p3003s1) <- c("Total cabecera")
rownames(Total_cab_p3003s1) <- c("Todos los días de la semana (dos o más veces al día)", 
                                 "Todos los días de la semana (una vez al día)",
                                 "Cuatro a seis veces a la semana", 
                                 "Dos o tres veces a la semana", 
                                 "Una vez a la semana", "Menos de una vez por semana", "Total p3003s1")
Total_cab_p3003s1

Total_rural_p3003s1 <- matrix(c(p3003s1_1_rural, p3003s1_2_rural, p3003s1_3_rural, p3003s1_4_rural,
                                p3003s1_5_rural, p3003s1_6_rural, p3003s1_total_rural), ncol = 1, nrow = 7)
colnames(Total_rural_p3003s1) <- c("Total rural")
rownames(Total_rural_p3003s1) <- c("Todos los días de la semana (dos o más veces al día)", 
                                   "Todos los días de la semana (una vez al día)",
                                   "Cuatro a seis veces a la semana", 
                                   "Dos o tres veces a la semana", 
                                   "Una vez a la semana", "Menos de una vez por semana", "Total p3003s1")
Total_rural_p3003s1

Indicador_p3003s1 <- cbind(Total_nacional_p3003s1, Total_cab_p3003s1, Total_rural_p3003s1)
Indicador_p3003s1

# p6133
Total_nacional_p6133 <-matrix(c(p6133_1, p6133_2, p6133_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p6133) <- c("Total nacional")
rownames(Total_nacional_p6133) <- c("Sí", "No", "Total p6133")
Total_nacional_p6133

Total_cab_p6133 <- matrix(c(p6133_1_cab, p6133_2_cab, p6133_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p6133) <- c("Total cabecera")
rownames(Total_cab_p6133) <- c("Sí", "No", "Total p6133")
Total_cab_p6133

Total_rural_p6133 <- matrix(c(p6133_1_rural, p6133_2_rural, p6133_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p6133) <- c("Total rural")
rownames(Total_rural_p6133) <- c("Sí", "No", "Total p6133")
Total_rural_p6133

Indicador_p6133 <- cbind(Total_nacional_p6133, Total_cab_p6133, Total_rural_p6133)
Indicador_p6133

# p8560s1
Total_nacional_p8560s1 <-matrix(c(p8560s1_1, p8560s1_2, p8560s1_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p8560s1) <- c("Total nacional")
rownames(Total_nacional_p8560s1) <- c("Sí", "No", "Total p8560s1")
Total_nacional_p8560s1

Total_cab_p8560s1 <- matrix(c(p8560s1_1_cab, p8560s1_2_cab, p8560s1_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p8560s1) <- c("Total cabecera")
rownames(Total_cab_p8560s1) <- c("Sí", "No", "Total p8560s1")
Total_cab_p8560s1

Total_rural_p8560s1 <- matrix(c(p8560s1_1_rural, p8560s1_2_rural, p8560s1_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p8560s1) <- c("Total rural")
rownames(Total_rural_p8560s1) <- c("Sí", "No", "Total p8560s1")
Total_rural_p8560s1

Indicador_p8560s1 <- cbind(Total_nacional_p8560s1, Total_cab_p8560s1, Total_rural_p8560s1)
Indicador_p8560s1

# p8560s2
Total_nacional_p8560s2 <-matrix(c(p8560s2_1, p8560s2_2, p8560s2_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p8560s2) <- c("Total nacional")
rownames(Total_nacional_p8560s2) <- c("Sí", "No", "Total p8560s2")
Total_nacional_p8560s2

Total_cab_p8560s2 <- matrix(c(p8560s2_1_cab, p8560s2_2_cab, p8560s2_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p8560s2) <- c("Total cabecera")
rownames(Total_cab_p8560s2) <- c("Sí", "No", "Total p8560s2")
Total_cab_p8560s2

Total_rural_p8560s2 <- matrix(c(p8560s2_1_rural, p8560s2_2_rural, p8560s2_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p8560s2) <- c("Total rural")
rownames(Total_rural_p8560s2) <- c("Sí", "No", "Total p8560s2")
Total_rural_p8560s2

Indicador_p8560s2 <- cbind(Total_nacional_p8560s2, Total_cab_p8560s2, Total_rural_p8560s2)
Indicador_p8560s2

# p8560s3
Total_nacional_p8560s3 <-matrix(c(p8560s3_1, p8560s3_2, p8560s3_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p8560s3) <- c("Total nacional")
rownames(Total_nacional_p8560s3) <- c("Sí", "No", "Total p8560s3")
Total_nacional_p8560s3

Total_cab_p8560s3 <- matrix(c(p8560s3_1_cab, p8560s3_2_cab, p8560s3_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p8560s3) <- c("Total cabecera")
rownames(Total_cab_p8560s3) <- c("Sí", "No", "Total p8560s3")
Total_cab_p8560s3

Total_rural_p8560s3 <- matrix(c(p8560s3_1_rural, p8560s3_2_rural, p8560s3_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p8560s3) <- c("Total rural")
rownames(Total_rural_p8560s3) <- c("Sí", "No", "Total p8560s3")
Total_rural_p8560s3

Indicador_p8560s3 <- cbind(Total_nacional_p8560s3, Total_cab_p8560s3, Total_rural_p8560s3)
Indicador_p8560s3

# p8560s4
Total_nacional_p8560s4 <-matrix(c(p8560s4_1, p8560s4_2, p8560s4_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p8560s4) <- c("Total nacional")
rownames(Total_nacional_p8560s4) <- c("Sí", "No", "Total p8560s4")
Total_nacional_p8560s4

Total_cab_p8560s4 <- matrix(c(p8560s4_1_cab, p8560s4_2_cab, p8560s4_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p8560s4) <- c("Total cabecera")
rownames(Total_cab_p8560s4) <- c("Sí", "No", "Total p8560s4")
Total_cab_p8560s4

Total_rural_p8560s4 <- matrix(c(p8560s4_1_rural, p8560s4_2_rural, p8560s4_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p8560s4) <- c("Total rural")
rownames(Total_rural_p8560s4) <- c("Sí", "No", "Total p8560s4")
Total_rural_p8560s4

Indicador_p8560s4 <- cbind(Total_nacional_p8560s4, Total_cab_p8560s4, Total_rural_p8560s4)
Indicador_p8560s4

# p8560s5
Total_nacional_p8560s5 <-matrix(c(p8560s5_1, p8560s5_2, p8560s5_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p8560s5) <- c("Total nacional")
rownames(Total_nacional_p8560s5) <- c("Sí", "No", "Total p8560s5")
Total_nacional_p8560s5

Total_cab_p8560s5 <- matrix(c(p8560s5_1_cab, p8560s5_2_cab, p8560s5_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p8560s5) <- c("Total cabecera")
rownames(Total_cab_p8560s5) <- c("Sí", "No", "Total p8560s5")
Total_cab_p8560s5

Total_rural_p8560s5 <- matrix(c(p8560s5_1_rural, p8560s5_2_rural, p8560s5_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p8560s5) <- c("Total rural")
rownames(Total_rural_p8560s5) <- c("Sí", "No", "Total p8560s5")
Total_rural_p8560s5

Indicador_p8560s5 <- cbind(Total_nacional_p8560s5, Total_cab_p8560s5, Total_rural_p8560s5)
Indicador_p8560s5

# p8561
Total_nacional_p8561 <-matrix(c(p8561_1, p8561_2, p8561_3, p8561_4, p8561_total), ncol = 1, nrow = 5)
colnames(Total_nacional_p8561) <- c("Total nacional")
rownames(Total_nacional_p8561) <- c("Muy buena", "Buena", "Mala", "Muy mala", "Total p8561")
Total_nacional_p8561

Total_cab_p8561 <- matrix(c(p8561_1_cab, p8561_2_cab, p8561_3_cab, p8561_4_cab, p8561_total_cab), ncol = 1, nrow = 5)
colnames(Total_cab_p8561) <- c("Total cabecera")
rownames(Total_cab_p8561) <- c("Muy buena", "Buena", "Mala", "Muy mala", "Total p8561")
Total_cab_p8561

Total_rural_p8561 <- matrix(c(p8561_1_rural, p8561_2_rural, p8561_3_rural, p8561_4_rural, p8561_total_rural), ncol = 1, nrow = 5)
colnames(Total_rural_p8561) <- c("Total rural")
rownames(Total_rural_p8561) <- c("Muy buena", "Buena", "Mala", "Muy mala", "Total p8561")
Total_rural_p8561

Indicador_p8561 <- cbind(Total_nacional_p8561, Total_cab_p8561, Total_rural_p8561)
Indicador_p8561

# p3335
Total_nacional_p3335 <-matrix(c(p3335_1, p3335_2, p3335_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p3335) <- c("Total nacional")
rownames(Total_nacional_p3335) <- c("Sí", "No", "Total p3335")
Total_nacional_p3335

Total_cab_p3335 <- matrix(c(p3335_1_cab, p3335_2_cab, p3335_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p3335) <- c("Total cabecera")
rownames(Total_cab_p3335) <- c("Sí", "No", "Total p3335")
Total_cab_p3335

Total_rural_p3335 <- matrix(c(p3335_1_rural, p3335_2_rural, p3335_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p3335) <- c("Total rural")
rownames(Total_rural_p3335) <- c("Sí", "No", "Total p3335")
Total_rural_p3335

Indicador_p3335 <- cbind(Total_nacional_p3335, Total_cab_p3335, Total_rural_p3335)
Indicador_p3335

# p8584
Total_nacional_p8584 <-matrix(c(p8584_1, p8584_2, p8584_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p8584) <- c("Total nacional")
rownames(Total_nacional_p8584) <- c("Sí", "No", "Total p8584")
Total_nacional_p8584

Total_cab_p8584 <- matrix(c(p8584_1_cab, p8584_2_cab, p8584_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p8584) <- c("Total cabecera")
rownames(Total_cab_p8584) <- c("Sí", "No", "Total p8584")
Total_cab_p8584

Total_rural_p8584 <- matrix(c(p8584_1_rural, p8584_2_rural, p8584_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p8584) <- c("Total rural")
rownames(Total_rural_p8584) <- c("Sí", "No", "Total p8584")
Total_rural_p8584

Indicador_p8584 <- cbind(Total_nacional_p8584, Total_cab_p8584, Total_rural_p8584)
Indicador_p8584

# p5694
Total_nacional_p5694 <-matrix(c(p5694_1, p5694_2, p5694_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p5694) <- c("Total nacional")
rownames(Total_nacional_p5694) <- c("Sí", "No", "Total p5694")
Total_nacional_p5694

Total_cab_p5694 <- matrix(c(p5694_1_cab, p5694_2_cab, p5694_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p5694) <- c("Total cabecera")
rownames(Total_cab_p5694) <- c("Sí", "No", "Total p5694")
Total_cab_p5694

Total_rural_p5694 <- matrix(c(p5694_1_rural, p5694_2_rural, p5694_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p5694) <- c("Total rural")
rownames(Total_rural_p5694) <- c("Sí", "No", "Total p5694")
Total_rural_p5694

Indicador_p5694 <- cbind(Total_nacional_p5694, Total_cab_p5694, Total_rural_p5694)
Indicador_p5694

# p5452
Total_nacional_p5452 <-matrix(c(p5452_1, p5452_2, p5452_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p5452) <- c("Total nacional")
rownames(Total_nacional_p5452) <- c("Sí", "No", "Total p5452")
Total_nacional_p5452

Total_cab_p5452 <- matrix(c(p5452_1_cab, p5452_2_cab, p5452_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p5452) <- c("Total cabecera")
rownames(Total_cab_p5452) <- c("Sí", "No", "Total p5452")
Total_cab_p5452

Total_rural_p5452 <- matrix(c(p5452_1_rural, p5452_2_rural, p5452_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p5452) <- c("Total rural")
rownames(Total_rural_p5452) <- c("Sí", "No", "Total p5452")
Total_rural_p5452

Indicador_p5452 <- cbind(Total_nacional_p5452, Total_cab_p5452, Total_rural_p5452)
Indicador_p5452

# p6161
Total_nacional_p6161 <-matrix(c(p6161_1, p6161_2, p6161_total), ncol = 1, nrow = 3)
colnames(Total_nacional_p6161) <- c("Total nacional")
rownames(Total_nacional_p6161) <- c("Sí", "No", "Total p6161")
Total_nacional_p6161

Total_cab_p6161 <- matrix(c(p6161_1_cab, p6161_2_cab, p6161_total_cab), ncol = 1, nrow = 3)
colnames(Total_cab_p6161) <- c("Total cabecera")
rownames(Total_cab_p6161) <- c("Sí", "No", "Total p6161")
Total_cab_p6161

Total_rural_p6161 <- matrix(c(p6161_1_rural, p6161_2_rural, p6161_total_rural), ncol = 1, nrow = 3)
colnames(Total_rural_p6161) <- c("Total rural")
rownames(Total_rural_p6161) <- c("Sí", "No", "Total p6161")
Total_rural_p6161

Indicador_p6161 <- cbind(Total_nacional_p6161, Total_cab_p6161, Total_rural_p6161)
Indicador_p6161

# p1089
Total_nacional_p1089 <-matrix(c(p1089_1, p1089_2, p1089_3, p1089_4, p1089_5, p1089_6, p1089_7, p1089_8, p1089_9, p1089_10,
                                p1089_11, p1089_total), ncol = 1, nrow = 12)
colnames(Total_nacional_p1089) <- c("Total nacional")
rownames(Total_nacional_p1089) <- c("No pensó que fuera necesario llevarlo/a a consulta",
                                    "La consulta es muy cara, no tiene plata",
                                    "El lugar donde lo atienden queda muy lejos / no hay servicio cerca", 
                                    "No pudo dejar el trabajo/no tuvo tiempo",
                                    "No está afiliado/a a EPS o a régimen subsidiado",
                                    "No consiguió cita cercana en el tiempo o lo atienden muy mal",
                                    "Los trámites en la EPS/IPS son muy complicados", 
                                    "Considera que no está en edad o es recién nacido/a", 
                                    "No tiene registro civil de nacimiento",
                                    "Cambio de EPS o de municipio",
                                    "Otra",
                                    "Total p1089")
Total_nacional_p1089

Total_cab_p1089 <- matrix(c(p1089_1_cab, p1089_2_cab, p1089_3_cab, p1089_4_cab, p1089_5_cab, p1089_6_cab, p1089_7_cab, 
                            p1089_8_cab, p1089_9_cab, p1089_10_cab, p1089_11_cab, p1089_total_cab), ncol = 1, nrow = 12)
colnames(Total_cab_p1089) <- c("Total cabecera")
rownames(Total_cab_p1089) <- c("No pensó que fuera necesario llevarlo/a a consulta",
                               "La consulta es muy cara, no tiene plata",
                               "El lugar donde lo atienden queda muy lejos / no hay servicio cerca", 
                               "No pudo dejar el trabajo/no tuvo tiempo",
                               "No está afiliado/a a EPS o a régimen subsidiado",
                               "No consiguió cita cercana en el tiempo o lo atienden muy mal",
                               "Los trámites en la EPS/IPS son muy complicados", 
                               "Considera que no está en edad o es recién nacido/a", 
                               "No tiene registro civil de nacimiento",
                               "Cambio de EPS o de municipio",
                               "Otra",
                               "Total p1089")
Total_cab_p1089

Total_rural_p1089 <- matrix(c(p1089_1_rural, p1089_2_rural, p1089_3_rural, p1089_4_rural, p1089_5_rural, p1089_6_rural, p1089_7_rural, 
                              p1089_8_rural, p1089_9_rural, p1089_10_rural, p1089_11_rural, p1089_total_rural), ncol = 1, nrow = 12)
colnames(Total_rural_p1089) <- c("Total rural")
rownames(Total_rural_p1089) <- c("No pensó que fuera necesario llevarlo/a a consulta",
                                 "La consulta es muy cara, no tiene plata",
                                 "El lugar donde lo atienden queda muy lejos / no hay servicio cerca", 
                                 "No pudo dejar el trabajo/no tuvo tiempo",
                                 "No está afiliado/a a EPS o a régimen subsidiado",
                                 "No consiguió cita cercana en el tiempo o lo atienden muy mal",
                                 "Los trámites en la EPS/IPS son muy complicados", 
                                 "Considera que no está en edad o es recién nacido/a", 
                                 "No tiene registro civil de nacimiento",
                                 "Cambio de EPS o de municipio",
                                 "Otra",
                                 "Total p1089")
Total_rural_p1089

Indicador_p1089 <- cbind(Total_nacional_p1089, Total_cab_p1089, Total_rural_p1089)
Indicador_p1089

# uniendo los diferentes indicadores de la encuesta

Tabla_encuesta_salud <- rbind(Indicador_p6090, Indicador_p768, Indicador_p6100, Indicador_p6115, Indicador_p5669, Indicador_p6181,
                              Indicador_p798, Indicador_p799s2, Indicador_p799s3, Indicador_p799s1, Indicador_p799s4, Indicador_p799s5,
                              Indicador_p6127, Indicador_p1930, Indicador_p1930s1, Indicador_p1906s1, Indicador_p1906s2, Indicador_p1906s3,
                              Indicador_p1906s4, Indicador_p1906s5, Indicador_p1906s6, Indicador_p1906s7, Indicador_p1906s8, Indicador_p1908s1,
                              Indicador_p1908s2, Indicador_p1908s3, Indicador_p1908s4, Indicador_p1908s5, Indicador_p1908s6, Indicador_p1908s7,
                              Indicador_p1908s8, Indicador_p1909s1, Indicador_p1909s2, Indicador_p1909s3, Indicador_p1909s4, Indicador_p1909s5,
                              Indicador_p1909s6, Indicador_p6126, Indicador_p6126s2, Indicador_p6126s3, Indicador_p5665, Indicador_p8563, 
                              Indicador_p1092, Indicador_p8573, Indicador_p8575, Indicador_p8577, Indicador_p770, Indicador_p6153, Indicador_p6199,
                              Indicador_p6199s1, Indicador_p6145, Indicador_p8554, Indicador_p801, Indicador_p8556s, Indicador_p6147,
                              Indicador_p6148, Indicador_p6149, Indicador_p3178, Indicador_p3178s1, Indicador_p3178s2, Indicador_p3178s3,
                              Indicador_p3179, Indicador_p3179s1, Indicador_p3179s2, Indicador_p3179s3, Indicador_p3181, Indicador_p3182,
                              Indicador_p3183, Indicador_p3184, Indicador_p3185, Indicador_p3186, Indicador_p3187s1, Indicador_p3188, Indicador_p3188s1,
                              Indicador_p3188s2, Indicador_p3188s3, Indicador_p3008s1, Indicador_p3008s1a1, Indicador_p3008s2, Indicador_p1707, 
                              Indicador_p1707s1, Indicador_p3003, Indicador_p3003s1, Indicador_p6133, Indicador_p8560s1, Indicador_p8560s2, Indicador_p8560s3,
                              Indicador_p8560s4, Indicador_p8560s5, Indicador_p8561, Indicador_p3335, Indicador_p8584, Indicador_p5694, Indicador_p5452, 
                              Indicador_p6161, Indicador_p1089)

# exportar los indicadores en tablas a excel
write.csv2(Tabla_encuesta_salud, file = "ENCV 2021 NACIONAL 1.csv")
write.table(Tabla_encuesta_salud, file = "ENCV 2021 NACIONAL 1.txt", sep = ";")

