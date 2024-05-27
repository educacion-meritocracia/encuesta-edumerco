# 0. Preparacion encuesta estudiantes ola 1. Se realiza un procesamiento a 9 variables 
      #referidas al experimento, merito, meritocracia en la escuela y justificacion de la desigualdad 

# 1. cargar librerias ---------------------------------------------------------
install.packages("pacman")
pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer)
library(haven)

# 2. cargar bbdd --------------------------------------------------------------
rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica


datos_estudiantes <- read_sav("input/data/original/300424_BDD_estudiantes.sav")
frq(proc_datos_estudiantes$p1_5)

# 3. seleccionar variables ----------------------------------------------------

#p1_1 (en chile, las personas son recompensadas por su esfuerzo)
#p1_2 (en chile, las personas son recompensadas por su inteligencia y habilidad)
#p2_1 (quienes se esfuerzan obtienen buenas notas)
#p2_2 (en esta esceula, queines son inteligentes obtienen buenas notas)
#p2_3 (en esta escuela, los/as estudiantes obtienen las notas que se merecen)
#p9_3 (esta bien que aquellos que puedan pagar mas tengan mejor educación)
#p9_4 (esta bien que aquellos que puedan pagar mas tengan mejor acceso a salud)
#p9_5 (esta bien que en Chile las personas con mayores ingresos puedan tener 
        #mejores pensiones que las personas de ingresos mas bajos) 
#d3_def (en que curso estas)
#p26 (nivel de estudios de la madre)
#p27 (nivel de estudios del padre)
#p30 (cantidad de libros en el hogar)

frq(datos_estudiantes$p26)


proc_datos_estudiantes <- datos_estudiantes %>% select(aleatorio, p1_1, p1_2, 
                                                       p2_1, p2_2, p2_3, p9_3, 
                                                       p9_4, p9_5, d3_def, p26, p27, 
                                                       p30, p20, check_atencion, 
                                                       tratamiento, control, d2,
                                                       p5, p7, p17_1, p17_2, p19, p10_1, p10_2,
                                                       p10_5, p10_6, p11_2, p11_3, p12_1,
                                                       p12_3, p13_2, p13_4, p13_6, p18_1,
                                                       p18_2, p18_5, p18_6, p3, p1_3, p1_4, 
                                                       p1_5, p1_6, p1_7, p1_8,p1_9, p1_10, 
                                                       p10_3, p10_3, p10_4, p10_7, p10_8, p12_2, 
                                                       p13_1, p13_5, p11_1, p18_3, p9_1, p9_2, p9_6, 
                                                       p14, p15, P16_o1, P16_o2, P16_o3, P16_o4, P16_o5, P16_o6)
#renombrar 
proc_datos_estudiantes <- proc_datos_estudiantes %>% rename(
                                                           merit_esfuerzo = p1_1,
                                                           merit_talento = p1_2,
                                                           school_esfuerzo = p2_1,
                                                           school_talento = p2_2,
                                                           school_merito = p2_3,
                                                           just_educ = p9_3,
                                                           just_salud = p9_4,
                                                           just_pensiones = p9_5,
                                                           curso_estudiante = d3_def,
                                                           ne_madre = p26,
                                                           ne_padre = p27, 
                                                           libros_hogar = p30,
                                                           genero = p20,
                                                           check_tratamiento = tratamiento,
                                                           check_control = control,
                                                           school_dependencia = d2,
                                                           notas_merit = p5,
                                                           notas_esfuerzo = p7,
                                                           pp_futura_pol = p17_1,
                                                           pp_presente_pol = p17_2, 
                                                           school_ciudadania = p19,
                                                           ciudadania_voto = p10_1,
                                                           ciudadania_pp = p10_2,
                                                           ciudadania_ley = p10_5, 
                                                           ciudadania_op = p10_6, 
                                                           pp_futura_voto = p11_2,
                                                           pp_futura_candidatos = p11_3, 
                                                           pp_presente_marcha = p12_1,
                                                           pp_presente_toma = p12_3, 
                                                           pp_presente_rrss = p13_2,
                                                           pp_presente_compartir = p13_4, 
                                                           pp_presente_like = p13_6,
                                                           school_ciudadania_es = p18_1,
                                                           school_ciudadania_op = p18_2,
                                                           school_ciudadania_dif = p18_5, 
                                                           school_ciudadania_class = p18_6)                                                            

# Comprobar
names(proc_datos_estudiantes)

# 4. procesamiento de variables -----------------------------------------------

#ordenar por variable (9)

## aleatorio ----

get_label(proc_datos_estudiantes$aleatorio)

### a. descriptivo basico ----
frq(proc_datos_estudiantes$aleatorio) #no tiene etiquetas y no presenta casos perdidos

### b. etiquetamiento ----
proc_datos_estudiantes$aleatorio <- set_labels(proc_datos_estudiantes$aleatorio,
                             labels=c( "Tratamiento"= 1,
                                       "Control"= 2))

### c. recodificacion ----
proc_datos_estudiantes$aleatorio <- factor(proc_datos_estudiantes$aleatorio, 
                             levels=c(1,2),
                             labels=c("Tratamiento","Control"))

summary(proc_datos_estudiantes$aleatorio) #confirmar

## merit_esfuerzo  ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$merit_esfuerzo) #buen sentido. Etiquetda.Casos perdidos:
  #88 no sabe tiene 2 casos y 99 preferiria no responder 0 casos. 

### b. recodificacion ----
proc_datos_estudiantes$merit_esfuerzo <- recode(proc_datos_estudiantes$merit_esfuerzo, "c(88,99)=NA")

### c. etiqutamiento ----
proc_datos_estudiantes$merit_esfuerzo <- set_label(x = proc_datos_estudiantes$merit_esfuerzo,label = 
                                           "En Chile, las personas son recompensadas por su esfuerzo")

get_label(proc_datos_estudiantes$merit_esfuerzo)

## merit_talento ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$merit_talento) #buen sentido. Etiquetada. Casos perdidos: 
  #88 no sabe 7 casos, 99 preferiria no responder 1 caso 

### b. recodificacion ----
proc_datos_estudiantes$merit_talento <- recode(proc_datos_estudiantes$merit_talento, "c(88,99)=NA")

## school_esfuerzo ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$school_esfuerzo) #buen sentido. Etiquetada. Casos perdidos: 
  #88 no sabe 0 casos, 99 preferiria no responder 2 casos

### b. recodificacion ----
proc_datos_estudiantes$school_esfuerzo <- recode(proc_datos_estudiantes$school_esfuerzo, "c(88,99)=NA")

## school_talento ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$school_talento) #buen sentido. Etiquetada. Casos perdidos: 
  #88 no sabe 0 casos, 99 preferiria no responder 1 caso. 

### b. recodificacion ----
proc_datos_estudiantes$school_talento <- recode(proc_datos_estudiantes$school_talento, "c(88,99)=NA")

## school_merito ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$school_merito) #buen sentido. Etiquetada. Casos perdidos: 
  #88 no sabe 3 casos, 99 preferiria no responder 3 casos.

### b. recodificacion ----
proc_datos_estudiantes$school_merito <- recode(proc_datos_estudiantes$school_merito, "c(88,99)=NA")

## just_educ ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$just_educ) #buen sentido. Etiquetada. Casos perdidos: 
  #88 no sabe 3 casos, 99 preferiria no responder 0 casos.

### b. recodificacion ----
proc_datos_estudiantes$just_educ <- recode(proc_datos_estudiantes$just_educ, "c(88,99)=NA")

## just_salud ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$just_salud) #buen sentido. Etiquetada. Casos perdidos: 
  #88 no sabe 3 casos, 99 preferiria no responder 2 casos

### b. recodificacion ----
proc_datos_estudiantes$just_salud <- recode(proc_datos_estudiantes$just_salud, "c(88,99)=NA")

## just_pensiones ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$just_pensiones) #buen sentido. Etiquetada. Casos perdidos: 
  #88 no sabe 2 casos, 99 preferiria no responder 1 caso

### b. recodificacion ----
proc_datos_estudiantes$just_pensiones <- recode(proc_datos_estudiantes$just_pensiones, "c(88,99)=NA")

## curso_estudiante ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$curso_estudiante) #no tiene NA 

### b. recodificacion ----
proc_datos_estudiantes <- proc_datos_estudiantes %>%
  mutate(curso_estudiante = case_when(
    grepl("^(2|1|II|segundo|Segundo|sugundo|Media)", curso_estudiante, ignore.case = TRUE) ~ 'Media',
    grepl("^(7|6|Basica|Septimo|Séptimo|septimo|séptimo|sexto)", curso_estudiante, ignore.case = TRUE) ~ 'Básica',
    TRUE ~ curso_estudiante  # mantener el valor original si no coincide con ninguna condición
  ))


## genero ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$genero) #muestra con una mayoria de muejeres 48.52% y 
  #categoria otro: 4.52%. No tiene casos perdidos

### b. recodificacion ----
proc_datos_estudiantes$genero <- factor(proc_datos_estudiantes$genero, 
                                           levels=c(1,2,3),
                                           labels=c("Hombre","Mujer","Otro"))

## check_tratamiento ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$check_tratamiento) #colegio privado 39.13%. Casos perdidos: 
  #concentra la mayoria de las respuestas 47.83% (275 casos)

### b. recodificacion ---- 
proc_datos_estudiantes$check_tratamiento <- factor(proc_datos_estudiantes$check_tratamiento, 
                                           levels=c(1,2),
                                           labels=c("Colegio Municipal","Colegio Privado"))

## check_control ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$check_control) #colegio privado 38.43%. Casos perdidos: 
  #concentra la mayoria de las respuestas 52.17% (300 casos)

### b. recodificacion ----
proc_datos_estudiantes$check_control <- factor(proc_datos_estudiantes$check_control, 
                                           levels=c(1,2),
                                           labels=c("Colegio Municipal","Colegio Privado"))

### c. otros ajustes ----

#variable check_comprension
proc_datos_estudiantes <- proc_datos_estudiantes %>% 
  mutate(check_comprension = case_when(
    check_tratamiento == "Colegio Privado" | check_control == "Colegio Privado" ~ 1,
    is.na(check_tratamiento) | is.na(check_control) ~ 0,
    TRUE ~ 0
  ))

frq(proc_datos_estudiantes$check_comprension)

## check_atencion ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$check_atencion) #en desacuerdo 88.17% (507 casos). 
  #no presenta casos perdidos

## school_dependencia ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$school_dependencia) #particular subvencionado 2, 3, 4, 7, 8, 10, 11
                                    #Municipal 5 
                                     #Privado  6
                                      
#10  Instituto del Puerto, San Antonio -> particular subvencionado  
#11  Liceo Santa Teresa de Los Andes -> particular subvencionado 
#12 tambien particular subvencionado

### b. recodificacion ----
proc_datos_estudiantes <- proc_datos_estudiantes %>%
  mutate(school_dependencia = case_when(
    school_dependencia %in% c(2, 3, 4, 7, 8, 10, 11) ~ 1,
    school_dependencia == 5 ~ 2,
    school_dependencia == 6 ~ 3,
    TRUE ~ NA_integer_
  ))

proc_datos_estudiantes$school_dependencia <- factor(proc_datos_estudiantes$school_dependencia, 
                                               levels=c(1,2,3),
                                               labels=c("Colegio Particular Subvencionado", "Colegio Municipal","Colegio Privado"))

## notas_merit ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$notas_merit) #Buen sentido. Casos perdidos: 
  #88 (no sabe) 1 caso; 99 (preferiria no responder) 0 casos

### b. recodificacion ----
proc_datos_estudiantes$notas_merit <- recode(proc_datos_estudiantes$notas_merit, "c(88,99)=NA")


## notas_esfuerzo ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$notas_esfuerzo) #Buen sentido. Casos perdidos: 
#88 (no sabe) 4 casos; 99 (preferiria no responder) 3 casos

### b. recodificacion ----
proc_datos_estudiantes$notas_esfuerzo <- recode(proc_datos_estudiantes$notas_esfuerzo, "c(88,99)=NA")

## pp_futura_pol ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$pp_futura_pol) #Buen sentido. Casos perdidos: 
#88 (no sabe) 1 caso; 99 (preferiria no responder) 0 casos

### b. recodificacion ----
proc_datos_estudiantes$pp_futura_pol <- recode(proc_datos_estudiantes$pp_futura_pol, "c(88,99)=NA")


## pp_presente_pol ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$pp_presente_pol) #Buen sentido. Casos perdidos: 
#88 (no sabe) 1 caso; 99 (preferiria no responder) 4 casos

### b. recodificacion ----
proc_datos_estudiantes$pp_presente_pol <- recode(proc_datos_estudiantes$pp_presente_pol, "c(88,99)=NA")


## school_ciudadania ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$school_ciudadania) #Buen sentido. Casos perdidos: 
#88 (no sabe) 2 casos; 99 (preferiria no responder) 2 casos

### b. recodificacion ----
proc_datos_estudiantes$school_ciudadania <- recode(proc_datos_estudiantes$school_ciudadania, "c(88,99)=NA")

## ciudadania_voto ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$ciudadania_voto) #Buen sentido. Casos perdidos: 
#88 (no sabe) 1 caso; 99 (preferiria no responder) 0 casos

### b. recodificacion ----
proc_datos_estudiantes$ciudadania_voto <- recode(proc_datos_estudiantes$ciudadania_voto, "c(88,99)=NA")


## ciudadania_ley ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$ciudadania_ley)#Buen sentido. Casos perdidos: 
#88 (no sabe) 0 casos; 99 (preferiria no responder) 1 caso

### b. recodificacion ----
proc_datos_estudiantes$ciudadania_ley <- recode(proc_datos_estudiantes$ciudadania_ley, "c(88,99)=NA")


## ciudadania_op ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$ciudadania_op)#Buen sentido. Casos perdidos: 
#88 (no sabe) 0 casos; 99 (preferiria no responder) 1 caso

### b. recodificacion ----
proc_datos_estudiantes$ciudadania_op <- recode(proc_datos_estudiantes$ciudadania_op, "c(88,99)=NA")


## ciudadania_pp ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$ciudadania_pp)#Buen sentido. Casos perdidos: 
#88 (no sabe) 2 casos; 99 (preferiria no responder) 0 casos

### b. recodificacion ----
proc_datos_estudiantes$ciudadania_pp <- recode(proc_datos_estudiantes$ciudadania_pp, "c(88,99)=NA")


## pp_futura_voto ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$pp_futura_voto) #Buen sentido. Casos perdidos: 
#88 (no sabe) 1 caso; 99 (preferiria no responder) 0 casos


### b. recodificacion ----
proc_datos_estudiantes$pp_futura_voto <- recode(proc_datos_estudiantes$pp_futura_voto, "c(88,99)=NA")


## pp_futura_candidatos ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$pp_futura_candidatos) #Buen sentido. Casos perdidos: 
#88 (no sabe) 0 casos; 99 (preferiria no responder) 0 casos


## pp_presente_marcha ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$pp_presente_marcha) #Buen sentido. Casos perdidos: 
 #99 (preferiria no responder) 3 casos

### b. recodificacion ----
proc_datos_estudiantes$pp_presente_marcha <- recode(proc_datos_estudiantes$pp_presente_marcha, "c(99)=NA")


## pp_presente_toma ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$pp_presente_toma)#Buen sentido. Casos perdidos: 
#99 (preferiria no responder) 5 casos

### b. recodificacion ----
proc_datos_estudiantes$pp_presente_toma <- recode(proc_datos_estudiantes$pp_presente_toma, "c(99)=NA")


## pp_presente_like ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$pp_presente_like)#Buen sentido. Casos perdidos: 
#99 (preferiria no responder) 1 caso; 88 (No sabe) 0 casos

### b. recodificacion ----
proc_datos_estudiantes$pp_presente_like <- recode(proc_datos_estudiantes$pp_presente_like, "c(88,99)=NA")


## pp_presente_rrss ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$pp_presente_rrss)#Buen sentido. Casos perdidos: 
#99 (preferiria no responder) 1 caso; 88 (No sabe) 0 casos

### b. recodificacion ----
proc_datos_estudiantes$pp_presente_rrss <- recode(proc_datos_estudiantes$pp_presente_rrss, "c(88,99)=NA")


## pp_presente_compartir ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$pp_presente_compartir)#Buen sentido. Casos perdidos: 
#99 (preferiria no responder) 0 casos; 88 (No sabe) 1 caso

### b. recodificacion ----
proc_datos_estudiantes$pp_presente_compartir <- recode(proc_datos_estudiantes$pp_presente_compartir, "c(88,99)=NA")


## school_ciudadania_class ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$school_ciudadania_class)#Buen sentido. Casos perdidos: 
#99 (preferiria no responder) 1 caso; 88 (No sabe) 0 casos

### b. recodificacion ----
proc_datos_estudiantes$school_ciudadania_class <- recode(proc_datos_estudiantes$school_ciudadania_class, "c(88,99)=NA")


## school_ciudadania_dif ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$school_ciudadania_dif) #Buen sentido. Casos perdidos: 
#99 (preferiria no responder) 1 caso; 88 (No sabe) 3 casos

### b. recodificacion ----
proc_datos_estudiantes$school_ciudadania_dif <- recode(proc_datos_estudiantes$school_ciudadania_dif, "c(88,99)=NA")


## school_ciudadania_es ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$school_ciudadania_es)


## school_ciudadania_op ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$school_ciudadania_op)#Buen sentido. Casos perdidos: 
#99 (preferiria no responder) 1 caso; 88 (No sabe) 0 casos

### b. recodificacion ----
proc_datos_estudiantes$school_ciudadania_op <- recode(proc_datos_estudiantes$school_ciudadania_op, "c(88,99)=NA")


# 5. base procesada -----------------------------------------------------------
proc_datos_estudiantes <-as.data.frame(proc_datos_estudiantes)
stargazer(proc_datos_estudiantes, type="text")

save(proc_datos_estudiantes,file = "input/data/proc/es_ola1.RData")
