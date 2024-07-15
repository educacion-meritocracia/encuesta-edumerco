# 0. Preparacion encuesta estudiantes ola 1. Se realiza un procesamiento a 9 variables 
      #referidas al experimento, merito, meritocracia en la escuela y justificacion de la desigualdad 

# 1. cargar librerias ---------------------------------------------------------

#install.packages("pacman")
pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven)


# 2. cargar bbdd --------------------------------------------------------------
rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica


datos <- read_sav("input/data/original/310524_BDD_edumer.sav")
frq(datos$p12_1)
frq(datos$d3_nueva_Apoderado)
names(datos)

# 3. seleccionar variables ----------------------------------------------------

proc_datos <- datos %>% select(aleatorio, p1_1, p1_2, p2_1, p2_2, p2_3, p9_3,
                               p9_4, p9_5, d3_def, p26, p27, p30, p20, 
                               check_atencion, tratamiento, control, d2,p5, p7,
                               p17_1, p17_2, p19, p10_1, p10_2, p10_5, p10_6,
                               p11_2, p11_3, p12_1, p12_3, p13_2, p13_4, p13_6,
                               p18_1,p18_2, p18_5, p18_6, p3, p1_3, p1_4, p1_5, 
                               p1_6, p1_7, p1_8,p1_9, p1_10, p10_3, p10_3, p10_4, 
                               p10_7, p10_8, p12_2,p13_1, p13_5, p11_1, p18_3, 
                               p9_1, p9_2, p9_6, p14, p15, P16_o1, P16_o2, P16_o3, 
                               P16_o4, P16_o5, P16_o6, p1_1_Apoderado, p1_1_docente, 
                               p1_2_docente,p1_2_Apoderado, p1_3_Apoderado, 
                               p1_3_docente, p1_4_Apoderado, p1_4_docente,
                               p1_9_Apoderado, p1_9_docente, p1_10_Apoderado, 
                               p1_10_docente, p15_Apoderado, p13_Apoderado, 
                               p11_Apoderado, p13_docente, p18_Apoderado,
                               p1_5_Apoderado, p1_6_Apoderado, p1_7_Apoderado,
                               p1_8_Apoderado, p5_6_Apoderado, p1_5_docente, 
                               p1_6_docente, p1_7_docente, p1_8_docente, 
                               p4_6_docente, p4, p6, d6_Apoderado, p2_1_Apoderado,
                               p2_2_Apoderado, p2_3_Apoderado, p3_Apoderado,
                               p2_1_docente, p2_2_docente, p2_3_docente, p3_0_docente,
                               SbjNum_docente)
#renombrar 
proc_datos <- proc_datos %>% rename(merit_esfuerzo_percep_ES = p1_1,
                                    merit_talento_percep_ES = p1_2,
                                    social_merito_percep_ES = p1_10,
                                    oportunidades_percep_ES = p1_9,
                                    buenos_contactos_percep_ES = p1_4,
                                    padres_ricos_percep_ES = p1_3,
                                    merit_esfuerzo_percep_PROF = p1_1_docente,
                                    merit_talento_percep_PROF = p1_2_docente,
                                    social_merito_percep_PROF = p1_10_docente,
                                    oportunidades_percep_PROF = p1_9_docente,
                                    buenos_contactos_percep_PROF = p1_4_docente,
                                    padres_ricos_percep_PROF = p1_3_docente,
                                    merit_esfuerzo_percep_AP = p1_1_Apoderado,
                                    merit_talento_percep_AP = p1_2_Apoderado,
                                    social_merito_percep_AP = p1_10_Apoderado,
                                    oportunidades_percep_AP = p1_9_Apoderado,
                                    buenos_contactos_percep_AP = p1_4_Apoderado,
                                    padres_ricos_percep_AP = p1_3_Apoderado,
                                    school_esfuerzo_ES = p2_1,
                                    school_talento_ES = p2_2,
                                    school_merito_ES = p2_3,
                                    school_esfuerzo_AP = p2_1_Apoderado,
                                    school_talento_AP =p2_2_Apoderado,
                                    school_merito_AP = p2_3_Apoderado,
                                    school_esfuerzo_PROF = p2_1_docente ,
                                    school_talento_PROF = p2_2_docente,
                                    school_merito_PROF = p2_3_docente,
                                    school_pref_ES =p3,
                                    school_pref_AP = p3_Apoderado,
                                    school_pref_PROF = p3_0_docente,
                                    just_educ = p9_3,
                                    just_salud = p9_4,
                                    just_pensiones = p9_5,
                                    curso_estudiante = d3_def,
                                    ne_madre = p26,
                                    ne_padre = p27,
                                    libros_hogar = p30,
                                    genero_ES = p20,
                                    genero_AP = p11_Apoderado,
                                    genero_PROF = p13_docente,
                                    iden_pol_AP = p13_Apoderado,
                                    religion_AP = p18_Apoderado,
                                    check_tratamiento = tratamiento,
                                    check_control = control,
                                    school_dependencia = d2,
                                    notas_merit = p5,
                                    notas_esfuerzo = p7,
                                    pp_futura_pol = p17_1,
                                    pp_presente_pol = p17_2,
                                    school_ciudadania = p19,
                                    ciudadania_voto_es = p10_1,
                                    ciudadania_pp = p10_2,
                                    ciudadania_ley = p10_5,
                                    ciudadania_op= p10_6, 
                                    pp_futura_voto = p11_2,
                                    pp_futura_candidatos = p11_3,
                                    pp_presente_marcha = p12_1,
                                    pp_presente_toma = p12_3,
                                    pp_presente_rrss = p13_2,
                                    pp_presente_compartir = p13_4,
                                    pp_presente_like= p13_6,
                                    school_ciudadania_es = p18_1,
                                    school_ciudadania_op = p18_2,
                                    school_ciudadania_dif = p18_5,
                                    school_ciudadania_class = p18_6,
                                    merit_esfuerzo_pref_ES = p1_5,
                                    merit_talento_pref_ES = p1_6,
                                    social_merit_pref_ES = p9_6,
                                    padres_ricos_pref_ES = p1_7,
                                    buenos_contactos_pref_ES = p1_8,
                                    school_merit_pref = p3, 
                                    nota_obtenida = p4, 
                                    nota_pref = p6, 
                                    merit_esfuerzo_pref_AP = p1_5_Apoderado,
                                    merit_talento_pref_AP = p1_6_Apoderado ,
                                    social_merit_pref_AP = p5_6_Apoderado,
                                    padres_ricos_pref_AP = p1_7_Apoderado, 
                                    buenos_contactos_pref_AP = p1_8_Apoderado,
                                    merit_esfuerzo_pref_PROF = p1_5_docente,
                                    merit_talento_pref_PROF = p1_6_docente,
                                    social_merit_pref_PROF = p4_6_docente, 
                                    padres_ricos_pref_PROF = p1_7_docente,
                                    buenos_contactos_pref_PROF = p1_8_docente,
                                    apoderado = d6_Apoderado)                                                            

# Comprobar
frq(proc_datos$school_merit_pref)

# 4. procesamiento de variables -----------------------------------------------

#ordenar por variable (9)

## aleatorio ----

get_label(proc_datos$aleatorio)

### a. descriptivo basico ----
frq(proc_datos$aleatorio) #no tiene etiquetas y no presenta casos perdidos

### b. etiquetamiento ----
proc_datos$aleatorio <- set_labels(proc_datos$aleatorio,
                             labels=c( "Tratamiento"= 1,
                                       "Control"= 2))

### c. recodificacion ----
proc_datos$aleatorio <- factor(proc_datos$aleatorio, 
                             levels=c(1,2),
                             labels=c("Tratamiento","Control"))

summary(proc_datos$aleatorio) #confirmar

# PERCEPCION MERITO SOCIAL -----------------------------------------------------

## merit_esfuerzo_es  ----

### a. descriptivo basico ----
frq(proc_datos$merit_esfuerzo_percep_ES) #buen sentido. Etiquetda.Casos perdidos:
  #88 no sabe tiene 2 casos y 99 preferiria no responder 0 casos. 

### b. recodificacion ----
proc_datos$merit_esfuerzo_percep_ES <- recode(proc_datos$merit_esfuerzo_percep_ES, "c(88,99)=NA")

### c. etiqutamiento ----
proc_datos$merit_esfuerzo_percep_ES <- set_label(x = proc_datos$merit_esfuerzo_percep_ES,label = 
                                           "En Chile, las personas son recompensadas por su esfuerzo")

get_label(proc_datos$merit_esfuerzo_percep_ES)

## merit_talento_ ----

### a. descriptivo basico ----
frq(proc_datos$merit_talento_percep_ES) #buen sentido. Etiquetada. Casos perdidos: 
  #88 no sabe 7 casos, 99 preferiria no responder 1 caso 

### b. recodificacion ----
proc_datos$merit_talento_percep_ES <- recode(proc_datos$merit_talento_percep_ES, "c(88,99)=NA")

### c. otros ajustes ----
proc_datos$merit_talento_percep_ES <- set_label(x = proc_datos$merit_talento_percep_ES,label = 
                                                     "En Chile, las personas son recompensadas por su inteligencia y habilidad")

get_label(proc_datos$merit_talento_percep_ES)


## social_merito_percep_ES ----

### a. descriptivo basico----
frq(proc_datos$social_merito_percep_ES)

### b. recodficiacion ----
proc_datos$social_merito_percep_ES <- recode(proc_datos$social_merito_percep_ES, "c(88,99)=NA")

### c. otros ajustes ----
proc_datos$social_merito_percep_ES <- set_label(x = proc_datos$social_merito_percep_ES,label = 
                                                     "En Chile, todas las personas obtienen lo que merecen")

get_label(proc_datos$social_merito_percep_ES)

## merit_esfuerzo_percep_PROF ----

### a. descriptivo basico----
frq(proc_datos$merit_esfuerzo_percep_PROF)

### c. otros ajustes ----
proc_datos$merit_esfuerzo_percep_PROF <- set_label(x = proc_datos$merit_esfuerzo_percep_PROF,label = 
                                                     "En Chile, las personas son recompensadas por su esfuerzo")

get_label(proc_datos$merit_esfuerzo_percep_PROF)

## merit_talento_percep_PROF ----

### a. descriptivo basico----
frq(proc_datos$merit_talento_percep_PROF)


### c. otros ajustes ----
proc_datos$merit_talento_percep_PROF <- set_label(x = proc_datos$merit_talento_percep_PROF,label = 
                                                    "En Chile, las personas son recompensadas por su inteligencia y habilidad")

get_label(proc_datos$merit_talento_percep_PROF)

## social_merito_percep_PROF ----

### a. descriptivo basico----
frq(proc_datos$social_merito_percep_PROF)

### c. otros ajustes ----
proc_datos$social_merito_percep_PROF <- set_label(x = proc_datos$social_merito_percep_PROF,label = 
                                                    "En Chile, todas las personas obtienen lo que merecen")

get_label(proc_datos$social_merito_percep_PROF)

## merit_esfuerzo_percep_AP ----

### a. descriptivo basico----
frq(proc_datos$merit_esfuerzo_percep_AP)

### b. recodficiacion ----
proc_datos$merit_esfuerzo_percep_AP <- recode(proc_datos$merit_esfuerzo_percep_AP, "c(88,99)=NA")

### c. otros ajustes ----
proc_datos$merit_esfuerzo_percep_AP <- set_label(x = proc_datos$merit_esfuerzo_percep_AP,label = 
                                                   "En Chile, las personas son recompensadas por su esfuerzo")

get_label(proc_datos$merit_esfuerzo_percep_AP)

## merit_talento_percep_AP  -----

### a. descriptivo basico----
frq(proc_datos$merit_talento_percep_AP)

### b. recodficiacion ----
proc_datos$merit_talento_percep_AP <- recode(proc_datos$merit_talento_percep_AP, "c(88,99)=NA")

### c. otros ajustes ----
proc_datos$merit_talento_percep_AP <- set_label(x = proc_datos$merit_talento_percep_AP,label = 
                                                  "En Chile, las personas son recompensadas por su inteligencia y habilidad")

get_label(proc_datos$merit_talento_percep_AP)

## social_merito_percep_AP ----

### a. descriptivo basico----
frq(proc_datos$social_merito_percep_AP)

### b. recodficiacion ----
proc_datos$social_merito_percep_AP <- recode(proc_datos$social_merito_percep_AP, "c(88,99)=NA")

### c. otros ajustes ----
proc_datos$social_merito_percep_AP <- set_label(x = proc_datos$social_merito_percep_AP,label = 
                                                  "En Chile,todas las personas obtienen lo que merecen")

get_label(proc_datos$social_merito_percep_AP)


# PERCEPCION FACTORES NO-MERITOCRACTICOS ---------------------------------------

## oportunidades_percep_ES ----

### a. descriptivo basico----
frq(proc_datos$oportunidades_percep_ES)

### b. recodficiacion ----
proc_datos$oportunidades_percep_ES <- recode(proc_datos$oportunidades_percep_ES, "c(88,99)=NA")

### c. otros ajustes ----
proc_datos$oportunidades_percep_ES <- set_label(x = proc_datos$oportunidades_percep_ES,label = 
                                                     "En Chile, todas las personas tienen las mismas oportunidades para salir adelante")

get_label(proc_datos$oportunidades_percep_ES)

## buenos_contactos_percep_ES ----

### a. descriptivo basico----
frq(proc_datos$buenos_contactos_percep_ES)

### b. recodficiacion ----
proc_datos$buenos_contactos_percep_ES <- recode(proc_datos$buenos_contactos_percep_ES, "c(88,99)=NA")


### c. otros ajustes ----
proc_datos$buenos_contactos_percep_ES <- set_label(x = proc_datos$buenos_contactos_percep_ES,label = 
                                                              "En Chile, quienes tienen buenos contactos les va mejor en la vida")

get_label(proc_datos$buenos_contactos_percep_ES)

## padres_ricos_percep_ES ----

### a. descriptivo basico----
frq(proc_datos$padres_ricos_percep_ES)

### b. recodficiacion ----
proc_datos$padres_ricos_percep_ES <- recode(proc_datos$padres_ricos_percep_ES, "c(88,99)=NA")


### c. otros ajustes ----
proc_datos$padres_ricos_percep_ES <- set_label(x = proc_datos$padres_ricos_percep_ES,label = 
                                                                 "En Chile, quienes tienen padres ricos les va mejor en la vida")

get_label(proc_datos$buenos_contactos_percep_ES)


## oportunidades_percep_PROF ----

### a. descriptivo basico----
frq(proc_datos$oportunidades_percep_PROF)

### c. otros ajustes ----
proc_datos$oportunidades_percep_PROF <- set_label(x = proc_datos$oportunidades_percep_PROF,label = 
                                                                "En Chile, todas las personas tienen las mismas oportunidades para salir adelante")

get_label(proc_datos$oportunidades_percep_PROF)

## buenos_contactos_percep_PROF ----

### a. descriptivo basico----
frq(proc_datos$buenos_contactos_percep_PROF)


### c. otros ajustes ----
proc_datos$buenos_contactos_percep_PROF <- set_label(x = proc_datos$buenos_contactos_percep_PROF,label = 
                                                                "En Chile, quienes tienen buenos contactos les va mejor en la vida")

get_label(proc_datos$buenos_contactos_percep_PROF)


## padres_ricos_percep_PROF ----

### a. descriptivo basico----
frq(proc_datos$padres_ricos_percep_PROF)

### c. otros ajustes ----
proc_datos$padres_ricos_percep_PROF <- set_label(x = proc_datos$padres_ricos_percep_PROF,label = 
                                                                   "En Chile, quienes tienen padres ricos les va mejor en la vida")

get_label(proc_datos$padres_ricos_percep_PROF)

## oportunidades_percep_AP ----

### a. descriptivo basico----
frq(proc_datos$oportunidades_percep_AP)

### b. recodficiacion ----
proc_datos$oportunidades_percep_AP <- recode(proc_datos$oportunidades_percep_AP, "c(88,99)=NA")

### c. otros ajustes ----
proc_datos$oportunidades_percep_AP <- set_label(x = proc_datos$oportunidades_percep_AP,label = 
                                                              "En Chile,todas las personas tienen las mismas oportunidades para salir adelante")

get_label(proc_datos$oportunidades_percep_AP)

## buenos_contactos_percep_AP -----

### a. descriptivo basico----
frq(proc_datos$buenos_contactos_percep_AP)

### b. recodficiacion ----
proc_datos$buenos_contactos_percep_AP <- recode(proc_datos$buenos_contactos_percep_AP, "c(88,99)=NA")

### c. otros ajustes ----
proc_datos$buenos_contactos_percep_AP <- set_label(x = proc_datos$buenos_contactos_percep_AP,label = 
                                                              "En Chile, quienes tienen buenos contactos les va mucho mejor en la vida")

get_label(proc_datos$buenos_contactos_percep_AP)

## padres_ricos_percep_AP -----

### a. descriptivo basico----
frq(proc_datos$padres_ricos_percep_AP)

### b. recodficiacion ----
proc_datos$padres_ricos_percep_AP <- recode(proc_datos$padres_ricos_percep_AP, "c(88,99)=NA")

### c. otros ajustes ----
proc_datos$padres_ricos_percep_AP <- set_label(x = proc_datos$padres_ricos_percep_AP,label = 
                                                                 "En Chile, quienes tienen padres ricos les va mucho mejor en la vida")

get_label(proc_datos$padres_ricos_percep_AP)

# PREFERENCIA MERITO SOCIAL ----------------------------------------------------

## merit_esfuerzo_pref_ES ----

### a. descriptivo basico -----
frq(proc_datos$merit_esfuerzo_pref_ES) # 88 = 7 casos; 99 = 1 caso

### b. recodificación ----
proc_datos$merit_esfuerzo_pref_ES <- recode(proc_datos$merit_esfuerzo_pref_ES, "c(88,99)=NA")

### c. otros ajustes ----
proc_datos$merit_esfuerzo_pref_ES <- set_label(x = proc_datos$merit_esfuerzo_pref_ES,label = 
                                                 "Quienes más se esfuerzan deberían obtener mayores recompensas que quienes se esfuerzan menos")

get_label(proc_datos$merit_esfuerzo_pref_ES)

## merit_talento_pref_ES ----

### a. descriptivo basico -----
frq(proc_datos$merit_talento_pref_ES) # 88 = 5 casos; 99 = 8 caso

### b. recodificación ----
proc_datos$merit_talento_pref_ES <- recode(proc_datos$merit_talento_pref_ES, "c(88,99)=NA")

### c. otros ajustes ----
proc_datos$merit_talento_pref_ES <- set_label(x = proc_datos$merit_talento_pref_ES,label = 
                                                 "Quienes poseen más talento deberían obtener mayores recompensas que quienes poseen menos talento")

get_label(proc_datos$merit_esfuerzo_pref_ES)

## social_merit_pref_ES ---- 

### a. descriptivo basico -----
frq(proc_datos$social_merit_pref_ES) # 88 = 5 casos; 99 = 1 caso

### b. recodificación ----
proc_datos$social_merit_pref_ES <- recode(proc_datos$social_merit_pref_ES, "c(88,99)=NA")

### c. otros ajustes ----
proc_datos$social_merit_pref_ES <- set_label(x = proc_datos$social_merit_pref_ES,label = 
                                                "Está bien que las personas más inteligentes y/o talentosas ganen más dinero, aun cuando requieran esforzarse menos para ello ")

get_label(proc_datos$social_merit_pref_ES)

## merit_esfuerzo_pref_AP ----

### a. descriptivo basico -----
frq(proc_datos$merit_esfuerzo_pref_AP) # 88 = 7 casos; 99 = 1 caso

### b. recodificación ----
proc_datos$merit_esfuerzo_pref_AP <- recode(proc_datos$merit_esfuerzo_pref_AP, "c(88,99)=NA")

### c. otros ajustes ----
proc_datos$merit_esfuerzo_pref_AP <- set_label(x = proc_datos$merit_esfuerzo_pref_AP,label = 
                                                 "Quienes más se esfuerzan deberían obtener mayores recompensas que quienes se esfuerzan menos")

get_label(proc_datos$merit_esfuerzo_pref_AP)

## merit_talento_pref_AP ----

### a. descriptivo basico -----
frq(proc_datos$merit_talento_pref_AP) # 88 = 5 casos; 99 = 8 caso

### b. recodificación ----
proc_datos$merit_talento_pref_AP <- recode(proc_datos$merit_talento_pref_AP, "c(88,99)=NA")

### c. otros ajustes ----
proc_datos$merit_talento_pref_AP <- set_label(x = proc_datos$merit_talento_pref_AP,label = 
                                                "Quienes poseen más talento deberían obtener mayores recompensas que quienes poseen menos talento")

get_label(proc_datos$merit_talento_pref_AP)

## social_merit_pref_AP ---- 

### a. descriptivo basico -----
frq(proc_datos$social_merit_pref_AP) # 88 = 5 casos; 99 = 1 caso

### b. recodificación ----
proc_datos$social_merit_pref_AP <- recode(proc_datos$social_merit_pref_AP, "c(88,99)=NA")

### c. otros ajustes ----
proc_datos$social_merit_pref_AP <- set_label(x = proc_datos$social_merit_pref_AP,label = 
                                               "Está bien que las personas más inteligentes y/o talentosas ganen más dinero, aun cuando requieran esforzarse menos para ello")

get_label(proc_datos$social_merit_pref_AP)


#PREFERENCIA FACTORES NO MERITOCRÁTICOS ----------------------------------------

## padres_ricos_pref_ES ----

### a. descriptivo basico -----
frq(proc_datos$padres_ricos_pref_ES) # 88 = 5 casos; 99 = 3 caso

### b. recodificación ----
proc_datos$padres_ricos_pref_ES <- recode(proc_datos$padres_ricos_pref_ES, "c(88,99)=NA")

### c. otros ajustes ----
proc_datos$padres_ricos_pref_ES <- set_label(x = proc_datos$padres_ricos_pref_ES,label = 
                                               "Está bien que quienes tienen padres ricos les vaya bien en la vida")

get_label(proc_datos$padres_ricos_pref_ES)


## buenos_contactos_pref_ES ----

### a. descriptivo basico -----
frq(proc_datos$buenos_contactos_pref_ES) # 88 = 2 casos; 99 = 5 caso

### b. recodificación ----
proc_datos$buenos_contactos_pref_ES <- recode(proc_datos$buenos_contactos_pref_ES, "c(88,99)=NA")

### c. otros ajustes ----
proc_datos$buenos_contactos_pref_ES <- set_label(x = proc_datos$buenos_contactos_pref_ES,label = 
                                               "Está bien que quienes tienen buenos contactos les vaya bien en la vida")

get_label(proc_datos$buenos_contactos_pref_ES)

## padres_ricos_pref_AP ----

### a. descriptivo basico -----
frq(proc_datos$padres_ricos_pref_AP) # 88 = 5 casos; 99 = 3 caso

### b. recodificación ----
proc_datos$padres_ricos_pref_AP <- recode(proc_datos$padres_ricos_pref_AP, "c(88,99)=NA")

### c. otros ajustes ----
proc_datos$padres_ricos_pref_AP <- set_label(x = proc_datos$padres_ricos_pref_AP,label = 
                                               "Está bien que quienes tienen padres ricos les vaya bien en la vida")

get_label(proc_datos$padres_ricos_pref_AP)


## buenos_contactos_pref_AP ----

### a. descriptivo basico -----
frq(proc_datos$buenos_contactos_pref_AP) # 88 = 2 casos; 99 = 5 caso

### b. recodificación ----
proc_datos$buenos_contactos_pref_AP <- recode(proc_datos$buenos_contactos_pref_AP, "c(88,99)=NA")

### c. otros ajustes ----
proc_datos$buenos_contactos_pref_AP <- set_label(x = proc_datos$buenos_contactos_pref_AP,label = 
                                                   "Está bien que quienes tienen buenos contactos les vaya bien en la vida")

get_label(proc_datos$buenos_contactos_pref_AP)

# ESCUELA ----------------------------------------------------------------------

## school_esfuerzo_ES ----

### a. descriptivo basico ----
frq(proc_datos$school_esfuerzo_ES) #buen sentido. Etiquetada. Casos perdidos: 
  #88 no sabe 0 casos, 99 preferiria no responder 2 casos

### b. recodificacion ----
proc_datos$school_esfuerzo_ES <- recode(proc_datos$school_esfuerzo_ES, "c(88,99)=NA")

### c. otros ajustes ----
proc_datos$school_esfuerzo_ES <- set_label(x = proc_datos$school_esfuerzo_ES,label = 
                                                   "En esta escuela, quienes se esfuerzan obtienen buenas notas")

get_label(proc_datos$school_esfuerzo_ES)

## school_talento_ES ----

### a. descriptivo basico ----
frq(proc_datos$school_talento_ES) #buen sentido. Etiquetada. Casos perdidos: 
  #88 no sabe 0 casos, 99 preferiria no responder 1 caso. 

### b. recodificacion ----
proc_datos$school_talento_ES <- recode(proc_datos$school_talento_ES, "c(88,99)=NA")

### c. otros ajustes ----
proc_datos$school_talento_ES <- set_label(x = proc_datos$school_talento_ES,label = 
                                             "En esta escuela, quienes son inteligentes obtienen buenas notas")

get_label(proc_datos$school_talento_ES)

## school_merito_ES ----

### a. descriptivo basico ----
frq(proc_datos$school_merito_ES) #buen sentido. Etiquetada. Casos perdidos: 
  #88 no sabe 3 casos, 99 preferiria no responder 3 casos.

### b. recodificacion ----
proc_datos$school_merito_ES <- recode(proc_datos$school_merito_ES, "c(88,99)=NA")

### c. otros ajustes ----
proc_datos$school_merito_ES <- set_label(x = proc_datos$school_merito_ES,label = 
                                            "En esta escuela, los/as estudiantes obtienen las notas que merecen")

get_label(proc_datos$school_merito_ES)


## school_esfuerzo_AP ----

### a. descriptivo basico ----
frq(proc_datos$school_esfuerzo_AP) #buen sentido. Etiquetada. Casos perdidos: 
#88 no sabe 0 casos, 99 preferiria no responder 2 casos

### b. recodificacion ----
proc_datos$school_esfuerzo_AP <- recode(proc_datos$school_esfuerzo_AP, "c(88,99)=NA")

### c. otros ajustes ----
proc_datos$school_esfuerzo_AP <- set_label(x = proc_datos$school_esfuerzo_AP,label = 
                                             "En esta escuela, quienes se esfuerzan obtienen buenas notas")

get_label(proc_datos$school_esfuerzo_AP)

## school_talento_AP ----

### a. descriptivo basico ----
frq(proc_datos$school_talento_AP) #buen sentido. Etiquetada. Casos perdidos: 
#88 no sabe 0 casos, 99 preferiria no responder 1 caso. 

### b. recodificacion ----
proc_datos$school_talento_AP <- recode(proc_datos$school_talento_AP, "c(88,99)=NA")

### c. otros ajustes ----
proc_datos$school_talento_AP <- set_label(x = proc_datos$school_talento_AP,label = 
                                            "En esta escuela, quienes son inteligentes obtienen buenas notas")

get_label(proc_datos$school_talento_AP)

## school_merito_AP----

### a. descriptivo basico ----
frq(proc_datos$school_merito_AP) #buen sentido. Etiquetada. Casos perdidos: 
#88 no sabe 3 casos, 99 preferiria no responder 3 casos.

### b. recodificacion ----
proc_datos$school_merito_AP <- recode(proc_datos$school_merito_AP, "c(88,99)=NA")

### c. otros ajustes ----
proc_datos$school_merito_AP <- set_label(x = proc_datos$school_merito_AP,label = 
                                           "En esta escuela, los/as estudiantes obtienen las notas que merecen")

get_label(proc_datos$school_merito_AP)

## school_merit_pref ----

### a. descriptivo basico 
frq(proc_datos$school_merit_pref)

### 



# DESIGUALDAD ------------------------------------------------------------------

## just_educ ----

### a. descriptivo basico ----
frq(proc_datos$just_educ) #buen sentido. Etiquetada. Casos perdidos: 
  #88 no sabe 3 casos, 99 preferiria no responder 0 casos.

### b. recodificacion ----
proc_datos$just_educ <- recode(proc_datos$just_educ, "c(88,99)=NA")

## just_salud ----

### a. descriptivo basico ----
frq(proc_datos$just_salud) #buen sentido. Etiquetada. Casos perdidos: 
  #88 no sabe 3 casos, 99 preferiria no responder 2 casos

### b. recodificacion ----
proc_datos$just_salud <- recode(proc_datos$just_salud, "c(88,99)=NA")

## just_pensiones ----

### a. descriptivo basico ----
frq(proc_datos$just_pensiones) #buen sentido. Etiquetada. Casos perdidos: 
  #88 no sabe 2 casos, 99 preferiria no responder 1 caso

### b. recodificacion ----
proc_datos$just_pensiones <- recode(proc_datos$just_pensiones, "c(88,99)=NA")


# SOCIODEMOGRAFICOS ------------------------------------------------------------

## curso_estudiante ----

### a. descriptivo basico ----
frq(proc_datos$curso_estudiante) #no tiene NA 

### b. recodificacion ----
#proc_datos <- proc_datos %>%
  #mutate(curso_estudiante = case_when(
    #grepl("^(2|1|II|segundo|Segundo|sugundo|Media)", curso_estudiante, ignore.case = TRUE) ~ 'Media',
    #grepl("^(7|6|Basica|Septimo|Séptimo|septimo|séptimo|sexto)", curso_estudiante, ignore.case = TRUE) ~ 'Básica',
    #TRUE ~ curso_estudiante  # mantener el valor original si no coincide con ninguna condición
  #))

proc_datos <- proc_datos %>%
  mutate(curso_estudiante = case_when(
    grepl("^(2|1)", curso_estudiante, ignore.case = TRUE) ~ 'Media',
    grepl("^(7|6)", curso_estudiante, ignore.case = TRUE) ~ 'Básica',
    TRUE ~ curso_estudiante  # mantener el valor original si no coincide con ninguna condición
  ))

## genero_ES ----

### a. descriptivo basico ----
frq(proc_datos$genero_ES) #muestra con una mayoria de muejeres 48.52% y 
  #categoria otro: 4.52%. No tiene casos perdidos
proc_datos$genero_ES <- recode(proc_datos$genero_ES, "3=NA")

### b. recodificacion ----
proc_datos$genero_ES <- factor(proc_datos$genero_ES, 
                                           levels=c(1,2),
                                           labels=c("Hombre","Mujer"))
## genero_AP ----
### a. descriptivo basico ----
frq(proc_datos$genero_AP)

### b. recodificacion ----
proc_datos$genero_AP <- factor(proc_datos$genero_AP, 
                               levels=c(1,2,3),
                               labels=c("Hombre","Mujer","Otro"))
## genero_PROF ---- 
### a. descriptivo basico ----
frq(proc_datos$genero_PROF)

### b. recodificacion ----
proc_datos$genero_PROF <- factor(proc_datos$genero_PROF, 
                               levels=c(1,2,3),
                               labels=c("Hombre","Mujer","Otro"))

## libros_hogar ----
### a, descriptivo basico
frq(proc_datos$libros_hogar)

### b. recodificación ----
proc_datos$libros_hogar <- recode(proc_datos$libros_hogar, "c(88,99)=NA; 1=1;2=1; 3=2; 4=2;5=2;6=2")

proc_datos$libros_hogar <- factor(proc_datos$libros_hogar, 
                                 levels=c(1,2),
                                 labels=c("Menos de 25 libros","Más de 25 libros"))

### c. otros ajustes ---- 

## ne más alto padres
proc_datos$ne_madre <- recode(proc_datos$ne_madre, "c(88,99)=NA")
proc_datos$ne_padre <- recode(proc_datos$ne_padre, "c(88,99)=NA")

proc_datos <- proc_datos %>%
  mutate(educ_max = case_when(
    !is.na(ne_madre) & is.na(ne_padre) ~ ne_madre,
    is.na(ne_madre) & !is.na(ne_padre) ~ ne_padre,
    !is.na(ne_madre) & !is.na(ne_padre) ~ pmax(ne_madre, ne_padre, na.rm = TRUE),
    TRUE ~ NA_real_
  ))

frq(proc_datos$educ_max)

proc_datos$educ_max <- recode(proc_datos$educ_max, "1=1; 2=1; 3=1; 4=2; 5=2; 6=2")

proc_datos$educ_max <- factor(proc_datos$educ_max, 
                              levels=c(1,2),
                              labels=c("Enseñanza media o menos","Estudios superiores"))

## ne_madre ----

### a, descriptivo basico
frq(proc_datos$ne_madre)

### b. recodificación ----
#proc_datos$ne_madre <- recode(proc_datos$ne_madre, "1=1; 2=1; 3=1; 4=2; 5=2; 6=2")

#proc_datos$ne_madre <- factor(proc_datos$ne_madre, 
 #                                 levels=c(1,2),
  #                                labels=c("Enseñanza media o menos","Estudios superiores"))

## ne_padre ----

### a, descriptivo basico
frq(proc_datos$ne_padre)

### b. recodificación ----

#proc_datos$ne_padre <- recode(proc_datos$ne_padre, "1=1; 2=1; 3=1; 4=2; 5=2; 6=2")

#proc_datos$ne_padre <- factor(proc_datos$ne_padre, 
 #                             levels=c(1,2),
  #                            labels=c("Enseñanza media o menos","Estudios superiores"))

# 1 Básica incompleta, 2 Básica completa, 3 Media completa, 
# 4 Instituto Profesional/Técnico completa, 5 Universidad pregrado completa, 6 Posgrado


# EXPERIMIENTO -----------------------------------------------------------------

## check_tratamiento ----

### a. descriptivo basico ----
frq(proc_datos$check_tratamiento) #colegio privado 39.13%. Casos perdidos: 
  #concentra la mayoria de las respuestas 47.83% (275 casos)

### b. recodificacion ---- 
proc_datos$check_tratamiento <- factor(proc_datos$check_tratamiento, 
                                           levels=c(1,2),
                                           labels=c("Colegio Municipal","Colegio Privado"))

## check_control ----

### a. descriptivo basico ----
frq(proc_datos$check_control) #colegio privado 38.43%. Casos perdidos: 
  #concentra la mayoria de las respuestas 52.17% (300 casos)

### b. recodificacion ----
proc_datos$check_control <- factor(proc_datos$check_control, 
                                           levels=c(1,2),
                                           labels=c("Colegio Municipal","Colegio Privado"))

### c. otros ajustes ----

#variable check_comprension
proc_datos <- proc_datos %>% 
  mutate(check_comprension = case_when(
    check_tratamiento == "Colegio Privado" | check_control == "Colegio Privado" ~ 1,
    is.na(check_tratamiento) | is.na(check_control) ~ 0,
    TRUE ~ 0
  ))

frq(proc_datos$check_comprension)

## check_atencion ----

### a. descriptivo basico ----
frq(proc_datos$check_atencion) #en desacuerdo 88.17% (507 casos).
  #no presenta casos perdidos

## school_dependencia ----

### a. descriptivo basico ----
frq(proc_datos$school_dependencia) #particular subvencionado 2, 3, 4, 7, 8, 10, 11
                                    #Municipal 5 
                                     #Privado  6
                                      
#10  Instituto del Puerto, San Antonio -> particular subvencionado  
#11  Liceo Santa Teresa de Los Andes -> particular subvencionado 
#12 tambien particular subvencionado




### b. recodificacion ----
proc_datos <- proc_datos %>%
  mutate(dependencia = case_when(
    school_dependencia %in% c(2, 3, 4, 7, 8, 10, 11) ~ 1,
    school_dependencia == 5 ~ 2,
    school_dependencia == 6 ~ 3,
    TRUE ~ NA_integer_
  ))

proc_datos$dependencia <- factor(proc_datos$dependencia, 
                                               levels=c(1,2,3),
                                               labels=c("Colegio Particular Subvencionado", "Colegio Municipal","Colegio Privado"))


# NOTAS ------------------------------------------------------------------------

## notas_merit ----

### a. descriptivo basico ----
frq(proc_datos$notas_merit) #Buen sentido. Casos perdidos: 
  #88 (no sabe) 1 caso; 99 (preferiria no responder) 0 casos

### b. recodificacion ----
proc_datos$notas_merit <- recode(proc_datos$notas_merit, "c(88,99)=NA")


## notas_esfuerzo ----

### a. descriptivo basico ----
frq(proc_datos$notas_esfuerzo) #Buen sentido. Casos perdidos: 
#88 (no sabe) 4 casos; 99 (preferiria no responder) 3 casos

### b. recodificacion ----
proc_datos$notas_esfuerzo <- recode(proc_datos$notas_esfuerzo, "c(88,99)=NA")


# CIUDADANIA -------------------------------------------------------------------

## pp_futura_pol ----

### a. descriptivo basico ----
frq(proc_datos$pp_futura_pol) #Buen sentido. Casos perdidos: 
#88 (no sabe) 1 caso; 99 (preferiria no responder) 0 casos

### b. recodificacion ----
proc_datos$pp_futura_pol <- recode(proc_datos$pp_futura_pol, "c(88,99)=NA")


## pp_presente_pol ----

### a. descriptivo basico ----
frq(proc_datos$pp_presente_pol) #Buen sentido. Casos perdidos: 
#88 (no sabe) 1 caso; 99 (preferiria no responder) 4 casos

### b. recodificacion ----
proc_datos$pp_presente_pol <- recode(proc_datos$pp_presente_pol, "c(88,99)=NA")


## school_ciudadania ----

### a. descriptivo basico ----
frq(proc_datos$school_ciudadania) #Buen sentido. Casos perdidos: 
#88 (no sabe) 2 casos; 99 (preferiria no responder) 2 casos

### b. recodificacion ----
proc_datos$school_ciudadania <- recode(proc_datos$school_ciudadania, "c(88,99)=NA")

## ciudadania_voto ----

### a. descriptivo basico ----
frq(proc_datos$ciudadania_voto_es) #Buen sentido. Casos perdidos: 
#88 (no sabe) 1 caso; 99 (preferiria no responder) 0 casos

### b. recodificacion ----
proc_datos$ciudadania_voto_es <- recode(proc_datos$ciudadania_voto_es, "c(88,99)=NA")


## ciudadania_ley ----

### a. descriptivo basico ----
frq(proc_datos$ciudadania_ley)#Buen sentido. Casos perdidos: 
#88 (no sabe) 0 casos; 99 (preferiria no responder) 1 caso

### b. recodificacion ----
proc_datos$ciudadania_ley <- recode(proc_datos$ciudadania_ley, "c(88,99)=NA")


## ciudadania_op ----

### a. descriptivo basico ----
frq(proc_datos$ciudadania_op)#Buen sentido. Casos perdidos: 
#88 (no sabe) 0 casos; 99 (preferiria no responder) 1 caso

### b. recodificacion ----
proc_datos$ciudadania_op <- recode(proc_datos$ciudadania_op, "c(88,99)=NA")


## ciudadania_pp ----

### a. descriptivo basico ----
frq(proc_datos$ciudadania_pp)#Buen sentido. Casos perdidos: 
#88 (no sabe) 2 casos; 99 (preferiria no responder) 0 casos

### b. recodificacion ----
proc_datos$ciudadania_pp <- recode(proc_datos$ciudadania_pp, "c(88,99)=NA")


## pp_futura_voto ----

### a. descriptivo basico ----
frq(proc_datos$pp_futura_voto) #Buen sentido. Casos perdidos: 
#88 (no sabe) 1 caso; 99 (preferiria no responder) 0 casos


### b. recodificacion ----
proc_datos$pp_futura_voto <- recode(proc_datos$pp_futura_voto, "c(88,99)=NA")


## pp_futura_candidatos ----

### a. descriptivo basico ----
frq(proc_datos$pp_futura_candidatos) #Buen sentido. Casos perdidos: 
#88 (no sabe) 0 casos; 99 (preferiria no responder) 0 casos


## pp_presente_marcha ----

### a. descriptivo basico ----
frq(proc_datos$pp_presente_marcha) #Buen sentido. Casos perdidos: 
 #99 (preferiria no responder) 3 casos

### b. recodificacion ----
proc_datos$pp_presente_marcha <- recode(proc_datos$pp_presente_marcha, "c(99)=NA")


## pp_presente_toma ----

### a. descriptivo basico ----
frq(proc_datos$pp_presente_toma)#Buen sentido. Casos perdidos: 
#99 (preferiria no responder) 5 casos

### b. recodificacion ----
proc_datos$pp_presente_toma <- recode(proc_datos$pp_presente_toma, "c(99)=NA")


## pp_presente_like ----

### a. descriptivo basico ----
frq(proc_datos$pp_presente_like)#Buen sentido. Casos perdidos: 
#99 (preferiria no responder) 1 caso; 88 (No sabe) 0 casos

### b. recodificacion ----
proc_datos$pp_presente_like <- recode(proc_datos$pp_presente_like, "c(88,99)=NA")


## pp_presente_rrss ----

### a. descriptivo basico ----
frq(proc_datos$pp_presente_rrss)#Buen sentido. Casos perdidos: 
#99 (preferiria no responder) 1 caso; 88 (No sabe) 0 casos

### b. recodificacion ----
proc_datos$pp_presente_rrss <- recode(proc_datos$pp_presente_rrss, "c(88,99)=NA")


## pp_presente_compartir ----

### a. descriptivo basico ----
frq(proc_datos$pp_presente_compartir)#Buen sentido. Casos perdidos: 
#99 (preferiria no responder) 0 casos; 88 (No sabe) 1 caso

### b. recodificacion ----
proc_datos$pp_presente_compartir <- recode(proc_datos$pp_presente_compartir, "c(88,99)=NA")


## school_ciudadania_class ----

### a. descriptivo basico ----
frq(proc_datos$school_ciudadania_class)#Buen sentido. Casos perdidos: 
#99 (preferiria no responder) 1 caso; 88 (No sabe) 0 casos

### b. recodificacion ----
proc_datos$school_ciudadania_class <- recode(proc_datos$school_ciudadania_class, "c(88,99)=NA")


## school_ciudadania_dif ----

### a. descriptivo basico ----
frq(proc_datos$school_ciudadania_dif) #Buen sentido. Casos perdidos: 
#99 (preferiria no responder) 1 caso; 88 (No sabe) 3 casos

### b. recodificacion ----
proc_datos$school_ciudadania_dif <- recode(proc_datos$school_ciudadania_dif, "c(88,99)=NA")


## school_ciudadania_es ----

### a. descriptivo basico ----
frq(proc_datos$school_ciudadania_es)


## school_ciudadania_op ----

### a. descriptivo basico ----
frq(proc_datos$school_ciudadania_op)#Buen sentido. Casos perdidos: 
#99 (preferiria no responder) 1 caso; 88 (No sabe) 0 casos

### b. recodificacion ----
proc_datos$school_ciudadania_op <- recode(proc_datos$school_ciudadania_op, "c(88,99)=NA")


# 5. base procesada -----------------------------------------------------------
proc_datos <-as.data.frame(proc_datos)
stargazer(proc_datos, type="text")

save(proc_datos,file = "input/data/proc/ola1.RData")
