################ POBREZA MULTIDIMENCIONAL DE CONEVAL ################

# Después de analizar la metodología...

# Calcular las carencias por espacios
# 2022 --------------

#importar la base de datos

library(readr)

viviendas_2022 <- read_csv("~/Downloads/R_MMP_2022/Bases de datos/viviendas.csv")

View(viviendas_2022)

# Ahora la de hogar (el concentrado)

hogar_con_2022 <- read_csv("01_datos/concentradohogar_2022.csv")
View(concentradohogar_2022)

car_cal_espacios_2022 <- left_join(hogar_con_2022, viviendas_coneval,  by = c("folioviv", "factor")) %>% #unirlas por folioviv y factor hace que ambas 
#coincidan en esas dos variables
  mutate(icv_pisos = as.numeric(mat_pisos), # Material de los pisos de la vivienda
         icv_pisos=case_when(icv_pisos>=2 ~ 0,
                             icv_pisos==1 ~ 1),
         icv_techos=as.numeric(mat_techos), # Material de los techos de la vivienda
         icv_techos=case_when(icv_techos >=3 ~ 0,
                              icv_techos <=2 ~ 1),
         icv_muros=as.numeric(mat_pared), # Material de muros en la vivienda
         icv_muros=case_when(icv_muros >=6 ~ 0,
                             icv_muros <=5 ~ 1),
         # Espacios en la vivienda (Hacinamiento)
         # Número de residentes en la vivienda
         num_ind = tot_resid ,
         # Número de cuartos en la vivienda
         num_cua = num_cuarto ,
         # Índice de hacinamiento
         cv_hac=num_ind/num_cua,
         # Indicador de carencia por hacinamiento en la vivienda
         icv_hac=case_when(
           cv_hac>2.5 & !is.na(cv_hac) ~ 1,
           cv_hac<=2.5 ~ 0 ))

carencia_espacios_2022_2 <- mutate(car_cal_espacios_2022,
                                   ic_cv=case_when(
                                     is.na(icv_pisos) | is.na(icv_techos) | is.na(icv_muros) | is.na(icv_hac) ~ NA_real_,
                                     icv_pisos==1 | icv_techos==1 | icv_muros==1 | icv_hac==1 ~ 1,     # Con carencia
                                     icv_pisos==0 & icv_techos==0 & icv_muros==0 & icv_hac==0 ~ 0)) %>% # Sin carencia
  select(folioviv, foliohog, icv_pisos, icv_techos, 
         icv_muros, icv_hac, ic_cv, factor, tot_integ) # seleccionar cada variable que usaremos


total_car_esp_2022 <-carencia_espacios_2022_2 %>% 
  transmute(folioviv = folioviv,
            ic_cv = as.numeric(ic_cv),
            tot_integ = as.numeric(tot_integ),
            pob_pr = ic_cv * tot_integ, # cuántos integrantes hay en las viviendas que tienen carencias
            factor = as.numeric(factor),
            pob_sub = pob_pr * factor, # cuántos integrantes totales hay en las viviendas con carencias
            pob_tot = sum(pob_sub, na.rm = T)) # se suma para poder establecer cuántas personas tienen carencias en espacios, no cuántas viviendas


car_esp_tot_2022 <- total_car_esp_2022 %>% 
  select(pob_tot) %>% 
  unique(total_car_esp_2022$pob_tot, incomparables = FALSE) #transformar el total en un solo número para poder graficar

print(car_esp_tot_2022)

# 2020 -------------------

# Exportar las bases de datos de 2020

library(readr)
viviendas_2020 <- read_csv("01_datos/viviendas_2020.csv")
View(viviendas_2020)

hogar_con_2020 <- read_csv("01_datos/concentradohogar_2020.csv")
View(hogar_2020)

car_cal_espacios_2020 <- left_join(hogar_con_2020, viviendas_2020,  by = c("folioviv", "factor")) %>%
  mutate(icv_pisos = as.numeric(mat_pisos), # Material de los pisos de la vivienda
         icv_pisos=case_when(icv_pisos>=2 ~ 0,
                             icv_pisos==1 ~ 1),
         icv_techos=as.numeric(mat_techos), # Material de los techos de la vivienda
         icv_techos=case_when(icv_techos >=3 ~ 0,
                              icv_techos <=2 ~ 1),
         icv_muros=as.numeric(mat_pared), # Material de muros en la vivienda
         icv_muros=case_when(icv_muros >=6 ~ 0,
                             icv_muros <=5 ~ 1),
         # Espacios en la vivienda (Hacinamiento)
         # Número de residentes en la vivienda
         num_ind = tot_resid ,
         # Número de cuartos en la vivienda
         num_cua = num_cuarto ,
         # Índice de hacinamiento
         cv_hac=num_ind/num_cua,
         # Indicador de carencia por hacinamiento en la vivienda
         icv_hac=case_when(
           cv_hac>2.5 & !is.na(cv_hac) ~ 1,
           cv_hac<=2.5 ~ 0 ))

carencia_espacios_2020 <- mutate(car_cal_espacios_2020_2,
                                   ic_cv=case_when(
                                     is.na(icv_pisos) | is.na(icv_techos) | is.na(icv_muros) | is.na(icv_hac) ~ NA_real_,
                                     icv_pisos==1 | icv_techos==1 | icv_muros==1 | icv_hac==1 ~ 1,     # Con carencia
                                     icv_pisos==0 & icv_techos==0 & icv_muros==0 & icv_hac==0 ~ 0)) %>% # Sin carencia
  select(folioviv, foliohog, icv_pisos, icv_techos, 
         icv_muros, icv_hac, ic_cv, factor, tot_integ) # seleccionar cada variable que usaremos

total_car_esp_2020 <-carencia_espacios_2020_2 %>% 
  transmute(folioviv = folioviv,
            ic_cv = as.numeric(ic_cv),
            tot_integ = as.numeric(tot_integ),
            pob_pr = ic_cv * tot_integ, # cuántos integrantes hay en las viviendas que tienen carencias
            factor = as.numeric(factor),
            pob_sub = pob_pr * factor, # cuántos integrantes totales hay en las viviendas con carencias
            pob_tot = sum(pob_sub, na.rm = T)) # se suma para poder establecer cuántas personas tienen carencias en espacios, no cuántas viviendas


car_esp_tot_2020 <- total_car_esp_2020 %>% 
  select(pob_tot) %>% 
  unique(total_car_esp_2020$pob_tot, incomparables = FALSE) #transformar el total en un solo número para poder graficar

print(car_esp_tot_2020)

# 2018 -----------------

# Exportar bases de datos de 2018

# El problema con este año es que el CONEVAL tiene unas bases de datos del INEGI (ENIGH) que no coinciden. Entonces, las bases estarán descargadas directamente desde la página del INEGI

library(readr)
viviendas_2018 <- read_csv("01_datos/Copia de enigh2018_ns_viviendas_csv.zip")
View(enigh2018_ns_viviendas_csv)

hogar_con_2018 <- read_csv("01_datos/enigh2018_ns_concentradohogar_csv.zip")
View(enigh2018_ns_concentradohogar_csv)

car_cal_espacios_2018 <- left_join(hogar_con_2018, viviendar_2018,  by = c("folioviv", "factor")) %>%
  mutate(icv_pisos = as.numeric(mat_pisos), # Material de los pisos de la vivienda
         icv_pisos = case_when(icv_pisos>=2 ~ 0,
                             icv_pisos==1 ~ 1),
         icv_techos=as.numeric(mat_techos), # Material de los techos de la vivienda
         icv_techos=case_when(icv_techos >=3 ~ 0,
                              icv_techos <=2 ~ 1),
         icv_muros=as.numeric(mat_pared), # Material de muros en la vivienda
         icv_muros=case_when(icv_muros >=6 ~ 0,
                             icv_muros <=5 ~ 1),
         # Espacios en la vivienda (Hacinamiento)
         # Número de residentes en la vivienda
         num_ind = tot_resid ,
         # Número de cuartos en la vivienda
         num_cua = num_cuarto ,
         # Índice de hacinamiento
         cv_hac=num_ind/num_cua,
         # Indicador de carencia por hacinamiento en la vivienda
         icv_hac=case_when(
           cv_hac>2.5 & !is.na(cv_hac) ~ 1,
           cv_hac<=2.5 ~ 0 ))

carencia_espacios_2018 <- mutate(car_cal_espacios_2018,
                                 ic_cv=case_when(
                                   is.na(icv_pisos) | is.na(icv_techos) | is.na(icv_muros) | is.na(icv_hac) ~ NA_real_,
                                   icv_pisos==1 | icv_techos==1 | icv_muros==1 | icv_hac==1 ~ 1,     # Con carencia
                                   icv_pisos==0 & icv_techos==0 & icv_muros==0 & icv_hac==0 ~ 0)) %>% # Sin carencia
  select(folioviv, foliohog, icv_pisos, icv_techos, 
         icv_muros, icv_hac, ic_cv, factor, tot_integ) #eseleccionar cada variable q usaremos


total_car_esp_2018 <-carencia_espacios_2018 %>% 
  transmute(folioviv = folioviv,
            ic_cv = as.numeric(ic_cv),
            tot_integ = as.numeric(tot_integ),
            pob_pr = ic_cv * tot_integ,
            factor = as.numeric(factor),
            pob_sub = pob_pr * factor,
            pob_tot = sum(pob_sub, na.rm = T))

car_esp_tot_2018 <- total_car_esp_2018 %>% 
  select(pob_tot) %>% 
  unique(total_car_esp_2018$pob_tot, incomparables = FALSE)

print(car_esp_tot_2018)

# Ahora es necesario juntar todas los números totales

tabulado_espacios <- list(car_esp_tot_2018, car_esp_tot_2020, car_esp_tot_2022)

tabulado_fial_esp <- bind_cols(tabulado_espacios)

print(tabulado_fial_esp)


# Ahora hacer lo mismo, pero con la carencia en servicios

# No es necesario importar bases de datos porque serán las mismas con las que se trabajó anteriormente

# 2022 ----------------

car_cal_svb_2022 <- left_join(hogar_con_2022, viviendas_2022,  by = c("folioviv", "factor")) %>%
  mutate(
    isb_agua=case_when(procaptar==1 & disp_agua=="4" ~ 0,
                       disp_agua>=3 & !is.na(disp_agua) ~ 1,
                       disp_agua<=2 & !is.na(disp_agua) ~ 0), # Indicador de carencia por acceso al agua
    # Indicador de carencia por servicio de drenaje
    isb_dren=case_when(drenaje>=3 ~1,
                       drenaje<=2 ~0),
    # Indicador de carencia por servicios de electricidad
    isb_luz=case_when(disp_elect>=5 ~1,
                      disp_elect<=4 ~0),
    # Indicador de carencia por combustible para cocinar
    combus=as.numeric(combustible),
    estufa=as.numeric(estufa_chi),
    isb_combus=(case_when((combus==1 | combus==2) & estufa==2 ~ 1,
                          (combus==1 | combus==2) & estufa==1 ~ 0,
                          combus>=3 & combus<=6 ~ 0)))

carencia_serv_2022 <- 
  mutate(car_cal_svb_2022,              
         ic_sbv=case_when(
           is.na(isb_agua) | is.na(isb_dren) | is.na(isb_luz) | is.na(isb_combus) ~ NA_real_,
           isb_agua==1 | isb_dren==1 | isb_luz==1 | isb_combus==1 ~ 1, # Con carencia
           isb_agua==0 & isb_dren==0 & isb_luz==0 & isb_combus==0 ~ 0)) %>% # Sin carencia
  select(folioviv, foliohog, isb_agua, isb_dren, isb_luz, 
         isb_combus, ic_sbv, factor, tot_integ)

total_car_sbv_2022 <-carencia_serv_2022 %>% 
  transmute(folioviv = folioviv,
            ic_sbv = as.numeric(ic_sbv),
            tot_integ = as.numeric(tot_integ),
            pob_pr = ic_sbv * tot_integ,
            factor = as.numeric(factor),
            pob_sub = pob_pr * factor,
            pob_tot = sum(pob_sub, na.rm = T))

car_sbv_tot_2022 <- total_car_sbv_2022 %>% 
  select(pob_tot) %>% 
  unique(total_car_sbv_2022$pob_tot, incomparables = FALSE)

print(car_sbv_tot_2022)

# 2020 -----------------

car_cal_svb_2020 <- left_join(hogar_con_2020, viviendas_2020,  by = c("folioviv", "factor")) %>%
  mutate(
    isb_agua=case_when(procaptar==1 & disp_agua=="4" ~ 0,
                       disp_agua>=3 & !is.na(disp_agua) ~ 1,
                       disp_agua<=2 & !is.na(disp_agua) ~ 0), # Indicador de carencia por acceso al agua
    # Indicador de carencia por servicio de drenaje
    isb_dren=case_when(drenaje>=3 ~1,
                       drenaje<=2 ~0),
    # Indicador de carencia por servicios de electricidad
    isb_luz=case_when(disp_elect>=5 ~1,
                      disp_elect<=4 ~0),
    # Indicador de carencia por combustible para cocinar
    combus=as.numeric(combustible),
    estufa=as.numeric(estufa_chi),
    isb_combus=(case_when((combus==1 | combus==2) & estufa==2 ~ 1,
                          (combus==1 | combus==2) & estufa==1 ~ 0,
                          combus>=3 & combus<=6 ~ 0)))

carencia_serv_2020 <- 
  mutate(car_cal_svb_2020,              
         ic_sbv=case_when(
           is.na(isb_agua) | is.na(isb_dren) | is.na(isb_luz) | is.na(isb_combus) ~ NA_real_,
           isb_agua==1 | isb_dren==1 | isb_luz==1 | isb_combus==1 ~ 1, # Con carencia
           isb_agua==0 & isb_dren==0 & isb_luz==0 & isb_combus==0 ~ 0)) %>% # Sin carencia
  select(folioviv, foliohog, isb_agua, isb_dren, isb_luz, 
         isb_combus, ic_sbv, factor, tot_integ)

total_car_sbv_2020 <-carencia_serv_2020 %>% 
  transmute(folioviv = folioviv,
            ic_sbv = as.numeric(ic_sbv),
            tot_integ = as.numeric(tot_integ),
            pob_pr = ic_sbv * tot_integ,
            factor = as.numeric(factor),
            pob_sub = pob_pr * factor,
            pob_tot = sum(pob_sub, na.rm = T))

car_sbv_tot_2020 <- total_car_sbv_2020 %>% 
  select(pob_tot) %>% 
  unique(total_car_sbv_2020$pob_tot, incomparables = FALSE)

print(car_sbv_tot_2020)

# 2018 ---------------
# En este año, a parte de descargar los datos directamente del INEGI, es necesario actualizar la metofología. En cuanto a la calidad de los espacios, la metodología no cambia.
# Sin emabrgo, en cuanto a la carencia por servicios básicos, sí cambia.

# Por eso, se cambiará el código a continuación

car_cal_svb_2018 <- left_join(hogar_con_2018, vivienda_2018,  by = c("folioviv", "factor")) %>%
  mutate(isb_agua=case_when(procaptar==1 & disp_agua=="4" ~ 0,
                            disp_agua>=3 & !is.na(disp_agua) ~ 1,
                            disp_agua<=2 & !is.na(disp_agua) ~ 0),
         # Indicador de carencia por servicio de drenaje
         isb_dren=case_when(drenaje>=3 ~1,
                            drenaje<=2 ~0),
         # Indicador de carencia por servicios de electricidad
         isb_luz=case_when(disp_elect>=5 ~1,
                           disp_elect<=4 ~0),
         # Indicador de carencia por combustible para cocinar
         combus=as.numeric(combustible),
         estufa=as.numeric(estufa_chi),
         isb_combus=(case_when((combus==1 | combus==2) & estufa==2 ~ 1,
                               (combus==1 | combus==2) & estufa==1 ~ 0,
                               combus>=3 & combus<=6 ~ 0)))

carencia_serv_2018 <- mutate(car_cal_svb_2018,              
          ic_sbv=case_when(
            is.na(isb_agua) | is.na(isb_dren) | is.na(isb_luz) | is.na(isb_combus) ~ NA_real_,
            isb_agua==1 | isb_dren==1 | isb_luz==1 | isb_combus==1 ~ 1, # Con carencia
            isb_agua==0 & isb_dren==0 & isb_luz==0 & isb_combus==0 ~ 0)) %>% # Sin carencia
  select(folioviv, foliohog, isb_agua, isb_dren, isb_luz, 
         isb_combus, ic_sbv, tot_integ, factor)

total_car_sbv_2018 <-carencia_serv_2018 %>% 
  transmute(folioviv = folioviv,
            ic_sbv = as.numeric(ic_sbv),
            tot_integ = as.numeric(tot_integ),
            pob_pr = ic_sbv * tot_integ,
            factor = as.numeric(factor),
            pob_sub = pob_pr * factor,
            pob_tot = sum(pob_sub, na.rm = T))

car_sbv_tot_2018 <- total_car_sbv_2018 %>% 
  select(pob_tot) %>% 
  unique(total_car_sbv_2018$pob_tot, incomparables = FALSE)

print(car_sbv_tot_2018)

# Ahora es necesario juntar todas los números totales

tabulado_servicios <- list(car_sbv_tot_2018, car_sbv_tot_2020, car_sbv_tot_2022)

tabulado_final_sbv <- bind_cols(tabulado_servicios)

print(tabulado_final_sbv)









