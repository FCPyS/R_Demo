
# Sesión 4 ----
#Hoy en escritorio por contingencia de rstudio.cloud

#Paquetería

if (!require("pacman")) install.packages("pacman") # instala pacman si se requiere
pacman::p_load(tidyverse, 
               readxl,writexl,googlesheets4, # importar hojas de cálculo
               haven, foreign, # importación de dta y sav
               sjlabelled, # etiquetas
               janitor, skimr, #limpieza y verificación
               imputeTS, # para imputar valores
               srvyr) # Para el diseño muestral

#Estableciendo mi directorio de trabajo
setwd("/Users/anaescoto/Dropbox/2020/2021-1 R para Demográfos/repo/R_Demo")

#Base 

library(haven)
ecovid0420 <- read_dta("https://github.com/aniuxa/R_Demo/raw/master/datos/ecovid0420.dta")
View(ecovid0420)

# Recodificando

ecovid0420 %>% 
  select(starts_with("pf3")) %>% 
  summary()


ecovid0420[is.na(ecovid0420$pf3_1),]$pf3_1<-0 # cambiamos los NA por 0
table(ecovid0420$pf3_1)

# pipes
ecovid0420 %>% 
  mutate(pf3_2=ifelse(is.na(pf3_2), #condición
                      0,#nuevo valor si la cumple
                      pf3_2) # valor si no la cumple
         ) %>% 
  tabyl(pf3_2)


ecovid0420<-ecovid0420 %>% 
  mutate(pf3_2=na_replace(pf3_2),
         pf3_3=na_replace(pf3_3))


ecovid0420<-ecovid0420 %>% 
  mutate_at(vars(starts_with("pf3")), na_replace) 

ecovid0420 %>% 
  select(starts_with("pf3")) %>% 
  summary()


# Tabulados

ecovid0420 %>%
  mutate(pb1=as_label(pb1))  %>% #cambia los valores a las etiquetas
  tabyl(pb1) # hace la tabla

ecovid0420 %>%
  mutate(pb1=as_label(pb1))  %>% #cambia los valores a las etiquetas
  tabyl(pb1) %>%
  adorn_totals() #primer enchulamiento


ecovid0420 %>%
  mutate(pb1=as_label(pb1)) %>% # cambia los valores de la variable a sus etiquetas
  tabyl(pb1) %>% # para hacer la tabla
  adorn_totals() %>% # añade totales
  adorn_pct_formatting()  # nos da porcentaje en lugar de proporción

glimpse(ecovid0420$pb3)

ecovid0420 %>% mutate(pb3=as_label(pb3)) %>% #esto sólo si hay etiquetas declaradas, recuerda
  tabyl(pb3)


ecovid0420 %>% 
  filter(pb2<40) %>% 
  mutate(pb3=as_label(pb3)) %>% 
  tabyl(pb3, show_missing_levels=F  ) %>% # esta opción elimina los valores con 0
  adorn_totals()  


# Tablas de doble entrada

ecovid0420 %>% 
  mutate(pb3=as_label(pb3)) %>% # para que las lea como factor
  mutate(pb1=as_label(pb1)) %>% # para que las lea como factor
  tabyl(pb3, pb1, show_missing_levels=F ) %>% # incluimos aquí 
  adorn_totals("row")  

ecovid0420 %>% 
  mutate(pb3=as_label(pb3)) %>% # para que las lea como factor
  mutate(pb1=as_label(pb1)) %>% # para que las lea como factor
  tabyl(pb3, pb1, show_missing_levels=F ) %>% # incluimos aquí 
  adorn_totals("col")  


ecovid0420 %>% 
  mutate(pb3=as_label(pb3)) %>% # para que las lea como factor
  mutate(pb1=as_label(pb1)) %>% # para que las lea como factor
  tabyl(pb3, pb1, show_missing_levels=F ) %>% # incluimos aquí 
  adorn_totals(c("col", "row") ) 



ecovid0420 %>% 
  mutate(pb3=as_label(pb3)) %>% # para que las lea como factor
  mutate(pb1=as_label(pb1)) %>% # para que las lea como factor
  tabyl(pb3, pb1, show_missing_levels=F ) %>% # incluimos aquí 
  adorn_totals(c("col", "row") ) %>% 
  adorn_percentages("col") %>% # Divide los valores entre el total de la columna
  adorn_pct_formatting() # lo vuelve porcentaje


ecovid0420 %>% 
  mutate(pb3=as_label(pb3)) %>% # para que las lea como factor
  mutate(pb1=as_label(pb1)) %>% # para que las lea como factor
  tabyl(pb3, pb1, show_missing_levels=F ) %>% 
  adorn_totals(c("col", "row")) %>% 
  adorn_percentages("row") %>% # Divide los valores entre el total de la fila
  adorn_pct_formatting() # lo vuelve porcentaje



ecovid0420 %>% 
  mutate(pb3=as_label(pb3)) %>% # para que las lea como factor
  mutate(pb1=as_label(pb1)) %>% # para que las lea como factor
  tabyl(pb3, pb1, show_missing_levels=F ) %>% # incluimos aquí pb1o
  adorn_totals(c("col", "row")) %>% 
  adorn_percentages("all") %>% # Divide los valores entre el total de la población
  adorn_pct_formatting() # lo vuelve porcentaje


# Descriptivos

summary(ecovid0420$pe10_1) ## horas trabajadas

tabla<-ecovid0420 %>% 
 group_by(pb1) %>% 
  summarize(nombre_indicador=mean(pe10_1, na.rm = T),
            otro_indicador=median(pe10_1, na.rm=T),
            edad_media=mean(pb2, na.rm=T)) 

write_xlsx(tabla, path="tabla.xlsx")

# Tally

ecovid0420 %>% 
  group_by(as_label(pb1)) %>% 
  tally(fac_per) %>% #nombre del factor
  adorn_totals()  # Agrega total

ecovid0420 %>% 
  group_by(as_label(pb1)) %>% 
  tally(fac_per) %>% #nombre del factor
  adorn_totals() %>% # Agrega total
  adorn_percentages("all")  %>% 
  adorn_pct_formatting()


# Count

ecovid0420 %>% 
  count(pb1, pb3,  wt = fac_per) 


ecovid0420 %>% 
  count(as_label(pb1), as_label(pb3),  wt = fac_per) 


ecovid0420 %>% 
  mutate_at(vars(pb1, pb3), as_label) %>% 
  count(pb1, pb3,  wt = fac_per) %>% 
  pivot_wider(names_from = pb1, 
              values_from = n)


ecovid0420 %>% 
  mutate_at(vars(pb1, pb3), as_label) %>% # otra forma de mutate y as_label
  count(pb1, pb3,  wt = fac_per) %>% 
  pivot_wider(names_from = pb1, 
              values_from = n) %>%
  adorn_totals() %>% # Agrega total
  adorn_percentages("col")  %>% 
  adorn_pct_formatting()


# Diseño muestral

ecovid_srvy <- ecovid0420 %>%
  as_survey_design(weights = fac_per)


ecovid_srvy %>%
  summarize(media_horas = survey_mean(pe10_1,
                                      na.rm=T))

ecovid_srvy %>%
  group_by(pb1) %>% 
  summarize(media_horas = survey_mean(pe10_1, level=0.99,
                                      vartype = c("se", "ci"), # da intervalo de confianza
                                      na.rm=T))



ecovid_srvy %>%
  group_by(pb1) %>% 
  summarize(p25 = survey_quantile(pe10_1,0.25, level=0.99,
                                      vartype = c("se", "ci"), # da intervalo de confianza
                                      na.rm=T))


## Cuali

ecovid_srvy %>%
  mutate(pb1=as_label(pb1)) %>%  # etiquetas
  group_by(pb1) %>% #variables cuali
  summarize(proportion = survey_mean(vartype="ci"), # proporción
            total = survey_total() )   # totales


# Fusión

vera_persona <- read_dta("https://github.com/aniuxa/R_Demo/raw/master/datos/Tr_persona30_001.dta")
vera_viv <- read_dta("https://github.com/aniuxa/R_Demo/raw/master/datos/Tr_vivienda30_001.dta")


vera_persona_1<-vera_persona %>% 
  filter(sexo==1) # Hombres

vera_persona_3<-vera_persona %>% 
  filter(sexo==3) # Mujeres

merge0<-merge(vera_persona_1, vera_persona_3, all=T) 
dim(merge0)

merge0<-rbind(vera_persona_1, vera_persona_3)
dim(merge0)

dim(vera_persona)

merge0<-merge(vera_persona_1, vera_persona_3,
              by="id_persona", all=T) 
dim(merge0)


merge0<-merge(vera_persona_1, vera_persona_3, all=F) 
merge0<-merge(vera_persona_1, vera_persona_3, all=T) # el que usamos arriba
dim(merge0)


merge0<-merge(vera_persona_1, vera_persona_3, all.x=T) # el que usamos arriba
dim(merge0)

merge0<-merge(vera_persona_1, vera_persona_3, all.y=T) # el que usamos arriba
dim(merge0)


vera_total<-merge(vera_persona, vera_viv, by="id_viv")
dim(vera_total)
names(vera_total)

table(vera_total$ent.x, vera_total$ent.y)

vera_total<-merge(vera_persona, vera_viv, by="id_viv")

vera_total<-vera_total %>% 
  select(-ends_with(".y")) %>% #elimina las variables que terminan en .y
  rename_at(.vars = vars(ends_with(".x")), 
            .funs = funs(sub("[.]x$", "", .))) #renombra
